# read and clean COVID files

#example of an analysis using SuperLearner and SHAP
library(dplyr)
library(SuperLearner)


VAR2 <- c("sexe","agecl","marital","mari","animal","travail",
          "categorie","niveau","debuthospit","ALD","psy","surf",
          #"addictopsy.SQ001.","addictopsy.SQ002.","addictopsy.SQ003.",
          #"accordconf","information","infoofficielles","cov19",
          #"accesprotection","accouchement","acconf","info","infooff",
          "produits",
          #"entourage","enfant","gardeenfant","confinementfamille",
          "stresstrav","stressperso","stressgeneral",
          #"contacts.1.","contacts.2.","contacts.3.","contacts.4.","exterieurcl",
          "lieudevie","modalitetravcl","chargetravail",
          #"entouragecl","inquiettravail",
          "WEMWBS_TOT","semaine","dept")
          # "finces","repercfince","contactsconfinement.1.","contactsconfinement.2.")#,
          # "contactsconfinement.3.","contactsconfinement.4.")#,
mydf=finale[complete.cases(finale[,VAR2]),VAR2]
Y=mydf$WEMWBS_TOT
X=select(mydf,-c(WEMWBS_TOT,dept))
X <- X %>%
  mutate(across(c(semaine),~as.factor(.x)))

SL.library= list(#list or c, list for 
  "SL.randomForest", #"SL.ranger",#"SL.rf_100", #1000 trees; ranger (500 trees) does not work ...
  #"SL.xgboost",
  # "SL.step.forward",
  #"SL.step_IA_glmnet",#"
  #"SL.step_IA_randomForest",#"
  "SL.glmnet", #"SL.glmnet_ridge",#"SL.hal9001",
  "SL.earth"#,
  #"SL.mean"
)#
control=list(saveFitLibrary = TRUE, saveCVFitLibrary = FALSE, trimLogit = 0.001)

cvControl = list(V = 2L, stratifyCV=FALSE, shuffle=TRUE)
innercvControl = list(list(V = 2L, stratifyCV=FALSE, shuffle=TRUE))

method = "method.NNLS" # "method.AUC" "method.NNloglik" "method.NNLS"

cv_sl_all = CV.SuperLearner(Y = Y, X=X,
                            family = gaussian(),
                            control = control,
                            cvControl = cvControl,
                            innerCvControl=innercvControl,
                            method=method,
                            SL.library = SL.library
)


##############################

  anal_shap<-function(SL_data,covars) { 
    
    source("my_wrappers.R")
    
    
    ##########
    ####prepare matrix
    #########
    SL_data=SL_data[complete.cases(SL_data[,c(covars,"WEMWBS_TOT")]),c(covars,"WEMWBS_TOT")]
    
    
    Y<-SL_data$WEMWBS_TOT
    
    X=select(SL_data,all_of(covars)) %>%
      mutate(across(everything(),~as.factor(.x))) 
    
    ###########
    #you have to re-estimate a model with a smaller number of features
    ###########
    big_model = CV.SuperLearner(Y = Y, X=X,
                                family = gaussian(),
                                control = control,
                                cvControl = cvControl,
                                innerCvControl=innercvControl,
                                method=method,
                                SL.library = SL.library
    )
    ###########
    
    
    
    all_preds_matrix=matrix(NA,nrow=nrow(X),ncol=ncol(X)+1)
    colnames(all_preds_matrix)=c("glob",covars)
    
    ###########
    #fold by fold
    N_fold <- length(big_model$AllSL)
    for (i in 1:N_fold) {
      
      test=as.integer(rownames(big_model$AllSL[[i]]$SL.predict))
      x_train <- SL_data %>% 
        select(all_of(covars)) %>%
        slice(-test) %>%
        mutate(across(everything(),~as.factor(.x)))  #%>%
        #as.matrix()#as.matrix(as.data.frame(SL_data[-test, covars]))
      y_train <- Y[-test]#factor(as.data.frame(SL_data)[train, outcome])
      x_test <- SL_data %>% 
        select(all_of(covars)) %>%
        slice(test) %>%
        mutate(across(everything(),~as.factor(.x)))  #%>%
       #as.matrix()
      
      model = big_model$AllSL[[i]]
      
      
      explainer <- shapr(x=x_train, model=model,n_combinations = NULL)#NULL or 10000
      
      # Specifying the phi_0, i.e. the expected prediction without any features
      p <- mean(y_train)
      
      # Computing the actual Shapley values with kernelSHAP accounting for feature dependence using
      # the empirical (conditional) distribution approach with bandwidth parameter sigma = 0.1 (default)
      explanation <- explain(
        x_test,
        approach = "ctree",# empirical, gaussian, copula, ctree
        explainer = explainer,
        prediction_zero = p
      )
      
      all_preds_matrix[test,] <- cbind(explanation$p,
                                       as.matrix(explanation$dt)[,-1])
    }#for N fold loop
    
    return(list(all_preds_matrix,big_model))
    
  }

#potential covariates of interest
covars <- c("sexe","agecl","marital","mari","animal","travail","semaine")
mydf=finale[complete.cases(finale[,c(covars,"WEMWBS_TOT")]),c(covars,"WEMWBS_TOT")]
mydf=select(mydf,c(all_of(covars),"WEMWBS_TOT") ) %>%
  mutate(across(c(semaine),~as.factor(.x)))

explanation_matrix<-anal_shap(SL_data=mydf,  covars = covars);print("done")