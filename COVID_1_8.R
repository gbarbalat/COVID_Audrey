# read and clean COVID files
library(dplyr)
library(caret)

cv_holdout="cv"

preProcess_missing="medianImpute"
preProcess_missing_factor="medianImpute"
preProcess=c("center","scale",preProcess_missing,"corr","nzv","conditionalX")

#preProcess_rd="pca"
Nfolds=5
summaryFunction = defaultSummary #twoClassSummary #CustomprSummary
metric="Rsquared"
algorithm = "ranger"
grid_search = "random"
tuneGrid=NULL
tuneLength=10
set.seed(123)



VAR2 <- c("semaine","sexe","enceinte","trimestre","agecl","marital","mari","animal","travail",
          "categorie","profdesante","niveau","debuthospit","ALD","psy","surf",
          "addictopsy.SQ001.","addictopsy.SQ002.","addictopsy.SQ003.",
          "atcdconfinement","accordconf","information","infoofficielles","cov19",
          "accesprotection","accouchement","dept","acconf","info","infooff",
          "produits","entourage","enfant","gardeenfant","confinementfamille",
          "stresstrav","stressperso","stressgeneral",
          "contacts.1.","contacts.2.","contacts.3.","contacts.4.","exterieurcl",
          "lieudevie","entouragecl","modalitetravcl","chargetravail","inquiettravail",
          "finances","repercfinance","contactsconfinement.1.","contactsconfinement.2.",
          "contactsconfinement.3.","contactsconfinement.4.","WEMWBS_TOT")
#mydf=finale[complete.cases(finale[,VAR2]),VAR2]
mydf=finale[,VAR2]
# Y=mydf$WEMWBS_TOT
# X=select(mydf,-c(WEMWBS_TOT))

fitControl <- caret::trainControl(method = "repeatedcv",#boot #cv #oob repeated_cv
                                  number = 5,
                                  repeats=1,
                                  allowParallel = TRUE,
                                  index=NULL,
                                  classProbs = FALSE,
                                  summaryFunction = summaryFunction, #CustomprSummary,
                                  search = grid_search,#random or grid
                                  preProcOptions = list(thresh = 0.95, #cumulative percent of variance to be retained by PCA
                                                        ICAcomp = 3, 
                                                        k = 5, knnSummary=mean,
                                                        freqCut = 95/5, #95% of most common compared to 2nd most common
                                                        uniqueCut =10,
                                                        cutoff = 0.9,#for correlation cutoff
                                                        na.remove = FALSE
                                  ),
                                  sampling = NULL,
                                  savePredictions="final"
)

#  I leave out FoldIndex[[kFold]]
# for (kFold in 1:length(FoldIndex)) {
#   
#   if (cv_holdout=="cv") {
    #replace train function by rfe
    #caret::train or CAST::ffs
    hyper_params <- caret::train(#y=as.factor(df[-FoldIndex[[kFold]],"PSR"]),#y or response
      #x=dplyr::select(df[-FoldIndex[[kFold]],],#x or predictors
      #-PSR),
      WEMWBS_TOT ~ .,
      data=mydf,#df[-FoldIndex[[kFold]],],#data=df,
      method = algorithm,
      preProcess=preProcess,
      tuneGrid = tuneGrid,
      tuneLength=tuneLength,
      trControl = fitControl,
      metric = metric,
      na.action=na.pass#na.pass na.omit na.exclude na.fail
    )
    
    # yhat.algo=predict.train(hyper_params #best_model
    #                         ,newdata=dplyr::select(df[FoldIndex[[kFold]],],-PSR)
    #                         ,"prob")#[,-1] if rfe
    # ypreds=predict.train(hyper_params,
    #                      newdata=dplyr::select(df[FoldIndex[[kFold]],],-PSR)
    # )
    # 
    # y_hat_algo[FoldIndex[[kFold]],]  <- as.matrix(yhat.algo)
    # y_class_algo[FoldIndex[[kFold]]]  <- ypreds
    # 
    # y_true[FoldIndex[[kFold]]] <- as.factor(as.matrix(df[FoldIndex[[kFold]],"PSR"]))
    # 
    # ### predict on training data
    # yhat.algo_OF=predict.train(hyper_params#best_model_lasso[[kFold]]
    #                            ,newdata=dplyr::select(df[-FoldIndex[[kFold]],],-PSR)
    #                            ,type = "prob")#[,-1] if rfe
    # ypreds_OF=predict.train(hyper_params
    #                         ,newdata=dplyr::select(df[-FoldIndex[[kFold]],],-PSR)
    # )#[,1] if rfe
    # y_true_OF <- as.factor(as.matrix(df[-FoldIndex[[kFold]],"PSR"]))
    # 
    # y_hat_algo_allF=c(y_hat_algo_allF,list(yhat.algo_OF))
    # y_class_algo_allF=c(y_class_algo_allF,list(ypreds_OF))
    # y_true_allF=c(y_true_allF,list(y_true_OF))
    # ###
    # 
    # hyper_params_all=c(hyper_params_all,list(hyper_params))