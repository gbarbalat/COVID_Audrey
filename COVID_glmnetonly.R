# This is the master file 
#it reads the cleaned csv file 
#it adds a new cleaning step
#it preprocess the data using caret preprocessing steps
#it processes the data using glmnet

# read and clean COVID files
close.screen(all=TRUE)
rm(list=ls())
load("finale.RData")

library(ggplot2)
library(dplyr)
library(plyr)
library(caret)
library(recipes)
library(glmnet)
str(finale)
colSums(is.na(finale))

cv_holdout="cv"
preProcess=c("corr","nzv")
sample_method="down_sample" #down_sample none over_sample
k_seed=1; #see for sampling

Nfolds=20 #if sample_method = none or over_sample, Nfolds=5, if down_sample, Nfolds=20
without_IA=FALSE
metric="Rsquared"
alpha=0;save_as="ridge"
###########################################################

############################################################
#downsample without replacement
downSample_replace=function (x, y, list = FALSE, yname = "Class")
{
  if (!is.data.frame(x)) {
    x <- as.data.frame(x, stringsAsFactors = TRUE)
  }
  if (!is.factor(y)) {
    warning("Down-sampling requires a factor variable as the response. The original data was returned.")
    return(list(x = x, y = y))
  }
  minClass <- min(table(y))
  x$.outcome <- y
  x <- ddply(x, .(y), function(dat, n) dat[sample(seq(along = dat$.outcome),
                                                  n, replace=TRUE), , drop = FALSE], n = minClass)
  y <- x$.outcome
  x <- x[, !(colnames(x) %in% c("y", ".outcome")), drop = FALSE]
  if (list) {
    if (inherits(x, "matrix")) {
      x <- as.matrix(x)
    }
    out <- list(x = x, y = y)
  }
  else {
    out <- cbind(x, y)
    colnames(out)[ncol(out)] <- yname
  }
  out
}
########################################################


##########################################################"
#output=WEMWBS or stress
#remove duplicated
tmp=finale[!duplicated(finale$id_part),]
#none!

finale$jour=as.numeric(finale$jour)

#sex: you kept sexe2 = women, 1=women and 0= men
#agecl: 1=<30; 2=30, 3=50, 4=65, 5=75
finale=finale %>%
  mutate(acconf=case_when(acconf==1 ~ 4,
                          acconf==2 ~ 3,
                          acconf==3 ~ 2,
                          acconf==4 ~ 1,
                          acconf==5 ~ 0,
                          
  )) %>%
  mutate(info=case_when(info==1 ~ 4,
                        info==2 ~ 3,
                        info==3 ~ 2,
                        info==4 ~ 1,
                        info==5 ~ 0,
                          
  )) %>%
  mutate(infooff=case_when(infooff==1 ~ 4,
                           infooff==2 ~ 3,
                           infooff==3 ~ 2,
                           infooff==4 ~ 1,
                           infooff==5 ~ 0,
                        
  )) %>%
  mutate(produits=case_when(produits==1 ~ 0,
                            produits==2 ~ 1,
                            produits==3 ~ 2
                          
  )) %>%
  mutate(accesprotection_num=case_when(accesprotection=="A1" ~ 0,
                                   accesprotection=="A2" ~ 1,
                                   accesprotection=="A3" ~ 2,
                                   accesprotection=="A4" ~ 3#very preoccupied
                                   )) %>%
  mutate(contacts.1._num=case_when(contacts.1.=='L004' ~ 0,
                               contacts.1.=='L005' ~ 1,
                               contacts.1.=='L006' ~ 2,
                               contacts.1.=='L007' ~ 3
                               )) %>%
  mutate(contacts.2._num=case_when(contacts.2.=='L004' ~ 0,
                               contacts.2.=='L005' ~ 1,
                               contacts.2.=='L006' ~ 2,
                               contacts.2.=='L007' ~ 3
  )) %>%
  mutate(contacts.3._num=case_when(contacts.3.=='L004' ~ 0,
                               contacts.3.=='L005' ~ 1,
                               contacts.3.=='L006' ~ 2,
                               contacts.3.=='L007' ~ 3
  )) %>%
  mutate(contacts.4._num=case_when(contacts.4.=='L004' ~ 0,
                               contacts.4.=='L005' ~ 1,
                               contacts.4.=='L006' ~ 2,
                               contacts.4.=='L007' ~ 3
  )) %>%
  mutate(finances_num=case_when(finances=='L8' ~ 3,#direct effects on your budget
                            finances=='L9' ~ 2,
                            finances=='L10' ~ 1,
                            finances=='L11' ~ 0
  )) %>%
  mutate(repercfinance_num=case_when(repercfinance=='L001' ~ 3,
                                 repercfinance=='L002' ~ 2,
                                 repercfinance=='L003' ~ 1,
                                 repercfinance=='L004' ~ 0
  )) %>%
  mutate(cov19cl_num=case_when(cov19==2 | cov19==6 ~ 0,#no contact
                            cov19==1 | cov19==3 | cov19==4 ~ 2,#suspicion or contam
                            cov19==5 ~ 1#contact but no suspicion
  )) %>%
  mutate(logement_num=case_when(logement=="A1" ~ 1,#usual accomodation
                                logement=="A2" ~ 0#non-usual accomodation
  ))  %>%
  mutate(lieudevie_num=case_when(lieudevie=="A1" ~ 0,#urban
                                 lieudevie=="A2" ~ 1,
                                 lieudevie=="A3" ~ 2#rural
  )) %>%
  mutate(psy_num=case_when(psy=="A1" ~ 2,#actuellement
                           psy=="A2" ~ 1,#anciennement
                           psy=="A3" ~ 0#jamais
  ))

finale$enceinte=ifelse(is.na(finale$enceinte),2, finale$enceinte)
finale$soutienperso.1.=ifelse(is.na(finale$soutienperso.1.),0,finale$soutienperso.1.)
finale$soutienperso.2.=ifelse(is.na(finale$soutienperso.2.),0,finale$soutienperso.2.)
finale$soutienperso.3.=ifelse(is.na(finale$soutienperso.3.),0,finale$soutienperso.3.)
finale$soutienperso.4.=ifelse(is.na(finale$soutienperso.4.),0,finale$soutienperso.4.)
finale$soutienperso.5.=ifelse(is.na(finale$soutienperso.5.),0,finale$soutienperso.5.)
finale$soutienperso.6.=ifelse(is.na(finale$soutienperso.6.),0,finale$soutienperso.6.)
finale$soutienperso.7.=ifelse(is.na(finale$soutienperso.7.),0,finale$soutienperso.7.)
#finale$soutienperso.8.=ifelse(is.na(finale$soutienperso.8.),0,finale$soutienperso.8.)
finale$soutienperso.9.=ifelse(is.na(finale$soutienperso.9.),0,finale$soutienperso.9.)

#remove important NA 
finale=finale[!is.na(finale$franceconfi),]
finale=finale[!is.na(finale$dept),]
finale=finale[!is.na(finale$surface),]

VAR <- c("WEMWBS_TOT","semaine","jour",'dept',#'enceinte',
          "sexe","age","mari","niveau",#agecl
         "surf","logement_num","exterieurcl",
         "travail.1.","travail.2.","travail.3.","travail.4.","travail.5.","travail.6.","travail.7.",
         "atcdconfinement","ALD","psy_num","debuthospit",
         "acconf","info","infooff",
         "accesprotection_num","produits",
         "lieudevie_num","confinementfamille","animal","cov19cl_num",
          "stresstrav","stressperso","stressgeneral",#"travcl","modalitetravcl","chargetravail"
          # "contactsconfinement.1.","contactsconfinement.2.","contactsconfinement.3.","contactsconfinement.4.",
         "contacts.1._num","contacts.2._num","contacts.3._num","contacts.4._num",
          "finances_num","repercfinance_num",
         'faireface.14.','faireface.1.','faireface.2.','faireface.3.','faireface.4.',
         'faireface.5.','faireface.6.','faireface.7.','faireface.8.','faireface.9.','faireface.10.','faireface.11.',
         'faireface.12.','faireface.13.',
         "soutienperso.1.","soutienperso.2.","soutienperso.3.","soutienperso.4.","soutienperso.5.",
         "soutienperso.6.","soutienperso.7.","soutienperso.9."
         # ,"activites.SQ001.","activites.SQ002.","activites.SQ003.","activites.SQ004.",
         # "activites.SQ005.","activites.SQ006.","activites.SQ007.","activites.SQ008.",
         # "activites.SQ009.","activites.SQ010.","activites.SQ011.","activites.SQ012.",
         # "activites.SQ013.","activites.SQ014.","activites.SQ015.","activites.SQ016.",
         # "activites.SQ017.","activites.SQ018.","activites.SQ019.","activites.SQ020.",
         # "activites.SQ021.","activites.SQ022."
         )

#mydf=finale[complete.cases(finale[,VAR2]),VAR2]
mydf=finale[,VAR] %>% 
  select(-c(surf, travail.3.,travail.5., travail.6.,logement_num,debuthospit,
            faireface.2.,faireface.6.,faireface.10.,faireface.13.,faireface.14.,
            soutienperso.6.,soutienperso.7.,soutienperso.9.
            )) %>%
  select(-c("stresstrav","stressperso", "stressgeneral")) %>%
  #filter(agecl!=5) %>%
  filter(age<75) %>%
  filter(sexe!=3) %>%
  mutate(niveau=case_when(niveau ==1~4,
                             niveau ==2 ~4,
                             niveau ==3 ~4,
                             TRUE ~ as.numeric(niveau)
  )) %>%
  mutate(acconf=case_when(acconf==0~1,
                          TRUE ~ as.numeric(acconf)),
         # agecl=case_when(agecl==4 ~ 3,
         #                 TRUE ~ as.numeric(agecl)),
         info=case_when(acconf==0~1,
                          TRUE ~ as.numeric(info)),
         infooff=case_when(acconf==0~1,
                          TRUE ~ as.numeric(infooff)),
         produits=case_when(acconf==3~2,
                          TRUE ~ as.numeric(produits)),
         repercfinance_num=case_when(repercfinance_num==3~2,
                          TRUE ~ as.numeric(repercfinance_num)),
         )

VAR <- c("WEMWBS_TOT","semaine","jour",'dept',#'enceinte',
         "sexe","age","mari","niveau",
         "surf","logement_num","exterieurcl",
         "travail.1.","travail.2.","travail.3.","travail.4.","travail.5.","travail.6.","travail.7.",
         "atcdconfinement","ALD","psy_num","debuthospit",
         "acconf","info","infooff",
         "accesprotection_num","produits",
         "lieudevie_num","confinementfamille","animal","cov19cl_num",
         "stresstrav","stressperso","stressgeneral",#"travcl","modalitetravcl","chargetravail"
         # "contactsconfinement.1.","contactsconfinement.2.","contactsconfinement.3.","contactsconfinement.4.",
         "contacts.1._num","contacts.2._num","contacts.3._num","contacts.4._num",
         "finances_num","repercfinance_num",
         'faireface.14.','faireface.1.','faireface.2.','faireface.3.','faireface.4.',
         'faireface.5.','faireface.6.','faireface.7.','faireface.8.','faireface.9.','faireface.10.','faireface.11.',
         'faireface.12.','faireface.13.',
         "soutienperso.1.","soutienperso.2.","soutienperso.3.","soutienperso.4.","soutienperso.5.",
         "soutienperso.6.","soutienperso.7.","soutienperso.9."
         # ,"activites.SQ001.","activites.SQ002.","activites.SQ003.","activites.SQ004.",
         # "activites.SQ005.","activites.SQ006.","activites.SQ007.","activites.SQ008.",
         # "activites.SQ009.","activites.SQ010.","activites.SQ011.","activites.SQ012.",
         # "activites.SQ013.","activites.SQ014.","activites.SQ015.","activites.SQ016.",
         # "activites.SQ017.","activites.SQ018.","activites.SQ019.","activites.SQ020.",
         # "activites.SQ021.","activites.SQ022."
)

table(mydf$semaine)
colSums(is.na(mydf))


#transform as categorical variables
mydf = mydf %>%
  dplyr::mutate(across(-c(WEMWBS_TOT, age, jour, acconf, info, infooff,# niveau, cov19cl_num,lieudevie_num,psy_num
                   accesprotection_num,
                   produits,
                   contacts.1._num,contacts.2._num, contacts.3._num, contacts.4._num,
                   finances_num, repercfinance_num, starts_with("activites")),
                ~as.factor(.))) #  %>%   na.omit()

DM = model.matrix(WEMWBS_TOT ~ . -1 -semaine
                  ,mydf)# + semaine:.
DM=as.data.frame(DM)
DM$WEMWBS_TOT=mydf$WEMWBS_TOT
DM$semaine=mydf$semaine


preProc=preProcess(DM %>% select(-c(WEMWBS_TOT)),
                   method=c("corr","nzv"),cutoff=0.9)
mydf_preproc=predict(preProc,DM)
colnames(mydf_preproc)

  DM_IA = model.matrix(WEMWBS_TOT ~ . -1  -semaine + jour:. -jour:semaine  
                    ,mydf_preproc)# + semaine:.
  DM_IA=as.data.frame(DM_IA)
  DM_IA$WEMWBS_TOT=mydf_preproc$WEMWBS_TOT
  DM_IA$semaine=mydf_preproc$semaine
  #DM_IA$jour=NULL

# preProc=preProcess(DM_IA %>% select(-c(WEMWBS_TOT)),
#                    method=c("corr","nzv"),cutoff=1)
# DM_preproc=predict(preProc,DM_IA)
# colnames(DM_preproc)
  
  all_kseed=1000
  all_coeff=NULL
  r2_range=NULL
  lambda_range=NULL
  alpha=0
  
  # alpha=1
  # cv.model=cv.glmnet(y=DM_IA$WEMWBS_TOT, x=DM_IA %>% select(-c(WEMWBS_TOT,semaine)) %>% as.matrix(), 
  #                    alpha=alpha, nfolds=20)
  # lasso.model <- glmnet(x=DM_IA %>% select(-c(WEMWBS_TOT,semaine)) %>% as.matrix(),
  #                       y=DM_IA$WEMWBS_TOT,  family = "gaussian",  alpha=alpha,  lambda = cv.model$lambda.min )
  # lasso.model$dev.ratio
  # tmp_coeff=as.matrix(coef(lasso.model,s=cv.model$lambda.min ))
  
  
for (k_seed in (1:all_kseed)) {

if (sample_method=="down_sample") {
set.seed(k_seed)#1:10

down_pretrain <- downSample_replace(x = dplyr::select(DM_IA, -semaine),
                                    y = factor(DM_IA$semaine))
table(down_pretrain$Class)

down_pretrain = down_pretrain %>%
  select(-Class)


output_resample=down_pretrain


} else if (sample_method=="over_sample") {
  up_pretrain <- caret::upSample(x = select(mydf, -semaine),
                                     y = mydf$semaine)
  table(up_pretrain$Class)
  
  up_pretrain = up_pretrain %>%
    mutate(Class=as.numeric(Class))
  
  output_resample=up_pretrain
  
} else if (sample_method=="SMOTE") {
  
} else if (sample_method=="none") {
  
  output_resample=mydf %>%
    mutate(Class=as.numeric(semaine))
  
  table(output_resample$Class)
  
}
  

##########################
#VIF
# dat=cbind(WEMWBS_TOT=DM$WEMWBS_TOT,preProc.x)
# formula1=as.formula(paste0("WEMWBS_TOT ~ ",paste0(colnames(preProc.x),collapse = "+")))
# mod <- lm(WEMWBS_TOT ~ .,data=dat)
# car::vif(mod)
# all_coeff=tmp
##########################

  
cv.model=cv.glmnet(y=output_resample$WEMWBS_TOT, x=output_resample %>% select(-WEMWBS_TOT) %>% as.matrix(), alpha=alpha, nfolds=20)
lasso.model <- glmnet(x=output_resample %>% select(-WEMWBS_TOT) %>% as.matrix(),
                      y=output_resample$WEMWBS_TOT,  family = "gaussian",  alpha=alpha,  lambda = cv.model$lambda.min )
r2_range=c(r2_range,lasso.model$dev.ratio)
lambda_range=c(lambda_range,cv.model$lambda.min)
tmp_coeff=as.matrix(coef(lasso.model,s=cv.model$lambda.min ))
tmp=data.frame(param=rownames(tmp_coeff),coeff=tmp_coeff,group=k_seed)

all_coeff=rbind(all_coeff,tmp)
message(paste0("k_seed",k_seed))
}

all_coeff_secours=all_coeff
source("C:/Users/Guillaume/Desktop/COVID/renamed_var.R")
save.image(file=paste0("anal_glmnet_31March2023",save_as,".RData"))


###################################################################
###################################################################
###################################################################

rm(list=ls())
save_as="ridge"
load(file=paste0("anal_glmnet_31March2023",save_as,".RData"))
library(dplyr)
library(ggplot2)
library(sjPlot)

which_plot=all_coeff %>%
  dplyr::filter(param!="(Intercept)") %>%
  dplyr::mutate(s1=case_when(grepl("Age",param) ~ s1*10,
                             grepl("Time",param) ~ s1*10,
                             TRUE ~ as.numeric(s1))) %>% 
  dplyr::filter(grepl("Time",param))   

which_plot$param=sub('Time:',"",which_plot$param)

C_order=which_plot %>% dplyr::group_by(param) %>% dplyr::summarise(n=median(s1)) %>% arrange(desc(abs(n)))


#append positive or negative (sign of the coefficient)
which_plot$positive=NULL

for (i in 1:nrow(which_plot)) {
  if (C_order[grepl(which_plot$param[i],C_order$param),"n"]>0) {which_plot$positive[i]="pos."}
  else {which_plot$positive[i]="neg."}
  
}


#########
#main function building tables and plots
##########
build_table_plot=function(x) {

which_plot_fin=which_plot %>% filter(param %in% x)
  
#Table
Tablex_neg=which_plot_fin %>%
  filter(positive=="neg.")  %>%
  group_by(param) %>%
  dplyr::summarise(#mean_50 = scales::percent(0.5),
    #mean_05 = scales::percent(0.05),
    lb = quantile(s1, 0.025),
    Median = quantile(s1, 0.5),
    ub = quantile(s1, 0.975),
  ) %>%
  arrange(ub)

Tablex_pos=which_plot_fin %>%
  filter(positive=="pos.") %>%
  group_by(param) %>%
  dplyr::summarise(#mean_50 = scales::percent(0.5),
    #mean_05 = scales::percent(0.05),
    lb = quantile(s1, 0.025),
    Median = quantile(s1, 0.5),
    ub = quantile(s1, 0.975),
  ) %>%
  arrange(desc(lb))

#Plot
##
mean_coeff_neg=which_plot_fin %>%
  filter(positive=="neg.") %>%
  dplyr::group_by(param) %>%
  dplyr::summarise(quantile = scales::percent(c(0.975)),
                   s1 = quantile(s1, c(0.975)))  %>%
  arrange(s1)
mean_coeff_neg_signif=mean_coeff_neg %>%
  filter(s1<0) %>%
  select(param) %>%
  unlist()

##
mean_coeff_pos=which_plot_fin %>%
  filter(positive=="pos.") %>%
  group_by(param) %>%
  dplyr::summarise(quantile = scales::percent(c(0.025)),
                   s1 = quantile(s1, c(0.025))) %>%
  arrange(desc(s1))

mean_coeff_pos_signif=mean_coeff_pos %>%
  filter(s1>0) %>%
  select(param) %>%
  unlist()


which_plot_fin$param=factor(which_plot_fin$param,
                            levels=C_order$param)
which_plot_fin = which_plot_fin %>%
  mutate(signif=case_when(param %in% c(mean_coeff_neg_signif,mean_coeff_pos_signif) ~ TRUE,
                          !param %in% c(mean_coeff_neg_signif,mean_coeff_pos_signif) ~ FALSE)
  )

which_plot_fin$param=factor(which_plot_fin$param,
                            levels=c(mean_coeff_neg$param,mean_coeff_pos$param))

########

##########

g=ggplot(which_plot_fin ,aes(x=s1,y=param, fill=signif)) +#
  #geom_violin(draw_quantiles = c(0.5)) +
  geom_boxplot()+
  #geom_point()+
  geom_vline(xintercept = 0, colour='black', linetype = 'dotdash') +
  scale_y_discrete(limits=rev) +
  facet_wrap(~positive, scales='free_y') +
  labs(y= "Parameter", x = "Coefficient") + 
  scale_fill_manual(values=c("black", "red")) +
  theme(legend.position = "none",
        legend.text=element_text(size=11),
        axis.text = element_text(size=15),
        axis.title = element_text(size = 17),
        panel.border = element_rect(colour = "black", fill=NA),
        axis.line.y.right  = element_line(colour="black"),
        panel.background = element_rect(fill = "white",colour = "black"),
        strip.text = element_blank(),
        strip.background = element_rect(fill = "white",colour = "black"),
        plot.title = element_text(colour = "black",face="bold", size=20)#,hjust = 0.5)#family, 
  ) 


return(list(Tablex_neg,Tablex_pos,g))

}

results=build_table_plot(Back_var)
results=build_table_plot(COVID_var)
results=build_table_plot(Coping_var)


tab_df(results[[1]], digits = 4)
tab_df(results[[2]], digits = 4)
results[[3]]


###################
