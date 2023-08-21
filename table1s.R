#COVID table 1


###############
library(table1)
library(dplyr)
my.render.cont <- function(x) {
  with(stats.default(x), 
       sprintf("%0.1f (%0.1f)", MEAN, SD))
}

#rename variables

# VAR <- c("jour",              "sexe2",               "sexe3",               "agecl",               "mari2",  
#          "enceinte2",
#          "niveau",              "surf",                "travail.1.1",         "travail.2.1",         "travail.3.1",        
#          "travail.4.1",         "travail.5.1",         "travail.6.1",         "travail.7.1",         "atcdconfinement1",   
#          "ALD1",                "psy_num",             "logement_num1",       "debuthospit2",        "acconf",             
#          "info",                "infooff",             "accesprotection_num", "produits",            "exterieurcl1",       
#          "lieudevie_num",       "confinementfamille1", "animal1",             "cov19cl_num",         "contacts.1._num",    
#          "contacts.2._num",     "contacts.3._num",     "contacts.4._num",     "finances_num",        "repercfinance_num",  
#          "faireface.14.1",      "faireface.1.1",       "faireface.2.1",       "faireface.3.1",       "faireface.4.1",      
#          "faireface.5.1",       "faireface.6.1",       "faireface.7.1",       "faireface.8.1",       "faireface.9.1",      
#          "faireface.10.1",      "faireface.11.1",      "faireface.12.1",      "faireface.13.1",      "soutienperso.1.1",   
#          "soutienperso.2.1",    "soutienperso.3.1",    "soutienperso.4.1",    "soutienperso.5.1",    "soutienperso.6.1",   
#          "soutienperso.7.1",    "soutienperso.9.1"
#          , "dept69", "dept75",    
#          "activites.SQ001.",    "activites.SQ002.",    "activites.SQ003.",   
#          "activites.SQ004.",    "activites.SQ005.",    "activites.SQ006.",    "activites.SQ007.",    "activites.SQ008.",   
#          "activites.SQ009.",    "activites.SQ010.",    "activites.SQ011.",    "activites.SQ012.",    "activites.SQ013.",   
#          "activites.SQ014.",    "activites.SQ015.",    "activites.SQ016.",    "activites.SQ017.",    "activites.SQ018.",   
#          "activites.SQ019.",    "activites.SQ020.",    "activites.SQ021.",    "activites.SQ022."   
# )


VAR <- c("jour",              "sexe",               "sexe3",               "agecl",               "mari",
         "enceinte2",
         "niveau",              "surf",                "travail.1.",         "travail.2.",         "travail.3.",
         "travail.4.",         "travail.5.",         "travail.6.",         "travail.7.",         "atcdconfinement",
         "ALD",                "psy_num",             "logement_num1",       "debuthospit2",        "acconf",
         "info",                "infooff",             "accesprotection_num", "produits",            "exterieurcl",
         "lieudevie_num",       "confinementfamille", "animal",             "cov19cl_num",         "contacts.1._num",
         "contacts.2._num",     "contacts.3._num",     "contacts.4._num",     "finances_num",        "repercfinance_num",
         "faireface.14.",             "faireface.2.",       "faireface.3.",       "faireface.4.",
         "faireface.5.",       "faireface.6.",       "faireface.7.",       "faireface.8.","faireface.9.",
         "faireface.10.",      "faireface.11.",      "faireface.12.",      "faireface.13.",  "faireface.1.", 
         "soutienperso.1.",
         "soutienperso.2.",    "soutienperso.3.",    "soutienperso.4.",    "soutienperso.5.",    "soutienperso.6.",
         "soutienperso.7.",    "soutienperso.9."
         , "dept69", "dept75",
         "activites.SQ001.",    "activites.SQ002.",    "activites.SQ003.",
         "activites.SQ004.",    "activites.SQ005.",    "activites.SQ006.",    "activites.SQ007.",    "activites.SQ008.",
         "activites.SQ009.",    "activites.SQ010.",    "activites.SQ011.",    "activites.SQ012.",    "activites.SQ013.",
         "activites.SQ014.",    "activites.SQ015.",    "activites.SQ016.",    "activites.SQ017.",    "activites.SQ018.",
         "activites.SQ019.",    "activites.SQ020.",    "activites.SQ021.",    "activites.SQ022."
)



RENAMES_VAR <- c("Day",              "Sex- Female",               "sexe3",               "Age",    "In a relationship", 
                 "Pregnant",
                 "Education","Size of acc.", 
                 "Employee",         "Indpdt",         "Job seeker",    "Student",
                 "Jobless",         "travail.6.1",         "Retired",  
                 "Ever been locked",   
                 "Chronic medical pb","Psychiatric hx", "Lockdown in usual acc.",    "Lockdown in hospital", 
                 "Agree w/ lockdown",             
                 "Satisfied with info", "Clarity of official info",
                 "Worried w/ PPE", "Worried w/ essential products",
                 "Access to outdoor space",       
                 "Rural",       "Lockdown w/ family", "Pet",   "Contamination risk",         
                 "Contacts- Face to face",    
                 "Contacts- Phone",     "Contacts- Text",     "Contacts- Soc. network",    
                 "Worry- Financial situation",        "Worry- Precar.",  
                 "Cope- Medications",      "Cope- Media says","Cope- Beliefs in better",
                 "Cope- Science",      
                 "Cope- Religious beliefs",       "Cope- Esoteric beliefs",       "Cope- Resilience",      
                 "Cope- Collective actions",       "Cope- Planet better",      
                 "Cope- Other people exp.","Cope- Indiv. better", "Cope- Nil",   "Cope- Substance use",
                 "Cope- Those close to you say",  
                 "Support- Family",   
                 "Support- Friends",    "Support- Colleagues",    "Support- Neighbours",
                 "Support- Same roof",    "Support- Health/social prof.",   
                 "Support- Associations",    "Support- Other"   
                 , "dis.69", "dis.75" 
                 # "activites.SQ001.",    "activites.SQ002.",    "activites.SQ003.",   
                 # "activites.SQ004.",    "activites.SQ005.",    "activites.SQ006.",    "Sport",    "activites.SQ008.",   
                 # "activites.SQ009.",    "activites.SQ010.",    "activites.SQ011.",    "activites.SQ012.",    "activites.SQ013.",   
                 # "activites.SQ014.",    "Internet",    "activites.SQ016.",    "DIY",    "Medidate",   
                 # "Daydream",    "Anxious thoughts",    "Naps",    "Talk"   
)





#take out Day: variables
mydfMdf=mydf %>% select(-starts_with("jour")) %>% select(-starts_with("dept")) %>%
  mutate(across(.cols = -"WEMWBS_TOT",~as.factor(.)))


for (i in 1:length(colnames(mydfMdf))) {
  
  for (j in 1:length(VAR)) {
    if (grepl("info",VAR[j])) {
      names(mydfMdf)[i]=gsub(paste0("\\<",VAR[j],"\\>"),RENAMES_VAR[j], names(mydfMdf)[i])
    } else {names(mydfMdf)[i]=gsub(VAR[j],RENAMES_VAR[j], names(mydfMdf)[i])}
    
  }
}

pvalue <- function(x, ...) {
  # Construct vectors of data y, and groups (strata) g
  y <- unlist(x)
  g <- factor(rep(1:length(x), times=sapply(x, length)))
  if (is.numeric(y)) {
    # For numeric variables, perform a standard 2-sample t-test
    p <- t.test(y ~ g)$p.value
  } else {
    # For categorical variables, perform a chi-squared test of independence
    p <- chisq.test(table(y, g))$p.value
  }
  # Format the p-value, using an HTML entity for the less-than sign.
  # The initial empty string places the output on the line below the variable label.
  c("", sub("<", "&lt;", format.pval(p, digits=2, eps=0.0001)))
}




################################
#MYDF (raw) table 1
################################


table1::table1(~ .
       | factor(semaine),
       data=mydfMdf,  
       rowlabelhead = "Variables",
       overall=T,# extra.col=list(`P-value`=pvalue)
       render.continuous=my.render.cont
) -> Table1_final
Table1_final

##################################
#################################


tmp1=mydfMdf %>% filter(sexe==1) %>% mutate(Male=WEMWBS_TOT) %>% select(c(semaine,Male))
tmp2=mydfMdf %>% filter(sexe==2) %>% mutate(Female=WEMWBS_TOT) %>% select(c(semaine,Female))
#tmp=left_join(tmp1,tmp2,by="semaine")
table1(~ Male
       | factor(semaine),
       data=tmp1,  
       rowlabelhead = "Variables",
       overall=F#, extra.col=list(`P-value`=pvalue)
       #,render.continuous=my.render.cont
) -> Table1_1
table1(~ Female
       | factor(semaine),
       data=tmp2,  
       rowlabelhead = "Variables",
       overall=F#, extra.col=list(`P-value`=pvalue)
       #,render.continuous=my.render.cont
) -> Table1_2
Table1_1
Table1_2



 table1(~ m
       | factor(semaine)+sexe ,
       data=tmp,  
       rowlabelhead = "Variables",transpose = TRUE,
       overall=F#, extra.col=list(`P-value`=pvalue)
       #,render.continuous=my.render.cont
) 
###########################
#DM - Table 1
###########################
#source("renamed_var.R")
#take out Day: variables
DMmd=DM %>% select(-starts_with("Day")) %>% select(-starts_with("dept")) %>%
  mutate(across(.cols = everything(),~as.factor(.)))

table1(~ .
       | factor(Class),
       data=DMmd,  
       rowlabelhead = "Variables",
       overall=F#, extra.col=list(`P-value`=pvalue)
       #,render.continuous=my.render.cont
) -> Table1_final
Table1_final


#############################
#DM - Table 1 bis
#############################
DM %>% group_by(Class, sexe) %>% summarize(m=mean(WEMWBS_TOT))

