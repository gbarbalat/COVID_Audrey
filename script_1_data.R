
# ?tude sur le confinement
# contact = Nicolas Franck
# script = ATM, 09/09/2020
# cr?ation de la base unique et datamanagement


### PARTIE 0 : PREAMBULE
close.screen(all=TRUE)
rm(list=ls())
header=1;

# chargement des packages
library(funModeling)
library(tidyverse)
#library(ez)

# chargement de l'environnement de travail et de la base
#setwd("C:/Users/tanguyau/Documents/Archives projets/Confinement")
here_data="C:/Users/Guillaume/Desktop/Recoverit 2022-04-17 at 22.09.53/_DATA/CRR/WEMWBS_COVID/"#C:/Users/tanguyau/Documents/Archives projets/Confinement/
base2 <- read.csv(file=paste0(here_data,"extraction semaine 2_complets.csv"),stringsAsFactors=F,na.strings="", sep = ",")
base3 <- read.csv(file=paste0(here_data,"extraction semaine 3_complets.csv"),stringsAsFactors=F,na.strings="")
base4 <- read.csv(file=paste0(here_data,"extraction semaine 4_complets.csv"),stringsAsFactors=F,na.strings="")
base5 <- read.csv(file=paste0(here_data,"extraction semaine 5_complets.csv"),stringsAsFactors=F,na.strings="")
base68 <- read.csv(file=paste0(here_data,"extraction semaine 6_a_8_complets.csv"),stringsAsFactors=F,na.strings="")

# suppression des lignes inutiles
base2 <- base2[!(is.na(base2$id)) , ]
base3 <- base3[!(is.na(base3$id)) , ]
base4 <- base4[!(is.na(base4$id)) , ]
base5 <- base5[!(is.na(base5$id)) , ]
base68 <- base68[!(is.na(base68$id)) , ]

# cr?ation d'une variable date utilisable
Sys.setlocale("LC_TIME","French")

base2$date <- as.POSIXct(base2$submitdate, format = "%Y-%m-%d %H:%M:%S")
base2$date2 <- format(base2$date, "%Y-%m-%d")
base3$date <- as.POSIXct(base3$submitdate, format = "%Y-%m-%d %H:%M:%S")
base3$date2 <- format(base3$date, "%Y-%m-%d")
base4$date <- as.POSIXct(base4$submitdate, format = "%Y-%m-%d %H:%M:%S")
base4$date2 <- format(base4$date, "%Y-%m-%d")
base5$date <- as.POSIXct(base5$submitdate, format = "%Y-%m-%d %H:%M:%S")
base5$date2 <- format(base5$date, "%Y-%m-%d")
base68$date <- as.POSIXct(base68$submitdate, format = "%Y-%m-%d %H:%M:%S")
base68$date2 <- format(base68$date, "%Y-%m-%d")

# cr?ation d'un indicateur "semaine"
base2$semaine <- 2
base3$semaine <- 3
base4$semaine <- 4
base5$semaine <- 5
base68$semaine <- ifelse(base68$date2 < "2020-04-29", 6,
                         ifelse(base68$date2 < "2020-05-06", 7, 8))

# exclusions base 2
base2$fr <- ifelse(as.character(base2$pays) %in% c("France", "france", "FRANCE", "France "), 1, 0)
base2 <- base2[!(base2$age < 16) , ]
base2 <- base2[!(base2$age > 100) , ]
base2 <- base2[!(base2$id == "15917") , ]
base2 <- base2[!(base2$fr == 0 & base2$france == 0), ]

# exclusions base 3
base3$fr <- ifelse(as.character(base3$pays) %in% c("France", "france", "FRANCE", "France "), 1, 0)
base3 <- base3[!(base3$age < 16) , ]
base3 <- base3[!(base3$age > 100) , ]
base3 <- base3[!(base3$id == "15917") , ]
base3 <- base3[!(base3$fr == 0 & base3$france == 0), ]

# exclusions base 4
base4$fr <- ifelse(as.character(base4$pays) %in% c("France", "france", "FRANCE", "France "), 1, 0)
base4 <- base4[!(base4$age < 16) , ]
base4 <- base4[!(base4$age > 100) , ]
base4 <- base4[!(base4$id == "15917") , ]
base4 <- base4[!(base4$fr == 0 & base4$france == 0), ]

# exclusions base 5
base5$fr <- ifelse(as.character(base5$pays) %in% c("France", "france", "FRANCE", "France "), 1, 0)
base5 <- base5[!(base5$age < 16) , ]
base5 <- base5[!(base5$age > 100) , ]
base5 <- base5[!(base5$id == "15917") , ]
base5 <- base5[!(base5$fr == 0 & base5$france == 0), ]

# exclusions base 6 ? 8
base68$fr <- ifelse(as.character(base68$pays) %in% c("France", "france", "FRANCE", "France "), 1, 0)
base68 <- base68[!(base68$age < 16) , ]
base68 <- base68[!(base68$age > 100) , ]
base68 <- base68[!(base68$id == "15917") , ]
base68 <- base68[!(base68$fr == 0 & base68$france == 0), ]
base6 <- base68[!(base68$semaine > 6), ]
base7 <- base68[!(base68$semaine == 6 | base68$semaine == 8), ]
base8 <- base68[!(base68$semaine < 8), ]

# cr?ation de la variable travail ? partir des 7 variables existantes
base2$travail <- ifelse(base2$travail.1. == 1, 1, ifelse(base2$travail.2. == 1, 2, 
                              ifelse(base2$travail.3. == 1, 3, ifelse(base2$travail.4. ==1, 4, 
                                            ifelse(base2$travail.5. == 1, 5, ifelse(base2$travail.6. == 1, 6, 
                                                          ifelse(base2$travail.7.== 1, 7, NA)))))))
base3$travail <- ifelse(base3$travail.1. == 1, 1, ifelse(base3$travail.2. == 1, 2, 
                               ifelse(base3$travail.3. == 1, 3, ifelse(base3$travail.4. ==1, 4, 
                                             ifelse(base3$travail.5. == 1, 5, ifelse(base3$travail.6. == 1, 6, 
                                                           ifelse(base3$travail.7.== 1, 7, NA)))))))
base4$travail <- ifelse(base4$travail.1. == 1, 1, ifelse(base4$travail2 == 1, 2, 
                                ifelse(base4$travail.3. == 1, 3, ifelse(base4$travail.4. ==1, 4, 
                                              ifelse(base4$travail.5. == 1, 5, ifelse(base4$travail.6. == 1, 6, 
                                                           ifelse(base4$travail.7.== 1, 7, NA)))))))
base5$travail <- ifelse(base5$travail.1. == 1, 1, ifelse(base5$travail.2. == 1, 2, 
                                 ifelse(base5$travail.3. == 1, 3, ifelse(base5$travail.4. ==1, 4, 
                                               ifelse(base5$travail.5. == 1, 5, ifelse(base5$travail.6. == 1, 6, 
                                                             ifelse(base5$travail.7.== 1, 7, NA)))))))
base6$travail <- ifelse(base6$travail.1. == 1, 1, ifelse(base6$travail.2. == 1, 2, 
                                  ifelse(base6$travail.3. == 1, 3, ifelse(base6$travail.4. ==1, 4, 
                                                ifelse(base6$travail.5. == 1, 5, ifelse(base6$travail.6. == 1, 6, 
                                                              ifelse(base6$travail.7.== 1, 7, NA)))))))
base7$travail <- ifelse(base7$travail.1. == 1, 1, ifelse(base7$travail.2. == 1, 2, 
                                   ifelse(base7$travail.3. == 1, 3, ifelse(base7$travail.4. ==1, 4, 
                                                     ifelse(base7$travail.5. == 1, 5, ifelse(base7$travail.6. == 1, 6, 
                                                              ifelse(base7$travail.7.== 1, 7, NA)))))))
base8$travail <- ifelse(base8$travail.1. == 1, 1, ifelse(base8$travail.2. == 1, 2, 
                                 ifelse(base8$travail.3. == 1, 3, ifelse(base8$travail.4. ==1, 4, 
                                                      ifelse(base8$travail.5. == 1, 5, ifelse(base8$travail.6. == 1, 6, 
                                                             ifelse(base8$travail.7.== 1, 7, NA)))))))
# cr?ation de la variable d?partement
base2$dept <- ifelse(base2$postalregion ==0, "NA",
                     ifelse(base2$postalregion <100 & base2$postalregion > 9999, base2$postalregion, substring(base2$postalregion,1,2)))
base2$dept <- ifelse(base2$dept == "1", "01", 
                     ifelse(base2$dept == "2", "02", 
                            ifelse(base2$dept == "3", "03",
                                   ifelse(base2$dept == "4", "04", 
                                          ifelse(base2$dept == "5", "05",
                                                 ifelse(base2$dept == "6", "06",
                                                        ifelse(base2$dept == "7", "07", 
                                                               ifelse(base2$dept == "8", "08", 
                                                                      ifelse(base2$dept == "9", "09", base2$dept)))))))))
base3$dept <- ifelse(base3$postalregion ==0, "NA",
                     ifelse(base3$postalregion <100 & base3$postalregion > 9999, base3$postalregion, substring(base3$postalregion,1,2)))
base3$dept <- ifelse(base3$dept == "1", "01", 
                     ifelse(base3$dept == "2", "02", 
                            ifelse(base3$dept == "3", "03",
                                   ifelse(base3$dept == "4", "04", 
                                          ifelse(base3$dept == "5", "05",
                                                 ifelse(base3$dept == "6", "06",
                                                        ifelse(base3$dept == "7", "07", 
                                                               ifelse(base3$dept == "8", "08", 
                                                                      ifelse(base3$dept == "9", "09", base3$dept)))))))))
base4$dept <- ifelse(base4$postalregion ==0, "NA",
                     ifelse(base4$postalregion <100 & base4$postalregion > 9999, base4$postalregion, substring(base4$postalregion,1,2)))
base4$dept <- ifelse(base4$dept == "1", "01", 
                     ifelse(base4$dept == "2", "02", 
                            ifelse(base4$dept == "3", "03",
                                   ifelse(base4$dept == "4", "04", 
                                          ifelse(base4$dept == "5", "05",
                                                 ifelse(base4$dept == "6", "06",
                                                        ifelse(base4$dept == "7", "07", 
                                                               ifelse(base4$dept == "8", "08", 
                                                                      ifelse(base4$dept == "9", "09", base4$dept)))))))))
base5$dept <- ifelse(base5$postalregion ==0, "NA",
                     ifelse(base5$postalregion <100 & base5$postalregion > 9999, base5$postalregion, substring(base5$postalregion,1,2)))
base5$dept <- ifelse(base5$dept == "1", "01", 
                     ifelse(base5$dept == "2", "02", 
                            ifelse(base5$dept == "3", "03",
                                   ifelse(base5$dept == "4", "04", 
                                          ifelse(base5$dept == "5", "05",
                                                 ifelse(base5$dept == "6", "06",
                                                        ifelse(base5$dept == "7", "07", 
                                                               ifelse(base5$dept == "8", "08", 
                                                                      ifelse(base5$dept == "9", "09", base5$dept)))))))))
base6$dept <- ifelse(base6$postalregion ==0, "NA",
                     ifelse(base6$postalregion <100 & base6$postalregion > 9999, base6$postalregion, substring(base6$postalregion,1,2)))
base6$dept <- ifelse(base6$dept == "1", "01", 
                     ifelse(base6$dept == "2", "02", 
                            ifelse(base6$dept == "3", "03",
                                   ifelse(base6$dept == "4", "04", 
                                          ifelse(base6$dept == "5", "05",
                                                 ifelse(base6$dept == "6", "06",
                                                        ifelse(base6$dept == "7", "07", 
                                                               ifelse(base6$dept == "8", "08", 
                                                                      ifelse(base6$dept == "9", "09", base6$dept)))))))))
base7$dept <- ifelse(base7$postalregion ==0, "NA",
                     ifelse(base7$postalregion <100 & base7$postalregion > 9999, base7$postalregion, substring(base7$postalregion,1,2)))
base7$dept <- ifelse(base7$dept == "1", "01", 
                     ifelse(base7$dept == "2", "02", 
                            ifelse(base7$dept == "3", "03",
                                   ifelse(base7$dept == "4", "04", 
                                          ifelse(base7$dept == "5", "05",
                                                 ifelse(base7$dept == "6", "06",
                                                        ifelse(base7$dept == "7", "07", 
                                                               ifelse(base7$dept == "8", "08", 
                                                                      ifelse(base7$dept == "9", "09", base7$dept)))))))))
base8$dept <- ifelse(base8$postalregion ==0, "NA",
                     ifelse(base8$postalregion <100 & base8$postalregion > 9999, base8$postalregion, substring(base8$postalregion,1,2)))
base8$dept <- ifelse(base8$dept == "1", "01", 
                     ifelse(base8$dept == "2", "02", 
                            ifelse(base8$dept == "3", "03",
                                   ifelse(base8$dept == "4", "04", 
                                          ifelse(base8$dept == "5", "05",
                                                 ifelse(base8$dept == "6", "06",
                                                        ifelse(base8$dept == "7", "07", 
                                                               ifelse(base8$dept == "8", "08", 
                                                                      ifelse(base8$dept == "9", "09", base8$dept)))))))))


# cr?ation de la variable score total : WEMWBS_TOT

all_WEMWBS=c("WEMWBStableau.SQ001.", "WEMWBStableau.SQ002.", "WEMWBStableau.SQ003.",
             "WEMWBStableau.SQ004." , "WEMWBStableau.SQ005.", "WEMWBStableau.SQ006." ,
             "WEMWBStableau.SQ007." , "WEMWBStableau.SQ008.", "WEMWBStableau.SQ009." ,
             "WEMWBStableau.SQ010." , "WEMWBStableau.SQ011.", "WEMWBStableau.SQ012." ,
             "WEMWBStableau.SQ013." , "WEMWBStableau.SQ014.")

calculate_WEMWBS_tot <- function(basex) {
a<-all_WEMWBS %>%
  map(
    function(x) 
      basex %>% 
      select(x) %>% 
      extract(x, 
              into = x,
              regex = "([1-5])"
              )
    ) %>%
  bind_cols() %>%
  mutate(across(everything(.),~as.numeric(.))) %>%
  mutate(WEMWBS_TOT = rowSums(.)) 
return(a$WEMWBS_TOT)
}

base2$WEMWBS_TOT <- calculate_WEMWBS_tot(base2)
base3$WEMWBS_TOT <- calculate_WEMWBS_tot(base3)
base4$WEMWBS_TOT <- calculate_WEMWBS_tot(base4)
base5$WEMWBS_TOT <- calculate_WEMWBS_tot(base5)
base6$WEMWBS_TOT <- calculate_WEMWBS_tot(base6)
base7$WEMWBS_TOT <- calculate_WEMWBS_tot(base7)
base8$WEMWBS_TOT <- calculate_WEMWBS_tot(base8)


### PARTIE 1 : APPARIEMENT


# cr?ation de l'identifiant patient unique avec 15 variables
base2$id_pat <- paste(base2$sexe, base2$age, base2$marital, base2$modevie, base2$enfantstotal, base2$dept, base2$animal, base2$categorie, base2$niveau, base2$travail, base2$atcdconfinement, base2$ALD, base2$psy, base2$logement, base2$surface, sep="") # 11381 patients uniques 
df_status(base2$id_pat)
base3$id_pat <- paste(base3$sexe, base3$age, base3$marital, base3$modevie, base3$enfantstotal, base3$dept, base3$animal, base3$categorie, base3$niveau, base3$travail, base3$atcdconfinement, base3$ALD, base3$psy, base3$logement, base3$surface, sep="") # 5107 patients uniques 
df_status(base3$id_pat)
base4$id_pat <- paste(base4$sexe, base4$age, base4$marital, base4$modevie, base4$enfantstotal, base4$dept, base4$animal, base4$categorie, base4$niveau, base4$travail, base4$atcdconfinement, base4$ALD, base4$psy, base4$logement, base4$surface, sep="") # 642 patients uniques 
df_status(base4$id_pat)
base5$id_pat <- paste(base5$sexe, base5$age, base5$marital, base5$modevie, base5$enfantstotal, base5$dept, base5$animal, base5$categorie, base5$niveau, base5$travail, base5$atcdconfinement, base5$ALD, base5$psy, base5$logement, base5$surface, sep="") # 1276 patients uniques 
df_status(base5$id_pat)
base6$id_pat <- paste(base6$sexe, base6$age, base6$marital, base6$modevie, base6$enfantstotal, base6$dept, base6$animal, base6$categorie, base6$niveau, base6$travail, base6$atcdconfinement, base6$ALD, base6$psy, base6$logement, base6$surface, sep="") # 884 patients uniques 
df_status(base6$id_pat)
base7$id_pat <- paste(base7$sexe, base7$age, base7$marital, base7$modevie, base7$enfantstotal, base7$dept, base7$animal, base7$categorie, base7$niveau, base7$travail, base7$atcdconfinement, base7$ALD, base7$psy, base7$logement, base7$surface, sep="") # 884 patients uniques 
df_status(base7$id_pat)
base8$id_pat <- paste(base8$sexe, base8$age, base8$marital, base8$modevie, base8$enfantstotal, base8$dept, base8$animal, base8$categorie, base8$niveau, base8$travail, base8$atcdconfinement, base8$ALD, base8$psy, base8$logement, base8$surface, sep="") # 884 patients uniques 
df_status(base8$id_pat)

# gestion des patients en doublon
tab <- base2[duplicated(base2$id_pat),] # table pr?sentant les 10 patients avec des identifiants non-uniques
base2 <- base2[-which(base2$id %in% tab$id),] # suppression de ces 10 patients
tab <- base3[duplicated(base3$id_pat),] 
base3 <- base3[-which(base3$id %in% tab$id),]
tab <- base5[duplicated(base5$id_pat),] 
base5 <- base5[-which(base5$id %in% tab$id),]

test <- rbind(base2, base3, base4, base5, base6, base7, base8)


# fonctions
q25<-function(x,na.rm) {return(quantile(x,probs=0.25,na.rm=na.rm))}
q75<-function(x,na.rm) {return(quantile(x,probs=0.75,na.rm=na.rm))}

# chargement de l'environnement de travail et de la base
finale <- test

# transformer en facteurs les variables qui ont besoin de l'?tre
finale$acconf <- ifelse(finale$accordconf == "L001", 1, 
                        ifelse(finale$accordconf == "L002", 2, 
                               ifelse(finale$accordconf == "L003", 3,
                                      ifelse(finale$accordconf == "L004", 4, 
                                             ifelse(finale$accordconf == "L005", 5, NA)))))
finale$info <- ifelse(finale$information == "L001", 1, 
                      ifelse(finale$information == "L002", 2, 
                             ifelse(finale$information == "L003", 3,
                                    ifelse(finale$information == "L004", 4, 
                                           ifelse(finale$information == "L005", 5, NA)))))
finale$infooff <- ifelse(finale$infoofficielles == "L001", 1, 
                         ifelse(finale$infoofficielles == "L002", 2, 
                                ifelse(finale$infoofficielles == "L003", 3,
                                       ifelse(finale$infoofficielles == "L004", 4, 
                                              ifelse(finale$infoofficielles == "L005", 5, NA)))))
finale$agecl <- ifelse(finale$age < 30, 1, 
                       ifelse(finale$age > 29 & finale$age < 50, 2, 
                              ifelse(finale$age > 49 & finale$age < 65, 3, 
                                     ifelse(finale$age > 64 & finale$age < 75, 4, 
                                            ifelse(finale$age > 74, 5, NA)))))
finale$mari <- ifelse(finale$marital == 1 | finale$marital == 3 | finale$marital == 4, 1, 
                      ifelse(finale$marital == 2, 2, NA))
finale$surf <- ifelse(finale$surface < 19, 1, 
                      ifelse(finale$surface > 18 & finale$surface < 30, 2, 
                             ifelse(finale$surface > 29 & finale$surface < 90, 3, 
                                    ifelse(finale$surface > 89 & finale$surface < 120, 4, 
                                           ifelse(finale$surface > 119, 5, NA)))))
finale$stresstrav <- ifelse(finale$stress.SQ001. < 6, 1, 
                            ifelse(finale$stress.SQ001. > 5, 2, NA))
finale$stressperso <- ifelse(finale$stress.SQ002. < 6, 1, 
                             ifelse(finale$stress.SQ002. > 5, 2, NA))
finale$stressgeneral <- ifelse(finale$stress.SQ003. < 6, 1, 
                               ifelse(finale$stress.SQ003. > 5, 2, NA))
finale$exterieurcl <- ifelse(finale$exterieur.1. == 1, 1, 
                             ifelse(finale$exterieur.2. == 1, 1, 
                                    ifelse(finale$exterieur.3. == 1, 1, 
                                           ifelse(finale$exterieur.4. == 1, 1, 
                                                  ifelse(finale$exterieur.5. == 1, 1, 
                                                         ifelse(finale$exterieur.6. == 1, 1, 0))))))
finale$entouragecl <- ifelse(finale$entourage ==1, 1, 
                             ifelse(finale$entourage > 1 & finale$entourage < 10, 2, 
                                    ifelse(finale$entourage > 9, 3, NA)))
finale$modalitetravcl <- ifelse(finale$modalitetrav ==2, 1, 
                                ifelse(finale$modalitetrav ==1 | finale$modalitetrav ==3, 2, 
                                       ifelse(finale$modalitetrav ==4 | finale$modalitetrav ==5 | finale$modalitetrav ==6 | finale$modalitetrav ==7, 3, 4)))

VAR2 <- c("sexe","enceinte","trimestre","agecl","marital","mari","animal","travail",
          "categorie","profdesante","niveau","debuthospit","ALD","psy","surf",
          "addictopsy.SQ001.","addictopsy.SQ002.","addictopsy.SQ003.",
          "atcdconfinement","accordconf","information","infoofficielles","cov19",
          "accesprotection","accouchement","dept","acconf","info","infooff",
          "produits","entourage","enfant","gardeenfant","confinementfamille",
          "stresstrav","stressperso","stressgeneral",
          "contacts.1.","contacts.2.","contacts.3.","contacts.4.","exterieurcl",
          "lieudevie","entouragecl","modalitetravcl","chargetravail","inquiettravail",
          "finces","repercfince","contactsconfinement.1.","contactsconfinement.2.",
          "contactsconfinement.3.","contactsconfinement.4.")

stop("Thanks, you'll take over from here!")
a=3

##################################################################################################################
##################################################################################################################
##################################################################################################################
##################################################################################################################
##################################################################################################################
# STOP #
##################################################################################################################
##################################################################################################################
##################################################################################################################
##################################################################################################################
##################################################################################################################
##################################################################################################################

# cr?ation d'une base unique avec les donn?es r?p?t?es
test <- rbind(base2, base3, base4, base5, base6, base7, base8)

# cr?er une variable r?gion
depreg <- read.csv2("C:/Users/Adrian/Desktop/Audrey/departements-francais.csv",stringsAsFactors=F,na.strings="")
test$dept <- as.character(test$dept)
test2 <- merge (test, depreg, by="dept", all.x=T)
test2 <- test2[-which(test2$region == "Hauts-de-France" & test2$postalregion ==2) , ]

# cr?ation des bases avec les visites pour les donn?es r?p?t?es
v2 <- test2[duplicated(test2$id_pat),]
base <- test2[which(test2$id_pat %in% v2$id_pat),]
v1 <- base[-which(base$id %in% v2$id),]

# sortie de la table finale avec toutes les semaines sans les donn?es r?p?t?es
finale <- test2
write.table(finale,file="finale.xls", append = F, quote = T, sep = "\t",eol = "\n", 
            na="NA", dec=".", row.names=F,col.names=T, qmethod=c("escape","double"))



################## TESTS ################

# cr?ation de l'identifiant patient unique avec 12 variables
base2$id_pat <- paste(base2$sexe, base2$age, base2$marital, base2$enfantstotal, base2$dept, base2$categorie, base2$niveau, base2$travail, base2$atcdconfinement, base2$ALD, base2$psy, base2$surface, sep="") # 11347 patients uniques 
df_status(base2$id_pat)
base3$id_pat <- paste(base3$sexe, base3$age, base3$marital, base3$enfantstotal, base3$dept, base3$categorie, base3$niveau, base3$travail, base3$atcdconfinement, base3$ALD, base3$psy, base3$surface, sep="") # 5100 patients uniques 
df_status(base3$id_pat)
base4$id_pat <- paste(base4$sexe, base4$age, base4$marital, base4$enfantstotal, base4$dept, base4$categorie, base4$niveau, base4$travail, base4$atcdconfinement, base4$ALD, base4$psy, base4$surface, sep="") # 642 patients uniques 
df_status(base4$id_pat)
base5$id_pat <- paste(base5$sexe, base5$age, base5$marital, base5$enfantstotal, base5$dept, base5$categorie, base5$niveau, base5$travail, base5$atcdconfinement, base5$ALD, base5$psy, base5$surface, sep="") # 1276 patients uniques 
df_status(base5$id_pat)
base6$id_pat <- paste(base6$sexe, base6$age, base6$marital, base6$enfantstotal, base6$dept, base6$categorie, base6$niveau, base6$travail, base6$atcdconfinement, base6$ALD, base6$psy, base6$surface, sep="") # 396 patients uniques 
df_status(base6$id_pat)
base7$id_pat <- paste(base7$sexe, base7$age, base7$marital, base7$enfantstotal, base7$dept, base7$categorie, base7$niveau, base7$travail, base7$atcdconfinement, base7$ALD, base7$psy, base7$surface, sep="") # 346 patients uniques 
df_status(base7$id_pat)
base8$id_pat <- paste(base8$sexe, base8$age, base8$marital, base8$enfantstotal, base8$dept, base8$categorie, base8$niveau, base8$travail, base8$atcdconfinement, base8$ALD, base8$psy, base8$surface, sep="") # 142 patients uniques 
df_status(base8$id_pat)

# cr?ation d'une base unique avec les donn?es r?p?t?es
test <- rbind.data.frame(base2, base3, base4, base5, base6, base7, base8)
tab <- test[duplicated(test$id_pat),]
base <- test[which(test$id_pat %in% tab$id_pat),]
table(base$semaine)


# cr?ation de l'identifiant patient unique avec 10 variables
base2$id_pat <- paste(base2$sexe, base2$age, base2$marital, base2$enfantstotal, base2$dept, base2$categorie, base2$travail, base2$atcdconfinement, base2$psy, base2$surface, sep="") # 11309 patients uniques 
df_status(base2$id_pat)
base3$id_pat <- paste(base3$sexe, base3$age, base3$marital, base3$enfantstotal, base3$dept, base3$categorie, base3$travail, base3$atcdconfinement, base3$psy, base3$surface, sep="") # 5093 patients uniques 
df_status(base3$id_pat)
base4$id_pat <- paste(base4$sexe, base4$age, base4$marital, base4$enfantstotal, base4$dept, base4$categorie, base4$travail, base4$atcdconfinement, base4$psy, base4$surface, sep="") # 642 patients uniques 
df_status(base4$id_pat)
base5$id_pat <- paste(base5$sexe, base5$age, base5$marital, base5$enfantstotal, base5$dept, base5$categorie, base5$travail, base5$atcdconfinement, base5$psy, base5$surface, sep="") # 1276 patients uniques 
df_status(base5$id_pat)
base6$id_pat <- paste(base6$sexe, base6$age, base6$marital, base6$enfantstotal, base6$dept, base6$categorie, base6$travail, base6$atcdconfinement, base6$psy, base6$surface, sep="") # 396 patients uniques 
df_status(base6$id_pat)
base7$id_pat <- paste(base7$sexe, base7$age, base7$marital, base7$enfantstotal, base7$dept, base7$categorie, base7$travail, base7$atcdconfinement, base7$psy, base7$surface, sep="") # 345 patients uniques 
df_status(base7$id_pat)
base8$id_pat <- paste(base8$sexe, base8$age, base8$marital, base8$enfantstotal, base8$dept, base8$categorie, base8$travail, base8$atcdconfinement, base8$psy, base8$surface, sep="") # 142 patients uniques 
df_status(base8$id_pat)

# cr?ation d'une base unique avec les donn?es r?p?t?es
test <- rbind.data.frame(base2, base3, base4, base5, base6, base7, base8)
tab <- test[duplicated(test$id_pat),]
base <- test[which(test$id_pat %in% tab$id_pat),]
table(base$semaine)

# cr?ation de l'identifiant patient unique avec 7 variables
base2$id_pat <- base2$id_pat <- paste(base2$sexe, base2$age, base2$marital, base2$modevie, base2$enfantstotal, base2$postalregion, base2$animal, sep="") # 11006 patients uniques 
df_status(base2$id_pat)
base3$id_pat <- base3$id_pat <- paste(base3$sexe, base3$age, base3$marital, base3$modevie, base3$enfantstotal, base3$postalregion, base3$animal, sep="") # 5051 patients uniques 
df_status(base3$id_pat)
base4$id_pat <- base4$id_pat <- paste(base4$sexe, base4$age, base4$marital, base4$modevie, base4$enfantstotal, base4$postalregion, base4$animal, sep="") # 642 patients uniques 
df_status(base4$id_pat)
base5$id_pat <- base5$id_pat <- paste(base5$sexe, base5$age, base5$marital, base5$modevie, base5$enfantstotal, base5$postalregion, base5$animal, sep="") # 1269 patients uniques 
df_status(base5$id_pat)
base6$id_pat <- base6$id_pat <- paste(base6$sexe, base6$age, base6$marital, base6$modevie, base6$enfantstotal, base6$postalregion, base6$animal, sep="") # 393 patients uniques 
df_status(base6$id_pat)
base7$id_pat <- base7$id_pat <- paste(base7$sexe, base7$age, base7$marital, base7$modevie, base7$enfantstotal, base7$postalregion, base7$animal, sep="") # 347 patients uniques 
df_status(base7$id_pat)
base8$id_pat <- base8$id_pat <- paste(base8$sexe, base8$age, base8$marital, base8$modevie, base8$enfantstotal, base8$postalregion, base8$animal, sep="") # 142 patients uniques 
df_status(base8$id_pat)

# cr?ation d'une base unique avec les donn?es r?p?t?es
test <- rbind.data.frame(base2, base3, base4, base5, base6, base7, base8)
tab <- test[duplicated(test$id_pat),]
base <- test[which(test$id_pat %in% tab$id_pat),]
table(base$semaine)



# cr?ation de l'identifiant patient unique avec 5 variables
base2$id_pat <- base2$id_pat <- paste(base2$sexe, base2$age, base2$marital, base2$dept, base2$enfantstotal, sep="") # 6803 patients uniques 
df_status(base2$id_pat)
base3$id_pat <- base3$id_pat <- paste(base3$sexe, base3$age, base3$marital, base3$dept, base3$enfantstotal, sep="") # 3989 patients uniques 
df_status(base3$id_pat)
base4$id_pat <- base4$id_pat <- paste(base4$sexe, base4$age, base4$marital, base4$dept, base4$enfantstotal, sep="") # 626 patients uniques 
df_status(base4$id_pat)
base5$id_pat <- base5$id_pat <- paste(base5$sexe, base5$age, base5$marital, base5$dept, base5$enfantstotal, sep="") # 1161 patients uniques 
df_status(base5$id_pat)
base6$id_pat <- base6$id_pat <- paste(base6$sexe, base6$age, base6$marital, base6$dept, base6$enfantstotal, sep="") # 367 patients uniques 
df_status(base6$id_pat)
base7$id_pat <- base7$id_pat <- paste(base7$sexe, base7$age, base7$marital, base7$dept, base7$enfantstotal, sep="") # 331 patients uniques 
df_status(base7$id_pat)
base8$id_pat <- base8$id_pat <- paste(base8$sexe, base8$age, base8$marital, base8$dept, base8$enfantstotal, sep="") # 141 patients uniques 
df_status(base8$id_pat)

# cr?ation d'une base unique avec les donn?es r?p?t?es
test <- rbind.data.frame(base2, base3, base4, base5, base6, base7, base8)
tab <- test[duplicated(test$id_pat),]
base <- test[which(test$id_pat %in% tab$id_pat),]
table(base$semaine)
