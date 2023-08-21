#rename variables

VAR <- c("jour",              "sexe2",               "sexe3",               "age",               "mari2",  
         "enceinte2",
         "niveau5","niveau6","niveau7","niveau8",              
         "surf",                "travail.1.1",         "travail.2.1",         "travail.3.1",        
         "travail.4.1",         "travail.5.1",         "travail.6.1",         "travail.7.1",         "atcdconfinement1",   
         "ALD1",               
         "psy_num1", "psy_num2",           
         "logement_num1",       "debuthospit2",        "acconf",             
         "info",                "infooff",             "accesprotection_num", "produits",            "exterieurcl1",       
         "lieudevie_num1","lieudevie_num2",       
         "confinementfamille1", "animal1",           
         "cov19cl_num1","cov19cl_num2",         
         "contacts.1._num",    
         "contacts.2._num",     "contacts.3._num",     "contacts.4._num",     "finances_num",        "repercfinance_num",  
         "faireface.14.1",      "faireface.1.1",       "faireface.2.1",       "faireface.3.1",       "faireface.4.1",      
         "faireface.5.1",       "faireface.6.1",       "faireface.7.1",       "faireface.8.1",       "faireface.9.1",      
         "faireface.10.1",      "faireface.11.1",      "faireface.12.1",      "faireface.13.1",      "soutienperso.1.1",   
         "soutienperso.2.1",    "soutienperso.3.1",    "soutienperso.4.1",    "soutienperso.5.1",    "soutienperso.6.1",   
         "soutienperso.7.1",    "soutienperso.9.1"
         , "dept69", "dept75", "dept38", "dept59","dept92",     
         "activites.SQ001.",    "activites.SQ002.",    "activites.SQ003.",   
         "activites.SQ004.",    "activites.SQ005.",    "activites.SQ006.",    "activites.SQ007.",    "activites.SQ008.",   
         "activites.SQ009.",    "activites.SQ010.",    "activites.SQ011.",    "activites.SQ012.",    "activites.SQ013.",   
         "activites.SQ014.",    "activites.SQ015.",    "activites.SQ016.",    "activites.SQ017.",    "activites.SQ018.",   
         "activites.SQ019.",    "activites.SQ020.",    "activites.SQ021.",    "activites.SQ022."   
)
# 
# VAR <- c("jour",              "sexe",               "sexe3",               "age",               "mari",
#          "enceinte2",
#          "niveau5","niveau6","niveau7","niveau8",              
#          "surf",                "travail.1.",         "travail.2.",         "travail.3.",
#          "travail.4.",         "travail.5.",         "travail.6.",         "travail.7.",         "atcdconfinement",
#          "ALD",               
#          "psy_num1", "psy_num2",           
#          "logement_num1",       "debuthospit2",        "acconf",
#          "info",                "infooff",             "accesprotection_num", "produits",            "exterieurcl",
#          "lieudevie_num1","lieudevie_num2",       
#          "confinementfamille", "animal",             
#          "cov19cl_num1","cov19cl_num2",         
#          "contacts.1._num",         "contacts.2._num",     "contacts.3._num",     "contacts.4._num",
#          "finances_num",        "repercfinance_num",
#          "faireface.14.",      "faireface.1.",       "faireface.2.",       "faireface.3.",       "faireface.4.",
#          "faireface.5.",       "faireface.6.",       "faireface.7.",       "faireface.8.","faireface.9.",
#          "faireface.10.",      "faireface.11.",      "faireface.12.",      "faireface.13.",      "soutienperso.1.",
#          "soutienperso.2.",    "soutienperso.3.",    "soutienperso.4.",    "soutienperso.5.",    "soutienperso.6.",
#          "soutienperso.7.",    "soutienperso.9."
#          , "dept69", "dept75",
#          "activites.SQ001.",    "activites.SQ002.",    "activites.SQ003.",
#          "activites.SQ004.",    "activites.SQ005.",    "activites.SQ006.",    "activites.SQ007.",    "activites.SQ008.",
#          "activites.SQ009.",    "activites.SQ010.",    "activites.SQ011.",    "activites.SQ012.",    "activites.SQ013.",
#          "activites.SQ014.",    "activites.SQ015.",    "activites.SQ016.",    "activites.SQ017.",    "activites.SQ018.",
#          "activites.SQ019.",    "activites.SQ020.",    "activites.SQ021.",    "activites.SQ022."
# )
# 

RENAMES_VAR <- c("Time","Sex: Female", "sexe3", "Age", "In a relationship", 
                 "Pregnancy",
                 "Educ: 12 to 14 y/","Educ: 14 y/ to Bach.","Educ: Bach. to Masters","Educ: Masters to PhD",              
                 "Size of acc.", 
                 "Employee",         "Self-employed",         "Job seeker",    "Student",
                 "Jobless",         "travail.6.1",         "Retired",  
                 "Prev. Lockdown",   
                 "Chronic medical pb.",
                 "Past psych.", "Current psych.",           
                 "Lockdown in usual acc.",    "Lockdown in hospital", 
                 "Agree w/ lockdown",             
                 "Satisfied w/ info", "Clarity of official info",
                 "Worry: PPE", "Worry: Essential products",
                 "Access to outdoor space",       
                 "Semi-urban","Rural",       
                 "Lockdown w/ family", "Pet",   
                 "COVID neg. & contact w/ others","COVID pos. or High risk",         
                 "Contacts: Face to face",    
                 "Contacts: Phone",     "Contacts: Text",     "Contacts: Soc. network",    
                 "Worry: Financial sit.",        "Worry: Precar.",  
                 "Cope: Medication","Cope: Words from people","Cope: Media","Cope: Positive beliefs",
                 "Cope: Science",      
                 "Cope: Religion","Cope: Esoteric","Cope: Resilience",      
                 "Cope: Collective","Cope: Benefits to planet",      
                 "Cope: Other people exp.","Cope: Benefits to indiv.", "Cope: Nil",
                 "Cope: Substance use",     
                 "Support: Family",   
                 "Support: Friends",    "Support: Colleagues",    "Support: Neighbors",
                 "Support: Under same roof",    "Support: Health/social prof.",   
                 "Support: Assoc.",    "Support: Other"   
                 , "dis.69", "dis.75", "dis.38", "dis.59","dis.92"
                 # "activites.SQ001.",    "activites.SQ002.",    "activites.SQ003.",   
                 # "activites.SQ004.",    "activites.SQ005.",    "activites.SQ006.",    "Sport",    "activites.SQ008.",   
                 # "activites.SQ009.",    "activites.SQ010.",    "activites.SQ011.",    "activites.SQ012.",    "activites.SQ013.",   
                 # "activites.SQ014.",    "Internet",    "activites.SQ016.",    "DIY",    "Medidate",   
                 # "Daydream",    "Anxious thoughts",    "Naps",    "Talk"   
)

Back_var=c("Sex: Female", "Age", "In a relationship", 
           "Pregnancy",
           "Educ: 12 to 14 y/","Educ: 14 y/ to Bach.","Educ: Bach. to Masters","Educ: Masters to PhD",              
           "Size of acc.", 
           "Employee",         "Self-employed",         "Student", "Retired",  
           "Prev. Lockdown",   
           "Chronic medical pb.",
           "Past psych.", "Current psych.",           
           "Access to outdoor space",       
           "Semi-urban","Rural",       
           "Pet",   
           "Contacts: Face to face",    
           "Contacts: Phone",     "Contacts: Text",     "Contacts: Soc. network",    
           "dis.69", "dis.75"
)

COVID_var= c(  "Time",     
                           "Lockdown in usual acc.",    "Lockdown in hospital", 
                           "Agree w/ lockdown",             
                           "Satisfied w/ info", "Clarity of official info",
                           "Worry: PPE", "Worry: Essential products",
                           "Access to outdoor space",       
                           "Semi-urban","Rural",       
                           "Lockdown w/ family", 
                           "COVID neg. & contact w/ others","COVID pos. or High risk",         
                           "Worry: Financial sit.",        "Worry: Precar."
                           
)


Coping_var=c("Cope: Medication","Cope: Words from people","Cope: Media","Cope: Positive beliefs",
"Cope: Science",      
"Cope: Religion","Cope: Esoteric","Cope: Resilience",      
"Cope: Collective","Cope: Benefits to planet",      
"Cope: Other people exp.","Cope: Benefits to indiv.", "Cope: Nil",
"Cope: Substance use",     
"Support: Family",   
"Support: Friends",    "Support: Colleagues",    "Support: Neighbors",
"Support: Under same roof",    "Support: Health/social prof.",   
"Support: Assoc.",    "Support: Other")


########## Not to be used in newer version of the script
# COVID_VAR <- c("Lockdown in usual acc.",    "Lockdown in hospital", 
#                "Agree w/ lockdown",             
#                "Satisfied w/ info", "Clarity of official info",
#                "Worry: PPE", "Worry: Essential products",
#                "Lockdown w/ family", 
#                "COVID - & contact w/ others","COVID + or High risk",         
#                "Worry: Financial sit.",        "Worry: Precar.",  
#                "Cope: Medication","Cope: Words from people","Cope: Media","Cope: Positive beliefs",
#                "Cope: Science",      
#                "Cope: Religion","Cope: Esoteric","Cope: Resilience",      
#                "Cope: Collective","Cope: Benefits to planet",      
#                "Cope: Other people exp.","Cope: Benefits to indiv.", "Cope: Nil",
#                "Cope- Substance use",     
#                "Support: Family",   
#                "Support: Friends",    "Support: Colleagues",    "Support: Neighbors",
#                "Support: Under same roof",    "Support: Health/social prof.",   
#                "Support: Assoc.",    "Support: Other"   
#                # "activites.SQ001.",    "activites.SQ002.",    "activites.SQ003.",   
#                # "activites.SQ004.",    "activites.SQ005.",    "activites.SQ006.",    "Sport",    "activites.SQ008.",   
#                # "activites.SQ009.",    "activites.SQ010.",    "activites.SQ011.",    "activites.SQ012.",    "activites.SQ013.",   
#                # "activites.SQ014.",    "Internet",    "activites.SQ016.",    "DIY",    "Medidate",   
#                # "Daydream",    "Anxious thoughts",    "Naps",    "Talk"   
# )
# 
# Day_COVID_VAR <- c("Day:Lockdown in usual acc.",    "Day:Lockdown in hospital", 
#                "Day:Agree w/ lockdown",             
#                "Day:Satisfied w/ info", "Day:Clarity of official info",
#                "Day:Worry:PPE", "Day:Worry: Essential products",
#                "Day:Lockdown w/ family", 
#                "Day:COVID - & contact w/ others","Day:COVID + or High risk",         
#                "Day:Worry: Financial sit.",        "Day:Worry: Precar.",  
#                "Day:Cope: Medication","Day:Cope: Words from people","Day:Cope: Media","Day:Cope: Positive beliefs",
#                "Day:Cope: Science",      
#                "Day:Cope: Religion","Day:Cope: Esoteric","Day:Cope: Resilience",      
#                "Day:Cope: Collective","Day:Cope: Benefits to planet",      
#                "Day:Cope: Other people exp.","Day:Cope: Benefits to indiv.", "Day:Cope: Nil",
#                "Day:Cope- Substance use",     
#                "Day:Support: Family",   
#                "Day:Support: Friends","Day:Support: Colleagues",    "Day:Support: Neighbors",
#                "Day:Support: Under same roof",    "Day:Support: Health/social prof.",   
#                "Day:Support: Assoc.",    "Day:Support: Other"      
#                # "activites.SQ001.",    "activites.SQ002.",    "activites.SQ003.",   
#                # "activites.SQ004.",    "activites.SQ005.",    "activites.SQ006.",    "Sport",    "activites.SQ008.",   
#                # "activites.SQ009.",    "activites.SQ010.",    "activites.SQ011.",    "activites.SQ012.",    "activites.SQ013.",   
#                # "activites.SQ014.",    "Internet",    "activites.SQ016.",    "DIY",    "Medidate",   
#                # "Daydream",    "Anxious thoughts",    "Naps",    "Talk"   
# )
###########################################

for (i in 1:length(rownames(all_coeff))) {
  
  #print(i)
  
  for (j in 1:length(VAR)) {
    
    if (grepl("info",VAR[j])) {
      all_coeff$param[i]=gsub(paste0("\\<",VAR[j],"\\>"),RENAMES_VAR[j], all_coeff$param[i])
    } else {all_coeff$param[i]=gsub(VAR[j],RENAMES_VAR[j],all_coeff$param[i])}
    
  }
}

# stop("halt")
# 
# for (i in 1:length(colnames(DM))) {
#   
#   for (j in 1:length(VAR)) {
#     
#     if (grepl("info",VAR[j])) {
#       names(DM)[i]=gsub(paste0("\\<",VAR[j],"\\>"),RENAMES_VAR[j], names(DM)[i])
#       } else {names(DM)[i]=gsub(VAR[j],RENAMES_VAR[j], names(DM)[i])}
# 
#   }
# }
#   
# for (i in 1:length(colnames(shap_values))) {
#   
#   for (j in 1:length(VAR)) {
#     if (grepl("info",VAR[j])) {
#       names(shap_values)[i]=gsub(paste0("\\<",VAR[j],"\\>"),RENAMES_VAR[j], names(shap_values)[i])
#     } else {names(shap_values)[i]=gsub(VAR[j],RENAMES_VAR[j], names(shap_values)[i])}
# 
#   }
# }
