#######################################################
# Project : Cours d'ecologie des communautés
# Script : 10.controle_continu_altered.datasets
# Realiser un contrôle continu automatisé
# Authors : Guillaume Papuga
# Last update : 26 novembre 2021
#######################################################

#####
# 0. Upload packges
#####
# Packages
library(tidyverse)
library(here)
library(stringr)
library(officer)


#####
# 1. Altering datasets
#####
# Loop
# list of all datasets
lst.subj = list.files(here::here("data", "cc1", "2023", "raw.datasets"))

for (i in lst.subj) {
  ### Upload the dataset
  mat1 = read.csv(file = here::here("data", "cc1", "2023", "raw.datasets", i), 
                 header = T, row.names = 1)
  mat= mat1
  ############################# 1. Changer 10 decimal au hasard ##########################
  lst.mistake = sample(1:60, 10, replace = FALSE) # choisit lignes valeurs à modifier
  
  mat [,4] = as.character(mat[,4]) # transforme la colonne en caractère
  
  mat [lst.mistake,4] = str_replace_all(mat[lst.mistake,4], # subset du tableau
                                        "[.]" , # cherche les .
                                        ",") # remplace par des virgules
  
  ############################# 3. Facteur 100 sur var.cont 2 (tout  un site) ############
  selec_site = sample(c("a", "b", "c", "d", "e", "f"), 1) # selectionne un site au hasard
  mat[which(mat$site==selec_site), "temperature"] = mat[which(mat$site==selec_site), "temperature"]*100 # multiplie la valeur par 100
  
  ############################# 4. Enlever les 0 #########################################
  mat [mat == 0] = "" # remplacer tous les 0 par des cellules vides
  
  ############################# 5. Passer des plots en majuscule #########################
  maj.count = sample(1:60, 10, replace = FALSE) # définit 10 plots aléatoirement
  
  mat$site = as.character(mat$site) # transforme de facteur en caractère pour s'astreindre du nombre de modalités
  mat[maj.count, "site"] = str_to_upper(as.vector(mat[maj.count, "site"])) # passe en majuscule
  
  ############################# 6. Mettre des majuscules aux oui/non #####################
  maj.count = sample(1:60, 5, replace = FALSE) # définit 10 plots aléatoirement
  
  mat[,3] = as.character(mat[,3]) # transforme de facteur en caractère pour s'astreindre du nombre de modalités
  mat[maj.count, 3] = str_to_upper(as.vector(mat[maj.count, 3]))
  
  ############################# 6. Mettre des espaces aux oui-non ########################
  rand.site = sample(c("a", "b", "c", "d", "e", "f"), 1, replace = FALSE) # définit 1 site aléatirement
  
  mat[mat$site==rand.site,3] = paste (mat[mat$site==rand.site,3], " ", sep = "") # selectionne la colonne traitement et ajoute un espace
  
  ############################# 8. Ajouter les unités de quelques températures ###########
  add_unit = sample(1:60, 15, replace = FALSE) # définit une seule fois l'aléatoire
  mat[add_unit, "temperature"] = paste (mat[add_unit, "temperature"], "°", sep = "") # selectionne la colonne temperature et ajoute un symbole degré
  mat
  ############################# 9. Séparer en 2 jeux de données ##########################
  # Séparer
  mat_var = mat[, c(1:5)] %>%
    arrange(site) # trie dans l'ordre des sites
  mat_sp = mat[, c("code", "sp1", "sp2", "sp3", "sp4", "sp5", "sp6", "sp7", "sp8")]%>%
    arrange(sp6) # trie dans un ordre quelconque = esp5
  
  # Enregistrer en Word
  doc_table <- read_docx (path = here::here("data", "cc1", "template.docx")) %>% # charge le template
    body_add_table(mat_sp, style = NULL, first_column = TRUE) %>% # ajoute la table espèces
    body_add_break() %>% # saute une ligne
    body_add_table(mat_var, style = NULL, first_column = TRUE) # ajoute la table variables
  
  print(doc_table, target = here::here("data", "cc1", "2023", "altered.datasets", 
                                       paste ("data_etud_", unlist(strsplit(i, split = "[.]"))[1], ".docx", sep = "")))
  
} ## Loop ends here
