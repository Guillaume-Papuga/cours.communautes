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


#####
# 1. Altering datasets
#####
# Loop
# list of all datasets
lst.subj = list.files(here::here("data", "cc1", "2022", "raw.datasets"))

# for (i in lst.subj) {}

### Upload the dataset
mat = read.csv(file = here::here("data", "cc1", "2022", "raw.datasets", i), 
               header = T, row.names = 1)

############################# 1. Changer 10 decimal au hasard ##########################
############################# 2. Facteur 10 sur var.cont 1 #############################
############################# 3. Facteur 100 sur var.cont 2 (tout  un site) ############
############################# 4. Enlever les 0 #########################################
############################# 5. Passer en majuscule un site ###########################
############################# 6. Mettre des majuscules / espaces au oui-non ############
############################# 7. Sauter des lignes #####################################
############################# 8. Modifier les entêtes (ajouter les unités) #############
############################# 9. Séparer en 2 jeux de données ##########################

# Séparer
# Trier

### Save as Word file

