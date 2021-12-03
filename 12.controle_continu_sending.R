#######################################################
# Project : Cours d'ecologie des communautés
# Script : 12.controle_continu_sending
# Realiser un contrôle continu automatisé
# Authors : Guillaume Papuga
# Last update : 26 novembre 2021
#######################################################

#####
# 0. Upload data
#####
# Packages
library(tidyverse)
library(here)

# Student database
# std.data = 

# Subject characteristics
subj = read.csv(here::here("data", "cc1", "tab.sujets.csv"), 
                           header = 1, row.names = 1, sep = ";", dec = ",")


#####
# 4. Send to students
#####

