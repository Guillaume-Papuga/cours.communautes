#######################################################
# Project : Cours d'ecologie des communautés
# Script : 23.td_diversite.analyse_etu
# Découverte des différentes analyses d'écologie des communautés
# Authors : Guillaume Papuga
# Last update : 2 décembre 2021
#######################################################

#####
# 0. Upload data
#####
# Packages
require (entropart)
require (vegan)

# Data

# definir le fichier sur lequel on va travailler avec `setwd()` puis charger le fichier avec read.csv
# les arguments de read.csv
## chemin d'accès
## la première ligne contient les titres
## décimale = , & separateur = ;

tab = read.csv (here::here ("data", "raw", "data.oiseau_feu_td5.csv"), 
                header = TRUE, 
                dec = ",", sep = ";")

# la table n'est pas dans le bon sens (variables en ligne) : il faut reprendre les données
# on crée trois tables 
## les espèces

bird = tab [, c(4:ncol(tab))] # selectionne les colonnes
bird = t (bird) # retourne le tableau
colnames (bird) = tab$species # change les noms de colonne
bird = as.data.frame(bird) # transforme en dataframe, format plus agréable a manipuler

## les préférences alimentaires
guild = as.data.frame(tab$guild) # transforme en dataframe
colnames(guild) = "guild"

## la correspondance des noms français
noms_fr = tab[,1:2]

#####
# 1. Exploration des données
#####

str(bird) # structure du jeu de données "communauté"
summary(bird) # analyse des données par variables (espèce)
table(bird)

# Quel est la fréquence de chaque espèce dans le jdd?
bird10 = bird
bird10[bird10>0] = 1
colSums(bird10)

#####
# 2. Diversité alpha
#####

##
# a. Richesse taxonomique des relevés
##
# Travailler sur un relevé 
# Solution 1
rel = bird [4,] # selectionne la ligne 4
sum (rel != 0) # condition qui définit un vecteur TRUE/FALSE, et somme des TRUE

# Solution 2
sum(bird10 [4,]) # somme du tableau binaire


# Travailler sur un tableau : foction apply
# nécessite de définir une fonction
nb.sp = apply(bird,  # la matrice
              1, # operation a réaliser sur les lignes (si 2 : sur colonnes)
              function(c)sum(c!=0)) # la fonction 

nb.sp = colSums(bird != 0) # méthode alternative, avec une fonction dédiée

# On crée une table pour stocker les résultats

##
# b. Diversité taxonomique des relevés
##

# Simpson
div.


# Shannon

#####
# 3. Diversité beta
#####
 
##
# a. Données de présence-absence
##

# Jaccard

# Sorensen

##
# b. Données d'abondance
##

# Bray Curtis

# Distance euclidienne



#####
# 4. Approche fonctionnelle
#####

bird$year = c(rep (0, 5), # on ajoute une colonne pour l'année de suivi
              1:28)




