#######################################################
# Project : Cours d'ecologie des communautés
# Script : 10.controle_continu_raw.datasets
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
                           header = T, row.names = 1, sep = ",", dec = ".")

#####
# 2. Generating datasets
#####

### Start the loop
# nb.dataset = length(unique(std.data$equipe))

for (i in 1:nrow(subj)){
  ### Generate
  # Submatrix for site 1
  study = subj[i,]
  
  # Empty matrix
  mat = as.data.frame(matrix (data = NA,
                              nrow = 60,
                              ncol = 0))
  
  ############################# 1. sites #############################
  # Creaton des sites randoms
  site = c(as.character (rep (study$zones1, 10)),
           as.character (rep (study$zones2, 10)),
           as.character (rep (study$zones3, 10)),
           as.character (rep (study$zones4, 10)),
           as.character (rep (study$zones5, 10)),
           as.character (rep (study$zones6, 10)))
  
  code = paste (site, rep(1:10, 6), sep = "_") # crée un code par plot

  mat = cbind (mat, site, code)
  
  ############################# 2. traitement ##########################
  traitement = matrix(ncol = 1, nrow = 60, data = c(c(rep (1, 10), # generate the ordering sequence
                                                      rep (2, 10), 
                                                      rep (3, 10), 
                                                      rep (4, 10), 
                                                      rep (5, 10), 
                                                      rep (6, 10))))
  
  attrib.tr = sample(1:6, 6, replace = FALSE) # generate a random vector of order
  
  traitement = as.data.frame(traitement) %>%
    arrange (factor(traitement[,1], levels = attrib.tr)) %>% # create a random order
    mutate (value = as.factor(c(rep("oui", 30),  # generate the 1-0 data
                                rep("non",30)))) %>%
    arrange (desc(V1)) %>% # reorder
    select (-V1) # delete the order columns
  
  names(traitement) = as.character(study$traitement) # automatically change the name
  mat = cbind(mat, traitement)
  
  ############################# 3. Variable continue 1 #############################
  var.c = as.data.frame(round(rnorm(n = 60, 
                                    mean = study$var.cont_val, 
                                    sd = study$var.cont_val*0.10), 2)) # 10% de variance autour de la moyenne
  names(var.c) = as.character(study$var.cont_name)
  mat = cbind(mat, var.c)
  
  ############################# 4. Variable continue 2 : température ###############
  # generate a random trend
  temp.dens = dnorm(seq(1, 60, by = 1), # sequence all data from 1 to 60
                    mean = sample(1:60, 1), # chose one value as the peak
                    sd = sample(1:15, 1)) # define a more or less large effect
  temp.trend =  temp.dens * (study$temp_val*0.2 / max(temp.dens)) # max de la tendance à 20% de la moyenne

    # generate some noise
  temp.noise = as.data.frame(rnorm(n = 60,
                                   mean = study$temp_val, # valeur moyenne du tableau
                                   sd = study$temp_val*0.05)) # ecart type de 5% autour de la moyenne
  
  # combine the two series
  temp = as.data.frame(round(temp.trend + temp.noise[,1], 2))
  names(temp) = "temperature"
  mat = cbind(mat, temp)

  ############################# 5. Les espèces #############################
  ######## sp1 ########
  # sp1 > pic d'abondance sur une serie
  mean_ab.sp1 = study$abun.moy * (runif(1, min=0.1, max=1)) # abondance moyenne de l'espece entre 10 et 150%
  
  # generate a trend
  sp1.dens = dnorm(seq(1, 60, by = 1), # sequence all data from 1 to 60
                   mean = sample(1:60, 1), # chose one value as the peak
                   sd = sample(1:15, 1)) # define a more or less large effect
  sp1.trend =  sp1.dens * (mean_ab.sp1*0.40 / max(sp1.dens)) # transform the density into a realistic value (la valeur max s'écarte de 40% de la moyenne)
  
  # generate some noise
  sp1.noise = as.data.frame(rnorm(n = 60,
                                  mean = mean_ab.sp1, 
                                  sd = mean_ab.sp1*0.1)) # ecart type de 10% de la moyenne
  
  # combine the two series
  sp1 = as.data.frame(round(abs(sp1.trend + sp1.noise[,1]), 0))
  names(sp1) = "sp1"
  
  ######## sp2 ########
  # sp2 > random rare
  mean_ab.sp2 = study$abun.moy * (runif(1, min=0.01, max=0.2)) # abondance moyenne de l'espece à 10-20% de la valeur moyenne
  
  # generate some noise
  sp2 = as.data.frame(round(abs(rnorm(n = 60,
                                      mean = mean_ab.sp2, 
                                      sd = mean_ab.sp2*0.5)), 0))
  # combine the two series
  names(sp2) = "sp2"
  
  # remplace des valeurs par 0 
  spot_0 = sample(1:60, 15, replace = FALSE)
  sp2$sp2[spot_0]=0
  
  ######## sp3 ########
  # sp3 > negative linear trend
  mean_ab.sp3 = study$abun.moy * (runif(1, min=0.1, max=1.2)) # definit abondance moyenne de l'espece
  
  # generate a trend
  slp = -0.15 # pente de la tendance
  sp3.trend = seq((1-slp):(1+slp), length.out = 60, by = ((2 * slp)/(60 - 1)))
  
  # generate some noise
  sp3.noise = as.data.frame(rnorm(n = 60,
                                  mean = mean_ab.sp3, 
                                  sd = mean_ab.sp3*0.05))
  
  # combine the two series
  sp3 = as.data.frame(round(abs(sp3.trend * sp3.noise[,1]), 0))
  names(sp3) = "sp3"
  
  ######## sp4 ########
  # sp4 > pic d'abondance sur une serie
  mean_ab.sp4 = study$abun.moy * (runif(1, min=0.5, max=0.8)) # abondance moyenne de l'espece entre 50 et 80% de l'abondance cible
  
  # generate a trend
  sp4.dens = dnorm(seq(1, 60, by = 1), # sequence all data from 1 to 60
                   mean = sample(1:60, 1), # chose one value as the peak
                   sd = sample(1:15, 1)) # define a more or less large effect
  sp4.trend =  sp4.dens * (mean_ab.sp4*0.30 / max(sp4.dens)) # transform the density into a realistic value (la valeur max s'écarte de 30% de la moyenne)
  
  # generate some noise
  sp4.noise = as.data.frame(rnorm(n = 60,
                                  mean = mean_ab.sp4, 
                                  sd = mean_ab.sp4*0.05))
  
  # combine the two series
  sp4 = as.data.frame(round(abs(sp4.trend + sp4.noise[,1]), 0))
  names(sp4) = "sp4"
  
  ######## sp5 ########
  # sp5 > random rare
  mean_ab.sp5 = study$abun.moy * (runif(1, min=0.005, max=0.2)) # abondance moyenne de l'espece
  
  # generate some noise
  sp5 = as.data.frame(round(abs(rnorm(n = 60,
                                      mean = mean_ab.sp5, 
                                      sd = mean_ab.sp5*0.5)), 0)) # espèce rare donne des valeurs négatives, supprimer par ABS
  
  
  # combine the two series
  names(sp5) = "sp5"
  
  # remplace des valeurs par 0 
  spot_0 = sample(1:60, 8, replace = FALSE)
  sp5$sp5[spot_0]=0
  
  ######## sp6 ########
  # sp6 > positive linear trend
  mean_ab.sp6 = study$abun.moy * (runif(1, min=0.1, max=1)) # definit abondance moyenne de l'espece
  
  # generate a trend
  slp = 0.1
  sp6.trend = seq((1-slp):(1+slp), length.out = 60, by = ((2 * slp)/(60 - 1)))
  
  # generate some noise
  sp6.noise = as.data.frame(rnorm(n = 60,
                                  mean = mean_ab.sp6, 
                                  sd = mean_ab.sp6*0.05))
  
  # combine the two series
  sp6 = as.data.frame(round(abs(sp6.trend * sp6.noise[,1]), 0))
  names(sp6) = "sp6"
  
  ######## sp7 ########
  # sp7 > random rare
  mean_ab.sp7 = study$abun.moy * (runif(1, min=0.1, max=0.3)) # abondance moyenne de l'espece
  
  # generate some noise
  sp7 = as.data.frame(round(abs(rnorm(n = 60,
                                      mean = mean_ab.sp7, 
                                      sd = mean_ab.sp7*0.5)), 0)) # espèce rare donne des valeurs négatives, supprimer par un ABS
  
  # combine the two series
  names(sp7) = "sp7"
  
  # remplace des valeurs par 0 
  spot_0 = sample(1:60, 30, replace = FALSE)
  sp7$sp7[spot_0]=0
  
  ######## sp8 ########
  # sp8 > random rare
  mean_ab.sp8 = study$abun.moy * (runif(1, min=0.3, max=0.4)) # abondance moyenne de l'espece
  mean_ab.sp8
  
  # generate some noise
  sp8 = as.data.frame(round(abs(rnorm(n = 60,
                                      mean = mean_ab.sp8, 
                                      sd = mean_ab.sp8*0.5)), 0)) # espèce rare donne des valeurs négatives, supprimer par un ABS
  
  # combine the two series
  names(sp8) = "sp8"
  
  # remplace des valeurs par 0 
  spot_0 = sample(1:60, 5, replace = FALSE)
  sp8$sp8[spot_0]=0
  
  ############################## Synthese #############################
  mat.complet = mat %>%
    arrange(mat[,3]) %>% # use the second column to sort the dataset before pasting the species (and create trends within the datasets)
    mutate(sp1, sp2, sp3, sp4, sp5, sp6, sp7, sp8) %>%
    arrange(site)
   ############################# Save as CSV file ########################
  write.csv(x = mat.complet,
            file = here::here("data", "cc1", "2023", "raw.datasets", 
                              paste("2023_", row.names(study), ".csv", sep = "")))
  
} ## loop end here


