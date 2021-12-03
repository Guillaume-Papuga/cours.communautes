#######################################################
# Project : Cours d'ecologie des communautés
# Script : 20.TD_alpha
# Figures de cours
# Authors : Guillaume Papuga
# Last update : 29 novembre 2021
#######################################################

#####
# 0. Upload data
#####
# Packages
require (entropart)
require (vegan)


#####
# 1. Simul alpha
#####
### Communautés de base
com.a = c(10,10,10,10,10,10) # communauté parfaitement équilibrée
com.b = c(55,1,1,1,1,1) # communaté la plus déséquilibrée
com.c = c(30,15,8,4,2,1) # communauté intermédiaire

# Démonstration de l'équation de base de Simpson
div =  com.a[1]/sum(com.a) * (com.a[1]-1)/(sum(com.a)-1) + 
       com.a[2]/sum(com.a) * (com.a[2]-1)/(sum(com.a)-1) +  
       com.a[3]/sum(com.a) * (com.a[3]-1)/(sum(com.a)-1) + 
       com.a[4]/sum(com.a) * (com.a[4]-1)/(sum(com.a)-1) + 
       com.a[5]/sum(com.a) * (com.a[5]-1)/(sum(com.a)-1) + 
       com.a[6]/sum(com.a) * (com.a[6]-1)/(sum(com.a)-1) 
1-div

# Deux manières de le calculer
div.com.a = Simpson(com.a) # calcul simple
diversity(com.a, index = "simpson") # extrapolation à la population

#####
# Courbe Aire - Espèce
#####
# Construire la communauté comme un vecteur
com.a
com.a.ind = c(rep("esp1", com.a[1]), 
              rep("esp2", com.a[2]), 
              rep("esp3", com.a[3]), 
              rep("esp4", com.a[4]), 
              rep("esp5", com.a[5]), 
              rep("esp6", com.a[6]))

# Coordonnées
x = round(runif(30, min=1, max=8),0)
y = round(runif(30, min=1, max=8),0)
case = round(runif(30, min=1, max=64),0)

# Matrice
mat.spat = as.data.frame(cbind(com.a.ind, case))

# Boucle
mat.spat = mat.spat %>%
  mutate (ord = sample(1:100, 60, replace = FALSE)) %>%
  arrange(ord) 
  
mat.spat = mat.spat %>%
  arrange(case) 


#####
# 2. Sensibilité de Simpson à la taille d'échantillon
#####



#####
# 3. Sensibilité de Simpson à la richesse
#####

# Crée une matrice de stockage des résultats
nb_run = 400
n_esp_rare = 0:nb_run

# Com.a
div.com.a = Simpson(com.a)
com.a.test = com.a

# Com.b
div.com.b = Simpson(com.b)
com.b.test = com.b

# Com.c
div.com.c = Simpson(com.c)
com.c.test = com.c

# boucle
for (i in 1:nb_run){
  # com a
  com.a.test = c(com.a.test, 1)
  div.com.a = c(div.com.a, Simpson(com.a.test))
  
  # com b
  com.b.test = c(com.b.test, 1)
  div.com.b = c(div.com.b, Simpson(com.b.test))
  
  # com c
  com.c.test = c(com.c.test, 1)
  div.com.c = c(div.com.c, Simpson(com.c.test))                         
}


# Figure
plot(div.com.a~n_esp_rare, 
     type = "l", col = "blue",
     xlab = "Nombre d'espèces rares", 
     ylab = "Diversité de Simpson", 
     ylim = c(0.2, 1))

lines(div.com.b~n_esp_rare, col = "red")
lines(div.com.c~n_esp_rare, col = "green")

