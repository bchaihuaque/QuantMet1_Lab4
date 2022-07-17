#*****************************************************************************
##### Proyecto  : Laboratorio 4 - Cluster Analysis          ##################
#**** Autor     : Bruno Chaihuaque-Dueñas                   ******************
#**** Email     : bchaihuaque@pucp.pe                       ******************
#**** Fecha     : 15/07/2022                                ******************
#*****************************************************************************
# ***************************************************************************
# > Configuración inicial ====
# ***************************************************************************
# *  Limpiando y seteando espacio de trabajo ====
rm(list = ls())
Sys.getenv("USERNAME")
# https://stackoverflow.com/questions/10164719/find-windows-user-name-within-r
if (Sys.getenv("USERNAME") == "bchai"){ 
  setwd("D:/Doctorado/Semestre I/Métodos Cuantitativos 1/Lab4")
}else if (Sys.getenv("USERNAME") == "bruno"){
  setwd("/home/bruno/Doctorado/QuantMet1/Lab4")
} else {
  print("Configurar directorio de trabajo")
}
system("java -version")
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,"Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("tidyverse", "cluster", "factoextra", "NbClust", "haven")
ipak(packages)

df<-read_sav("Facebook Use.sav")
# Calcular la matriz de distancias
m.distancias<-get_dist(df, method = "euclidean")
fviz_dist(m.distancias, gradient=list(low='blue', mid='white', high='red'))

#Estimar el número de clusters
#Elbow, silhoutte o gap_stat method
fviz_nbclust(df, kmeans, method = 'wss')
fviz_nbclust(df, kmeans, method = 'silhouette')

#con esta función se pueden calcular:
#the index to be calculated. This should be one of : "kl", "ch", "hartigan", "ccc", "scott",
#"marriot", "trcovw", "tracew", "friedman", "rubin", "cindex", "db", "silhouette", "duda",
#"pseudot2", "beale", "ratkowsky", "ball", "ptbiserial", "gap", "frey", "mcclain", "gamma",
#"gplus", "tau", "dunn", "hubert", "sdindex", "dindex", "sdbw", "all" (all indices except GAP,
#Gamma, Gplus and Tau), "alllong" (all indices with Gap, Gamma, Gplus and Tau included).
resnumclust<-NbClust(df, distance = "euclidean", min.nc=2, max.nc=10, method = "kmeans", index = "alllong")
fviz_nbclust(resnumclust)

# Calculamos los cuatro clusters
k4<-kmeans(df, centers = 4, nstart = 25)
k4
str(k4)

k3<-kmeans(df, centers = 3, nstart = 25)

k2<-kmeans(df, centers = 2, nstart = 25)

#Plotear los clusters 
fviz_cluster(k4, data = df)
fviz_cluster(k4, data = df, ellipse.type = 'euclid', repel = TRUE, star.plot= TRUE)
fviz_cluster(k4, data = df, ellipse.type = 'norm')

# con 3 clusters
fviz_cluster(k3, data = df)

# con 2 clusters
fviz_cluster(k2, data = df)
