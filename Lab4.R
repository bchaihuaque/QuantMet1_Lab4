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

# Calculamos los cuatro clusters
k4<-kmeans(df, centers = 4, nstart = 25)
k4
str(k4)

#Plotear los clusters 
fviz_cluster(k4, data = df)
fviz_cluster(k4, data = df, ellipse.type = 'euclid', repel = TRUE, star.plot= TRUE)
fviz_cluster(k4, data = df, ellipse.type = 'norm')
