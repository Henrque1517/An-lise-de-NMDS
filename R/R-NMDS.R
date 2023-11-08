#NMDS - Analise de composiçao de especies e ambientes 

install.packages("here")
library(here)
library(vegan)


ictio <- read.csv2(here("data", "Ictio.csv"),h=T,sep = ";",check.names=F)

Ictio.env <- read.csv2(here("data", "Ictio_env.csv"),h=T,sep = ";",check.names=F)


ictio.envi<-Ictio.env[, 1:4]

ictio.abund<-ictio[,5:17]


#Rode a analise 


NMDS <- metaMDS(ictio.abund, distance = "bray") # exsite outros como bray curtis para abundancia ou manhatan

help("metaMDS") #para ver qual distancia e a melhor


#verificar o nivel de stress >0,3 e um bom indicador pois implica na robuster 
#da sua analise

NMDS$stress


#Analise estatistica


Permanova<- adonis2(ictio.abund ~ Ictio.env$Ambiente, data = ictio.envi, by=NULL)

summary(Permanova)


#Plota o grafico ordinario 

plot(NMDS, type = "p")

#data visualization

col=c("red", "blue")

shape=c(18,16)

plot(NMDS$points, main = "Composição das especies")

ordispider(NMDS, groups = Ictio.env$Ambiente, label = F)


#Legenda 

txt<- c("Lêntico", "Lótico")
legend('topleft',txt,col = c("red", "blue"), bty="y", pch=c (18,16))

