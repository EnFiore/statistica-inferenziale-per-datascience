
dati<-read.csv("energy output.csv")
attach(dati)

#Costruisco scaterplot
plot(Temperature,Pressure)

#calcolo covarianza a mano
covarianza <- sum( (Temperature-mean(Temperature))*(Pressure-mean(Pressure)) )/(length(Temperature)-1)
covarianza
#misuro la covarianza
cov(Temperature,Pressure)

#calcolo coefficiente di correlazione lineare di pearson
pearson.rho <- cov(Temperature,Pressure)/ (sd(Temperature)*sd(Pressure))
pearson.rho
#fuznione nativa di R
cor(Temperature,Pressure)
#il valore è negativo, si ha una correlaizone negativa e il valore -0.56 indica una media correlaizone

plot(Temperature,Vacuum)
cov(Temperature,Vacuum)
cor(Temperature,Vacuum)
#i coefficienti di correlazione (cor) delle due variabili pressione e cvacuum sono confrontabili

library(tidyverse)

dati<-datasauRus::datasaurus_dozen

filter(dati,dataset!="x_shape") %>% 
  group_by(dataset) %>% 
  summarise(muX=mean(x),
            muY=mean(y),
            rhoXY= cor(x,y))



ggplot(data=filter(dati,dataset!="x_shape"))+
  geom_point(aes(x=x,y=y))+
  facet_wrap(~dataset,
             nrow = 3,
             ncol = 4)




#non parametriche

dati <- read.csv("gare.csv",sep=";")
dati
#calcolo le posizioni in classiffica
dati$pos.100m<- rank(dati$m100)
dati$pos.maratona<- rank(dati$Maratona)
dati
#faccio la differenza tra le posizoni delle due classifiche
dati$diff<-dati$pos.100m - dati$pos.maratona

n=dim(dati)[1]
  
cor.spearman <- 1 - 6 * (sum((dati$diff)^2) / (n*(n^2-1)))
#correlazione leggermente negativa, bassa congrduazione le due classifiche non sono correlate

#funzone per calcolo corrlazioni non parametriche
cor(dati$m100,dati$Maratona,method = "spearman")
cor(dati$m100,dati$Maratona,method = "kendall")
