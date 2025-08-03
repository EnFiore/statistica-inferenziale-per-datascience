dati<-read.csv("energy output.csv")
attach(dati)
summary(dati)
#centrale termoelettrica con sensori che misurano le variabili ambientali intorno alla centrale
#energy in megawatt
#temperatura in gradi
#pressione in millibar
#umidità relativa e vapori di scarico


plot(Temperature,Energy.output)
#coefficiente di relazione
cor(Temperature,Energy.output)
#negativo e vicino a 1. Glia umenti di temperatura sfavoriscono la produzione di energia

b1<- cov(Temperature,Energy.output)/var(Temperature) #coefficiente di regressione è negativo
b0<- mean(Energy.output)-b1*mean(Temperature)
b0;b1  

#funzione che crea un modello lineare
#dà la possibilità di indagare 12 caratteristiche
mod_lin<-lm(Energy.output~Temperature, data=dati)  #[-76,]

#analizzo i coeefficienti
mod_lin$coefficients

#mi sitentizza tutte le info del modello
#p value piccolo indica che l'efetto di decremento è statisticamente rillevante
summary(mod_lin)
#aggiunfo la linea al scaterplot
abline(mod_lin,col=2)


#CONTROLLO BONTA' MODELLO
#divido in due la visualizazione
par(mfrow=c(1,2)) #1 riga due colonne

#rapresento i residui e l'indice delle osservazioni da 0 a 100
plot(residuals(mod_lin))
abline(h=mean(residuals(mod_lin)))
#osservo distribuzione dei residui
#la distribuzione ha una coda dovuti dagli outliner
plot(density(residuals(mod_lin)))

#test che permette di capire se una funzione è distribuita secondo una normale
#p-value è piccolo si rifiuta l'ipotesi di nromalità

#install.packages("lmtest")
library(lmtest)

shapiro.test(mod_lin$residuals)
#controllo omoschedadisticità e di non correlazione
#p-value piccoli del valore soglia, si rifiuta ipotesi nulla
lmtest::bptest(mod_lin) 
lmtest::dwtest(mod_lin) #correlazione dei residui


which.min(Energy.output)

#utilizzo il modello per fare previsione, con una temperatura di 40
predict(mod_lin,data.frame(Temperature=40))



plot(Pressure,Energy.output)
cor(Pressure,Energy.output)
mod_lin2<-lm(Energy.output~Pressure, data=dati)  #[-76,]
#fai vedere con dollaro

summary(mod_lin2)
abline(mod_lin2,col=2)

#par(mfrow=c(1,2))
plot(residuals(mod_lin2))
abline(h=mean(residuals(mod_lin2)))



plot(density(residuals(mod_lin2)))

shapiro.test(mod_lin2$residuals)
lmtest::bptest(mod_lin2)
lmtest::dwtest(mod_lin2)



predict(mod_lin2,data.frame(Pressure=1000))
