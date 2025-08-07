dati<-read.csv("energy output.csv")
attach(dati)
summary(dati)
n<-nrow(dati) #numero di righe del dataset

#Verifico che la variabile risposta segua distribuzione normale
moments::skewness(Energy.output) #leggermente positiva, ma è simmetrica
moments::kurtosis(Energy.output)-3 #coefficiente negativo la ditribuzione è platicurtica
shapiro.test(Energy.output) # p-value di 0.08, non si rifiuta l'ipotesi di normalità

#matrice di correlazione per vedere correlazioni tra vraiabili
round(cor(dati),2) # matrice simmetrica, 
#le variabili molto correlate alla varibile riposta sono quelel che portano maggiore informazione
#temperatura e vapori scarico sono le più correlate. Per le altre variabili la corelzione non è chiara.
#Ttra i regressori non si vedono correlazioni molto elevate. Tranne per 0,84 tra vacum e temperature, ma che si nalizza dopo

#visulizzazine corelazioni con funzione pairs
?pairs #documentazione con blocco coidice per rappreentazione grafica:
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- (cor(x, y))
  txt <- format(c(r, 1), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = 1.5)
}
#correlazioni
pairs(dati,lower.panel=panel.cor, upper.panel=panel.smooth)

#stimiamo un modello lineare con tutte le variabili
mod1<-lm(Energy.output~Humidity+Pressure+Temperature+Vacuum, #indico tutte le variabili come somma
        data=dati)
summary(mod1) #èer valutare il modello. Tabella dei coefficienti rappresenta gli effetti marginali sulla variabile 
#ripsota. Temperatura ha effetto negativo, e per ogni grado di temperatura in più, siha una diminuzione della profuzione di 2,1kw
#p-value elevato indica nessun effetto sulla variabile rispoata come per la PRessure
#r quadro di 0,90 indica un modello buono, ma di deve guardare R-quadro aggiustato.

#verifico altri modelli togliendo le varibili una per volta - procedura step ways
mod2<-update(mod1,~.-Pressure) #dato che la pressione è molto non significativa,la si toglie
summary(mod2)
#rimane quasi tutto invariato e la significatività co umidità e temperatura molto significative e con vacum al limite.
#indica che Pressure è una variabile poco utile

anova(mod2,mod1)

#dato che si era visto un effetto non lineare nel grafico si aggiunge +I(Vacuum^2)
mod3 <-update(mod1,~.+I(Vacuum^2))
#visulizziamo la relazione di vacum con la temperatura
plot(Vacum,Energyoutput,pch=20)
summary(mod3)
#+I(Vacuum^2) annulla l'effeto negativo di vacum effetto di secondo grado, ma p-value al limite
#r quadro aggiustato indica che il modello non ha dato un contributo rilevante

#dato che modelli meno complessi osno preferibili a medlli complessi, si toglie vacum
mod4 <- lm(Energy.output~Humidity+Temperature,data=dati)
summary(mod4)
#due variabili molto significatice

#vedere se variabili hanno effetto congiunto
mod.interaz <- lm(Energy.output~Humidity*Temperature)
summary(mod.interaz)
#la varibile umidità diventa non siginificativa, quindi non si deve aggiungere interazione tra le variabili

#analisi anova o test-f per capire se una variabile è utile, si ocnfronanto i due modelli con e senza interazione
anova(mod.interaz,mod4)
#p-value si 0,2198 indica che aggiungere l'interazione non aumenta significatività e quindi si preferisce
#modello più semplice senza interazioni


#modelli di informazione. GLi indici vanno conforntati con altri modelli. I modelli con valori più bassi
#sono i migliori
AIC(mod,mod2,mod3,mod4,mod.interaz)
BIC(mod,mod2,mod3,mod4,mod.interaz)
#L'AIC tende a prendere modelli più parametrizati, BIC invece tende a premiare modelli con meno parametri

install.packages("car")
car::vif(mod4) #indicatori di multilinearità che devono essere sotto 5. Nessun problema

#funzione che fa la procedura step wise per valutare il modello
#si passa il modello con tutte le varaibili - full model
stepwise.mod <- MASS::stepAIC(mod,
              direction = "both", #si indica la direzione della prcedura
              k=log(n)) #si indica il criterio da utilizzare di default AIC k= 2, per imporatere BIC k=log(n)
summary(stepwise.mod) #resituisce il modello 4.

#analisi residui la parte erratica, devono rispettare le regole
par(mfrow=c(2,2)) #divide la finestra grafica in 4
plot(mod4)
#i grafici indicano 1 i punti devono essere sparsi casualmente intorno la medi adi zero. Pattern ricurvo
3relazione residui e i quantili di una distribuzione noramle. I punti seguono la retta = distribuzione normale
#, tranne la paret inferioe 3 - non si devono vedere pattern per avere varianza costante
#4 valori influenti o eprchè levarege o outline. Sogli 0,25 è di avvertimento, soglia a 1 è allarme. nessun punto supera
#a,25 non ci dovrebbero essere problemi di valori influenti

#leverage
lev<-hatvalues(mod4)
plot(lev)
# valore soglia
p<-sum(lev)
n<-length(lev)
soglia=2*p/n
abline(h=soglia,col=2) #aggiungo linea la grafico
lev[lev>soglia] #estraggo valori oltre soglia
#Sono i leverage, sono le osservazioni, che si trovano lontane nello spazio dei regressori


#outliers - calori estremi delle a varaibile risposta
plot(rstudent(mod4)) #si usa t-student
abline(h=c(-2,2))
car::outlierTest(mod4) #estreae le osservaizoni outlier


#distanza di cook
cook<-cooks.distance(mod4)  #valuta sia outlier che leverage
plot(cook,ylim = c(0,1)) 
max(cook) #distanza di cook max = 0.23. che però non si avvicina alla soglia di 0.25. osservazione
#non ha influenza sul modello sulle stime di regressione
summary(mod4)

#test sui residui
lmtest::bptest(mod4) #omoschelarità varianza costa non si rifiuta lì'ipotesi nulla
lmtest::dwtest(mod4) #darwin watson i residui non sono auto correlate, non si rifiuta l'ipotesi nulla
shapiro.test(mod4$residuals) #per normalità, si rifiutà lìipotesi nulla di normalità. Non è una dstribuzione normale
plot(density(residuals(mod4)))

car::crPlots(mod4)

mod5<-lm(lm(Energy.output~Humidity+I(Humidity^2)+Temperature,data=dati))
summary(mod5)
#NO


mod_76 <- lm(Energy.output~Humidity+Temperature,data=dati[-76,])
summary(mod_76)


car::scatter3d(Energy.output~Humidity+Temperature)


MSE<-function(y_oss,y_prev){
  return(sum((y_oss-y_prev)^2)/length(y_prev))
}


mse_train<-MSE(dati$Energy.output, fitted(mod4))
mean(mod4$residuals^2)
deviance(mod4)/n

dati.test <- read.csv("energy.test.csv")
oss.test <- dati.test$Energy.output
prev.test<- predict(mod4,newdata = dati.test)
mse_test<-MSE(oss.test,prev.test)

mse_train;mse_test

plot(oss.test,prev.test)
abline(a=0,b=1)



