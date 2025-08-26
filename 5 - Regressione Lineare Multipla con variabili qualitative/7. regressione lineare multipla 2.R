#ANALISI LINEARE MULTIPLA CON VARIABILI QUALITATIVE


#dati<-read.csv("stop smoking.csv",stringsAsFactors = T,sep=";") 


#dati$reddito <- rchisq(nrow(dati),20)
#set.seed(42)
#dati<-dati[sample(1:nrow(dati),150),]

#stringasAsFactors indica che cisono variabili qualitative,
#Quindil le variabili testo sono considetate dei fattori
dati<-read.csv("stopsmoking150.csv",stringsAsFactors = T,sep=";")


summary(dati)
attach(dati)
moments::skewness(peso)
moments::kurtosis(peso)-3
n <- nrow(dati) #salvo il numero di osservazioni

shapiro.test(peso) #valuto l'ipotesi di normalità della variabile risposta.
#non si rifiuta l'ìipotesi di normalità, pertanto la distribuzione è nomrale e il modello di regrezzione lineare multipla è adeguato

#VISUALIZZO MATRICE CORRELAZIONE TRA VARIABILE
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
#peso latezza correlazione alta e positivo anche nello scaterlpoy
#peso età vicno allo 0 correlazione bassa
#peso mesistop  il peso tende a diminuire, non è una correlazione eclatante
#reddito nessuna correlazione
#variabili qualitative non vanno ben gli scaterplot e anche le correlaizoni perdono di significato

#ANALISI CORRELAZIONI VARIABILI QUALITATIVE
par(mfrow=c(1,2))
boxplot(peso)
boxplot(peso~sesso) #distribuzoni condizionati sulla variabile sesso si vede che i maschi pesano di più

#valuto l'ipotesi di uguaglianza delle medie. p-value basso si rifiuta l'ipotesi nulla.
t.test(peso~sesso) #i valori di medie stimate sono significative

#analizzo la variabile sport
boxplot(peso)
boxplot(peso~sport)
t.test(peso~sport)

#MODELLO DI REGRESSIONE LINEARE MULTIPLA (LM)
#mod<-lm(peso~altezza+età+mesistop+sesso+sport+reddito)
mod1<-lm(peso~ ., data=dati)
summary(mod1)
#mesistop correlazione negativa, ogni mese che passa si perde peso
#sesso M parametro per maschi 8,19 indica che si trova un peso di +8,9kg rispetto le femmine
#sportSi per gli sportivi si rileva un decremento medio di 7.9 kg sempre tenendo invariate le altre variabili
#R-quadro del 76%, si può migliorare. Due variabili non significative, rddito ed età

#mod2<-lm(peso~altezza+mesistop+sesso+sport)
mod2<-update(mod1~ -reddito) #olgo reddito dal modello 1 e creo il modello
summary(mod2)

#Test per validare la modifica
anova(mod2,mod) #l'aumento di carianza spiegata aggiungendo il reddito non è significativo
BIC(mod,mod2) #modello 2 è preferibile ha BIC minore
car::vif(mod2) #valori bassi vicino a 1 , non si ha multilinearità


#analisi residui
par(mfrow=c(2,2))
plot(mod2)


lmtest::bptest(mod2)
lmtest::dwtest(mod2)
shapiro.test(mod2$residuals)
plot(density(residuals(mod2)))


#leverage
lev<-hatvalues(mod2)
plot(lev)
p<-sum(lev)
n<-length(lev)
soglia=2*p/n
abline(h=soglia,col=2)
lev[lev>soglia]

#outliers
plot(rstudent(mod2))
abline(h=c(-2,2))
car::outlierTest(mod2)

#distanza di cook
cook<-cooks.distance(mod2)
plot(cook,ylim = c(0,1)) 


summary(età)
età_cl<-cut(età,breaks=c(25,40,55,65))


mod3<-lm(peso~altezza+mesistop+sesso+sport+età_cl,data=dati)
summary(mod3)
BIC(mod2,mod3)

#RAPPRESENTO IL MODELLO CON UN GRAFICO -TENENDO IN CONSIDERAZIONE IL SESSO
library(ggplot2)
ggplot(data=dati)+
  geom_point(aes(x=mesistop,
               y=peso,
               col=sesso),position = "jitter")+ #raprresenta inmodo più realistico la variabile temporle
  geom_smooth(aes(x=mesistop,
                  y=peso,
                  col=sesso),se=F,method = "lm")
#relazioni tr i due gruppi è simile, ma la retta dei mashi è più in alto
#RAPPRESENTO IL MODELLO CON UN GRAFICO -TENENDO IN CONSIDERAZIONE IL SESSO
library(ggplot2)
ggplot(data=dati)+
  geom_point(aes(x=mesistop,
               y=peso,
               col=sport),position = "jitter")+ #raprresenta inmodo più realistico la variabile temporle
  geom_smooth(aes(x=mesistop,
                  y=peso,
                  col=spot),se=F,method = "lm")
  geom_smooth(aes(x=mesistop,
                  y=peso,
                  ),se=F,method = "lm",col=black)
#relazione tra peso è mesistop è più accentuata per chi non fa sport. Perchè chi fa sport è più in forma


  
