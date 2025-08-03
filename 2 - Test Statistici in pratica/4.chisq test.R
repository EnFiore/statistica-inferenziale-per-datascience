#tabella di frequenze congiunte
#i dati si riferiscono al numero di figli rispetto a livello di istruzione
# si crea la tabelal di contingeza
tabella <- matrix(data = c(45,22,32,12,
                           53,24,21,30,
                           24,65,40,3,
                           12,43,2,2,
                           20,13,7,0),
                  nrow = 5,
                  ncol = 4,
                  byrow = T)

#etichette di colonna e di riga
colnames(tabella)<- c("0","1","2","3+")

row.names(tabella) <- c("analfabeta",
                        "elementare",
                        "media",
                        "superiore",
                        "universitaria")
#per una rappresentazione grafica delle frequenze congiunte
#estensione ggplot pernette costruire baloon plot

install.packages("ggpubr") 
ggpubr::ggballoonplot(data=as.data.frame(tabella),
                      fill="blue")
#se ci sono pattern diagonali di capisce che c'è associzione tra le due variabili
#se le variabili sono indipendenti i palloncini dovrbbero essere tutti grandi uguali


#restituisce le distribuzioni marginali delle varabili, sono i totali dir gia e colonna
margin.table(tabella,1) #restituisce i totali di riga
margin.table(tabella,2) #restituisce i totali di colonna
margin.table(tabella) #restituisce il totale della tabella

##creo tabella delle frequenze attese
attese<- tabella

for(i in 1:nrow(tabella)){
  for(j in 1:ncol(tabella)){
    
    attese[i,j] <- margin.table(tabella,1)[i] * margin.table(tabella,2)[j] / margin.table(tabella)
    
  }
}

#calcolo della statistica test xquadro
osservate<-tabella
sum((osservate-attese)^2/attese) #calcolo x_quadro

#visualizzo una distribuzione chi quadro
plot(density(rchisq(1000000,df = 12)),xlim=c(0,130))
#visualizzo la linea retta del valore soglia corrispondente a un valore di significatività del 5% al 95%
abline(v=qchisq(0.95,12),col=2)
#calcolo valore della statistica test
points(123.89,0,cex=3,col=4)
#il valore è maggiore e quindi si rifiuta l'ipotesi nulla di indipendenza e 
#quindi c'è un'associazione statistica tra le due variabili

#funzione che calcola direttamente il chi_quadro
test.indipendenza<- chisq.test(tabella)
#il p-value è più piccolo di alfa e quinfi, lìipotsi nulla viene scartata

#calcola frequenxe attese
test.indipendenza$expected


#applichiamo il test chi quadro su altro dato
#colore occhi e sesso sono le variabili di cui si vuole verificare la dipendenza
dati<-HairEyeColor[1,,]
#alcuni colori degli occhi sono più rari, ma non diffeenza netta tra colore cohi femmine e maschi, 
#ma i pallini sono sempre grandi uguali
ggpubr::ggballoonplot(data=as.data.frame(dati),
                      fill="blue")
chisq.test(dati)
#il valore si 2.16 ha p value di 0.54 e quindi non si rifiuta l'ipotesi nulla e le varibil sono indipendenti


tabella <- as.table(tabella)

countsToCases <- function(x, countcol = "Freq") {
  # Get the row indices to pull from x
  idx <- rep.int(seq_len(nrow(x)), x[[countcol]])
  
  # Drop count column
  x[[countcol]] <- NULL
  
  # Get the rows from x
  x[idx, ]
}


dati_long <- countsToCases(as.data.frame(tabella))
colnames(dati_long)<-c("Istruzione","N.figli")
chisq.test( table(dati_long))

cor.test(as.numeric(dati_long$Istruzione),
    as.numeric(dati_long$N.figli),
    method = "kendall")

