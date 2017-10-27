library(readxl)
Dati <- read_excel("C:/Users/marco/Desktop/Econometria/Elaborati.xlsx")
View(Dati)
library(plm)
library(foreign)
library(lattice)
library(lme4)
library(scales)
fix(Dati)
Dati<-plm.data(Dati,index=c("Country","year"))
plot(density(Dati$ret))
retAus<-subset(Dati$ret,Dati$Country==1)
plot(density(retAus))
retChi<-subset(Dati$ret,Dati$Country==2)
plot(density(retChi))
retGer<-subset(Dati$ret,Dati$Country==3)
plot(density(retGer))
retIta<-subset(Dati$ret,Dati$Country==4)
plot(density(retIta))
retJap<-subset(Dati$ret,Dati$Country==5)
plot(density(retJap))
retUk<-subset(Dati$ret,Dati$Country==6)
plot(density(retUk))
retUSA<-subset(Dati$ret,Dati$Country==7)
plot(density(retUSA))
xyplot(ret~year|Country,Dati,type="l",strip=FALSE)
indva<-Dati$tindva
capit<-Dati$tcapit
manva<-Dati$tmanva
servva<-Dati$tservva
bcpriv<-Dati$tbcpriv
list<-Dati$tlist

#Effetti fissi
pmod<-plm(ret~grow+infl+indva+manva+servva+unr+bcpriv+rir+capit+list,data=Dati,model="within")
summary(pmod)
plot(predict(pmod),residuals(pmod),xlab="Fitted",ylab="Residuals")
title("Fitted vs Residuals")
abline(h=0)
#pvalue basso, modello corretto. Stima gli effetti fissi ma è significativo solo
#per inflazione e capitalizzazione di mercato

#Effetti casuali
rand<-plm(ret~grow+infl+indva+manva+servva+unr+bcpriv+rir+capit+list,data=Dati,model="random")
summary(rand)
plot(predict(rand),residuals(rand),xlab="Fitted",ylab="Residuals")
title("Fitted vs Residuals random")
abline(h=0)
#Anche il modello random stima come significativi capitalizzazione di mercato e 
#inflazione. p-value molto basso, modello sempre significativo

#Modello 
#Decisione del modello: fixed o random? Usiamo il test di Hausman
#Ipotesi nulla: modello random. Ipotesi alternativa: modello fixed.
phtest(pmod,rand)
#Risultato del test di Hausman: pvalue alto, uso modello random.

#Effetti misti lineari
rret<-rescale(Dati$ret)
rgrow<-rescale(Dati$grow)
rinfl<-rescale(Dati$infl)
rindva<-rescale(Dati$indva)
rmanva<-rescale(Dati$manva)
rservva<-rescale(Dati$servva)
runr<-rescale(Dati$unr)
rbcpriv<-rescale(Dati$bcpriv)
rrir<-rescale(Dati$rir)
rcapit<-rescale(Dati$capit)
rlist<-rescale(Dati$list)
mix<-lmer(rret~rgrow+rinfl+rindva+rmanva+rservva+runr+rbcpriv+rrir+rcapit+rlist+(1|Country)+(1|year),data=Dati)
summary(mix)
plot(predict(mix),residuals(mix),xlab="Fitted",ylab="Residuals")
title("Fitted vs Residuals mixed")
#I residui presentano un trend. Provo con random slopes:
mix2<-lmer(rret~rgrow+rinfl+rindva+rmanva+rservva+runr+rbcpriv+rrir+rcapit+rlist+(1+rinfl|year),data=Dati)
summary(mix2)
plot(predict(mix2),residuals(mix2),xlab="Fitted",ylab="Residuals")
title("Fitted vs Residuals with slope")
abline(h=0)

#Predittori
Dati$pred<-pmodel.response(rand)
good<-mean((Dati$ret-Dati$pred)^2)
good
Dati$predf<-pmodel.response(pmod)
goodf<-mean((Dati$ret-Dati$predf)^2)
goodf
Dati$predmix2<-predict(mix2)
goodmix2<-mean((Dati$ret-Dati$predmix2)^2)
goodmix2

