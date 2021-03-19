
library(ggplot2)
library(factoextra)
library(ade4)
library(dendextend)
library(readr)
library(readxl)
library(car)


X2016 <- read_csv("/Volumes/CHRISTIAN/programmation R/Projet Analyse des Données M1 S1/2016.csv")

mydata.bis<- X2016[,c(1,2, 7, 8, 9, 10, 11, 12)]
head(mydata.bis)

#countries <- read_excel("~/Downloads/countries of the world.xls")

countries <- read_excel("/Volumes/CHRISTIAN/programmation R/Projet Analyse des Données M1 S1/countries of the world.xls")
clima<- countries[c(1,15)]

mydata<- matrix(rep(0, 145),145 , 9)
vect.1<-rep(0,145)
vect.2<-rep(0,145)
i=1
j=2
w=0

for(i in 1:157){ 
  for(j in 2:228){  
    if(mydata.bis$Country[i]==clima$Country[j] ){
      w=w+1
      vect.1[w]<-i
      vect.2[w]<-j
    }
  }
}

mydata<-cbind(mydata.bis[vect.1,],clima[vect.2,2])

mydata[111,9]<-NA


for(aa in 1:145){   
  if( mydata[aa,2]=='Western Europe'|mydata[aa,2]== 'Australia and New Zealand'| mydata[aa,2]=='North America'){
    mydata[aa,2]<-'OCCI'
  } 
}

aa=1
for(aa in 1:145){   
  if( mydata[aa,2]=='Middle East and Northern Africa'|mydata[aa,2]== 'Sub-Saharan Africa'){
    mydata[aa,2]<-'AFR'
  } 
}

aa=1
for(aa in 1:145){   
  if( mydata[aa,2]=='Southeastern Asia'|mydata[aa,2]== 'Southern Asia'|mydata[aa,2]== 'Eastern Asia'){
    mydata[aa,2]<-'ASI'
  } 
}

aa=1
for(aa in 1:145){   
  if( mydata[aa,2]== 'Central and Eastern Europe'){
    mydata[aa,2]<-'EUR_C_E'
  } 
}

aa=1
for(aa in 1:145){   
  if( mydata[aa,2]== 'Latin America and Caribbean'){
    mydata[aa,2]<-'AMER_C_S'
  } 
}

names(mydata)=c('Pays','Region','Economie', 'Famille', 'Santé', 'Liberté', 'Corruption','Generosité', 'Climat' )


bb=1
for (bb  in 1:145) {
  if(mydata[bb, 4] <0.4 ){
    mydata[bb, 4]<- 'INSUF'
  } 
}

bb=1
for (bb  in 1:145) {
  if(mydata[bb, 4] >=0.4 & mydata[bb, 4] <0.8){
    mydata[bb, 4]<- 'MOYEN'
  } 
}

bb=1
for (bb  in 1:145) {
  if( mydata[bb, 4] >= 0.8 & mydata[bb, 4] <1.2 ){
    mydata[bb, 4]<- 'IMPORT'
  } 
}

mydata[48,2]<- "ASI"; mydata[94,2]<- "ASI"; mydata[69,2]<- "ASI"; mydata[77,2]<- "ASI"
mydata[99,2]<- "ASI"; mydata[105,2]<- "ASI"; mydata[11,2]<- "ASI";mydata[76,2]<- "ASI"

mydata[36 ,2]<- "ASI"; mydata[34,2]<- "ASI"; mydata[144,2]<- "ASI"; mydata[74,2]<- "ASI"
mydata[28,2]<- "ASI"; mydata[135,2]<- "ASI"; mydata[117,2]<- "ASI"; mydata[53,2]<- "ASI"

mydata[81,2]<- "ASI"; mydata[62,2]<- "ASI"; mydata[113,2]<- "ASI"

mydata<-mydata[-123,]
mydata<-mydata[-104,]
mydata<-mydata[-93,]
mydata<-mydata[-83,]
mydata<-mydata[-73,]

##########################################
# VARIABLES


var.quanti<-mydata[c(3,5,6,7,8)]
var.quali<-mydata[c(2,4)]


#### variables quantitatives ####

summary(var.quanti) # faire un tableau
summary(var.quanti$Economie)
var(var.quanti$Economie)*(140-1)/140

var(var.quanti$Santé)*(140-1)/140
var(var.quanti$Liberté)*(140-1)/140
var(var.quanti$Corruption)*(140-1)/140
var(var.quanti$Generosité)*(140-1)/140

# Caractéristiques de position de chaque variable
boxplot(var.quanti[,c(1,2)],horizontal = T, col = "purple")
boxplot(var.quanti[,c(3,4,5)],horizontal = T, col="purple")

hist(var.quanti$Economie, breaks=8,col = "purple",ylim = c(0,35),labels = T, main = "Nombre de pays en fonction de l'ECONOMIE", xlab = "ECONOMIE", ylab = "Effectifs" )
hist(var.quanti$Economie*5, nclass=8, col = "purple", ylim = c(0,0.25),proba= TRUE, labels = T, main="Proportion de pays en fonction de l'ECONOMIE", xlab = "ECONOMIE", ylab = "Proportion" )
lines(density(var.quanti$Economie*5, na.rm = TRUE), lwd = 2, col = "orange" )
legend(0.1, 0.25, legend= c("densité"), col= c("orange"), lty = 1,  lwd = 2)

hist(var.quanti$Santé, breaks=6,ylim = c(0, 60), col = "purple",labels = TRUE, main = 'Nombre de pays en fonction de la SANTE', xlab = "SANTE", ylab = "Effectifs")
hist(var.quanti$Santé*5, nclass=6, col = "purple", ylim = c(0,0.4), freq = F,labels = T, main="Proportion de pays en fonction de la SANTE", xlab = "SANTE", ylab = "Proportion")
lines(density(var.quanti$Santé*5, na.rm = TRUE), lwd = 2, col = "orange" )
legend(0.1, 0.4, legend= c("densité"), col= c("orange"), lty = 1, lwd = 2)


hist(var.quanti$Liberté,nclass = 8, ylim = c(0, 50),  col = "purple", labels = TRUE, main = "Nombre de pays en fonction de la LIBERTE", xlab = "LIBERTE", ylab = "Effectifs")
hist(var.quanti$Liberté*10, nclass=8, col = "purple", ylim = c(0,0.3),proba= T, labels = T , main="Proportion de pays en fonction de la LIBERTE", xlab = "LIBERTE", ylab = "Proportion")
lines(density(var.quanti$Liberté*10, na.rm = TRUE), lwd = 2, col = "orange" )
legend(0.1, 0.3, legend= c("densité"), col= c("orange"), lty = 1,  lwd = 2)


hist(var.quanti$Corruption, nclass = 6, ylim = c(0, 80),  col = "purple", labels = TRUE, main = "Nombre de pays en fonction de la CORRUPTION", xlab = "CORRUPTION", ylab = "Effectifs")
hist(var.quanti$Corruption*10, nclass=6, col = "purple", ylim = c(0,0.6),proba= T, labels = T , main="Proportion de pays en fonction de la CORRUPTION", xlab = "CORRUPTION", ylab = "Proportion")
lines(density(var.quanti$Corruption*10, na.rm = TRUE), lwd = 2, col = "orange" )
legend(4, 0.6, legend= c("densité"), col= c("orange"), lty = 1,  lwd = 2)


hist(var.quanti$Generosité, nclass = 6,  ylim = c(0, 50), col = "purple", labels = TRUE, main = "Nombre de pays en fonction de la GENEROSITE", xlab = "GENEROSITE", ylab = "Effectifs")
hist(var.quanti$Generosité*10, nclass=6, col = "purple",ylim = c(0,0.4),proba= T, labels = T , main="Proportion de pays en fonction de la GENEROSITE", xlab = "GENEROSITE", ylab = "Proportion")
lines(density(var.quanti$Generosité*10, na.rm = TRUE), lwd = 2, col = "orange" )
legend(4, 0.4, legend= c("densité"), col= c("orange"), lty = 1,  lwd = 2)


# coef de correlation
cor(var.quanti)
shapiro.test(var.quanti$Economie)
shapiro.test(var.quanti$Santé)
shapiro.test(var.quanti$Liberté)
shapiro.test(var.quanti$Corruption)
shapiro.test(var.quanti$Generosité)

# analyse entre l'ECONOMIE et SANTE  ---> regression lineaire prediction residus

plot(var.quanti$Santé ~ var.quanti$Economie, main= "La SANTE en fonction de l'ECONOMIE ", ylab = "SANTE", xlab= "ECONOMIE", col=4)
shapiro.test(var.quanti$Economie) # suit pas une loi normale
shapiro.test(var.quanti$Santé) # suite pas une loi normale
# on ne peut pas utiliser le test de fisher pour savoir si les variances sont égales ou non.

cor.test(var.quanti$Economie,var.quanti$Santé,method = "spearman") # rho est 0.8561219 donc la relation linéaire entre les variables est forte


# regression lineaire

reg <- lm(var.quanti$Santé ~ var.quanti$Economie)
summary(reg)

plot(var.quanti$Santé ~ var.quanti$Economie) # 0.45290*x + 0.12803
abline(reg, col="red")

scatterplot(var.quanti$Santé ~ var.quanti$Economie, xlab = 'ECONOMIE', ylab='SANTE') 
abline(reg, col="red", lwd=4)
title("Régression linéaire ")
legend(0.1, 1, legend = c("Régression local", "Régression linéaire \n 0.45290*x + 0.12803"), col = c("blue","red"), lty=c(2,1), cex=0.7, lwd = c(2,4))

res = reg$residuals
plot(res, main = "Les résidus en fonction des pays",ylab = "Résidus" , xlab = "PAYS")
abline(h=-sd(res), col="red", lty = 2)
abline(h=sd(res), col="red", lty = 2)
abline(h=0, col = "brown")

shapiro.test(res) #  p-value = 4.161e-07  non il ne suit pas une loi normale

# prédiction
ind = 1:140
subind = sample(ind, 120)
restind = setdiff(ind, subind)


var.quanti.ind.sante <- var.quanti$Santé[subind]
var.quanti.ind.economie <- var.quanti$Economie[subind]

reg1 = lm(var.quanti.ind.sante ~ var.quanti.ind.economie)
summary(reg1)


vec<-predict(reg1, data.frame(var.quanti.ind.economie=var.quanti$Economie[restind] ))

plot(var.quanti$Santé[restind], main = "SANTE réelle et prédite", ylab = "SANTE",xlab = "Pays pris au hasard",type = "l", col="blue")
lines(vec, col="red")
legend("bottomleft",legend=c("Santé réelle"," Santé prédite"), col = c("blue","red"), lty = 1)
       
       
resid1<- var.quanti$Santé[restind]-vec
plot(resid1, main = "Résidus des valeurs réels moins les valeurs prédites", ylab = "Résidus", xlab = "Pays pris au hasard")
abline(h=-sd(resid1), col="red", lty = 2)
abline(h=sd(resid1), col="red", lty = 2)
abline(h=0, col = "brown")

shapiro.test(resid1) # p-value = 0.443 il suit une loi normale 





##########    #############  ACP


summary(mydata)
res.acp <- prcomp(var.quanti, scale=T) 

summary(res.acp)


acp <- dudi.pca(var.quanti, scannf = FALSE, nf = 3)
acp$eig
acp$eig/sum(acp$eig)*100
barplot(prop.table(acp$eig)*100,main='Histogramme des valeurs propres', ylab = "Pourcentage d'inertie", names.arg = 1:5, col ="orange", probability=T)

# invidivus
inertieL<-inertia.dudi(acp, row.inertia=TRUE)
round(inertieL$row.abs/100, digits = 3) # contribution,
round(inertieL$row.rel/100, digits = 4) # qualité

#variable
inertieC<-inertia.dudi(acp, col.inertia=TRUE)
round(inertieC$col.abs/100, digits = 3) # contribution,
round(inertieC$col.rel/100, digits = 4) # qualité


# cercle de corrélation et plan factoriel

par(mfrow=c(2,1))
s.corcircle(acp$co, xax = 1, yax = 2,clabel = 1.2 )
s.label(acp$li, xax = 1, yax = 2, clabel =  0.6)


par(mfrow=c(2,1))
s.corcircle(acp$co, xax = 1, yax = 3,clabel = 1.2)
s.label(acp$li, xax = 1, yax = 3, clabel =  0.6)

par(mfrow=c(2,1))
s.corcircle(acp$co, xax = 2, yax = 3,clabel = 1.2)
s.label(acp$li, xax = 2, yax = 3, clabel =  0.6)

# information supplémentaire une variable qualitative définissant des groupes d’individus
gcol1=c(1:5)
gcol2=c(1:3)
s.class(dfxy = acp$li, fac = as.factor(var.quali$Region), col = gcol1, xax = 1, yax = 2)
s.class(dfxy = acp$li, fac = as.factor(var.quali$Region), col = gcol1, xax = 1, yax = 3)
s.class(dfxy = acp$li, fac = as.factor(var.quali$Region), col = gcol1, xax = 2, yax = 3)


s.class(dfxy = acp$li, fac = as.factor(var.quali$Famille), col = gcol2, xax = 1, yax = 2)
s.class(dfxy = acp$li, fac = as.factor(var.quali$Famille), col = gcol2, xax = 1, yax = 3)
s.class(dfxy = acp$li, fac = as.factor(var.quali$Famille), col = gcol2, xax = 2, yax = 3)


#######


### variables qualitatives #####

table(var.quali$Region)
prop.table(table(var.quali$Region))

barplot( table(var.quali$Region), main= 'Nombre de pays en fonction de la REGION', ylab = "Effectifs", xlab = "REGION", col = "purple")
barplot( prop.table(table(var.quali$Region)), main= 'Proportion de pays en fonction de la REGION',ylab = "Proportion", xlab = "REGION",col = "purple")

table(var.quali$Famille)
prop.table(table(var.quali$Famille))

barplot( table(var.quali$Famille), main= 'Nombre de pays en fonction de la FAMILLE', ylab = "Effectifs", xlab = "FAMILLE", col = "purple")
barplot( prop.table(table(var.quali$Famille)), main= 'Proportion de pays en fonction de la FAMILLE',ylab = "Proportion", xlab = "FAMILLE",col = "purple")



chisq.test(var.quali$Region, var.quali$Famille)$expected ## non car on remplie pas les conditions de ce test. On va mettre ces tableaux sur le rapport
chisq.test(var.quali$Region, var.quali$Famille)$observed
chisq.test(var.quali$Region, var.quali$Famille)$residuals

chisq.test(var.quali$Region, var.quali$Famille)

fisher.test(var.quali$Region, var.quali$Famille, workspace = 20000000) # p-value = 3.416e-07
# il y a dependance entre des 2 variables

# tableau de contingence   AFC

tableau<-table(var.quali$Region, var.quali$Famille)
fisher.test(tableau, workspace = 20000000) # p-value = 3.416e-07 # donc les 2 variables sont liées

prop.table(tableau,1)*100 # par rapport aux lignes
prop.table(tableau,2)*100 # par rapport aux colonnes

barplot(prop.table(tableau,2), main="Région selon le soutient familial",xlab = "FAMILLE", col = rainbow(5)) 
barplot(t(prop.table(tableau,1)), main = "Le soutient familial selon la REGION", xlab = "REGION", col=rainbow(3) )

tab2<-as.matrix.data.frame(tableau)
rownames(tab2)= c('AFR', 'ASI', 'EUR_C_E','OCCI', 'AMER_C_S')
colnames(tab2) = c('IMPORT', 'INSUF', 'MOYEN')

afc<-dudi.coa(tab2,scannf = FALSE, nf = 3)
afc$eig # valeurs propres
barplot(prop.table(afc$eig)*100,main='Histogramme des valeurs propres', ylab = "Pourcentage d'inertie (%)", names.arg = 1:2, col ="orange")

afc$li # on verra ca après
afc$co # on verra ca après

inertie <-inertia.dudi(afc, row.inertia=TRUE)
inertie
# Coordonnées profils ligne
round(afc$li,2)

round(inertie$row.abs/100, digits = 3)  # contrib
round(inertie$row.re/100, digits = 3)  #qualit
#Coordonnées profils colonne
round(afc$co,2)

round(inertie$col.abs/100,digits = 5) # contribu # 0
round(inertie$col.re/100) # qualit # 0


s.label(afc$li,xax=1,yax=2, boxes = F)
s.label(afc$co,xax=1,yax=2,add.plot=T, boxes= T)

scatter(afc) # il faut choisir les modalites de region ou les modalites de famille


##### classification

# CAH sur les individus

dzebu = dist.dudi(acp, amongrow=TRUE) 
round(dzebu,1) 
cah=hclust(dzebu^2,method="ward.D") 


plot(cah,hang=-1,main =  "Classification hiérarchique", xlab = "Pays", ylab = "Height") # dendogramme
#rect.hclust(cah,h=20)
#cutree(cah,h=20)
s.label(acp$li, boxes = T)    # plan factoriel et après j'ai regroupe les individus par groupe 

s.class(dfxy=acp$li,fac=as.factor(cutree(cah,k=3)) ) #h=grand # à titre exemplaire
s.class(dfxy=acp$li,fac=as.factor(cutree(cah,k=4)) ) #
s.class(dfxy=acp$li,fac=as.factor(cutree(cah,k=6)) ) #
s.class(dfxy=acp$li,fac=as.factor(cutree(cah,k=7)) ) #

# cherchons le nombre de classe optimal:

barplot(cah$height[order(cah$height,decreasing=TRUE)],col="orange",main = "Le prix à payer pour passer de k à  k-1 groupes", xlab = 'groupes',ylab='Height', xlim = c(0,20))
plot(cah$height,main = "Le prix à payer par rapport au nombre d'itération", ylab = "Height", xlab = "Nombre d'itération", col="blue")

colgr6=color_labels(cah,k=5,col=rainbow(5))

plot(colgr6, main =  "Classification hiérarchique", xlab = "Pays" , ylab = "Height") # dendogramme
s.class(dfxy=acp$li,fac=as.factor(cutree(cah,k=5)) ) # le nombre de classe optimal est 5

cutree(cah, k=5)
# ici le le groupe 1 represente plutot l'OCCIDENT et le groupe 5 plutot l'AFRIQUE, ils ne representent pas completement l'OCCIDENT et l'AFRIQUE  et on peut rien dire sur les autres groupes
####

colgr6=color_labels(cah,k=3,col=rainbow(3))
plot(colgr6, main =  "Classification hiérarchique", xlab = "Pays" , ylab = "Height") # dendogramme
s.class(dfxy=acp$li,fac=as.factor(cutree(cah,k=3)), col=c(2:4) )
cutree(cah, k=3)



## on pourrait aussi regrouper tous les individus en 2 groupes

colgr6=color_labels(cah,k=2,col=c(2:3))
plot(colgr6, main =  "Classification hiérarchique", xlab = "Pays" , ylab = "Height") # dendogramme
s.class(dfxy=acp$li,fac=as.factor(cutree(cah,k=2)), col=c(2:3) )
cutree(cah, k=2)

#  on peut interpreter ceci
# 1: les pays où la qualité de vie est fortement élévé
# 2 : les pays où la qualité de vie est moyenne ou médiocre


# CAH sur les variables

dzebu_1 = dist.dudi(acp, amongrow=FALSE)
round(dzebu_1,1)
cah_1=hclust(dzebu_1^2,method="ward.D") 
barplot(cah_1$height[order(cah_1$height,decreasing=TRUE)], main = "Le prix à payer pour passer de k à  k-1 groupes", xlab = 'groupes',ylab='Height')

plot(cah_1,hang=-1, main= "Classification hiérarchique pour les variables", xlab = "Variables quantitatives") # dendogramme
rect.hclust(cah_1,h=1)


s.label(acp$co)  ## ACP vari quanti

 # regroupe les variables en groupe
cutree(cah_1,k=4)
s.class(dfxy=acp$co,fac=as.factor(cutree(cah_1,k=4)) ) # on peut regroupe l'economie et la santé

cutree(cah_1,k=3)
s.class(dfxy=acp$co,fac=as.factor(cutree(cah_1,k=3)) ) # on peut regrouper la liberte et la corruption

cutree(cah_1,k=2)
s.class(dfxy=acp$co,fac=as.factor(cutree(cah_1,k=2)) ) # on peut regrouper la liberte, la corrpution et la generosite
# rq : 


### Variables quantitatives et qualitatives ###  ANALYSE DISCRIMINANTE

par(mfrow=c(1,2))

boxplot(var.quanti$Economie ~ var.quali$Region, col="purple", main="L'ECONOMIE sachant la REGION", ylab="ECONOMIE", xlab="REGION")
boxplot(var.quanti$Economie ~ var.quali$Famille, col="purple", main="L'ECONOMIE sachant la FAMILLE", ylab="ECONOMIE", xlab="FAMILLE")
tapply(var.quanti$Economie,var.quali$Region,mean) # moyenne
tapply(var.quanti$Economie, var.quali$Famille, mean) # moyenne

#wilcox.test(var.quanti$Economie~ var.quali$Famille, conf.int=TRUE) # PAS
#t.test(var.quanti$Economie ~ var.quali$Famille) # PAS

boxplot(var.quanti$Santé ~ var.quali$Region, col="purple", main="La SANTE sachant la REGION", ylab="SANTE", xlab="REGION")
boxplot(var.quanti$Santé ~ var.quali$Famille, col="purple", main="La SANTE sachant la FAMILLE", ylab="SANTE", xlab="FAMILLE")
tapply(var.quanti$Santé,var.quali$Region,mean) # moyenne
tapply(var.quanti$Santé, var.quali$Famille, mean) # moyenne


boxplot(var.quanti$Liberté ~ var.quali$Region, col="purple", main="La LIBERTE sachant la REGION", ylab="LIBERTE", xlab="REGION")
boxplot(var.quanti$Liberté ~ var.quali$Famille, col="purple",  main="La LIBERTE sachant la FAMILLE", ylab="LIBERTE", xlab="FAMILLE")
tapply(var.quanti$Liberté,var.quali$Region,mean) # moyenne
tapply(var.quanti$Liberté, var.quali$Famille, mean) # moyenne
tapply(var.quanti$Liberté, var.quali$Famille, sd) # 



boxplot(var.quanti$Corruption ~ var.quali$Region, col="purple", main="La CORRUPTION sachant la REGION", ylab="CORRUPTION", xlab="REGION")
boxplot(var.quanti$Corruption ~ var.quali$Famille, col="purple",   main="La CORRUPTION sachant la FAMILLE", ylab="CORRUPTION", xlab="FAMILLE")
tapply(var.quanti$Corruption,var.quali$Region,mean) # moyenne
tapply(var.quanti$Corruption, var.quali$Famille, mean) # moyenne ### intéressante
tapply(var.quanti$Corruption, var.quali$Famille, sd) # 


boxplot(var.quanti$Generosité ~ var.quali$Region, col="purple", main="La GENEROSITE sachant la REGION", ylab="GENEROSITE", xlab="REGION")
boxplot(var.quanti$Generosité ~ var.quali$Famille, col="purple",  main="La GENEROSITE sachant la FAMILLE", ylab="GENEROSITE", xlab="FAMILLE")
tapply(var.quanti$Generosité,var.quali$Region,mean) # moyenne
tapply(var.quanti$Generosité, var.quali$Famille, mean) # moyenne  ### intéressante
tapply(var.quanti$Generosité, var.quali$Famille, sd) # 



shapiro.test(var.quanti$Economie) #  p-value = 0.002527
shapiro.test(var.quanti$Santé) # p-value = 3.965e-05
shapiro.test(var.quanti$Liberté) #  p-value = 0.0003636
shapiro.test(var.quanti$Corruption) # p-value = 1.413e-10
shapiro.test(var.quanti$Generosité) #  p-value = 0.00131

# aucune variable quantitative suit une loi normale 
# donc on ne peut pas utiliser le test de Student (t.test) pour comparer des moyennes issues d’une variable numérique et catégorielle
# de plus, on peut pas non plus faire le test de Wilcoxon car le facteur de regroupement n'a pas 2 niveaux

# On peut essayer de regrouper les modalités INSUF et MOYEN afin d'obtenir 2 modalités pour la variables FAMILLE 



##### ANALYSE DISCRIMANTE


w<- dudi.pca(var.quanti ,scal=F,scannf = FALSE)
inertia.dudi(w)
discr <- discrimin(w,fac=as.factor(var.quali$Famille),scannf=F, nf=2)

discr$eig
summary(lm(discr$li[,1]~ var.quali$Famille )) # pourvoir discriminant 0.4469


w$eig
w$c1

# L’analyse inter-classe cherche les axes sur la base de la variabilit´e inter-classe :
wbet<-bca(w, fac= as.factor(var.quali$Famille), scannf = F)
wbet$eig
wbet$c1

# L’analyse discriminante est celle qui maximisera le rapport :
wdis<-discrimin(w,fac= as.factor(var.quali$Famille) , scannf = F)
wdis$fa/sqrt(sum(wdis$fa^2))

wdis$eig



#@##
library(adegraphics)
library(scatterplot3d)
library(MASS)

s.class(var.quanti, as.factor(var.quali$Famille),xax=1:4, yax=1:4 , porigin.include=FALSE, plabels.cex=1.5, col=c("blue","black","red"), ppoints.cex=1, starSize=0.5)  


par(mfrow=c(2,2))
mar0=c(2,3,2,3)
scatterplot3d(var.quanti[,1],var.quanti[,2],var.quanti[,3],mar=mar0,color=c("blue","black",
                                                          "red")[as.factor(var.quali$Famille)],pch=19)
scatterplot3d(var.quanti[,1],var.quanti[,3],var.quanti[,4],mar=mar0,color=c("blue","black",
                                                          "red")[as.factor(var.quali$Famille)],pch=19)
scatterplot3d(var.quanti[,3],var.quanti[,4],var.quanti[,1],mar=mar0,color=c("blue","black",
                                                          "red")[as.factor(var.quali$Famille)],pch=19)
scatterplot3d(var.quanti[,4],var.quanti[,1],var.quanti[,2],mar=mar0,color=c("blue","black",
                                                          "red")[as.factor(var.quali$Famille)],pch=19)




###   Discrimination pr´edictive ou descriptive

# pour la variable Famille
library(MASS)

echa <- sample(1:140,110)
tabref <- var.quanti[echa,1:5] # selection de 50 var.quanti
espref <- var.quali[echa,2] # modalités de la var.  Famille de la s´election
tabsup <- var.quanti[-echa,1:5] # tableau des 25 autres
espsup <- var.quali[-echa,2] # nom de l'esp`ece des 25 autres
lda2 <- lda(tabref,espref)
lda2

espestim <- predict(lda2,tabsup)$class # fonction g´en´erique utilise predict.lda
table(espestim,espsup)


# Pour la variable Region

echa <- sample(1:140,110)
tabref <- var.quanti[echa,1:5] # selection de 120 individus et les variables quanti correspondnates
espref <- var.quali[echa,1] # noms d'esp`eces de la s´election
tabsup <- var.quanti[-echa,1:5] # tableau des 100 autres
espsup <- var.quali[-echa,1] # nom de l'esp`ece des 100 autres
lda2 <- lda(tabref,espref)
lda2

espestim <- predict(lda2,tabsup)$class # fonction g´en´erique utilise predict.lda
table(espestim,espsup)






