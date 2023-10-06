###############################################
###### PROJET FINAL - MODELE LINEAIRE #########
###############################################
###### ERTAS Elif - M1 SEP ####################
###############################################

                ####### Biblioteque ###### 
library(readr) 



          ######## Lecture et affectation ########

Spot <- read_csv("Spotify2010-2019_Top100.csv", col_types = cols(bpm = col_double(), 
                                              nrgy = col_double(), dnce = col_double(), 
                                              dB = col_double()))
Spotify <- as.data.frame(Spot)
str(Spotify)  # pour vérifier la nature des variables importées
names(Spotify)   #liste des noms de variables
#on prend uniquement les colonnes que l'on va étudier 
don <- Spotify[,c(1,6:8,15)]
colnames(don)<-c("titre","bpm","energy","danceability","popularite")

str(don)
attach(don) 
View(don)

          ###### Analyse de donnees ###########

summary(don)

#correlation entre chaque variables 
cor1 <- cor.test(bpm, energy, method = "pearson")
print(cor1)
cor2 <- cor.test(bpm, danceability, method = "pearson")
print(cor2)
cor3 <- cor.test(energy, danceability, method = "pearson")
print(cor3)
#correlation avec la variable explicative 
cor.test(popularite, bpm, method = "pearson")
cor.test(popularite, energy, method = "pearson")
cor.test(popularite, danceability, method = "pearson")

#regression multiple 
reg_tot<-lm(pop~bpm+energy+danceability+dB+val+acous+spch, data=Spotify)
summary(reg_tot)
reg1<-lm(popularite~bpm+energy+danceability, data=don)
summary(reg1)
#intervalle de confiance
confint(reg1)
#effet levier
hatvalues(reg1)
cooks.distance(reg1)[cooks.distance(reg1) > 1] 
#graphique 
plot(reg1)



