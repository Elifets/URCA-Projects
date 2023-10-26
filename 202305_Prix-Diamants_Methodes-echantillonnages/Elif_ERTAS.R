# Entete ----

# Fichier : ERTAS_ELIF-PROJET_SEP0832.R
# Desc : Projet SEP0832 - M1 SEP - 2022-2023
# Date : 09/05/2023
# Auteur : Elif Ertas (elif.ertas@etudiant.univ-reims.fr)

rm(list = ls()) # vider la memoire 

#Librairie ---- 

library(ggplot2)
library(dplyr)
library(knitr)
library(lmtest)
library(glmulti)

# Partie 1 : Base de donnée ----

?diamonds
str(diamonds)
kable(head(diamonds), caption = "Tableau des 6 premières observations de la base de données diamonds")
bdd_diamonds <- diamonds 


# Partie 2 : Régression linéaire multiple ----

## RLM complet -----

RLM <- lm(price ~ . , data=bdd_diamonds) 
str(RLM) # structure de la reg
summary(RLM)
summary(RLM)$coefficients
# On observe que toutes les variables sont significatives sauf clarity^6, y et z. 

residus <- RLM$residuals
acf(RLM$residuals) # la fonction d auto correlation des erreurs 
# D apres le graphique on observe que les erreurs sont non-correlees 

## Linéarité du modèle -----

plot(RLM, 1) # relation lineaire entre la variable reponse et les variables explicatives
# Une courbe rouge horizontale indique une relation lineaire entre variable reponse et variables explicatives. 
# Ici pas de relation de linearité entre la variable reponse et les variables explicatives. 
plot(RLM, 1, main="Linéarité entre les variables")

## Normalité ----

plot(RLM, 3) # hypothese d homoscedasticite des erreurs 
#ici cest pareil sauf quon prend en ordonnees la racine de la valeur absolue des residus 
##on peut dire que les residus sont homogenes car ils sont reparties de maniere homogene de part et dautre de la droite rouge (haut et bas)
#hypothese dhomoscedasticite est bien verifiee 

residus <- RLM$residuals
hist(residus, freq = FALSE,
     main = "Histogramme des résidus")
curve(dnorm(x, mean = mean(residus), sd = sd(residus)), 
      col = 2, lty = 2, lwd = 2, add = TRUE)

ks.test(residuals(RLM), "pnorm")

## Influence des outliers ----

par(mfrow=c(1,2))
plot(RLM,4) # distance de Cook
abline(h=4/(100-5-1), col=c("blue"), lty = 2, lwd=3)
# Outliers et points leviers extrêmes
plot(RLM,5)
abline(h=c(-3,3), v=2*(5+1)/100, col=c("blue","blue","green"),lty=c(2,2,2),lwd=c(3,3,3))


n <- nrow(diamonds) # nombre d'observations
modele1 <- glm(formula = price ~ 1+cut+color+clarity+carat+depth+table+x, data = bdd_diamonds)
library(boot) #on charge le package `boot` pour pouvoir utiliser la fonction cv.glm()
estimation_erreur_modele1 <- cv.glm(data = bdd_diamonds, glmfit = modele1, K = n)$delta[1] #estimation de l'erreur du modele1 par la méthode LOOCV
modele2 <- glm(formula = price ~ 1+cut+color+clarity+carat+depth+table+x+z, data = bdd_diamonds)
estimation_erreur_modele2 <- cv.glm(data = bdd_diamonds, glmfit = modele2, K = n)$delta[1] #estimation de l'erreur du modele2 par la méthode LOOCV

print(c("Résultats des estimations par LOOCV : ",
        paste("Estimation de l'erreur du modele1 = ", as.character(estimation_erreur_modele1)),
        paste("Estimation de l'erreur du modele2 = ", as.character(estimation_erreur_modele2)))) 

## Selection du meilleur modèle (méthode génétique) ----

#AIC 
select.mod.gen <- glmulti(price ~ ., data = bdd_diamonds, level = 1, method = "g", 
                          fitfunction = lm, crit = 'aic', plotty = F)
aic.best.model <- summary(select.mod.gen)$bestmodel

aic.best.model

#BIC
select.mod.gen <- glmulti(price ~ ., data = bdd_diamonds, level = 1, method = "g", 
                          fitfunction = lm, crit = 'bic', plotty = F)
bic.best.model <- summary(select.mod.gen)$bestmodel

bic.best.model


