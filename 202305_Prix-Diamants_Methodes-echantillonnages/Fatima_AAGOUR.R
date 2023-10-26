# Modèle de régression linéaire multiple
# Tests d'hypothèses

#Vider la mémoire
rm(list = ls())

#installer le package "ISLR"

library(ggplot2)
library(lmtest)
library(glmulti)


str(diamonds)
View(diamonds)
?diamonds
head(diamonds)

#construction du modele regression lineaire mulriple
#Modele RLM 
modele.RLM <- lm(formula = price ~ ., data = diamonds)
summary(modele.RLM)

#Analyse des résidus 
#Graphiquement
# la non-corrélation des erreurs
acf(modele.RLM$residuals)
#la relation linéaire entre la variable réponse et les variables explicatives
#graphiquement :
plot(modele.RLM, 1)
# l’hypothèse d’homoscédasticité des erreurs 
plot(modele.RLM, 3)

#Tester la non-corrélation (d'ordre 1) des erreurs : test de Durbin-Watson
dwtest(modele.RLM, alternative = c("two.sided"))  
#on obtient p-value < 2.2e-16, On rejette H0, les erreures sont corrélés

#homoscédasticité des erreurs: de Breusch-Pagan
require(lmtest)
bptest(modele.RLM, studentize = FALSE) 
#on obtient p-value < 2.2e-16, on rejtte donc H_0, 
#Le terme d'erreur est éroscédasticique


#### vérifier la normalité des erreurs ####
#Graphiquement : normal Q-Q plot
#normal Q-Q plot
plot(modele.RLM, 2)


#Test de Kolmogorov-Smirnov pour tester l'hypothèse de normalité du terme d'erreur
ks.test(residuals(modele.RLM),"pnorm") 
#on obtient une p-value < 2.2e-16, on rejette donc H_0, i.e., la loi du terme d'erreur ne suit pas une loi normale 

#Test de fisher 

#Ordonner les variables explicatives numériques/qualitatives 
#selon les valeurs des P_values croissantes du test de Fisher
tests.Fisher <- anova(modele.RLM)
tests.Fisher
str(tests.Fisher)
m <- nrow(tests.Fisher)
vect.pvalues.Fisher <- tests.Fisher[1:m-1,"Pr(>F)"] #Extrait le vecteur des p_values
names(vect.pvalues.Fisher) <- rownames(tests.Fisher[1:m-1,])
sort(vect.pvalues.Fisher) #Attention pour les variables explicatives qualitatives ayant
#plus de deux modalités (on compare à dimension égale)
#dans ce cas il faudrait faire comme suit
XX <- model.matrix(price ~., data = diamonds)[,-1] #Cette fonction construit la matrice de design en remplaçant 
#chacune des variables qualitatives par les indicatrices 
#de ses modalités (la première modalité est supprimée)
#on supprime la première colonne correspondant à l'intercept

View(XX)
diamonds.num.data <- cbind(price = diamonds[,"price"],XX)
diamonds.num.data <- as.data.frame(diamonds.num.data) #Bd constituée que de variables numériques
View(diamonds.num.data)
tests.Fisher2 <- anova(lm(price~., data = diamonds.num.data))
tests.Fisher2
m <- nrow(tests.Fisher2)
vect.pvalues.Fisher2 <- tests.Fisher2[1:m-1,"Pr(>F)"] #Extrait le vecteur des p_values
names(vect.pvalues.Fisher2) <- rownames(tests.Fisher2[1:m-1,])
sort(vect.pvalues.Fisher2)

#Outliers
n <- nrow(diamonds) #nombre d'observations
p <- ncol(diamonds)-1 #nombre de variables explicatives
plot(modele.RLM,5)
abline(h = c(-3,3), v = 2*(p + 1)/n,  col = c("blue","blue","green"), lty = c(2,2,2), lwd = c(3,3,3))

par(mfrow = c(1, 2))
plot(modele.RLM, 4) #Distance de Cook
abline(h = 4/(n-p-1), col = c("blue"), lty = 2, lwd = 3)
plot(modele.RLM, 5) #Résidus versus Leverage
abline(h = c(-3,3), v = 2*(p + 1)/n,  col = c("blue","blue","green"), lty = c(2,2,2), lwd = c(3,3,3))

#Sélection de modèle
#Par recherche exaustive 
#La fonction glmulti() : sélection de variables explicatives numériques/qualitatives
require(glmulti)
select.modele.aic <- glmulti(price ~., data = diamonds, level = 1, 
                             fitfunction = lm, crit = "aic", plotty = FALSE, method = "h")
modele.opt.aic <- summary(select.modele.aic)$bestmodel
modele.opt.aic
anova(lm(modele.opt.aic, data = diamonds))
select.modele.bic <- glmulti(price ~., data = diamonds, level = 1,
                             fitfunction = lm, crit = "bic", plotty = FALSE, method = "h")
modele.opt.bic <- summary(select.modele.bic)$bestmodel
modele.opt.bic
anova(lm(modele.opt.bic, data = diamonds))

#La fonction glmulti() appliquée aux variables explicatives numériques 
#après transformation des variables explicatives qualitatives en quantitatives
XX <- model.matrix(price ~., data = diamonds)[,-1] #Cette fonction construit la matrice de design en remplaçant 
#chacune des variables qualitatives par les indicatricesde ses modalités (la première modalité est supprimée)
#on supprime la première colonne correspondant à l'intercept
diamonds.num.data <- cbind(price = diamonds[,"price"],XX)
diamonds.num.data <- as.data.frame(diamonds.num.data) #Bd constituée que de variables numériques

select.modele.aic <- glmulti(price ~., data = diamonds.num.data, level = 1, 
                             fitfunction = lm, crit = "aic", plotty = FALSE, method = "h")
modele.opt.aic <- summary(select.modele.aic)$bestmodel
modele.opt.aic
select.modele.bic <- glmulti(price ~., data = diamonds.num.data, level = 1,
                             fitfunction = lm, crit = "bic", plotty = FALSE, method = "h")
modele.opt.bic <- summary(select.modele.bic)$bestmodel
select.modele.bic






#### Estimation de l'erreur par leave-one-out VC (LOOCV), i.e., la K-fold CV avec K = n, le nombre d'observations ####
#Modele 1

# 1
modele1 <- glm(formula = price ~ 1+cut+color+clarity+carat+depth+table+x , data = diamonds)
modele1

# 2
library(boot) #on charge le package `boot` pour pouvoir utiliser la fonction cv.glm()
estimation_erreur_modele1 <- cv.glm(data = diamonds, glmfit = modele1, K = 10)$delta[1] #estimation de l'erreur du modele1 par la méthode LOOCV
estimation_erreur_modele1

# 3
estimation_erreur_modele1 <- cv.glm(data = diamonds, glmfit = modele1, K = 10)$delta[1] #estimation de l'erreur du modele1 par la méthode LOOCV
estimation_erreur_modele1

# 6
print(c("Résultats des estimations par LOOCV : ",
        paste("Estimation de l'erreur du modele1 = ", as.character(estimation_erreur_modele1)))) 


#Modele 2
modele2 <- glm(formula = price ~ 1+cut+color+clarity+carat+depth+table+x +z, data = diamonds)
modele2

# 2
estimation_erreur_modele2 <- cv.glm(data = diamonds, glmfit = modele2, K = 10)$delta[1] #estimation de l'erreur du modele1 par la méthode LOOCV
estimation_erreur_modele2

# 3
estimation_erreur_modele2 <- cv.glm(data = diamonds, glmfit = modele2, K = 10)$delta[1] #estimation de l'erreur du modele1 par la méthode LOOCV
estimation_erreur_modele2

# 6
print(c("Résultats des estimations par LOOCV : ",
        paste("Estimation de l'erreur du modele2 = ", as.character(estimation_erreur_modele2)))) 







#### Méthode 2 : estimation de l'erreur par leave-one-out VC (LOOCV), i.e., la K-fold CV avec K = n, le nombre d'observations ####
# 1
modele1 <- glm(formula = price ~ 1+cut+color+clarity+carat+depth+table+x, data = diamonds)
modele1

# 2
library(boot) #on charge le package `boot` pour pouvoir utiliser la fonction cv.glm()
estimation_erreur_modele1 <- cv.glm(data = diamonds, glmfit = modele1, K = n)$delta[1] #estimation de l'erreur du modele1 par la méthode LOOCV
estimation_erreur_modele1

# 4
modele2 <- glm(formula = price ~ 1+cut+color+clarity+carat+depth+table+x+z, data = diamonds)
modele2

estimation_erreur_modele2 <- cv.glm(data = diamonds, glmfit = modele2, K = n)$delta[1] #estimation de l'erreur du modele2 par la méthode LOOCV


# 6
print(c("Résultats des estimations par LOOCV : ",
        paste("Estimation de l'erreur du modele1 = ", as.character(estimation_erreur_modele1)),
        paste("Estimation de l'erreur du modele2 = ", as.character(estimation_erreur_modele2)))) 


