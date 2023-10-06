rm(list = ls())

library(ISLR)
library(ggplot2)

str(diamonds) #structure du jeu de données "Auto"
?diamonds
n <- nrow(diamonds) #nombre d'observations
n


#### Méthode1 : estimation de l'erreur de prévision par l'approche de l'ensemble de validation ####
indices <- sample(x = n, size = trunc((2/3)*n), replace = FALSE) #vecteur d'indices, de taille `partie entière de (2/3)*n', 
#les indices sont tirés de manière aléatoire sans remise dans l'ensemble {1,2, ...,n} 
indices

#on choisit ici de prendre 2/3 *n observations pour l'apprentissage, et le reste pour le test (ou la validation)
#1
ensemble_apprentissage <- diamonds[indices, ] #la partie de la base de données `Auto` qui va servir pour l'apprentissage
str(ensemble_apprentissage)

ensemble_validation <- diamonds[ - indices, ] #la base de validation
str(ensemble_validation)

#2
modele1 <- lm(formula = price ~ 1+cut+color+clarity+carat+depth+table+x, data = ensemble_apprentissage) 
modele1

#estimation de l'erreur du modele1
#3
valeurs_predites <- predict(object = modele1, newdata = ensemble_validation) 
str(valeurs_predites)
valeurs_predites

#4
estimation_erreur_modele1 <- mean((ensemble_validation$price - valeurs_predites)^2)  
estimation_erreur_modele1

# 5
indices <- sample(x = n, size = trunc((2/3)*n), replace = FALSE)
ensemble_apprentissage <- diamonds[indices, ]
ensemble_validation <- diamonds[ - indices, ]
modele1 <- lm(formula = price ~ 1+cut+color+clarity+carat+depth+table+x, data = ensemble_apprentissage)
valeurs_predites <- predict(object = modele1, newdata = ensemble_validation)
estimation_erreur_modele1 <- mean((ensemble_validation$price - valeurs_predites)^2)  
estimation_erreur_modele1
##### comment améliorer la méthode   ####
M <- 1500
erreur_modele1 = NULL
for (i in 1:M)
{
  indices <- sample(x = n, size = trunc((2/3)*n), replace = FALSE)
  ensemble_apprentissage <- diamonds[indices, ]
  ensemble_validation <- diamonds[ - indices, ]
  modele1 <- lm(formula = price ~ 1+cut+color+clarity+carat+depth+table+x, data = ensemble_apprentissage)
  valeurs_predites <- predict(object = modele1, newdata = ensemble_validation)
  erreur_modele1[i] <- mean((ensemble_validation$price - valeurs_predites)^2)
}
Err = NULL
for (m in 1:M)
{Err[m] = mean(erreur_modele1[1:m])
}
plot(Err, type = 'l')
Erreur_prevision_modele1 = Err[M]
print(Erreur_prevision_modele1)

#############################################################################
#############################################################################
#### Méthode 2 : estimation de l'erreur par leave-one-out VC (LOOCV), i.e., la K-fold CV avec K = n, le nombre d'observations ####
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

#### Méthode 3 : estimation de l'erreur des trois modèles précédents par K-fold CV, avec K = 10 ####
# 1
estimation_erreur_modele1 <- cv.glm(data = diamonds, glmfit = modele1, K = 10)$delta[1] #estimation de l'erreur du modele1 par la méthode  K-fold CV


print(c("Résultats des estimations par 10-fold CV : ",
        paste("Estimation de l'erreur du modele1 = ", as.character(estimation_erreur_modele1))))

# 2
estimation_erreur_modele1 <- cv.glm(data = diamonds, glmfit = modele1, K = 10)$delta[1] #estimation de l'erreur du modele1 par la méthode  K-fold CV

print(c("Résultats des estimations par 10-fold CV : ",
        paste("Estimation de l'erreur du modele1 = ", as.character(estimation_erreur_modele1)))) 


###################estimation de l'erreur du modele2#########################
#################################################################################

#### Méthode1 : estimation de l'erreur de prévision par l'approche de l'ensemble de validation ####
indices <- sample(x = n, size = trunc((2/3)*n), replace = FALSE) #vecteur d'indices, de taille `partie entière de (2/3)*n', 
#les indices sont tirés de manière aléatoire sans remise dans l'ensemble {1,2, ...,n} 
indices

#on choisit ici de prendre 2/3 *n observations pour l'apprentissage, et le reste pour le test (ou la validation)
#1
ensemble_apprentissage <- diamonds[indices, ] #la partie de la base de données `Auto` qui va servir pour l'apprentissage
str(ensemble_apprentissage)

ensemble_validation <- diamonds[ - indices, ] #la base de validation
str(ensemble_validation)

#2
modele2 <- lm(formula = price ~ 1+cut+color+clarity+carat+depth+table+x+z, data = ensemble_apprentissage) 
modele2

#estimation de l'erreur du modele2
#3
valeurs_predites <- predict(object = modele2, newdata = ensemble_validation) 
str(valeurs_predites)
valeurs_predites

#4
estimation_erreur_modele2 <- mean((ensemble_validation$price - valeurs_predites)^2)  
estimation_erreur_modele2

# 5
indices <- sample(x = n, size = trunc((2/3)*n), replace = FALSE)
ensemble_apprentissage <- diamonds[indices, ]
ensemble_validation <- diamonds[ - indices, ]
modele2 <- lm(formula = price ~ 1+cut+color+clarity+carat+depth+table+x+z, data = ensemble_apprentissage)
valeurs_predites <- predict(object = modele2, newdata = ensemble_validation)
estimation_erreur_modele2 <- mean((ensemble_validation$price - valeurs_predites)^2)  
estimation_erreur_modele2
##### comment améliorer la méthode   ####
M <- 1500
erreur_modele2 = NULL
for (i in 1:M)
{
  indices <- sample(x = n, size = trunc((2/3)*n), replace = FALSE)
  ensemble_apprentissage <- diamonds[indices, ]
  ensemble_validation <- diamonds[ - indices, ]
  modele2 <- lm(formula = price ~ 1+cut+color+clarity+carat+depth+table+x+z, data = ensemble_apprentissage)
  valeurs_predites <- predict(object = modele2, newdata = ensemble_validation)
  erreur_modele2[i] <- mean((ensemble_validation$price - valeurs_predites)^2)
}
Err = NULL
for (m in 1:M)
{Err[m] = mean(erreur_modele2[1:m])
}
plot(Err, type = 'l')
Erreur_prevision_modele2 = Err[M] #meilleur estimation de l'erreur de prévision du modele1
print(Erreur_prevision_modele2)
print(Erreur_prevision_modele2)

#############################################################################
#############################################################################
#### Méthode 2 : estimation de l'erreur par leave-one-out VC (LOOCV), i.e., la K-fold CV avec K = n, le nombre d'observations ####
# 1
modele2 <- glm(formula = price ~ 1+cut+color+clarity+carat+depth+table+x +z, data = diamonds)
modele2

# 2
library(boot) #on charge le package `boot` pour pouvoir utiliser la fonction cv.glm()
estimation_erreur_modele2 <- cv.glm(data = diamonds, glmfit = modele2, K = 10)$delta[1] #estimation de l'erreur du modele1 par la méthode LOOCV
estimation_erreur_modele2

# 3
estimation_erreur_modele2 <- cv.glm(data = diamonds, glmfit = modele2, K = 10)$delta[1] #estimation de l'erreur du modele1 par la méthode LOOCV
estimation_erreur_modele2

# 6
print(c("Résultats des estimations par LOOCV : ",
        paste("Estimation de l'erreur du modele2 = ", as.character(estimation_erreur_modele2)))) 

#### Méthode 3 : estimation de l'erreur des trois modèles précédents par K-fold CV, avec K = 10 ####
# 1
estimation_erreur_modele2 <- cv.glm(data = diamonds, glmfit = modele2, K = 10)$delta[1] #estimation de l'erreur du modele2 par la méthode  K-fold CV


print(c("Résultats des estimations par 10-fold CV : ",
        paste("Estimation de l'erreur du modele2 = ", as.character(estimation_erreur_modele2))))

# 2
estimation_erreur_modele2 <- cv.glm(data = diamonds, glmfit = modele2, K = 10)$delta[1] #estimation de l'erreur du modele2 par la méthode  K-fold CV

print(c("Résultats des estimations par 10-fold CV : ",
        paste("Estimation de l'erreur du modele2 = ", as.character(estimation_erreur_modele2)))) 

###conclusion: pour le deuxieme modele2: On remarque que méthode de 
#              K-fold donne l'erreur la plus petite qui est de méme pas loin 
#              la valeur de l'erreur de la methode LOOCV


#En comparant maintenant l'erreur entre les deux modèle, ou en deduisant que 
#               le modèle 2 donne une erreur plus petit avec la methode de K-fold



##############utilisant Bootstrap -modele1#################
f_estimateurs_w <- function(data, index){return(coef(lm(formula = price ~ 1+cut+color+clarity+carat+depth+table+x, data = data, 
                                                        subset = index)))}
f_estimateurs_w(data = diamonds, index = 1:53940)

f_estimateurs_w(data = diamonds, index = sample(53940, 53940, replace = TRUE))

f_estimateurs_w(data = diamonds, index = sample(53940, 53940, replace = TRUE))

boot(data = diamonds, statistic = f_estimateurs_w, R = 2000)

#comparaison avec les estimations des écarts-type données par la lm() 
modele <- lm(formula = price ~ 1+cut+color+clarity+carat+depth+table+x, data = diamonds)
summary(modele)

##############utilisant Bootstrap -modele2#################
f_estimateurs_w <- function(data, index){return(coef(lm(formula = price ~ 1+cut+color+clarity+carat+depth+table+x+z, data = data, 
                                                        subset = index)))}
f_estimateurs_w(data = diamonds, index = 1:53940)

f_estimateurs_w(data = diamonds, index = sample(53940, 53940, replace = TRUE))

f_estimateurs_w(data = diamonds, index = sample(53940, 53940, replace = TRUE))

boot(data = diamonds, statistic = f_estimateurs_w, R = 2000)

#comparaison avec les estimations des écarts-type données par la lm() 
modele <- lm(formula = price ~ 1+cut+color+clarity+carat+depth+table+x, data = diamonds)
summary(modele)
