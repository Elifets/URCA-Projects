# Entete ----

# Fichier : detection_ruptures_waves.R
# Desc : Mémoire de recherche sur la détection de rupture - M1 SEP - 2022-2023
# Date : 10/04/2023
# Auteur : Elif Ertas (elif.ertas@etudiant.univ-reims.fr)

#Librairie ----

library(changepoint)
library(ggplot2)

#Partie 2 : Méthode offline de détection de rupture ------

va1 <- rnorm(100, mean = 4, sd = 1)
va2 <- rnorm(100, mean = 8, sd = 1)
va3 <- rnorm(100, mean = 2, sd = 1)

va <- c(va1,va2,va3)
exemple1 <- cpt.mean(va, method = "PELT")
plot(exemple1, col="blue", lwd = 1, xlab = "x", ylab = "y", main = "Détection de rupture sur la moyenne de 3 échantillons")

#Partie 4 : Detection de rupture sur la hauteur des vagues -----

##2. Base de donnée sur les vagues -----

waves <- wave.c44137[1:5000]
waves_tot <- wave.c44137

plot(wave_tot, type="l", lwd = 2, col = "blue", xlab = "Temps en heure (à partir de 2005)", ylab = "Hauteur en mètres",main = "Hauteur des vagues")
resultats_tot <- cpt.mean(waves_tot, method = "PELT")
plot(resultats_tot, type="l", lwd = 2, col = "gray", xlab = "Temps en heure (à partir de 2005)", ylab = "Hauteur en mètres",main = "Détection de rupture sur la hauteur des vagues")


plot(waves, type="l", lwd = 2, col = "blue", axes = FALSE, xlab = "Temps en heure (2005)", ylab = "Hauteur en mètres",main = "Hauteur des vagues")
new_labels <- c("janvier", "février", "mars", "avril", "mai", "juin","juillet","aout")
new_ticks <- c(0, 720, 720*2, 3*720, 4*720, 5*720, 6*720, 7*720)
axis(1, at = new_ticks, labels = new_labels)
axis(2)

resultats <- cpt.mean(waves, method = "PELT")
plot(resultats, type="l", lwd = 2, col = "gray", axes = FALSE, xlab = "Temps en heure (2005)", ylab = "Hauteur en mètres",main = "Détection de rupture sur la hauteur des vagues")
axis(1, at = new_ticks, labels = new_labels)
axis(2)

PELT(waves, penalty = "AIC", cost_func = "norm.mean")
