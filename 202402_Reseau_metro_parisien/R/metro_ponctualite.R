# Entete ----

# Fichier : metro_ponctualite.R
# Desc : Interrogation écrite - Traitement et valorisation des données 
# Date : 05/02/2024
# Auteurs : Elif Ertas

# Librairies ----

library(readr)
library(rvest)
library(dplyr)
library(stringr)
library(lubridate)
library(hms)
library(ggplot2)
library(ggtext)

# Question 3. (a) ---- 

# Importation du fichier CSV en tant que tibble nommé metro
metro <- readr::read_csv2("data/metro.csv")

# Question 3. (b) ----

# Charger la fonction scrap_long depuis le fichier scraping_longueurs_metro.R
source("R/scraping_longueurs_metro.r")

# Stocker les noms des lignes de métro
lignes <- c("Ligne_1", "Ligne_2", "Ligne_3", "Ligne_4", "Ligne_5", 
            "Ligne_6", "Ligne_7", "Ligne_8", "Ligne_9", "Ligne_10", 
            "Ligne_11", "Ligne_12", "Ligne_13", "Ligne_14")

# Créer les URL à partir des noms de lignes
urls <- paste0("https://fr.wikipedia.org/wiki/", lignes, "_du_m%C3%A9tro_de_Paris")

# Initialiser un tibble pour stocker les résultats
resultats <- tibble::tibble(
  ligne = character(length(urls)),
  longueur = numeric(length(urls))
)


Pour inclure les longueurs de Ligne_3_bis et Ligne_7_bis dans les longueurs de Ligne_3 et Ligne_7 respectivement, vous pouvez suivre cette approche :

r
Copy code
# Charger la fonction scrap_long depuis le fichier scraping_longueurs_metro.R
source("R/scraping_longueurs_metro.r")

# Stocker les noms des lignes de métro
lignes <- c("Ligne_1", "Ligne_2", "Ligne_3", "Ligne_4", "Ligne_5", 
            "Ligne_6", "Ligne_7", "Ligne_8", "Ligne_9", "Ligne_10", 
            "Ligne_11", "Ligne_12", "Ligne_13", "Ligne_14")

# Créer les URL à partir des noms de lignes
urls <- paste0("https://fr.wikipedia.org/wiki/", lignes, "_du_m%C3%A9tro_de_Paris")

# Initialiser un tibble pour stocker les résultats
resultats <- tibble::tibble(
  ligne = character(length(lignes)),
  longueur = numeric(length(lignes))
)

# Appliquer la fonction scrap_long à tous les URLs
for (i in seq_along(urls)) {
  # Gérer les lignes Ligne_3 et Ligne_7
  if (lignes[i] == "Ligne_3") {
    longueur_3 <- scrap_long(urls[i])
    longueur_3_bis <- scrap_long(paste0("https://fr.wikipedia.org/wiki/Ligne_3_bis_du_m%C3%A9tro_de_Paris"))
    resultats$ligne[i] <- lignes[i]
    resultats$longueur[i] <- longueur_3 + longueur_3_bis
  } else if (lignes[i] == "Ligne_7") {
    longueur_7 <- scrap_long(urls[i])
    longueur_7_bis <- scrap_long(paste0("https://fr.wikipedia.org/wiki/Ligne_7_bis_du_m%C3%A9tro_de_Paris"))
    resultats$ligne[i] <- lignes[i]
    resultats$longueur[i] <- longueur_7 + longueur_7_bis
  } else {
    # Pour les autres lignes
    resultats$ligne[i] <- lignes[i]
    resultats$longueur[i] <- scrap_long(urls[i])
  }
}

# Question 3. (c) ----

metro$longueur <- resultats$longueur

# Question 4. ----

calendar <- tibble(date = c(seq(from = ymd("2019-01-01"), to = ymd("2019-12-31"), by = 1),
                            seq(from = ymd("2023-01-01"), to = ymd("2023-12-31"), by = 1)))

# Question 4. (a) ----

# Ajouter une variable logique weekend au tibble calendar
calendar$weekend <- wday(calendar$date) %in% c(1, 7)

# Question 4. (b) ----

#Importer les fichiers CSV evenements et vacances 
evenements <- readr::read_csv2("data/evenements.csv")
vacances <- readr::read_csv2("data/vacances.csv")

# Convertir les colonnes de dates en classe Date
evenements$date <- as.Date(evenements$date)
vacances$date <- as.Date(vacances$date)

# Ajouter une variable logique "evenement"
calendar$evenement <- calendar$date %in% evenements$date 

# Ajouter une variable logique "vacances"
calendar$vacances <- calendar$date %in% vacances$date 

# Question 4. (c) ----

# Calculer le nombre de jours pour lesquels les créneaux d'heures de pointe s'appliquent en 2019 et 2023
njhp <- calendar %>%
  group_by(annee = year(date)) %>%
  summarise(njhp = sum(evenement | (!weekend & !vacances & !evenement)))

# Afficher le tibble
print(njhp)

# Question 5. (a) ----

# Calculer nbtrains pour l'année 2019
metro$nbtrains19 <- (10^6 * metro$TKC2019 * metro$ponct19) / (100 * 5 * metro$longueur * njhp$njhp[njhp$annee == 2019])

# Calculer nbtrains pour l'année 2023
metro$nbtrains23 <- (10^6 * metro$TKC2023 * metro$ponct23) / (100 * 5 * metro$longueur * njhp$njhp[njhp$annee == 2023])

# Question 5. (b) ----

# Calculer le délai moyen entre les passages de deux trains pour l'année 2019
metro$delai19 <- hms(minute = 1 / metro$nbtrains19 * 60)

# Calculer le délai moyen entre les passages de deux trains pour l'année 2023
metro$delai23 <- hms(minute = 1 / metro$nbtrains23 * 60) 

# Afficher le tibble metro avec les variables delai19 et delai23 ajoutées
print(metro)

# Question 5. (c) ----

# Calculer la différence de délai entre 2019 et 2023
metro$delta <- metro$delai23 - metro$delai19

# Afficher le tibble metro avec la variable delta ajoutée
print(metro)

# Question 6. ----

# Trier le tibble metro par ordre décroissant de différence de délais
metro <- metro[order(metro$delta), ]

# Créer les étiquettes avec les icônes
labels <- paste0("<img src='icons/metro_", 1:14, "_small.png' width='25' />")

# Création du graphique dumbbell
dumbbell_chart <- ggplot(metro, aes(x = delta, y = reorder(ligne, delta))) +
  geom_point(aes(color = factor(ifelse(delta > 0, "2023", "2019"))), size = 3) +
  geom_segment(aes(x = 0, xend = delta, y = ligne, yend = ligne), color = "black", size = 1) +
  scale_color_manual(values = c("2019" = "purple", "2023" = "pink", "black" = "black"),
                     guide = FALSE) +
  labs(x = "Différence de délais (en minutes)", y = "Ligne de métro",
       color = "Année") +
  theme_minimal() +
  ggtitle("Représentation de l’évolution entre 2019 et 2023, métro parisien") +
  theme(axis.text.y = element_text(color = "black", size = 4, family = "Arial"),  # Correction de l'appel de la fonction element_text
        axis.text.x = element_text(size = 8),  # Réglage de la taille des étiquettes de l'axe x
        plot.title = element_text(hjust = 0.5)) +  # Centrage du titre
  scale_x_continuous(labels = scales::number_format(accuracy = 0.01)) +  # Utiliser les nombres comme étiquettes pour l'axe x avec une précision de 0.01
  geom_text(aes(label = paste(round(delta, 2), "min")), hjust = -0.2, size = 3) +
  scale_y_discrete(name = NULL, labels = paste0("<img src='icons/metro_", 1:14, "_small.png' width='25' />")[order(metro$delta)]) +  # Ajout des icônes pour les étiquettes de l'axe y
  theme(axis.text.y = ggtext::element_markdown(color = "black", size = 4))  # Utilisation de Markdown pour formater les étiquettes de l'axe y

# Affichage du graphique
print(dumbbell_chart)

# Question 7. ----

# Sources de données différentes : Le Monde pourrait utiliser des sources de données différentes, 
# avec des méthodes de collecte ou de traitement différentes, ce qui peut entraîner des résultats différents.
# Période de collecte des données : Les données utilisées dans cet exercice peuvent provenir d'une période différente de celle utilisée par Le Monde. 
# Les conditions de trafic, les horaires des transports en commun et d'autres facteurs peuvent varier d'une période à l'autre, ce qui peut influencer 
# les résultats de la ponctualité.



