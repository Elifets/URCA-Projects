# Entete ----

# Fichier : ADD_IMDb.R
# Desc : Analyse factorielle concernant les films sur la plateforme IMDb 
# Date : 17/11/2023
# Auteur : Elif Ertas (elif.ertas@etudiant.univ-reims.fr)

#--##--##--###--##--##--###--##--##--###--##--##--###--##--##--###--##--##--###--##--##--###--##--##--###--##-

# Importer les librairires nécessaires -----

library(Factoshiny)
library(tidyr)
library(dplyr)
library(ggplot2)

#--##--##--###--##--##--###--##--##--###--##--##--###--##--##--###--##--##--###--##--##--###--##--##--###--##-

# Importer la base de donnees data.csv -----

data <- read.csv("data/data.csv")
data <- subset(data, select = -c(deathYear))
df <- as.data.frame(data)

df_movies <- subset(df, grepl("^movie$", titleType, ignore.case = TRUE))
df_movies <- subset(df_movies, select = -c(tconst, title, FirstGenre, titleType, directors, primaryName))

# Séparer les genres en différentes colonnes
df_movies_dummies <- df_movies %>%
  separate_rows(genres, sep = ",") %>%  # Séparation des genres
  mutate(genre_present = 1) %>%  # Ajout d'une colonne indiquant la présence du genre
  pivot_wider(names_from = genres, values_from = genre_present, values_fill = 0)  # Pivoter pour obtenir les dummies

df_movies_dummies[is.na(df_movies_dummies)] <- 0

# Séparer les professions en différentes colonnes
df_movies_dummies <- df_movies_dummies %>%
  separate_rows(primaryProfession, sep = ",") %>%  # Séparation des professions
  mutate(profession_present = 1) %>%  # Ajout d'une colonne indiquant la présence de la profession
  pivot_wider(names_from = primaryProfession, values_from = profession_present, values_fill = 0)  # Pivoter pour obtenir les dummies

df_movies_dummies[is.na(df_movies_dummies)] <- 0

cor(df_movies_dummies)

# On observe que les variables sont très faiblement corrélés dans la globalité, donc il y a peu de chance que les variables aillent dans le même sens.

#--##--##--###--##--##--###--##--##--###--##--##--###--##--##--###--##--##--###--##--##--###--##--##--###--##-

# Analyse descriptive 

newDF <- df_movies_dummies[,c("runtimeMinutes","isAdult","averageRating","numVotes","Biography","Drama","History","Fantasy","Music","Comedy","Documentary","Horror","Mystery","Sport","Crime","Thriller","Action","Adventure","Romance","Animation","Family","Sci-Fi","Musical","Western","War","special_effects","make_up_department","script_department","editorial_department","casting_department","executive","sound_department","camera_department","music_department","production_manager","assistant_director","writer","producer","music_artist","art_director","production_designer","cinematographer","animation_department","visual_effects","stunts","composer","editor","soundtrack","actor","director","art_department","actress","miscellaneous")]
View(newDF)
write.csv(newDF, file = "data/newDF.csv", row.names = FALSE)

summary(newDF[c("averageRating", "numVotes", "runtimeMinutes")])

# Comptage du nombre de lignes pour chaque genre
genre_counts <- colSums(newDF[, c("Biography", "Drama", "History", "Fantasy", "Music", "Comedy", "Documentary", "Horror", "Mystery", "Sport", "Crime", "Thriller", "Action", "Adventure", "Romance", "Animation", "Family", "Sci-Fi", "Musical", "Western", "War")])
# Affichage du nombre de lignes pour chaque genre
print(genre_counts)
# Création d'un dataframe pour le graphique
genre_data <- data.frame(Genre = names(genre_counts), Count = genre_counts)
# Sélection des cinq genres les plus représentés
top5_genres <- head(genre_data[order(-genre_data$Count), ], 5)
# Création du graphique pour le top 5
ggplot(top5_genres, aes(x = reorder(Genre, -Count), y = Count)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Top 5 des genres les plus fréquents", x = "Genre", y = "Nombre de films") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Comptage du nombre de lignes pour chaque catégorie
category_counts <- colSums(newDF[, c("special_effects","make_up_department","script_department","editorial_department","casting_department","executive","sound_department","camera_department","music_department","production_manager","assistant_director","writer","producer","music_artist","art_director","production_designer","cinematographer","animation_department","visual_effects","stunts","composer","editor","soundtrack","actor","director","art_department","actress","miscellaneous")])
# Affichage du nombre de lignes pour chaque category
print(category_counts)
# Création d'un dataframe pour le graphique
category_data <- data.frame(Category = names(category_counts), Count = category_counts)
# Sélection des cinq genres les plus représentés
top5_category <- head(category_data[order(-category_data$Count), ], 5)
# Création du graphique pour le top 5
ggplot(top5_category, aes(x = reorder(Category, -Count), y = Count)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Top 5 des catégories les plus fréquentes", x = "Catégorie", y = "Nombre de films") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#--##--##--###--##--##--###--##--##--###--##--##--###--##--##--###--##--##--###--##--##--###--##--##--###--##-

# AFM 

#Factoshiny(newDF)

res.MFA<-MFA(newDF,group=c(2,2,21,28), type=c("s","s","s","s"),name.group=c("Caracteristiques","Note","Genres","Profession"),graph=FALSE)

summary(res.MFA)

# Graphe des groupes 
plot.MFA(res.MFA, choix="group",title="Graphe des groupes")
# Graphe des axes partiels 
plot.MFA(res.MFA, choix="axes",title="Graphe des axes partiels",habillage='group', shadow=TRUE)
# Cercle des corrélations 
plot.MFA(res.MFA, choix="var",habillage='group',title="Cercle des corrélations", cex=0.7, select="contrib 10", shadow=TRUE)
# Graphe des individus 
plot.MFA(res.MFA, choix="ind",lab.par=FALSE,title="Graphe des individus")
