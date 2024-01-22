# Entete ----

# Fichier : data_preparation.R
# Desc : Preparation de la base de donnees (importations en disk.frame car trop volumineuses)
#         Fusion des bases concernées et exportation au format .csv pour l'analyse factorielle
# Date : 17/11/2023
# Auteur : Elif Ertas (elif.ertas@etudiant.univ-reims.fr)

#--##--##--###--##--##--###--##--##--###--##--##--###--##--##--###--##--##--###--##--##--###--##--##--###--##-

# Importer les librairires nécessaires -----

library(disk.frame)
library(dplyr)

#--##--##--###--##--##--###--##--##--###--##--##--###--##--##--###--##--##--###--##--##--###--##--##--###--##-

# Importation et manipulations de title_basics -----  

# Importer le fichier tsv title_basics dans un disk.frame
title_basics <- as.disk.frame(read.delim("data/title_basics.tsv", sep = "\t"))

# Remplacer "\N" par NA dans toutes les colonnes
title_basics <- title_basics %>%
  mutate_all(~na_if(., "\\N"))

# Supprimer les lignes avec des valeurs manquantes dans title_basics
title_basics_clean <- na.omit(title_basics)

# Filtrer la base de données en fonction de la variable startYear égale à 2018
title_basics_clean <- filter(title_basics_clean, startYear == 2018)

# Convertir le disk.frame en data.frame
data_filtered_basics_df <- as.data.frame(title_basics_clean)

# Exporter le data.frame en CSv
write.csv(data_filtered_basics_df, "data/data_filtered_basics.csv", row.names = FALSE)

#--##--##--###--##--##--###--##--##--###--##--##--###--##--##--###--##--##--###--##--##--###--##--##--###--##-

# Importation et manipulations de title_ratings -----  

# Importer le fichier tsv title_ratings dans un disk.frame 
title_ratings <- as.disk.frame(read.delim("data/title_ratings.tsv", sep = "\t"))

# Remplacer "\N" par NA dans toutes les colonnes
title_ratings <- title_ratings %>%
  mutate_all(~na_if(., "\\N"))

# Supprimer les lignes avec des valeurs manquantes dans title_ratings
title_ratings_clean <- na.omit(title_ratings)

# Convertir le disk.frame en data.frame
data_filtered_ratings_df <- as.data.frame(title_ratings_clean)

# Exporter le data.frame en CSv
write.csv(data_filtered_ratings_df, "data/data_filtered_ratings.csv", row.names = FALSE)

# Fusion des deux dataframes basics et ratings -----  

# Fusion des dataframes basics et ratings en utilisant la variable 'tconst'
merged_data <- merge(data_filtered_ratings_df, data_filtered_basics_df, by = 'tconst')

#--##--##--###--##--##--###--##--##--###--##--##--###--##--##--###--##--##--###--##--##--###--##--##--###--##-

# Importation et manipulations de title_akas -----  

# Importer le fichier tsv title_akas dans un disk.frame 
title_akas <- as.disk.frame(read.delim("data/title_akas.tsv", sep = "\t"))

# Remplacer "\N" par NA dans toutes les colonnes
title_akas <- title_akas %>%
  mutate_all(~na_if(., "\\N"))

# Supprimer les lignes avec des valeurs manquantes dans title_akas
title_akas_clean <- na.omit(title_akas)

# Filtrer la base de données en fonction de la variable isOriginalTitle égale à 1
title_akas_clean <- filter(title_akas_clean, isOriginalTitle == 1)

# Convertir le disk.frame en data.frame
data_filtered_akas_df <- as.data.frame(title_akas_clean)

# Exporter le data.frame en CSv
write.csv(data_filtered_akas_df, "data/data_filtered_akas.csv", row.names = FALSE)

#--##--##--###--##--##--###--##--##--###--##--##--###--##--##--###--##--##--###--##--##--###--##--##--###--##-

# Fusion des deux dataframes merged_data et akas -----  

# Fusion des dataframes merged_data et akas en utilisant la variable 'tconst' pour merged_data et titleId pour akas
merged_data2 <- merge(merged_data, data_filtered_akas_df, by.x = 'tconst', by.y = 'titleId')

#--##--##--###--##--##--###--##--##--###--##--##--###--##--##--###--##--##--###--##--##--###--##--##--###--##-

# Importation et manipulations de title_crew -----  

# Importer le fichier tsv title_crew dans un disk.frame 
title_crew <- as.disk.frame(read.delim("data/title_crew.tsv", sep = "\t"))

# Remplacer "\N" par NA dans toutes les colonnes
title_crew <- title_crew %>%
  mutate_all(~na_if(., "\\N"))

# Supprimer les lignes avec des valeurs manquantes dans title_crew
title_crew_clean <- na.omit(title_crew)

# Convertir le disk.frame en data.frame
data_filtered_crew_df <- as.data.frame(title_crew_clean)

# Exporter le data.frame en CSv
write.csv(data_filtered_crew_df, "data/data_filtered_crew.csv", row.names = FALSE)

#--##--##--###--##--##--###--##--##--###--##--##--###--##--##--###--##--##--###--##--##--###--##--##--###--##-

# Fusion des deux dataframes merged_data2 et crew -----  

# Fusion des dataframes merged_data2 et crew en utilisant la variable 'tconst' 
merged_data3 <- merge(merged_data2, data_filtered_crew_df, by = 'tconst')

# Filtrer les lignes en ne gardant que les lignes ayant qu'un seul directors et writers, et enlever celles n'en ayant pas. 
merged_data3 <- merged_data3 %>%
  filter(!grepl(",", directors) & !grepl(",", writers) & !is.na(directors) & !is.na(writers) & writers != "\\N" & directors != "\\N")

# Enlever la variable ordering et writers 
merged_data3 <- merged_data3 %>%
  select(-ordering, -writers)

#--##--##--###--##--##--###--##--##--###--##--##--###--##--##--###--##--##--###--##--##--###--##--##--###--##-

# Importation et manipulations de title_principals -----  

# Importer le fichier tsv title_principals dans un disk.frame 
title_principals <- as.disk.frame(read.delim("data/title_principals.tsv", sep = "\t"))

# Remplacer "\N" par NA dans toutes les colonnes
title_principals <- title_principals %>%
  mutate_all(~na_if(., "\\N"))

# Supprimer les lignes avec des valeurs manquantes dans title_principals
title_principals_clean <- na.omit(title_principals)

# Convertir le disk.frame en data.frame
data_filtered_principals_df <- as.data.frame(title_principals_clean)

# Exporter le data.frame en CSv
write.csv(data_filtered_principals_df, "data/data_filtered_principals.csv", row.names = FALSE)

#--##--##--###--##--##--###--##--##--###--##--##--###--##--##--###--##--##--###--##--##--###--##--##--###--##-

# Importation et manipulations de name_basics -----  

# Importer le fichier tsv name_basics dans un disk.frame 
name_basics <- as.disk.frame(read.delim("data/name_basics.tsv", sep = "\t"))

# Remplacer "\N" par NA dans toutes les colonnes
name_basics <- name_basics %>%
  mutate_all(~na_if(., "\\N"))

# Supprimer les lignes avec des valeurs manquantes dans name_basics
name_basics_clean <- na.omit(name_basics)

# Convertir le disk.frame en data.frame
data_filtered_nbasics_df <- as.data.frame(name_basics_clean)

# Exporter le data.frame en CSv
write.csv(data_filtered_nbasics_df, "data/data_filtered_nbasics.csv", row.names = FALSE)

#--##--##--###--##--##--###--##--##--###--##--##--###--##--##--###--##--##--###--##--##--###--##--##--###--##-

# Fusion des deux dataframes merged_data3 et name_basics -----  

# Fusion des dataframes merged_data3 et name_basics en utilisant la variable 'tconst'
merged_data4 <- merge(merged_data3, data_filtered_nbasics_df, by.x = "directors", by.y = "nconst", all.x = TRUE)

#--##--##--###--##--##--###--##--##--###--##--##--###--##--##--###--##--##--###--##--##--###--##--##--###--##-

# Mettre en évidence les valeurs manquantes ------

# Remplacer \N par NA dans votre dataframe
final_merged_data <- data.frame(lapply(merged_data4, function(x) {
  x[x == "\\N"] <- NA
  return(x)
}))

# Suppressions des variables non utiles  ------

final_merged_data <- subset(final_merged_data, select = -c(primaryTitle, originalTitle, startYear, endYear, region, language, types, attributes, isOriginalTitle, knownForTitles))

#--##--##--###--##--##--###--##--##--###--##--##--###--##--##--###--##--##--###--##--##--###--##--##--###--##-

# Fonction pour extraire le premier bloc avant la virgule
extract_first_block <- function(value) {
  if (!is.na(value)) {
    # Recherche du premier bloc avant la virgule
    extracted_block <- strsplit(value, ",")[[1]][1]
    return(extracted_block)
  }
  return('')
}

# Appliquer la fonction d'extraction à la colonne 'genre'
final_merged_data$FirstGenre <- sapply(final_merged_data$genres, extract_first_block)

# Organisation des variables ------

final_merged_data <- final_merged_data %>%
  select(tconst, title, FirstGenre, genres, titleType, runtimeMinutes, isAdult, averageRating, numVotes, directors, primaryName, birthYear, deathYear, primaryProfession)


# Supprimer les lignes contenant des valeurs manquantes (NA) dans toutes les variables sauf "deathYear"
final_merged_data <- final_merged_data[complete.cases(final_merged_data[, !colnames(final_merged_data) %in% "deathYear"]), ]

#--##--##--###--##--##--###--##--##--###--##--##--###--##--##--###--##--##--###--##--##--###--##--##--###--##-

# Exporter le data frame final_merged_data en fichier CSV -----

write.csv(final_merged_data, file = "data/data.csv", row.names = FALSE)
