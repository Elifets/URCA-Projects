# * Entete ----

# Fichier : ERTAS_ELIF-PROJET_SEP0831.R
# Desc : Projet SEP0831 - Statistique appliquée - M1 SEP - 2022-2023
# Date : 14/03/2023
# Auteur : Elif Ertas (elif.ertas@etudiant.univ-reims.fr)

# * Librairie -----

library(readr) 
library(ggplot2)
library(esquisse)
library(Hmisc)
library(dplyr)
library(gghalves)
library(nortest) # test Anderson-Darling
library(moments) # test Jarque-Berra 
library(ggpubr) # compare_means
library(mvnormtest) # mshapiro.test


#Partie 1 : Lecture et affectation -----

data <- read_csv("Spotify2010-2019_Top100.csv", col_types = cols(bpm = col_double(), 
                                                                 nrgy = col_double(), dnce = col_double(), 
                                                                 dB = col_double()))
Spotify <- as.data.frame(data)
save(Spotify, file = "Spotify.Rdata")
load("Spotify.Rdata")
str(Spotify)  # pour vérifier la nature des variables importées
names(Spotify)   #liste des noms de variables

str(Spotify)
attach(Spotify) 
View(Spotify)

nb_music <- nrow(Spotify)
save(nb_music,file="nb_music.Rdata")


# Partie 2 : Etude des données -----

##2.1. La répartition des artistes -----

labels <- unique(Spotify$artist_type)
new_labels <- c("Groupe","Duo","Solo","Trio")
new_vector <- factor(unique(Spotify$artist_type), levels = new_labels)
pct <- round(prop.table(table(Spotify$artist_type)) * 100, 1)
lbl <- paste0(new_labels, " ", pct, "%")
colors <- c("red", "green", "blue","orange")
pie(table(Spotify$artist_type), labels = lbl, col = colors, main = "La répartition des types d'artistes",cex.main = 0.8) -> RepartArtiste
save(RepartArtiste,file="RepartArtiste.Rdata")
# On observe que le type d'artiste qui domine est le Solo à 75% 
# Suivi par le type Groupe a 17%, le type Duo à 7% puis pour finir les trio à 1% 

# Cette dominance est dû en particulier aux nombreuses collaborations entre artistes. En effet le type "Solo" prend en compte les 
# titres de musiques faites par un seul interprète tel que Rihanna mais aussi les titres de musiques faites en collaboration
# où l'artiste principal est un Solo tel que "Sucker for Pain" qui est un titre de Lil Wayne (Solo) 
# en collaboration avec Wiz Khalifa (Solo), Imagine Dragons (Groupe de 4) et d'autres artistes. 


##2.2. La popularité des musiques en fonction du type d'artiste ------

# https://www.europavox.com/fr/news/how-the-featured-artist-is-excelling-across-europe/ 
# D'après l'article, les artistes solo auraient tendance à produire des chansons plus populaires que les groupes, 
# notamment grâce aux nombreuses collaborations qui ont énormément du succès. 
# Cette popularité serait dû aux mélanges des genres musicales et donc une manière d'atteindre un nouveau public 
# pour tous les collaborateurs du titre de musique. 

# Etudions alors la popularité des musiques en fonction du type d'artiste 

###2.2.1. Représentation graphique -----
Spotify %>%
  ggplot(mapping = aes(x = artist_type, y = pop)) +
  labs(x = "Type d'artiste", y = "Popularité") +
  ggtitle("La popularité en fonction du type d'artiste") +
  geom_half_boxplot(side = "l", fill="pink") +
  geom_half_violin(side = "r") +
  stat_summary(fun = "mean", geom = "errorbar", 
               mapping = aes(ymax = ..y.., ymin = ..y..),
               linetype = "dashed", col = "red", lwd = 1, show.legend = TRUE) + 
  geom_point(shape = 16) +
  theme_bw() -> Pop_ArtisteType
Pop_ArtisteType
save(Pop_ArtisteType,file="Pop_ArtisteType.Rdata")
# On observe pas de grosses différences entre chaque type d'artiste 
# Il y a 13 outliers pour le type Solo, ce qui signifique qu'il y a 13 titres de musique
#   d'artistes solo qui ont une popularité nettement plus faible que les autres. 
# On a une moyenne assez similaire les unes aux autres 
# De plus on observe un défaut de symétrie pour le type d'artiste Trio 

###2.2.2. Tests de normalité -----
lapply(split(Spotify$pop, Spotify$artist_type), shapiro.test)
lapply(split(Spotify$pop, Spotify$artist_type), jarque.test)
lapply(split(Spotify$pop, Spotify$artist_type), ad.test)
# D'après le test de Anderson-Darling le type d'artiste Duo a une p-value de 0.1733 > 0.05 
# donc on accepterait l'hypothèse de normalité pour ce type. 
# Or il suffit qu'un test pour un seul type rejette l'hypothèse pour faire la procédure non paramètrique : 
# Tout les groupes ont une une p-value significative dans au moins un des tests,
# Ici les types Group, Trio et Solo rejette l'hypothèse de normalité dans tout les tests effectués 
# Le type Duo rejette l'hypothèse de normalité d'après les tests de Shapiro-Wilk et Jarque-Berra

# On en conclut que tous les types rejette l'hypothèse de normalité. 

###2.2.3. Tests de l'égalité des variances -----
# Comme on a rejeté l'hypothèse de normalité dans au moins un type (ici tous) 
# On va tester l’égalité des variances grâce au test de Fligner-Killeen
fligner.test(pop ~ artist_type, data = Spotify)
# On a une p-value = 0.3005 > 0.05 non significative donc on accepte l'égalité des variances 

###2.2.4. Test de comparaison des médianes -----
kruskal.test(pop ~ artist_type, data = Spotify)
# On a une p-value = 0.01012 significative donc on va comparer deux-à-deux les groupes
#   Il existe une relation de dependance significative entre la popularité et le type d'artiste
# On conclut qu'il existe une difference de popularité entre les types d'artistes  

###2.2.5. Comparaison deux à deux des groupes -----
compare_means(pop ~ artist_type, data = Spotify,
              method = "wilcox.test",
              p.adjust.method = "bonferroni") %>%
  select(-`.y.`, -method )
# On observe une significativité entre Duo - Solo et Duo - Group 
# On en conclut que la popularité pour le type Solo est significativement plus grande que pour le type Duo 
#   et que la popularité pour le type Groupe est significativement plus grande pour que pour le type Duo 

###2.2.6. Conclusion ------ 
# On a donc une tendance de popularité élevée dans les Groupes et Solo par rapport à Duo 
# Mais on ne peut se prononcer entre Groupe et Solo au vu de nos données. 
# On suppose que la non-significativité avec le groupe Trio est dû au manque de données (1% pour Trio)
# On conclut qu'il y a bien une différence de popularité entre les types d'artistes, cependant on ne peut 
# pas dire si un type est significativement plus populaire que tous les autres types. 

##2.3. La popularité en fonction de la danceability ------

# On cherche à savoir s'il y a une relation de dépendance entre la popularité et la danceability d'une musique

###2.3.1. Représentation graphique -----

Spotify %>%
  dplyr::select(pop, dnce) %>%
  identity() -> popdnce

Spotify %>%
  ggplot(mapping = aes(y = pop, x = dnce)) +
  labs(x = "Danceabilité", y = "Popularité") +
  ggtitle("Nuage de points avec une régression linéaire") +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() -> popdnce_np
popdnce_np
save(popdnce_np,file="popdnce_np.Rdata")
# On n'observe aucune relation particulière entre la popularité et la danceability

###2.3.2. Test de normalité -----
mshapiro.test(t(popdnce))
# p-value = 3.50e-16 < 0.05 significative donc on rejette l'hypothèse de normalité 

###2.3.3. Tests de non-correlation monotone -----
cor.test(Spotify$pop,Spotify$dnce, method = "kendall")
cor.test(Spotify$pop,Spotify$dnce, method = "spearman")
# p-value singificative pour les deux tests 
# donc il y a existence d'une relation monotone de dépendance entre la popularité et la danceabilité 

##2.4. La popularité en fonction de l'acoustique ------

# On cherche à savoir s'il y a une relation de dépendance entre la popularité et l'acoustique d'une musique

###2.4.1. Représentation graphique -----

Spotify %>%
  dplyr::select(pop, acous) %>%
  identity() -> popac

Spotify %>%
  ggplot(mapping = aes(y = pop, x = acous)) +
  labs(x = "Acoustique", y = "Popularité") +
  ggtitle("Nuage de points avec une régression linéaire") +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() -> popac_np
popac_np
save(popac_np,file="popac_np.Rdata")
# On n'observe aucune relation particulière entre la popularité et l'acoustique

###2.4.2. Test de normalité -----
mshapiro.test(t(popac))
# p-value < 2.2e-16 < 0.05 significative donc on rejette l'hypothèse de normalité 

###2.4.3. Tests de non-correlation monotone -----
cor.test(Spotify$pop,Spotify$acous, method = "kendall")
cor.test(Spotify$pop,Spotify$acous, method = "spearman")
# p-value singificative pour les deux tests 
# donc il y a existence d'une relation monotone de dépendance entre la popularité et l'acoustique 


#----------------------------------------------------------------------------#