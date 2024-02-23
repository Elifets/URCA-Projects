Délais moyens entre deux passages de trains dans le métro parisien
================

Ce dépôt contient des ressources aidant à la création de la
visualisation, sous la forme d’un diagramme « en haltères » (*dumbbell
chart*, en anglais), de l’évolution entre 2019 et 2023 des délais moyens
entre le passage de deux trains aux heures de pointe pour les quatorze
lignes principales du métro parisien.

Ces ressources sont utilisées dans le cadre d’une évaluation du master
SEP de l’URCA, visant à reproduire le plus fidèlement possible une
visualisation proposée par *Les Décodeurs* du quotidien *Le Monde*,
publiée dans [cet
article](https://www.lemonde.fr/les-decodeurs/article/2023/12/22/metros-parisiens-la-frequence-s-est-degradee-depuis-2019-la-plupart-des-lignes-n-atteignent-pas-leur-objectif_6207225_4355770.html).

## Contexte

Quelques éléments de contexte tirés des contrats entre la Région Île de
France et la RATP pour 2019 et 2023.

- La région Île de France définit des objectifs de service de transport
  public (métro, tramway, RER, bus) et les contractualise avec les
  opérateurs que sont la RATP et la SNCF. Ces derniers ont la charge de
  la mise œuvre des moyens à déployer en réponse aux objectifs. D’un
  point de vue pratique, pour le métro, le contrat entre la région et la
  RATP définit pour chaque ligne des objectifs en termes de
  *trains-kilomètres customer* (TKC), c’est-à-dire, en nombre de
  kilomètres parcourus sur une année par tous les trains d’une ligne, en
  distinguant les heures de pointe des autres horaires.
- Les heures de pointe sont définies comme les créneaux 7h30–9h30 puis
  16h30–19h30 des jours ouvrés [^1] et jours d’événements planifiés[^2].
- Le contrat instaure l’obligation pour l’opérateur de publier
  mensuellement les performances réalisées dans les « bulletins de
  ponctualité »[^3]. Concrètement, la RATP publie la proportion de
  l’objectif de TKC réalisé au cours du mois, puis depuis le début de
  l’année.
- La RATP est payée par la région pour ces services selon un système de
  bonus/malus, fonction de la proportion de l’objectif de TKC réalisé
  chaque mois.

## Contenu du dépôt

Trois dossiers sont présents :

- le dossier `data/` contient trois fichiers de données :
  - `vacances.csv` présente sous forme d’un jeu de données à deux
    colonnes les dates et libellés des jours de vacances scolaires (pour
    Paris) et des jours fériés en 2019 et 2023 ;
  - `evenements.csv` présente sous forme d’un jeu de données à deux
    colonnes les dates et libellés des jours auxquels une manifestation
    culturelle nécessite la mise en place par la RATP d’un service de
    transport similaire aux jours ouvrés avec heures de pointe ;
  - `metro.csv` est un jeu de données de 6 variables sur les 14
    principales lignes de métro parisiennes. Ces variables sont :
    - `id_ligne` : identifiant de la ligne ;
    - `ligne` : nom de la ligne ;
    - `TKC2019` et `TKC2023` : les objectifs de TKC contractualisés
      entre la région et la RATP pour les années 2019 et 2023, exprimés
      en million de kilomètres ;
    - `ponct19` et `ponct23` : les proportions des objectifs de TKC
      réalisés au cours des années 2019 et 2023, exprimés en
      pourcentage. (Ces pourcentages sont supérieurs à 100 lorsque
      l’objectif a été dépassé.)
- le dossier `R/` contient le script `scraping_longueurs_metro.R` dans
  lequel est définie la fonction `scrap_long()` permettant de scraper
  les longueurs des lignes de métro depuis les notices Wikipédia des
  lignes de métro ;
- le dossier `icons/` contient des miniatures des logos des quatorze
  lignes principales du métro parisien..

[^1]: Jour ouvré : jour de l’année hors période de vacances scolaires,
    jours fériés, samedis et dimanches.

[^2]: Événement planifié : manifestation d’envergure identifiée dans le
    contrat, tel que la foire de Paris, le salon de l’agriculture, etc.

[^3]: Les bulletins de ponctualité de la RATP pour les années 2022 et
    2023 sont consultables sur l’internet à [cette
    adresse](https://www.iledefrance-mobilites.fr/la-qualite-de-service-en-chiffres-bulletin-d-information-trimestriel-bulletin-de-la-ponctualite).
