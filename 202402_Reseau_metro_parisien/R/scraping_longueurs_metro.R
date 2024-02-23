


#' Extrait la longueur d'une ligne du métro parisien de sa page Wikipedia
#'
#' @param path L'adresse URL de la notice Wikipédia associée à une ligne de métro parisienne
#'
#' @return Une valeur numérique représentant la longueur de la ligne en kilomètre.
#' @import rvest
#' @examples
#' path <- "https://fr.wikipedia.org/wiki/Ligne_1_du_m%C3%A9tro_de_Paris"
#' scrap_long(path)

scrap_long <- function(path) {
  page <- read_html(path)
  page %>%
    html_elements("td") %>%
    html_text2() %>%
    str_subset(pattern = "^[:digit:]+,?[:digit:]+ km") %>%
    str_extract(pattern = "^[:digit:]+,?[:digit:]+") %>%
    str_replace(pattern = ",", replacement = ".") %>%
    as.numeric()
}




