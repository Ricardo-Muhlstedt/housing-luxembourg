library(tidyverse)
library(readxl)
library(roxygen2)
library(janitor)

file_name <- "data\\input\\vente-maison-2010-2021.xlsx"

sheets <- excel_sheets(file_name)

read_clean <- function(..., sheet){
  read_excel(..., sheet = sheet) %>%
    mutate(year = sheet)
}

data <- map(
  sheets,
  ~read_clean(file_name,
              skip = 10,
              sheet = .)
) %>%
  bind_rows() %>%
  clean_names()

data <- data %>%
  rename(locality = commune,
         n_offers = nombre_doffres,
         average_price_nominal_euros = prix_moyen_annonce_en_courant,
         average_price_m2_nominal_euros = prix_moyen_annonce_au_m2_en_courant,
         average_price_m2_nominal_euros = prix_moyen_annonce_au_m2_en_courant
         ) %>%
  mutate(locality = str_trim(locality)) %>%
  select(year, locality, n_offers, starts_with("average"))
  
data <- data %>%
  mutate(locality = ifelse(grepl("Luxembourg-Ville", locality),
                           "Luxembourg",
                           locality),
         localitt = ifelse(grepl("P.tange", locality),
                           "Pétange",
                           locality)
         ) %>%
  mutate(across(starts_with("average"), as.numeric))


data <- data %>%
  filter(!grepl("Source", locality))

commune_level_data <- data %>%
  filter(!grepl("nationale|offres", locality),
         !is.na(locality))

country_data <- data %>%
  filter(!grepl("nationale", locality)) %>%
  select(-n_offers)

offers_country <- data %>%
  filter(grepl("Total d.offres", locality)) %>%
  select(year, n_offers)

country_level_data <- full_join(country_data, offers_country) %>%
  select(year, locality, n_offers, everything()) %>%
  mutate(locality = "Grand-Duchy of Luxembourg")

## First time scraping tables from web pages
current_communes <- "https://en.wikipedia.org/wiki/List_of_communes_of_Luxembourg" %>%
  rvest::read_html() %>%
  rvest::html_table() %>%
  purrr::pluck(1) %>%
  clean_names()
  
former_communes <- "https://en.wikipedia.org/wiki/Communes_of_Luxembourg#Former_communes" %>%
  rvest::read_html() %>%
  rvest::html_table() %>%
  purrr::pluck(3) %>%
  clean_names() %>%
  filter(year_dissolved > 2009)

communes <- unique(c(former_communes$name, current_communes$commune))

communes[which(communes == "Clemency")] <- "Clémency"
communes[which(communes == "Redange")] <- "Redange-sur-Attert"
communes[which(communes == "Erpeldange-sur-Sûre")] <- "Erpeldange"
communes[which(communes == "Luxembourg-City")] <- "Luxembourg"
communes[which(communes == "Käerjeng")] <- "Kaerjeng"
communes[which(communes == "Petange")] <- "Pétange"

write.csv(commune_level_data, "data//commune_level_data.csv", row.names = TRUE)
write.csv(country_level_data, "data//country_level_data.csv", row.names = TRUE)
