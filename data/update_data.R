# load required packages
library(readr)
library(dplyr)
library(tools)
library(stringr)
library(ggplot2)
library(tidyr)

# load and process data
laureates <- read_csv("http://api.nobelprize.org/v1/laureate.csv") %>%
  mutate(
    born_country_original = bornCountry,
    born_country_cleaned = ifelse(grepl("now ", bornCountry),
      word(bornCountry, -1), bornCountry
    ),
    born_country_cleaned = gsub(")", "", born_country_cleaned),
    born_city_original = bornCity,
    born_city_cleaned = ifelse(grepl("now ", bornCity),
      word(bornCity, -1), bornCity
    ),
    born_city_cleaned = gsub(")", "", born_city_cleaned),
    died_country_original = diedCountry,
    died_country_cleaned = ifelse(grepl("now ", diedCountry),
      word(diedCountry, -1), diedCountry
    ),
    died_country_cleaned = gsub(")", "", died_country_cleaned),
    died_city_original = diedCity,
    died_city_cleaned = ifelse(grepl("now ", diedCity),
      word(diedCity, -1), diedCity
    ),
    died_city_cleaned = gsub(")", "", died_city_cleaned),
    city_original = city,
    city_cleaned = ifelse(grepl("now ", city), word(city, -1), city),
    city_cleaned = gsub(")", "", city_cleaned),
    country_original = country,
    country_cleaned = ifelse(grepl("now ", country), word(country, -1), country),
    country_cleaned = gsub(")", "", country_cleaned),
    category = toTitleCase(category),
    instance = paste0(id, "_", year, "_", category)
  ) %>%
  arrange(id, year, category) %>%
  mutate(
    overallMotivation = gsub("\n", " ", overallMotivation),
    overallMotivation = gsub('"', "'", overallMotivation),
    motivation = gsub("\n", " ", motivation),
    motivation = gsub('"', "'", motivation),
    # if starts with "the ", remove it
    born_country_cleaned = ifelse(grepl("^the ", born_country_cleaned),
      sub("^the ", "", born_country_cleaned), born_country_cleaned
    ),
    died_country_cleaned = ifelse(grepl("^the ", died_country_cleaned),
      sub("^the ", "", died_country_cleaned), died_country_cleaned
    ),
    country_cleaned = ifelse(grepl("^the ", country_cleaned),
      sub("^the ", "", country_cleaned), country_cleaned
    )
  )


# names(nobel_v1)
# names(laureates_dedupe)

# this selects only the first mentioned affiliation for each laureate/prize combination

cleaned_laureates <- subset(laureates, !duplicated(laureates$instance)) %>% select(
  id,
  firstname,
  surname,
  year,
  category,
  affiliation = name,
  city = city_cleaned,
  country = country_cleaned,
  born_date = born,
  died_date = died,
  gender,
  born_city = born_city_cleaned,
  born_country = born_country_cleaned,
  born_country_code = bornCountryCode,
  died_city = died_city_cleaned,
  died_country = died_country_cleaned,
  died_country_code = diedCountryCode,
  overall_motivation = overallMotivation,
  share,
  motivation,
  born_country_original,
  born_city_original,
  died_country_original,
  died_city_original,
  city_original,
  country_original
)

nobel <- cleaned_laureates
write_csv(nobel, "data/nobel.csv")
