# load required packages
library(readr)
library(dplyr)
library(tools)
library(stringr)
library(ggplot2)
library(tidyr)

# load and process data
laureates <- read_csv("https://api.nobelprize.org/v1/laureate.csv",
  col_types = cols(.default = col_character()),
  name_repair = "unique_quiet",
  lazy = TRUE
)


laureates <- laureates %>%
  mutate(
    year = as.integer(year),
    share = as.integer(share),
    id = as.integer(id),
    born = case_when(
      born == "0000-00-00" ~ NA_character_,
      TRUE ~ born
    ),
    born = as.Date(born),
    died = case_when(
      died == "0000-00-00" ~ NA_character_,
      TRUE ~ died
    ),
    died = as.Date(died),
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
# fill in missing birth dates based on external information
cleaned_laureates <- cleaned_laureates %>%
  mutate(
    died_date = case_when(
      firstname == "Nansen International Office for Refugees" & is.na(died_date) ~ as.Date("1939-01-01"),
      TRUE ~ died_date
    ),

    born_date = case_when(
      firstname == "Albert" & surname == "Lutuli" & is.na(born_date) ~ as.Date("1898-01-01"),
      firstname == "Louis" & surname == "Brus" & is.na(born_date) ~ as.Date("1943-08-10"),
      firstname == "A. Michael" & surname == "Spence" & is.na(born_date) ~ as.Date("1945-11-07"),
      # Venkatraman Ramakrishnan
      firstname == "Venkatraman" & surname == "Ramakrishnan" & is.na(born_date) ~ as.Date("1952-01-01"),
      # Saul        Perlmutter
      firstname == "Saul" & surname == "Perlmutter" & is.na(born_date) ~ as.Date("1959-09-22"),
      # Paul M.     Romer
      firstname == "Paul M." & surname == "Romer" & is.na(born_date) ~ as.Date("1955-11-06"),
      #  5 Michael     Houghton     NA        NA
      firstname == "Michael" & surname == "Houghton" & is.na(born_date) ~ as.Date("1949-01-01"),
      #    1 Ardem        Patapoutian NA        NA
      firstname == "Ardem" & surname == "Patapoutian" & is.na(born_date) ~ as.Date("1967-10-01"),
      #     2 Abdulrazak   Gurnah      NA        NA
      firstname == "Abdulrazak" & surname == "Gurnah" & is.na(born_date) ~ as.Date("1948-12-20"),
      #    3 David        Card        NA        NA
      firstname == "David" & surname == "Card" & is.na(born_date) ~ as.Date("1956-01-01"),
      #    4 Morten       Meldal      NA        NA
      firstname == "Morten" & surname == "Meldal" & is.na(born_date) ~ as.Date("1954-01-16"),
      #     5 Moungi       Bawendi     NA        NA
      firstname == "Moungi" & surname == "Bawendi" & is.na(born_date) ~ as.Date("1961-03-15"),
      #    6 Aleksey      Yekimov     NA        NA
      firstname == "Aleksey" & surname == "Yekimov" & is.na(born_date) ~ as.Date("1945-01-01"),
      #    7 Claudia      Goldin      NA        NA
      firstname == "Claudia" & surname == "Goldin" & is.na(born_date) ~ as.Date("1946-05-14"),
      #     8 Gary         Ruvkun      NA        NA
      firstname == "Gary" & surname == "Ruvkun" & is.na(born_date) ~ as.Date("1957-03-26"),
      #    9 David        Baker       NA        NA
      firstname == "David" & surname == "Baker" & is.na(born_date) ~ as.Date("1962-10-06"),
      #    10 John         Jumper      NA        NA
      firstname == "John" & surname == "Jumper" & is.na(born_date) ~ as.Date("1985-01-01"),
      #    11 Simon        Johnson     NA        NA
      firstname == "Simon" & surname == "Johnson" & is.na(born_date) ~ as.Date("1963-01-13"),
      #    12 James        Robinson    NA        NA
      firstname == "James" & surname == "Robinson" & is.na(born_date) ~ as.Date("1960-01-01"), # https://en.wikipedia.org/wiki/James_A._Robinson
      #    13 Mary E.      Brunkow     NA        NA
      firstname == "Mary E." & surname == "Brunkow" & is.na(born_date) ~ as.Date("1961-01-01"),
      #   14 John         Clarke      NA        NA
      firstname == "John" & surname == "Clarke" & is.na(born_date) ~ as.Date("1942-02-10"),
      #    15 Michel H.    Devoret     NA        NA
      firstname == "Michel H." & surname == "Devoret" & is.na(born_date) ~ as.Date("1953-01-01"),
      #    16 John M.      Martinis    NA        NA
      firstname == "John M." & surname == "Martinis" & is.na(born_date) ~ as.Date("1958-01-01"),
      #    17 Maria Corina Machado     NA        NA
      firstname == "Maria Corina" & surname == "Machado" & is.na(born_date) ~ as.Date("1967-10-07"),
      # Organizations
      #  1 Institute of International Law           NA        NA
      firstname == "Institute of International Law" & is.na(born_date) ~ as.Date("1873-09-08"),
      # 2 Permanent International Peace Bureau     NA        NA
      firstname == "Permanent International Peace Bureau" & is.na(born_date) ~ as.Date("1891-11-13"),
      # 3 International Committee of the Red Cross NA        NA
      firstname == "International Committee of the Red Cross" & is.na(born_date) ~ as.Date("1863-02-17"),
      # 6 Nansen International Office for Refugees NA        NA
      firstname == "Nansen International Office for Refugees" & is.na(born_date) ~ as.Date("1930-01-01"),
      # 7 Friends Service Council
      firstname == "Friends Service Council" & is.na(born_date) ~ as.Date("1947-01-01"),
      # 8 American Friends Service Committee       NA        NA
      firstname == "American Friends Service Committee" & is.na(born_date) ~ as.Date("1917-04-30"),
      # 9 League of Red Cross Societies            NA        NA
      firstname == "League of Red Cross Societies" & is.na(born_date) ~ as.Date("1919-05-05"),
      # 10 United Nations Children's Fund           NA        NA
      firstname == "United Nations Children's Fund" & is.na(born_date) ~ as.Date("1946-12-11"),
      #      1 International Labour Organization                    NA        NA
      firstname == "International Labour Organization" & is.na(born_date) ~ as.Date("1919-04-11"),
      #     2 Amnesty International                                NA        NA
      firstname == "Amnesty International" & is.na(born_date) ~ as.Date("1961-07-15"),
      #     3 International Physicians for the Prevention of Nucl… NA
      firstname == "International Physicians for the Prevention of Nuclear War" & is.na(born_date) ~ as.Date("1980-01-01"),
      #     4 United Nations Peacekeeping Forces                   NA        NA
      firstname == "United Nations Peacekeeping Forces" & is.na(born_date) ~ as.Date("1945-01-01"),
      #     5 Pugwash Conferences on Science and World Affairs     NA        NA
      firstname == "Pugwash Conferences on Science and World Affairs" & is.na(born_date) ~ as.Date("1957-01-01"),
      #     6 International Campaign to Ban Landmines              NA        NA
      firstname == "International Campaign to Ban Landmines" & is.na(born_date) ~ as.Date("1992-10-01"),
      #     7 Doctors Without Borders                              NA        NA
      firstname == "Doctors Without Borders" & is.na(born_date) ~ as.Date("1971-12-22"),
      #     8 United Nations                                       NA        NA
      firstname == "United Nations" & is.na(born_date) ~ as.Date("1945-06-26"),
      #     9 International Atomic Energy Agency                   NA        NA
      firstname == "International Atomic Energy Agency" & is.na(born_date) ~ as.Date("1957-07-29"),
      #     10 Grameen Bank                                         NA        NA
      #     11 Intergovernmental Panel on Climate Change            NA        NA
      #     12 European Union                                       NA        NA
      #    13 Organisation for the Prohibition of Chemical Weapons NA        NA
      #     14 National Dialogue Quartet                            NA        NA
      #     15 International Campaign to Abolish Nuclear Weapons    NA        NA
      #    16 World Food Programme                                 NA        NA
      #    17 Memorial                                             NA        NA
      #    18 Center for Civil Liberties                           NA        NA


      TRUE ~ born_date
    )
  )
# fill in missing information
if (FALSE) {
  cleaned_laureates %>%
    filter(is.na(born_date) & gender != "org") %>%
    select(firstname, surname, born_date, died_date, category)

  cleaned_laureates %>%
    filter(is.na(born_date) & gender == "org") %>%
    select(firstname, born_date, died_date)



}
nobel <- cleaned_laureates
write_csv(nobel, "data/nobel.csv")
write.table(
  paste0(
    "Last updated: ",
    format(Sys.Date(), "%Y-%m-%d")
  ),
  file = "data/nobel_date.txt"
)

remove(laureates, cleaned_laureates)
