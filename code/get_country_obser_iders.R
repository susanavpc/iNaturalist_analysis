# get number of fungi observers and identifiers per country, Europe and globally 
# verifiable observations only and cut-off date of 2024-06-26 (matches observations download)
# input needed: "country_place_ids.csv", generated in "get_iNat_country_prop.R"


library(dplyr)
library(httr)
library(jsonlite)
library(tidyverse)

# function to get number of observers from API per place id ----
get_n_observers <- function(place_id = NULL){
        
        call <- paste0("https://api.inaturalist.org/v1/observations/observers?verifiable=true&created_d2=2024-06-26&iconic_taxa=Fungi&per_page=0&place_id=", place_id)
        get_call_json <- GET(url = call) %>%
                content(as = "text", encoding = "UTF-8") %>%
                fromJSON(flatten = TRUE) 
        
        get_call_json$total_results
}

# function to get number of identifiers from API per place id ----
get_n_identifiers <- function(place_id = NULL){
        
        call <- paste0("https://api.inaturalist.org/v1/observations/identifiers?verifiable=true&created_d2=2024-06-26&iconic_taxa=Fungi&per_page=0&place_id=", place_id)
        get_call_json <- GET(url = call) %>%
                content(as = "text", encoding = "UTF-8") %>%
                fromJSON(flatten = TRUE) 
        
        get_call_json$total_results
}

# get data per country ----
country_ids <- read_csv("data/country_place_ids.csv")

country_obser_ident <- data.frame(
        country = character(),
        n_observers = numeric(),
        n_identifiers = numeric())

for (i in 1:nrow(country_ids)) {
        Sys.sleep(5)
        place_id <- country_ids$place_id[i]
        n_observers <- get_n_observers(place_id)
        n_identifiers <- get_n_identifiers(place_id)
        
        country_obser_ident <- rbind(country_obser_ident, data.frame(
                country = country_ids$country[i],
                n_observers = n_observers,
                n_identifiers = n_identifiers))
        print(country_ids$country[i])
}

# calculate proportion of identifiers/observers per country
country_obser_ident <- country_obser_ident %>%
        mutate(prop_iders_obsers = n_identifiers / n_observers)

write_csv(country_obser_ident, "data/country_obser_iders.csv")

# get data for Europe and at global level----
europe_obser_ident <- data.frame(
        region = "europe",
        n_observers =  get_n_observers(97391),
        n_identifiers = get_n_identifiers(97391)
)

europe_obser_ident <- europe_obser_ident %>%
        mutate(prop_iders_obsers = n_identifiers / n_observers)


global_obser_ident <- data.frame(
        region = "global",
        n_observers =  get_n_observers(),
        n_identifiers = get_n_identifiers()
)

global_obser_ident <- global_obser_ident %>%
        mutate(prop_iders_obsers = n_identifiers / n_observers)

region_obser_ident <- rbind(europe_obser_ident, global_obser_ident)

write_csv(region_obser_ident, "data/region_obser_iders.csv")
