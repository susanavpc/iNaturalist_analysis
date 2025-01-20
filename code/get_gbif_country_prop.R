# get number of occurrences for plants, animals and fungi + proportion from GBIF
# in one case records were filtered to exclude:
# fossil specimens, literature, living specimens, material citation and unknown

library(rgbif)
library(tidyverse)

# get country iso codes (alpha2) to query gbif ----
countries_iso <- read_csv("data/country_iso_codes.csv")
data_countries <- read_csv("data/data_country_names.csv")

countries_iso <- countries_iso %>% 
        rename(country_iso = `alpha-2`)

#get ISO codes for countries in iNat data
country_codes <- countries_iso %>%
        filter(name %in% data_countries$country) %>%
        select(name, country_iso)

#check and add countries missing due to missmatch (XK - Kosovo not in ISO)

values_in_countries_not_in_codes <- setdiff(data_countries$country, countries_iso$name)
values_in_codes_not_in_countries <- setdiff(countries_iso$name, data_countries$country)

missing_country_codes <- data.frame(
        name = c("United Kingdom","Russia","Kosovo","Åland","Moldova","Vatican City"),
        country_iso = c("GB", "RU", "XK", "AX", "MD", "VA"))

country_codes <- rbind(country_codes, missing_country_codes)

#exclude records from FOSSIL_SPECIMEN, LITERATURE, LIVING_SPECIMEN, MATERIAL_CITATION, UNKNOWN
basis_record<- "HUMAN_OBSERVATION; MACHINE_OBSERVATION; MATERIAL_SAMPLE; OBSERVATION; OCCURRENCE; PRESERVED_SPECIMEN"

# get data from gbif per country ----

gbif_data <- data.frame(
        country_iso = character(),
        n_fungi = numeric(),
        n_plants = numeric(),
        n_animals = numeric(),
        prop_fungi = numeric())

for (i in country_codes$country_iso) {
        
        # count occurrences 
        n_fungi <- occ_count(scientificName = "Fungi", country = i)
        n_plants <- occ_count(scientificName = "Plantae", country = i)
        n_animals <- occ_count(scientificName = "Animalia", country = i)
        
        # calculate proportion of fungi
        prop_fungi <- n_fungi / (n_fungi + n_plants + n_animals)
        
        gbif_data <- rbind(gbif_data, data.frame(
                country_iso = i,
                n_fungi = n_fungi,
                n_plants = n_plants,
                n_animals = n_animals,
                prop_fungi = prop_fungi))
}

joined_gbif_data <- left_join(country_codes, gbif_data, by = "country_iso")
write_csv(joined_gbif_data, "data/gbif_country_prop.csv")

global_n_fungi <- occ_count(scientificName="Fungi")
global_n_plants <- occ_count(scientificName="Plantae")
global_n_animals <- occ_count(scientificName="Animalia")
global_fungi_prop <- global_n_fungi / (global_n_fungi + global_n_plants + global_n_animals)
        
eu_n_fungi <- occ_count(scientificName="Fungi", continent = "europe")
eu_n_plants <- occ_count(scientificName="Plantae", continent = "europe")
eu_n_animals <- occ_count(scientificName="Animalia", continent = "europe")
eu_fungi_prop <- eu_n_fungi / (eu_n_fungi + eu_n_plants + eu_n_animals)

# get data from gbif

gbif_data_filtered <- data.frame(
        country_iso = character(),
        n_fungi = numeric(),
        n_plants = numeric(),
        n_animals = numeric(),
        prop_fungi = numeric())

for (i in country_codes$country_iso) {
        
        # count occurrences 
        n_fungi <- occ_count(scientificName = "Fungi", basisOfRecord = basis_record, country = i)
        n_plants <- occ_count(scientificName = "Plantae", basisOfRecord = basis_record, country = i)
        n_animals <- occ_count(scientificName = "Animalia", basisOfRecord = basis_record, country = i)
        
        # calculate proportion of fungi
        prop_fungi <- n_fungi / (n_fungi + n_plants + n_animals)
        
        gbif_data_filtered <- rbind(gbif_data_filtered, data.frame(
                country_iso = i,
                n_fungi = n_fungi,
                n_plants = n_plants,
                n_animals = n_animals,
                prop_fungi = prop_fungi))
}


joined_gbif_data_filtered <- left_join(country_codes, gbif_data_filtered, by = "country_iso")
write_csv(joined_gbif_data_filtered, "data/gbif_country_prop_filtered.csv")

# get global and european data (filtered) ----

global_n_fungi_filtered <- occ_count(scientificName="Fungi", basisOfRecord = basis_record)
global_n_plants_filtered <- occ_count(scientificName="Plantae", basisOfRecord = basis_record)
global_n_animals_filtered <- occ_count(scientificName="Animalia", basisOfRecord = basis_record)
global_fungi_prop_filtered <- global_n_fungi / (global_n_fungi + global_n_plants + global_n_animals)

eu_n_fungi_filtered <- occ_count(scientificName="Fungi", basisOfRecord = basis_record, continent = "europe")
eu_n_plants_filtered <- occ_count(scientificName="Plantae", basisOfRecord = basis_record, continent = "europe")
eu_n_animals_filtered <- occ_count(scientificName="Animalia", basisOfRecord = basis_record, continent = "europe")
eu_fungi_prop_filtered <- eu_n_fungi / (eu_n_fungi + eu_n_plants + eu_n_animals)

gbif_data_global_europe <- data.frame(
        region = c("global", "europe"),
        n_fungi = c(global_n_fungi_filtered, eu_n_fungi_filtered),
        n_plants = c(global_n_plants_filtered, eu_n_plants_filtered),
        n_animals = c(global_n_animals_filtered, eu_n_animals_filtered),
        prop_fungi = c(global_fungi_prop_filtered, eu_fungi_prop_filtered)
)

write_csv(gbif_data_global_europe, "data/gbif_prop_global_europe.csv")

