#join iNat and GBIF fungi proportion data 

library(tidyverse)

inat_counts<- read_csv("data/iNat_country_prop.csv")

inat_global_region <-  read_csv("data/iNat_prop_global_europe.csv")

gbif_counts <- read_csv("data/gbif_country_prop.csv")

gbif_global_region <- read_csv("data/gbif_prop_global_europe.csv")

#make data uniform - kept filtered data from gbif and verifiable data from inat

inat_counts <- inat_counts %>% 
        select(country, n_fungi_verifiable, n_plants_verifiable, n_animals_verifiable, prop_fungi_verifiable) %>% 
        mutate(platform = "inat") %>%
        rename(n_fungi = n_fungi_verifiable, n_plants = n_plants_verifiable, n_animals = n_animals_verifiable , prop_fungi = prop_fungi_verifiable)

inat_global_region <- inat_global_region %>% 
        select(region, n_fungi_verifiable, n_plants_verifiable, n_animals_verifiable, prop_fungi_verifiable) %>% 
        mutate(platform = "inat") %>%
        rename(n_fungi = n_fungi_verifiable, n_plants = n_plants_verifiable, n_animals = n_animals_verifiable , prop_fungi = prop_fungi_verifiable)
        
gbif_counts <- gbif_counts %>% 
        mutate(platform = "gbif") %>% 
        select(-country_iso) %>% 
        rename(country = name)

gbif_global_region <- gbif_global_region %>% 
        mutate(platform = "gbif")

#join data from gbif and inaturalist (1 csv at country level, another at Europe/global)
country_fungi_prop <- rbind(inat_counts, gbif_counts)

region_fungi_prop <- rbind(inat_global_region, gbif_global_region)

write_csv(country_fungi_prop, "data/country_fungi_prop.csv")
write_csv(region_fungi_prop, "data/region_fungi_prop.csv")
