# Get population size for countries in our dataset  
# original UN data file from: United Nations, Department of Economic and Social Affairs, Population Division (2024). World Population Prospects 2024, Online Edition.

library(tidyverse)

UN_data <- read_csv("data/WPP2024_Demographic_Indicators_Medium.csv")

#Filter data for countries in dataset, for 2023 (original population size in thousands)
country_codes <- read_csv("data/gbif_country_prop_filtered.csv")
country_codes <- country_codes %>% select(name, country_iso)


UN_Data_countries <- UN_data %>%
        filter(ISO2_code %in% country_codes$country_iso) %>% 
        select(Location, ISO2_code, Time, TPopulation1July ) %>% 
        filter(Time == 2023) %>% 
        mutate(Population = TPopulation1July * 1000) %>% 
        rename(country_iso = ISO2_code)

#Replace values for Turkey and Russia with values for European parts only (russtat and turkstat data)
UN_Data_countries$Population[UN_Data_countries$country_iso == "RU"] <- 109546819
UN_Data_countries$Population[UN_Data_countries$country_iso == "TR"] <- 12059080

#check for missing countries - Svalbard and Jan Mayen & Åland are not in Dataset
length(unique(country_codes$country_iso))
length(unique(UN_Data_countries$country_iso))
setdiff(country_codes$country_iso, UN_Data_countries$country_iso)

#edit final table 
pop_data_countries <- left_join(country_codes, UN_Data_countries, by = "country_iso") 
pop_data_countries <- pop_data_countries %>% 
        select (-Location, -TPopulation1July, -Time) %>% 
        rename (country = name,
                population_2023 = Population)

write_csv(pop_data_countries, "data/pop_data_countries.csv")
