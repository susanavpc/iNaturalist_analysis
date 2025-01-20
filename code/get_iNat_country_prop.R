library(dplyr)
library(httr)
library(jsonlite)
library(tidyverse)

# get country place ids from API  ----

get_country_id <- function(country){
        
        call <- paste0("https://api.inaturalist.org/v1/places/autocomplete?q=", 
                       URLencode(country),"&order_by=area") 
        
        get_call_json <- GET(url = call) %>%
                content(as = "text", encoding = "UTF-8") %>%
                fromJSON(flatten = TRUE) 
        
        get_call_json$results$id[1] #ordered by area so that first id match is for country 
        
}

# get countries from our iNaturalist dataset
country_names <- read_csv("data/data_country_names.csv")

# initialise new column in country_names dataframe
country_names$place_id <- 0

# loop through list of countries and get their corresponding place_id
for (i in 1:nrow(country_names)) {
        country_name <- country_names$country[i]
        
        place_id <- get_country_id(country_name)
        
        if (is.null(place_id)) {
                country_names$place_id[i] <- NA  # Assign NA if no result
        } else {
                country_names$place_id[i] <- place_id  
        }
        
}

# iNaturalist has North Macedonia as Macedonia
country_names$place_id[country_names$country == "North Macedonia"] <- get_country_id("Macedonia")


# get number of fungi + plants + animals observations per country ----

get_n_obs <- function(place_id = NULL, taxon = NULL, grade = NULL, verifiable = NULL, spam = NULL){
        #calls only data created on and before 2024-06-26 (matches our date of observations download)
        # 0 results per page because we only want total number, not the results
        # spam = false is needed when downloading all data to match results from website
        call <- paste0("https://api.inaturalist.org/v1/observations?created_d2=2024-06-26&per_page=0&place_id=",
                       place_id,"&taxon_id=",taxon,"&quality_grade=",grade,"&verifiable=",verifiable,"&spam=",spam)
        get_call_json <- GET(url = call) %>%
                content(as = "text", encoding = "UTF-8") %>%
                fromJSON(flatten = TRUE) 
        
        get_call_json$total_results
}

#initialise dataframe to save results
iNat_data_counts <- data.frame(
        country = character(),
        n_fungi_research = numeric(),
        n_fungi_needs_id = numeric(),
        n_fungi_verifiable = numeric(),
        n_fungi_all = numeric(),
        n_plants_research = numeric(),
        n_plants_needs_id = numeric(),
        n_plants_verifiable = numeric(),
        n_plants_all = numeric(),
        n_animals_research = numeric(),
        n_animals_needs_id = numeric(),
        n_animals_verifiable = numeric(),
        n_animals_all = numeric(),
        prop_fungi_research = numeric(),
        prop_fungi_needs_id = numeric(),
        prop_fungi_verifiable = numeric(),
        prop_fungi_all = numeric())

#define taxon_ids for different groups
fungi <- 47170
plantae <- 47126
animalia <- 1

for (i in 1:nrow(country_names)) {
        Sys.sleep(30)
        place_id <- country_names$place_id[i]
        
        # count occurrences 
        n_fungi_research = get_n_obs(place_id, taxon = fungi, grade = "research")
        n_fungi_needs_id = get_n_obs(place_id, taxon = fungi, grade = "needs_id")
        n_fungi_verifiable = get_n_obs(place_id, taxon = fungi, verifiable = "true")
        n_fungi_all = get_n_obs(place_id, taxon = fungi, spam = "false")
        
        n_plants_research = get_n_obs(place_id, taxon = plantae, grade = "research")
        n_plants_needs_id = get_n_obs(place_id, taxon = plantae, grade = "needs_id")
        n_plants_verifiable = get_n_obs(place_id, taxon = plantae, verifiable = "true")
        n_plants_all = get_n_obs(place_id, taxon = plantae, spam = "false")
        
        n_animals_research = get_n_obs(place_id, taxon = animalia, grade = "research")
        n_animals_needs_id = get_n_obs(place_id, taxon = animalia, grade = "needs_id")
        n_animals_verifiable = get_n_obs(place_id, taxon = animalia, verifiable = "true")
        n_animals_all = get_n_obs(place_id, taxon = animalia, spam = "false")
        
        # calculate proportion of fungi
        prop_fungi_research = n_fungi_research/ (n_fungi_research + n_plants_research + n_animals_research)
        prop_fungi_needs_id = n_fungi_needs_id/ (n_fungi_needs_id + n_plants_needs_id + n_animals_needs_id)
        prop_fungi_verifiable = n_fungi_verifiable/ (n_fungi_verifiable + n_plants_verifiable + n_animals_verifiable)
        prop_fungi_all = n_fungi_all/ (n_fungi_all + n_plants_all + n_animals_all)
        
        #save results in dataframe
        iNat_data_counts <- rbind(iNat_data_counts, data.frame(
                
                country = country_names$country[i],
                
                n_fungi_research = n_fungi_research,
                n_fungi_needs_id = n_fungi_needs_id,
                n_fungi_verifiable = n_fungi_verifiable,
                n_fungi_all = n_fungi_all,
                
                n_plants_research = n_plants_research,
                n_plants_needs_id = n_plants_needs_id,
                n_plants_verifiable = n_plants_verifiable,
                n_plants_all = n_plants_all,
                
                n_animals_research = n_animals_research,
                n_animals_needs_id = n_animals_needs_id,
                n_animals_verifiable = n_animals_verifiable,
                n_animals_all = n_animals_all,
                
                prop_fungi_research = prop_fungi_research,
                prop_fungi_needs_id = prop_fungi_needs_id,
                prop_fungi_verifiable = prop_fungi_verifiable,
                prop_fungi_all = prop_fungi_all))
        print(country_names$country[i])
}

write_csv(iNat_data_counts, "data/iNat_country_prop.csv")
