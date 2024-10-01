# library(tidyverse)
# source("code/get_vars_functions.R")
# selected_variables <- read.csv("data/selected_variables.csv")$var_name 

function_vars <-list()

for (i in 1:nrow(obs)) {
        id <- obs$id[i]
        
        n_photos <- get_n_photos(i)
        GBIF_url <- get_gbif_url(i)
        days_since_upload <- get_days_upload(i)
        n_identifiers <- get_n_identifiers(i)
        n_identifications <- get_n_identifications(i)
        n_obs_fields <- get_obs_fields(i)
        presence_notes<- get_presence_notes(i)
        length_notes <- get_length_notes(i) 
        n_tags <- get_n_tags(i)
        n_projects <- get_n_projects(i)
        microscopy_project <- get_microscopy_project(i)
        
        function_vars[[i]] <- data.frame(
                id, 
                GBIF_url,
                n_identifications,
                n_identifiers,
                n_photos, 
                n_obs_fields,
                presence_notes, 
                length_notes, 
                n_tags,
                n_projects, 
                microscopy_project,
                days_since_upload)
}

simple_vars <- obs %>% select(any_of(selected_fields))

rm(obs)
gc()

#join vars from functions + vars selected from original dataframe
tidy_data <- bind_rows(function_vars) %>% 
        left_join(simple_vars,., by="id") %>% 
        separate(location, into = c("lat", "long"), sep = ",") %>% 
        mutate(lat = as.numeric(lat),
               long = as.numeric(long),
               created_at_year = year(created_at), 
               observed_on_year = year(observed_on),
               created_at_month = month(created_at, label =TRUE),
               observed_on_month = month(created_at, label =TRUE),
               across(everything(), ~ replace(., . == "", NA))) %>% 
        rename(notes = description, 
               taxon_id = taxon.id,
               taxon_rank = taxon.rank,
               taxon_observations_count = taxon.observations_count,
               taxon_conservation_authority = taxon.conservation_status.authority,
               taxon_conservation_status = taxon.conservation_status.status,
               user_id = user.id,
               user_created_at = user.created_at,
               user_activity_count = user.activity_count,
               user_identifications_count = user.identifications_count,
               user_species_count = user.species_count,
               user_observations_count = user.observations_count)

rm(simple_vars, function_vars)
gc()

