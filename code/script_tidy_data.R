
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
        sum_pixels <- get_sum_pixels(i)
        observer_taxon_rank <- get_observer_taxon_rank(i)
        observer_taxon_id <- get_observer_taxon_id(i)
        
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
                days_since_upload,
                sum_pixels,
                observer_taxon_rank,
                observer_taxon_id)
}

simple_vars <- obs %>% 
        select(any_of(selected_variables))

rm(obs)
gc()

#join vars from functions + vars selected from original dataframe
tidy_data <- bind_rows(function_vars) %>% 
        left_join(simple_vars,., by="id") 

#categorise note length, n tags, n projects and n observation fields
tidy_data <-  tidy_data %>%
        mutate(
                cat_notes = case_when(
                        length_notes == 0  ~ 0,
                        length_notes >= 1 & length_notes <= 60 ~ 1,
                        length_notes >= 61 & length_notes <= 180  ~ 2,
                        length_notes >= 181 & length_notes <= 300 ~ 3,
                        length_notes > 300 ~ 4
                ),
                cat_obs_fields = case_when(
                        n_obs_fields == 0  ~ 0,
                        n_obs_fields >= 1 & n_obs_fields <= 3 ~ 1,
                        n_obs_fields >= 4 & n_obs_fields <= 6  ~ 2,
                        n_obs_fields > 6 ~ 3
                ),
                cat_tags = case_when(
                        n_tags == 0  ~ 0,
                        n_tags >= 1 & n_tags <= 3 ~ 1,
                        n_tags  >= 4 & n_tags <= 6 ~ 2,
                        n_tags > 6 ~ 3
                ),
                cat_projects = case_when(
                        n_projects == 0 ~ 0,
                        n_projects == 1 | n_projects == 2 ~ 1,
                        n_projects > 3 ~ 2,
                )
        )

# separate coordinates column and add months and year variables
tidy_data <- tidy_data %>% 
        separate(location, into = c("lat", "long"), sep = ",") %>% 
        mutate(lat = as.numeric(lat),
               long = as.numeric(long),
               created_at_year = year(created_at), 
               observed_on_year = year(observed_on),
               created_at_month = month(created_at, label =TRUE),
               observed_on_month = month(created_at, label =TRUE),
               across(everything(), ~ replace(., . == "", NA)))


rm(simple_vars, function_vars)
gc()

