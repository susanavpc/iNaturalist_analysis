library(tidyverse)
library(cowplot)


tidy_data <- read_csv("data/tidy_data/tidy_data_verifiable.csv")

#### Calculate amount of data per filtering step ----
tidy_data <- tidy_data %>%
        select("id", "uri", "quality_grade", "created_at", "taxon_id", "n_photos",
               "microscopy_project","days_since_upload", "created_at_year", 
               "observed_on_year", "created_at_month","observed_on_month", "country",
               "kingdom", "phylum", "class", "order", "family","genus", "specificEpithet", "taxon_rank",
               "sum_pixels", "n_obs_fields", "n_tags", "n_projects", "length_notes", 
               "presence_notes") %>% 
        
        #filter out 0 photos obs (likely a bug) 
        filter(!is.na(sum_pixels) & n_photos != 0) %>%
        
        mutate(
                #make RG only at spp level, and numeric                
                quality_sp_level = ifelse(quality_grade == "research" & specificEpithet == "Not identified", "needs_id", quality_grade), 
                rg = as.numeric(quality_sp_level == "research"),
                
                #create column for species name 
                species = as.factor(paste(genus, specificEpithet))) 


df <- tidy_data %>%
        filter(specificEpithet != "Not identified") 

species_counts <- df %>%
        group_by(species) %>%
        summarise(
                n_obs = n(),
                n_rg = sum(rg == 1),
                prop_rg = n_rg / n_obs
        ) %>%
        filter(n_obs >= 10 & prop_rg >= 0.3 & n_rg >=10 & prop_rg!=1 ) 

data_30_test_month <- df %>% 
        filter(species %in% (species_counts$species))


#tidy_data_filtered = 2509366
#only at spp level = 1673233
#with 30% thresholds = 1389547

#### PLOTS for n_obs and n species per threshold ----
thresholds <- seq(0, 1, by = 0.05)

# For each threshold, calculate how many species have %RG >= threshold
species_count_by_threshold <- map_dfr(thresholds, function(thresh) {
        # Summarize RG proportion per species
        species_summary <- df %>%
                group_by(species) %>%
                summarise(prop_RG = mean(rg), .groups = "drop")
        
        # Count how many species meet the threshold
        count <- species_summary %>%
                filter(prop_RG >= thresh) %>%
                summarise(n_species = n())
        
        # Return data frame with threshold and count
        tibble(threshold = thresh, n_species = count$n_species)
})

# Plot
ggplot(species_count_by_threshold, aes(x = threshold, y = n_species)) +
        geom_line() +
        geom_point() +
        labs(
                x = "Minimum RG Prop. per Species",
                y = "No.Species"
        ) +
        theme_cowplot()

obs_count_by_threshold <- map_dfr(thresholds, function(thresh) {
        # Compute %RG per species
        species_summary <- df %>%
                group_by(species) %>%
                summarise(prop_RG = mean(rg), .groups = "drop")
        
        # Filter species meeting the threshold
        selected_species <- species_summary %>%
                filter(prop_RG >= thresh) %>%
                pull(species)
        
        # Filter the original dataset to include only those species
        n_obs <- df %>%
                filter(species %in% selected_species) %>%
                summarise(n_obs = n()) %>%
                pull(n_obs)
        
        # Return threshold and number of observations
        tibble(threshold = thresh, n_obs = n_obs)
})

# Plot
ggplot(obs_count_by_threshold, aes(x = threshold, y = n_obs)) +
        geom_line() +
        geom_point() +
        labs(
                x = " Minimum RG Prop. per Species",
                y = "No. Observations"
        ) +
        theme_cowplot()
