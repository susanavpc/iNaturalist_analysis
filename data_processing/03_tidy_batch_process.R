# Batch process to transform RData files into tidy CSV files and combine them 
# 
# Input files: 
# - observation RData files
# - get_vars_functions.R
# - tidy_data_script.R
# - selected_variables.csv
# - inat_fungi_taxa.csv
# - taxon_swap_mapping.csv

library(tidyverse)

source("data_processing/functions_get_vars.R")

#Read in list of names of variables needed
selected_variables <- read.csv("data/selected_variables.csv")$var_name 

file_list <- list.files(path=("data/raw"),
                    pattern="\\.RData$",
                    full.names=TRUE)

#Loop through RData files, process obs object and save tidy_data csv files ----
for (file in file_list) {
        load(file)
        source("data_processing/script_tidy_data.R")
        output_name <- sub("\\.RData$", ".csv", sub("obs", "tidy_data", file)) %>% 
                basename()
        write_csv(tidy_data, paste0("data/processed_csvs/", output_name))
        print(output_name)        
}

rm(tidy_data)

# Combine all files into a single one ----
csv_files <- list.files(path=("data/processed_csvs"),
                        pattern="*.csv",
                        full.names = TRUE)

combined_data <- lapply(csv_files, read_csv) %>%
        bind_rows() %>% 
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

#optional: save temporary file with combined data
write_csv(combined_data,"temporary/temp_combined_data.csv")

# Add taxonomic information for each observation ----

fungi_taxa <- read.csv("data/inat_fungi_taxa.csv")

# check for taxa that don't have taxonomic correspondence 
NA_taxa_data <- combined_data %>% 
        left_join(.,fungi_taxa, by="taxon_id") %>% 
        filter(is.na(phylum) & taxon_rank != "kingdom")

view(NA_taxa_data$taxon_id)

# read in taxon swaps not yet updated in taxonomic dataset to fix NAs 
taxon_swap <- read.csv("data/taxon_swap_mapping.csv")


#change taxon ids in combined data for the updated ones
swap_ids<- taxon_swap$new_taxon_id[
        match(combined_data$taxon_id, taxon_swap$old_taxon_id)]

combined_data$taxon_id <- coalesce(swap_ids, combined_data$taxon_id)

#add the taxonomic information
combined_data <- combined_data %>% 
        left_join(.,fungi_taxa, by="taxon_id")

#final check
combined_data %>% 
        filter(is.na(phylum) & taxon_rank != "kingdom") %>% view()

#replace NAs in taxonomy columns
combined_data <- combined_data %>% 
        mutate(across(kingdom:specificEpithet, ~ ifelse(is.na(.), "Not identified", .)))

# Save dataset ----
write_csv(combined_data,"data/tidy_data/combined_data.csv")

tidy_data_verifiable <- combined_data %>% 
        filter(quality_grade != "casual") 

write_csv(tidy_data_verifiable, "data/tidy_data/tidy_data_verifiable.csv")

