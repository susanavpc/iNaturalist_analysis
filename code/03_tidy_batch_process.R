# Batch process to transform RData files into tidy CSV files and combine them 
# 
# Input files: 
# - get_vars_functions.R
# - tidy_data_script.R
# - selected_variables.csv
# - inat_fungi_taxa.csv
# - taxon_swap_mapping.csv

library(dplyr) 
library(readr)
library(stringr)

source("code/get_vars_functions.R")
selected_variables <- read.csv("data/selected_variables.csv")$var_name 

file_list <- list.files(path=("data/raw"),
                    pattern="\\.RData$",
                    full.names=TRUE)

#Loop through RData files, process obs object and save tidy_data csv files ----
for (file in file_list) {
        load(file)
        source("./R_files/tidy_data_script.R")
        output_name <- sub("\\.RData$", ".csv", sub("obs", "tidy_data", file)) %>% 
                basename()
        write_csv(tidy_data, paste0("data/processed_csvs/", output_name))
        print(file)        
}

rm(tidy_data)

# Combine all files into a single one ----
csv_files <- list.files(path=("data/processed_csvs"),
                        pattern="*.csv")

combined_data <- lapply(csv_files, read.csv) %>%
        bind_rows()


# Add taxonomic information for each observation ----

fungi_taxa <- read.csv("inat_fungi_taxa.csv")

# check for taxa that don't have taxonomic correspondence 
NA_taxa_data <- combined_data %>% 
        left_join(.,fungi_taxa, by="taxon_id") %>% 
        filter(is.na(phylum) & taxon.rank != "kingdom")
view(NA_taxa_data$taxon_id)

# fix NAs caused by taxon swaps not updated in taxonomic dataset
taxon_swap <- read.csv("taxon_swap_mapping.csv")

combined_data$taxon_id <- taxon_swap$new_taxon_id[
        match(combined_data$taxon_id, taxon_swap$old_taxon_id)]

combined_data <- combined_data %>% 
        left_join(.,fungi_taxa, by="taxon_id")

# Save final dataset ----
write_csv(combined_data,"data/tidy_data/tidy_data.csv")