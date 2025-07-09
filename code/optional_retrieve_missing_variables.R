library(tidyverse)

file_list <- list.files(path=("data/raw"),
                        pattern="\\.RData$",
                        full.names=TRUE)


get_genetic_obsf <- function(i, dataset = obs){
        if (any(dataset$ofvs[[i]]$name == "DNA Barcode ITS" |  
                dataset$ofvs[[i]]$name == "GenBank number (URL)" | 
                dataset$ofvs[[i]]$name == "Genbank Accession Number")){
                "yes"
        } else {
                "no"
        }
}



results_list <- list()

counter <- 1

for (file in file_list) {
        load(file)
        
        result <- lapply(1:nrow(obs), function(i) {
                id <- obs$id[i]
                genetic_obsf <- get_genetic_obsf(i, obs)
                data.frame(id = id, genetic_obsf = genetic_obsf, stringsAsFactors = FALSE)
        })
        
        results_list[[counter]] <- do.call(rbind, result)
        message("Completed file ", counter, ": ", basename(file))
        
        counter <- counter + 1
        
        rm(obs)
        gc()
}

new_vars <- do.call(rbind, results_list)

saveRDS(new_vars, file = "data/data_genetic_obsf.rds")

tidy_data <- read_csv("data/tidy_data/tidy_data_verifiable.csv")

tidy_data_obsf <- left_join(tidy_data, new_vars, by = "id" )

write_csv(tidy_data_obsf, file = "data/tidy_data/tidy_data_genetic.csv")
