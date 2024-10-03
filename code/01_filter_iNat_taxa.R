#Filter iNaturalist taxonomic reference dataset 

library(readr)
library(dplyr)

taxa <- read_csv("data/taxa.csv")
fungi_taxa <- taxa %>% 
        filter(kingdom == "Fungi") %>%
        rename(taxon_id = id) %>% # prevents confusion with observation IDs in other files
        select(-(taxonID:parentNameUsageID), -modified, -taxonRank, -references)
        
write_csv(fungi_taxa,"data/inat_fungi_taxa.csv", row.names = FALSE)