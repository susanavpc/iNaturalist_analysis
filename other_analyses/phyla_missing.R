# check which phyla are represented in the final modelling dataset
library(tidyverse)

load(file = "data/tidy_data/data_30perc.RData")

#check which phyla we have in our dataset
ggplot(data_30_test_month,aes(x = fct_infreq(phylum), fill= phylum))+
        geom_bar()+
        labs(title = "Number of observations per phylum",
             x = "Phylum", y = "Number of observations (log10)")+
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))+
        scale_y_log10(labels = scales::label_number())


#read in GBIF's fungal taxa in europe - data downloaded from GBIF.org (22 September 2026) GBIF Occurrence Download https://doi.org/10.15468/dl.k2qdfb
fungi_gbif <- read_tsv( "data/GBIF_europe_fungal_taxa_sep_2026.csv", show_col_types = FALSE)


unique(fungi_gbif$phylum)

all_phyla <- fungi_gbif %>%
        filter(!is.na(phylum)) %>%
        distinct(phylum)

#join phyla in inat dataset with phyla that are missing but exist in gbif
plot_data <- data_30_test_month %>%
        count(phylum, name = "n") %>%
        right_join(all_phyla, by = "phylum") %>%
        mutate(n = replace_na(n, 0))


ggplot(plot_data, aes(x = fct_reorder(phylum, n), y = n)) +
        geom_col() +
        geom_text(data = filter(plot_data, n == 0), aes(label = "0"),
                  vjust = -0.5, size = 3) +
        scale_y_continuous(
                trans  = scales::pseudo_log_trans(base = 10),
                breaks = c(0, 10, 100, 1e3, 1e4, 1e5, 1e6),
                labels = scales::label_number(),
                expand = expansion(mult = c(0, 0.05))
        ) +
        labs(
             x = "Phylum", y = "No. observations (pseudo-log scale)") +
        cowplot::theme_cowplot() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
