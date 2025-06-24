#Overall plots of iNat data
library(tidyverse)
library(cowplot)
library(scales)

#### Comparison between fungi and other taxa ----
taxa_proportions <- read_csv("data/iNat_prop_global_europe.csv")

taxa_proportions_all <- taxa_proportions %>% 
        mutate(prop_RG_fungi = n_fungi_research/n_fungi_all,
               prop_RG_plants = n_plants_research/n_plants_all,
               prop_RG_animals = n_animals_research/n_animals_all) %>% 
        pivot_longer(cols = starts_with("prop_RG_"), 
                     names_to = "Taxon", 
                     values_to = "Proportion") %>%
        mutate(Taxon = str_replace_all(Taxon, "prop_RG_", ""))

taxa_proportions_verifiable <-taxa_proportions %>% 
        mutate(prop_RG_fungi_verifiable = n_fungi_research/n_fungi_verifiable,
               prop_RG_plants_verifiable = n_plants_research/n_plants_verifiable,
               prop_RG_animals_verifiable = n_animals_research/n_animals_verifiable) %>% 
        pivot_longer(cols = starts_with("prop_RG_"), 
                     names_to = "Taxon", 
                     values_to = "Proportion") %>%
        mutate(Taxon = str_replace_all(Taxon, "prop_RG_", ""))

#comparison europe vs global
ggplot(taxa_proportions_all,aes(x = fct_reorder(Taxon, Proportion, .desc = TRUE), y = Proportion, fill = Taxon)) +
        geom_col() +
        facet_wrap(~region, labeller = labeller(region = c(europe = "Europe",
                                                           global = "Global"))) +
        labs(x = "Taxon", 
             y = "RG Proportion") +
        scale_fill_manual(values = c("animals" = "skyblue2",   # Blue
                                     "plants" = "darkolivegreen3",    # Green
                                     "fungi" = "indianred")) +  # Pink
        theme_cowplot(font_size = 13) +
        theme(legend.position = "none") 

#all data - comparison for Europe
taxa_proportions_all %>% 
        filter(region == "europe") %>% 
        ggplot(aes(x = fct_reorder(Taxon, Proportion, .desc = TRUE), y = Proportion, fill = Taxon)) +
        geom_col(width = 0.7) +
        labs(x = "Taxon", 
             y = "RG Proportion") +
        scale_fill_manual(values = c("animals" = "skyblue2",   # Blue
                                     "plants" = "darkolivegreen3",    # Green
                                     "fungi" = "indianred")) +  # Pink
        scale_y_continuous() + 

        theme_cowplot(font_size = 13) +
        theme(axis.title.x = element_text(margin = margin(t = 9)),
              axis.title.y = element_text(margin = margin(r = 9)),
              legend.position = "none") 

#only verifiable data
taxa_proportions_verifiable %>% 
        filter(region == "europe") %>% 
        ggplot(aes(x = fct_reorder(Taxon, Proportion, .desc = TRUE), y = Proportion, fill = Taxon)) +
        geom_col(width = 0.7) +
        labs(x = "Taxon", 
             y = "RG Proportion") +
        scale_x_discrete(labels = c("animals_verifiable" = "animals",
                                    "plants_verifiable" = "plants",
                                    "fungi_verifiable" = "fungi")) +
        scale_fill_manual(values = c("animals_verifiable" = "skyblue2",   # Blue
                                     "plants_verifiable" = "darkolivegreen3",    # Green
                                     "fungi_verifiable" = "indianred")) +  # Pink
        theme_cowplot(font_size = 13) +
        theme(axis.title.x = element_text(margin = margin(t = 9)),
              axis.title.y = element_text(margin = margin(r = 9)),
              legend.position = "none") 


#### Quality Grade no within fungi ----
tidy_data_all <- read_csv("data/tidy_data/new_combined_data.csv")

ggplot(tidy_data_all,aes(x = fct_infreq(quality_grade), fill= quality_grade)) +
        geom_bar() +
        scale_fill_manual(values = c(research= "#a8cc08", needs_id= "#ffee91",casual= "#aaaaaa"))+
        labs(x = "Quality Grade", 
             y = "No. of Observations") +
        scale_x_discrete(labels = c("needs_id" = "Needs ID",
                                    "research" = "Research",
                                    "casual" = "Casual"))+
        #scale_y_continuous(labels = label_number(scale_cut = cut_short_scale()))+ #abbrev. numbers
        theme_cowplot(font_size = 13) +
        theme(axis.title.x = element_text(margin = margin(t = 10)),
              axis.title.y = element_text(margin = margin(r = 10)),
              legend.position = "none") 

