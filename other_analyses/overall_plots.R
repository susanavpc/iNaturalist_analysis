#Overall plots of iNat data
library(tidyverse)
library(cowplot)
library(scales)

get_legend_sub <- function(plot, legend = NULL) {
        
        gt <- ggplotGrob(plot)
        
        pattern <- "guide-box"
        if (!is.null(legend)) {
                pattern <- paste0(pattern, "-", legend)
        }
        
        indices <- grep(pattern, gt$layout$name)
        
        not_empty <- !vapply(
                gt$grobs[indices], 
                inherits, what = "zeroGrob", 
                FUN.VALUE = logical(1)
        )
        indices <- indices[not_empty]
        
        if (length(indices) > 0) {
                return(gt$grobs[[indices[1]]])
        }
        return(NULL)
}

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

p_taxa<- taxa_proportions_verifiable %>% 
        filter(region == "europe") %>% 
        ggplot(aes(x = fct_reorder(Taxon, Proportion, .desc = TRUE), y = Proportion, fill = Taxon)) +
        geom_col(width = 0.9) +
        labs(x = "Taxon", 
             y = "RG Proportion") +
        scale_y_continuous(
                # don't expand y scale at the lower end
                expand = expansion(mult = c(0, 0.05)))+
        scale_x_discrete(labels = c("animals_verifiable" = "animals",
                                    "plants_verifiable" = "plants",
                                    "fungi_verifiable" = "fungi")) +
        scale_fill_manual(values = c("animals_verifiable" = "skyblue2",   # Blue
                                     "plants_verifiable" = "darkolivegreen3",    # Green
                                     "fungi_verifiable" = "indianred")) +  # Pink
        theme_cowplot(font_size = 13) +
        theme(axis.title.x = element_text(margin = margin(t = 12)),
              axis.title.y = element_text(margin = margin(r = 10)),
              legend.position = "none") 


#### Quality Grade no within fungi ----
tidy_data_all <- read_csv("data/tidy_data/new_combined_data.csv")

p_quality<- ggplot(tidy_data_all,aes(x = fct_infreq(quality_grade), fill= quality_grade)) +
        geom_bar(width = 0.9) +
        scale_fill_manual(values = c(research= "#a8cc08", needs_id= "#ffee91",casual= "#aaaaaa"))+
        labs(x = "Quality Grade", 
             y = "No. of Observations") +
        scale_x_discrete(labels = c("needs_id" = "Needs ID",
                                    "research" = "Research",
                                    "casual" = "Casual"))+
        scale_y_continuous(
                # don't expand y scale at the lower end
                expand = expansion(mult = c(0, 0.05))) +
         geom_text(stat = "count", 
            aes(label = paste0("n = ", ..count..)), 
            vjust = -0.7) +
        
        #scale_y_continuous(labels = label_number(scale_cut = cut_short_scale()))+ #abbrev. numbers
        theme_cowplot(font_size = 13) +
        theme(axis.title.x = element_text(margin = margin(t = 12)),
              axis.title.y = element_text(margin = margin(r = 10)),
              legend.position = "none") 

aligned <- cowplot::align_plots(p_taxa, p_quality, align = "hv", axis = "tblr")
cowplot::plot_grid(aligned[[1]], NULL, aligned[[2]], ncol = 3,rel_widths = c(1, 0.15, 1))


#### ccby ----
tidy_data <- read_csv("data/tidy_data/tidy_data_verifiable.csv")

tidy_data %>% 
        filter(quality_grade == "research") %>% 
        mutate(license_code = ifelse(is.na(license_code), "not licensed", license_code)) %>% 
        ggplot(aes(x = fct_rev(fct_infreq(license_code)))) +
        geom_bar() +
        labs(x = "Licence Code", 
             y = "No. of Observations") +
        scale_y_continuous(labels = label_number())+
        coord_flip()+
        theme_cowplot(font_size = 13)


prop_cc_gbif <- tidy_data %>% 
        filter(quality_grade == "research") %>% 
        count(license_code) %>%
        summarise(gbif = sum(n[license_code %in% c("cc0", "cc-by-nc", "cc-by")]),
                  total = sum(n),
                  prop_gbif = gbif/total)


####microscopy project ----

ggplot(data_30_test_month, aes(x = microscopy_project, fill = quality_grade)) +
        geom_bar(position ="fill") +
        scale_fill_manual(values = c(research= "#a8cc08", needs_id= "#ffee91",casual= "#aaaaaa"))+
        labs(x = "Added to \"Mushroom microscopy\" project", 
             y = "No. of Observations",
             fill = "Quality Grade") +
        scale_x_discrete(labels = c("needs_id" = "Needs ID",
                                    "research" = "Research",
                                    "casual" = "Casual"))+
        #scale_y_continuous(labels = label_number(scale_cut = cut_short_scale()))+ #abbrev. numbers
        theme_cowplot(font_size = 13) +
        theme(axis.title.x = element_text(margin = margin(t = 12)),
              axis.title.y = element_text(margin = margin(r = 10)))

labels_microscopy <- tidy_data %>%
        count(microscopy_project)  

p1 <- ggplot(tidy_data, aes(x = microscopy_project, fill = quality_grade)) +
        geom_bar(position ="fill")+
        scale_fill_manual(values = c(research= "#a8cc08", needs_id= "#ffee91"),
                          labels = c(research = "Research", needs_id = "Needs ID"))+
        labs(x = "Added to \"Mushroom microscopy\" project", 
             y = "Proportion of Observations",
             fill = "Quality Grade") +
        scale_y_continuous(
                # don't expand y scale at the lower end
                expand = expansion(mult = c(0, 0.05))) +
        theme_cowplot(font_size = 13) +
        theme(axis.title.x = element_text(margin = margin(t = 12)),
              axis.title.y = element_text(margin = margin(r = 10)),legend.position="none")+
        geom_text(data = labels_microscopy, aes(x = microscopy_project, y = 1.05, label = paste0("n = ", n)), inherit.aes = FALSE)

tidy_data_obsf <- read_csv("data/tidy_data/tidy_data_genetic.csv")   

labels_genetic <- tidy_data_obsf %>%
        count(genetic_obsf) 

p2 <- ggplot(tidy_data_obsf, aes(x = genetic_obsf, fill = quality_grade)) +
        geom_bar(position ="fill")+
        scale_fill_manual(values = c(research= "#a8cc08", needs_id= "#ffee91"),
                          labels = c(research = "Research", needs_id = "Needs ID"))+
        labs(x = "Includes Barcoding Observation Fields", 
             y = "Proportion of Observations",
             fill = "Quality Grade") +
        scale_y_continuous(
                # don't expand y scale at the lower end
                expand = expansion(mult = c(0, 0.05))) +
        #scale_y_continuous(labels = label_number(scale_cut = cut_short_scale()))+ #abbrev. numbers
        theme_cowplot(font_size = 13) +
        theme(axis.title.x = element_text(margin = margin(t = 12)),
              axis.title.y = element_text(margin = margin(r = 10)),legend.position="none")+
        geom_text(data = labels_genetic, aes(x = genetic_obsf, y = 1.05, label = paste0("n = ", n)), inherit.aes = FALSE)


p3 <- p2 + theme(
        legend.position = "bottom",
        legend.box = "vertical",
        legend.direction = "horizontal",
        legend.justification = "center",
        legend.title = element_text(hjust = 0.5),         # updated syntax
        legend.spacing.x = unit(0.5, "cm"),                # space between keys
        legend.box.margin = margin(t = 5)   # top margin
)

legend <- get_legend_sub(p3)

grid <- cowplot::plot_grid(plotlist = list(p1, p2),
                           label_size = 13, 
                           ncol =2)
plot_grid(grid, legend, ncol = 1, rel_heights = c(1, .11))


proportions_micro <- tidy_data %>%
       count(microscopy_project, quality_grade) %>%
        group_by(microscopy_project) %>%
        mutate(prop = n / sum(n))

proportions_genetic<- tidy_data_obsf %>%
        count(genetic_obsf, quality_grade) %>%
        group_by(genetic_obsf) %>%
        mutate(prop = n / sum(n))
