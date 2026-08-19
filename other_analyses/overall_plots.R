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

p_taxa <- taxa_proportions_verifiable %>% 
        filter(region == "europe") %>% 
        ggplot(aes(x = fct_reorder(Taxon, Proportion, .desc = TRUE), y = Proportion, fill = Taxon)) +
        geom_col(width = 0.9) +
        labs(x = "Taxon", 
             y = "Research Grade (RG) Proportion") +
        scale_y_continuous(
                limits = c(0,1),
                expand = expansion(mult = c(0, 0)),
                breaks = scales::breaks_extended(n = 5))+
        
        scale_x_discrete(labels = c("animals_verifiable" = "Animals",
                                    "plants_verifiable" = "Plants",
                                    "fungi_verifiable" = "Fungi")) +
        scale_fill_manual(values = c("animals_verifiable" = "#5FA8D3",   # Blue
                                     "plants_verifiable" = "#7BC47F",    # Green
                                     "fungi_verifiable" = "#E06A5F")) +  # Pink
        theme_cowplot(font_size = 13) +
        theme(axis.title.x = element_text(margin = margin(t = 12)),
              axis.title.y = element_text(margin = margin(r = 12)),
              legend.position = "none") 


#### Quality Grade no within fungi ----
tidy_data_all <- read_csv("data/tidy_data/combined_data.csv")

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
                expand = expansion(mult = c(0, 0.1)),
                labels = scales::comma,
                breaks = scales::breaks_extended(n = 5)) +
        
         geom_text(stat = "count", 
            aes(label = paste0("n = ", scales::comma(..count..))), 
            vjust = -0.7,
            size=3.5) +
        
        #scale_y_continuous(labels = label_number(scale_cut = cut_short_scale()))+ #abbrev. numbers
        theme_cowplot(font_size = 13) +
        theme(axis.title.x = element_text(margin = margin(t = 12)),
              axis.title.y = element_text(margin = margin(r = 12)),
              legend.position = "none") 

aligned <- cowplot::align_plots(p_taxa, p_quality, align = "hv", axis = "tblr")
cowplot::plot_grid(aligned[[1]], NULL, aligned[[2]],
                   ncol = 3,
                   rel_widths = c(1, 0.2, 1),
                   label_size = 20)

plot_grid(grid, legend, ncol = 1, rel_heights = c(1, .11))


#### ccby ----
tidy_data <- read_csv("data/tidy_data/tidy_data_verifiable.csv")

#filter at species level & RG only at spp
tidy_data <- tidy_data  %>%
        
        #filter out 0 photos obs (likely a bug) 
        filter(!is.na(sum_pixels) & n_photos != 0) %>%
        
        mutate(
                #make RG only at spp level, and numeric                
                quality_sp_level = ifelse(quality_grade == "research" & specificEpithet == "Not identified", "needs_id", quality_grade), 
                rg = as.numeric(quality_sp_level == "research"),
                
                #create column for species name 
                species = as.factor(paste(genus, specificEpithet)),
                
                microscopy_project = as.factor(microscopy_project),
                genus = as.factor(genus)) %>% 
        filter(specificEpithet != "Not identified") 

tidy_data %>% 
        filter(quality_grade == "research") %>% 
        mutate(license_code = ifelse(is.na(license_code), "not licensed", license_code)) %>% 
        ggplot(aes(x = fct_rev(fct_infreq(license_code)), fill = license_code)) +
        geom_bar() +
        geom_text(
                aes(label = after_stat(count)),
                stat = "count",
                hjust = -0.1,
                size = 3.5 ) +
        labs(x = "Licence Code", 
             y = "No. of Observations") +
        scale_y_continuous(labels = label_number())+
        scale_fill_manual(values = c(
                "cc0" = "#9ACD9B",
                "cc-by" = "#9ACD9B",
                "cc-by-nc" = "#9ACD9B"
        ), na.value = "#FFB08C") +
        coord_flip()+
        theme_cowplot(font_size = 14)+
        theme(legend.position = "none")


prop_cc_gbif <- tidy_data %>% 
        filter(quality_grade == "research") %>% 
        count(license_code) %>%
        summarise(gbif = sum(n[license_code %in% c("cc0", "cc-by-nc", "cc-by")]),
                  total = sum(n),
                  prop_gbif = gbif/total)


####microscopy project ----


labels_microscopy <- tidy_data %>%
        count(microscopy_project)  

p1 <- ggplot(tidy_data, aes(x = microscopy_project, fill = quality_grade)) +
        geom_bar(position ="fill")+
        scale_fill_manual(values = c(research= "#a8cc08", needs_id= "#ffee91"),
                          labels = c(research = "Research", needs_id = "Needs ID"))+
        labs(x = "Added to \"Mushroom microscopy\" project", 
             y = "Proportion of Verifiable Observations",
             fill = "Quality Grade") +
        scale_y_continuous(
                # don't expand y scale at the lower end
                expand = expansion(mult = c(0, 0.05))) +
        theme_cowplot(font_size = 13) +
        theme(axis.title.x = element_text(margin = margin(t = 12)),
              axis.title.y = element_text(margin = margin(r = 12)),legend.position="none")+
        geom_text(data = labels_microscopy,  size = 3.5, aes(x = microscopy_project, y = 1.05, label = paste0("n = ", scales::comma(n))), inherit.aes = FALSE)


####genetic fields ----
tidy_data_obsf <- read_csv("data/tidy_data/tidy_data_genetic.csv")   

#filtering out dataset to spp level
tidy_data_obsf  <- tidy_data_obsf  %>%

        #filter out 0 photos obs (likely a bug) 
        filter(!is.na(sum_pixels) & n_photos != 0) %>%
        
        mutate(
                #make RG only at spp level, and numeric                
                quality_sp_level = ifelse(quality_grade == "research" & specificEpithet == "Not identified", "needs_id", quality_grade), 
                rg = as.numeric(quality_sp_level == "research"),
                
                #create column for species name 
                species = as.factor(paste(genus, specificEpithet)),
                
                microscopy_project = as.factor(microscopy_project),
                genus = as.factor(genus)) %>% 
        filter(specificEpithet != "Not identified") 


labels_genetic <- tidy_data_obsf %>%
        count(genetic_obsf) 

p2 <- ggplot(tidy_data_obsf, aes(x = genetic_obsf, fill = quality_grade)) +
        geom_bar(position ="fill")+
        scale_fill_manual(values = c(research= "#a8cc08", needs_id= "#ffee91"),
                          labels = c(research = "Research", needs_id = "Needs ID"))+
        labs(x = "Includes Barcoding Observation Fields", 
             y = "Proportion of Verifiable Observations",
             fill = "Quality Grade") +
        scale_y_continuous(
                # don't expand y scale at the lower end
                expand = expansion(mult = c(0, 0.05))) +
        #scale_y_continuous(labels = label_number(scale_cut = cut_short_scale()))+ #abbrev. numbers
        theme_cowplot(font_size = 13) +
        theme(axis.title.x = element_text(margin = margin(t = 12)),
              axis.title.y = element_text(margin = margin(r = 12)),legend.position="none")+
        geom_text(data = labels_genetic, size = 3.5, aes(x = genetic_obsf, y = 1.05, label = paste0("n = ", scales::comma(n))), inherit.aes = FALSE)


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

grid <- cowplot::plot_grid(p1, NULL, p2,
        ncol = 3,
        rel_widths = c(1, 0.2, 1),
        label_size = 17)
plot_grid(grid, legend, ncol = 1, rel_heights = c(1, .11))


proportions_micro <- tidy_data %>%
       count(microscopy_project, quality_grade) %>%
        group_by(microscopy_project) %>%
        mutate(prop = n / sum(n))

proportions_genetic<- tidy_data_obsf %>%
        count(genetic_obsf, quality_grade) %>%
        group_by(genetic_obsf) %>%
        mutate(prop = n / sum(n))

#needs id subset ------

#categorise needs_id observations according to presence of community id 

tidy_data <- read_csv("data/tidy_data/tidy_data_verifiable.csv")
data_community <- tidy_data %>% 
        filter(quality_grade == "needs_id") %>% 
        mutate(community_ids = if_else(n_identifiers == 0, "No community IDs", "IDed by community")) %>% 
        count(community_ids) %>% 
        mutate(proportion = n / sum(n)) 

# hsize <- 1.7
# p_needs_id<- ggplot(data_community, aes(x = hsize, y = proportion, fill = community_ids)) +
#         geom_col(width = 1) +
#         coord_polar(theta = "y") +
#         xlim(c(0.2, hsize + 0.5)) +
#         labs(fill = NULL) +
#         theme(panel.background = element_rect(fill = "white"),
#               panel.grid = element_blank(),
#               axis.title = element_blank(),
#               axis.ticks = element_blank(),
#               axis.text = element_blank(),
#               legend.title = element_text(size = 13,margin = margin(b = 14, t=0)),
#               legend.text = element_text(size = 13),
#               legend.position = "bottom")+
#         guides(fill = guide_legend(title.position = "top", title.hjust = 0.5,  nrow = 2))
# 

# grid <- cowplot::plot_grid(plotlist = list(p_needs_id, p_ccby),
#                            label_size = 13, 
#                            ncol =2, 
#                            rel_widths = c(1, 2.5))
