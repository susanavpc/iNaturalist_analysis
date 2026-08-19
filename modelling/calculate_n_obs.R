library(tidyverse)
library(cowplot)
library(scales)

# original cowplot get_legend not working - using function suggestion from github
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

load("data/tidy_data/data_species_level.RData")

worst_notes <- df %>%  filter(presence_notes == 1 & n_information_types == 4 & quality_grade == "needs_id")

### VERIFIABLE DATA ----
#preprocessing again because I didn't include user_id previously
tidy_data <- read_csv("data/tidy_data/tidy_data_verifiable.csv")
tidy_data <- tidy_data %>%
        select("id", "uri", "user_id", "quality_grade", "created_at", "taxon_id", "n_photos",
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
                species = as.factor(paste(genus, specificEpithet)),
                
                
                #create ysu, where day of upload counts as year 1
                ysu = ceiling((days_since_upload + 1)/365),
                log_ysu = log10(ysu),
                
                #change vars to factors
                observed_on_month = factor(
                        observed_on_month, levels = month.abb, ordered = TRUE),
                microscopy_project = as.factor(microscopy_project),
                genus = as.factor(genus),
                
                #create presence/ absence vars
                presence_projects = as.factor(ifelse(n_projects == 0, "no", "yes")),
                presence_obs_fields = as.factor(ifelse(n_obs_fields == 0, "no", "yes")),
                presence_tags = as.factor(ifelse(n_tags == 0, "no", "yes")),
                presence_notes = as.factor(presence_notes)) %>% 
        
        
        #mutate continuous variables based on bins
        mutate(
                #bin variables based on quantiles (see below)
                photos_binned = cut(n_photos, breaks = c(0,1,2,3,4,10,Inf), 
                                    labels=c("1","2","3","4","5-10", ">10")),
                projects_binned = cut(n_projects, breaks = c(-Inf, 0,1,2,3,4,10,Inf), 
                                      labels=c("0","1","2","3","4","5-10", ">10")),
                obsf_binned = cut(n_obs_fields, breaks = c(-Inf, 0,1,2,3,4,10,Inf), 
                                  labels=c("0","1","2","3","4","5-10", ">10")),
                notes_binned = cut(length_notes, breaks = c(-Inf, 0, 25, 50,100, 300, Inf), 
                                   labels=c("0", "1-25", "26-50", "51-100", "101-300", ">300")),
                tags_binned = cut(n_tags, breaks = c(-Inf,0,1,2,5,10,30,Inf), 
                                  labels=c("0","1","2","3-5","6-10","11-30", ">30")),
                
                #make continuous vars numeric
                Nproj= as.numeric(projects_binned)-1,
                Nobsf= as.numeric(obsf_binned)-1,
                Ntags= as.numeric(tags_binned)-1,
                Nnotes= as.numeric(notes_binned)-1,
                
                #make photos factor (changed due to model diagnostics)
                Nph = as.factor(photos_binned),
                
                #make presence/absence numeric
                presence_projects = as.numeric(presence_projects=="yes"),
                presence_obs_fields = as.numeric(presence_obs_fields == "yes"),
                presence_tags = as.numeric(presence_tags == "yes"),
                presence_notes = as.numeric(presence_notes == "yes"),
                
                #calculate amount of information
                information_types = factor(paste0(
                        ifelse(Nnotes>0, "N", ""),
                        ifelse(Nproj>0, "P", ""),
                        ifelse(Nobsf>0, "O", ""),
                        ifelse(Ntags>0, "T", ""))),
                n_information_types = str_length(information_types),
                
                #calculate n_info excluding self
                info_no_notes = paste0(
                        ifelse(Nproj > 0, "P", ""),
                        ifelse(Nobsf > 0, "O", ""),
                        ifelse(Ntags > 0, "T", "")),
                n_info_no_notes = str_length(info_no_notes),
                
                info_no_proj = paste0(
                        ifelse(Nnotes > 0, "N", ""),
                        ifelse(Nobsf > 0, "O", ""),
                        ifelse(Ntags > 0, "T", "")),
                n_info_no_proj = str_length(info_no_proj),
                
                info_no_obsf = paste0(
                        ifelse(Nnotes > 0, "N", ""),
                        ifelse(Nproj > 0, "P", ""),
                        ifelse(Ntags > 0, "T", "")),
                n_info_no_obsf = str_length(info_no_obsf),
                
                info_no_tags = paste0(
                        ifelse(Nnotes > 0, "N", ""),
                        ifelse(Nproj > 0, "P", ""),
                        ifelse(Nobsf > 0, "O", "")),
                n_info_no_tags = str_length(info_no_tags))


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

#### Calculate amount of data per filtering step ----
#tidy_data_filtered = 2509366
#only at spp level = 1673233
#with 30% thresholds = 1389547

#### PLOTS for n_obs and n species per threshold 
thresholds <- seq(0, 1, by = 0.05)

# For each threshold, calculate how many species have %RG >= threshold
species_count_by_threshold <- map_dfr(thresholds, function(thresh) {
        # Summarize RG proportion per species
        species_summary <- df %>%
                group_by(species) %>%
                summarise(n_obs = n(),
                          n_rg = sum(rg == 1),
                        prop_RG = mean(rg), .groups = "drop") %>% 
                filter(
                        n_obs >= 10,
                        n_rg >= 10,
                        prop_RG != 1
                )
        
        # Count how many species meet the threshold
        count <- species_summary %>%
                filter(prop_RG >= thresh) %>%
                summarise(n_species = n())
        
        # Return data frame with threshold and count
        tibble(threshold = thresh, n_species = count$n_species)
})

# Plot
p_species <- ggplot(species_count_by_threshold, aes(x = threshold, y = n_species)) +
        geom_line() +
        geom_point() +
        scale_y_continuous(
                labels = scales::comma,
                limits = c(0, NA),
                expand = expansion(mult = c(0, 0.15)),   # small headroom
                breaks = scales::breaks_extended(n = 3)
        )+
        geom_vline(xintercept = 0.3, linetype = "dashed", linewidth = 0.8, colour = "gray60") +
        labs(
                x = "Minimum RG Prop. per Species",
                y = "No. Species"
        ) +
        theme_cowplot(font_size = 13)+
        theme(axis.title.y = element_text(margin = margin(r = 12)),
              axis.title.x = element_text(margin = margin(t = 12))
        )

obs_count_by_threshold <- map_dfr(thresholds, function(thresh) {
        # Compute %RG per species
        species_summary <- df %>%
                group_by(species) %>%
                summarise(n_obs = n(),
                          n_rg = sum(rg == 1),
                          prop_RG = mean(rg), .groups = "drop") %>% 
                filter(
                        n_obs >= 10,
                        n_rg >= 10,
                        prop_RG != 1
                )
        
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
p_obs<- ggplot(obs_count_by_threshold, aes(x = threshold, y = n_obs)) +
        geom_line() +
        geom_point() +
        scale_y_continuous(
                labels = scales::comma,
                limits = c(0, NA),
                expand = expansion(mult = c(0, 0.05)),   # small headroom
                breaks = scales::breaks_extended(n = 5)
        )+
        geom_vline(xintercept = 0.3, linetype = "dashed", linewidth = 0.8, colour = "gray60") +
        labs(
                x = " Minimum RG Prop. per Species",
                y = "No. Observations"
        ) +
        theme_cowplot(font_size = 13)+
        theme(axis.title.y = element_text(margin = margin(r = 12)),
              axis.title.x = element_text(margin = margin(t = 12))
        )

plot_grid(p_obs, p_species, ncol = 1, align = "v")

### Exploring observations with a lot of tags ----

#rename top users in tags == 6
top_users_tags_6 <- data_30_test_month %>%
        
        filter(Ntags==6) %>% 
        
        group_by(user_id) %>% 
        add_count(name = "n_user") %>% 
        ungroup() %>% 
        
        distinct(user_id, n_user) %>%
        arrange(desc(n_user)) %>%
        slice_head(n = 10) %>%
        mutate(user_label =  paste0("User_",LETTERS[1:10]))

#rename top users in overall dataset 
top_users_all <- data_30_test_month %>%
        
        group_by(user_id) %>% 
        add_count(name = "n_user") %>% 
        ungroup() %>% 
        
        distinct(user_id, n_user) %>%
        arrange(desc(n_user)) %>%
        slice_head(n = 10) %>%
        mutate(user_label =  paste0("User_",LETTERS[11:20])) 

top_users_combined <- bind_rows(top_users_all, top_users_tags_6)

#join with normal dataset
data_labeled <- data_30_test_month %>%
        left_join(top_users_combined, by = "user_id")

#plot users tags ==6
p1 <- data_labeled  %>% 
        filter(Ntags==6) %>%
        mutate(research = ifelse(rg==1, "RG", "NeedsID")) %>% 
        mutate(user_label = fct_infreq(user_label)) %>%
        filter(user_label %in% levels(user_label)[1:10]) %>%
        
        ggplot(aes(x = fct_rev(user_label), fill = research)) +
        geom_bar() +
        labs(y = "No. observations",
             x = "User",
             fill = "Quality Grade")+
        scale_fill_manual(values = c(RG= "#a8cc08", NeedsID= "#ffee91"))+
        coord_flip() +
        theme_cowplot(font_size = 13) 

#plot users overall
p3 <- data_labeled  %>% 
        mutate(research = ifelse(rg==1, "RG", "NeedsID")) %>% 
        mutate(user_label = fct_infreq(user_label)) %>%
        filter(user_label  %in% levels(user_label)[1:10]) %>%
        
        ggplot(aes(x = fct_rev(user_label), fill = research)) +
        geom_bar() +
        labs(y = "No. observations",
             x = "User",
             fill = "Quality Grade")+
        scale_fill_manual(values = c(RG= "#a8cc08", NeedsID= "#ffee91"))+
        coord_flip() +
        theme_cowplot(font_size = 13) 

#plot taxa tags == 6 
p2 <- data_30_test_month %>% 
        filter(Ntags==6) %>%
        
        mutate(research = ifelse(rg==1, "RG", "NeedsID")) %>% 
        mutate(species = fct_infreq(species)) %>%
        filter(species %in% levels(species)[1:10]) %>%
        
        ggplot(aes(x = fct_rev(species), fill = research)) +
        geom_bar() +
        labs(y = "No. observations",
             x = "Species",
             fill = "Quality Grade")+
        scale_fill_manual(values = c(RG= "#a8cc08", NeedsID= "#ffee91"))+
        coord_flip() +
        theme_cowplot(font_size = 13) +
        theme(axis.text.y = element_text(face = "italic"))

p4 <-data_30_test_month %>% 
        mutate(research = ifelse(rg==1, "RG", "NeedsID")) %>% 

        mutate(species = fct_infreq(species)) %>%
        filter(species %in% levels(species)[1:10]) %>%
        
        ggplot(aes(x = fct_rev(species), fill = research)) +
        geom_bar() +
        labs(y = "No. observations",
             x = "Species",
             fill = "Quality Grade")+
        scale_fill_manual(values = c(RG= "#a8cc08", NeedsID= "#ffee91"))+
        coord_flip() +
        theme_cowplot(font_size = 13) +
        theme(axis.text.y = element_text(face = "italic"),
              legend.position = "bottom")
        
#make grid
p4 <- p4 + theme(
        legend.position = "bottom",
        legend.box = "vertical",
        legend.direction = "horizontal",
        legend.justification = "center",
        legend.title = element_text(hjust = 0.5),         # updated syntax
        legend.spacing.x = unit(0.5, "cm"),                # space between keys
        legend.box.margin = margin(t = 5)   # top margin
)

legend <- get_legend_sub(p4)

#update plots to not have legend
p1 <- p1 + theme_cowplot(font_size = 13) + theme(legend.position="none")
p2 <- p2 + theme_cowplot(font_size = 13) + theme(legend.position="none", axis.text.y = element_text(face = "italic"))
p3 <- p3 + theme_cowplot(font_size = 13) + theme(legend.position="none")
p4 <- p4 + theme_cowplot(font_size = 13) + theme(legend.position="none", axis.text.y = element_text(face = "italic"))

grid <- cowplot::plot_grid(plotlist = list(p1, p2, p3, p4),
                           labels = "auto",
                           label_size = 13, 
                           ncol =2,
                           rel_widths = c(1, 1.3))
plot_grid(grid, legend, ncol = 1, rel_heights = c(1, .1))


