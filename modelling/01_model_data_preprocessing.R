library(tidyverse)
library(glmmTMB)
library(DHARMa)
library(sjPlot)

load("data/tidy_data/tidy_data_verifiable.RData")

#data processing for RG at spp level ----
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


# Where to cut to bin variable "quality"? ----

# quantile(df %>% filter(length_notes>0) %>% pull(length_notes), c(0.5,0.75, 0.9, 0.95, 0.99,1))
# ecdf(df %>% filter(length_notes>0) %>% pull(length_notes)) (c(25, 50,100, 300, Inf))
# # 
# quantile(df %>% filter(n_tags>1) %>% pull(n_tags), c(0.5,0.75, 0.9, 0.95, 0.99,1))
# ecdf(df %>% filter(n_tags>1) %>% pull(n_tags)) (c(1,2,5,10,30,Inf))
# 
# quantile(df %>% filter(n_obs_fields>1) %>% pull(n_obs_fields), c(0.5,0.75, 0.9, 0.95, 0.99,1))
# ecdf(df %>% filter(n_obs_fields>1) %>% pull(n_obs_fields)) (c(1,2,3,4,10,Inf))
# 
# quantile(df %>% filter(n_projects>1) %>% pull(n_projects), c(0.5,0.75, 0.9, 0.95, 0.99,1))
# ecdf(df %>% filter(n_projects>1) %>% pull(n_projects)) (c(1,2,3,4,10,Inf))
# # 
# quantile(df %>% pull(n_photos), c(0,0.5,0.75, 0.9, 0.95, 0.99,1))
# ecdf(df %>% pull(n_photos)) (c(1,2,3,4,10,Inf))

                        
save(tidy_data, file = "data/tidy_data/data_sp_RG_level.RData")                        
               
##filter data that has been identified to species level ----
df <- tidy_data %>%
        filter(specificEpithet != "Not identified") 

save(df, file = "data/tidy_data/data_species_level.RData")
