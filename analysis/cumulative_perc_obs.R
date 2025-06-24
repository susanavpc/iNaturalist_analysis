library(tidyverse)
library(cowplot)

tidy_data_verifiable <- read_csv("data/tidy_data/tidy_data_verifiable.csv")

research_2ids <- tidy_data_verifiable %>%  
        filter(!is.na(sum_pixels) & n_photos != 0) %>%
        
        #make RG only at spp level
        mutate(quality_sp_level = ifelse(quality_grade == "research" & specificEpithet == "Not identified", "needs_id", quality_grade)) %>% 
        
        filter(quality_grade  == "research" & n_identifications == 2) %>% 
        
        mutate(created_at_month = factor(created_at_month, levels = month.abb, ordered = TRUE))


rm(tidy_data_verifiable)

research_2ids <- research_2ids %>% 
        mutate(days_to_RG = as.numeric(as.Date(updated_at) - as.Date(created_at))) %>% 
        arrange(days_to_RG) %>%
        mutate(observation = 1:n())

perc <- research_2ids %>% 
        mutate(perc = observation / n()) %>% 
        select(perc, days_to_RG)

ggplot(perc, aes(x = (days_to_RG/365), y = perc)) +
        geom_step(size = 1) +
        labs(x = "Time to Research Grade (Years)",
             y = "Cumulative Proportion of Obs.") +
        scale_x_continuous(breaks = seq(0,15, by = 1)) +
        theme_cowplot(font_size = 13) +
        theme(axis.title.x = element_text(margin = margin(t = 9 )),
              axis.title.y = element_text(margin = margin(r = 9)))



perc <- research_2ids %>% 
        mutate(perc = observation / n()) %>% 
        select(perc, days_to_RG)

perc %>% filter(days_to_RG == 0)  %>% 
        slice_max(order_by = perc, n = 1) 

perc %>% 
        filter(days_to_RG == 365)  %>% 
        slice_max(perc, n = 1) #used max because I want maximum percentage achieved within 1 year

perc %>% 
        filter(perc >= 0.5)  %>% 
        slice_min(perc, n = 1) #used min because i want the first day at witch 50% is reached

perc %>% 
        filter(perc >= 0.6)  %>% 
        slice_min(perc, n = 1)

ggplot(perc, aes(x = days_to_RG, y = perc)) +
        geom_step() +
        labs(x = "Days to Research Grade",
             y = "Cumulative Proportion of Obs.") +
        coord_cartesian(xlim = c(0, 30)) +
        scale_x_continuous(breaks = seq(0, 30, by = 4)) +
        theme_cowplot(font_size = 13) +
        theme(axis.title.x = element_text(margin = margin(t = 9 )),
              axis.title.y = element_text(margin = margin(r = 9)))
