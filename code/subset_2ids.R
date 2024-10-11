# Create subset of research grade obs with only 2 identifications

library(tidyverse)

tidy_data_verifiable <- read_csv("data/tidy_data/tidy_data_verifiable.csv")

research_2ids <- tidy_data_verifiable %>%
        filter(quality_grade  == "research" & n_identifications == 2)

research_2ids <- research_2ids %>% 
        mutate(days_to_RG = as.numeric(as.Date(updated_at) - as.Date(created_at)))

write_csv(research_2ids, "data/tidy_data/research_2ids.csv")

#plots
research_2ids_new <- research_2ids %>%
        arrange(days_to_RG) %>%
        mutate(observation = 1:n())

research_2ids_new %>% 
        ggplot(aes(x=days_to_RG, y= observation))+
        geom_line() +
        geom_point() +
        scale_y_continuous(labels = scales::label_number()) +
        theme_minimal()

perc <- research_2ids_new %>% mutate(perc = observation / n())





days_count <- research_2ids %>%
        group_by(days_to_RG) %>%
        summarise(count = n()) %>%
        ungroup()

ggplot(days_count, aes(x = days_to_RG, y = count)) +
        geom_point() +
        labs(title = "Count of Observations by Days to RG",
             x = "Days to RG",
             y = "Count of Observations (log10)") +
        scale_y_log10(labels = scales::label_number())+
        theme_minimal()



research_2ids <- research_2ids %>%
        mutate(weeks_to_RG = as.numeric(floor(days_to_RG / 7)))

weeks_count <- research_2ids %>%
        group_by(weeks_to_RG) %>%
        summarise(count = n()) %>%
        ungroup()

ggplot(weeks_count, aes(x = weeks_to_RG, y = count)) +
        geom_point() +
        labs(title = "Count of Observations by Weeks to RG",
             x = "Weeks to RG",
             y = "Count of Observations (log10)") +
        scale_y_log10(labels = scales::label_number())+
        theme_minimal()



research_2ids <- research_2ids %>%
        mutate(months_to_RG = as.numeric(floor(days_to_RG / 30)))

months_count <- research_2ids %>%
        group_by(months_to_RG) %>%
        summarise(count = n()) %>%
        ungroup()

months_count %>% 
        # filter(count >= 1000) %>% 
        ggplot(aes(x = months_to_RG, y = count)) +
        geom_line() +
        labs(title = "Count of Observations by Months to RG",
             x = "Months to RG",
             y = "Count of Observations") +
        #scale_y_log10(labels = scales::label_number())+
        scale_y_continuous(labels = scales::label_number())+
        scale_x_continuous(breaks = seq(min(months_count$months_to_RG), 
                                        max(months_count$months_to_RG), 
                                        by = 3))+
        theme_minimal()