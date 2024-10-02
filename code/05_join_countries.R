# Get countries from coordinates for verifiable observations (excludes casual)
# Join with other variables and save final tidy_data file
#
# Input files:
# - gadm_euro_countries.shp
# - combined_data.csv

library(sf)
library(tidyverse)

points <- read_csv("data/tidy_data/combined_data.csv") %>% 
  select(id, lat, long) %>% 
  filter(quality_grade != "casual")

gadm_sf_euro <- st_read("data/gadm_euro_countries.shp")

# Make spatial points ----
points_sf <- sf::st_as_sf(points, coords = c("long", "lat"), crs = 4326)

rm(points)
gc()

# Define chunks of data to run in steps ----
chunk_size <- 100000

points_sf <- points_sf %>%
  mutate(chunk_id = ceiling(row_number() / chunk_size))

points_list <- split(points_sf, points_sf$chunk_id)

rm(points_sf)
gc()


# Loop through each chunk, do intersection and save csv files----

sf_int_list <- list()

for (i in seq_along(points_list)) {
        chunk <- points_list[[i]]
        
        points_sf_int <- st_intersection(chunk, gadm_sf_euro)
        

        st_write(points_sf_int, paste0("temporary/intersections/int_", i, ".csv"))
        
        #  store the result in a list to combine later
        #sf_int_list[[i]] <- points_sf_int
        
        rm(chunk, points_sf_int)
        gc()  
}

# Join all files ----
csv_files <- list.files(pattern = "temporary/intersections/int.*\\.csv")

combined_countries <- csv_files %>%
        lapply(read.csv) %>%  
        bind_rows()    

write_csv(combined_countries, "temporary/combined_countries.csv")


# Apply nearest feature for NAs ----

na_countries <- combined_countries %>% 
        filter(is.na(country))

na_points_sf <- st_as_sf(points, coords = c("long", "lat"), crs = 4326)

# sf function to get nearest feature
nn = st_nearest_feature(na_points_sf, gadm_sf_euro)

# convert to df
nn_df <- data.frame(country_id = nn)

# bind back to points
points_nn <- bind_cols(points, nn_df)

# remove cols not needed
gadm_countries <- gadm_sf_euro %>% 
        select(COUNTRY) %>%
        st_drop_geometry()

# add index needed to join
gadm_countries$id <- 1:nrow(gadm_countries)

# join to get country name
na_countries <- points_nn %>%
        left_join(gadm_countries, by = c("country_id" = "id")) %>%
        select(-country_id, -country) %>%
        rename(new_country = COUNTRY) 

# Join and coalesce countries ----
country_data <- left_join(country_data, na_countries, by = "id")

country_data <- country_data %>% 
        mutate(country = coalesce(country, na_country)) 

# confirm there are no more NAs
view(filter(country_data, is.na(country))) 

country_data <- country_data %>%
        select(-na_country)

# Save final combined result (optional) ----
write_csv(country_data, "country_data.csv")


# Join countries with observation tidy data and save ----

tidy_data_verifiable <- read_csv("data/tidy_data/combined_data.csv") %>% 
  filter(quality_grade != "casual") 

tidy_data_verifiable <- left_join(tidy_data_verifiable, country_data, by="id") 

write_csv(tidy_data_verifiable, "data/tidy_data/tidy_data_verifiable.csv")



