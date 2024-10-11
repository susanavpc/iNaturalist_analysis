# Get countries from coordinates for verifiable observations (excludes casual)
# Join with other variables and save final tidy_data file
#
# Input files:
# - gadm_euro_countries.shp
# - combined_data.csv

library(sf)
library(tidyverse)

points <- read_csv("data/tidy_data/combined_data.csv") %>% 
  filter(quality_grade != "casual") %>% 
  select(id, lat, long)

gadm_sf_euro <- st_read("data/gadm/gadm_euro_countries.shp")

# Make spatial points ----
points_sf <- st_as_sf(points, coords = c("long", "lat"), crs = 4326)

rm(points)
gc()

# Define chunks of data to run in steps ----
chunk_size <- 100000

points_sf <- points_sf %>%
  mutate(chunk_id = ceiling(row_number() / chunk_size))

points_list <- split(points_sf, points_sf$chunk_id)

# Loop through each chunk, do intersection and save csv files----

sf_int_list <- list()

if (!dir.exists("temporary/intersections")) {
  dir.create("temporary/intersections")
}

for (i in seq_along(points_list)) {
        chunk <- points_list[[i]]
        
        points_sf_int <- st_intersection(chunk, gadm_sf_euro)

        st_write(points_sf_int, paste0("temporary/intersections/int_", i, ".csv"))
        
        rm(chunk, points_sf_int)
        gc()  
}

# Join all files ----
csv_files <- list.files(path="temporary/intersections",
                        pattern = "int.*\\.csv",
                        full.names = TRUE)

combined_countries <- csv_files %>%
  lapply(read_csv) %>%  
  bind_rows() %>% 
  rename(country=COUNTRY)

write_csv(combined_countries, "temporary/combined_countries.csv")


# Select points don't have gadm correspondence and apply nearest feature ----

na_points <-  combined_countries %>% 
  select(-chunk_id) %>% 
  left_join(points_sf, ., by="id") %>% 
  filter(is.na(country)) %>% 
  select(-country, -chunk_id)


# sf function to get nearest feature
nn = st_nearest_feature(na_points, gadm_sf_euro)

# convert to df
nn_df <- data.frame(country_id = nn)

# bind back to points
points_nn <- bind_cols(na_points, nn_df)

# remove cols not needed
gadm_countries <- gadm_sf_euro %>% 
  select(COUNTRY) %>%
  st_drop_geometry()

# add index needed to join 
gadm_countries$id <- 1:nrow(gadm_countries)

# join to get country name
new_countries <- points_nn %>%
  left_join(gadm_countries, by = c("country_id" = "id")) %>%
  select(-country_id) %>%
  rename(new_country = COUNTRY) 

# Join and coalesce countries ----
country_data <- bind_rows(combined_countries, new_countries) %>% 
  select(-chunk_id, -geometry)

country_data <- country_data %>% 
  mutate(country = coalesce(country, new_country)) %>%  

# confirm there are no more NAs
count(filter(country_data, is.na(country))) 

country_data <- country_data %>%
  select(-new_country)

# Save combined result (optional) ----
write_csv(country_data, "temporary/country_data.csv")

# Join countries with observation tidy data and save ----
rm(list = setdiff(ls(), "country_data"))

tidy_data_verifiable <- read_csv("data/tidy_data/combined_data.csv") %>% 
  filter(quality_grade != "casual") 

tidy_data_verifiable <- left_join(tidy_data_verifiable, country_data, by="id") 

write_csv(tidy_data_verifiable, "data/tidy_data/tidy_data_verifiable.csv")



