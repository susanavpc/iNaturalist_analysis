#Reduce global GADM file to Europe country level and convert to shp file 

library(dplyr)
library(sf)

gadm_sf <- st_read("data/gadm_410.gpkg")

# List transcontinental countries that are tagged as part of other continents
transcont_countries <- c("Russia","Turkey", "Kazakhstan", "Azerbaijan", "Georgia", "United Kingdom", "Italy", "Spain", "Netherlands", "Portugal", "Denmark", "France", "Greece", "Iceland")

# Filter for Europe 
# removed Chukot in Russia by UID due to invalid geometry and not relevant to analysis
gadm_sf_euro <- gadm_sf %>% 
        filter((CONTINENT =="Europe" | COUNTRY %in% transcont_countries) & UID != 272742) %>%
        select(c("UID","GID_0","NAME_0","COUNTRY","CONTINENT","geom"))

# Check geometry validity 
test_valid <- st_is_valid(gadm_sf_euro)
test_valid %>% unique()

# Join polygons at country level, does not keep sublevels
gadm_euro <- gadm_sf_euro %>% 
        group_by(COUNTRY) %>% 
        summarise()

st_write(gadm_euro, "data/gadm_euro_countries.shp")

