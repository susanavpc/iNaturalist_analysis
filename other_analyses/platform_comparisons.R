library(tidyverse)
library(rgbif)
library(sf)
library(rnaturalearth)
library(readxl)

cutoff_date <- "2024-06-24"

fungi_key <- name_backbone("Fungi", rank = "kingdom")$usageKey

publishers <- c("iNaturalist" = "50c9509d-22c7-4a22-a47d-8c48425ef4a7" , 
                "Mushroom Observer" = "d714382d-5890-4234-ae81-696eeb53658a", 
                "Observation.org" = "8a863029-f435-446a-821e-275f4f641165")


# Get europe polygon to use in download ----

#gets gbifs europe polygon (not cropped to land margins)
gbif_raw <- st_read("https://raw.githubusercontent.com/gbif/continents/master/continent_cookie_cutter.geojson") |>
        filter(continent_part == "europe")

#download has a max 10,000 points polygon limit - polygon needs to be simplified

nrow(st_coordinates(gbif_raw))
#55386

sf_use_s2(FALSE)
gbif_simplified <- st_simplify(gbif_raw, dTolerance = 0.01, preserveTopology = TRUE)
sf_use_s2(TRUE)

nrow(st_coordinates(gbif_simplified ))
#1582

#check simplified polygon
world <- ne_countries(scale = "medium", returnclass = "sf")

ggplot() +
        geom_sf(data = world, fill = "grey92", colour = "grey70", linewidth = 0.2) +
        geom_sf(data = gbif_raw, fill = NA, colour = "black", linewidth = 0.3) +
        geom_sf(data = gbif_simplified, fill = NA, colour = "firebrick", linewidth = 0.5, linetype = "dashed") +
        coord_sf(xlim = c(-45, 80), ylim = c(30, 88), expand = FALSE) +
        theme_minimal() +
        labs(title = "GBIF Europe boundary: raw (black, ~55k vertices) vs simplified (red dashed, ~1.6k vertices)")

#polygon needs to be in text for search
europe_wkt <- st_as_text(st_geometry(gbif_simplified)[[1]])

#fixes issue with polygon related to inside vs outside of polygon
flip_ring_if_cw <- function(wkt) {
        geom <- st_as_sfc(wkt, crs = 4326)[[1]]
        coords <- st_coordinates(geom)[, 1:2]
        
        # shoelace formula: positive = counter-clockwise, negative = clockwise
        signed_area <- sum((coords[-nrow(coords), 1] * coords[-1, 2]) -
                                   (coords[-1, 1] * coords[-nrow(coords), 2])) / 2
        
        if (signed_area < 0) {
                coords_rev <- coords[nrow(coords):1, ]
                st_as_text(st_polygon(list(coords_rev)))
        } else {
                wkt
        }
}

europe_wkt_fixed <- flip_ring_if_cw(europe_wkt)


# Data download ----

download_key <- occ_download(
        pred("taxonKey", fungi_key),
        pred("basisOfRecord", "HUMAN_OBSERVATION"),
        pred_in("datasetKey", unname(publishers)),
        pred_within(europe_wkt_fixed),
        pred("hasCoordinate", TRUE),
        pred("hasGeospatialIssue", FALSE),
        pred_lte("eventDate", cutoff_date),
        format = "SIMPLE_CSV"
)

occ_download_wait(download_key)  

dir.create("data/data_gbif_comparison", showWarnings = FALSE)
dl_path <- occ_download_get(download_key, path = "data/data_gbif_comparison", overwrite = TRUE)
dat_raw <- occ_download_import(dl_path)

saveRDS(dat_raw, "data/data_gbif_comparison/fungi_europe_human_obs_raw.rds")

# Clean data ----

key_to_name <- setNames(names(publishers), publishers)

data <- dat_raw %>%
        mutate(publisher = key_to_name[datasetKey],
               #get year
               year= as.integer(substr(eventDate, 1, 4))) %>%
        filter(!is.na(publisher),
               !is.na(year),
               !is.na(decimalLatitude), !is.na(decimalLongitude)) 

saveRDS(data, "data/data_gbif_comparison/fungi_europe_clean.rds")

# Plot maps ----

#change projection to fit Europe better (EPSG:3035)
data_sf <- st_as_sf(data, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)
data_proj <- st_transform(data_sf, 3035)

data_plot <- data_proj %>%
        st_coordinates() %>%
        as.data.frame() %>%
        bind_cols(publisher = data_proj$publisher)

#reproject base map
europe_basemap <- ne_countries(scale = "medium", continent = "Europe", returnclass = "sf") %>%
        st_transform(3035)

europe_boundary_sf <- st_as_sfc(europe_wkt, crs = 4326) %>%
        st_transform(3035) %>%
        st_make_valid()

europe_basemap_clipped <- europe_basemap %>%
        st_make_valid() %>%
        st_intersection(europe_boundary_sf)

ggplot() +
        geom_sf(data = europe_basemap_clipped, fill = "grey95", colour = "grey70", linewidth = 0.2) +
        stat_bin_hex(
                data = data_plot, aes(x = X, y = Y),
                bins = 60, alpha = 0.9
        ) +
        scale_fill_viridis_c(trans = "log10", 
                             labels = label_comma(), 
                             name = "No. observations") +
        coord_sf(crs = 3035, datum = NA) +   # datum = NA drops the lon/lat grid, which isn't meaningful in a projected view
        facet_wrap(~publisher, ncol = 3) +
        theme_minimal(base_size = 14) +
        theme(axis.title = element_blank(),
              legend.position = "bottom")
              #panel.border = element_rect(colour = "grey40", fill = NA, linewidth = 0.4))

# Plot n_obs comparison between platforms  ----

df <- read_excel("data/platform_comparison.xlsx", sheet = "Sheet1")

# order platforms
platform_order <- df  %>% arrange(rev(n_obs_global)) %>% pull(Platform)

df_long <- df %>%
        pivot_longer(
                cols = c(n_obs_global, n_obs_global_before_cutoff),
                names_to = "metric", values_to = "n_obs"
        ) %>%
        mutate(
                Platform = factor(Platform, levels = platform_order),
                metric = factor(
                        metric,
                        levels = c("n_obs_global", "n_obs_global_before_cutoff"),
                        labels = c("Current", "Before download cutoff date")
                )
        )

ggplot(df_long, aes(x = Platform, y = n_obs)) +
        geom_col() +
        facet_wrap(~metric) +
        scale_y_continuous(labels = label_comma(),
                           expand = expansion(mult = c(0, 0.05))) +
        labs(
                x = NULL, y = "No.observations",
        ) +
        theme_cowplot(font_size = 14)+
        theme(legend.position = "none")
