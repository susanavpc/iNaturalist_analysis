# iNaturalist_analysis

## Code organisation

### Pre-processing

**"01_filter_iNat_taxa.R"**
- Filters Fungi taxa from iNaturalist taxonomic archive
- Input: "taxa.csv" - iNaturalist Taxonomy DarwinCore Archive, downloaded from <https://www.inaturalist.org/pages/developers> (downloaded 1 July 2024)
- Output: "inat_fungi_taxa.csv"

**"02_process_GADM_europe.R"** 
- Reduces global GADM file to Europe at country level
- Input: "gadm_410.gpkg" , GADM world GeoPackage, version 4.1, downloaded from <https://gadm.org/download_world.html> (July 2024)
- Output: "gadm_euro_countries.shp"


### Data tidying

**"03_retrieve_obs.R"** 
- API call for Fungi and Lichens iNaturalist observations
- results are downloaded and saved as pages of 200 obs
- Download end date: 26 June 2024
- Output: "obs.*\\.RData" files


**"04_tidy_batch_process.R"**
- processes and joins page RData files to get variables needed for analysis, using **"functions_get_vars.R"**, **"script_tidy_data.R"** and "selected_variables.csv"
- adds taxonomic data fixes taxonomic issues, using "inat_fungi_taxa.csv" and "taxon_swap_mapping.csv"
- Output: **"combined_data.csv"** (includes Research Grade, Needs ID and Casual observations) 

**"05_join_countries.R"** 
- Gets countries from coordinates for verifiable observations in "combined_data.csv" (excludes Casual) using data from "gadm_euro_countries.shp"
- Joins countries to other variables and saves final tidy_data file for verifiable observations only (Research and Needs ID)
- Output: **"tidy_data_verifiable.csv"**

### Modelling

**"01_model_data_preprocessing.R"**
- Mutates verifiable data to RG only at sp. level (e.g. RG at genus --> needs_id)
- Creates variables needed for modelling 
- Output: **"data_sp_RG_level.RData"** & **"data_species_level.RData"** (same as previous but only data at species level and below)

**"02_modelling_final.R"**
- Filters data data that has a chance at reaching RG : 30%RG, over 10 obs, over 10 RG obs
- Model fits with glmmTB + AIC selection + Diagnostics 
- Results from AIC selection in **"model_manual_drops.xlsx"**
- Output: **"data_30perc.RData"**, **"model_final.RData"**

**"03_model_plots.R"**
- Model Interpretation with emmeans
- Plotting 

### Additional Code

**"get_iNat_country_prop.R"** 
- Gets number of occurrences for plants, animals and fungi from iNaturalist data and calculates proportion for fungi per country, at Europe level and globally
- Initial API call is used to get place ID from country names in dataset followed by call to get number of observations
- API call for n_observations at European level and global
- Input: "data_country_names.csv" (list of countries in dataset)
- Output: "iNat_country_prop.csv" & "iNat_prop_global_europe.csv" & "country_place_ids.csv"
- Cut-off date: 26 June 2024 (matches observations dataset download)

**"get_gbif_country_prop.R"** 
- Gets number of occurrences for plants, animals and fungi from GBIF and calculates proportion for fungi per country, at Europe level and globally
- Input:"data_country_names.csv" (list of countries in dataset) & 
        "country_iso_codes.csv" (country iso codes downloaded from <https://github.com/lukes/ISO-3166-Countries-with-Regional-Codes/blob/master/all/all.csv>)
- Output: "gbif_country_prop_filtered.csv" &
         "gbif_prop_global_europe.csv"
- Date of download: 20 Jan 2025

**"join_inat_gbif_proportion.R"**

- Joins gbif and iNat fungi proportion data obtained through "get_gbif_country_prop.R" and "get_iNat_country_prop.R"
- Keeps only verifiable data from iNaturalist
- Output: "country_fungi_prop.csv" & "region_fungi_prop.csv"

**"get_country_obser_iders.R"** 

- Gets number of fungi observers and identifiers per country, Europe and globally 
- Only includes observers and identifiers of verifiable observations
- Cut-off date: 26 June 2024 (matches observations download)
- Input: "country_place_ids.csv"

**"get_country_population.R"** 

- Gets data country population data from United Nations World Population Prospects 2024 & edit values for European parts of Russia and Turkey
- Input: "WPP2024_Demographic_Indicators_Medium.csv" & "gbif_country_prop_filtered.csv"
- Output: "pop_data_countries.csv"



