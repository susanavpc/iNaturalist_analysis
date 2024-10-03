# iNaturalist_analysis

## Code files:

### Pre-processing

**"01_filter_iNat_taxa.R"**
- Filters Fungi taxa from iNaturalist taxonomic archive
- Input: "taxa.csv" - iNaturalist Taxonomy DarwinCore Archive, downloaded from <https://www.inaturalist.org/pages/developers> (June 2024)
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
- processes and joins observation pages files to get variables needed for analysis, using **"functions_get_vars.R"**, **"script_tidy_data.R"** and "selected_variables.csv"
- adds taxonomic data fixes taxonomic issues, using "inat_fungi_taxa.csv" and "taxon_swap_mapping.csv"
- Output: "combined_data.csv" (includes Research Grade, Needs ID and Casual observations) 

**"05_join_countries.R"** 
- Gets countries from coordinates for verifiable observations in "combined_data.csv" (excludes Casual) using data from "gadm_euro_countries.shp"
- Joins countries to other variables and saves final tidy_data file for verifiable observations only (Research and Needs ID)
- Output: "tidy_data_verifiable.csv"

**"06_subset_2ids.R"** 
