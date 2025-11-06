# iNaturalist_analysis

Code used for data analysis in "Identifying drivers of iNaturalist fungal data quality to support conservation efforts"
## Code organisation

### Pre-processing

**"01_filter_iNat_taxa.R"**
- Filters Fungi taxa from iNaturalist taxonomic archive
- Input: "taxa.csv" - iNaturalist Taxonomy DarwinCore Archive, downloaded from <https://www.inaturalist.org/pages/developers> (downloaded 1 July 2024)
- Output: "inat_fungi_taxa.csv"

### Data tidying

**"02_retrieve_obs.R"** 
- API call for Fungi and Lichens iNaturalist observations
- results are downloaded and saved as pages of 200 obs
- Download end date: 26 June 2024
- Output: "obs.*\\.RData" files


**"03_tidy_batch_process.R"**
- processes and joins page RData files to get variables needed for analysis, using **"functions_get_vars.R"**, **"script_tidy_data.R"** and "selected_variables.csv"
- adds taxonomic data fixes taxonomic issues, using "inat_fungi_taxa.csv" and "taxon_swap_mapping.csv"
- Output: **"combined_data.csv"** (includes Research Grade, Needs ID and Casual observations) 

**"04_retrieve_genetic_variables.R"**
- retrieve genetic fields data

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

**"03_modelling_plots.R"**
- Model Interpretation with emmeans
- Plotting 

**"04_calculating_model_parameters.R"**
- Get emmeans and contrast coefficients

### Other analysis

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

**"sankey_rank_flow"** 
- Code for sankey plot

**"overall_plots.R"**
- Plotting all other analyses




