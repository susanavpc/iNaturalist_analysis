# iNaturalist_analysis

Code used for data processing and analysis for the manuscript "Well-documented iNaturalist fungal observations result in higher-quality data for research and conservation"

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
- Output: **"combined_data.csv"** (includes Research Grade, Needs ID and Casual observations) and **"tidy_data_verifiable.csv"** (no casual)

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


**"sankey_rank_flow"** 
- Code for sankey plot

**"overall_plots.R"**
- Plotting all other analyses




