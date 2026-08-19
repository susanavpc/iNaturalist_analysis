# Functions to calculate variables needed (not all were used in the end)

#get sum of number of pixels for all photos in observation
get_sum_pixels <- function(i, dataset = obs){
        sum_pixels <- sum(dataset$photos[[i]]$original_dimensions.width * dataset$photos[[i]]$original_dimensions.height)
        return(sum_pixels)
}

#get taxon rank of the initial id given by observer (if user.id matches on 1st id)
get_observer_taxon_rank <- function(i, dataset = obs){
        if ((is.null(dataset$identifications[[i]]) || 
             !is.data.frame(dataset$identifications[[i]]) || 
             nrow(dataset$identifications[[i]]) == 0)) {
                return("No ids")
        }
        if (dataset$user.id[i] == dataset$identifications[[i]]$user.id[1]) {
                return(dataset$identifications[[i]]$taxon.rank[1])
        } else {
                return("First id by community") }
}

#get taxon rank of the initial id given by observer (if user.id matches on 1st id)
get_observer_taxon_id <- function(i, dataset = obs){
        if ((is.null(dataset$identifications[[i]]) || 
             !is.data.frame(dataset$identifications[[i]]) || 
             nrow(dataset$identifications[[i]]) == 0)) {
                return(NA)
        }
        if (dataset$user.id[i] == dataset$identifications[[i]]$user.id[1]) {
                return(dataset$identifications[[i]]$taxon.id[1])
        } else {
                return(NA) 
        }
}


# get GBIF link   
get_gbif_url <- function(i, dataset = obs){
                outlinks <- dataset$outlinks[[i]]
                if (nrow(outlinks) == 0 || 
                    is.null(outlinks) || 
                    !is.data.frame(outlinks)) {
                        NA
                } else {
                       gbif_link <- outlinks %>%
                                filter (source == "GBIF") %>%
                                select(url) %>% .$url
                if (length(gbif_link) == 0) {
                        return(NA)
                } else {
                        return(gbif_link)
                }
}}


# get number of photos
get_n_photos <- function(i, dataset = obs){
        if (is.null(dataset$photos[[i]]) || 
            !is.data.frame(dataset$photos[[i]]) || 
            nrow(dataset$photos[[i]]) == 0) {
                0
        } else {
        nrow(dataset$photos[[i]])
        }
}


# get days since upload
get_days_upload <- function(i, dataset = obs){
        if (dataset$created_at[i]=="" || is.na(dataset$created_at[i])) {
                NA
        } else {
        ymd_hms(dataset$created_at[[i]]) %>%
                as.Date() %>%
                difftime(as.Date("2024-06-26"), ., units="days") #using fixed date of last downloaded file from API instead of today() because of processing time
        }
}

# get number of identifiers (excludes observer)
get_n_identifiers <- function(i, dataset = obs){
        as.list(dataset$non_owner_ids[[i]]$user.id) %>% n_distinct()
}

# get number of identifications - NOTE: includes different ids from the same identifiers
get_n_identifications <- function(i, dataset = obs){
       nrow(dataset$identifications[[i]]) 
}


# get number of observation fields
get_obs_fields <- function(i, dataset = obs){
        if (is.null(dataset$ofvs[[i]]) || 
            !is.data.frame(dataset$ofvs[[i]]) || 
            nrow(dataset$ofvs[[i]]) == 0) {
                0
        } else {
                nrow(dataset$ofvs[[i]])
        }
}

# get whether they have most used genetic obs fields
get_genetic_obsf <- function(i, dataset = obs){
        if (any(dataset$ofvs[[i]]$name == "DNA Barcode ITS" |  
                dataset$ofvs[[i]]$name == "GenBank number (URL)" | 
                dataset$ofvs[[i]]$name == "Genbank Accession Number")){
                "yes"
        } else {
                "no"
        }
}


# get number of tags
get_n_tags <- function(i, dataset = obs){
        if (length(obs$tags[[i]])==0) {
                0
        } else {
                length(dataset$tags[[i]])
        }
}

# get number of projects observation has manually been added to 
get_n_projects <- function(i, dataset = obs){
        length(dataset$project_ids[[i]])
}

# get presence in mushroom microscopy project (project id = 176528)
get_microscopy_project <- function(i, dataset = obs){
        if (any(dataset$project_ids[[i]] == 176528)){
                "yes"
        } else {
                "no"
        }
}



# get presence of notes
get_presence_notes <- function(i, dataset = obs){
        if (dataset$description[i]=="" || is.na(dataset$description[i])){
                "no"
        } else {
                "yes"
        }
}

# get length of notes
get_length_notes <- function(i, dataset = obs){
        if (dataset$description[i]=="" || is.na(dataset$description[i])){
                0
        } else {
                str_length(obs$description[i])
        }
}
