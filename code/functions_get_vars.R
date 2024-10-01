# Functions to calculate variables needed 

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
