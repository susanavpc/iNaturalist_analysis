# Retrieve observations of Fungi and Lichens in Europe and save individual pages

library(dplyr)
library(httr)
library(jsonlite)

# API call function 
get_obs <- function(max_id){
        # max results per page possible ordered from lowest to highest ID
        call <- paste0("https://api.inaturalist.org/v1/observations?place_id=97391&taxon_id=47170&per_page=200&order_by=id&order=asc&id_above=", 
                       max_id) 
        GET(url = call) %>%
                content(as = "text", encoding = "UTF-8") %>%
                fromJSON(flatten = TRUE) -> get_call_json
        
        as.data.frame(get_call_json$results)
        
}

# Get first page and update values
obs <- get_obs(max_id = 0)
page_list <- list(page_1 = obs)

max_id <- max(obs[["id"]]) 
page <- 1
save(obs,file=paste0("data/raw/obs_",page,".RData"))

# Retrieve and save observations
# loop condition is based on nrow of obs of last page retrieved

while (nrow(obs) == 200) { 
        Sys.sleep(0.5)
        page <- page + 1
        page_count <- paste("page", page, sep = "_")
        obs <- get_obs(max_id = max_id)
        page_list[[page_count]] <- obs
        max_id <- max(obs[["id"]])
        print(page_count)
        print(max_id)
        save(obs,file=paste0("data/raw/obs_",page,".RData"))
}


