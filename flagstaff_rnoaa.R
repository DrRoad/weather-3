library(rnoaa)
library(dplyr)

search_for_name <- function(search_term, locationcategoryid="CITY", 
                          number_of_matches_to_return=1){ 
  matches <- NULL
  offset <- 0
  chunk_size <- 1000
  repeat{
    these_cities <- ncdc_locs(locationcategoryid=locationcategoryid, 
                              limit=chunk_size, 
                              offset=offset * chunk_size)$data
    matches <- these_cities[grepl(search_term, these_cities$name, ignore.case=TRUE),]
    if (nrow(matches) >= number_of_matches_to_return | 
        nrow(these_cities) %% chunk_size > 0) break
    offset <- offset + 1
  }
  
  matches[number_of_matches_to_return,]
}


location_info <- search_for_name("Flagstaff")
flagstaff_stations <- ncdc_stations(locationid=location_info$id, limit=1000)$data
#ghcnd_station <- flagstaff_stations %>% filter(id == 'GHCND:USW00003103')
#datacats <- ncdc_datacats(stationid=ghcnd_station)
flagstaff_ghcnd_id <- 'USW00003103'
dd <- ghcnd_search(flagstaff_ghcnd_id)
