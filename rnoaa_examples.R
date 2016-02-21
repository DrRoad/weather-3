library(rnoaa)
searchForName <- function(searchTerm, locationcategoryid="CITY", 
                          numberOfMatchesToReturn=1){ 
  matches <- NULL
  offset <- 0
  chunkSize <- 1000
  repeat{
    theseCities <- ncdc_locs(locationcategoryid=locationcategoryid, 
                             limit=chunkSize, 
                             offset=offset * chunkSize)$data
    matches <- theseCities[grepl(searchTerm, theseCities$name, ignore.case=TRUE),]
    if (nrow(matches) >= numberOfMatchesToReturn | 
        nrow(theseCities) %% chunkSize > 0) break
    offset <- offset + 1
  }
  
  matches[numberOfMatchesToReturn,]
}


locationInfo <- searchForName("Flagstaff")
flagstaffStations <- ncdc_stations(locationid=locationInfo$id, limit=1000)$data
ourFavoriteStation <- flagstaffStations[flagstaffStations$name == 'FORT VALLEY AZ, US',]
ncdc_datacats(stationid=ourFavoriteStation$id, limit=1000)
