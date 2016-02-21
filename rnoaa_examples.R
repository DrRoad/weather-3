#' # Using the rnoaa package
#' The `rnoaa` R package from [ropensci](http://www.ropensci.org) is a
#' powerful and useful package for getting weather and climate data for US
#' weather stations. It took me some time to learn how to best use the package,
#' hopefully this article will save you repeating the steps.
#' 
#' I'll let the [official
#' tutorial](https://ropensci.org/tutorials/rnoaa_tutorial.html) best explain
#' how to get the package installed, or trust you know how to do it, so we'll
#' start from there.
#' 
#' ## Getting some data
#' First we'll try to find the `locationid` for a certain location. Locationids
#' are "geopolitical" flim-flams which define an area, and which we can use to
#' find stations for that area. The problem is that I'm not sure _exactly_ how
#' the name is specified, so I have to search for it. I wrote a simple function 
#' to search for the correct location ID, given an (hopefully) appropriate name
#+ search, message=FALSE
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


#' Let's see if it is able to find a location for Flagstaff (my home city)
#+ locationInfo
locationInfo <- searchForName("Flagstaff")
locationInfo

#' Now that we know the locationid we can find a list of stations in this area
#+ stations
flagstaffStations <- ncdc_stations(locationid=locationInfo$id, limit=1000)$data
flagstaffStations

#' I'll pick the `Fort Valley` station, not just because it has the longest
#' period of data, but because it's the closest to my home. Let's see what
#' datasets are available for this station
#+ datasets
ourFavoriteStation <- flagstaffStations[flagstaffStations$name == 'FORT VALLEY AZ, US',]
ncdc_datacats(stationid=ourFavoriteStation$id, limit=1000)
