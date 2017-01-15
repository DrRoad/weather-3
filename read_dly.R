# 
# Each ".dly" file contains data for one station.  The name of the file
# corresponds to a station's identification code.  For example, "USC00026481.dly"
# contains the data for the station with the identification code USC00026481).
# 
# Each record in a file contains one month of daily data.  The variables on each
# line include the following:
# 
# ------------------------------
# Variable   Columns   Type
# ------------------------------
# ID            1-11   Character
# YEAR         12-15   Integer
# MONTH        16-17   Integer
# ELEMENT      18-21   Character
# VALUE1       22-26   Integer
# MFLAG1       27-27   Character
# QFLAG1       28-28   Character
# SFLAG1       29-29   Character
# VALUE2       30-34   Integer
# MFLAG2       35-35   Character
# QFLAG2       36-36   Character
# SFLAG2       37-37   Character
#   .           .          .
#   .           .          .
#   .           .          .
# VALUE31    262-266   Integer
# MFLAG31    267-267   Character
# QFLAG31    268-268   Character
# SFLAG31    269-269   Character

library(readr)
library(tidyr)
library(dplyr)
library(lubridate)

read_dly <- function(f, data_cols=c('TMAX', 'TMIN', 'TAVG', 'PRCP', 'SNOW',
                                    'SNWD', 'TSUN')){
  day_widths <- c(5, 1, 1, 1)
  col_widths <- c(11,4, 2, 4, rep(day_widths, 31))

  day_names <- unlist(lapply(1:31, function(n) 
                             paste0(c('VALUE', 'MFLAG', 'QFLAG', 'SFLAG'), n)))
  col_names <- c('ID', 'YEAR', 'MONTH', 'ELEMENT', day_names)
  widths <- fwf_widths(col_widths, col_names)
  # May want to put in column types and remove guess_max. Was getting integer 
  # values for some of the flag columns with default
  read_fwf(f, widths, guess_max=1e5, na=c("-9999","")) %>%
    gather(var_day, value, -ID, -YEAR, -MONTH, -ELEMENT) %>% 
    extract(var_day, c("var", "day"), "([A-Z]+)([0-9]+)") %>%
# for now, ignore metadata
    dplyr::filter(var == "VALUE") %>%
    spread(ELEMENT, value) %>%
    mutate(date=ymd(paste0(YEAR, MONTH, day))) %>%
# Some impossible dates here eg 2/30 11/31 etc
    filter(!is.na(date)) %>%
    select(ID, date, one_of(data_cols))
}
dly_file <- "USW00003103.dly"

j <- read_dly(dly_file) 


