####################################################################################################
########################### MAPLIGHT IDEAL POINT SCORES PROJECT SCRIPT #############################
####################################################################################################


############# PROJECT PROGRESS ##################

## Looked at US repo to see if there are action timestamp. Link: 
## Looked at how difficult it would be to create new column of dates from Maplight data (should be straightforward).



############# DATA MANIPULATION AND CREATION #####################

## Creation of Postition Date Column

library(readr)


pos114 <- read_csv("/Users/alexanderfurnas/Projects/Maplight_ideal_points/114positions.csv")

pos114 <- pos114 %>% mutate(position_date = lubridate::ymd(stringr::str_split(citation, "[()]")[[1]][2]))

pos114 <- pos114 %>% mutate(bill_id = paste(session, prefix, number, sep = "_"))


library(lubridate)


############# DATA ANALYSIS #################

require("emIRT")

