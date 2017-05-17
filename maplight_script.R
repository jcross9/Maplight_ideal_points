####################################################################################################
########################### MAPLIGHT IDEAL POINT SCORES PROJECT SCRIPT #############################
####################################################################################################


############# PROJECT PROGRESS ##################

## Looked at US repo to see if there are action timestamp. Link: 
## Need to fix the position_date column; all one date at present.



############# DATA MANIPULATION AND CREATION #####################

## Creation of Postition Date Column

library(readr)
library(dplyr)
library(lubridate)

pos114 <- read_csv("/Users/alexanderfurnas/Projects/Maplight_ideal_points/114positions.csv")
pos114 <- read_csv("C:/Users/jcros/Documents/Maplight Ideal Points/Maplight_ideal_points/114positions.csv")

pos114 <- pos114 %>% mutate(position_date = lubridate::ymd(stringr::str_split(citation, "[()]")[[1]][2]))

pos114 <- pos114 %>% mutate(bill_id = paste(session, prefix, number, sep = "_"))


############# DATA ANALYSIS #################

require("emIRT")



