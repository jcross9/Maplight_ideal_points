####################################################################################################
########################### MAPLIGHT IDEAL POINT SCORES PROJECT SCRIPT #############################
####################################################################################################


############# PROJECT PROGRESS ##################

## Looked at US repo to see if there are action timestamp. Link: 
## Looked at how difficult it would be to create new column of dates from Maplight data (should be straightforward).



############# DATA MANIPULATION AND CREATION #####################

## Creation of Postition Date Column

library(readr)
library(dplyr)
library(lubridate)

pos114 <- read_csv("/Users/alexanderfurnas/Projects/Maplight_ideal_points/114positions.csv")
pos114 <- read_csv("C:/Users/jcros/Documents/Maplight Ideal Points/Maplight_ideal_points/114positions.csv")

pos114 <- pos114 %>% mutate(position_date = lubridate::ymd(stringr::str_split(citation, "[()]")[[1]][2]))

pos114 <- pos114 %>% mutate(bill_id = paste(session, prefix, number, sep = "_"))


pos114 <- pos114 %>% mutate(bill_id_index = as.numeric(as.factor(as.character(bill_id))))

pos114 <- pos114 %>% mutate(org_index = as.numeric(as.factor(as.character(orgname))))

pos114$disposition.dichot <- NA
pos114$disposition.dichot[pos114$disposition == "oppose"] <- -1
pos114$disposition.dichot[pos114$disposition == "support"] <- 1

est_mat <- NA
m1 <- matrix(0, nrow = length(unique(pos114$org_index)), ncol = length(unique(pos114$bill_id_index)))
est_mat <- pos114 %>% dplyr::select(16,15,17)
est_mat <- est_mat%>% filter(complete.cases(est_mat))
est_mat <- as.matrix(est_mat)
m1[est_mat[,1:2] ]<- as.numeric(est_mat[,3])
print(dim(m1))


rc<-rollcall(m1,yea=1,nay=-1,missing=0)
rm(m1)
gc()




############# DATA ANALYSIS #################

require("emIRT")

