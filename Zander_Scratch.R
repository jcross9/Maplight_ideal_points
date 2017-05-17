library(readr)
library(dplyr)
library(lubridate)

pos114 <- read_csv("/Users/alexanderfurnas/Projects/Maplight_ideal_points/114positions.csv")

pos114 <- pos114 %>% mutate(position_date = lubridate::ymd(stringr::str_split(citation, "[()]")[[1]][2]))

pos114 <- pos114 %>% mutate(bill_id = paste(session, prefix, number, sep = "_"))

pos114 <- pos114 %>% mutate(bill_id_index = as.numeric(as.factor(as.character(bill_id))))

pos114 <- pos114 %>% mutate(org_index = as.numeric(as.factor(as.character(orgname))))

pos114$disposition.dichot <- NA
pos114$disposition.dichot[pos114$disposition == "oppose"] <- -1
pos114$disposition.dichot[pos114$disposition == "support"] <- 1


m1 <- matrix(0, nrow = length(unique(pos114$org_index)), ncol = length(unique(pos114$bill_id_index)))
est_mat <- as.matrix(est_dat2[,2:4])
m1[est_dat2_mat[,1:2] ]<- as.numeric(est_dat2_mat[,3])
print(dim(m1))


m1 <- matrix(0, nrow = length(unique(est_dat2$state_leg_id_index)), ncol = length(unique(est_dat2$state_vote_id_index)))
est_dat2_mat <- as.matrix(est_dat2[,2:4])
m1[est_dat2_mat[,1:2] ]<- as.numeric(est_dat2_mat[,3])
print(dim(m1))