##
## Update by Zander 11.6.2017

rm(list=ls())
library(readr)
library(dplyr)
library(lubridate)
setwd("/Users/alexanderfurnas/Projects/Maplight_ideal_points/replication data/")
#read in all of the bill positions, cbp, votes etc.
pos114 <- read_csv("114positions.csv")
pos114 <- pos114 %>% select(-X1)
pos_all <- read_csv("all_positions.csv")
#read in roll call votes
rollcalls <- read_csv("HSall_rollcalls.csv")
votes <- read_csv("HSall_votes.csv")
#read in the congressional bill project to get topic codes
cbp <- read_csv("bills93-114 2.csv")

pos_all <- bind_rows(pos_all, pos114)

#standardize bill types in CBP to match Maplight ids
pos_all$prefix[pos_all$prefix == "S"] <- "s"
pos_all$prefix[pos_all$prefix == "H"] <- "hr"
pos_all$prefix[pos_all$prefix == "HR"] <- "hres"
pos_all$prefix[pos_all$prefix == "HC"] <- "hconres"
pos_all$prefix[pos_all$prefix == "SR"] <- "sres"
pos_all$prefix[pos_all$prefix == "SJ"] <- "sjres"
pos_all$prefix[pos_all$prefix == "HJ"] <- "hjres"
pos_all$prefix[pos_all$prefix == "SC"] <- "sconres"

cbp_select <- cbp %>% select(BillID, BillNum, BillType, Chamber, Cong, IntrDate, Major)

pos_all <- pos_all %>% left_join(cbp_select, c("number" = "BillNum", "prefix" = "BillType", "session" = "Cong"))
sessions <- pos_all %>% select(BillID, session) %>% unique()

pos_bills <- unique(pos_all$BillID)

#Create the edge matrix so that we can k-core (5) filter the bills
edge_mat <- NA
edge_mat <- pos_all %>% dplyr::select(orgname,BillID)
edge_mat <- edge_mat%>% filter(complete.cases(edge_mat))
edge_mat <- as.matrix(edge_mat)

#kcore filtration
library(igraph)
est_g <- graph_from_edgelist(edge_mat, directed = FALSE)
core <- coreness(est_g)
V(est_g)$core <- coreness(est_g)
core5 <- induced_subgraph(est_g,V(est_g)$core>4)
#extract ids of the bills and orgs to keep
core_names <- V(core5)$name

#filter bills and orgs to the 5 core
pos_all_core <- filter(pos_all, BillID %in% core_names & orgname %in% core_names)
pos_all_core_cat <- unique(select(pos_all_core, BillID, Major))

rollcalls <- filter(rollcalls, congress > 108)
votes <- filter(votes, congress > 108)

#create bill id that matches cbp and maplight
rollcalls$bill_type <- gsub("[0-9]", "", rollcalls$bill_number) 
rollcalls$bill_num <- gsub("[^0-9]", "", rollcalls$bill_number)
rollcalls$BillID <- paste(rollcalls$congress, rollcalls$bill_type, rollcalls$bill_num, sep ="-")
rollcalls <- dplyr::select(rollcalls, congress, chamber, rollnumber, BillID)

rollcalls <- rollcalls %>% left_join(select(cbp_select, BillID, Major))

rollcall_cat <- unique(select(rollcalls, BillID, Major))

library(ggplot2)

ggplot(rollcall_cat, aes(x=as.factor(Major))) + geom_bar()




##################LOOKING AT SCORES############
