##
## Update by Zander 11.6.2017

rm(list=ls())
library(readr)
library(dplyr)
library(lubridate)
setwd("/Users/alexanderfurnas/Projects/Maplight_ideal_points/replication data/")
#read in all of the bill positions, cbp, votes etc.
pos114 <- read_csv("114positions.csv")
pos114 <- pos114 %>% dplyr::select(-X1)
pos_all <- read_csv("all_positions.csv")
#read in roll call votes
rollcalls <- read_csv("HSall_rollcalls.csv")
votes <- read_csv("HSall_votes.csv")
#read in the congressional bill project to get topic codes
cbp <- read_csv("bills93-114 2.csv")
cbp_codes <- read_csv("CBP_MajorCodes.csv")
cbp_codes$Major <- as.character(cbp_codes$Major)
cbp <- filter(cbp, Cong >108, ImpBill == 1)
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

cbp_select <- cbp %>% dplyr::select(BillID, BillNum, BillType, Chamber, Cong, Major)

pos_all <- pos_all %>% left_join(cbp_select, c("number" = "BillNum", "prefix" = "BillType", "session" = "Cong"))
sessions <- pos_all %>% dplyr::select(BillID, session) %>% unique()

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
pos_all_core_cat <- unique(dplyr::select(pos_all_core, BillID, Major))

rollcalls <- filter(rollcalls, congress > 108)
votes <- filter(votes, congress > 108)

#create bill id that matches cbp and maplight
rollcalls$bill_type <- gsub("[0-9]", "", rollcalls$bill_number) 
rollcalls$bill_num <- gsub("[^0-9]", "", rollcalls$bill_number)
rollcalls$BillID <- paste(rollcalls$congress, rollcalls$bill_type, rollcalls$bill_num, sep ="-")
rollcalls <- dplyr::select(rollcalls, congress, chamber, rollnumber, BillID)

rollcalls <- rollcalls %>% left_join(dplyr::select(cbp_select, BillID, Major))

rollcall_cat <- unique(dplyr::select(rollcalls, BillID, Major))

library(ggplot2)

ggplot(rollcall_cat, aes(x=as.factor(Major))) + geom_bar()
 
cbp_select <- unique(cbp_select)
cbp_select_maj <- dplyr::select(cbp_select, BillID, Major) %>% filter(!is.na(Major) & Major != 99 )
cbp_select_maj$Major <- as.character(cbp_select_maj$Major)
cbp_select_maj <- cbp_select_maj %>% left_join(cbp_codes)
cbp_select_maj <- cbp_select_maj %>% dplyr::select(-Major)
cbp_select_maj <- cbp_select_maj %>% filter(!is.na(MajorText))
cbp_select_maj$roll_call <- 0
cbp_select_maj$IG_pos <- 0
cbp_select_maj$fivecore <- 0
cbp_select_maj$use_est <- 0
cbp_select_maj$roll_call[cbp_select_maj$BillID %in% unique(rollcalls$BillID)] <- 1
cbp_select_maj$IG_pos[cbp_select_maj$BillID %in% unique(pos_all$BillID)] <- 1
cbp_select_maj$fivecore[cbp_select_maj$BillID %in% unique(pos_all_core$BillID)] <- 1
cbp_select_maj$use_est[cbp_select_maj$BillID %in% unique(final_positions_edgelist$BillID)] <- 1


mm<- model.matrix( ~ MajorText - 1, data=dplyr::select(cbp_select_maj, MajorText ))
mm <- as_tibble(mm)
cbp_select_maj_onehot <- dplyr::select(cbp_select_maj, -MajorText)
cbp_select_maj_onehot  <- cbind(cbp_select_maj_onehot , mm)
cbp_select_maj_onehot  <- as_tibble(cbp_select_maj_onehot )

names(cbp_select_maj)

# for (i in 5:23){
#   print(names(cbp_select_maj)[i])
#   print(chisq.test(cbp_select_maj[,i], cbp_select_maj$use_est), sim=T,B=50000 )
# }
# 
# rc_selection <- glm(roll_call ~MajorText,family=binomial(link='logit'),data=cbp_select_maj)
# IGpos_selection <- glm(IG_pos ~MajorText,family=binomial(link='logit'),data=cbp_select_maj)
# useest_selection <- glm(use_est ~MajorText,family=binomial(link='logit'),data=cbp_select_maj)
# IGpos_selection_rc <- glm(IG_pos ~MajorText+roll_call,family=binomial(link='logit'),data=cbp_select_maj)
# useest_selection_rc <- glm(use_est ~MajorText+roll_call,family=binomial(link='logit'),data=cbp_select_maj)


bill_table <- prop.table(table(cbp_select_maj$MajorText))*100
roll_table <- prop.table(table(cbp_select_maj$MajorText[cbp_select_maj$roll_call ==1]))*100
IG_table <- prop.table(table(cbp_select_maj$MajorText[cbp_select_maj$IG_pos ==1]))*100
use_table <- prop.table(table(cbp_select_maj$MajorText[cbp_select_maj$use_est ==1]))*100

bill_table <- cbind(bill_table,roll_table, IG_table, use_table)

bill_table <- round(bill_table , digits = 2)
library(xtable) 
xtable(bill_table)
library(stargazer)
stargazer(rc_selection,IGpos_selection, useest_selection, IGpos_selection_rc,useest_selection_rc , style="ajps")


