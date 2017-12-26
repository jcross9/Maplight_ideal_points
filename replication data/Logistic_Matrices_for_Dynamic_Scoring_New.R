##############################################################################
######## DYNAMIC IG/MC SCORES, USING MCMCPACK() ##############################
##############################################################################

rm(list=ls())
library(readr)
library(dplyr)
library(lubridate)
library(stringr)
setwd("/Users/alexanderfurnas/Projects/Maplight_ideal_points/replication data/")
setwd("C:/Users/jcros/Documents/Maplight_ideal_points/")
setwd("/home/jessemc/Maplight_ideal_points")

#read in all of the bill positions, cbp, votes etc.
pos_all <- read_csv("all_positions_including114-csv.csv")


#read in roll call votes
rollcalls <- read_csv("HSall_rollcalls.csv")
votes <- read_csv("HSall_votes.csv")

#read in the congressional bill project to get topic codes
cbp <- read_csv("bills93-114 2.csv")
dim(pos_all)

unique(cbp$BillType)
unique(pos_all$prefix)

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



pos_all_edges <- pos_all %>% select(orgname, BillID, disposition)
pos_all_edges <- pos_all_edges %>% filter(complete.cases(pos_all_edges)) 
sessions <- pos_all %>% select(BillID, session) %>% unique()



rollcalls <- filter(rollcalls, congress > 108 & (grepl("passage", vote_desc) | grepl("Passage", vote_question)))
votes <- filter(votes, congress > 108)

votes$disposition <- NA
votes$disposition[votes$cast_code == 1] <- "support"
votes$disposition[votes$cast_code == 6] <- "oppose"

votes <- votes %>% filter(!is.na(disposition))

rollcalls$bill_type <- gsub("[0-9]", "", rollcalls$bill_number) 
rollcalls$bill_num <- gsub("[^0-9]", "", rollcalls$bill_number)
rollcalls$BillID <- paste(rollcalls$congress, rollcalls$bill_type, rollcalls$bill_num, sep ="-")
rollcalls <- dplyr::select(rollcalls, congress, chamber, rollnumber, BillID, date)

last_rolls <- rollcalls %>% group_by(BillID) %>% summarise(lastrolldate = max(date))

rollcalls <- rollcalls %>% left_join(last_rolls)
rollcalls <- rollcalls %>% filter(date == lastrolldate)

rollcalls <- rollcalls %>% left_join(votes)


rollcalls_pos <- rollcalls %>% select(BillID, icpsr, disposition) %>% rename(orgname = icpsr)
rollcalls_pos$orgname <- as.character(rollcalls_pos$orgname)

combined_pos <- bind_rows(pos_all_edges, rollcalls_pos)

unanimous_votes <- combined_pos %>% group_by(BillID) %>% summarise(num_votes = n(), num_support = sum(disposition == "support"), num_oppose = sum(disposition == "oppose"))
unanimous_votes <- unanimous_votes %>% filter(num_votes == num_support | num_votes == num_oppose)
combined_pos <- combined_pos %>% filter(!(BillID %in% unanimous_votes$BillID))  %>% unique()


g_mat <- matrix(0,nrow = length(unique(combined_pos$orgname)), ncol = length(unique(combined_pos$BillID)), dimnames = list(unique(combined_pos$orgname), unique(combined_pos$BillID)))


for (i in 1:nrow(combined_pos)){
  row_org <- combined_pos$orgname[i]
  col_bill <- combined_pos$BillID[i]
  g_mat[row_org , col_bill] <- 1
}

library(igraph)
est_g <- graph_from_incidence_matrix(g_mat)

V(est_g)$core <- coreness(est_g)
core5 <- induced_subgraph(est_g,V(est_g)$core>4)
#extract ids of the bills and orgs to keep
core_names <- V(core5)$name


pos_all_edges_core <- as_tibble(combined_pos) %>% filter(BillID %in% core_names) %>% filter(orgname %in% core_names)

pos_all_core_dat <- select(pos_all_edges_core, BillID, orgname, disposition)



anchor_orgs <- c("10713" ,"14657","Sierra Club","Americans for Tax Reform")
non_anchors <- filter(pos_all_core_dat, !(orgname %in% anchor_orgs))

anchor_pos <- filter(pos_all_core_dat, orgname %in% anchor_orgs)

non_anchors$org_index <- as.numeric(as.factor(non_anchors$orgname)) + 4
non_anchors$bill_index <- as.numeric(as.factor(non_anchors$BillID))

anchor_pos$org_index[anchor_pos$orgname == "10713" ] <- 1
anchor_pos$org_index[anchor_pos$orgname == "Sierra Club"] <- 2
anchor_pos$org_index[anchor_pos$orgname == "14657"] <- 3
anchor_pos$org_index[anchor_pos$orgname == "Americans for Tax Reform"] <- 4

anchor_pos <- anchor_pos %>% left_join(select(non_anchors, BillID, bill_index)) %>% unique()

final_positions_edgelist <- bind_rows(select(anchor_pos, org_index, BillID, disposition), select(non_anchors, org_index, bill_index, disposition))
final_positions_edgelist$vote <- NA
final_positions_edgelist$vote[final_positions_edgelist$disposition == "support"] <- 1
final_positions_edgelist$vote[final_positions_edgelist$disposition == "oppose"] <- 0

final_positions_edgelist <- final_positions_edgelist %>% select(-disposition)

final_positions_edgelist <- final_positions_edgelist %>% left_join(sessions) 

session_edgelists <- split(final_positions_edgelist, as.factor(final_positions_edgelist$session))

org_nums <- max(final_positions_edgelist$org_index)

estimation_matrices <- vector("list", length = length(session_edgelists))
for (i in (1:length(session_edgelists))){
  edgelist <- session_edgelists[[i]]
  #make the voting matrix for estimation
  edgelist  <- edgelist  %>% mutate(bill_id_index = as.numeric(as.factor(as.character(BillID))))
  est_mat <- NA
  est_mat <- edgelist %>% dplyr::select(org_index,bill_id_index, vote)
  est_mat <- est_mat%>% filter(complete.cases(est_mat))
  est_mat <- as.matrix(est_mat)
  m1 <- matrix(NA, nrow = org_nums, ncol = length(unique(edgelist$bill_id_index)))
  m1[est_mat[,1:2] ]<- as.numeric(est_mat[,3])
  print(dim(m1))
  
  
  estimation_matrices[[i]] <- m1
}


