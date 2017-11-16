##
## Update by Zander 11.3.2017
#with no absetntions
#assigning conyers and PP (liberal) as 1 and 2 row index, Sensenbrenner and NRA as 3 and 4 row index (conservative)
#the final output of this file is a list of matrices for the years in the dataset, each matrix is 1 or -1 (support/oppose), with NAs for abstentions
#
#The data to run this is contained in a zipped file that I have also shared. You need to adjust the filepaths.
#there is a really tiny dataset from the 109th congres that can probably be excluded? This is the first item in the estimation_matrices list.
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


#summarise bills and orgs after the filtration
(org_poscounts_post <- pos_all_core %>% group_by(orgname) %>% summarise(num = n()) %>% arrange(desc(num)))

(bill_orgcounts_post <- pos_all_core %>% group_by(BillID) %>% summarise(num = n()) %>% arrange(desc(num)))

# #find how many bills of a given major topic code organizations took positions on
# bill_catcounts <- pos_all %>% group_by(orgname, Major) %>% summarise(bill_count = n()) %>% arrange(desc(bill_count))
# 
# #filter to only include orgs with more than one position on a bill
# bill_catcounts <- bill_catcounts %>% filter(bill_count > 1, !is.na(Major))

#get all posible bill org dyads
#pos_all_core_dat <- expand.grid(BillID=unique(pos_all_core$BillID), orgname = unique(pos_all_core$orgname))

#add the dispositions in from the 5-core set, to the list of all possible ones
pos_all_core_dat <- select(pos_all_core, BillID, orgname, disposition)
#pos_all_core_dat <- pos_all_core_dat %>% left_join(unique(select(pos_all_core, BillID, Major)))

#merge in the catcounts on for dyads where the org took more than one position on the major category
#pos_all_core_dat <- pos_all_core_dat %>% left_join(bill_catcounts)

library(tibble)
pos_all_core_dat <- as_tibble(pos_all_core_dat)
# 
# #assign absention to all dyads where there is not a currently recorded position of yes or no, but there is a bill count indicator
# pos_all_core_dat$disposition[is.na(pos_all_core_dat$disposition) & !is.na(pos_all_core_dat$bill_count)] <- "abstention"
# 
# #filter out all remaining, true NAs to get the edgelist
# pos_all_core_abst <- filter(pos_all_core_dat, !is.na(disposition))
# 
# pos_all_core_abst
# 

#pull out anchoring units for identification



#filter out earlier congresses
rollcalls <- filter(rollcalls, congress > 108)
votes <- filter(votes, congress > 108)

#create bill id that matches cbp and maplight
rollcalls$bill_type <- gsub("[0-9]", "", rollcalls$bill_number) 
rollcalls$bill_num <- gsub("[^0-9]", "", rollcalls$bill_number)
rollcalls$BillID <- paste(rollcalls$congress, rollcalls$bill_type, rollcalls$bill_num, sep ="-")
rollcalls <- dplyr::select(rollcalls, congress, chamber, rollnumber, BillID)

votes <- votes %>% left_join(rollcalls)

#follow previous procedure for assigning votes and assigning abstentions
votes <- votes %>% select(BillID, icpsr, cast_code, congress)
votes <-  filter(votes, BillID %in% core_names) 
votes$disposition[votes$cast_code == 1] <- "support"
votes$disposition[votes$cast_code == 6] <- "oppose"
#votes$disposition[votes$cast_code == 7 | votes$cast_code == 9] <- "abstention"

# bill_major <- pos_all %>% dplyr::select(BillID, Major) %>% unique()
# votes <- votes %>% left_join(bill_major)
# 
# MOC_catcounts <- votes %>% group_by(icpsr, Major) %>% summarise(bill_count = n()) %>% arrange(desc(bill_count))
# MOC_catcounts <- MOC_catcounts %>% filter(bill_count > 1, !is.na(Major))
# 
# 
# votes <- votes %>% left_join(MOC_catcounts)
# votes_abst <- filter(votes, !is.na(bill_count))

#bind the two together
votes <- votes %>% select(-cast_code) %>% rename(orgname=icpsr)
votes$orgname <- as.character(votes$orgname)


final_positions_edgelist <- bind_rows(pos_all_core_dat,votes)
final_positions_edgelist$congress[is.na(final_positions_edgelist$congress)] <- str_split(final_positions_edgelist$BillID, "-")[[1]][1]

#make the final vote variable
final_positions_edgelist$vote <- NA
final_positions_edgelist$vote[final_positions_edgelist$disposition == "support"] <- 1
final_positions_edgelist$vote[final_positions_edgelist$disposition == "oppose"] <- -1
# final_positions_edgelist$vote[final_positions_edgelist$disposition == "abstention"] <- 0 

#find bills that don't have unanimous position taking on them
unanimous_bills <- final_positions_edgelist %>% group_by(BillID) %>% summarise(num_positions = length(disposition), sum_positions = sum(vote))
unanimous_bills <- filter(unanimous_bills, num_positions == abs(sum_positions))

#keep only non-unaninimous bills
final_positions_edgelist <- final_positions_edgelist %>% filter(!(BillID %in% unanimous_bills$BillID))

anchor_orgs <- c("10713" ,"14657","Sierra Club","Americans for Tax Reform")

anchors <- filter(final_positions_edgelist, orgname %in% anchor_orgs)

final_positions_edgelist <- filter(final_positions_edgelist, !(orgname %in% anchor_orgs))


final_positions_edgelist  <- final_positions_edgelist  %>% mutate(org_index = as.numeric(as.factor(as.character(orgname))) +4)

#Give the right indices to the anchor sheet
anchors <- left_join(anchors, unique(select(final_positions_edgelist, BillID)))
anchors$org_index <- NA

#assigning conyers and PP (liberal) as 1 and 2 row index, Sensenbrenner and NRA as 3 and 4 row index (conservative)
anchors$org_index[anchors$orgname == "10713" ] <- 1
anchors$org_index[anchors$orgname == "Sierra Club"] <- 2
anchors$org_index[anchors$orgname == "14657"] <- 3
anchors$org_index[anchors$orgname == "Americans for Tax Reform"] <- 4

#add the anchors back in with their indices
final_positions_edgelist <- bind_rows(final_positions_edgelist, anchors)

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




#write_csv(final_positions_edgelist, "/Users/alexanderfurnas/Projects/Maplight_ideal_points/all_positions_withabstention_andCong.csv")



