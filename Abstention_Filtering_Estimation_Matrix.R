##
## Update by Zander 10.19.2017
library(readr)
library(dplyr)
library(lubridate)

#read in all of the bill positions
pos114 <- read_csv("/Users/alexanderfurnas/Projects/Maplight_ideal_points/114positions.csv")
pos114 <- pos114 %>% select(-X1)
pos_all <- read_csv("/Users/alexanderfurnas/Projects/Maplight_ideal_points/all_positions.csv")

pos_all <- bind_rows(pos_all, pos114)

dim(pos_all)


#read in the congressional bill project to get topic codes
cbp <- read_csv("/Users/alexanderfurnas/Projects/Maplight_ideal_points/bills93-114 2.csv")

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

#find how many bills of a given major topic code organizations took positions on
bill_catcounts <- pos_all %>% group_by(orgname, Major) %>% summarise(bill_count = n()) %>% arrange(desc(bill_count))

#filter to only include orgs with more than one position on a bill
bill_catcounts <- bill_catcounts %>% filter(bill_count > 1, !is.na(Major))

#get all posible bill org dyads
pos_all_core_dat <- expand.grid(BillID=unique(pos_all_core$BillID), orgname = unique(pos_all_core$orgname))

#add the dispositions in from the 5-core set, to the list of all possible ones
pos_all_core_dat <- pos_all_core_dat %>% left_join(select(pos_all_core, BillID, orgname, disposition))
pos_all_core_dat <- pos_all_core_dat %>% left_join(unique(select(pos_all_core, BillID, Major)))

#merge in the catcounts on for dyads where the org took more than one position on the major category
pos_all_core_dat <- pos_all_core_dat %>% left_join(bill_catcounts)

library(tibble)
pos_all_core_dat <- as_tibble(pos_all_core_dat)

#assign absention to all dyads where there is not a currently recorded position of yes or no, but there is a bill count indicator
pos_all_core_dat$disposition[is.na(pos_all_core_dat$disposition) & !is.na(pos_all_core_dat$bill_count)] <- "abstention"

#filter out all remaining, true NAs to get the edgelist
pos_all_core_abst <- filter(pos_all_core_dat, !is.na(disposition))

pos_all_core_abst


#read in roll call votes
rollcalls <- read_csv("/Users/alexanderfurnas/Projects/Maplight_ideal_points/Bill_Data/HSall_rollcalls.csv")
votes <- read_csv("/Users/alexanderfurnas/Projects/Maplight_ideal_points/Bill_Data/HSall_votes.csv")

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
votes <- votes %>% select(BillID, icpsr, cast_code)
votes <-  filter(votes, BillID %in% core_names) 
votes$disposition[votes$cast_code == 1] <- "support"
votes$disposition[votes$cast_code == 6] <- "oppose"
votes$disposition[votes$cast_code == 7 | votes$cast_code == 9] <- "abstention"

bill_major <- pos_all %>% dplyr::select(BillID, Major) %>% unique()
votes <- votes %>% left_join(bill_major)

MOC_catcounts <- votes %>% group_by(icpsr, Major) %>% summarise(bill_count = n()) %>% arrange(desc(bill_count))
MOC_catcounts <- MOC_catcounts %>% filter(bill_count > 1, !is.na(Major))


votes <- votes %>% left_join(MOC_catcounts)
votes_abst <- filter(votes, !is.na(bill_count))

#bind the two together
votes_abst <- votes_abst %>% select(-cast_code) %>% rename(orgname=icpsr)
votes_abst$orgname <- as.character(votes_abst$orgname)

final_positions_edgelist <- bind_rows(pos_all_core_abst,votes_abst) 

#make the final vote variable
final_positions_edgelist$vote <- NA
final_positions_edgelist$vote[final_positions_edgelist$disposition == "support"] <- 1
final_positions_edgelist$vote[final_positions_edgelist$disposition == "oppose"] <- -1
final_positions_edgelist$vote[final_positions_edgelist$disposition == "abstention"] <- 0 

#find bills that don't have unanimous position taking on them
unanimous_bills <- final_positions_edgelist %>% group_by(BillID) %>% summarise(num_positions = length(disposition), sum_positions = sum(vote))
unanimous_bills <- filter(unanimous_bills, num_positions == abs(sum_positions))

#keep only non-unaninimous bills
final_positions_edgelist <- final_positions_edgelist %>% filter(!(BillID %in% unanimous_bills$BillID))

final_positions_edgelist  <- final_positions_edgelist  %>% mutate(bill_id_index = as.numeric(as.factor(as.character(BillID))))

final_positions_edgelist  <- final_positions_edgelist  %>% mutate(org_index = as.numeric(as.factor(as.character(orgname))))

#make the voting matrix for estimation
est_mat <- NA
est_mat <- final_positions_edgelist %>% dplyr::select(org_index,bill_id_index, vote)
est_mat <- est_mat%>% filter(complete.cases(est_mat))
est_mat <- as.matrix(est_mat)
m1 <- matrix(NA, nrow = length(unique(final_positions_edgelist$org_index)), ncol = length(unique(final_positions_edgelist$bill_id_index)))
m1[est_mat[,1:2] ]<- as.numeric(est_mat[,3])
print(dim(m1))


#write_csv(final_positions_edgelist, "/Users/alexanderfurnas/Projects/Maplight_ideal_points/all_positions_withabstention_andCong.csv")



