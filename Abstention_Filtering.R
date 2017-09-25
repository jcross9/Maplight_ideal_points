library(readr)
library(dplyr)
library(lubridate)

pos114 <- read_csv("/Users/alexanderfurnas/Projects/Maplight_ideal_points/114positions.csv")
pos114 <- pos114 %>% select(-X1)
pos_all <- read_csv("/Users/alexanderfurnas/Projects/Maplight_ideal_points/all_positions.csv")

pos_all <- bind_rows(pos_all, pos114)

dim(pos_all)

cbp <- read_csv("/Users/alexanderfurnas/Projects/Maplight_ideal_points/bills93-114 2.csv")

unique(cbp$BillType)
unique(pos_all$prefix)

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
org_poscounts_post <- pos_all_core %>% group_by(orgname) %>% summarise(num = n()) %>% arrange(desc(num))

bill_orgcounts_post <- pos_all_core %>% group_by(BillID) %>% summarise(num = n()) %>% arrange(desc(num))

bill_catcounts <- pos_all %>% group_by(orgname, Major) %>% summarise(bill_count = n()) %>% arrange(desc(bill_count))
bill_catcounts <- bill_catcounts %>% filter(bill_count > 1, !is.na(Major))

pos_all_core_dat <- expand.grid(BillID=unique(pos_all_core$BillID), orgname = unique(pos_all_core$orgname))

pos_all_core_dat <- pos_all_core_dat %>% left_join(select(pos_all_core, BillID, orgname, disposition))
pos_all_core_dat <- pos_all_core_dat %>% left_join(unique(select(pos_all_core, BillID, Major)))

pos_all_core_dat <- pos_all_core_dat %>% left_join(bill_catcounts)

library(tibble)
pos_all_core_dat <- as_tibble(pos_all_core_dat)

pos_all_core_dat$disposition[is.na(pos_all_core_dat$disposition) & !is.na(pos_all_core_dat$bill_count)] <- "abstention"

pos_all_core_abst <- filter(pos_all_core_dat, !is.na(disposition))

pos_all_core_abst

rollcalls <- read_csv("/Users/alexanderfurnas/Projects/Maplight_ideal_points/Bill_Data/HSall_rollcalls.csv")

votes <- read_csv("/Users/alexanderfurnas/Projects/Maplight_ideal_points/Bill_Data/HSall_votes.csv")

rollcalls <- filter(rollcalls, congress > 108)
votes <- filter(votes, congress > 108)

rollcalls$bill_type <- gsub("[0-9]", "", rollcalls$bill_number) 
rollcalls$bill_num <- gsub("[^0-9]", "", rollcalls$bill_number)
rollcalls$BillID <- paste(rollcalls$congress, rollcalls$bill_type, rollcalls$bill_num, sep ="-")
rollcalls <- dplyr::select(rollcalls, congress, chamber, rollnumber, BillID)

votes <- votes %>% left_join(rollcalls)

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

votes_abst <- votes_abst %>% select(-cast_code) %>% rename(orgname=icpsr)
votes_abst$orgname <- as.character(votes_abst$orgname)

final_positions_edgelist <- bind_rows(pos_all_core_abst,votes_abst) 


write_csv(final_positions_edgelist, "/Users/alexanderfurnas/Projects/Maplight_ideal_points/all_positions_withabstention_andCong.csv")



