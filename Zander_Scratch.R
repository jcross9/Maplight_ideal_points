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
core3 <- induced_subgraph(est_g,V(est_g)$core>2)
#extract ids of the bills and orgs to keep
core_names3 <- V(core3)$name

#filter bills and orgs to the 5 core
pos_all_core3 <- filter(pos_all, BillID %in% core_names3 & orgname %in% core_names3)

#summarise bills and orgs after the filtration
org_poscounts_post <- pos_all_core3 %>% group_by(orgname) %>% summarise(num = n()) %>% arrange(desc(num))

bill_orgcounts_post <- pos_all_core3 %>% group_by(BillID) %>% summarise(num = n()) %>% arrange(desc(num))

bill_catcounts <- pos_all %>% group_by(orgname, session, Major) %>% summarise(bill_count = n()) %>% arrange(desc(bill_count))











pos114 <- pos114 %>% mutate(position_date = lubridate::ymd(sapply(stringr::str_split(pos114$citation, "[()]"),function(x) x[2])))
pos114 <- pos114 %>% mutate(bill_id = paste(session, prefix, number, sep = "_"))

pos114 <- pos114 %>% mutate(bill_id_index = as.numeric(as.factor(as.character(bill_id))))

pos114 <- pos114 %>% mutate(org_index = as.numeric(as.factor(as.character(orgname))))

pos114$disposition.dichot <- NA
pos114$disposition.dichot[pos114$disposition == "oppose"] <- - 1
pos114$disposition.dichot[pos114$disposition == "support"] <- 1


bills <- group_by(pos114, bill_id) %>% summarise(min_date = min(position_date))

pos114 <- left_join(pos114, bills)

pos114 <- mutate(pos114, time_to_pos = position_date - min_date)

library(ggplot2)
ggplot(pos114, aes(x=time_to_pos, group = disposition, color=disposition)) + geom_density()

org_poscounts <- pos114 %>% group_by(orgname) %>% summarise(num = n()) %>% filter( num>4) %>% arrange(desc(num))

bill_orgcounts <- pos114 %>% group_by(bill_id) %>% summarise(num = n()) %>% filter( num>4) %>% arrange(desc(num))

length(unique(pos114$bill_id))
summary(pos114)

pos114 <- pos114 %>% mutate(bill_id_index = as.numeric(as.factor(as.character(bill_id))))

pos114 <- pos114 %>% mutate(org_index = as.numeric(as.factor(as.character(orgname))))

# dichotomize disposition as numeric for vote matrix
pos114$disposition.dichot <- NA
pos114$disposition.dichot[pos114$disposition == "oppose"] <- -1
pos114$disposition.dichot[pos114$disposition == "support"] <- 1

#fill vote matrix
est_mat <- NA
m1 <- matrix(0, nrow = length(unique(pos114$org_index)), ncol = length(unique(pos114$bill_id_index)))
est_mat <- pos114 %>% dplyr::select(16,15,17)
est_mat <- est_mat%>% filter(complete.cases(est_mat))
est_mat <- as.matrix(est_mat)
m1[est_mat[,1:2] ]<- as.numeric(est_mat[,3])
print(dim(m1))


bill_orgcounts <- pos114 %>% group_by(bill_id) %>% summarise(num = n(), posit = sum(disposition.dichot), pos_share = posit/num) %>% arrange(desc(num))



edge_mat <- NA
edge_mat <- pos114 %>% dplyr::select(orgname,bill_id)
edge_mat <- edge_mat%>% filter(complete.cases(edge_mat))
edge_mat <- as.matrix(edge_mat)

#kcore filtration
library(igraph)
est_g <- graph_from_edgelist(edge_mat, directed = FALSE)
core <- coreness(est_g)
V(est_g)$core <- coreness(est_g)
core5 <- induced_subgraph(est_g,V(est_g)$core>4)

core_names <- V(core5)$name

pos114_core <- filter(pos114, bill_id %in% core_names & orgname %in% core_names)

org_poscounts <- pos114_core %>% group_by(orgname) %>% summarise(num = n()) %>% arrange(desc(num))

bill_orgcounts <- pos114_core %>% group_by(bill_id) %>% summarise(num = n()) %>% arrange(desc(num))

