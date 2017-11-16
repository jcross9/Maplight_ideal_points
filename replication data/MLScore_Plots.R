rm(list=ls())
library(dplyr)
library(readr)
library(tidyr)
library(dplyr)
crp <- read_csv("/Users/alexanderfurnas/2015-16Coursework/CRP_Categories.csv")
nominate <- read_csv("/Users/alexanderfurnas/Projects/Maplight_ideal_points/replication data/HSall_members.csv")
nominate <- dplyr::select(nominate, icpsr, dim1, party_code, congress, chamber) %>% filter(congress >108)
nominate$icpsr <- as.character(nominate$icpsr)
IGs <- read_csv("/Users/alexanderfurnas/Projects/Maplight_ideal_points/replication data/dynamicscores_IG_MC_11-6.csv")
IGs$org_index <- as.character(IGs$org_index)
IGscores <- read_csv("/Users/alexanderfurnas/Projects/Maplight_ideal_points/replication data/dynamic_scores_MCMCpack.csv")
IGscores <- IGscores[1:16296,]

IGscores <- separate(IGscores, X1, c("theta", "org_index", "time"),sep="[.]") %>% dplyr::select(-theta)
IGscores$org_index <- as.character(IGscores$org_index)
IGscores <- left_join(IGscores, dplyr::select(IGs, org_index, orgname))
names(IGscores) <- c("org_index",  "congress", "MLscore", "orgname")
IGscores$MLscore <-IGscores$MLscore*-1

IGscores$congress[IGscores$congress == "t1"] <- 109
IGscores$congress[IGscores$congress == "t2"] <- 110
IGscores$congress[IGscores$congress == "t3"] <- 111
IGscores$congress[IGscores$congress == "t4"] <- 112
IGscores$congress[IGscores$congress == "t5"] <- 113
IGscores$congress[IGscores$congress == "t6"] <- 114

IGscores$congress <- as.numeric(IGscores$congress)

nominate <- left_join(nominate, dplyr::select(IGscores, orgname, MLscore, congress), by = c("icpsr" = "orgname", "congress"="congress"))

cong_14scat <- ggplot(filter(nominate, congress==114), aes(x=dim1, y=MLscore, color = as.factor(party_code))) +geom_point() +geom_smooth(method="lm")+ theme_classic() + scale_color_manual(values = c("#05426C", "#712422","#708259")) +ggtitle("MLscores vs. DW-Nominate 1st dimension for 114th Congress") +xlab("DW-Nominate dim1") 
#ggsave("cong_14scat.pdf", cong_14scat, device="pdf")

cor.test(nominate$dim1, nominate$MLscore)

cor.test(nominate$dim1[nominate$party_code==100], nominate$MLscore[nominate$party_code==100])

cor.test(nominate$dim1[nominate$party_code==200], nominate$MLscore[nominate$party_code==200])


party_medians <- group_by(nominate, congress, party_code, chamber) %>% summarise(med_MLscore = median(MLscore, na.rm=T), ML_25th = quantile(MLscore, .25, na.rm=T), ML_75th = quantile(MLscore, .75, na.rm=T)) 



party_meds <- ggplot(filter(party_medians, party_code!=328), aes(x=congress, y=med_MLscore, ymin=ML_25th, ymax=ML_75th, color=as.factor(party_code))) +geom_point(size=4) +geom_line() +geom_errorbar(width=0, size=4, alpha=.4)+theme_classic() + scale_color_manual(values = c("#05426C", "#712422","#708259")) +ggtitle("Dynamic MLscore party medians for 109th-114th Congresses") +ylab("MLScore") +facet_wrap(~chamber)+ theme(legend.position="none")
#ggsave("party_meds.pdf", party_meds, device="pdf")


pos114 <- read_csv("114positions.csv")
pos114 <- pos114 %>% dplyr::select(-X1)
pos_all <- read_csv("all_positions.csv")
#read in roll call votes
#read in the congressional bill project to get topic codes
cbp <- read_csv("bills93-114 2.csv")

group_types <- bind_rows(pos_all, pos114) %>% filter(orgname %in% unique(IGscores$orgname)) %>% dplyr::select(orgname, grouptype) %>% mutate(grouptype = toupper(grouptype)) %>% filter(!is.na(grouptype)) %>% unique()

group_types <- group_types %>% left_join(crp, by=c("grouptype" = "Catcode"))

grouptype_counts <- group_by(group_types, Catname) %>% summarise(num_groups = length(unique(orgname))) %>% arrange(desc(num_groups))
three_groups <- unique(filter(grouptype_counts, num_groups >2)$Catname)

IGscore_groups <- left_join(IGscores, group_types)

IGscore_groups_catname <- IGscore_groups %>% filter(Catname %in% three_groups)

IGscores_sector <- filter(IGscore_groups, Sector != "Ideology/Single-Issue" & Sector != "Party Cmte" & Sector != "Unknown" & Sector != "Other" & Sector != "Non-contribution") %>% dplyr::select(orgname,MLscore, Sector, congress) %>% unique()

sec_dens <- ggplot(IGscores_sector, aes(x=MLscore, y=..scaled..)) +geom_density(fill="#E3BA22") +facet_wrap(~Sector) +theme_classic() +ylab("Scaled Density") +ggtitle("Distributions of 114th Cong MLscores for single issue groups by type")
#ggsave("sec_dens.pdf", sec_dens, device="pdf")

Sector_medians <- group_by(IGscores_sector, congress, Sector) %>% summarise(med_MLscore = median(MLscore, na.rm=T), ML_25th = quantile(MLscore, .25, na.rm=T), ML_75th = quantile(MLscore, .75, na.rm=T))

Sector_medians <- ungroup(Sector_medians)

sector_med <- ggplot(Sector_medians, aes(x=congress, y=med_MLscore, ymin=ML_25th, ymax=ML_75th)) +geom_point(size=4,color="#E3BA22") +
  geom_line(color="#E3BA22") +geom_errorbar(width=0, size=4, alpha=.4, color="#E3BA22")+theme_classic()  +facet_wrap(~Sector)

#ggsave("sector_med.pdf", sector_med, device="pdf")



IGscores_singleissue <- filter(IGscore_groups_catname, Sector == "Ideology/Single-Issue" & congress==114) %>% dplyr::select(orgname,MLscore, Catname) %>% unique()
IGscores_Other <- filter(IGscore_groups_catname, Sector == "Other" & congress==114) %>% dplyr::select(orgname,MLscore, Catname) %>% unique()
IGscores_Labor<- filter(IGscore_groups_catname, Sector == "Labor" & congress==114) %>% dplyr::select(orgname,MLscore, Catname) %>% unique()
IGscores_Health <- filter(IGscore_groups_catname, Sector == "Health" & congress==114) %>% dplyr::select(orgname,MLscore, Catname) %>% unique()



si_dens <- ggplot(IGscores_singleissue, aes(x=MLscore, y=..scaled..)) +geom_density(fill="#E3BA22") +facet_wrap(~Catname) +theme_classic() +ylab("Scaled Density") +ggtitle("Distributions of 114th Cong MLscores for single issue groups by type")
#ggsave("si_dens.pdf", si_dens, device="pdf")

other_dens <- ggplot(IGscores_Other, aes(x=MLscore, y=..scaled..)) +geom_density(fill="#E3BA22") +facet_wrap(~Catname) +theme_classic() +ylab("Scaled Density") +ggtitle("Distributions of 114th Cong MLscores for other groups by type")
#ggsave("other_dens.pdf", other_dens, device="pdf")

labor_dens <- ggplot(IGscores_Labor, aes(x=MLscore, y=..scaled..)) +geom_density(fill="#E3BA22") +facet_wrap(~Catname) +theme_classic() +ylab("Scaled Density") +ggtitle("Distributions of 114th Cong MLscores for labor groups by type")
#ggsave("labor_dens.pdf", labor_dens, device="pdf")

health_dens <- ggplot(IGscores_Health, aes(x=MLscore, y=..scaled..)) +geom_density(fill="#E3BA22") +facet_wrap(~Catname) +theme_classic() +ylab("Scaled Density") +ggtitle("Distributions of 114th Cong MLscores for health groups by type")
#ggsave("health_dens.pdf", health_dens, device="pdf")



business_test <- filter(IGscore_groups, orgname ==  "National Federation of Independent Business" |orgname ==  "U.S. Chamber of Commerce") %>% dplyr::select(congress, orgname, MLscore) %>% unique()
gun_test <- filter(IGscore_groups, orgname ==  "National Rifle Association" |orgname ==  "Gun Owners of America") %>% dplyr::select(congress, orgname, MLscore) %>% unique()



doctors_test <- filter(IGscore_groups, orgname ==  "American College of Physicians" |orgname ==  "Association of American Physicians and Surgeons") %>% dplyr::select(congress, orgname, MLscore) %>% unique()


lawyers_test <- filter(IGscore_groups, orgname ==  "National Lawyers Guild" |orgname ==  "National District Attorneys Association") %>% dplyr::select(congress, orgname, MLscore) %>% unique()


enviro_test <- filter(IGscore_groups, orgname ==  "Greenpeace" |orgname ==  "Natural Resources Defense Council") %>% dplyr::select(congress, orgname, MLscore) %>% unique()


labor_test <- filter(IGscore_groups, orgname ==  "International Brotherhood of Teamsters" |orgname ==  "American Federation of State, County and Municipal Employees") %>% dplyr::select(congress, orgname, MLscore) %>% unique()

business_test$type <- "Business Association"
gun_test$type <- "Gun Rights"
doctors_test$type <- "Medical Association"
lawyers_test$type <- "Lawyers Association"
enviro_test$type <- "Environmental Organization"
labor_test$type <- "Labor Union"


left_predicts <-c("Greenpeace", "National Lawyers Guild", "American College of Physicians", "National Rifle Association", "U.S. Chamber of Commerce", "American Federation of State, County and Municipal Employees")
test_orgs <- bind_rows(business_test, gun_test, doctors_test, lawyers_test, enviro_test, labor_test)
test_orgs$predict_left <- 0
test_orgs$predict_left[test_orgs$orgname %in% left_predicts] <- 1

testorg_plot <- ggplot(test_orgs, aes(x=congress, y=MLscore, group=orgname, color=as.factor(predict_left))) +geom_point() +geom_line() +theme_classic() +facet_wrap(~type) + scale_color_manual(values = c("#137B80", "#8E6C8A")) + theme(legend.position="none") +xlab("Congress") + ggtitle("Comparisons of dplyr::select within sector groups")
ggsave("testorg_plot.pdf", testorg_plot, device="pdf")

IGscores$legislator <- "Interest Group"
IGscores$legislator[IGscores$orgname %in% nominate$icpsr] <- "Member of Congress" 

IGvMOV_dens <- ggplot(filter(IGscores, congress==114), aes(x=MLscore, fill=as.factor(legislator))) +geom_density(alpha=.5, color=NA) +theme_classic() + scale_fill_manual(values = c("#E3BA22", "#684664")) 
#ggsave("IGvMOV_dens.pdf", IGvMOV_dens, device="pdf")
