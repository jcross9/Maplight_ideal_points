nominate <- read_csv("/Users/alexanderfurnas/Downloads/HS114_members.csv")
nominate <- select(nominate, icpsr, dim1, party_code)
nominate$icpsr <- as.character(nominate$icpsr)
IGscores <- read_csv("/Users/alexanderfurnas/Downloads/dynamicscores_IG_MC_11-6.csv")

nominate <- left_join(nominate, select(IGscores, orgname, t6_109_114), by = c("icpsr" = "orgname"))

ggplot(nominate, aes(x=dim1, y=t6_109_114, color = as.factor(party_code))) +geom_point() + theme_classic() + scale_color_manual(values = c("#05426C", "#712422","#708259"))

