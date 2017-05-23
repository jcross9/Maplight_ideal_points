####################################################################################################
########################### MAPLIGHT IDEAL POINT SCORES PROJECT SCRIPT #############################
####################################################################################################


############# PROJECT PROGRESS ##################

## Looked at US repo to see if there are action timestamp. Link: 
## Need to fix the position_date column; all one date at present.
## We have prelimiary estimates. They look totally bogus. 
## Almost no variance around 0. 
##The vote matrix density is 
## > (38054+11741)/25893984
## [1] 0.001923034
## which is super sparse so that could be the problem. I think em is shittier at sparsity than MCMC.
## We might want to look into just using the jackman package -- it takes the same rollcall object



############# DATA MANIPULATION AND CREATION #####################

## Creation of Postition Date Column

library(readr)
library(dplyr)
library(lubridate)
library(emIRT)
library(pscl)



pos114 <- read_csv("/Users/alexanderfurnas/Projects/Maplight_ideal_points/114positions.csv")
pos114 <- read_csv("C:/Users/jcros/Documents/Maplight_ideal_points/114positions.csv")

pos114 <- pos114 %>% mutate(position_date = lubridate::ymd(sapply(stringr::str_split(pos114$citation, "[()]"),function(x) x[2])))

# Create bill id
pos114 <- pos114 %>% mutate(bill_id = paste(session, prefix, number, sep = "_"))

# dichotomize disposition as numeric for vote matrix
pos114$disposition.dichot <- NA
pos114$disposition.dichot[pos114$disposition == "oppose"] <- -1
pos114$disposition.dichot[pos114$disposition == "support"] <- 1

#summarise bills and orgs prior to filtration
org_poscounts_pre <- pos114 %>% group_by(orgname) %>% summarise(num = n()) %>% arrange(desc(num))
bill_orgcounts_pre <- pos114 %>% group_by(bill_id) %>% summarise(num = n()) %>% arrange(desc(num))

#5-core filter a graph of the connections. 
#construct the edgelist
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

#extract ids of the bills and orgs to keep
core_names <- V(core5)$name

#filter bills and orgs to the 5 core
pos114_core <- filter(pos114, bill_id %in% core_names & orgname %in% core_names)

#summarise bills and orgs after the filtration
org_poscounts_post <- pos114_core %>% group_by(orgname) %>% summarise(num = n()) %>% arrange(desc(num))

bill_orgcounts_post <- pos114_core %>% group_by(bill_id) %>% summarise(num = n()) %>% arrange(desc(num))

# Create index for bill and organization to be used for slotting into the matrix -- this will also crosswalk back to orgname
#this must be done after filtration so that the matrix dimensions and indices line up.
pos114_core  <- pos114_core  %>% mutate(bill_id_index = as.numeric(as.factor(as.character(bill_id))))

pos114_core  <- pos114_core  %>% mutate(org_index = as.numeric(as.factor(as.character(orgname))))

#fill vote matrix
est_mat <- NA
est_mat <- pos114_core %>% dplyr::select(org_index,bill_id_index, disposition.dichot)
est_mat <- est_mat%>% filter(complete.cases(est_mat))
est_mat <- as.matrix(est_mat)
m1 <- matrix(0, nrow = length(unique(pos114_core$org_index)), ncol = length(unique(pos114_core$bill_id_index)))
m1[est_mat[,1:2] ]<- as.numeric(est_mat[,3])
print(dim(m1))


# create roll call object that the estimation process takes
rc<-rollcall(m1,yea=1,nay=-1,missing=0)
rm(m1)
gc()
p <- makePriors(rc$n, rc$m, 1)
s <- getStarts(rc$n, rc$m, 1)



############# DATA ANALYSIS #################


### emIRT 

# estimate
fit <- binIRT(.rc=rc,
              .priors=p,
              .starts=s,
              .control= {list(threads=3, verbose=TRUE, checkfreq=1)},
)

# plot results
hist(fit$means$x) #looks bogus af
hist(fit$means$beta) #looks slightly less bogus?
d <- density(fit$means$beta)
plot(d) # how is this being identified? is the right-side tail indicating a conservative bent? If so, kinda good?


### ideal (from the Jackman Package)

## with NO imputation of missing data

# generate estimates
fit.ideal <- ideal(rc, 
     maxiter = 10000, thin = 100, burnin = 5000,
     impute = FALSE,
     normalize = TRUE,
     priors = NULL, startvals = "eigen",
     store.item = TRUE, file = NULL,
     verbose=TRUE)

# plot results
hist(fit.ideal$xbar) # looks pretty dadgum good
dens <- density(fit.ideal$xbar)
plot(dens)

hist(fit.ideal$betabar[,2]) # also a little less weird
dens <- density(fit.ideal$betabar[,2]) # perhaps less variance than we'd like
plot(dens)


## WITH imputation of missing data

# generate estimates
fit.ideal2 <- ideal(rc, 
                   maxiter = 10000, thin = 100, burnin = 5000,
                   impute = TRUE,
                   normalize = TRUE,
                   priors = NULL, startvals = "eigen",
                   store.item = TRUE, file = NULL,
                   verbose=TRUE)

# plot results
hist(fit.ideal2$xbar) # unsurprisingly a bit smoother
dens <- density(fit.ideal2$xbar)
plot(dens)

hist(fit.ideal2$betabar[,2]) # also a little less weird
dens <- density(fit.ideal2$betabar[,2]) # perhaps less variance than we'd like
plot(dens)

#write.csv(fit.ideal$xbar, "initial_group_scores.csv")
#write.csv(fit.ideal$betabar, "intial_bill_scores.csv")
#write.csv(fit.ideal$xbar, "initial_group_scores_trunc_noimpute.csv")
#write.csv(fit.ideal$betabar, "intial_bill_scores_trunc_noimpute.csv")
#write.csv(fit.ideal2$xbar, "initial_group_scores_trunc_impute.csv")
#write.csv(fit.ideal2$betabar, "intial_bill_scores_trunc_impute.csv")

scatter.smooth(fit.ideal$xbar, fit.ideal2$xbar)
cor(fit.ideal$xbar, fit.ideal2$xbar)

scatter.smooth(fit.ideal$betabar[,2], fit.ideal2$betabar[,2])
cor(fit.ideal$betabar[,2], fit.ideal2$betabar[,2])



#### Merge Scores back to Data ####

#Groups
groups <- subset(pos114_core, !duplicated(X1)) #Is X1 the appropriate column?
groups <- data.frame(cbind(groups$X1,groups$orgname))
names(groups) <- c("X1","orgame")

no_impute <- read.csv("initial_group_scores_trunc_noimpute.csv")
no_impute$index <- c(1:nrow(no_impute))
imputed <- read.csv("initial_group_scores_trunc_impute.csv")
imputed$index <- c(1:nrow(imputed))
scores <- merge(no_impute, imputed, by = "index")

group_scores <- merge(scores, groups, by.x = "index", by.y = "X1")
names(group_scores) <- c("index", "Legislator.x", "non_impute_score", "Legislator.y","imputed_score", "orgname")

#write.csv(group_scores, "merged_group_scores_0522.csv")


