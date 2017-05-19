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

pos114 <- pos114 %>% mutate(position_date = lubridate::ymd(stringr::str_split(citation, "[()]")[[1]][2]))

# Create bill id
pos114 <- pos114 %>% mutate(bill_id = paste(session, prefix, number, sep = "_"))


# Create index for bill and organization to be used for slotting into the matrix -- this will also crosswalk back to orgname
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

# with NO imputation of missing data

fit.ideal <- ideal(rc, 
     maxiter = 10000, thin = 100, burnin = 5000,
     impute = FALSE,
     normalize = TRUE,
     priors = NULL, startvals = "eigen",
     store.item = FALSE, file = NULL,
     verbose=FALSE)

hist(fit.ideal$xbar) # looks pretty dadgum good
dens <- density(fit.ideal$xbar)
plot(dens)
hist(fit.ideal$betabar) # looks pretty dadgum good
dens <- density(fit.ideal$xbar)
plot(dens)



# WITH imputation of missing data
