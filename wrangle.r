# IODS final assignment
# Henna Kettunen

setwd("L:\\Users\\Henna\\storage\\r\\iods\\IODS-final\\IODS-final")

library(tidyr)
library(dplyr)

# Tropical tree leaf data from BRIDGE database obtained via TRY
trees <- read.csv("trees.csv")
str(trees)

# Combine genus and species to a same column
trees$name <- paste0(trees$Genus, "_", trees$species)

# How many families? 59
famn <- summarise(trees, nfam=n_distinct(Family))

# How many genera? 222
genn <- summarise(trees, nfam=n_distinct(Genus))

# How many species? 646
specn <- summarise(trees, nfam=n_distinct(name))

# How many genera per family?
ngen <- trees %>% group_by(Family) %>% summarise(ngen=n_distinct(Genus))

# How many species per genus?
nspec <- trees %>% group_by(Genus) %>% summarise(nspec=n_distinct(species))

# How many observations per species?
nobs <- count(trees[complete.cases(trees)==T,], name)
nobs <- arrange(nobs, desc(n))
nobs[1:10,]

# How many observations per genus?
nobsg <- trees[complete.cases(trees),] %>% group_by(Genus) %>% summarise(nobs=n())
nobsg <- arrange(nobsg, desc(nobs))

# Genera with >= 50 observations
gen50 <- nobsg[which(nobsg$nobs>=50),]
gen50 <- gen50[gen50$Genus!="IND",]

# Which families do these belong to?
fam50 <- unique(trees[trees$Genus %in% gen50$Genus, c("Family","Genus")])
fam50$n.obs <- gen50$nobs[match(fam50$Genus, gen50$Genus)]

# 6 genera with >=50 complete cases belonging to 4 different families selected for further analyses
dat <- trees[complete.cases(trees) & trees$Genus %in% gen50$Genus,]
specn2 <- length(unique(dat$name))

# Number of species per genus
nspec2 <- dat %>% group_by(Genus) %>% summarise(nspec=n_distinct(species))
fam50$n.spec <- nspec2$nspec[match(fam50$Genus, nspec2$Genus)]

# Select columns
keep <- c("Genus","DBH","leaf_thickness","leaf_toughness","sapwood_density","N","C_N","N15","C13","chlorophyll_concentration","surface_area","SLA")
dat <- dat[,keep]

# # Standardize the data
# dat2 <- dat
# dat[,2:ncol(dat)] <- scale(dat[,2:ncol(dat)])
# summary(dat)

dat$Genus <- droplevels(dat$Genus)
save(dat, file="tree_data.RData")


## Should the analysis be done on family-level after all
# dat3 <- trees[complete.cases(trees) & trees$Genus %in% gen50$Genus,]
# keep3 <- c("Family","DBH","leaf_thickness","leaf_toughness","sapwood_density","N","C_N","N15","C13","chlorophyll_concentration","surface_area","SLA")
# dat3 <- dat3[,keep3]
# dat3$Family <- droplevels(dat3$Family)
# pairs(dat3, cex=0.1, pch=16, col=dat3$Family)

