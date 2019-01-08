# clear workspace
rm(list=ls())



# Load and prepare data ---------------------------------------------------

data("dat")
data("whichRev")

nDim <- 5 # number of content dimensions (Neuroticism, Extraversion, Openness, Agreeableness, Conscientiousness)
nDimRS <- 3 # number of response styles (ERS, MRS, ARS)

nItems <- ncol(dat) # number of items
nItemsDim <- nItems/nDim # number of items per content dimension

nCat <- table(dat$neuro1) %>% length
nThres <- nCat - 1

# control list to fit models
controlList <- list(snodes=1000,fac.oldxsi=0.4,max.increment=1.05)

temp <- dat



# Model fit ---------------------------------------------------------------

# Fit PCM
source("analysis/fit_models/fit_PCM.R")
save(PCM, file="analysis/fitted/PCM.Rda")
rm(PCM)


# Fit Random Threshold model (Wang et al., 2006)
source("analysis/fit_models/fit_wang2006.R")
save(wang2006, file = "analysis/fitted/wang2006.Rda")
rm(wang2006)


# Fit gen. Random Threshold Models (Wang & Wu, 2011)
source("analysis/fit_models/fit_wang2011.R")
save(wang2011, file = "analysis/fitted/wang2011.Rda")
rm(wang2011)


# Fit multidimensional NRM (Bolt & Johnson, 2009)
source("analysis/fit_models/fit_bolt2009.R")
save(bolt2009, file="analysis/fitted/bolt2009.Rda")
rm(bolt2009)


# Fit multidimensional NRM (Bolt et al., 2014)
source("analysis/fit_models/fit_bolt2014.R")
save(bolt2014, file = "analysis/fitted/bolt2014.Rda")
rm(bolt2014)


# Fit multidimensional PCM (Bolt & Newton, 2011; Wetzel & Carstensen, 2017)
source("analysis/fit_models/fit_wetzel2017.R")
save(wetzel2017, file="analysis/fitted/wetzel2017.Rda")
rm(wetzel2017)


# Fit gen. multidimensional PCM
source("analysis/fit_models/fit_falk2016.R")
save(falk2016, file = "analysis/fitted/falk2016.Rda")
rm(falk2016)


# Fit flexible ARS model
source("analysis/fit_models/fit_flexibleARS.R")
save(flexibleARS, file = "analysis/fitted/flexibleARS.Rda")
rm(flexibleARS)


# Fit constrained gen. multidimensional PCM
source("analysis/fit_models/fit_constrainedLoadings.R")
save(constrainedLoadings, file = "analysis/fitted/constrainedLoadings.Rda")
rm(constrainedLoadings)


# Fit constrained gen. multidimensional PCM Version 2 (only one discrimination parameter per RS dimension)
source("analysis/fit_models/fit_constrainedLoadingsRandom.R")
save(constrainedLoadingsRandom, file = "analysis/fitted/constrainedLoadingsRandom.Rda")
rm(constrainedLoadingsRandom)


save(temp, whichRev, file = "temp.rda")
q()


