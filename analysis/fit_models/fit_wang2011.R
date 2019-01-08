
# Fit Random Threshold Models (Wang & Wu, 2011) ----------------------------

# This is not the model by Wang & Wu, but an extension.
# Wang & Wu assumed that discrimination is the same across all latent dimensions,
# that is the trait and the k-1 random thresholds.
# The assumption that discrimination is equal for the trait and random threshold is likely to be violated
# makes the discrimination parameters hard to interpret (do they reflect the trait or varying thresholds?)
# Hence, we extend the model for a new set of discrimination parameter that differs from the ones of the trait
# but are set equal between random threshold dimensions

nDimW2011 <- nDim + nThres # number of dimensions in random threshold model (traits + random thresholds)
nGammaW2011 <- nItems + nItems # number of discrimination parameters (gammaslope in TAM): one for each item for content traits and joint discrimination parameter for each item and RS-dimensions

# cumulative scoring weights for random thresholds
styleW2011 <- rbind(c(0,1,1,1,1),
                 c(0,0,1,1,1),
                 c(0,0,0,1,1),
                 c(0,0,0,0,1))

# design matrix
E <- array(0, dim = c(nItems,nCat,nDimW2011,nGammaW2011)) # I x (K+1) x D x N

# Trait
for(d in 1:nDim){
  for (k in 1:nCat){
    dim_i <- (nItemsDim * (d-1)+1) : (nItemsDim * d)
    for (i in dim_i){
      if(!i %in% whichRev){
        E[i, k, d, i] <- k-1
      }
      else if(i %in% whichRev){
        E[i, k, d, i] <- nThres - (k-1)
      }
    }
  }
}

# Random Thresholds
for (i in 1:nItems){
  for (d in 1:nThres){
    E[i,,(nDim + d),(i+nItems)] <- styleW2011[d,]
  }
}


# variance fixed
variance.fixed <- matrix(c(
  # fix covariances between latent dimensions to zero
  # within Big Five
  # 1,2,0,  1,3,0,  1,4,0,  1,5,0,
  # 2,3,0,  2,4,0,  2,5,0,
  # 3,4,0,  3,5,0,
  # 4,5,0,
  # between Big Five and RS and within RS
  1,6,0,  1,7,0,  1,8,0,  1,9,0,
  2,6,0,  2,7,0,  2,8,0,  2,9,0,
  3,6,0,  3,7,0,  3,8,0,  3,9,0,
  4,6,0,  4,7,0,  4,8,0,  4,9,0,
  5,6,0,  5,7,0,  5,8,0,  5,9,0,
  6,7,0,  6,8,0,  6,9,0,
  7,8,0,  7,9,0,
  8,9,0,
  # fix variance of trait dimensions to 1 for estimation of discrimination parameters
  1,1,1,
  2,2,1,
  3,3,1,
  4,4,1,
  5,5,1,
  6,6,1,
  7,7,1,
  8,8,1,
  9,9,1),ncol=3,byrow=TRUE)


# fit model
wang2011 <- tam.mml.3pl(resp=temp,
                         E=E,
                         variance.fixed=variance.fixed,
                         control = controlList)




# Alternative specification -----------------------------------------------

# # alternative to fixing the trait variance: first gammaslope of each gammaslope set is fixed 1, all others are estimated
# gammaslope.fixed <- matrix(c(1,1, # dim 1
#                              1*nItemsDim+1,1, # dim 2
#                              2*nItemsDim+1,1, # dim 3
#                              3*nItemsDim+1,1, # dim 4
#                              4*nItemsDim+1,1, # dim 5
#                              nItems+1,1), # Response style dimension
#                            ncol=2, byrow=TRUE)
