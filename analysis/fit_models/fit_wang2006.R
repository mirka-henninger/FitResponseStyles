
# Fit Random Threshold Models (Wang et al, 2006) ---------------------------

nDimW2009 <- nDim + nThres # number of dimensions in random threshold model (traits + random thresholds)

# cumulative scoring weights for random thresholds
styleW2009 <- rbind(c(0,1,1,1,1),
                 c(0,0,1,1,1),
                 c(0,0,0,1,1),
                 c(0,0,0,0,1))

# design matrix
B <- array(0, dim = c(nItems,nCat,nDimW2009)) # I x (K+1) x D

# Trait (fill in design matrix with scoring weights for the trait)
for (d in 1:nDim){
  for (k in 1:nCat){
    dim_i <- (nItemsDim * (d-1)+1) : (nItemsDim * d)
    for (i in dim_i){
      if(!i %in% whichRev){
        B[i, k, d] <- k-1
      }
      else if(i %in% whichRev){
        B[i, k, d] <- nThres - (k-1)
      }
    }
  }
}

# Random Thresholds
for (i in 1:nItems){
  for (d in 1:nThres){
    B[i,,(nDim + d)] <- styleW2009[d,]
  }
}

# variance fixed
# matrix to define combinations of dimensions (column 1 and 2) that are fixed to a certain value (column 3)
variance.fixed <- matrix(c(
  # fix covariances between latent dimensions to zero
  # within Big Five
  # 1,2,0,  1,3,0,  1,4,0,  1,5,0,
  # 2,3,0,  2,4,0,  2,5,0,
  # 3,4,0,  3,5,0,
  # 4,5,0,
  # between Big Five and RS and between RS
  1,6,0,  1,7,0,  1,8,0,  1,9,0,
  2,6,0,  2,7,0,  2,8,0,  2,9,0,
  3,6,0,  3,7,0,  3,8,0,  3,9,0,
  4,6,0,  4,7,0,  4,8,0,  4,9,0,
  5,6,0,  5,7,0,  5,8,0,  5,9,0,
  6,7,0,  6,8,0,  6,9,0,
  7,8,0,  7,9,0,
  8,9,0),ncol=3,byrow=TRUE)

wang2006 <- TAM::tam.mml(resp=dat,
                         B=B,
                         variance.fixed=variance.fixed,
                         control = controlList)
