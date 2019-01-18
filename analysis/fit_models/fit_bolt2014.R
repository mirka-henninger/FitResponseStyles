# Fit Bolt et al, 2014 model ----------------------------------------------

nDimB <- nDim + nCat -1 # traits + categories

# cumulative scoring weights for centered category specific preferences
styleB2014 <- rbind(c(-1,1,0,0,0),
                 c(-1,0,1,0,0),
                 c(-1,0,0,1,0),
                 c(-1,0,0,0,1))

# design matrix
B <- array(0, dim = c(nItems,nCat,nDimB)) # I x (K+1) x D

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
    B[i,,(nDim + d)] <- styleB2014[d,]
  }
}


bolt2014 <- TAM::tam.mml(resp=temp,
                         B=B,
                         control = controlList)

