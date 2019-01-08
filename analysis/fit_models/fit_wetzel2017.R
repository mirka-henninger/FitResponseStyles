
# Fit Wetzel model --------------------------------------------------------

nDimW2017 <- nDim + nDimRS

# Design matrix
B <- array(0, dim = c(nItems,nCat,nDimW2017)) # I x (K+1) x D

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

# Response styles
B[, c(1,5), (nDim + 1)] <- 1 # ERS
B[, 3     , (nDim + 2)] <- 1 # MRS
B[, c(4,5), (nDim + 3)] <- 1 # ARS

# Fit model
wetzel2017 <- tam.mml(temp, irtmodel ="PCM", B = B,
                      control = controlList)

