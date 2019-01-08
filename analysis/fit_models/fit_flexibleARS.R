

# Fit Lifted Symmetry Model -----------------------------------------------

nDimLf <- nDim + 3 # only ERS dimension
nGammaLf <- 1 + 1 + 1 + 2 # for trait´, ERS, MRS it is fixed to one, for ARS one is estimated, one is fixed for lifted symmetry

# Design matrix
E <- array(0, dim = c(nItems, nCat, nDimLf, nGammaLf)) # I x (K+1) x D x N

# Trait
for(d in 1:nDim){
  for (k in 1:nCat){
    dim_i <- (nItemsDim * (d-1)+1) : (nItemsDim * d)
    for (i in dim_i){
      if(!i %in% whichRev){
        E[i, k, d, 1] <- k-1
      }
      else if(i %in% whichRev){
        E[i, k, d, 1] <- nThres - (k-1)
      }
    }
  }
}

# Response styles
for (i in 1:nItems){
  E[i, c(1,5) ,6, 2] <- 1 # ERS
  E[i, 3, 7, 3] <- 1 # MRS
  E[i, 4, 8, 4] <- 1 # ARS
  E[i, 5, 8, nGammaLf] <- 1 # ARS
}

gammaslope.fixed <- matrix(c(
  1,1, # trait
  2,1, # ERS
  3,1, # MRS
  4,1  # ARS 1
), ncol=2,byrow=TRUE)


flexibleARS <- tam.mml.3pl(resp=temp,
                           E=E,
                           gammaslope.fixed=gammaslope.fixed,
                           control = controlList)


