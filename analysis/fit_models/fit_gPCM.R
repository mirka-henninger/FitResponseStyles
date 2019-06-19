

# Fit gPCM model ----------------------------------------------------------

nDimgPCM <- nDim
nGammagPCM <- nItems

# Design matrix
E <- array(0, dim = c(nItems, nCat, nDimgPCM, nGammagPCM)) # I x (K+1) x D x N

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


# variance of dimensions is fixed to 1
variance.fixed <- matrix(c(
  1,1,1,
  2,2,1,
  3,3,1,
  4,4,1,
  5,5,1),ncol=3,byrow=TRUE)

gPCM <- tam.mml.3pl(resp=dat,
                    E=E,
                    variance.fixed=variance.fixed,
                    control = controlList)


