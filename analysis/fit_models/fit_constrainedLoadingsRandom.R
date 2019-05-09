
# Fit constrained loadings model ----------------------------------------------------------

nDimF2016 <- nDim + nDimRS
nGammaRS <- 8
nGammaF2016 <- nItems + nGammaRS + nGammaRS + 1 # 1 gammaslope für ERS, MRS, ARS

# Design matrix
E <- array(0, dim = c(nItems, nCat, nDimF2016, nGammaF2016)) # I x (K+1) x D x N

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


# draw random number to create a designvector that is independent of any item characteristics
set.seed(15102018)
designvector <- base::sample(1:8, 60, replace = TRUE)


# Response styles
for (i in 1:nItems){
  E[i, c(1,5) ,6, nItems + designvector[i] ] <- 1 # ERS
  E[i, 3,      7, nItems + nGammaRS + designvector[i] ] <- 1 # MRS
  E[i, c(4,5), 8, nGammaF2016  ] <- 1 # ARS loadings are fixed equal across items (see Falk 2016)
}

# variance of dimensions is fixed to 1
variance.fixed <- matrix(c(
  1,1,1,
  2,2,1,
  3,3,1,
  4,4,1,
  5,5,1,
  6,6,1,
  7,7,1,
  8,8,1),ncol=3,byrow=TRUE)

constrainedLoadingsRandom <- TAM::tam.mml.3pl(resp=dat,
                                              E=E,
                                              variance.fixed=variance.fixed,
                                              control = controlList)


