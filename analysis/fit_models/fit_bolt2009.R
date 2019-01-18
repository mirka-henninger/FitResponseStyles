# Fit multidimensional NRM ------------------------------------------------

nDimB <- nDim + nDimRS # number of dimensions in mNRM (trait + response styles)
nGammaTraitB <- 1 # number of discrimination parameters for the trait that are fixed (see below)
nGammaB <- nGammaTraitB + nCat * nDimRS # total number of discrimination parameters; discr. par. of the trait + one for each category-response style combination

# design matrix E
E <- array(0, dim = c(nItems, nCat, nDimB, nGammaB)) # I x (K+1) x D x N

# Trait
for (d in 1:nDim){
  for (k in 1:nCat){
    dim_i <- (nItemsDim * (d-1)+1) : (nItemsDim * d)
    for (i in dim_i){
      if(!i %in% whichRev){
        E[i, k, d, nGammaTraitB] <- k-1
      }
      else if(i %in% whichRev){
        E[i, k, d, nGammaTraitB] <- nThres - (k-1)
      }
    }
  }
}

# RS
for (k in 1:nCat){
  E[, k, nDim + 1, nGammaTraitB + k] <- 1 # RS Dim 1
  E[, k, nDim + 2, nGammaTraitB + 1* nDim + k] <- 1 # RS Dim 2
  E[, k, nDim + 3, nGammaTraitB + 2* nDim + k] <- 1 # RS Dim 3
}


# variance fixed
variance.fixed <- matrix(c(
  # fix covariances between latent dimensions to zero
  # within Big Five
  # 1,2,0,  1,3,0,  1,4,0,  1,5,0,
  # 2,3,0,  2,4,0,  2,5,0,
  # 3,4,0,  3,5,0,
  # 4,5,0,
  # between trait and RS and within RS
  1,6,0,  1,7,0,  1,8,0,
  2,6,0,  2,7,0,  2,8,0,
  3,6,0,  3,7,0,  3,8,0,
  4,6,0,  4,7,0,  4,8,0,
  5,6,0,  5,7,0,  5,8,0,
  6,7,0,  6,8,0,
  7,8,0,
  # fix response style variances to 1
  6,6,1,
  7,7,1,
  8,8,1
),ncol=3,byrow=TRUE) # fix correlation between two dimensions to 0


# fix gammaslope 1 to 1 for traits
# estimate gammaslope 2 to 16 (response style scoring weights)
gammaslope.fixed <- cbind(1,1)



# fit model
bolt2009 <- TAM::tam.mml.3pl(resp=temp,
                             E=E,
                             gammaslope.fixed = gammaslope.fixed,
                             variance.fixed=variance.fixed,
                             control = controlList)




# Alternative specifications ----------------------------------------------

# # fix gammaslope 1-5 (for each category of the trait to 0:4)
# # estimate gammaslope 6 to 10 (response style scoring weights)
# gammaslope.fixed <- cbind(1:(nCat * nDim),0:4)
