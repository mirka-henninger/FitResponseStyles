
# Fit PCM -----------------------------------------------------------------

nDim <- nDim # number of dimensions in PCM

# Design matrix
B <- array(0, dim = c(nItems,nCat,nDim)) # I x (K+1) x D

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


# Fit model
PCM <- TAM::tam.mml(dat, irtmodel ="PCM", B = B,
                    control = controlList)




# Alternative Specification -----------------------------------------------

# # Trait (fill in design matrix with scoring weights for the trait); only positively coded items
# for (d in 1:n_dim){
#   for (k in 1:nCat){
#     B[(nItemsDim * (d-1)+1) : (nItemsDim * d), k, d] <- k-1
#   }
# }
