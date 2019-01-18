
# Designmatrix for item loadings ------------------------------------------
# # if reversed coded items are modeled:
# reversed <- rep(0,60)
# reversed[whichRev] <- 1

negation <- c(1,0,1,0,0, # item 1; columns are dimensions (N, E, O, A, C)
              0,0,0,0,0, # item 2
              0,1,0,0,1, # item 3
              0,0,0,0,0, # item 4
              0,0,1,0,0, # item 5
              0,0,0,0,0, # item 6
              0,0,0,0,0, # item 7
              0,0,0,0,0, # item 8
              0,1,0,0,1, # item 9
              0,0,0,0,0, # item 10
              0,0,0,1,1, # item 11
              0,0,0,0,0) # item 12
complexity <- c(0,0,0,1,0, # item 1; columns are dimensions (N, E, O, A, C)
                0,0,0,0,1, # item 2
                1,0,1,0,0, # item 3
                0,0,1,1,0, # item 4
                0,0,0,0,1, # item 5
                0,0,0,1,1, # item 6
                0,0,1,0,0, # item 7
                1,0,1,0,1, # item 8
                1,0,1,0,1, # item 9
                0,0,1,0,1, # item 10
                1,0,0,1,1, # item 11
                1,1,0,1,0) # item 12

pos <- data.frame(NEOFFI_pos = 1:60,
                  NEOPIR_pos = c(1,37,93,44,40,136,237,53,229,25,86,147,
                                  98,14,70,11,122,28,19,15,91,142,128,4,
                                  50,41,67,108,64,55,61,107,163,164,110,6,
                                  177,88,74,135,221,87,188,59,45,71,197,173,
                                  104,85,26,227,203,109,130,76,162,23,39,200
                  )
)

position <- ifelse(pos$NEOPIR_pos<=120, 0, 1)

# transpose into N*12, E*12, O*12, A*12, C*12 format
negationT <- negation %>% matrix(., ncol=12, byrow=FALSE) %>% t() %>% c()
complexityT <- complexity %>% matrix(., ncol=12, byrow=FALSE) %>% t() %>% c()
positionT <- position %>% matrix(., ncol=12, byrow=FALSE) %>% t() %>% c()


designmatrixF <- data.frame(colnames(dat),negationT,complexityT,positionT)
designmatrix <- designmatrixF[,-1]

factors <- designmatrix[!duplicated(designmatrix),]

# create designvector that indicate which factor loadings are set equal
designvector <- c()
for (i in 1:60){
  if(all(designmatrix[i,]== factors[1,])){
    designvector[i] <- 1
  } else if (all(designmatrix[i,]==factors[2,])){
    designvector[i] <- 2
  } else if (all(designmatrix[i,]==factors[3,])){
    designvector[i] <- 3
  } else if (all(designmatrix[i,]==factors[4,])){
    designvector[i] <- 4
  } else if (all(designmatrix[i,]==factors[5,])){
    designvector[i] <- 5
  } else if (all(designmatrix[i,]==factors[6,])){
    designvector[i] <- 6
  } else if (all(designmatrix[i,]==factors[7,])){
    designvector[i] <- 7
  } else
    designvector[i] <- 8
}



# Fit constrained loadings model ----------------------------------------------------------

nDimF2016 <- nDim + nDimRS
nGammaRS <- nrow(factors)
nGammaF2016 <- nItems + nGammaRS + nGammaRS + 1 # drei Prädiktoren für ERS und MRS, 1 gammaslope für ARS

# Design matrix
E <- array(0, dim = c(nItems, nCat, nDimF2016, nGammaF2016)) # I x (K+1) x D x N

# Trait
for (d in 1:nDim){
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

constrainedLoadings <- TAM::tam.mml.3pl(resp=dat,
                                        E=E,
                                        variance.fixed=variance.fixed,
                                        control = controlList)
