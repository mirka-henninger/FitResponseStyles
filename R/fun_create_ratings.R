#' Create Ratings New
#'
#' Creates rating data from a Partial Credit Model with optional response styles ERS, MRS, and ARS
#'
#' @param N number of participants
#' @param nItems number of items
#' @param nCat number of categories
#' @param nDim number of dimensions (first dimension is trait dimension)
#' @param nDimC number of trait / content dimensions
#' @param dimNames a vector of length nDim with names of dimensions;
#' (response styles must be named ERS, MRS, and ARS!);
#' for ERS, extreme categories are coded 1, and 0 otherwise
#' for MRS, the middle category/ies is coded 1, and 0 otherwise
#' for ARS, agreement categories are coded 1, and 0 otherwise
#' @param thresholdPar a vector of length nCat-1 of threshold parameters (optional);
#' if empty threshold parameter are drawn from a uniform distribution U(-2.5,2.5)
#' @param itemPar a vector of length nItems of item parameters (optional);
#' if empty itemParameter are drawn from a truncated normal distribution TN(0,1,-1.5,1.5)
#' @param personPar a matrix of dimensions N x nDim (optiona);
#' if empty, person parameters are drawn from a multivariate normal distribution with mean = 0 and Sigma = Sigma;
#' @param Sigma a symmetric variance-covariance matrix of dimensions nDim x nDim,
#' if empty, Sigma is drawn from a Wishart distribution with an identity scale matrix;
#' latent trait variances are set to 1
#' @param wishartDf degrees of freedom of the Wishart distribution; default = 10
#' @param whichRev vector containing item numbers of reversed coded items
#' @export
#' @return a list containing "input": function arguments; "parameter": parameter used for data generation; "ratingdata": a dataframe with rating scale data
#'
#' @seealso draw_parameters
#'
#' @examples
#' set.seed(222)
#' dat <- create_ratings(N = 200, nItems = 20, nCat = 5, nDim = 3, nDimC = 2, dimNames= c("trait","ERS","MRS"))
#' dat$input
#' dat$parameter
#' dat$ratingdata
#' apply(dat$ratingdata,2,table) %>% barplot
create_ratings <- function(N, nItems, nCat, nDim, nDimC,
                           nItemsDim,
                           dimNames = c("trait"),
                           thresholdPar = NA,
                           itemPar = NA,
                           personPar = NA,
                           Sigma = NA,
                           wishartDf = 10,
                           whichRev = 0){
  require(dplyr)
  require(truncnorm)

  if(length(dimNames) != nDim){
    warning("please specify argument dimNames correctly")
  }


  nThres <- nCat-1 # number of thresholds = number of categories - 1


  # Draw Parameters ---------------------------------------------------------

  parameter <- draw_parameters(N = N, nItems = nItems, nCat = nCat,
                               nDim = nDim, dimNames = dimNames,
                               Sigma = Sigma, wishartDf = wishartDf)

  thresholdPar <- parameter$parameter$thresholdpar
  itemPar <- parameter$parameter$itempar

  personPar <- parameter$parameter$theta
  theta <- data.frame(personPar)
  colnames(theta) <- dimNames

  Sigma <- parameter$parameter$Sigma




  # Define Scoring Weights --------------------------------------------------

  weights <- scoring_weights(nCat)

  styleERS <- weights$thresWeights["styleERS",]
  styleMRS <- weights$thresWeights["styleMRS",]
  styleARS <- weights$thresWeights["styleARS",]

  trait <- theta[,1:nDimC]
  traitERS <- if("ERS" %in% dimNames) theta[,"ERS"] else rep(0,N)
  traitMRS <- if("MRS" %in% dimNames) theta[,"MRS"] else rep(0,N)
  traitARS <- if("ARS" %in% dimNames) theta[,"ARS"] else rep(0,N)


  # Generate Data -----------------------------------------------------------

  u <- matrix(runif(n=N*nItems), ncol=nItems) # create a matrix of random numbers
  ratingdata  <- matrix(nrow=N, ncol=nItems) # create a frame for the data

  cumexp <- vector(mode="numeric", length=nCat) # create a frame for vector "cumexp"

  for(n in 1:N) { # one loop for persons
    for(i in 1:nItems) { # one for items

      if(i %in% whichRev){
        # for reversed coded items
        measure <- 0
        cumexp[nCat] <- 1

        for (k in nCat:2){ # one for thresholds

          # check item dimension
          for (d in 1:nDimC){
            if(i %in% (nItemsDim*(d-1)+1):(nItemsDim*d)){
              dd <- d
            }
          }

          # define the numerator of the Rasch model (the exponential function is applied in the next row)
          measure <- measure +
            trait[n,dd] +
            styleERS[nCat-k+1]*traitERS[n]+
            styleMRS[nCat-k+1]*traitMRS[n]+
            styleARS[nCat-k+1]*traitARS[n]-
            thresholdPar[k-1] -
            itemPar[i]
          cumexp[k-1] <- cumexp[k] + exp(measure) # this is the normalizing constant (denominator)
        } # close k in 2:nCat

        for (kk in nCat:1) {
          # a trick to do normalization: numerator / denominator = u/1 --> numerator = u * denominator
          u_temp <- u[n,i] * cumexp[1]
          # if the random number*numerator <= cumulative probability --> save the category in the dataframe
          if(u_temp <= cumexp[kk]) {
            # save entries in dataframe
            ratingdata[n,i] <- kk
            # break, if entry was made
            break
          } # close if statement
        }

      } else{
        # non reversed coded items
        measure <- 0
        cumexp[1] <- 1

        for (k in 2:nCat){ # one for thresholds

          # check item dimension
          for (d in 1:nDimC){
            if(i %in% (nItemsDim*(d-1)+1):(nItemsDim*d)){
              dd <- d
            }
          }

          # define the numerator of the Rasch model (the exponential function is applied in the next row)
          measure <- measure +
            trait[n,dd] +
            styleERS[k-1]*traitERS[n] +
            styleMRS[k-1]*traitMRS[n] +
            styleARS[k-1]*traitARS[n] -
            thresholdPar[k-1] -
            itemPar[i]
          # this is the normalizing constant (denominator)
          cumexp[k] <- cumexp[k-1] + exp(measure)
        } # close k in 2:nCat

        for (kk in 1:nCat) {
          # a trick to do normalization: numerator / denominator = u/1 --> numerator = u * denominator
          u_temp <- u[n,i] * cumexp[nCat]
          # if the random number*numerator <= cumulative probability --> save the category in the dataframe
          if(u_temp <= cumexp[kk]) {
            # save entries in dataframe
            ratingdata[n,i] <- kk
            break # break, if entry was made
          } # close if statement
        } # close kk in 1:nCat

      } # close else statement

    } # close i in 1:nItems
  } # close n in 1:N




  # Save Parameters and Data ------------------------------------------------

  input <- list(N = N,
                nItems = nItems,
                nCat = nCat,
                nThres = nThres,
                nDim = nDim,
                dimNames = dimNames,
                thresholdPar = thresholdPar,
                itemPar = itemPar,
                personPar = personPar,
                Sigma = Sigma,
                wishartDf = wishartDf)


  parameter <- list(
    thresholdPar = thresholdPar,
    itemPar = itemPar,
    Sigma = Sigma,
    theta = theta,
    traitWeights = data.frame(normal = 0:nThres,reversed = nThres:0),
    thresWeights = weights$thresWeights,
    cumWeights = weights$cumWeights
  )

  ratingdata <- data.frame(ratingdata)
  colnames(ratingdata) <- paste0("item",1:nItems)




  # Return Output -----------------------------------------------------------

  to.return <- list(input = input,
                    parameter=parameter,
                    ratingdata=ratingdata)
  return(to.return)
}

