#' Draw parameter to create ratingdata
#'
#' Draws item, threshold, and person parameters that can be used as an input to create simulated rating data
#'
#' @param N number of participants
#' @param nItems number of items
#' @param nDim number of dimensions
#' @param dimNames a vector of length nDim with names of dimensions;
#' (response styles must be named ERS, MRS, and ARS!)
#' @param Sigma optional: a symmetric variance-covariance matrix of dimensions nDim x nDim,
#' if empty, Sigma is drawn from a Wishart distribution with an identity scale matrix and 10 df;
#' latent trait variances are set to 1;
#'
#' @export
#'
#' @return a list containing: input: a list of function arguments; parameter: a list of drawn parameter
#'
#' @examples
#' set.seed(222)
#' dat <- draw_parameters(N = 200, nItems = 20, nCat = 5, nDim = 1, dimNames= "trait")
#' dat$input
#' dat$parameter
draw_parameters <- function(N, nItems, nCat, nDim,
                            dimNames = c("trait"),
                            Sigma = NA,
                            wishartDf = 10){

  # save input parameters
  input <- list(N = N,
                nItems = nItems,
                nCat = nCat,
                nDim = nDim,
                dimNames = dimNames,
                Sigma = Sigma)

  if(length(dimNames) != nDim){
    warning("please specify argument dimNames correctly")
  }

  # draw threshold parameters
  nThres <- nCat-1 # number of thresholds = number of categories - 1
  thresholdPar <- rep(999, nThres)
  while(max(thresholdPar) & abs(min(thresholdPar)) > 2.5) {
    thresholdPar <- runif(nThres, -2.5,2.5) %>% scale %>% sort; # as in Plieninger, 2017
    if (max(thresholdPar) & abs(min(thresholdPar)) < 2.5) break
  }

  # draw item parameters
  itemPar <- rtruncnorm(nItems, a = -1.5, b = 1.5, mean = 0, sd = 1) %>% scale # as in Plieninger 2017

  # draw covariance matrix Sigma
  if(nDim == 1){
    Sigma <- 1
  } else if (is.na(Sigma)){
    sd.vec <- rep(1,nDim)
    Imatrix <- diag(nDim)
    covarmat <- rWishart(1, wishartDf, Sigma = Imatrix) [,,1]
    cormat <- stats::cov2cor(covarmat)
    Sigma <- MBESS::cor2cov(cor.mat = cormat, sd = sd.vec)
  }

  # draw person parameters
  mu = rep(0,nDim)
  theta <- mvrnorm(N, mu=mu, Sigma=Sigma)

  theta <- data.frame(theta)
  colnames(theta) <- dimNames

  # save parameters
  parameter <- list(
    thresholdpar = thresholdPar,
    itempar = itemPar,
    Sigma = Sigma,
    theta = theta
  )

  to.return <- list(input = input,
                    parameter=parameter)
  return(to.return)
}
