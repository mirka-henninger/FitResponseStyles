#' Scoring Weights for Response Styles ERS, MRS, ARS
#'
#' Create scoring weights for ERS, MRS, ARS based on the number of categories
#'
scoring_weights <- function(nCat){
  nThres <- nCat-1
  styleERS <- c(-1, rep(0,nThres-2),1)
  styleMRS <- rep(0,nThres)
  if(nCat %% 2 == 0){ # for even numbers of categories
    styleMRS[floor(nThres/2)] <- 1
    styleMRS[ceiling(nThres/2)+1] <- -1
  } else{ # for odd numbers of categories
    styleMRS[floor(nCat/2)] <- 1
    styleMRS[ceiling(nCat/2)] <- -1
  }
  styleARS <- rep(0,nThres)
  styleARS[nThres/2+1] <- +1

  thresWeights <- rbind(styleERS,styleMRS,styleARS)
  colnames(thresWeights) <- paste0("Thres",1:nThres)

  cumWeights <- rbind(c(0,cumsum(styleERS)),
                     c(0,cumsum(styleMRS)),
                     c(0,cumsum(styleARS)))
  colnames(cumWeights) <- paste0("Cat",1:nCat)
  rownames(cumWeights) <- c("ERS","MRS","ARS")

  weights <- list(
    thresWeights=thresWeights,
    cumWeights=cumWeights
  )
  return(weights)
}
scoring_weights(5)
