# generate data to illustrate model fit

N <- 500
nItems <- 60
nDim <- 5
nDimC <- 5
nItemsDim <- 12
nCat <- 5
dimNames <- c("neuro", "extra", "open", "agree", "consc")
whichRev <- c(1,4,7,10,13,16,19,22,25,28,31,34,37,40,43,46,69,52,55,58)

simDat <- create_ratings(N = N, nItems = nItems, nDim = nDim, nDimC = nDimC, 
                         dimNames = dimNames, nCat = nCat, nItemsDim,
                         whichRev = whichRev)

dat <- simDat$ratingdata -1

colnames(dat) <- c(
  paste0("neuro",1:12),
  paste0("extra",1:12),
  paste0("open",1:12),
  paste0("agree",1:12),
  paste0("consc",1:12))



# export data -------------------------------------------------------------

usethis::use_data_raw()
usethis::use_data(dat,whichRev,overwrite=TRUE)
