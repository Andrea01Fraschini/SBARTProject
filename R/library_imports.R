libraries <- c(
    "lmf", 
    "MASS",
    "mnormt", 
    "MCMCpack", 
    "rootSolve", 
    "R.matlab", 
    "truncnorm", 
    "data.table", 
    "Rcpp", 
    "reshape", 
    "covr"
    )

for(lib in libraries) {
  if (!require(lib, character.only = TRUE)) {
    install.packages(lib)
    library(lib, character.only = TRUE)
  }
}