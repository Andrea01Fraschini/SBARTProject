rm(list = ls())

# CHANGE THIS PATH TO YOUR LOCAL PATH
Sys.setenv("PKG_CXXFLAGS" = "-std=c++11 -IC:/Users/camil/AppData/Local/R/win-library/4.3/RcppArmadillo/include -IC:/Users/camil/AppData/Local/R/win-library/4.3/Rcpp/include")

# CHANGE THIS PATH TO YOUR LOCAL PATH
if (Sys.info()["sysname"] == "Linux") {
  setwd(getwd())
} else {
    setwd("C:/Users/camil/OneDrive - Universidad del Norte/Universidad POLIMI/Bayesian stats/CODE Bart/SBARTProject") 
}