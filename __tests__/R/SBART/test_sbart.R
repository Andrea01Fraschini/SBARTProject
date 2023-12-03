# rm(list = ls())
# setwd(".")
# library(testthat)
# source("R/library_imports.R")

# describe("Test SBART functions",{
#     source("R/SBART/sbart.R")

#     data <- sample_data(1)

#     model <- sbart.fit(
#         X = data$Xpred,
#         y.train = data$Y[-data$mis.ind], 
#         missing_indexes = data$mis.ind,
#         W = data$Ws, 
#         SIAM = data$wind_mat,
#         n.trees = 10L
#     )

#     predictions <- sbart.predict(
#         sbart.output = model, 
#         X.test = data$Xpred[data$mis.ind],
#         mis.ind = data$mis.ind
#     )

#     # TODO: Test for functions
# })