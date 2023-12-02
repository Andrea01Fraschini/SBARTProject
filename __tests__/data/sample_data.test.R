setwd("C:/Users/camil/OneDrive - Universidad del Norte/Universidad POLIMI/Bayesian stats/CODE Bart/SBARTProject")
library(testthat)
source("R/library_imports.R")

describe("Test sample data function",{
    source("data/sample_data.R")
    source("__tests__/data/kim_output/output_sample_data_1.R")
    source("__tests__/data/kim_output/output_sample_data_2.R")

    set.seed(1)

    results <- sample_data()

    it("should return a list",{
        expect_is(results, "list")
    })

    it("should return a list with 5 elements",{
        expect_equal(length(results), 5)
    })

    it("should return a list with the correct names",{
        expect_equal(names(results), c("Xpred", "Y", "mis.ind", "Ws", "wind_mat"))
    })

    it("should return a list with the correct dimensions",{
        expect_equal(dim(results$Xpred), c(225, 50))
        expect_equal(length(results$Y), 225)
        expect_equal(length(results$Ws), 5)
        expect_equal(dim(results$wind_mat), c(225, 225))
        expect_equal(length(results$mis.ind), 195)
    })

    it("Should return a list with the correct class",{
        expect_is(results$Xpred, "matrix")
        expect_is(results$Y, "numeric")
        expect_is(results$Ws, "list")
        expect_is(results$wind_mat, "matrix")
        expect_is(results$mis.ind, "integer")
    })

    it("Should return Kim's results",{
        expect_equal(results$Xpred, output_sample_data_1$Xpred)
        expect_equal(results$Y, output_sample_data_1$Y)
        expect_equal(results$Ws, output_sample_data_2$Ws)
        expect_equal(results$wind_mat, output_sample_data_1$wind_mat)
        expect_equal(results$mis.ind, output_sample_data_1$mis.ind)
    })
})