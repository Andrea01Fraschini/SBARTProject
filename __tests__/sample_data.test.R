library(testthat)
source("R/library_imports.R")

describe("Test sample data function",{
    source("data/sample_data.R")

    results <- sample_data(1)

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
})