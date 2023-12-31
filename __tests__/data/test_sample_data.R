source('../env_setup_tests.R')
source("R/library_imports.R")
library(testthat)

describe("Test sample data function",{
    source("data/sample_data.R")
    source("output/KIM/output_sample_data_1.R")
    source("output/KIM/output_sample_data_2.R")

    set.seed(1)
    results <- sample_data()

    it("should return a list",{
        expect_is(results, "list")
    })

    it("should return a list with 5 elements",{
        expect_equal(length(results), 5)
    })

    it("should return a list with the correct names",{
        expect_equal(names(results), c("y", "x_predictors", "missing_indexes", "ws", "wind_matrix"))
    })

    it("Should return a list with the correct class",{
        expect_is(results$x_predictors, "matrix")
        expect_is(results$y, "numeric")
        expect_is(results$ws, "list")
        expect_is(results$wind_matrix, "matrix")
        expect_is(results$missing_indexes, "integer")
    })

    it("Should return Kim's results",{
        expect_equal(results$x_predictors, output_sample_data_1$x_predictors)
        expect_equal(results$y, output_sample_data_1$y)
        expect_equal(results$ws, output_sample_data_2$ws)
        expect_equal(results$wind_matrix, output_sample_data_1$wind_matrix)
        expect_equal(results$missing_indexes, output_sample_data_1$missing_indexes)
    })
})