source('../env_setup_tests.R')
source("R/library_imports.R")
library(testthat)

describe("Test predict function",{
    source("R/SBART/predict.R")
    source("output/KIM/input_predict.R")
    source("output/KIM/output_predict.R")
    source("__tests__/scripts/transform_dt_list.R")

    transformed_dt_list <- transform_dt_list(input_predict$dt_list)

    set.seed(1)
    trees_pred <- matrix(unlist(sapply(1:input_predict$n_trees, function(x) mean_predict(transformed_dt_list[[x]], input_predict$x_list, input_predict$x_mult, input_predict$x_unique, input_predict$n))), nrow = input_predict$n, ncol = input_predict$n_trees)

    it("Should return Kim's results",{
        expect_equal(trees_pred, output_predict$trees_pred)
    })
})