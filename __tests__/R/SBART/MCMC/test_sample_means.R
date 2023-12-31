source('../../../env_setup_tests.R')
source("R/library_imports.R")
library(testthat)

describe("Test sample_means function",{
    source("R/SBART/MCMC/sample_means.R")
    source("output/KIM/output_init_model_parameters.R")
    source("output/KIM/output_init_chain.R")
    source("__tests__/scripts/transform_dt_list.R")
    source("output/KIM/output_update_residuals.R")
    source("output/KIM/output_sample_trees.R")
    source("output/KIM/output_sample_means.R")

    dt_list_transformed <- transform_dt_list(output_sample_trees$dt_list)

    set.seed(1)
    means <- sample_means(
        sigma2_samples = output_init_chain$sigma2_samples,
        sigma_mu = output_init_model_parameters$sigma_mu,
        obs_list_ind = output_sample_trees$obs_list_ind,
        residuals = output_update_residuals$residuals,
        x_cut = output_init_chain$x_unique,
        n_available = output_init_model_parameters$n,
        trees = output_init_chain$trees,
        dt_list = dt_list_transformed,
        t = 1,
        j = 2
    )

    it("should return a list",{
        expect_is(means, "list")
    })

    it("should return a list with 2 elements",{
        expect_equal(length(means), 2)
    })

    it("should return a list with the correct names",{
        expect_equal(names(means), c("trees", "dt_list"))
    })

    it("should return Kim's results",{
        for(i in seq_along(means$dt_list)) {
            dt_list1 <- means$dt_list[[i]]
            dt_list2 <- output_sample_means$dt_list[[i]]
        
            dt_list2$terminal <- ifelse(dt_list2$Terminal == 1, TRUE, FALSE)
            dt_list2$split <- dt_list2$Split
            dt_list2$value <- dt_list2$Value
            dt_list2$mu <- dt_list2$MU

            expect_equal(dt_list1$position, dt_list2$position)
            expect_equal(dt_list1$parent, dt_list2$parent)
            expect_equal(dt_list1$terminal, dt_list2$terminal)
            expect_equal(dt_list1$split, dt_list2$split)
            expect_equal(dt_list1$value, dt_list2$value)
            expect_equal(dt_list1$mu, dt_list2$mu)
            expect_equal(dt_list1$begin, dt_list2$begin)
            expect_equal(dt_list1$end, dt_list2$end)
        }

        expect_equal(means$trees, output_sample_means$trees)
    })


})