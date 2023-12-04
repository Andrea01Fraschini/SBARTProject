source('../../../env_setup_tests.R')
source("R/library_imports.R")
library(testthat)

describe("Test init_chain function",{
    source("data/sample_data.R")
    source("R/SBART/init_model_parameters.R")
    source("R/SBART/MCMC/init_chain.R")
    source("__tests__/R/SBART/MCMC/kim_output/output_init_chain.R")

    set.seed(1)
    data <- sample_data()
    
    set.seed(1)
    params <- init_model_parameters(
        X = data$Xpred,
        Y = data$Y,
        SIAM = data$wind_mat,
        n.trees = 50L
    )

    set.seed(1)
    vars <- init_chain(
        n.iterations = 10000L,
        n.locations.all = params$n.locations.all,
        p = params$p,
        n.trees = 50L,
        n = params$n,
        X = data$Xpred,
        Y = params$Y,
        missing_indexes = data$mis.ind,
        SIAM = data$wind_mat,
        W = data$Ws,
        rho = params$rho
    )

    it("should return a list",{
        expect_is(vars, "list")
    })

    it("should return a list with 26 elements",{
        expect_equal(length(vars), 26)
    })

    it("should return a list with the correct names",{
        expect_equal(names(vars), c(
            "sigma2.samples",
            "rho.samples",
            "tau2.samples",
            "spatial_theta",
            "cov_sel",
            "obs_list.ind",
            "dt_list",
            "trees",
            "trees.pred",
            "Xlist",
            "Xmult",
            "X.unique",
            "W_sel",
            "W_sel.samples",
            "W.count",
            "W.siam",
            "W.siam.full",
            "W.post",
            "W.post.full",
            "Wstar",
            "Wstar.eigen",
            "Wstar.eigen_vals",
            "det.Q",
            "Y",
            "missing_indexes",
            "Y.da"
        ))
    })

    it("should return a list with the correct class",{
        expect_equal(class(vars), "list")
    })

    it("should return kim's results",{
        expect_equal(vars$sigma2.samples, output_init_chain$sigma2.samples)
        expect_equal(vars$rho.samples, output_init_chain$rho.samples)
        expect_equal(vars$tau2.samples, output_init_chain$tau2.samples)
        expect_equal(vars$spatial_theta, output_init_chain$spatial_theta)
        expect_equal(vars$cov_sel, output_init_chain$cov_sel)
        expect_equal(vars$obs_list.ind, output_init_chain$obs_list.ind)

        for(i in seq_along(vars$dt_list)) {
            dt_list1 <- vars$dt_list[[i]]
            dt_list2 <- output_init_chain$dt_list[[i]]
        
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
        
        expect_equal(vars$trees, output_init_chain$trees)
        expect_equal(vars$trees.pred, output_init_chain$trees.pred)
        expect_equal(vars$Xlist, output_init_chain$Xlist)
        expect_equal(vars$Xmult, output_init_chain$Xmult)
        expect_equal(vars$X.unique, output_init_chain$X.unique)
        expect_equal(vars$W_sel, output_init_chain$W_sel)
        expect_equal(vars$W_sel.samples, output_init_chain$W_sel.samples)
        expect_equal(vars$W.count, output_init_chain$W.count)
        expect_equal(vars$W.siam, output_init_chain$W.siam)
        expect_equal(vars$W.siam.full, output_init_chain$W.siam.full)

        expect_equal(vars$W.post$n.rows, output_init_chain$W.post$n)
        W_post1 <- vars$W.post
        W_post2 <- output_init_chain$W.post
        W_post1$n <- NULL
        W_post2$n <- NULL
        W_post1$n.rows <- NULL
        W_post2$n.rows <- NULL
        expect_equal(W_post1, W_post2)

        expect_equal(vars$W.post.full$n.rows, output_init_chain$W.post.full$n)
        W_post_full1 <- vars$W.post.full
        W_post_full2 <- output_init_chain$W.post.full
        W_post_full1$n <- NULL
        W_post_full2$n <- NULL
        W_post_full1$n.rows <- NULL
        W_post_full2$n.rows <- NULL
        expect_equal(W_post_full1, W_post_full2)

        expect_equal(vars$Wstar, output_init_chain$Wstar)
        expect_equal(vars$Wstar.eigen, output_init_chain$Wstar.eigen)
        expect_equal(vars$Wstar.eigen_vals, output_init_chain$Wstar.eigen_vals)
        expect_equal(vars$det.Q, output_init_chain$det.Q)
        expect_equal(vars$Y, output_init_chain$Y)
        expect_equal(vars$missing_indexes, output_init_chain$missing_indexes)
        expect_equal(vars$Y.da, output_init_chain$Y.da)
    })

})