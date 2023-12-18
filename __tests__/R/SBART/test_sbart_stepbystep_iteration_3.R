source('../../env_setup_tests.R')
source("R/library_imports.R")
library(testthat)

describe("Test step by step iteration 3 sbart",{
    source("data/sample_data.R")
    source("R/SBART/init_model_parameters.R")
    source("R/SBART/MCMC/init_chain.R")
    source("R/SBART/MCMC/update_residuals.R")
    source("R/SBART/MCMC/sample_trees.R")
    source("R/SBART/MCMC/sample_means.R")
    source("R/SBART/MCMC/sample_variance.R")
    source("R/SBART/MCMC/update_spatial_effect.R")
    source("R/SBART/MCMC/update_tau.R")
    source("R/SBART/MCMC/update_rho.R")
    source("R/SBART/MCMC/update_f.R")
    source("R/SBART/MCMC/update_dirichlet_alpha.R")
    source("R/SBART/predict.R")

    source("__tests__/scripts/transform_dt_list.R")

    source("output/KIM/input_sample_variance_2.R")
    source("output/KIM/output_sample_data_1.R")
    source("output/KIM/output_sample_data_2.R")
    source("output/KIM/output_init_model_parameters.R")
    source("output/KIM/output_init_chain.R")
    source("output/KIM/output_update_residuals_2.R")
    source("output/KIM/output_sample_trees_2.R")
    source("output/KIM/output_sample_means_2.R")
    source("output/KIM/output_sample_variance_2.R")
    source("output/KIM/output_update_spatial_effect_2.R")
    source("output/KIM/output_update_tau_2.R")
    source("output/KIM/output_update_rho_2.R")
    source("output/KIM/output_update_f_2.R")
    source("output/KIM/output_update_dirichlet_alpha_2.R")
    source("output/KIM/output_predict_2.R")
    source("output/KIM/output_loop_trees_3_2.R")

    n_trees <- 50L
    n_iterations <- 10000L

    set.seed(1)
    data <- sample_data() # Xpred, Y, mis.ind, Ws, wind_mat

    it("Should return Kim's results",{
        expect_equal(data$x_predictors, output_sample_data_1$x_predictors)
        expect_equal(data$y, output_sample_data_1$y)
        expect_equal(data$ws, output_sample_data_2$ws)
        expect_equal(data$wind_matrix, output_sample_data_1$wind_matrix)
        expect_equal(data$missing_indexes, output_sample_data_1$missing_indexes)
    })

    set.seed(1)
    params <- results <- init_model_parameters(
        x = data$x_predictors,
        y = data$y,
        siam = data$wind_matrix,
        n_trees = n_trees
    ) # p, n, n.locations.all, prob.grow, prob.prune, prob.change, alpha, beta, dirichlet.alpha, posterior.dirichlet.alpha, cov.sel_prob, tau2.a, tau2.b, tau2.posterior.shape, tau2, proposal.sd.rho, rho, a0, b0, Y, residuals, sigm2, nu, lambda, sigma2.a, sigma2.b, sigma_mu, missing_indexes

    it("Should return Kim's results",{
        expect_equal(results$p, output_init_model_parameters$p)
        expect_equal(results$n, output_init_model_parameters$n)
        expect_equal(results$n_locations_all, output_init_model_parameters$n_locations_all)
        expect_equal(results$prob_grow, output_init_model_parameters$prob_grow)
        expect_equal(results$prob_prune, output_init_model_parameters$prob_prune)
        expect_equal(results$prob_change, output_init_model_parameters$prob_change)
        expect_equal(results$alpha, output_init_model_parameters$alpha)
        expect_equal(results$beta, output_init_model_parameters$beta)
        expect_equal(results$dirichlet_alpha, output_init_model_parameters$dirichlet_alpha)
        expect_equal(results$posterior_dirichlet_alpha, output_init_model_parameters$posterior_dirichlet_alpha)
        expect_equal(results$cov_sel_prob, output_init_model_parameters$cov_sel_prob)
        expect_equal(results$tau2_alpha, output_init_model_parameters$tau2_alpha)
        expect_equal(results$tau2_beta, output_init_model_parameters$tau2_beta)
        expect_equal(results$tau2_posterior_shape, output_init_model_parameters$tau2_posterior_shape)
        expect_equal(results$tau2, output_init_model_parameters$tau2)
        expect_equal(results$proposal_sd_rho, output_init_model_parameters$proposal_sd_rho)
        expect_equal(results$rho, output_init_model_parameters$rho)
        expect_equal(results$a0, output_init_model_parameters$a0)
        expect_equal(results$b0, output_init_model_parameters$b0)
        expect_equal(results$y, output_init_model_parameters$y)
        expect_equal(results$residuals, output_init_model_parameters$residuals)
        expect_equal(results$sigma2, output_init_model_parameters$sigma2)
        expect_equal(results$nu, output_init_model_parameters$nu)
        expect_equal(results$lambda, output_init_model_parameters$lambda)
        expect_equal(results$sigma2_a, output_init_model_parameters$sigma2_a)
        expect_equal(results$sigma2_b, output_init_model_parameters$sigma2_b)
        expect_equal(results$sigma_mu, output_init_model_parameters$sigma_mu)
        expect_equal(results$missing_indexes, output_init_model_parameters$missing_indexes)
    })

    set.seed(1)
    vars <- init_chain(
        n_iterations = n_iterations,
        n_locations_all = params$n_locations_all,
        p = params$p,
        n_trees = n_trees,
        n = params$n,
        x = data$x_pred,
        y = params$y,
        missing_indexes = data$missing_indexes,
        siam = data$wind_matrix,
        ws = output_sample_data_2$ws,
        rho = params$rho
    ) # sigma2.samples, rho.samples, tau2.samples, spatial_theta, cov_sel, obs_list.ind, dt_list, trees, trees.pred, Xlist, Xmult, X.unique, W_sel, W_sel.samples, W.count, W.siam, W.siam.full, W.post, W.post.full, Wstar, Wstar.eigen, Wstar.eigen_vals, det.Q, Y, missing_indexes, Y.da

    it("should return kim's results",{
        expect_equal(vars$sigma2_samples, output_init_chain$sigma2_samples)
        expect_equal(vars$rho_samples, output_init_chain$rho_samples)
        expect_equal(vars$tau2_samples, output_init_chain$tau2_samples)
        expect_equal(vars$spatial_theta, output_init_chain$spatial_theta)
        expect_equal(vars$cov_sel, output_init_chain$cov_sel)
        expect_equal(vars$obs_list_ind, output_init_chain$obs_list_ind)

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
        expect_equal(vars$trees_pred, output_init_chain$trees_pred)
        expect_equal(vars$x_list, output_init_chain$x_list)
        expect_equal(vars$x_mult, output_init_chain$x_mult)
        expect_equal(vars$x_unique, output_init_chain$x_unique)
        expect_equal(vars$w_sel, output_init_chain$w_sel)
        expect_equal(vars$w_sel_samples, output_init_chain$w_sel_samples)
        expect_equal(vars$w_count, output_init_chain$w_count)
        expect_equal(vars$w_siam, output_init_chain$w_siam)
        expect_equal(vars$w_siam_full, output_init_chain$w_siam_full)

        expect_equal(vars$w_post$n.rows, output_init_chain$w_post$n)
        W_post1 <- vars$w_post
        W_post2 <- output_init_chain$w_post
        W_post1$n <- NULL
        W_post2$n <- NULL
        W_post1$n.rows <- NULL
        W_post2$n.rows <- NULL
        expect_equal(W_post1, W_post2)

        expect_equal(vars$w_post_full$n.rows, output_init_chain$w_post_full$n)
        W_post_full1 <- vars$w_post_full
        W_post_full2 <- output_init_chain$w_post_full
        W_post_full1$n <- NULL
        W_post_full2$n <- NULL
        W_post_full1$n.rows <- NULL
        W_post_full2$n.rows <- NULL
        expect_equal(W_post_full1, W_post_full2)

        expect_equal(vars$w_star, output_init_chain$w_star)
        expect_equal(vars$w_star_eigen$values, output_init_chain$w_star_eigen$values)
        # expect_equal(vars$w_star_eigen$vectors, output_init_chain$w_star_eigen$vectors)
        expect_equal(vars$w_star_eigen_vals, output_init_chain$w_star_eigen_vals)
        expect_equal(vars$det_q, output_init_chain$det_q)
        expect_equal(vars$y, output_init_chain$y)
        expect_equal(vars$missing_indexes, output_init_chain$missing_indexes)
        expect_equal(vars$y_da, output_init_chain$y_da)
    })

    sigma2_samples <- vars$sigma2_samples
    rho_samples <- vars$rho_samples
    tau2_samples <- vars$tau2_samples
    spatial_theta <- vars$spatial_theta
    cov_sel <- vars$cov_sel
    obs_list_ind <- vars$obs_list_ind
    dt_list <- vars$dt_list
    trees <- vars$trees
    trees_pred <- vars$trees_pred
    x_list <- vars$x_list
    x_mult <- vars$x_mult
    x_unique <- vars$x_unique
    w_sel <- vars$w_sel
    w_sel_samples <- vars$w_sel_samples
    w_count <- vars$w_count
    w_siam <- vars$w_siam
    w_siam_full <- vars$w_siam_full
    w_post <- vars$w_post
    w_post_full <- vars$w_post_full
    w_star <- vars$w_star
    w_star_eigen <- vars$w_star_eigen
    w_star_eigen_vals <- vars$w_star_eigen_vals
    det_q <- vars$det_q
    y <- vars$y
    missing_indexes <- vars$missing_indexes
    y_da <- vars$y_da

    cov_sel_prob <- params$cov_sel_prob
    tau2 <- params$tau2
    rho <- params$rho
    dirichlet_alpha <- params$dirichlet_alpha

    for (j in 2:3){
        for(t in 1:50){
            if(t == 3 && j == 3){
                it("should return kim's results",{
                    expect_equal(sigma2_samples, output_loop_trees_3_2$sigma2_samples)
                    expect_equal(rho_samples, output_loop_trees_3_2$rho_samples)
                    expect_equal(tau2_samples, output_loop_trees_3_2$tau2_samples)
                    expect_equal(spatial_theta, output_loop_trees_3_2$spatial_theta)
                    
                    cov_sel_1 <- output_loop_trees_3_2$cov_sel
                    dimnames(cov_sel_1) <- list(c("cov_sel", "cov_sel_temp"))

                    expect_equal(cov_sel, cov_sel_1)
                    expect_equal(obs_list_ind, output_loop_trees_3_2$obs_list_ind)

                    for(i in seq_along(dt_list)) {
                        dt_list1 <- dt_list[[i]]
                        dt_list2 <- output_loop_trees_3_2$dt_list[[i]]
                    
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
                    
                    expect_equal(trees, output_loop_trees_3_2$trees)
                    expect_equal(trees_pred, output_loop_trees_3_2$trees_pred)
                    expect_equal(x_list, output_loop_trees_3_2$x_list)
                    expect_equal(x_mult, output_loop_trees_3_2$x_mult)
                    expect_equal(x_unique, output_loop_trees_3_2$x_unique)
                    expect_equal(w_sel, output_loop_trees_3_2$w_sel)
                    expect_equal(w_sel_samples, output_loop_trees_3_2$w_sel_samples)
                    expect_equal(w_count, output_loop_trees_3_2$w_count)
                    expect_equal(w_siam, output_loop_trees_3_2$w_siam)
                    expect_equal(w_siam_full, output_loop_trees_3_2$w_siam_full)
                    expect_equal(w_post$n.rows, output_loop_trees_3_2$w_post$n)
                    expect_equal(w_post_full$n.rows, output_loop_trees_3_2$w_post_full$n)
                    expect_equal(w_star, output_loop_trees_3_2$w_star)
                    expect_equal(w_star_eigen$values, output_loop_trees_3_2$w_star_eigen$values)
                    # expect_equal(w_star_eigen$vectors, output_loop_trees_3_2$w_star_eigen$vectors)
                    expect_equal(w_star_eigen_vals, output_loop_trees_3_2$w_star_eigen_vals)
                    expect_equal(det_q, output_loop_trees_3_2$det_q)
                    expect_equal(y[-missing_indexes], output_loop_trees_3_2$y[-missing_indexes])
                    expect_equal(missing_indexes, output_loop_trees_3_2$missing_indexes)
                    # expect_equal(y_da, output_loop_trees_3_2$y_da)
                })
            }

            set.seed(1)
            residuals <- update_residuals(
                y = y, 
                trees = trees, 
                t = t,
                spatial_theta = spatial_theta,
                missing_indexes = vars$missing_indexes
            ) # residuals

            if(t == 1 && j == 3) {
                it("should return kim's result for first iteration and first tree",{
                    expect_equal(residuals, output_update_residuals_2$residuals)
                })
            }

            set.seed(1)
            result_trees <- sample_trees(
                dt_list = dt_list,
                prob_grow = params$prob_grow,
                prob_change = params$prob_change,
                prob_prune = params$prob_prune,
                sigma2_samples = sigma2_samples,
                j = j,
                sigma_mu = params$sigma_mu,
                t = t,
                residuals = residuals,
                cov_sel_prob = cov_sel_prob,
                obs_list_ind = obs_list_ind,
                x_list = x_list,
                x_unique = x_unique,
                n = params$n,
                alpha = params$alpha,
                beta = params$beta
            ) # dt_list, obs_list_ind

            dt_list <- result_trees$dt_list
            obs_list_ind <- result_trees$obs_list_ind

            if(t == 1 && j == 3){
                it("should return Kim's results, for firt iteration, first tree",{
                        for(i in seq_along(dt_list)) {
                            dt_list1 <- dt_list[[i]]
                            dt_list2 <- output_sample_trees_2$dt_list[[i]]
                        
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
                        expect_equal(obs_list_ind, output_sample_trees_2$obs_list_ind)
                })
            }
            

            set.seed(1)
            result_means <- sample_means(
                sigma2_samples = sigma2_samples,
                sigma_mu = params$sigma_mu,
                obs_list_ind = obs_list_ind,
                residuals = residuals,
                x_cut = x_unique,
                n_available = params$n,
                trees = trees,
                dt_list = dt_list,
                t = t,
                j = j
            ) # trees, dt_list

            trees <- result_means$trees
            dt_list <- result_means$dt_list

            if(t == 1 && j == 3){
                it("should return Kim's results",{
                        for(i in seq_along(dt_list)) {
                            dt_list1 <- dt_list[[i]]
                            dt_list2 <- output_sample_means_2$dt_list[[i]]
                        
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

                        expect_equal(trees, output_sample_means_2$trees)
                })
            }

            

        }

        if(j == 3){
            it("should return a value equal to the one in the kim_output file",{  
                expect_equal(y[-missing_indexes], input_sample_variance_2$y[-missing_indexes])
                expect_equal(trees, input_sample_variance_2$trees)
                expect_equal(spatial_theta, input_sample_variance_2$spatial_theta)
                expect_equal(missing_indexes, input_sample_variance_2$missing_indexes)
                expect_equal(sigma2_samples, input_sample_variance_2$sigma2_samples)
                expect_equal(params$sigma2_a, input_sample_variance_2$sigma2_a)
                expect_equal(params$sigma2_b, input_sample_variance_2$sigma2_b)
                expect_equal(params$n, input_sample_variance_2$n)
            })
        }


        set.seed(1)
        sigma2_samples <- sample_variance(
            y = y,
            trees = trees,
            spatial_theta = spatial_theta,
            missing_indexes = missing_indexes,
            sigma2_a = params$sigma2_a,
            sigma2_b= params$sigma2_b,
            n = params$n,
            sigma2_samples = sigma2_samples,
            j = j
        ) # sigma2_samples

        if(j == 3){
            it("should return a value equal to the one in the kim_output file",{  
                expect_equal(sigma2_samples, output_sample_variance_2$sigma2_samples)
            })
        }

        set.seed(1)
        spatial_theta <- update_spatial_effect(
            y = y,
            trees_pred = trees_pred,
            w_post_full = w_post_full,
            n_locations_all = params$n_locations_all,
            spatial_theta = spatial_theta,
            tau2 = tau2,
            rho = rho,
            sigma2_samples = sigma2_samples,
            j = j
        )  # spatial_theta

        if(j == 3){
            it("should return a value equal to the one in the kim_output file",{  
                expect_equal(spatial_theta, output_update_spatial_effect_2$spatial_theta)
            })
        }

        set.seed(1)
        results_tau2 <- update_tau2(
            w_post_full = w_post_full,
            n_locations_all = params$n_locations_all,
            spatial_theta = spatial_theta,
            rho = rho,
            tau2_b = params$tau2_b,
            tau2_posterior_shape = params$tau2_posterior_shape,
            tau2_samples = tau2_samples,
            j = j
        ) # tau2_samples, temp

        tau2_samples <- results_tau2$tau2_samples
        tau2 <- tau2_samples[j]
        temp <- results_tau2$temp

        if(j == 3){
            it("should return a value equal to the one in the kim_output file",{  
                expect_equal(tau2_samples, output_update_tau_2$tau2_samples)
                expect_equal(temp, output_update_tau_2$temp)
            })
        }

        set.seed(1)
        results_rho <- update_rho(
            rho = rho,
            proposal_sd_rho = params$proposal_sd_rho,
            w_post_full = w_post_full,
            n_locations_all = params$n_locations_all,
            spatial_theta = spatial_theta,
            tau2 = tau2,
            w_star_eigen_vals = w_star_eigen_vals,
            det_q = det_q,
            temp = temp,
            rho_samples = rho_samples,
            j = j
        ) # rho_samples, temp, det_q

        rho_samples <- results_rho$rho_samples
        rho <- rho_samples[j]
        temp <- results_rho$temp
        det_q <- results_rho$det_q

        if(j==3){
            it("should return a value equal to the one in the kim_output file",{  
                expect_equal(rho_samples, output_update_rho_2$rho_samples)
                expect_equal(temp, output_update_rho_2$temp)
                expect_equal(det_q, output_update_rho_2$det_q)
            })
        }

        set.seed(1)
        results_f <- update_f(
            ws = data$ws,
            w_count = w_count,
            siam = data$wind_matrix,
            n_locations_all = params$n_locations_all,
            spatial_theta = spatial_theta,
            rho = results_rho$rho_samples[j],
            tau2 = results_tau2$tau2_samples[j],
            det_q = results_rho$det_q,
            temp = results_tau2$temp,
            w_sel = w_sel,
            w_siam_full = w_siam_full,
            w_post_full = w_post_full,
            w_star = w_star,
            w_star_eigen = w_star_eigen,
            w_star_eigen_vals = w_star_eigen_vals,
            w_sel_samples = w_sel_samples,
            j = j
        ) # w_sel_samples, det_q, w_siam_full, w_post_full, w_star, w_star_eigen, w_star_eigen_vals

        w_sel_samples <- results_f$w_sel_samples
        det_q <- results_f$det_q
        w_siam_full <- results_f$w_siam_full
        w_post_full <- results_f$w_post_full
        w_star <- results_f$w_star
        w_star_eigen <- results_f$w_star_eigen
        w_star_eigen_vals <- results_f$w_star_eigen_vals

        if(j == 3){
            it("should return a value equal to the one in the kim_output file",{  
                expect_equal(w_sel_samples, output_update_f_2$w_sel_samples)
                expect_equal(det_q, output_update_f_2$det_q)
                expect_equal(w_siam_full, output_update_f_2$w_siam_full)

                W_post_full2 <- output_update_f_2$w_post_full
                W_post_full2$n.rows <- output_update_f_2$w_post_full$n
                W_post_full2$n <- NULL

                expect_equal(w_post_full, W_post_full2)
                expect_equal(w_star, output_update_f_2$w_star)
                expect_equal(w_star_eigen$values, output_update_f_2$w_star_eigen$values)
                # expect_equal(w_star_eigen$vectors, output_update_f_2$w_star_eigen$vectors)
                expect_equal(w_star_eigen_vals, output_update_f_2$w_star_eigen_vals)
            })
        }

        set.seed(1)
        result_dirichlet <- update_dirichlet_alpha(
            dt_list = dt_list,
            p = params$p,
            j = j,
            warmup = 1000L,
            dirichlet_alpha = dirichlet_alpha,
            a0 = params$a0,
            b0 = params$b0,
            cov_sel_prob = cov_sel_prob
        ) # cov_sel_prob, rules_count, dirichlet_alpha, posterior_dirichlet_alpha

        cov_sel_prob <- result_dirichlet$cov_sel_prob
        rules_count <- result_dirichlet$rules_count
        dirichlet_alpha <- result_dirichlet$dirichlet_alpha
        posterior_dirichlet_alpha <- result_dirichlet$posterior_dirichlet_alpha

        if(j == 3){
            it("should return a value equal to the one in the kim_output file",{  
                expect_equal(cov_sel_prob, output_update_dirichlet_alpha_2$cov_sel_prob)
                expect_equal(rules_count, output_update_dirichlet_alpha_2$rules_count)
                expect_equal(dirichlet_alpha, output_update_dirichlet_alpha_2$dirichlet_alpha)
                expect_equal(posterior_dirichlet_alpha, output_update_dirichlet_alpha_2$posterior_dirichlet_alpha)
            })
        }

        set.seed(1)
        trees_pred <- matrix(unlist(sapply(1:n_trees, function(x) mean_predict(dt_list[[x]], x_list, x_mult, x_unique, params$n_locations_all))), nrow = params$n_locations_all, ncol = n_trees)

        if(j == 3){
            it("should return a value equal to the one in the kim_output file",{  
                expect_equal(trees_pred, output_predict_2$trees_pred)
            })
        }

        cov_sel_temp<-ifelse(rules_count > 0, 1, 0)
        cov_sel <- rbind(cov_sel, cov_sel_temp)

        y_da[,j-1] <- rnorm(length(missing_indexes), c(rowSums(trees_pred)+spatial_theta)[missing_indexes], sqrt(sigma2_samples[j]))
        y[missing_indexes] <- y_da[,j-1]
    }
})