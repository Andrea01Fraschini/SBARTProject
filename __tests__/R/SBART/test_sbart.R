# rm(list = ls())
# setwd("C:/Users/camil/OneDrive - Universidad del Norte/Universidad POLIMI/Bayesian stats/CODE Bart/SBARTProject")
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

# ALL the rest will be unitary, this one will verify final result but also each step based on outputs

# set.seed(1)
#     data <- sample_data() # Xpred, Y, mis.ind, Ws, wind_mat
    
#     set.seed(1)
#     params <- init_model_parameters(
#         X = data$Xpred,
#         Y = data$Y,
#         SIAM = data$wind_mat,
#         n.trees = 50L
#     ) # p, n, n.locations.all, prob.grow, prob.prune, prob.change, alpha, beta, dirichlet.alpha, posterior.dirichlet.alpha, cov.sel_prob, tau2.a, tau2.b, tau2.posterior.shape, tau2, proposal.sd.rho, rho, a0, b0, Y, residuals, sigm2, nu, lambda, sigma2.a, sigma2.b, sigma_mu, missing_indexes

#     set.seed(1)
#     vars <- init_chain(
#         n.iterations = 10000L,
#         n.locations.all = params$n.locations.all,
#         p = params$p,
#         n = params$n,
#         n.trees = 50L,
#         X = data$Xpred,
#         missing_indexes = data$mis.ind,
#         SIAM = data$wind_mat,
#         W = data$Ws,
#         Y = params$Y,
#         rho = params$rho
#     ) # sigma2.samples, rho.samples, tau2.samples, spatial_theta, cov_sel, obs_list.ind, dt_list, trees, trees.pred, Xlist, Xmult, X.unique, W_sel, W_sel.samples, W.count, W.siam, W.siam.full, W.post, W.post.full, Wstar, Wstar.eigen, Wstar.eigen_vals, det.Q, Y, missing_indexes, Y.da

#     trees <- vars$trees
#     dt_list <- vars$dt_list
#     obs_list.ind <- vars$obs_list.ind

#     for(t in 1:50){

#         residuals <- update_residuals(
#             Y = vars$Y, 
#             trees = trees, 
#             t = t,
#             spatial_theta = vars$spatial_theta,
#             missing_indexes = vars$missing_indexes
#         ) # residuals

#         set.seed(1)
#         trees_output <- sample_trees(
#             dt_list = dt_list,
#             prob.grow = params$prob.grow,
#             prob.change = params$prob.change,
#             prob.prune = params$prob.prune,
#             sigma2.samples = vars$sigma2.samples,
#             j = 2,
#             sigma_mu = params$sigma_mu,
#             t = t,
#             residuals = residuals,
#             cov.sel_prob = params$cov.sel_prob,
#             obs_list.ind = obs_list.ind,
#             Xlist = vars$Xlist,
#             X.unique = vars$X.unique,
#             n = params$n,
#             alpha = params$alpha,
#             beta = params$beta
#         ) # dt_list, obs_list.ind

#         dt_list <- trees_output$dt_list
#         obs_list.ind <- trees_output$obs_list.ind

#         set.seed(1)
#         means <- sample_means(
#             sigma2 = vars$sigma2.samples,
#             sigma_mu = params$sigma_mu,
#             obs = obs_list.ind,
#             residuals = residuals,
#             xcut = vars$X.unique,
#             n.available = params$n,
#             trees = trees,
#             dt_list = dt_list,
#             t = t,
#             j = 2
#         ) # trees, dt_list

#         trees <- means$trees
#         dt_list <- means$dt_list

#     }

#     set.seed(1)
#     sigma2.samples <- sample_variance(
#         Y = vars$Y,
#         trees = means$trees,
#         spatial_theta = vars$spatial_theta,
#         missing_indexes = vars$missing_indexes,
#         sigma2.a = params$sigma2.a,
#         sigma2.b = params$sigma2.b,
#         n = params$n,
#         sigma2.samples = vars$sigma2.samples,
#         j = 2
#     ) # sigma2.samples


#     set.seed(1)
#     spatial_theta <- update_spatial_effect(
#         Y = vars$Y,
#         trees_pred = vars$trees.pred,
#         W.post.full = vars$W.post.full,
#         n_locations_all = params$n.locations.all,
#         spatial_theta = vars$spatial_theta,
#         tau2 = params$tau2,
#         rho = params$rho,
#         sigma2_samples = sigma2.samples,
#         j = 2
#     ) # spatial_theta

#     update_tau_results <- update_tau2(
#         W.post.full = vars$W.post.full,
#         n_locations_all = params$n.locations.all,
#         spatial_theta = spatial_theta,
#         rho = params$rho,
#         tau2_b = params$tau2.b,
#         tau2_posterior_shape = params$tau2.posterior.shape,
#         tau2.samples = vars$tau2.samples,
#         j = 2
#     ) # tau2.samples, temp

#     set.seed(1)
#     rho_update_results <- update_rho(
#         rho = params$rho,
#         proposal_sd_rho = params$proposal.sd.rho,
#         W_post_full = vars$W.post.full,
#         n_locations_all = params$n.locations.all,
#         spatial_theta = spatial_theta,
#         tau2 = params$tau2,
#         Wstar_eigen_vals = vars$Wstar.eigen_vals,
#         det_Q = vars$det.Q,
#         temp = update_tau_results$temp,
#         rho_samples = vars$rho.samples,
#         j = 2
#     ) # rho.samples, temp, det.Q

#     f_update_results <- update_f(
#         W = data$Ws,
#         W_count = vars$W.count,
#         SIAM = data$wind_mat,
#         n_locations_all = params$n.locations.all,
#         spatial_theta = spatial_theta,
#         rho = rho_update_results$rho.samples[2],
#         tau2 = update_tau_results$tau2.samples[2],
#         det_Q = rho_update_results$det.Q,
#         temp = update_tau_results$temp,
#         W_sel = vars$W_sel,
#         W_siam_full = vars$W.siam.full,
#         W_post_full = vars$W.post.full,
#         Wstar = vars$Wstar,
#         Wstar_eigen = vars$Wstar.eigen,
#         Wstar_eigen_vals = vars$Wstar.eigen_vals,
#         W_sel.samples = vars$W_sel.samples,
#         j = 2
#     ) # W_sel.samples, det.Q, W.siam.full, W.post.full, Wstar, Wstar.eigen, Wstar.eigen_vals

#     cov.sel_prob <- update_dirichlet_alpha(
#         dt_list = dt_list,
#         p = params$p,
#         j = 2,
#         warmup = 1000L,
#         dirichlet_alpha = params$dirichlet.alpha,
#         a0 = params$a0,
#         b0 = params$b0,
#         cov_sel_prob = params$cov.sel_prob
#     ) # cov.sel_prob