# ############################################################################
# (c) Karol Podemski 2016                                                    #
#                                                                            #
# Authors: Karol Podemski                                                    #
# ############################################################################
# Example for gEcon.estimation package: estimation of log-linearised 
# SW'03 model.
# 
# In contrast to SW_03_est.R, in this script log-linearized model
# with normalization of shocks is estimated (similiar to original paper). 
# The magnitude of shock standard deviations is hence comparable 
# to paper values.
# ############################################################################

# load gEcon.estimation package
library(gEcon.estimation)

# create and solve the model
SW_03 <- make_model("SW_03_loglin_est.gcn")

SW_03 <- steady_state(SW_03)

SW_03 <- solve_pert(SW_03, loglin = TRUE)

SW_03  <- set_shock_cov_mat(SW_03, shock_order = c("eta_G", "eta_I", "eta_L",
                                                   "eta_R", "eta_a",
                                                   "eta_b", "eta_p", "eta_pi",
                                                   "eta_w") ,
                                   cov_matrix=diag(c(eta_G = (0.325 ^ 2),
                                                    eta_I = (0.085 ^ 2),
                                                    eta_L = (3.52 ^ 2),
                                                    eta_R = (0.081 ^ 2),
                                                    eta_a = (0.598 ^ 2),
                                                    eta_b = (0.336 ^ 2),      
                                                    eta_p = (0.7896512 ^ 2),
                                                    eta_pi = (0.017 ^ 2),
                                                    eta_w = (0.6853261 ^ 2)) 
                                                  )
                            )

# declare prior distribution for the model parameters
SW_03_prior <- gecon_prior(prior_list = list(
                            list(par = "sd(eta_a)", type = "inv_gamma",
                                 mean = 0.4, sd = 2, lower_bound = 0.3, upper_bound  = 0.9, initial = 0.437),
                            list(par = "sd(eta_pi)", type = "inv_gamma",
                                 mean = 0.02, sd = 2, lower_bound = 0.0001, upper_bound  = 0.2, initial = 0.009),
                            list(par = "sd(eta_b)", type = "inv_gamma",
                                 mean = 0.2, sd = 2, lower_bound = 0.01, upper_bound  = 0.7, initial = 0.073),
                            list(par = "sd(eta_G)", type = "inv_gamma",
                                 mean = 0.3, sd = 2, lower_bound = 0.1, upper_bound  = 2, initial = 0.355),
                            list(par = "sd(eta_L)", type = "inv_gamma",
                                 mean = 1, sd = 2, lower_bound = 1, upper_bound  = 6, initial = 3.781),
                            list(par = "sd(eta_I)", type = "inv_gamma",
                                 mean = 0.1, sd = 2, lower_bound = 0.01, upper_bound  = 2, initial = 0.486),
                            list(par = "sd(eta_R)", type = "inv_gamma",
                                 mean = 0.1, sd = 2, lower_bound = 0.01, upper_bound  = 0.7, initial = 0.045),
                            list(par = "sd(eta_p)", type = "inv_gamma",
                                 mean = 0.15, sd = 2, lower_bound = 0.01, upper_bound  = 0.4, initial = 0.157),
                            list(par = "sd(eta_w)", type = "inv_gamma",
                                 mean = 0.25, sd = 2, lower_bound = 0.01, upper_bound  = 0.4, initial = 0.309),
                            list(par = "rho_a", type = "beta",
                                 mean = 0.85, sd = 0.1, lower_bound = 0.1, upper_bound  = 0.9999, initial = 0.990),
                            list(par = "rho_pi_bar", type = "beta",
                                 mean = 0.85, sd = 0.1, lower_bound = 0.1, upper_bound  = 0.999, initial = 0.927),
                            list(par = "rho_b", type = "beta",
                                 mean = 0.85, sd = 0.1, lower_bound = 0.1, upper_bound  = 0.99, initial = 0.813),
                            list(par = "rho_G", type = "beta",
                                 mean = 0.85, sd = 0.1, lower_bound = 0.1, upper_bound  = 0.9999, initial = 0.977),
                            list(par = "rho_L", type = "beta",
                                 mean = 0.85, sd = 0.1, lower_bound = 0.1, upper_bound  = 0.9999, initial = 0.894),
                            list(par = "rho_I", type = "beta",
                                 mean = 0.85, sd = 0.1, lower_bound = 0.1, upper_bound  = 0.99, initial = 0.441),
                            list(par = "varphi", type = "normal",
                                 mean = 4, sd = 1.5, lower_bound = 1, upper_bound  = 15, initial = 6.388),
                            list(par = "sigma_c", type = "normal",
                                 mean = 1, sd = 0.375, lower_bound = 0.25, upper_bound  = 3, initial = 1.416),
                            list(par = "h", type = "beta",
                                 mean = 0.7, sd = 0.1, lower_bound = 0.3, upper_bound  = 0.95, initial = 0.574),
                            list(par = "sigma_l", type = "normal",
                                 mean = 2, sd = 0.75, lower_bound = 0.5, upper_bound  = 5, initial = 2.690),
                            list(par = "xi_e", type = "beta",
                                 mean = 0.5, sd = 0.15, lower_bound = 0.1, upper_bound  = 0.95, initial = 0.490),
                            list(par = "psi", type = "normal",
                                 mean = 0.2, sd = 0.075, lower_bound = 0.01, upper_bound  = 2, initial = 0.309),
                            list(par = "xi_w", type = "beta",
                                 mean = 0.75, sd = 0.05, lower_bound = 0.3, upper_bound  = 0.9, initial = 0.825),
                            list(par = "xi_p", type = "beta",
                                 mean = 0.75, sd = 0.05, lower_bound = 0.3, upper_bound  = 0.95, initial = 0.888),
                            list(par = "gamma_w", type = "beta",
                                 mean = 0.75, sd = 0.15, lower_bound = 0.1, upper_bound  = 0.99, initial = 0.867),
                            list(par = "gamma_p", type = "beta",
                                 mean = 0.75, sd = 0.15, lower_bound = 0.1, upper_bound  = 0.99, initial = 0.421),
                            list(par = "r_pi", type = "normal",
                                 mean = 1.7, sd = 0.1, lower_bound = 1.2, upper_bound  = 2, initial = 1.694),
                            list(par = "r_Delta_pi", type = "normal",
                                 mean = 0.3, sd = 0.1, lower_bound = 0.01, upper_bound  = 0.5, initial = 0.260),
                            list(par = "rho", type = "beta",
                                 mean = 0.8, sd = 0.1, lower_bound = 0.5, upper_bound  = 0.99, initial = 0.876),
                            list(par = "r_Y", type =  "normal",
                                 mean = 0.125, sd = 0.05, lower_bound = 0.01, upper_bound  = 0.2, initial = 0.114),
                            list(par = "r_Delta_Y", type = "normal",
                                 mean = 0.0625, sd = 0.05, lower_bound = 0.05, upper_bound  = 0.5, initial = 0.185)
                            ),
                            model = SW_03
                        )

# load model data
raw_data <- read.csv(file = "SW_03_data.csv", row.names = NULL)
SW_03_data <- ts(data = raw_data, 
                 start = c(1970, 3), 
                 frequency = 4)
SW_03_estimation_data <- window(SW_03_data, 
                                start = c(1980, 2), 
                                end = c(1999,4))
                                
observables_names <- c("C", "Emp", "I" , "pi",
                       "R", "W", "Y")

# estimate the model
estimation_result <- bayesian_estimation(model = SW_03,
                                         prior = SW_03_prior,
                                         data_set = SW_03_estimation_data, 
                                         mcmc_options_list = list(chain_length = 10000, 
                                                                  burn = 2000,
                                                                  scale = rep(0.3, 30),
                                                                  cores = 3, chains = 3),
                                         observables =   observables_names)

plot_posterior(estimation_result, bw_adjust = 1.5, to_eps = TRUE)

est_par <- get_estimated_par(estimation_result)
free_par <- est_par$free_par
shock_distr_par <- est_par$shock_distr_par

# update the model
estimated_SW_03 <- set_free_par(SW_03, free_par = free_par)
estimated_SW_03 <- set_shock_distr_par(estimated_SW_03, distr_par = shock_distr_par)

# solve the updated model
estimated_SW_03 <- steady_state(estimated_SW_03)
estimated_SW_03 <- solve_pert(estimated_SW_03, loglin = TRUE)


# forecast based on data
sw_forecast <- forecast_posterior(est_results = estimation_result, 
                                  posterior_sample = 1000,
                                  data_set = SW_03_estimation_data, 
                                  variables = observables_names, 
                                  observables = observables_names, 
                                  horizon = 4)

# plot forecasts
plot_forecast(sw_forecast, to_eps = TRUE)

# perform and plot historical shock decomposition
dsge_shock_decomposition <- shock_decomposition(model = estimated_SW_03, 
                                                data_set =  window(SW_03_estimation_data,
                                                                   start = c(1990, 1), 
                                                                   end = c(1999,4)), 
                                                observables =  observables_names, 
                                                variables = c("K", "I", "Y"))
plot_shock_decomposition(dsge_shock_decomposition, to_eps = TRUE)
