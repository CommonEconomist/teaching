# ############################################################################
# (c) Chancellery of the Prime Minister 2012-2015                            #
#                                                                            #
# Authors: Grzegorz Klima, Karol Podemski,                                   #
#          Kaja Retkiewicz-Wijtiwiak, Anna Sowińska                          #
# ############################################################################
# DSGE model based on Smets Wouters (2003), with corrected equations:
# - (31) The correct loglinearised law of motion for the capital
#        should be written as: K[] = (1 - tau) * K[-1] + tau * I[].
# - (35) The goods market equilibrium condition should be written as:
#        Y_P[] = (1 - tau * k_Y - g_Y) * C[] + tau * k_Y * I[] +
#                g_Y * epsilon_G[] + k_Y * r_k_bar * r_k[] * psi
#        In the [SW'03] the last term accounting for
#        the cost of capacity utilisation was missing.
# The shock $\eta^Q_t$ from equation (30) has not been introduced.
# ############################################################################

# load gEcon package
library(gEcon)

# make and load the model
sw_gecon <- make_model("SW_03.gcn")

# set initial values
initv <- list(z = 1, z_f = 1, Q = 1, Q_f = 1, pi = 1, 
              pi_obj = 1, epsilon_b = 1, epsilon_L = 1, 
              epsilon_I = 1, epsilon_a = 1, epsilon_G = 1,
              r_k = 0.01, r_k_f = 0.01)
sw_gecon <- initval_var(sw_gecon, init_var = initv)

# find and print steady-state values
sw_gecon <- steady_state(sw_gecon)
get_ss_values(sw_gecon, to_tex = TRUE)

# find and print perturbation solution
sw_gecon <- solve_pert(sw_gecon, loglin = TRUE)
get_pert_solution(sw_gecon, to_tex = TRUE)

# set the shock distribution parameters
variances <- c(eta_b = 0.336 ^ 2, eta_L = 3.52 ^ 2, eta_I = 0.085 ^ 2, 
               eta_a = 0.598 ^ 2, eta_w = 0.6853261 ^ 2, eta_p = 0.7896512 ^ 2,
               eta_G = 0.325 ^ 2, eta_R = 0.081 ^ 2, eta_pi = 0.017 ^ 2)
sw_gecon  <- set_shock_cov_mat(sw_gecon, 
                               cov_matrix = diag(variances), 
                               shock_order = names(variances))
shock_info(sw_gecon, all = TRUE)

# compute and print correlations
sw_gecon <- compute_model_stats(sw_gecon)
get_model_stats(sw_gecon, 
                variables = c("q", "pi", "r_k",
                              "z", "C", "G", "I", "K", "L",
                              "Q", "R", "W", "T", "Y"),
                to_tex = TRUE)

# compute and print the IRFs 
sw_gecon_irf <- compute_irf(sw_gecon, variables = c("C", "Y", "K", "I", "L"),
                            chol = T, shocks = c("eta_a", "eta_R"), 
                            sim_length = 40)
plot_simulation(sw_gecon_irf, to_eps = TRUE)
