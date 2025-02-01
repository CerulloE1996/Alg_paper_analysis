

#### ------- R function to repare data for Stan pilot studies + benchmarks : -------------------------------------------------------------------------

R_fn_prep_Stan_binary_data <- function(  global_list,
                                    fully_vectorised = 1,
                                    handle_numerical_issues = 1,
                                    prior_only = 0) {

                # fully_vectorised <- 1  
                # #### GHK_comp_method <- 2
                # handle_numerical_issues <- 1 
                # ##
                # prior_only <-  0
                
                ## Make Stan data lists for each N:
                Stan_data_list <- list()
                
                N_datasets <- global_list$N_datasets
                  
                for (df_index in 1:N_datasets) {
                  
                           ## Extract quantities needed from the global list:
                           Model_settings_list <-  global_list$Model_settings_list
                           n_tests <- Model_settings_list$n_tests
                           n_class <- Model_settings_list$n_class
                           n_pops <- Model_settings_list$n_pops
                           n_covariates_per_outcome_mat <- Model_settings_list$n_covariates_per_outcome_mat
                           overflow_threshold <- Model_settings_list$overflow_threshold
                           underflow_threshold <- Model_settings_list$underflow_threshold
                           Phi_type_int <- Model_settings_list$Phi_type_int
                           ##
                           priors_list <-  global_list$priors_list
                           corr_force_positive <- priors_list$prior_Omega_list$corr_force_positive
                           a_priori_known_corrs <- priors_list$prior_Omega_list$a_priori_known_corrs
                           ##
                           N_sample_sizes_vec <- global_list$N_sample_sizes_vec
                           y_binary_list <- global_list$data_sim_outs$y_binary_list
                           ##
                           n_covariates_max_nd <- Model_settings_list$n_covariates_max_nd
                           n_covariates_max_d <-  Model_settings_list$n_covariates_max_d
                           n_covariates_max <-    Model_settings_list$n_covariates_max
                           ## n_covariates <- Model_settings_list$n_covariates
                           ##
                           prior_beta_mean <- array(dim = c(n_class, n_tests, n_covariates_max))
                           prior_beta_mean[,,1] <-  t(priors_list$prior_beta_list$prior_beta_mean_vec)
                           prior_beta_sd <- array(dim = c(n_class, n_tests, n_covariates_max))
                           prior_beta_sd[,,1] <-  t(priors_list$prior_beta_list$prior_beta_sd_vec)
                           {
                             ## As lists of mats (needed for C++ manual-grad models:
                             prior_beta_mean_as_list <- prior_beta_sd_as_list <- list()
                             for (c in 1:n_class) {
                               prior_beta_mean_as_list[[c]] <- matrix(prior_beta_mean[c,,], ncol = n_tests, nrow = n_covariates_max)
                               prior_beta_sd_as_list[[c]] <-   matrix(prior_beta_sd[c,,],   ncol = n_tests, nrow = n_covariates_max)
                             }
                           }
                           ##
                           prior_LKJ <- priors_list$prior_Omega_list$prior_lkj_cholesky_eta
                           ##
                           prior_prev_alpha <- priors_list$prior_prev_list$prior_prev_alpha
                           prior_prev_beta <- priors_list$prior_prev_list$prior_prev_beta
                           ##
                           ## "Data" which depens on N:
                           N <- N_sample_sizes_vec[df_index]
                           y <- y_binary_list[[df_index]]
                           pop_group <- rep(1, N)
                           
                           X_nd <- list()
                           X_d <- list()
                           
                           for (t in 1:n_tests) {
                             X_nd[[t]] <- matrix(data = 1, nrow = N, ncol = n_covariates_max_nd)
                             X_d[[t]] <-  matrix(data = 1, nrow = N, ncol = n_covariates_max_d)
                           }
             
                           
                           Stan_data_list[[df_index]] <- list(    N = N,
                                                                  n_tests = n_tests,
                                                                  y = y,
                                                                  n_class = n_class,
                                                                  n_pops = n_pops,
                                                                  pop = pop_group,
                                                                  n_covariates_max_nd = n_covariates_max_nd,
                                                                  n_covariates_max_d = n_covariates_max_d,
                                                                  n_covariates_max = n_covariates_max,
                                                                  X_nd = X_nd,
                                                                  X_d = X_d,
                                                                  n_covs_per_outcome = n_covariates_per_outcome_mat,
                                                                  corr_force_positive = corr_force_positive,
                                                                  known_num = a_priori_known_corrs,
                                                                  overflow_threshold = overflow_threshold,
                                                                  underflow_threshold = underflow_threshold,
                                                                  ## ///// priors:
                                                                  prior_only = prior_only,
                                                                  prior_beta_mean = prior_beta_mean_as_list,
                                                                  prior_beta_sd = prior_beta_sd_as_list,
                                                                  prior_LKJ = matrix(prior_LKJ, ncol = 1),
                                                                  prior_p_alpha = matrix(rep(prior_prev_alpha, n_pops), ncol = 1),
                                                                  prior_p_beta =  matrix(rep(prior_prev_beta, n_pops), ncol = 1),
                                                                  ## ///// other:
                                                                  Phi_type = Phi_type_int, ## //// 1 for "Phi" and 2 for "Phi_approx"
                                                                  handle_numerical_issues = handle_numerical_issues,
                                                                  fully_vectorised = fully_vectorised)
                           
                           # Stan_data_list[[df_index]] <-  Stan_data_list_single
                           
                }
                  
                return(Stan_data_list)
                
}

      
    
 

 
   
    # # -------------------------------------------------------------------------
    # u_initial <- array(0.01, dim = c(N, n_tests))
    # 
    # km1_choose_2 = 0.5 * (n_tests - 2) * (n_tests - 1)
    # known_num = 0
    # 
    # beta_vec_init <- rep(0, n_class * n_tests)
    # beta_vec_init[1:n_tests] <- - 1   # tests 1-T, class 1 (D-)
    # beta_vec_init[(n_tests + 1):(2*n_tests)] <- + 1   # tests 1-T, class 1 (D+)
    # 
    # init = list(
    #   u_raw = (u_initial),
    #   p_raw =  (-0.6931472), # equiv to 0.20 on p
    #   beta_vec = beta_vec_init,
    #   off_raw = array(0.01, dim = c(n_class, km1_choose_2 - known_num)),
    #   col_one_raw =   array(0.01, dim = c(n_class, n_tests - 1))
    # )
    # 
    #  
    # 
  

    

 