

# - | ----------  run models using Stan --------------------------------------------------------------------
 

{
 
  if (N == 500)     dataset_index = 1
  if (N == 1000)    dataset_index = 2
  if (N == 2500)    dataset_index = 3
  if (N == 5000)    dataset_index = 4
  if (N == 12500)   dataset_index = 5
  if (N == 25000)   dataset_index = 6
  
  y = y_master_list_seed_123_datasets[[dataset_index]]
  
  
  N_sims <- 123
  
  
  model_type <- "LC_MVP"
 
   fully_vectorised <- 1  ;   GHK_comp_method <- 2 ;       handle_numerical_issues <- 1 ;     overflow_threshold <- +5 ;    underflow_threshold <- -5  #  MAIN MODEL SETTINGS 

 
   # Phi_type <- 2 # using Phi_approx and inv_Phi_approx - in  Stan these are slower than Phi() and inv_Phi() !!!!!!!! (as log/exp are slow)
    Phi_type <- 1 # using Phi and inv_Phi
  
 
    corr_param <- "Sean" ; prior_lkj <- c(12, 3) ;  corr_prior_beta =  0 ;  corr_force_positive <-  0  ;  prior_param_3 <- 0 ; uniform_indicator <- 0 
  #  corr_param <- "Sean" ; prior_lkj <- c(10, 2) ;  corr_prior_beta =  0 ;  corr_force_positive <-  0  ;  prior_param_3 <- 0 ; uniform_indicator <- 0 

  CI <- 0
  
  
  prior_only <-  0
  
  mvr_cholesky <- 0 # dont even need to use LKJ anymore thanks to generalising it with skewed-LKJ / shifted beta ??!?!?
  
  corr_prior_norm = 0

  
  
  gamma_indicator <-  0
  skew_norm_indicator <- 0


  
  prior_lkj_skewed_diseased <-   prior_lkj
  prior_lkj_skewed_non_diseased <-  prior_lkj
  
  
  
  #  tailored_corr_priors <- TRUE # priors which are more consistent with posteior (should recover parameters better, especially for lower N and in the smaller latent class)
  tailored_corr_priors <- FALSE # priors which are more consistent with posteior (should recover parameters better, especially for lower N and in the smaller latent class)
  
  
  n_class <- 2
  n_covariates <- 0
  n_tests <-   n_tests 
  n_ordinal_tests <- 0
  n_binary_tests <- n_tests
  Thr =   rep(1, n_tests)
  #N <- nrow(y)
  
  prior_a_mean <-   array(c(0,rep(0, n_tests-1),
                            0,rep(0, n_tests-1)), dim = c(n_class, n_tests, n_covariates+1))
  prior_a_sd  <-    array(c(1,rep(1, n_tests-1),
                            1,rep(1, n_tests-1)), dim = c(n_class, n_tests, n_covariates+1))
  
  
  #prior_a_mean[c,t,cov]
  
  
  
  # intercepts / coeffs prior means
  prior_a_mean[1,1,1] <- -2.10
  prior_a_sd[1,1,1] <- 0.45
  
  prior_a_mean[2,1,1] <- +0.40
  prior_a_sd[2,1,1] <-  0.375
  
  
  # prior_a_mean[,,] <- 0
  # prior_a_mean[1,1:3,1] <- - 3.09 # Sp prior mean ~ Wang et al data 
  
  prior_for_skewed_LKJ_a <- array(0, dim = c(n_tests, n_tests, n_class)) # a > b for positive-skew
  prior_for_skewed_LKJ_b <- array(0, dim = c(n_tests, n_tests, n_class))
  
  bs_to_set_to_0 = array(0, dim = c(n_class, n_tests)) # for LT model
   
  
  prior_b_shape <- prior_b_scale <- array(dim = c(n_class, n_tests))
  prior_b_shape[1,] <- prior_b_shape_nd  # D-
  prior_b_scale[1,] <- prior_b_scale_nd  # D-
  prior_b_shape[2,] <- prior_b_shape_d  # D+
  prior_b_scale[2,] <- prior_b_scale_d  # D+
  
 
  
  
  n_pops <- 1
  
  
  group <- rep(1, N)
  
  
  stan_data_list <- list()
  
  
  n_covs_per_outcome = array(1, dim = c(n_class, n_tests))
  
  
    n_covariates_total_nd  =    (sum( (n_covs_per_outcome[1,])));
    n_covariates_total_d   =     (sum( (n_covs_per_outcome[2,])));
    n_covariates_total  =       n_covariates_total_nd + n_covariates_total_d;
  
    k_choose_2   = (n_tests * (n_tests - 1)) / 2;
    km1_choose_2 =  ((n_tests - 1) * (n_tests - 2)) / 2;
  
  for (i in 1:N_sims) {
    
    
    # if (prior_only == 0 ) { 
    stan_data = list(  N =   (N), # length(corr_prior_dist_LKJ_4), #  N, 
                       D = n_tests, 
                       rough_approx = 0, 
                       prior_param_3 = prior_param_3,
                       uniform_indicator = uniform_indicator,
                       gamma_indicator = gamma_indicator,
                       skew_norm_indicator = skew_norm_indicator,
                       corr_force_positive = corr_force_positive,
                       mvr_cholesky = mvr_cholesky,
                       n_tests = n_tests,
                       n_binary_tests = n_binary_tests,
                       n_ordinal_tests = n_ordinal_tests, 
                       n_class = 2,
                       vector_classes = seq(1:2),
                       prior_beta_mean =   prior_a_mean ,
                       prior_beta_sd  =   prior_a_sd ,
                       prior_a_mean =   prior_a_mean ,
                       prior_a_sd  =   prior_a_sd ,
                       prior_b_shape = prior_b_shape,
                       prior_b_scale = prior_b_scale,
                       prior_for_corr_a = prior_for_skewed_LKJ_a, # array(4, dim = c(n_class, n_tests, n_tests)), 
                       prior_for_corr_b = prior_for_skewed_LKJ_b, # array(3, dim = c(n_class, n_tests, n_tests)),
                       bs_to_set_to_0 = bs_to_set_to_0, # for latent trait
                       prior_LKJ = prior_lkj, 
                       prior_lkj = prior_lkj, 
                       prior_cons_corr_mean = rep(0, n_class), 
                       prior_cons_corr_sd = rep(0.25, n_class),
                       prior_ind_dirichlet_array =    array(1,dim = c(n_class, n_ordinal_tests, (max(Thr)+1))),
                       prior_anchor_point = array(0, dim = c(n_class, n_ordinal_tests)),
                       Thr =   Thr, # item-specific
                       ###    y = (y_list[[i]]),
                       y = (y_master_list_seed_123_datasets[[dataset_index]]),
                       homog_corr = 0,
                       cons_tests_corr = 0,
                       perfect_gs = 0,
                       prior_only = prior_only, ######
                       alpha_classes = c(1,1),
                       n_pops = 1, #  n_pops,
                       pop = group,
                       prior_alpha_classes = array(1, dim = c(n_pops, n_class)),
                       prior_p_alpha = array(rep(5, n_pops)),
                       prior_p_beta =  array(rep(10, n_pops)),
                       CI = CI,
                       induced_dir = 1,
                       K = 1,
                       x = t(array(rep(1, N), dim = c(N, 1))),
                       prior_cutpoint_sd = 5,
                       corr_pareto_alpha = 2,
                       corr_dist_mean = -1, 
                       corr_dist_sd = 2,
                       boundary_for_rough_approx = 5,
                       lb = -1, 
                       ub = 1,
                       known_num = 0,
                       corr_prior_beta = corr_prior_beta,
                       corr_prior_norm = corr_prior_norm,
                       X_nd = array(1, dim = c(n_tests, 1, N)), 
                       X_d = array(1, dim = c(n_tests, 1, N)), 
                        n_covs_per_outcome = n_covs_per_outcome, 
                       n_covariates_total_nd = n_covariates_total_nd,
                       n_covariates_total_d = n_covariates_total_d,
                       n_covariates_total = n_covariates_total,
                       k_choose_2 = k_choose_2,
                       km1_choose_2 = km1_choose_2,
                       n_covariates_max_nd = 1,
                       n_covariates_max_d = 1,
                       n_covariates_max = 1,
                       handle_numerical_issues = handle_numerical_issues,
                       fully_vectorised = fully_vectorised,
                       GHK_comp_method = GHK_comp_method,
                       overflow_threshold = overflow_threshold,
                       underflow_threshold = underflow_threshold,
                       Phi_type = Phi_type
    )
    
    
    
    if (model_type == "LT") {
      stan_data$prior_a_mean <-  prior_a_mean[,,1]
      stan_data$prior_a_sd <-  prior_a_sd[,,1]
      stan_data$LT_prior_on_mean_accuracy = LT_prior_on_mean_accuracy
    }
    
    
    stan_data_list[[i]] <- stan_data
    
  }
  
 
  # model files  
  if (corr_param == "Sean") {
    file <- file.path(file = "LC_MVP_bin_PartialLog_v4.stan")
    if (prior_only == 1 )  file <- file.path(file = "LC_MVP_bin_PO.stan")
  }
  
        mod <- cmdstan_model(file)
 
            
  
  
  # if (LT_find_b_priors_using_density == TRUE) {
  #   file <- file.path(file = "LT_PO_b_finder.stan")
  #   mod <- cmdstan_model(file)
  # }
  # 
  
  
  # -------------------------------------------------------------------------
  
  u_initial <- array(0.01, dim = c(N, n_tests))
  
  km1_choose_2 = 0.5 * (n_tests - 2) * (n_tests - 1)
  known_num = 0

  beta_vec_init <- rep(0, n_class * n_tests)
  beta_vec_init[1:n_tests] <- - 1   # tests 1-T, class 1 (D-)
  beta_vec_init[(n_tests + 1):(2*n_tests)] <- + 1   # tests 1-T, class 1 (D+)
  
  init = list(
    u_raw = (u_initial),
    p_raw =  (-0.6931472), # equiv to 0.20 on p
    beta_vec = beta_vec_init,
    off_raw = array(0.01, dim = c(n_class, km1_choose_2 - known_num)), 
    col_one_raw =   array(0.01, dim = c(n_class, n_tests - 1))
  )
  
 
  
  
}





 