




{
  
  # Set working direcory ---------------
  try({  setwd("/home/enzocerullo/Documents/Work/PhD_work/R_packages/BayesMVP/examples")   }, silent = TRUE)
  try({  setwd("/home/enzocerullo/Documents/Work/PhD_work/R_packages/BayesMVP/examples")    }, silent = TRUE)
  #  options(repos = c(CRAN = "http://cran.rstudio.com"))
  
  # options -------------------------------------------------------------------------
  #  totalCores = 8
  rstan::rstan_options(auto_write = TRUE)
  options(scipen = 999)
  options(max.print = 1000000000)
  #  rstan_options(auto_write = TRUE)
  options(mc.cores = parallel::detectCores() / 2)
  
}



# - |  |  | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ 




{
  
  #source("load_R_packages.R")
#  source("load_data_binary_LC_MVP_sim.R")
  source("BayesMVP_LC_MVP_prep_priors_settings.R") ## this also sources "load_R_packages.R" and "load_data_binary_LC_MVP_sim.R"
  
}






# - |  |  | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ 


 




# -| -------------------------   Run --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# -| -------------------------   Run --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# -| -------------------------   Run --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------






{
  
  experiment = "algorithm_binary"
  
       #   sample_all_at_once <- TRUE
        sample_all_at_once <- FALSE
         
       sample_us_using_Adv_HMC <- TRUE
    #   sample_us_using_Adv_HMC <- FALSE
     
     
        nuisance_transformation <- "Phi" # seems correct
      #   nuisance_transformation <- "Phi_approx"  # wrong 
      # nuisance_transformation <- "Phi_approx_rough" # seems correct 
       #nuisance_transformation <- "tanh"  # seems correct
     
      
      eps = 1
      eps_us = 1
      adapt_delta <-   0.80 ; 
      max_depth = 10  ;     fix_rho <- TRUE ;             adapt_M_us <- TRUE ;      adapt_M_main = TRUE
      metric_type <- "Hessian"   ;  dense_M <- TRUE 
      # metric_type <-   "Empirical" ;   dense_M <- FALSE  
     
          #   u_Euclidean_metric_const = TRUE
            u_Euclidean_metric_const = FALSE
       
       #  smooth_M_main = TRUE
        smooth_M_main = FALSE
        
                run_type = "pilot_burn_and_targ_ESS"

           
          #  run_type= "test_tau_target_method"
                
            start_runs <- 1;  n_runs = 3
      
                   #    N_vec <- N <-   5000
                  #  N_vec <- c(500, 1000)
             #  N_vec <-  c(500, 1000, 2500)
             # N_vec <-  c(500, 1000, 2500, 5000)
            #       N_vec <-   25000
                 N <-  N_vec <-  500
                   #N_vec  <-   25000
          #   N_vec <-  c(12500, 25000)
        #    N_vec <-  c(500, 1000, 2500, 5000, 12500, 25000)
                 
            #  vect_type = "AVX2"
         #  vect_type = "AVX512"
           vect_type = "Stan" 
          #  vect_type = "Eigen" 
         #   vect_type = "Loop"
               
       Phi_type = "Phi"
       # Phi_type = "Phi_approx"
               #    Phi_type = "Phi_approx_2"im 
            
            #
         inv_Phi_type = "inv_Phi"
        #  inv_Phi_type = "inv_Phi_approx"
               
                #  skip_checks = TRUE 
                   skip_checks = FALSE 
 
             #       overflow_threshold <-   +2000  ;            underflow_threshold <-  -2000 # for  testing
          #  overflow_threshold <-   +0.0001  ;            underflow_threshold <-  -2000 # for  testing
          #  overflow_threshold <-   +2000  ;            underflow_threshold <-  -0.0001 # for  testing
 
           overflow_threshold <-   +5  ;            underflow_threshold <-  -5
        #     overflow_threshold <-   +8  ;            underflow_threshold <-  -8
          
          
         # overflow_threshold <-   +0.0001  ;            underflow_threshold <-  -0.0001 # for  testing
         
}
   
    



 






for (N in N_vec) { 
   
   
   if (N == 500)           L_main_if_manual = 6   # 8 is OK, but lot of variation - try 6?
   if (N == 1000)          L_main_if_manual = 8    # N = 1000 --- seems to work well
   if (N == 2500)          L_main_if_manual = 12   # N = 2500  --- seems to work well
   if (N == 5000)          L_main_if_manual = 16   # N = 5000  --- seems to work well
   if (N == 12500)         L_main_if_manual = 20   # N = 12,500  --- seems to work well  
   if (N == 25000)         L_main_if_manual = 24   # N =   
   
   
        if (Model_type == "MVP_standard") { 
          
          try({
            source("/home/enzo/Documents/Work/PhD_work/R_packages/BayesMVP/R/example_run_MVP_standard.R") 
          }, silent = TRUE)
          try({
            source("/home/enzocerullo/Documents/Work/PhD_work/R_packages/BayesMVP/R/example_run_MVP_standard.R")
          }, silent = TRUE )
          
        } else { 
          
          n_covariates_max <- 1 # max(n_covariates_per_outcome_vec)
          prior_coeffs_mean <- array(0, dim = c(n_class, n_tests, n_covariates_max))
          prior_coeffs_sd <-   array(0, dim = c(n_class, n_tests, n_covariates_max))
          prior_coeffs_mean[,,1] <- prior_coeffs_mean
          prior_coeffs_sd[,,1] <- prior_coeffs_sd
          
          X_no_covariates_vec <- rep(1, N)
          X <- array(999999, dim = c(n_tests, n_covariates_max, N))
          
          for (t in 1:n_tests) {
            X[t,1,1:N] <- X_no_covariates_vec
          }
          
          # for (t in 1:n_tests) {
          #   X_list[[t]] <- array(999999, dim = c(N, n_covariates_per_outcome_vec[t]))
          #   for (k in 1:n_covariates_per_outcome_vec[t]) {
          #     for (n in 1:N) {
          #       X_list[[t]][n,  k] <- X[t, k, n]
          #     }
          #   }
          # }
          
        }
        
  
        {
          
               n_burnin_vec <- c(125) ;     lr_vec = c(0.05, 0.10, 0.15)
          #    n_burnin_vec <- c(250)  ;    lr_vec = c(0.05, 0.10)
          
       
          #  n_chains_vec <- c(32, 64) ;    n_chains_burnin_vec <- c(8, 16)
           # n_chains_vec <- c(8) ;           n_chains_burnin_vec <- c(8)
       
        
           if (run_type == "pilot_burn_and_targ_ESS") { 
             
                      n_chains_vec <- c(8) ;     n_chains_burnin_vec <- c(8)    ;    n_burnin_vec <- c(250) ;       lr_vec <- c(0.10)  ;    adapt_width_type_vec = c("short")
                  ######  n_chains_vec <- c(64) ;     n_chains_burnin_vec <- c(8)    ;    n_burnin_vec <- c(250) ;       lr_vec <- c(0.10)  ;    adapt_width_type_vec = c("medium")
                   ##### n_chains_vec <- c(64) ;     n_chains_burnin_vec <- c(8)    ;    n_burnin_vec <- c(125) ;       lr_vec <- c(0.10)  ;    adapt_width_type_vec = c("medium")
                    
                   #    n_chains_vec <- c(8)  ;     n_chains_burnin_vec <- c(8)     ;    n_burnin_vec <- c(250) ;       lr_vec <- c(0.10)  ;    adapt_width_type_vec = c("short")
           } else if (run_type == "final_benchmarks") { 
    
                        
                     #  n_chains_vec <- c(32)  ;     n_chains_burnin_vec <- c(32)     ;    n_burnin_vec <- c(500) ;       lr_vec <- c(0.05)  ;    adapt_width_type_vec = c("very_short")
                   #    n_chains_vec <- c(8)  ;     n_chains_burnin_vec <- c(8)     ;    n_burnin_vec <- c(500) ;       lr_vec <- c(0.05)  ;    adapt_width_type_vec = c("short")
                    #  n_chains_vec <- c(8)  ;     n_chains_burnin_vec <- c(8)     ;    n_burnin_vec <- c(500) ;       lr_vec <- c(0.05)  ;    adapt_width_type_vec = c("medium")
                        
                     n_chains_vec <- c(8)  ;     n_chains_burnin_vec <- c(8)     ;    n_burnin_vec <- c(500) ;       lr_vec <- c(0.05)  ;    adapt_width_type_vec = c("medium")
                       
                       if (N < 2500) { 
                         adapt_width_type_vec = c("long") ;  lr_vec <- c(0.05) 
                       } else if (N == 2500) { 
                         adapt_width_type_vec = c("long") ;  lr_vec <- c(0.05) 
                       } else if (N == 5000) { 
                         adapt_width_type_vec = c("medium") ;  lr_vec <- c(0.05)  # 500_burn + medium_adapt + 0.05_LR seems v good for N=5000
                       } else if (N == 12500) {
                         adapt_width_type_vec = c("medium") ;  lr_vec <- c(0.05) 
                       } else if (N == 25000) { 
                         adapt_width_type_vec = c("medium")  ;  lr_vec <- c(0.05) 
                       }
                       
  
              
                 
           }
 
         
          if (  (parallel::detectCores() < 17) == TRUE) { 
            n_chains_vec = 8
          }
 
       
          n_chains = n_chains_vec
          n_chains_burnin = n_chains_burnin_vec
          
    
    {
      
      if (max(n_chains_burnin_vec) > min(n_chains_vec))  { 
        max_n_chains_burnin <- max(n_chains_burnin_vec)
        n_chains_vec[which(max_n_chains_burnin > n_chains_vec )] <- max_n_chains_burnin
      }
      
      print(paste("n_chains_burnin_vec = ", n_chains_burnin_vec))
      print(paste("n_chains_vec = ", n_chains_vec))
      print(paste("n_burnin_vec = ", n_burnin_vec))
      print(paste("lr_vec = ", lr_vec))
      print(paste("adapt_width_type_vec = ", adapt_width_type_vec))
      
      
    }
    
    
    
    
  }
  
  
  
  
  
 # ----------------------------- 
  
 
      for (n_chains_burnin in n_chains_burnin_vec)
         for (n_chains in n_chains_vec)
           for (n_burnin in n_burnin_vec) 
              for (adapt_width_type in adapt_width_type_vec)
                 for (learning_rate_main in lr_vec)
  {
             
  
{

  
  model_outs_list <- list()
  vec_L_burnin <- vec_L_sampling <- vec_min_ESS <-  vec_Max_rhat <- vec_Max_rhat_nested <- vec_time_total <-  vec_time_sampling <- c()
  vec_Min_ESS_per_sec_overall <- vec_Min_ESS_per_sec_sampling <- vec_Min_ESS_per_grad_overall <- vec_Min_ESS_per_grad_sampling  <- c()
  vec_divs <- c()
  vec_divs_pct <- c()
  Se_ests <- array(dim = c(n_runs, n_tests))
  Sp_ests <- array(dim = c(n_runs, n_tests))
  Se_median_ests <- array(dim = c(n_runs, n_tests))
  Sp_median_ests <- array(dim = c(n_runs, n_tests))
  Se_mean_ests <- array(dim = c(n_runs, n_tests))
  Sp_mean_ests <- array(dim = c(n_runs, n_tests))
  prev_ests <- c()
  LC_LT_a_ests  <- array(dim = c(n_runs,  n_tests*n_class))
  LC_LT_b_ests  <- array(dim = c(n_runs,  n_tests*n_class))
  LC_MVP_mu_ests  <- array(dim = c(n_runs,  n_tests*n_class))
  
  
  if  (experiment == "algorithm_binary")  { 
    
              seed_dataset <- 123 
              
              if (parallel::detectCores() < 17) { # ;laptop
                if (N == 500)    num_chunks <-  1
                if (N == 1000)   num_chunks <-  2
                if (N == 2500)   num_chunks <-  2
                if (N == 5000)   num_chunks <-  10
                if (N == 12500)  num_chunks <-  20 # 25
                if (N == 25000)  num_chunks <-  40
              } else { # HPC
                if (N == 500)    num_chunks <-  1
                if (N == 1000)   num_chunks <-  1
                if (N == 2500)   num_chunks <-  2
                if (N == 5000)   num_chunks <-  10 # 5
                if (N == 12500)  num_chunks <-  20 # 10
                if (N == 25000)  num_chunks <-  40 # 20
              }
              
              
    
  } else { 
    
            seed_dataset <- df_i
    
  }
  
 
          if (run_type == "pilot_burn_and_targ_ESS") {
            
                if (n_chains < 17) { 
                  n_iter =  4000
                } else if (n_chains %in% c(18:32)) { 
                  if (N < 5001) n_iter =  8000  
                  else          n_iter =  2000  
                } else { 
                  n_iter =  1000  
                }
 
          } else if (run_type == "final_benchmarks") { 
            
            n_iter =       1000
            
          }
  
  
  
          df_i = 1
 
}


 


    {

          
    for (df_i in start_runs:n_runs) {
      
            {
              
              {
              
                  if (N == 500)     dataset_index = 1
                  if (N == 1000)    dataset_index = 2
                  if (N == 2500)    dataset_index = 3 
                  if (N == 5000)    dataset_index = 4
                  if (N == 12500)   dataset_index = 5
                  if (N == 25000)   dataset_index = 6
     
                  
                  
                  {
                      if (n_burnin < 500) {
                      
                          if (adapt_width_type == "very_short")     adapt_interval_width = 25# 10
                          if (adapt_width_type == "short")          adapt_interval_width = 25
                          if (adapt_width_type == "medium")         adapt_interval_width = 84# 63 
                          if (adapt_width_type == "long")           adapt_interval_width = 84                      
                      
                      } else { 
                        
                          if (adapt_width_type == "very_short")     adapt_interval_width = 25
                          if (adapt_width_type == "short")          adapt_interval_width = 50
                          if (adapt_width_type == "medium")         adapt_interval_width = 100 
                          if (adapt_width_type == "long")           adapt_interval_width = 167  
                          
                      }
                      
                      if (adapt_width_type == "full_burnin")      adapt_interval_width = n_burnin
                  }
                  
                  
                  
                  
                  N_obs <- N*n_tests
                  
                  
                  
              
              }
              
            #   vect_type = "Stan"
               #  vect_type = "Loop" ### ; approx = F ;  skip_checks = T ;  Phi_type = "Phi_approx" 
              ## vect_type = "AVX512"
              
              # df_i = 4
              
             #  overflow_threshold <-   +0.000001  ;            underflow_threshold <-  -0.000001
            #   overflow_threshold <-   +500  ;            underflow_threshold <-  -500
             
          ##    num_chunks = 1  ###  -------------------------
              
                    model_outs <- BayesMVP_sample(seed =  df_i, 
                                                  autodiff = FALSE,
                                                  Model_type = Model_type,
                                                  num_chunks = num_chunks,
                                                  y = y_master_list_seed_123_datasets[[dataset_index]], 
                                                  X = X,
                                                  n_covariates_per_outcome_vec = n_covariates_per_outcome_vec,
                                                  Phi_type = Phi_type,
                                                  inv_Phi_type = inv_Phi_type,
                                                  vect_type =  vect_type ,
                                                  skip_checks   = skip_checks,
                                                  overflow_threshold = overflow_threshold,
                                                  underflow_threshold = underflow_threshold,
                                                  n_chains  = n_chains, # using 48 chains (for main results in paper) 
                                                  n_chains_burnin = n_chains_burnin,
                                                  adapt_interval_width =  adapt_interval_width, 
                                                  max_depth = max_depth,
                                                  n_burnin =  n_burnin,            ###
                                                  learning_rate_main = learning_rate_main,    ###
                                                  adapt_delta = adapt_delta, 
                                                  fix_rho = fix_rho,               ###
                                                  n_iter = n_iter,                 ###
                                                  forked = FALSE,
                                                  save_individual_log_lik = FALSE,
                                                  LOO = FALSE,
                                                  metric_type = metric_type,
                                                  prev_prior_a =   5,  # 1, ##########
                                                  prev_prior_b =   10, # 1, ##########
                                                  corr_param = corr_param, 
                                                  prior_only = prior_only,
                                                  beta_prior_mean_vec = beta_prior_mean_vec,
                                                  beta_prior_sd_vec = beta_prior_sd_vec,
                                                  corr_prior_beta = corr_prior_beta,
                                                  corr_force_positive = corr_force_positive,
                                                  lkj_cholesky_eta = lkj_cholesky_eta, 
                                                  prior_for_corr_a = prior_for_skewed_LKJ_a,
                                                  prior_for_corr_b =  prior_for_skewed_LKJ_b,
                                                  known_values_indicator_list = known_values_indicator_list,
                                                  known_values_list= known_values_list,
                                                  LT_b_priors_shape = LT_b_priors_shape,
                                                  LT_b_priors_scale = LT_b_priors_scale,
                                                  LT_known_bs_values = LT_known_bs_values,
                                                  LT_known_bs_indicator = LT_known_bs_indicator,
                                                  adapt_M_main = adapt_M_main,
                                                 #  dense_G_indicator = TRUE,      ###
                                                  main_L_manual = FALSE,  
                                                  L_main_if_manual = L_main_if_manual ,  # N = 5000 
                                                  adapt_M_us = adapt_M_us,        ###
                                                  max_eps = 1, 
                                                  smooth_M_main = smooth_M_main ,
                                                  u_Euclidean_metric_const = u_Euclidean_metric_const,
                                                  dense_M = dense_M,
                                                  sample_all_at_once = sample_all_at_once,
                                                  sample_us_using_Adv_HMC = sample_us_using_Adv_HMC,
                                                 nuisance_transformation = nuisance_transformation
                                                )
                    
                    

                    model_outs_list[[df_i]] <- model_outs
                    
                    trace_theta_main =  model_outs$trace_array
                    
                   
                    
                    
                  
                    
                    str(trace_theta_main)
                    
                  
                    model_outs$trace_div[4,]
                    
                    
                   vec_L_burnin[df_i] <-   model_outs$L_burnin
                   vec_L_sampling[df_i] <-   model_outs$L_sampling
                   vec_min_ESS[df_i] <-   model_outs$min_ESS
                   vec_Max_rhat[df_i] <-   model_outs$Max_rhat
                   vec_Max_rhat_nested[df_i] <-   model_outs$Max_rhat_nested
                   vec_time_total[df_i] <-   model_outs$time_total
                   vec_time_sampling[df_i] <-   model_outs$time_sampling
                   vec_Min_ESS_per_sec_overall[df_i] <-   model_outs$Min_ESS_per_sec_overall
                   vec_Min_ESS_per_sec_sampling[df_i] <-   model_outs$Min_ESS_per_sec_sampling
                   vec_Min_ESS_per_grad_overall[df_i] <-   model_outs$Min_ESS_per_grad_overall
                   vec_Min_ESS_per_grad_sampling[df_i] <-   model_outs$Min_ESS_per_grad_sampling
                   vec_divs[df_i] <-   model_outs$n_divs
                   vec_divs_pct[df_i] <-   (   model_outs$n_divs  / (n_chains * n_iter) ) * 100
                   
                   

                   # if (vec_divs_pct[df_i] > 5) {
                   #   break
                   # }
              
                   
                   {
                     
                     n_iter_test <- round(n_iter * 1)
                     
                     samps <-  model_outs$trace_array
                     samps_3d_array <- model_outs$trace_array[,1:n_iter_test,] 
                     
                     i_start <- 1
                     i_end <- n_iter_test
                     
                     rhat_vec <- c()
                     rhat_nested_vec <- rhat_basic_vec <-  c()
                     ess_vec <- c()
                     
                     superchain_ids = seq(from = 1, to = n_chains, by = 1)
                     if (n_chains > 4)  superchain_ids = c(rep(1, n_chains/2), rep(2, n_chains/2))
                     if (n_chains > 15)  superchain_ids = c(rep(1, n_chains/4), rep(2, n_chains/4), rep(3, n_chains/4), rep(4, n_chains/4))
                     if (n_chains > 47)  superchain_ids = c(rep(1, n_chains/8), rep(2, n_chains/8), rep(3, n_chains/8), rep(4, n_chains/8), 
                                                            rep(5, n_chains/8), rep(6, n_chains/8), rep(7, n_chains/8), rep(8, n_chains/8))
                     
                     for (i_param in 1:dim(samps_3d_array)[3]) {
                       rhat_vec[i_param] <- posterior::rhat(t(samps_3d_array[,,i_param]))
                       rhat_nested_vec[i_param] <- posterior::rhat_nested(t(samps_3d_array[,,i_param]), superchain_ids =  superchain_ids)
                       ess_vec[i_param] <- posterior::ess_bulk(t(samps_3d_array[,,i_param]))
                     }
                     
                     print(round(max(rhat_vec), 3))
                     print(round(max(rhat_nested_vec), 3))
                     print(round(min(ess_vec), 0))
                     
                     
                     if (Model_type == "MVP_LC") {
                       
   
                               ## raw params
                               samps_3d_array_means <- apply(samps_3d_array, c(1, 3), mean)
                               samps_3d_array_means <- apply(samps_3d_array_means, 2, mean)
 
                               coeffs_index <- (n_corrs + 1):(n_corrs + n_class*n_tests)
                               LC_MVP_mu_ests[df_i, ] <-  samps_3d_array_means[coeffs_index]    
                               
                               ##  summary/inference params
                               # Sp
                               samps_3d_array_means <- apply(1 - pnorm(samps_3d_array), c(1, 3), mean)
                               samps_3d_array_means <- apply(samps_3d_array_means, 2, median)
                               Sp_ests[df_i, ] <- print(round(samps_3d_array_means[(n_corrs+1):(n_corrs+n_tests)] * 100, 3))
                               Sp_mean_ests[df_i, ] <-   Sp_ests[df_i, ]   # mean=median for non-LT MVP! (NOT MCMC posterior median/means!)
                               Sp_median_ests[df_i, ] <- Sp_ests[df_i, ] # mean=median for non-LT MVP! (NOT MCMC posterior median/means!)
                         
                               # Se
                               samps_3d_array_means <- apply(pnorm(samps_3d_array), c(1, 3), mean)
                               samps_3d_array_means <- apply(samps_3d_array_means, 2, median)
                               Se_ests[df_i, ] <- print(round(samps_3d_array_means[(n_corrs + n_tests + 1):(n_corrs + 2*n_tests)] * 100, 3))
                               Se_mean_ests[df_i,] <-   Se_ests[df_i, ]   # mean=median for non-LT MVP! (NOT MCMC posterior median/means!)
                               Se_median_ests[df_i,] <- Se_ests[df_i, ] # mean=median for non-LT MVP! (NOT MCMC posterior median/means!)
  
                               # prev
                               samps_3d_array_means <- apply((tanh(samps_3d_array) + 1)/2, c(1, 3), mean)
                               samps_3d_array_means <- apply(samps_3d_array_means, 2, median)
                               prev_ests[df_i] <- print(round(samps_3d_array_means[n_params_main] * 100, 3))
                             
                            
                             
                     } else if (Model_type == "LT_LC") { 
                       
                             n_bs_LT =  n_tests * n_class;
                             
                             ## raw params
                             # a's
                             coeffs_index <- (n_bs_LT + 1):(n_bs_LT + n_class*n_tests)
                             samps_array_LT_a <- samps_3d_array[,,coeffs_index]
                             samps_array_LT_a_means_per_chain <- apply(samps_array_LT_a, c(1, 3), mean)
                             samps_array_LT_a_means_summary <- apply(samps_array_LT_a_means_per_chain, 2, mean)
                             LC_LT_a_ests[df_i, ] <-  samps_array_LT_a_means_summary
                             
                             # b's
                             LT_b_index <- 1:(n_class*n_tests)
                             samps_array_LT_b <- exp(samps_3d_array[,,LT_b_index])
                             samps_array_LT_b_means_per_chain <- apply(samps_array_LT_b, c(1, 3), mean)
                             samps_array_LT_b_means_summary <- apply(samps_array_LT_b_means_per_chain, 2, mean)
                             LC_LT_b_ests[df_i, ] <-  samps_array_LT_b_means_summary

                             
                             ##  summary/inference params
                             # Sp
                             LT_a_index_nd <- head(coeffs_index, n_tests)
                             samps_array_LT_Sp_Medians <- 1  - Phi_approx_piecewise(samps_3d_array[,,LT_a_index_nd])
                             samps_array_LT_Sp_Medians_per_chain <- apply(samps_array_LT_Sp_Medians, c(1, 3), mean)
                             samps_array_LT_Sp_Medians_summary <- apply(samps_array_LT_Sp_Medians_per_chain, 2, mean)
                             Sp_median_ests[df_i, ] <- print(signif( samps_array_LT_Sp_Medians_summary * 100, 3))  # mean=median for non-LT MVP! (NOT MCMC posterior median/means!)
                             
                             LT_b_index_nd <- head(LT_b_index, n_tests)
                             samps_array_LT_Sp_Means <- 1  - Phi_approx_piecewise(samps_3d_array[,,LT_a_index_nd] / sqrt(1 + exp(samps_3d_array[,,LT_b_index_nd])^2 ) ) 
                             samps_array_LT_Sp_Means_per_chain <- apply(samps_array_LT_Sp_Means, c(1, 3), mean)
                             samps_array_LT_Sp_Means_summary <- apply(samps_array_LT_Sp_Means_per_chain, 2, mean)
                             Sp_mean_ests[df_i, ] <- print(signif( samps_array_LT_Sp_Means_summary * 100, 3))  # mean=median for non-LT MVP! (NOT MCMC posterior median/means!)
                             
                             if (LT_prior_on_mean_accuracy == 1) { 
                               Sp_ests[df_i, ]  <-  Sp_mean_ests[df_i,] # mean=median for non-LT MVP! (NOT MCMC posterior median/means!)
                             }   else  {
                               Sp_ests[df_i, ]  <-  Sp_median_ests[df_i,] # mean=median for non-LT MVP! (NOT MCMC posterior median/means!)
                             }
            
                             # Se
                             LT_a_index_d <- tail(coeffs_index, n_tests)
                             samps_array_LT_Se_Medians <-   Phi_approx_piecewise(samps_3d_array[,,LT_a_index_d])
                             samps_array_LT_Se_Medians_per_chain <- apply(samps_array_LT_Se_Medians, c(1, 3), mean)
                             samps_array_LT_Se_Medians_summary <- apply(samps_array_LT_Se_Medians_per_chain, 2, mean)
                             Se_median_ests[df_i, ] <- print(signif( samps_array_LT_Se_Medians_summary * 100, 3))  # mean=median for non-LT MVP! (NOT MCMC posterior median/means!)
                             
                             LT_b_index_d <- tail(LT_b_index, n_tests)
                             samps_array_LT_Se_Means <-   Phi_approx_piecewise(samps_3d_array[,,LT_a_index_d] / sqrt(1 + exp(samps_3d_array[,,LT_b_index_d])^2 ) ) 
                             samps_array_LT_Se_Means_per_chain <- apply(samps_array_LT_Se_Means, c(1, 3), mean)
                             samps_array_LT_Se_Means_summary <- apply(samps_array_LT_Se_Means_per_chain, 2, mean)
                             Se_mean_ests[df_i, ] <- print(signif( samps_array_LT_Se_Means_summary * 100, 3))  # mean=median for non-LT MVP! (NOT MCMC posterior median/means!)
                             
                             if (LT_prior_on_mean_accuracy == 1)  Se_ests[df_i, ]  <-  Se_mean_ests[df,i] # mean=median for non-LT MVP! (NOT MCMC posterior median/means!)
                             else                                 Se_ests[df_i, ]  <-  Se_median_ests[df,i] # mean=median for non-LT MVP! (NOT MCMC posterior median/means!)
                             
                           
                             samps_3d_array_means <- apply((tanh(samps_3d_array) + 1)/2, c(1, 3), mean)
                             samps_3d_array_means <- apply(samps_3d_array_means, 2, mean)
                             prev_ests[df_i] <- print(round(samps_3d_array_means[n_params_main] * 100, 3))
                       
                     }
                     
                     
                   }
                   
                   
                   

                   
                   
                   
            }
      
      
      # save individual file here 
  
      
      
      
      
      
      try({
        
                file_list <- list(
                  
                          model_outs = model_outs,
                          
                          # basic / summary  info
                          paste("seed = ", df_i),
                          paste("DGP = ", DGP),
                          paste("N = ", N),
                          paste("n_chains = ", n_chains), # 5
                          (paste("n_burnin = ", n_burnin)),
                          (paste("n_iter = ", n_iter)), 
                          (paste("adapt_delta  = ", adapt_delta))  ,
                          (paste("learning_rate  = ", learning_rate_main)),
                          (paste("fix_rho  = ", fix_rho)),  # 10
                          (paste("u_Euclidean_metric_const  = ", u_Euclidean_metric_const)),
                         # (paste("clip_iter = ", clip_iter)),
                        #  (paste("G_dense_ (main) = ", M_dense)),
                          (paste("n_burnin", n_burnin)),
                          (paste("learning_rate_main", learning_rate_main)), # 30
                          (paste("adapt_interval_width", adapt_interval_width)),
                          (paste("n_iter", n_iter)),
                          (paste("learning_rate_main", learning_rate_main)),
                          (paste("metric_type", metric_type)),
                        #  (paste("M_dense", M_dense)),
                          (paste("fix_rho", fix_rho)),
                          (paste("vect_type = ", vect_type)),
                          (paste("skip_checks = ", skip_checks)),
                          (paste("Phi_type = ", Phi_type))
                )
                
                
                
                
              
                file_name <- paste0("single_model_out_seed", df_i, "_", 
                                          "DGP", DGP,"_", 
                                          "N", N,"_", 
                                          "type", run_type, "_",
                                          "n_chains", n_chains,"_", 
                                          "burn", n_burnin,"_", 
                                          "iter", n_iter,"_", 
                                          "AD", adapt_delta,"_", 
                                          "LR", learning_rate_main,"_",
                                          "width", adapt_interval_width,"_",
                                        #  "Dense", M_dense,"_",
                                          "U_const", u_Euclidean_metric_const, "_",
                                          "U_M", adapt_M_us, "_",
                                          "M_type_", metric_type, "_",
                                         "vect_type", vect_type,  "_",
                                         "skip_checks", skip_checks,  "_",
                                         "Phi_type", Phi_type
                                        )
                
         
                
                if (parallel::detectCores() < 17) { 
                  file_name <- paste0("Laptop_", file_name) 
                }
                
                file_name <- paste0( file_name, "_", "n_ch_burn", n_chains_burnin )
                file_name <- paste0( file_name, "_", "reps", n_runs - start_runs + 1 )
                
                if (parallel::detectCores() < 17) { 
                  saveRDS(file_list, file =   paste0("Info_", file_name)) 
                } else { 
                  saveRDS(file_list, file =   paste0("Info_", file_name)) 
                }
                
        
      })
        
        
      
    }
      
     
          
          
          for (jj in 1:2) {
            
              {
  
                print(paste( " | ---------------------------------------------------------------------- | "))
                
                try({ 
                print(paste("n_runs = ", n_runs))
                print(paste("n_chunks", num_chunks))
                print(paste("n_chains = ", n_chains))
                print(paste("n_burnin", n_burnin))
                print(paste("n_iter", n_iter))
                print(paste("adapt_delta", adapt_delta))
                print(paste("learning_rate_main", learning_rate_main))
                print(paste("adapt_interval_width", adapt_interval_width))
                print(paste("metric_type", metric_type))
               # print(paste("M_dense", M_dense))
                print(paste("fix_rho", fix_rho))
                print(paste("adapt_M_us", adapt_M_us))
                print(paste("u_Euclidean_metric_const", u_Euclidean_metric_const))
                
                print(paste("L_burnin = ", mean(vec_L_burnin, na.rm = TRUE), sd(vec_L_burnin, na.rm = TRUE)))
                print(paste("vec_L_sampling = ", mean(vec_L_sampling, na.rm = TRUE), sd(vec_L_sampling, na.rm = TRUE)))
    
                print(paste("vec_min_ESS = ", mean(vec_min_ESS, na.rm = TRUE), sd(vec_min_ESS, na.rm = TRUE)))
                
                print(paste("vec_Max_rhat = ", mean(vec_Max_rhat, na.rm = TRUE), sd(vec_Max_rhat, na.rm = TRUE)))
                print(paste("vec_Max_rhat_nested = ", mean(vec_Max_rhat_nested, na.rm = TRUE), sd(vec_Max_rhat_nested, na.rm = TRUE) ))
                
                print(paste("vec_time_total = ", mean(vec_time_total, na.rm = TRUE),  sd(vec_time_total, na.rm = TRUE) ))
                print(paste("vec_time_sampling = ", mean(vec_time_sampling, na.rm = TRUE), sd(vec_time_sampling, na.rm = TRUE)))
                
                print(paste("vec_Min_ESS_per_sec_overall = ", mean(vec_Min_ESS_per_sec_overall, na.rm = TRUE),  sd(vec_Min_ESS_per_sec_overall, na.rm = TRUE)))   
                print(paste("vec_Min_ESS_per_sec_sampling = ", mean(vec_Min_ESS_per_sec_sampling, na.rm = TRUE),  sd(vec_Min_ESS_per_sec_sampling, na.rm = TRUE)))
              
                print(paste("vec_Min_ESS_per_grad_overall = ", mean(vec_Min_ESS_per_grad_overall, na.rm = TRUE),  sd(vec_Min_ESS_per_grad_overall, na.rm = TRUE)))
                print(paste("vec_Min_ESS_per_grad_sampling = ", mean(vec_Min_ESS_per_grad_sampling, na.rm = TRUE),  sd(vec_Min_ESS_per_grad_sampling, na.rm = TRUE)))
                })
                
                try({  
                print(paste("n_divs (%) = ", round(mean(vec_divs / (n_iter * n_chains) ), 4) * 100 , round(sd(vec_divs / (n_iter * n_chains) ), 8 ) * 100    )) 
                })
                  
                 ESS_target <-  100
                #   
                  vals_1 = vec_time_total -  vec_time_sampling +    ( ((ESS_target/(vec_Min_ESS_per_sec_sampling*1000/1))*1000) / 60 )
                  print(paste(mean(vals_1, na.rm = TRUE), sd(vals_1, na.rm = TRUE)))
 
              ESS_target <-  1000  #   2000
              
              vals_3 = vec_time_total -  vec_time_sampling +    ( ((ESS_target/(vec_Min_ESS_per_sec_sampling))) / 60 ) 
              print(paste(mean(vals_3, na.rm = TRUE), sd(vals_3, na.rm = TRUE)))
 
              ESS_target <- 10000
              
              vals_5 = vec_time_total -  vec_time_sampling +    ( ((ESS_target/(vec_Min_ESS_per_sec_sampling*1000/1))*1000) / 60 )  
            #  print(paste(mean(vals_5), sd(vals_5)))
 
              
              
              n_grad_evals_total <- n_chains * (   n_burnin *   (vec_L_burnin)  +            n_iter *     (vec_L_sampling)  )
              
              n_grad_evals_for_1000_ESS <- n_grad_evals_total * (1000 / mean(vec_min_ESS, na.rm = TRUE))
              
              if (metric_type == "Hessian")  n_grad_evals_for_1000_ESS <- n_grad_evals_for_1000_ESS + ( ( n_chains * n_burnin * n_params_main ) / 5 ) * 0.80 
              print(paste("n_grad_evals_for_1000_ESS = ", round(mean(n_grad_evals_for_1000_ESS, na.rm = TRUE),0 ),  round(sd(n_grad_evals_for_1000_ESS, na.rm = TRUE), 0)))
              
              
              print(paste( " | ---------------------------------------------------------------------- | "))
              
              
              print(signif(vec_L_burnin, 3))
              print(signif(vec_L_sampling, 3))
              print(signif(vec_Max_rhat, 4))
              print(signif(vec_Max_rhat_nested, 4))
              print(signif(vec_min_ESS, 1000))
              print(signif(vec_Min_ESS_per_grad_overall, 3))
              print(signif(vec_Min_ESS_per_grad_sampling, 3))
              print(signif(vec_time_total, 3))
              print(signif(vec_time_sampling, 3))
              print(signif(vec_Min_ESS_per_sec_overall, 3))
              print(signif(vec_Min_ESS_per_sec_sampling, 3))
              
              print(paste( " | ---------------------------------------------------------------------- | "))
              
              }
            
          }
 
            
            print(signif(vec_L_burnin, 3))
            print(signif(vec_L_sampling, 3))
            print(signif(vec_Max_rhat, 4))
            print(signif(vec_Max_rhat_nested, 4))
            print(signif(vec_min_ESS, 1000))
            print(signif(vec_Min_ESS_per_grad_overall, 3))
            print(signif(vec_Min_ESS_per_grad_sampling, 3))
            print(signif(vec_time_total, 3))
            print(signif(vec_time_sampling, 3))
            print(signif(vec_Min_ESS_per_sec_overall, 3))
            print(signif(vec_Min_ESS_per_sec_sampling, 3))
            
            
  
        
            {
              print(paste("|----------------------------------------|"))
              
            iqr_vec <- c()
            iqr_vec_upper <- c()
            iqr_vec_lower <- c()
            
            range_vec <- c()
            range_vec_upper <- c()
            range_vec_lower <- c()
            
            median_vec <- c()
            
            counter <- 1
            {
                for (t in 1:n_tests) {
                    iqr_vec[counter] <-   iqr(   Se_ests[,t]  ) 
                    iqr_vec_lower[counter] <- c(summary(Se_ests[,t])[2])
                    iqr_vec_upper[counter] <- c(summary(Se_ests[,t])[5])
                    print(paste0("IQR of Se", t, " = ",  round(iqr(   Se_ests[,t]  ), 3) ))
                    counter <- counter + 1
                }
                for (t in 1:n_tests) {
                  iqr_vec[counter] <-   iqr(   Sp_ests[,t]  ) 
                  iqr_vec_lower[counter] <- c(summary(Sp_ests[,t])[2])
                  iqr_vec_upper[counter] <- c(summary(Sp_ests[,t])[5])
                  print(paste0("IQR of Sp", t, " = ", round(iqr(   Sp_ests[,t]  ), 3)  ))
                  counter <- counter + 1
                }
              
                iqr_vec[counter] <-   iqr(  prev_ests  ) 
                iqr_vec_lower[counter] <- c(summary(prev_ests)[2])
                iqr_vec_upper[counter] <- c(summary(prev_ests)[2])
                print(paste0("IQR of prev", t, " = ", round(iqr(  prev_ests  ), 3)  ))
 
                print(paste0("max IQR = ", round(max(iqr_vec), 3)))
            }
            
            counter <- 1
            {
              for (t in 1:n_tests) {
                range_vec[counter] <-   range(   Se_ests[,t]  )[2] - range(   Se_ests[,t]  )[1]
                range_vec_lower[counter] <- c(range(Se_ests[,t])[1])
                range_vec_upper[counter] <- c(range(Se_ests[,t])[2])
                print(paste0("Range of Se", t, " = ",  round(   range_vec[counter] , 3) ))
                counter <- counter + 1
              }
              for (t in 1:n_tests) {
                range_vec[counter] <-   range(   Sp_ests[,t]  )[2] - range(   Sp_ests[,t]  )[1]
                range_vec_lower[counter] <- c(range(Sp_ests[,t])[1])
                range_vec_upper[counter] <- c(range(Sp_ests[,t])[2])
                print(paste0("Range of Sp", t, " = ", round(   range_vec[counter] , 3)  ))
                counter <- counter + 1
              }
              
              range_vec[counter] <-   range(   prev_ests  )[2] - range(   prev_ests   )[1]
              range_vec_lower[counter] <- c(range(prev_ests)[1])
              range_vec_upper[counter] <- c(range(prev_ests)[2])
              print(paste0("Range of prev", t, " = ", round(   range_vec[counter] , 3)  ))
              
              print(paste0("max Range = ", round(max(range_vec), 3)))
            }
            
     
            
            print(paste("|----------------------------------------|"))
      
            print(paste0("max IQR (lower value) = ", round(           iqr_vec_lower[which(iqr_vec == max(iqr_vec))]  , 3)))
            print(paste0("max IQR (upper value) = ", round(           iqr_vec_upper[which(iqr_vec == max(iqr_vec))]  , 3)))
            
            print(paste0("max Range (lower value) = ",  round(           iqr_vec_lower[which(range_vec == max(range_vec))]  , 3)))
            print(paste0("max Range (upper value) = ",  round(           iqr_vec_upper[which(range_vec == max(range_vec))]  , 3)))
  
            print(paste("|----------------------------------------|"))
            
            
            print(paste("|----------------------------------------|"))
            print(paste0("max Range = ", round(max(range_vec), 3)))
            print(paste0("max Range index = ", which(range_vec == max(range_vec))))
            print(paste0("max IQR = ",  round(max(iqr_vec), 3)))
            print(paste0("max IQR index = ",  which(iqr_vec == max(iqr_vec))))  
            print(paste("|----------------------------------------|"))
 

            print(paste("|----------------------------------------|"))
            print(paste("vec_min_ESS = ",       (round(min(ess_vec), 0)) ))
            print(paste("N = ", mean(N)))
            print(paste("n_iter = ", mean(n_iter)))
            print(paste("n_iter_test = ", mean(n_iter_test)))
          #  print(paste("Phi_approx = ", Phi_approx))
            print(paste("n_divs (%) = ", round(mean(vec_divs / (n_iter * n_chains) ), 4) ))
            print(paste("|----------------------------------------|"))
            }
            
            mean((((rnorm(n  = 1000000, 0, (2))))))
            sd((((rnorm(n  = 1000000, 0, (2))))))
            
            log(abs(mean((((rnorm(n  = 1000000, 0, (1))))))  * 2))
            log(abs(sd((((rnorm(n  = 1000000, 0, (1)))))) *  2))
            
            
            
            
            try({
              file_list <- list(
                
                model_outs_list = model_outs_list,
                
                
                
                
                # basic / summary  info
                paste("seed = ", df_i),
                paste("DGP = ", DGP),
                paste("N = ", N),
                paste("n_chains = ", n_chains), # 5
                (paste("n_burnin = ", n_burnin)),
                (paste("n_iter = ", n_iter)), 
                (paste("adapt_delta  = ", adapt_delta))  ,
                (paste("learning_rate  = ", learning_rate_main)),
                (paste("fix_rho  = ", fix_rho)),  # 10
                (paste("u_Euclidean_metric_const  = ", u_Euclidean_metric_const)),
               # (paste("clip_iter = ", clip_iter)),
              #  (paste("G_dense_ (main) = ", M_dense)),
                
                # efficiency stats
                 (paste("vec_L_burnin = ", mean(vec_L_burnin, na.rm = TRUE), sd(vec_L_burnin, na.rm = TRUE))), 
                 (paste("vec_L_sampling = ", mean(vec_L_sampling, na.rm = TRUE), sd(vec_L_sampling, na.rm = TRUE))), 
                
                (paste("vec_min_ESS = ", mean(vec_min_ESS, na.rm = TRUE), sd(vec_min_ESS, na.rm = TRUE))),
                
                (paste("vec_Max_rhat = ", mean(vec_Max_rhat, na.rm = TRUE), sd(vec_Max_rhat, na.rm = TRUE))),
                (paste("vec_Max_rhat_nested = ", mean(vec_Max_rhat_nested, na.rm = TRUE), sd(vec_Max_rhat_nested, na.rm = TRUE) )),
                
                (paste("vec_time_total = ", mean(vec_time_total, na.rm = TRUE),  sd(vec_time_total, na.rm = TRUE) )),
                (paste("vec_time_sampling = ", mean(vec_time_sampling, na.rm = TRUE), sd(vec_time_sampling, na.rm = TRUE))),
                
                (paste("vec_Min_ESS_per_sec_overall = ", mean(vec_Min_ESS_per_sec_overall, na.rm = TRUE),  sd(vec_Min_ESS_per_sec_overall, na.rm = TRUE))) ,    
                (paste("vec_Min_ESS_per_sec_sampling = ", mean(vec_Min_ESS_per_sec_sampling, na.rm = TRUE),  sd(vec_Min_ESS_per_sec_sampling, na.rm = TRUE))),  
                
                (paste("vec_Min_ESS_per_grad_overall = ", mean(vec_Min_ESS_per_grad_overall, na.rm = TRUE),  sd(vec_Min_ESS_per_grad_overall, na.rm = TRUE))) , 
                (paste("vec_Min_ESS_per_grad_sampling = ", mean(vec_Min_ESS_per_grad_sampling, na.rm = TRUE),  sd(vec_Min_ESS_per_grad_sampling, na.rm = TRUE)))  ,
                
                (paste("time_to_100_ESS = ",   (paste(mean(vals_1, na.rm = TRUE), sd(vals_1, na.rm = TRUE))) ))  , # 25
                (paste("time_to_1000_ESS = ",  (paste(mean(vals_3, na.rm = TRUE), sd(vals_3, na.rm = TRUE))) )) ,
                
                (paste("n_grad_evals_for_1000_ESS = ", mean(n_grad_evals_for_1000_ESS, na.rm = TRUE), sd(n_grad_evals_for_1000_ESS, na.rm = TRUE)) )  ,
                
                
                (paste("n_divs (%) = ", round(mean(vec_divs / (n_iter * n_chains) ), 4) * 100 , round(sd(vec_divs / (n_iter * n_chains) ), 8 ) * 100     ))  , 
                
                (paste("n_burnin", n_burnin)),
                (paste("learning_rate_main", learning_rate_main)), # 30
                (paste("adapt_interval_width", adapt_interval_width)),
                (paste("n_iter", n_iter)),
                (paste("learning_rate_main", learning_rate_main)),
                (paste("metric_type", metric_type)),
              #  (paste("M_dense", M_dense)),
                (paste("fix_rho", fix_rho)),
                
                (paste(mean(vals_1), sd(vals_1))), 
                (paste(mean(vals_3), sd(vals_3))),
                
                (paste("n_runs = ", n_runs)),
                "vec_L_burnin = ", vec_L_burnin,
                "vec_L_sampling = ", vec_L_sampling,
                "vec_Max_rhat = ", vec_Max_rhat,
                "vec_Max_rhat_nested = ", vec_Max_rhat_nested,
                "vec_min_ESS = ", vec_min_ESS,
                "vec_Min_ESS_per_grad_overall = ", vec_Min_ESS_per_grad_overall,
                "vec_Min_ESS_per_grad_sampling = ", vec_Min_ESS_per_grad_sampling,
                "vec_time_total = ", vec_time_total,
                "vec_time_sampling = ", vec_time_sampling,
                "vec_Min_ESS_per_sec_overall = ", vec_Min_ESS_per_sec_overall,
                "vec_Min_ESS_per_sec_sampling = ", vec_Min_ESS_per_sec_sampling,
                "Se_ests = ",        Se_ests,
                "Sp_ests = ",        Sp_ests,
                "prev_ests = ",      prev_ests, 
                "Se_true = ",         Se_true_observed_list[[123]]  *100,        ###################### sort this out !!
                "Sp_true = ",         Sp_true_observed_list[[123]]  *100,        ###################### sort this out !!
                "prev_true = ",       prev_true_observed_list[[123]]*100, ###################### sort this out !!
                (paste0("max IQR = ", round(max(iqr_vec), 3))),
                (paste0("max Range = ", round(max(range_vec), 3))), 
                (paste0("vect_type = ", vect_type)),
                (paste0("skip_checks = ", skip_checks)), 
                (paste0("Phi_type = ", Phi_type)),
                "vec_divs = ",  vec_divs,
                "vec_divs_pct = ",  vec_divs_pct
              )
              
            file_name <- paste0( # "seed", df_i,"_", 
                                "DGP", DGP,"_", 
                                "N", N,"_", 
                                "type", run_type, "_", 
                                "n_chains", n_chains, "_", 
                                "burn", n_burnin, "_", 
                                "iter", n_iter, "_", 
                                "AD", adapt_delta, "_", 
                                "LR", learning_rate_main, "_", 
                                "width", adapt_interval_width, "_", 
                               # "Dense", M_dense,"_", 
                                "U_const", u_Euclidean_metric_const, "_", 
                                "U_M", adapt_M_us, "_", 
                                "M_type_", metric_type, "_", 
                                "vect_type", vect_type,    "_", 
                                "skip_checks", skip_checks,   "_", 
                                "Phi_type", Phi_type 
            )
            
            if (parallel::detectCores() < 17) {
              file_name <- paste0("Laptop_", file_name)
            }
            
            file_name <- paste0( file_name, "_", "n_ch_burn", n_chains_burnin )
            file_name <- paste0( file_name, "_", "reps", n_runs - start_runs + 1 )
            
            if (parallel::detectCores() < 17) { 
                saveRDS(file_list, file =   paste0("Info_", file_name)) 
            } else { 
                saveRDS(file_list, file =   paste0("Info_", file_name)) 
            }
            
            beepr::beep("random")  # make sound to know model has finished running 
             
          }) 
      
            }
  
        }
  
 }



















mean(prev_ests)
colMedians(Se_ests)
          
M_diag_vec <- model_outs$M_diag_vec
# M_inv_us <- 1 / M_diag_vec[1:(N*5)]
# M_diag_vec[1:(N*5)] <- max(M_inv_us) * (1 / M_inv_us)
  
sqrt(1 / M_diag_vec)

min(sqrt(1 / M_diag_vec[1:(N*5)]))
max((sqrt(1 / M_diag_vec[1:(N*5)])))

max((sqrt(1 / M_diag_vec[1:(N*5)]))) / min(sqrt(1 / M_diag_vec[1:(N*5)]))


min(M_diag_vec[1:(N*5)])
max(M_diag_vec[1:(N*5)])

model_outs$ess_us
str(model_outs$trace_theta_us)
# model_outs$trace_theta_us[1:8,1,1:50]


ess_bulk(t(model_outs$trace_theta_us[1:n_chains,,1]))
ess_bulk(t(model_outs$trace_theta_us[1:n_chains,,2]))


rhats_us <- c()
ess_us <- c()
for (i in 1:10) {
#  rhats_us[i] <-  rstan::rhat(t(model_outs$trace_theta_us[1:n_chains,,i]))
  ess_us[i] <-  rstan::ess_bulk(t(model_outs$trace_theta_us[1:n_chains,,i]))
}

ess_us


min(ess_us) ; mean(ess_us)
max(rhats_us)  ; mean(rhats_us)


rhats_us
ess_us




 vec_divs
  vec_divs_pct
 vec_Min_ESS_per_grad_sampling
 mean(vec_Min_ESS_per_grad_sampling)
 median(vec_Min_ESS_per_grad_sampling)

 mean(vec_divs_pct)
 median(vec_divs_pct)
 
 
 
N_vec <- c(500, 1000, 2500, 5000)
Phi_type_vec <- c("Phi_approx")
i = 1
j = 1
run_type = "pilot_approx"

 
{
  
  file_name <-  "Info_DGP5_N2500_typepilot_approx_n_chains32_burn500_iter500_AD0.8_LR0.05_width50_U_constFALSE_U_MTRUE_M_type_Hessian_vect_typeAVX512_skip_checksFALSE_Phi_typePhi_approx_n_ch_burn32_reps10"
  file_name <-  paste0("Info_DGP5_N", N_vec[i],
                      "_type", run_type, 
                      "_n_chains48_burn250_iter500_AD0.8_LR0.075_width25_DenseTRUE_U_constFALSE_U_MTRUE_M_type_Hessian_",
                      "vect_typeAVX512",
                      "skip_checksTRUE",
                      "Phi_typePhi_approx",
                      "_n_ch_burn8_reps10")
  
  
  mod  <- readRDS(file_name)
  
  
  
  
  
  print(paste("N =", N_vec[i]))
  print(paste("Phi_type =", Phi_type_vec[j]))

  mod[[1]] <- NA
  print( (mod[[28]] ))
  print(mod[[23]] ) # print( round(  signif(as.numeric(substr(mod[[22]], start = 32, stop = 38), 3) )   ) , 3))
  print( mod[[25]] ) # , 3))
  print(  paste("Ratio = ",  signif( (mean(mod[[61]])    /  mean(mod[[53]]) ) / 8, 3) )  )
  
  
  print(mod)
  
  print( mod[[27]] ) 
  
  vec_time_total    <- mod[[55]]
  vec_time_sampling <- mod[[57]]
  vec_time_burnin <- vec_time_total -  vec_time_sampling
  vec_ESS <- mod[[49]]
  # n_iter = 500
  
  
  time_1000_ESS <- vec_time_burnin + vec_time_sampling * (1000 / vec_ESS)
  mean_time_1000_ESS <- mean(time_1000_ESS)
  print(  paste("mean_time_1000_ESS = ",  signif(mean_time_1000_ESS, 3), "minutes" )  )
  
  if (N_vec[i] == 500)   target_ESS = 5000
  if (N_vec[i] == 1000)  target_ESS = 5000
  if (N_vec[i] == 2500)  target_ESS = 2500
  if (N_vec[i] == 5000)  target_ESS = 2000
  if (N_vec[i] == 12500) target_ESS = 500
  if (N_vec[i] == 25000) target_ESS = 500
  
  time_target_ESS <- vec_time_burnin + vec_time_sampling * (target_ESS / vec_ESS)
  mean_time_target_ESS <- mean(time_target_ESS)
  print(  paste("mean_time_target_ESS = ",  signif(mean_time_target_ESS, 3), "minutes" )  )
  
}








vec_divs_pct             
              
       
ESS_target = 5000 ; mins = 1.5 ;  (2 * ESS_target) / (mins * 60)
ESS_target = 5000 ; mins = 4.2 ;  (2 * ESS_target) / (mins * 60)
ESS_target = 2500 ; mins = 18 ;  (2 * ESS_target) / (mins * 60)
ESS_target = 2000 ; mins = 39 ;  (2 * ESS_target) / (mins * 60)
ESS_target = 500 ; mins = 43 ;  (2 * ESS_target) / (mins * 60)
ESS_target = 500 ; mins = 217 ;  (2 * ESS_target) / (mins * 60)



{

 N_vec <- c(5000, 12500, 25000)
# N_vec <- c(5000, 12500, 25000)
Phi_type_vec <- c("Phi_approx", "Phi")
fast_log_vec <- c(TRUE, FALSE)


for (i in  1:length(N_vec)) {  
  for (j in  1:length(Phi_type_vec)) {
  #  for (k in  1:length(aprx_ll_vec)) {
      for (l in  1:length(fast_log_vec)) {
        
        if (fast_log_vec[l] == TRUE)    { 
             exp_fast = TRUE
             log_fast = TRUE
        } else { 
          exp_fast = FALSE
          log_fast = FALSE
        }
        
        try({  
        file_name <- paste0("Info_DGP5_N", N_vec[i],
                            "type", run_type, "_",
                            "_n_chains64_burn250_iter500_AD0.8_LR0.1_width25_DenseTRUE_U_constFALSE_U_MTRUE_M_type_Hessian",
                            "_Phi_type_", Phi_type_vec[j],
                            "_aprx_exp_", exp_fast, 
                            "_aprx_log_", log_fast, 
                            "_n_ch_burn8_reps5")
        
       ##  "Info_DGP5_N25000_n_chains64_burn250_iter500_AD0.8_LR0.1_width25_DenseTRUE_U_constFALSE_U_MTRUE_M_type_Hessian_Phi_type_Phi_aprx_exp_TRUE_aprx_log_TRUE_n_ch_burn8_reps5"
        
        #Info_DGP5_N25000_n_chains64_burn250_iter500_AD0.8_LR0.1_width25_DenseTRUE_U_constFALSE_U_MTRUE_M_type_Hessian_Phi_type_Phi_approx_aprx_exp_TRUE_aprx_log_FALSE_n_ch_burn16_reps10
        #Info_DGP5_N25000_n_chains64_burn250_iter500_AD0.8_LR0.1_width25_DenseTRUE_U_constFALSE_U_MTRUE_M_type_Hessian_Phi_type_Phi_exp_fast_FALSE_log_fast_FALSE_n_ch_burn16_reps10
        
        mod  <- readRDS(file_name)
        {
          print(paste("N =", N_vec[i]))
          print(paste("Phi_type =", Phi_type_vec[j]))
          print(paste("_exp_fast_=", exp_fast))
          print(paste("_log_fast_=", log_fast))
          mod[[1]] <- NA
          print( (mod[[28]] ))
          print(mod[[22]] ) # print( round(  signif(as.numeric(substr(mod[[22]], start = 32, stop = 38), 3) )   ) , 3))
          print( mod[[24]] ) # , 3))
          print(  paste("Ratio = ",  signif( (mean(mod[[61]])    /  mean(mod[[53]]) ) / 8, 3) )  )
          
          vec_time_total    <- mod[[55]]
          vec_time_sampling <- mod[[57]]
          vec_time_burnin <- vec_time_total -  vec_time_sampling
          vec_ESS <- mod[[49]]
         # n_iter = 500
          
       
          time_1000_ESS <- vec_time_burnin + vec_time_sampling * (1000 / vec_ESS)
          mean_time_1000_ESS <- mean(time_1000_ESS)
          print(  paste("mean_time_1000_ESS = ",  signif(mean_time_1000_ESS, 3), "minutes" )  )
          
          if (N_vec[i] == 500)   target_ESS = 5000
          if (N_vec[i] == 1000)  target_ESS = 5000
          if (N_vec[i] == 2500)  target_ESS = 2500
          if (N_vec[i] == 5000)  target_ESS = 2000
          if (N_vec[i] == 12500) target_ESS = 500
          if (N_vec[i] == 25000) target_ESS = 500
          
          time_target_ESS <- vec_time_burnin + vec_time_sampling * (target_ESS / vec_ESS)
          mean_time_target_ESS <- mean(time_target_ESS)
          print(  paste("mean_time_target_ESS = ",  signif(mean_time_target_ESS, 3), "minutes" )  )
          
        }
        })
        
  #    }
    }
  }
}

}



require(Rmpfr)

BayesMVP::fn_ld_exp_bitshift_1(-52)

# for approx log using dooubles:
mpfr(BayesMVP::fn_ld_exp_bitshift_1(-52), precBits =  64)
mpfr(0x1.d8c0f0p-3, precBits = 64)
mpfr(-0x1.1de8dap-2, precBits = 64)
mpfr(0x1.53ca34p-2, precBits = 64)
mpfr(-0x1.fee25ap-2, precBits = 64)
mpfr(0x1.62e430p-1, precBits = 64)

# for apporox exp usiung doubles:
mpfr(BayesMVP::fn_ld_exp_bitshift_1(-52), precBits =  64)
mpfr(0x1.d8c0f0p-3, precBits = 64)
mpfr(-0x1.1de8dap-2, precBits = 64)
mpfr(0x1.53ca34p-2, precBits = 64)
mpfr(-0x1.fee25ap-2, precBits = 64)
mpfr(0x1.62e430p-1, precBits = 64)



# -0x1.6e75d58p+2 + 0x1.bba7414p+4 / (0x1.35eccbap+2 - z) - 0x1.f5e53c2p-2 * z; 
mpfr(-0x1.6e75d58p+2, precBits = 64)
mpfr( 0x1.bba7414p+4 ,  precBits = 64)
mpfr(0x1.35eccbap+2 , precBits = 64)
mpfr( 0x1.f5e53c2p-2 , precBits = 64)




set.seed(12345)
probs <-  runif(n = 1000)
z_vals <- rnorm(n = 1000, mean = 0, sd = 1)
 

probs_exact_Phi <-         BayesMVP::Phi_using_erfc_stan(z_vals)   # using EXACT  exp
probs_Phi_approx  <-       BayesMVP::Phi_approx_1_Eigen(z_vals)   # using EXACT  exp
probs_fast_Phi_approx   <- BayesMVP::fast_Phi_approx_1_Eigen(z_vals)  # using exp_fast

1000*mean(abs( BayesMVP::Phi_using_erfc_stan(z_vals) -     BayesMVP::Phi_approx_1_Eigen(z_vals)))
1000*mean(abs( BayesMVP::Phi_using_erfc_stan(z_vals) -     BayesMVP::fast_Phi_approx_1_Eigen(z_vals)))




1000*mean(abs(z_vals -  BayesMVP::qnorm_rcpp_vec(probs_exact_Phi)))  # using EXACT log  (no exp needed)
#1000*mean(abs(z_vals -  inv_Phi_approx_1_Eigen(probs_exact_Phi))) # using EXACT log and exp
1000*mean(abs(z_vals -  BayesMVP::qnorm_w_fastlog_rcpp_vec(probs_exact_Phi)))    # using log_fast (no exp needed)
#1000*mean(abs(z_vals -  fast_inv_Phi_approx_1_Eigen(probs_exact_Phi)))  # using log_fast and exp_fast


1000*mean(abs(z_vals -  BayesMVP::qnorm_rcpp_vec(probs_Phi_approx)))  # using EXACT log  (no exp needed)
1000*mean(abs(z_vals -  BayesMVP::inv_Phi_approx_1_Eigen(probs_Phi_approx))) # using EXACT log and exp
#1000*mean(abs(z_vals -  qnorm_w_fastlog_rcpp_vec(probs_Phi_approx)))    # using log_fast (no exp needed)
#1000*mean(abs(z_vals -  fast_inv_Phi_approx_1_Eigen(probs_Phi_approx)))  # using log_fast and exp_fast

#1000*mean(abs(z_vals - qnorm_rcpp_vec(probs_fast_Phi_approx)))  # using EXACT log  (no exp needed)
#1000*mean(abs(z_vals - inv_Phi_approx_1_Eigen(probs_fast_Phi_approx))) # using EXACT log and exp
1000*mean(abs(z_vals - BayesMVP::qnorm_w_fastlog_rcpp_vec(probs_fast_Phi_approx)))   # using log_fast (no exp needed)
1000*mean(abs(z_vals - BayesMVP::fast_inv_Phi_approx_1_Eigen(probs_fast_Phi_approx))) # using log_fast and exp_fast


100 * mean( (  (abs(z_vals -  BayesMVP::qnorm_rcpp_vec(probs_exact_Phi))) - (abs(z_vals - BayesMVP::qnorm_w_fastlog_rcpp_vec(probs_exact_Phi)))  )   )
100 * mean( (  (abs(z_vals -   BayesMVP::inv_Phi_approx_1_Eigen(probs_Phi_approx))) - (abs(z_vals - BayesMVP::fast_inv_Phi_approx_1_Eigen(probs_fast_Phi_approx)))  )    )

100 * mean( (  (abs(z_vals -  BayesMVP::qnorm_rcpp_vec(probs_exact_Phi))) - (abs(z_vals - BayesMVP::qnorm_w_fastlog_rcpp_vec(probs_exact_Phi)))  ) /   (abs(z_vals -   BayesMVP::qnorm_rcpp_vec(probs_exact_Phi)))  )
100 * mean( (  (abs(z_vals -   BayesMVP::inv_Phi_approx_1_Eigen(probs_Phi_approx))) - (abs(z_vals - BayesMVP::fast_inv_Phi_approx_1_Eigen(probs_fast_Phi_approx)))  ) /  (abs(z_vals -   BayesMVP::inv_Phi_approx_1_Eigen(probs_Phi_approx)))   )

1000*mean(abs(z_vals -  BayesMVP::inv_Phi_approx_1_Eigen(probs_Phi_approx))) - 1000*mean(abs(z_vals - BayesMVP::fast_inv_Phi_approx_1_Eigen(probs_fast_Phi_approx))) 



 # vals_to_log <-  runif(n = 1000, min = 0, max = 0.001)


vals_to_log <-  (1.0/probs_exact_Phi-1.0)

mean_error_vec <- c() 
mean_error_vec_1  <-  abs((  ((BayesMVP::my_faster_log_double_Eigen(vals_to_log)) - (log(vals_to_log))) ) )  ; mean(mean_error_vec_1)
mean_error_vec_2 <-   abs( (  (BayesMVP::fast_log_1_Eigen(vals_to_log) - log(vals_to_log))  )  ) ;  mean(mean_error_vec_2) # mean_error_vec[2]# / log(vals_to_log) ) 
plot(mean_error_vec_1, ylim = c(0, 0.0001), col = "red")
points(mean_error_vec_2,  col = "blue")

mean(mean_error_vec_1) / mean(mean_error_vec_2)


mean_error_vec <- c() 
mean_error_vec[1] <- mean( abs (  fast_log_1_Eigen_double_3(vals_to_log) -  log(vals_to_log) ) ) ; mean_error_vec[1]
mean_error_vec[1] <- mean( abs ( (fast_log_1_Eigen_double_2(vals_to_log)) -  (log(vals_to_log))) ) ; mean_error_vec[1] 
mean_error_vec[1] <- mean( abs ( (fast_log_1_Eigen_double_1(vals_to_log)) -  (log(vals_to_log))) ) ; mean_error_vec[1]#  / (abs(log(vals_to_log)))   )
mean_error_vec[2] <- mean( abs ( (BayesMVP::fast_log_1_Eigen(vals_to_log)) -  (log(vals_to_log))) ) ; mean_error_vec[2]
mean_error_vec[2] <- mean( abs(BayesMVP::fast_log_1_Eigen(vals_to_log) - log(vals_to_log))  ) # / log(vals_to_log) ) 
plot(mean_error_vec)


(vals_to_log - 1) -  0.5 * (vals_to_log - 1)^2 +    (1/3)*(vals_to_log - 1)^3 - (1/4)*   (vals_to_log - 1)^4  + (1/5)*   (vals_to_log - 1)^5
log(vals_to_log)

BayesMVP::fast_sinh_approx_1_double

BayesMVP::fast_arcsinh_approx_1_double(vals_to_log)
 

mean_error_vec[2] <- 100 *  mean( abs ( (fast_log_1_Eigen_double_1(vals_to_log)) -  (log(vals_to_log))) / (log(vals_to_log))  ) ; mean_error_vec[2]


mean_error_vec[2] <- 100 *  mean( abs ( (fast_log_1_Eigen_double_3(vals_to_log)) -  (log(vals_to_log))) / (log(vals_to_log))  ) ; mean_error_vec[2]

mean_error_vec[2] <- 100 * mean( abs ( (BayesMVP::fast_exp_1_Eigen(z_vals)) -  (exp(z_vals))) / exp(z_vals) ) ; mean_error_vec[2]

BayesMVP::fast_exp_0_Eigen(z_vals)


probs <-  runif(n = 1000,  min =  -5, max = 0)

mean_error_vec <- c() 
mean_error_vec[1] <- mean( abs (  BayesMVP::fast_exp_4_Eigen(probs) -  exp(probs) ) ) ; mean_error_vec[1]
mean_error_vec[1] <- mean( abs ( (BayesMVP::fast_exp_4_Eigen(probs)) -  (exp(probs))) ) ; mean_error_vec[1] 
 



mean_error_vec <- c() 
mean_error_vec[1] <- mean( abs (  fast_exp_4_Eigen_float(probs) -  exp(probs) ) ) ; mean_error_vec[1]
mean_error_vec[1] <- mean( abs (  fast_exp_4_Eigen(probs) -  exp(probs) ) ) ; mean_error_vec[1] 






-### Rcpp_Chol(Sigma_d)

 
 
 

# -- | ------------------------- Target-ESS pilot study stuff ---------------------------------------------------------------------------------------------------------------------
 
 
 
 
 {
   
   
   DGP = 5
   M_dense = TRUE
   metric_type = "Hessian"
 
   n_burnin_vec = c(250)
   learning_rate_main_vec = c(0.10)
   adapt_interval_width_vec = c(25)
   u_Euclidean_metric_const_vec =  FALSE # c(TRUE, FALSE)
   adapt_M_us_vec = TRUE #  c(TRUE, FALSE)
   adapt_delta_vec =  c(0.80) # c(0.65, 0.80)
 
   N_vec = c(500, 1000, 2500, 5000, 12500, 25000)
   n_chains_vec =  c(32) 
   n_chains_burnin_vec =  c(16) 
   device_vec <- c("HPC")
   
   Phi_type_vec <- c(1)
   
   outs_list_to_plot <- list()
   
   start_runs <- 1
   n_runs <- 10
   

   
   
   counter <- 1
   
 }
 
 



 
 
 {
   
   stuff_rds_list_of_lists <- list()
 
 counter <- 1
 
 for (r in 1:length(device_vec)) { 
  for (o in 1:length(N_vec)) {
    for (n in 1:length(n_chains_vec)) {
     for (i in 1:length(n_burnin_vec)) {
       for (m in 1:length(adapt_delta_vec)) {
         for (j in 1:length(learning_rate_main_vec)) {
           for (p in 1:length(adapt_interval_width_vec)) {
             for (s in 1:length(Phi_type_vec)) {
              for (k in 1:length(u_Euclidean_metric_const_vec)) {
                 for (l in 1:length(adapt_M_us_vec)) {
                  #  for (q in 1:length(n_iter_vec)) {
                      for (t in 1:length(n_chains_burnin_vec)) {
                
                       
                        n_chains = n_chains_vec[n]
                        
                        
                        N = N_vec[o]
                        
                        if (n_chains < 17) { 
                          n_iter =  4000
                        } else if (n_chains %in% c(18:32)) { 
                          if (N < 5001) n_iter =  8000  
                          else          n_iter =  2000  
                        } else { 
                          n_iter =  1000  
                        }
                        
                        
                       {
                       print(paste0("N = ",  N_vec[o]))
                       print(paste0("adapt_interval_width_vec = ",  adapt_interval_width_vec[p]))
                       print(paste0("n_iter_vec = ",  n_iter))
                       print(paste0("device_vec = ", device_vec[r]))
                       print(paste0("Phi_type = ",  Phi_type_vec[s]))
                       }
                       
                       try({  
                         
                         Phi_type =  Phi_type_vec[s]
                         N = N_vec[o]
                         n_chains_burnin = n_chains_burnin_vec[t] 
                           
                         ###
                         {
                           
                           file_name <- paste0(#"seed", df_i,"_", 
                             "DGP", DGP,"_", 
                             "N", N_vec[o],"_", 
                             "n_chains", n_chains_vec[n],"_", 
                             "burn", n_burnin_vec[i],"_", 
                             "iter", n_iter,"_", 
                             "AD", adapt_delta_vec[m],"_", 
                             "LR", learning_rate_main_vec[j],"_", 
                             "width", adapt_interval_width_vec[p],"_",
                            # "Dense", M_dense,"_",
                             "U_const",  u_Euclidean_metric_const_vec[k], "_",
                             "U_M", adapt_M_us_vec[l], "_",  
                             "M_type_", metric_type,"_",
                             "Phi_type_", Phi_type_vec[s]
                           )
                           
                           if (parallel::detectCores() < 17) { 
                             file_name <- paste0("Laptop_", file_name) 
                           } 
                           
                           file_name <- paste0( file_name, "_", "n_ch_burn", n_chains_burnin_vec[t] )
                           file_name <- paste0( file_name, "_", "reps", n_runs - start_runs + 1 )
                           
                           file_name = paste0("Info_", file_name)
                           
                           
                           
                           if (parallel::detectCores() < 17) { 
                             stuff_rds <- readRDS(    file_name)  
                           } else { 
                             stuff_rds <- readRDS(   file_name)
                           }
   
                           # run_index <- 1
                           # str(
                           #   stuff_rds[[1]][[run_index]]$trace_individual_log_lik_array 
                           #   )
                           
                           for (run_index in 1:n_runs) {
                              stuff_rds[[1]][[run_index]]$trace_individual_log_lik_array <- 0 
                           }
                           
                           
                           stuff_rds_list_of_lists[[counter]] <- stuff_rds 
                           
                           
                          ###  model_outs_list_of_lists[[counter]] <- stuff_rds[[1]]
                           
                         }
                       })
                         
                       counter = counter + 1
                       
                       gc(reset = TRUE)
                         
                       }
                     }
                   }
            #     }
               }
             }
           }
         }
       }
     }
   }

 }
 }
 
 
 
 
 
 
 

 
 
 
 {
   
   

   DGP = 5
   M_dense = TRUE
   metric_type = "Hessian"
   
  # n_iter_vec <- c(1000)
   n_burnin_vec = c(250)
   learning_rate_main_vec = c(0.10)
   adapt_interval_width_vec = c(25)
   u_Euclidean_metric_const_vec =  FALSE # c(TRUE, FALSE)
   adapt_M_us_vec = TRUE #  c(TRUE, FALSE)
   adapt_delta_vec =  c(0.80) # c(0.65, 0.80)
   
   N_vec = c(500, 1000, 2500, 5000, 12500, 25000)
   n_chains_vec =  c(32) 
   n_chains_burnin_vec =  c(16) 
   device_vec <- c("HPC")
   
   Phi_type_vec <- c(1)
   
   outs_list_to_plot <- list()
   
   start_runs <- 1
   n_runs <- 10
   
   Se_ests <- array(dim = c(n_runs, n_tests))
   Sp_ests <- array(dim = c(n_runs, n_tests))
   prev_ests <- c()
   
   
   df_target_ESS_pilot_study <- tibble(DGP = DGP, 
                                       N =  N ,    
                                       algorithm = "HI_HMC",  #
                                       n_runs = 5,
                                       device =   "HPC",
                                       n_chains = 64, 
                                       n_chains_burnin  =  NA,
                                       n_iter = 50,
                                       metric_type = "Hessian", 
                                       M_dense = M_dense, 
                                       n_burn = 500, LR = 0.05, adapt_int_width = 25, AD = 0.80 , U_M_const = FALSE, adapt_M_us = TRUE,
                                       ESS_per_sec_samp_mean =   NA_real_,    ESS_per_sec_samp_SD =   NA_real_, 
                                       ESS_per_sec_mean =    NA_real_,   ESS_per_sec_SD =   NA_real_,
                                       ESS_per_grad_samp_mean =   NA_real_, ESS_per_grad_samp_SD =  NA_real_,
                                       ESS_per_grad_mean =    NA_real_,     ESS_per_grad_SD =      NA_real_,
                                       time_100_ESS_mean = NA_real_,    time_100_ESS_SD =     NA_real_,
                                       time_1000_ESS_mean = NA_real_,   time_1000_ESS_SD =     NA_real_,
                                       grad_evals_1000_ESS_mean = NA_real_,  grad_evals_1000_ESS_SD = NA_real_, 
                                       L_burnin = NA_real_,  L_sampling = NA_real_ , ESS = NA_real_ ,
                                       Phi_type = NA_real_,
                                       n_iter_test = NA_real_, 
                                       n_divs_pct = NA_real_,
                                       min_ESS_test =  NA_real_,
                                       min_ESS_target_test = NA_real_,
                                       max_IQR = NA_real_,
                                       max_IQR_index =NA_real_,
                                       max_Range = NA_real_,
                                       max_Range_index = NA_real_, 
                                       max_Rhat = NA_real_,
                                       max_nested_Rhat = NA_real_ )
   
   
   
   
   counter_2 <- 1

 }
 
 



  



















































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































































dxs 1:n_tests) {
                                 iqr_vec[counter] <-   iqr(   Sp_ests[,t]  ) 
                                 iqr_vec_lower[counter] <- c(summary(Sp_ests[,t])[2])
                                 iqr_vec_upper[counter] <- c(summary(Sp_ests[,t])[5])
                                 print(paste0("IQR of Sp", t, " = ", iqr(   Sp_ests[,t]  )  ))  
                                 counter <- counter + 1
                               }
                               
                               iqr_vec[counter] <-   iqr(  prev_ests  ) 
                               iqr_vec_lower[counter] <- c(summary(prev_ests)[2])
                               iqr_vec_upper[counter] <- c(summary(prev_ests)[2])
                               print(paste0("IQR of prev", t, " = ",  iqr(  prev_ests  )  ))  
                               
                               print(paste0("max IQR = ", max(iqr_vec) ))
                             }
                             
                             counter <- 1
                             {
                               for (t in 1:n_tests) {
                                 range_vec[counter] <-   range(   Se_ests[,t]  )[2] - range(   Se_ests[,t]  )[1]
                                 range_vec_lower[counter] <- c(range(Se_ests[,t])[1])
                                 range_vec_upper[counter] <- c(range(Se_ests[,t])[2])
                                 print(paste0("Range of Se", t, " = ",     range_vec[counter] ) )
                                 counter <- counter + 1
                               }
                               for (t in 1:n_tests) {
                                 range_vec[counter] <-   range(   Sp_ests[,t]  )[2] - range(   Sp_ests[,t]  )[1]
                                 range_vec_lower[counter] <- c(range(Sp_ests[,t])[1])
                                 range_vec_upper[counter] <- c(range(Sp_ests[,t])[2])
                                 print(paste0("Range of Sp", t, " = ",    range_vec[counter]  ))
                                 counter <- counter + 1
                               }
                               
                               range_vec[counter] <-   range(   prev_ests  )[2] - range(   prev_ests   )[1]
                               range_vec_lower[counter] <- c(range(prev_ests)[1])
                               range_vec_upper[counter] <- c(range(prev_ests)[2])
                               print(paste0("Range of prev", t, " = ",    range_vec[counter]   ))
                               
                               print(paste0("max Range = ", max(range_vec) ))
                             }
                             
                             
                             
                             print(paste("|----------------------------------------|"))
                             
                             print(paste0("max IQR (lower value) = ", round(           iqr_vec_lower[which(iqr_vec == max(iqr_vec))]  , 3)))
                             print(paste0("max IQR (upper value) = ", round(           iqr_vec_upper[which(iqr_vec == max(iqr_vec))]  , 3)))
                             
                             print(paste0("max Range (lower value) = ",  round(           iqr_vec_lower[which(range_vec == max(range_vec))]  , 3)))
                             print(paste0("max Range (upper value) = ",  round(           iqr_vec_upper[which(range_vec == max(range_vec))]  , 3)))
                             
                             print(paste("|----------------------------------------|"))
                             
                             
                             print(paste("|----------------------------------------|"))
                             print(paste0("max Range = ", round(max(range_vec), 3)))
                             print(paste0("max Range index = ", which(range_vec == max(range_vec))))
                             print(paste0("max IQR = ",  round(max(iqr_vec), 3)))
                             print(paste0("max IQR index = ",  which(iqr_vec == max(iqr_vec))))  
                             print(paste("|----------------------------------------|"))
                             
                             
                             min_ESS_val <- (round(mean(min_ess_vec), 0))
                             ESS_diff_from_target <- abs(ESS_target - min_ESS_val)
                             ESS_diff_from_target_pct <- ( ESS_diff_from_target / ESS_target ) * 100
                             
                             
                             print(paste("|----------------------------------------|"))
                             # print(paste("ESS_diff_from_target_pct = ",   ESS_diff_from_target_pct ))
                             
                             if (n_iter_test != n_iter) {
                                 if (ESS_diff_from_target_pct %in% c(5:10)) { 
                                   cat(colourise(      (paste( "ESS_diff_from_target_pct  = ", ESS_diff_from_target_pct))      , "yellow"), "\n")
                                 } else  if (ESS_diff_from_target_pct %in% c(0:4.999)) { 
                                   cat(colourise(      (paste( "ESS_diff_from_target_pct  = ", ESS_diff_from_target_pct))      , "darkgreen"), "\n")
                                 } else { 
                                   cat(colourise(      (paste( "ESS_diff_from_target_pct  = ", ESS_diff_from_target_pct))      , "red"), "\n")
                                 }
                               
                             }
                             
                             print(paste("vec_min_ESS = ",   min_ESS_val ))
                             print(paste("N = ", mean(N)))
                             print(paste("n_iter = ", mean(n_iter)))
                             print(paste("n_iter_test = ", mean(n_iter_test)))
                             print(paste("Phi_type = ", Phi_type))
                             print(paste("n_divs (%) = ", round( 100 * mean(vec_divs / (n_iter_test * n_chains) ), 4) ))
                             print(paste("|----------------------------------------|"))
                           }
                       })
                     
                     
                     try({   
                         {
                             df_row <-  tibble(DGP = DGP, 
                                               N = N_vec[o] ,    
                                               algorithm = "HI_HMC",  
                                               n_runs =      as.numeric(strsplit( stuff_rds[[39]], "[=]")[[1]][2]),
                                               device = device_vec[r],
                                               n_chains = n_chains_vec[n], 
                                               n_chains_burnin  =  n_chains_burnin_vec[t],
                                               n_iter = n_iter,
                                               metric_type = metric_type, 
                                               M_dense = M_dense, 
                                               n_burn = n_burnin_vec[i], 
                                               LR = learning_rate_main_vec[j], 
                                               adapt_int_width = adapt_interval_width_vec[p], 
                                               AD =  adapt_delta_vec[m] , 
                                               U_M_const = u_Euclidean_metric_const_vec[k], 
                                               adapt_M_us = adapt_M_us_vec[l], 
                                               ESS_per_sec_samp_mean =   round( as.numeric(    strsplit(  stuff_rds[[22]], "[ing =]")[[1]][10]   ) , 3),   
                                               ESS_per_sec_samp_SD =   round( as.numeric(    strsplit(stuff_rds[[22]], "[ing =]")[[1]][11]  ) , 3),    
                                               ESS_per_sec_mean =     round( as.numeric(    strsplit(stuff_rds[[21]], "[all =]")[[1]] [8] ) , 3),    
                                               ESS_per_sec_SD =  round( as.numeric(    strsplit(stuff_rds[[21]], "[all =]")[[1]] [9] ) , 3), 
                                               ESS_per_grad_samp_mean = round( min_ESS_per_grad_samp_test_mean, 3),   
                                               ESS_per_grad_samp_SD =  round( min_ESS_per_grad_samp_test_SD , 3),  
                                               ESS_per_grad_mean =    round( as.numeric(    strsplit(stuff_rds[[23]], "[all =]")[[1]][9] ) , 3),    
                                               ESS_per_grad_SD =  round( as.numeric(    strsplit(stuff_rds[[23]], "[all =]")[[1]][10]  ) , 3), 
                                               time_100_ESS_mean = round( as.numeric(   strsplit(stuff_rds[[25]], "[_ESS = ]")[[1]][[11]] ) , 3),      
                                               time_100_ESS_SD = round( as.numeric(   strsplit(stuff_rds[[25]], "[_ESS = ]")[[1]][[12]] ) , 3), 
                                               time_1000_ESS_mean =round( as.numeric(    strsplit(stuff_rds[[26]], "[_ESS = ]")[[1]][[11]] ) , 3),       
                                               time_1000_ESS_SD = round( as.numeric(   strsplit(stuff_rds[[26]], "[_ESS = ]")[[1]][[12]] ) , 3), 
                                               grad_evals_1000_ESS_mean =  round( as.numeric(    strsplit( stuff_rds[[39]], split = "\\s+" )[[1]][1]   ) , 3), 
                                               grad_evals_1000_ESS_SD = round( as.numeric(   strsplit( stuff_rds[[39]], split = "\\s+" )[[1]][2]  ) , 3), 
                                               L_burnin =  round( as.numeric(    strsplit(stuff_rds[[14]], "[=]")[[1]][2]  ) , 3), 
                                               L_sampling =  round( as.numeric(        strsplit(stuff_rds[[15]], "[=]")[[1]][2]  ) , 3), 
                                               ESS = round( as.numeric(    strsplit(stuff_rds[[16]], "[=]")[[1]][2]  ) , 0), 
                                               Phi_type = Phi_type_vec[s] ,
                                               n_iter_test = n_iter_test, 
                                               n_divs_pct = 100 *  round(mean(vec_divs / (n_iter_test * n_chains) ), 4) ,
                                               min_ESS_test =  (round(mean(min_ess_vec), 0)) ,
                                               min_ESS_target_test = ESS_target,
                                               max_IQR = round(max(iqr_vec), 3) ,
                                               max_IQR_index =  which(iqr_vec == max(iqr_vec)) ,
                                               max_Range = round(max(range_vec), 3) ,
                                               max_Range_index =  which(range_vec == max(range_vec)) , 
                                               max_Rhat =  round(max(rhat_vec), 4) ,
                                               max_nested_Rhat =  round(max(rhat_nested_vec), 4) 
                             )
                                         
                                               
                             
                             
                             
                             print(df_row)
                             try({  
                               df_target_ESS_pilot_study <- rbind(df_target_ESS_pilot_study, df_row)
                             })
                             
                             print(df_target_ESS_pilot_study)
                         }
                     })
                     
                    
                     
                     
                     
                     
                     }  ### end of "for (ii in 1:length(ESS_target_vec))"
                     
                     
                     
                     
                     })
                     
                       
                       counter_2 = counter_2 + 1
                       
                     
                     
                   }
                 }
               }
          #   }
           }
         }
       }
     }
   }
   }
   }
 }

               


}      
 
 


 




  
  try({ 
{
  
  
 
     
     df_target_ESS_pilot_study %>% dplyr::select(N, 
                                                 LR,
                                                 n_burn, 
                                                 Phi_type,
                                                 min_ESS_target_test, 
                                                 min_ESS_test, 
                                                 n_iter_test, 
                                                 n_divs_pct, 
                                                 max_IQR,
                                                 max_IQR_index,
                                                 max_Range,
                                                 max_Range_index)   
     
  #                                                      #  dplyr::filter(ESS_target_within_10_pct  == 1) %>%  ####  
     
  
     df_target_ESS_pilot_study_simplified_subset <- df_target_ESS_pilot_study %>% dplyr::select(N, LR, n_burn, 
                                                                                                 Phi_type,
                                                                                                 min_ESS_target_test, 
                                                                                                 min_ESS_test, 
                                                                                                 n_iter_test, 
                                                                                                 n_divs_pct, 
                                                                                                 max_IQR,
                                                                                                 max_IQR_index,
                                                                                                 max_Range,
                                                                                                max_Range_index,
                                                                                                ESS_per_grad_samp_mean,
                                                                                                ESS_per_grad_samp_SD,
                                                                                                ESS_per_sec_samp_mean,
                                                                                                ESS_per_sec_samp_SD,
                                                                                                max_Rhat,
                                                                                                max_nested_Rhat
                                                                                                ) %>%
                                                       dplyr::mutate(ESS_target_m_actual = abs(min_ESS_target_test - min_ESS_test) ) %>%
                                                       dplyr::mutate(ESS_target_within_5_pct  =  ifelse(  abs(ESS_target_m_actual / min_ESS_test ) * 100 < 5, 1, 0)) %>%
                                                       dplyr::mutate(ESS_target_within_10_pct =  ifelse(  abs(ESS_target_m_actual / min_ESS_test ) * 100 < 10 , 1, 0)) %>%
                                                       dplyr::mutate(n_iter_test_2 =  round( (min_ESS_target_test / min_ESS_test ) * n_iter_test, 0) ) %>% 
                                                       dplyr::arrange(Phi_type, N) %>% dplyr::select(N, 
                                                                                                     # LR, 
                                                                                                     # n_burn, 
                                                                                                  #   n_chains,
                                                                                                   #  Phi_type,
                                                                                                     min_ESS_target_test, 
                                                                                                     min_ESS_test, 
                                                                                                     n_iter_test, 
                                                                                                     n_iter_test_2,
                                                                                                     n_divs_pct, 
                                                                                                     max_IQR,
                                                                                                     max_IQR_index,
                                                                                                     max_Range,
                                                                                                     max_Range_index,
                                                                                                     ESS_target_within_5_pct,
                                                                                                  ESS_target_within_10_pct,
                                                                                                  ESS_per_grad_samp_mean,
                                                                                                  ESS_per_grad_samp_SD,
                                                                                                  ESS_per_sec_samp_mean,
                                                                                                  ESS_per_sec_samp_SD,
                                                                                                  max_Rhat,
                                                                                                  max_nested_Rhat
                                                                                                  ) # %>%
       #dplyr::filter(n_iter_test < n_iter) 
     
     
     df_target_ESS_pilot_study_simplified_subset %>%    filter(Phi_type == 3) %>%    print(n = 1000) 
     df_target_ESS_pilot_study_simplified_subset %>%    filter(Phi_type == 1) %>%    print(n = 1000)
     # 
     # df_target_ESS_pilot_study_simplified_subset %>%    filter(Phi_type == 3) %>%  
     #   select(N, n_iter_test, n_iter_test_2)  %>%  
     # #  arrange(N, -min_ESS_target_test) %>% 
     #   print(n = 1000) 
     
     df_target_ESS_pilot_study_simplified_subset %>%    
       filter(Phi_type == 1) %>% 
     #  select(N, n_iter_test, n_iter_test_2)  %>% 
       arrange(N, -min_ESS_target_test) %>%  
       print(n = 1000)

 
     
}
  })
 
 
 
 
 
 
 
 
 
 


  
  
  {
 
 
 
 df_target_ESS_pilot_study_2  <-  df_target_ESS_pilot_study %>%    dplyr::mutate(ESS_target_m_actual = abs(min_ESS_target_test - min_ESS_test) ) %>%
   dplyr::mutate(ESS_target_within_5_pct =  ifelse( (ESS_target_m_actual / min_ESS_test ) * 100 < 5, 1, 0)) %>%
   dplyr::mutate(n_iter_test_2 =  round( (min_ESS_target_test / min_ESS_test ) * n_iter_test, 0) ) 
 
 
 df_target_ESS_pilot_study_3 <- df_target_ESS_pilot_study_2 %>%   dplyr::filter(ESS_target_within_5_pct  == 1) 
 
 
 
 df_target_ESS_pilot_study_3$adapt_int_width <- factor(df_target_ESS_pilot_study_3$adapt_int_width)
 df_target_ESS_pilot_study_3$Phi_type <- factor(df_target_ESS_pilot_study_3$Phi_type)
 
 df_target_ESS_pilot_study_3 <- dplyr::mutate(df_target_ESS_pilot_study_3, N_label =  paste0("N = ",  df_target_ESS_pilot_study_3$N))
 df_target_ESS_pilot_study_3 <- dplyr::mutate(df_target_ESS_pilot_study_3, N_chains_label =  paste0("N_chains = ",  df_target_ESS_pilot_study_3$n_chains))
 
 df_target_ESS_pilot_study_3$N_label <- factor( df_target_ESS_pilot_study_3$N_label )
 
 df_target_ESS_pilot_study_3$N_label <- factor(df_target_ESS_pilot_study_3$N_label,  levels = c("N = 500", "N = 1000", "N = 2500", "N = 5000", "N = 12500", "N = 25000"))
 # df_target_ESS_pilot_study_3$N_label 
 
 df_target_ESS_pilot_study_3$N_chains_label <- factor(df_target_ESS_pilot_study_3$N_chains_label, levels = c( "N_chains = 4", "N_chains = 8", "N_chains = 16", "N_chains = 32", "N_chains = 64" ))
 # df_target_ESS_pilot_study_3$N_chains_label 
 
 
 
 df_target_ESS_pilot_study_3$min_ESS_target_test <- factor(df_target_ESS_pilot_study_3$min_ESS_target_test )
 
 
 df_target_ESS_pilot_study_3 <- df_target_ESS_pilot_study_3 %>% dplyr::mutate(
   # target_ESS = case_when(
   #   N == 500   ~ 1000, 
   #   N == 1000  ~ 1000, 
   #   N == 2500  ~ 1000,
   #   N == 5000  ~ 1000,
   #   N == 12500 ~ 500, 
   #   N == 25000 ~ 500),
   # N_iter_target_ESS_64_ch =   round((min_ESS_target_test/ESS) * n_iter *    (n_chains / 64)  , 0), 
   # N_iter_target_ESS_8_ch =   round((min_ESS_target_test/ESS) * n_iter *    (n_chains / 8)  , 0), 
   N_iter_target_ESS_for_chosen_N_ch = round( ( as.numeric(as.character(min_ESS_target_test))/ESS) * n_iter , 0), 
   time_sampling = ESS * as.numeric(as.character(1 / as.numeric(as.character(ESS_per_sec_samp_mean)) ) ), 
   time_total   =  ESS * as.numeric(as.character(1 / as.numeric(as.character(ESS_per_sec_mean)) ) ),
   time_burnin = (time_total - time_sampling) / 60,
   time_sampling =  ( time_sampling * (min_ESS_test / ESS) ) / 60,
   time_total   =  (time_burnin + time_sampling) ,
   time_target_ESS_mean =  time_burnin +   time_sampling * ( as.numeric(as.character(min_ESS_target_test)) / min_ESS_test )   ,
   time_target_ESS_1000_ESS_ratio = time_target_ESS_mean / as.numeric(as.character(time_1000_ESS_mean)),
   time_target_ESS_SD = as.numeric(as.character(time_1000_ESS_SD)) * time_target_ESS_1000_ESS_ratio
 )
 
 
 
 
}

 

# plots

 
 {
   
   plot_pilot_study_target_ESS_plot_IQR <-  ggplot(data = df_target_ESS_pilot_study_3, 
                                     aes(x = min_ESS_target_test, 
                                         y =  max_IQR, 
                                         colour = Phi_type, 
                                         shape =  adapt_int_width) ) +
   geom_point(size = 3, aes(x =  min_ESS_target_test, y = max_IQR), position = position_jitter(width = 0.01, height = 0, seed = 123))  + 
   theme_bw(base_size = 16) + 
   facet_wrap( ~   N_label  ,
               scales = "free", 
               ncol = 2)  + 
   ylab("Max IQR") + 
   xlab("Min ESS") + 
   theme(legend.position = "bottom")  + 
   #scale_x_continuous(breaks = c(250, 500, 1000, 2000, 2500, 5000, 10000)) + 
   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
     geom_hline(yintercept = 0.25,  linetype="dashed", color = "red", size = 1)#  + 
   #  geom_hline(yintercept = 0.40,  linetype="dashed", color = "red", size = 1) 
  
 plot_pilot_study_target_ESS_plot_IQR
 
 }
 

 
 {
   
   plot_pilot_study_target_ESS_plot_Range <-  ggplot(data = df_target_ESS_pilot_study_3, 
                                     aes(x = min_ESS_target_test, 
                                         y =  max_Range, 
                                         colour = Phi_type, 
                                         shape =  adapt_int_width) ) +
   geom_point(size = 3, aes(x =  min_ESS_target_test, y = max_Range), position = position_jitter(width = 0.01, height = 0, seed = 123))  + 
   theme_bw(base_size = 16) + 
   facet_wrap( ~   N_label  ,
               scales = "free", 
               ncol = 2)  + 
   ylab("Max Range") + 
   xlab("Min ESS") + 
   theme(legend.position = "bottom")  + 
   # scale_x_continuous(breaks = c(250, 500, 1000, 2000, 2500, 5000, 10000)) + 
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))  + 
     geom_hline(yintercept = 0.75,  linetype="dashed", color = "red", size = 1) # + 
   #  geom_hline(yintercept = 0.80,  linetype="dashed", color = "red", size = 1) 
 
     plot_pilot_study_target_ESS_plot_Range 
 
 
 }
 

 
    

plot_pilot_study_target_ESS_plot_IQR +
  plot_pilot_study_target_ESS_plot_Range 
    

{
    png("Figure_target_ESS_pilot_study_1.png" ,units = "in", width = 16, height=9, res=800)
    plot_pilot_study_target_ESS_plot_IQR +
      plot_pilot_study_target_ESS_plot_Range 
    dev.off()
}
    



{
  
  df_target_ESS_pilot_study_3_stacked_for_ggplot_sec_1 <- mutate(df_target_ESS_pilot_study_3, value = max_Range, Type = "max(Range)")
  df_target_ESS_pilot_study_3_stacked_for_ggplot_sec_2 <- mutate(df_target_ESS_pilot_study_3, value = max_IQR, Type = "max(IQR)")
  
  df_target_ESS_pilot_study_3_stacked_for_ggplot <-  rbind(df_target_ESS_pilot_study_3_stacked_for_ggplot_sec_1, 
                                                           df_target_ESS_pilot_study_3_stacked_for_ggplot_sec_2)
  
  
  df_target_ESS_pilot_study_3_stacked_for_ggplot  %>% select(              N,        min_ESS_test, 
                                                                                                     n_iter_test, 
                                                                                                     n_iter_test_2,
                                                                                                     n_divs_pct, 
                                                                                                     max_IQR,
                                                                                                     max_IQR_index,
                                                                                                     max_Range,
                                                                                                     max_Range_index,
                                                                                                     ESS_target_within_5_pct,
                                                                                                #  ESS_target_within_10_pct,
                                                                                                #  ESS_per_grad_samp_mean,
                                                                                                #  ESS_per_grad_samp_SD,
                                                                                               #   ESS_per_sec_samp_mean,
                                                                                               #   ESS_per_sec_samp_SD,
                                                                                                 # max_Rhat,
                                                                                                  max_nested_Rhat) %>% print(n  = 1000)
  
  
  plot_pilot_study_target_ESS_plot_Range_and_IQR <-  ggplot(data = df_target_ESS_pilot_study_3_stacked_for_ggplot, 
                                                    aes(x = min_ESS_target_test, 
                                                        y =  value, 
                                                        # colour = Phi_type, 
                                                        colour =  Type) ) +
     geom_point(size = 3, aes(x =  min_ESS_target_test, y = value), position = position_jitter(width = 0.01, height = 0, seed = 123))  + 
    #geom_point(size = 3, aes(x =  min_ESS_target_test, y = max_IQR),   position = position_jitter(width = 0.01, height = 0, seed = 123))  + 
    theme_bw(base_size = 16) + 
    facet_wrap( ~   N_label  ,
                scales = "free", 
                ncol = 2)  + 
    ylab("Max Range (blue) and IQR (red)") + 
    xlab("Min ESS") + 
    theme(legend.position = "bottom")  + 
    # scale_x_continuous(breaks = c(250, 500, 1000, 2000, 2500, 5000, 10000)) + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))  + 
    geom_hline(yintercept = 0.50,  linetype="dashed", color = "red", size = 0.5) + 
    geom_hline(yintercept = 0.75,  linetype="dashed", color = "skyblue", size = 0.5) 
  
  plot_pilot_study_target_ESS_plot_Range_and_IQR 
  
  
}


{
  png("Figure_target_ESS_pilot_study_2.png" ,units = "in", width = 4*3, height=3*3, res=800)
  plot_pilot_study_target_ESS_plot_Range_and_IQR
  dev.off()
}





    
    
    
    {
   
   # get "final" dataset which has Phi_type and target_ESS to use for each N. 
   {
     
 
     
     
     print(df_target_ESS_pilot_study_3, n = 1000)
     
     df_target_ESS_pilot_study_4 <- df_target_ESS_pilot_study_3 %>%
                                   dplyr::mutate(Accept_target_ESS = ifelse((max_IQR < max_IQR_limit) & (max_Range < max_range_limit),   ##############################
                                                                           1, 0)) %>% 
                                    dplyr::filter(Accept_target_ESS == 1) %>%
     dplyr::arrange(N, Phi_type, min_ESS_target_test)
     
     print(df_target_ESS_pilot_study_4, n = 1000)
 
 
     
     df_target_ESS_pilot_study_5 <- df_target_ESS_pilot_study_4[1,]
 
     
     N_vec <- unique(df_target_ESS_pilot_study_4$N)
     
     try({  
  for (i in 1:length(N_vec)) {
    for (j in 1:length(Phi_type_vec)) {
    
          
      try({  
           df_target_ESS_pilot_study_subset <-   df_target_ESS_pilot_study_4 %>% 
                                                 dplyr::filter(N == N_vec[i])  %>% 
                                                 filter(Phi_type == Phi_type_vec[j])  
      })
           
      try({  
           accept_indicators_reverse <- rev(df_target_ESS_pilot_study_subset$Accept_target_ESS)
      })
           
           try({  
           counter = 0
             for (ii in 1:nrow(df_target_ESS_pilot_study_subset)) { 
                 if (accept_indicators_reverse[ii] == 1) { 
                   counter = counter + 1
                 } else { 
                   break
                 }
               index_of_chosen_ESS = nrow(df_target_ESS_pilot_study_subset) - counter + 1
             }
           })
           
           
       
      try({ 
           df_target_ESS_pilot_study_5 <-   rbind(df_target_ESS_pilot_study_5,   
                                                  df_target_ESS_pilot_study_subset[index_of_chosen_ESS,])
      })
     
    }
  }
     })
     
     df_target_ESS_pilot_study_6 <- df_target_ESS_pilot_study_5[-1, ]
     
   
 
     
     df_target_ESS_pilot_study_6_less_variables <-      df_target_ESS_pilot_study_6  %>% dplyr::select(N, LR, n_burn, 
                                                                                                           Phi_type,
                                                                                                           min_ESS_target_test, 
                                                                                                           min_ESS_test, 
                                                                                                           n_iter_test, 
                                                                                                           n_divs_pct, 
                                                                                                           max_IQR,
                                                                                                           max_IQR_index,
                                                                                                           max_Range,
                                                                                                           max_Range_index, 
                                                                                                           time_total,
                                                                                                           time_sampling,
                                                                                                           time_target_ESS_mean,
                                                                                                           ESS_per_sec_samp_mean, 
                                                                                                       ESS_per_grad_samp_mean,
                                                                                                       ESS_per_grad_samp_SD,
                                                                                                           ESS_per_sec_samp_SD, 
                                                                                                           max_nested_Rhat,
                                                                                                           max_Rhat)
     
     
       
       
       
     df_target_ESS_pilot_study_6_less_variables  %>%    dplyr::filter(Phi_type == 1)  
     df_target_ESS_pilot_study_6_less_variables  %>%    dplyr::filter(Phi_type == 3)  
     
     
 
       
       df_target_ESS_pilot_study_4_subset_Phi_type_3 <- df_target_ESS_pilot_study_6_less_variables %>%     
         dplyr::filter(Phi_type == 3)    %>% print()
       
       
     df_target_ESS_pilot_study_4_subset_Phi_type_1 <- df_target_ESS_pilot_study_6_less_variables %>%     
       dplyr::filter(Phi_type == 1)    %>% print()
     
     
     
     df_target_ESS_pilot_study_4_subset_Phi_type_1_selected_N <- df_target_ESS_pilot_study_4_subset_Phi_type_3 %>%    
       dplyr::filter(N %in% c(500, 
                              1000,
                              2500,
                              5000
                            #  12500,
                              #25000
                              ))  
     
     
     df_target_ESS_pilot_study_4_subset_Phi_type_3_selected_N <- df_target_ESS_pilot_study_4_subset_Phi_type_1 %>%     
       dplyr::filter(N %in% c(0, 
                              12500,
                              25000
                              ))   
     
     
     
     
     
     df_target_ESS_pilot_study_4_subset_FINAL <- rbind(df_target_ESS_pilot_study_4_subset_Phi_type_1_selected_N, 
                                                       df_target_ESS_pilot_study_4_subset_Phi_type_3_selected_N)
     
     
     
     }

    
    
    


    {
       df_target_ESS_pilot_study_4_subset_Phi_type_3   %>% dplyr::select(N, 
                                                                         #LR,
                                                                         #n_burn, 
                                                                         Phi_type,
                                                                         min_ESS_target_test, 
                                                                         min_ESS_test, 
                                                                         n_iter_test, 
                                                                         n_divs_pct, 
                                                                         max_IQR,
                                                                       #  max_IQR_index,
                                                                         max_Range,
                                                                        # max_Range_index, 
                                                                       #  time_total,
                                                                       #  time_sampling,
                                                                       ESS_per_grad_samp_mean,
                                                                       ESS_per_grad_samp_SD,
                                                                         max_nested_Rhat,
                                                                         max_Rhat) %>% print()
      
       df_target_ESS_pilot_study_4_subset_Phi_type_1  %>% dplyr::select(N, 
                                                                        #LR,
                                                                        #n_burn, 
                                                                        Phi_type,
                                                                        min_ESS_target_test, 
                                                                        min_ESS_test, 
                                                                        n_iter_test, 
                                                                        n_divs_pct, 
                                                                        max_IQR,
                                                                       # max_IQR_index,
                                                                        max_Range,
                                                                      #  max_Range_index, 
                                                                     #   time_total,
                                                                     #   time_sampling,
                                                                      ESS_per_grad_samp_mean,
                                                                      ESS_per_grad_samp_SD,
                                                                        max_nested_Rhat,
                                                                        max_Rhat) %>% print()
       # 
       # df_target_ESS_pilot_study_4_subset_FINAL      %>% dplyr::select(N,
       #                                                                 #LR,
       #                                                                 #n_burn, 
       #                                                                 Phi_type,
       #                                                                 min_ESS_target_test, 
       #                                                                 min_ESS_test, 
       #                                                                 n_iter_test, 
       #                                                                 n_divs_pct, 
       #                                                                 max_IQR,
       #                                                               #  max_IQR_index,
       #                                                                 max_Range,
       #                                                               #  max_Range_index, 
       #                                                              #   time_total,
       #                                                              #   time_sampling,
       #                                                               ESS_per_grad_samp_mean,
       #                                                               ESS_per_grad_samp_SD,
       #                                                                 max_nested_Rhat,
       #                                                                 max_Rhat)  %>% print()
       
       print(paste("max_IQR_limit = ", max_IQR_limit ))
       print(paste("max_range_limit = ", max_range_limit ))
       
       
       
    }
           

   }
   
   
   
   
   
   
   
    # 
    #   max_range_limit <-  1.00 ;     max_IQR_limit   <-  0.50
    # # max_range_limit <-  1.00 ;     max_IQR_limit   <-  0.45
    # # max_range_limit <-  0.80 ;     max_IQR_limit   <-  0.50
    # #      max_range_limit <-  0.75 ;     max_IQR_limit   <-  0.50
    # #  max_range_limit <-  0.75 ;     max_IQR_limit   <-  0.25
    #   max_range_limit <-  0.50 ;     max_IQR_limit   <-  0.25
    # 
    # #  max_range_limit <-  0.70 ;     max_IQR_limit   <-  0.35
    #    max_range_limit <-  0.60 ;     max_IQR_limit   <-  0.35
    # 
    # 
    # #  max_range_limit <-  0.70 ;     max_IQR_limit   <-  0.25
        max_range_limit <-  0.75 ;     max_IQR_limit   <-  0.25
        max_range_limit <-  0.75 ;     max_IQR_limit   <-  0.5
 
    # 
    # 
     #  max_range_limit <-  0.80 ;     max_IQR_limit   <-  0.40
   
   
    #  max_range_limit <-  0.70  ;     max_IQR_limit   <-  0.25
    #   max_range_limit <-  0.75 ;     max_IQR_limit   <-  0.25
    #  #  max_range_limit <-  0.75 ;     max_IQR_limit   <-  0.20
      
      
    #   max_range_limit <-  0.60  ;     max_IQR_limit   <-  0.30
   
      
      
     { 
       
           df_target_ESS_pilot_study_simplified_subset %>%   
             filter(Phi_type == 1) %>%   
             mutate(cond_satisfied = ifelse( (max_IQR < max_IQR_limit) & (max_Range < max_range_limit), 1, 0)) %>%
             select(- ESS_target_within_5_pct, - n_iter_test_2, - Phi_type)        %>%  
             print(n = 1)  
          
             df_target_ESS_pilot_study_simplified_subset_2 <-  df_target_ESS_pilot_study_simplified_subset   %>%   
                                                               mutate(cond_satisfied = ifelse( (max_IQR < max_IQR_limit) & (max_Range < max_range_limit), 1, 0)) %>% 
                                                               group_by(N)  %>%
                                                               arrange(N, - min_ESS_target_test) %>%
                                                               mutate(cond_2 = cumsum(cond_satisfied)) %>% 
                                                               slice(seq_len(min(which(cond_satisfied == 0)))) %>%
                                                               select(- ESS_target_within_5_pct, - n_iter_test_2, - Phi_type) %>% 
                                                               mutate(cond_satisfied_lag_1 = lag(cond_satisfied)) %>%
                                                              # filter(cond_satisfied == 1, cond_satisfied_lag_1 == 1) %>% # optional
                                                               filter(cond_satisfied == 1 ) %>%
                                                               group_by(N)  %>%
                                                               filter(min_ESS_target_test == min(min_ESS_target_test)) %>% 
                                                               print(n = 100)
                                                            
           
           
           {
             print(paste("max_IQR_limit = ", max_IQR_limit ))
             print(paste("max_range_limit = ", max_range_limit ))
             print(paste("inc = ", inc ))
           }
     
     }
   
        
        
        
        
        
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#     
           #    
           #      if (corr_force_positive == TRUE)   {
           #          lb = 0 
           #        }  else { 
           #          lb = -1
           #        }
           #      
           #      
           # #     str(model_outs$outs)
           #      
           #    #  model_outs$outs[[1]][[16]]
           #          Omega_uppers <- Omega_lowers <- Omega_medians <- list() 
           #          L_Omega_medians <- L_Omega_lowers <- L_Omega_uppers <- list() 
           #          Omega_vec_per_chain  <- array(dim = c(n_chains, n_iter, n_tests * (n_tests - 1) * 0.5))
           #      
           #          L_Omega_per_chain <- Omega_per_chain <- list()
           #          
           #          class <- 2
           #          
           #      for (kk in 1:n_chains) { 
           #        
           #        samps <-   model_outs$outs[[1]][[kk]][1,,1:20]
           #        
           #        for (ii in 1:n_iter) {
           #         
           #            try({ 
           #             if (corr_param == "Chol_Nump")  L_Omega_per_chain[[ii]] <-  lcmMVPbetav2::Nump_cholesky_corr_lp_double(n_tests, lb = lb, ub = +1,  lcmMVPbetav2::fn_convert_std_vec_of_corrs_to_3d_array(samps[ii, ], n_tests, 2)[[class]] )[2:(n_tests + 1), 1:n_tests]
           #             if (corr_param == "Arch") {
           #                if (corr_force_positive == TRUE)    L_Omega_per_chain[[ii]] <-   lcmMVPbetav2::corr_constrain_Archakov_Spinkney_double(gamma_in =  lcmMVPbetav2::fn_convert_std_vec_of_corrs_to_3d_array( corr_pos_offset + exp(samps[ii, ]), n_tests, 2)[[class]], dim = n_tests, tol = 1e-6)[2:(1 + n_tests), 1:n_tests]
           #                else                                L_Omega_per_chain[[ii]] <-   lcmMVPbetav2::corr_constrain_Archakov_Spinkney_double(gamma_in =  lcmMVPbetav2::fn_convert_std_vec_of_corrs_to_3d_array(samps[ii, ], n_tests, 2)[[class]], dim = n_tests, tol = 1e-6)[2:(1 + n_tests), 1:n_tests]
           #             }
           #            })
           #             
           #             Omega_per_chain[[ii]] <- L_Omega_per_chain[[ii]] %*% t((L_Omega_per_chain[[ii]])) ;  
           #             Omega_vec_per_chain[kk, ii, ] <- lcmMVPbetav2::fn_convert_3d_array_of_corrs_to_std_vec(list(Omega_per_chain[[ii]]) , n_tests, 1)
           #         
           #        }
           #      }
           #      
           #      
           #          
           #        round( Reduce("+", Omega_per_chain) / length(Omega_per_chain) , 2)
           #          
           #         round( apply(Omega_vec_per_chain, 3, quantile, probs = c(0.025, 0.50, 0.975)), 2)
           #   
           #         sd( apply(Omega_vec_per_chain, 3, quantile, probs = c(0.025, 0.50, 0.975))[1,])
           #         sd( apply(Omega_vec_per_chain, 3, quantile, probs = c(0.025, 0.50, 0.975))[2,])
           #         sd( apply(Omega_vec_per_chain, 3, quantile, probs = c(0.025, 0.50, 0.975))[3,])
           #         
           #      # Omega_median <- Reduce("+", Omega_medians) / n_chains ; Omega_median
           #      # Omega_upper <- Reduce("+", Omega_uppers) / n_chains ; Omega_upper
           #      # Omega_lower <- Reduce("+", Omega_lowers) / n_chains ; Omega_lower
           #      
           # 
           # schur_outs =   Schur_cholesky_corr_lp_double(n = n_tests, lb = 0, ub = +1,  lcmMVPbetav2::fn_convert_std_vec_of_corrs_to_3d_array(samps[ii, ], n_tests, 2)[[class]]     )
           # schur_outs[1,1]
           # 
           # Jacobian_square_wrt_L =  schur_outs[22:31, 1:10]
           # 
           # sum(Jacobian_square_wrt_L)
           # 
           # d_z_d_unc_full =  schur_outs[33:42, 1:10]
           # sum(d_z_d_unc_full)
           # 
           # sum(d_z_d_unc_full) +    sum(Jacobian_square_wrt_L)
           # 
           # schur_outs[1,1]
           # 
           # 
           # Jacobian <-  diag(exp(diag(Jacobian_square_wrt_L))) %*%     diag(exp(diag(d_z_d_unc_full)))
           # 
           # sum(log(det(Jacobian)))
           # 
           # 
           # nump_outs =   Nump_cholesky_corr_lp_double(n = n_tests, lb = -1, ub = +1,  lcmMVPbetav2::fn_convert_std_vec_of_corrs_to_3d_array(samps[ii, ], n_tests, 2)[[class]]     )
           # 
           # nump_outs[1,1]
           # 
           # Jacobian = nump_outs[22:31, 1:10]
           # 
           # log(det(((Jacobian))))
           # log(det(diag(diag(Jacobian))))
           # 
           
           
           

# 
# 
# 
#            {
#              
#            # time estimate for LC-MVP model, for N =  500
#            n_cores <- 96
#            n_chains <- 4
#            scaling_factor_for_parallel <- 1.3333333333333
#            N_sim <- 500
#           # N_scenarios_per_sim <-  ( 3*4 + 3*(3)) + ( 3*4 + 3*(3)) + ( 6*4 + 6*(3))
#            N_prior_param_combos <- 6
#            N_toggle_known_zeros <- 2
#            N_Se_Sp_combos <- 2
#            N_toggle_CD_in_both_classes_or_only_diseased <- 2
#            N_toggle_CD_mat_type <- 2
#            N_CD_struct_combos <-  N_toggle_CD_in_both_classes_or_only_diseased * N_toggle_CD_mat_type ; N_CD_struct_combos
#            N_DGMs_except_CI <- N_Se_Sp_combos * N_CD_struct_combos ; N_DGMs_except_CI
#            N_DGMs_CI <- N_Se_Sp_combos
#            N_DGMs <- N_DGMs_except_CI + N_DGMs_CI ; N_DGMs
#            N_scenarios_per_sim <- (N_DGMs_CI) + (N_DGMs_except_CI * N_toggle_known_zeros * N_prior_param_combos)    ; N_scenarios_per_sim 
#            if (n_chains == 4) seconds_per_sim <-   60 * 0.30 # need to check
#            if (n_chains == 8) seconds_per_sim <-   60 * 0.20   
#            seconds_per_sim * N_sim
#            mins_per_sim <- seconds_per_sim / 60 ; mins_per_sim * N_sim
#            hours_per_sim <- mins_per_sim / 60 ; hours_per_sim * N_sim
#            hours_for_all_N_sims <- hours_per_sim * N_sim; hours_for_all_N_sims
#            hours_for_all_N_sims_and_all_scenarios <- hours_for_all_N_sims * N_scenarios_per_sim ; hours_for_all_N_sims_and_all_scenarios
#            total_days_for_given_N_serial <- hours_for_all_N_sims_and_all_scenarios / 24 ; total_days_for_given_N_serial
#            total_days_for_given_N_parallel_unadjusted <- total_days_for_given_N_serial / (n_cores/n_chains) ; total_days_for_given_N_parallel_unadjusted
#            total_days_for_given_N_parallel_adjusted_1 <- total_days_for_given_N_parallel_unadjusted * scaling_factor_for_parallel ; total_days_for_given_N_parallel_adjusted_1
#            
#            
#            n_cores <-  96
#            n_chains <- 4
#            scaling_factor_for_parallel <- 1.3333333333333
#            N_sim <- 500
#            # N_scenarios_per_sim <-  ( 3*4 + 3*(3)) + ( 3*4 + 3*(3)) + ( 6*4 + 6*(3))
#            N_prior_param_combos <- 3 
#            N_toggle_known_zeros <- 2
#            N_Se_Sp_combos <- 2
#            N_toggle_CD_in_both_classes_or_only_diseased <- 2
#            N_toggle_CD_mat_type <- 2
#            N_CD_struct_combos <-  N_toggle_CD_in_both_classes_or_only_diseased * N_toggle_CD_mat_type ; N_CD_struct_combos
#            N_DGMs_except_CI <- N_Se_Sp_combos * N_CD_struct_combos ; N_DGMs_except_CI
#            N_DGMs_CI <- N_Se_Sp_combos
#            N_DGMs <- N_DGMs_except_CI + N_DGMs_CI ; N_DGMs
#            N_scenarios_per_sim <- (N_DGMs_CI) + (N_DGMs_except_CI * N_toggle_known_zeros * N_prior_param_combos)    ; N_scenarios_per_sim 
#            # N_scenarios_per_sim <-  49 ; N_scenarios_per_sim
#            #    N_scenarios_per_sim <- 49
#            if (n_chains == 4) seconds_per_sim <-   60 * 0.30 # need to check
#            if (n_chains == 8) seconds_per_sim <-   60 * 0.20 
#            seconds_per_sim * N_sim
#            mins_per_sim <- seconds_per_sim / 60 ; mins_per_sim * N_sim
#            hours_per_sim <- mins_per_sim / 60 ; hours_per_sim * N_sim
#            hours_for_all_N_sims <- hours_per_sim * N_sim; hours_for_all_N_sims
#            hours_for_all_N_sims_and_all_scenarios <- hours_for_all_N_sims * N_scenarios_per_sim ; hours_for_all_N_sims_and_all_scenarios
#            total_days_for_given_N_serial <- hours_for_all_N_sims_and_all_scenarios / 24 ; total_days_for_given_N_serial
#            total_days_for_given_N_parallel_unadjusted <- total_days_for_given_N_serial / (n_cores/n_chains) ; total_days_for_given_N_parallel_unadjusted
#            total_days_for_given_N_parallel_adjusted_2 <- total_days_for_given_N_parallel_unadjusted * scaling_factor_for_parallel ; total_days_for_given_N_parallel_adjusted_2
#            
#            
#            # time estimate for LC-MVP model, for N = 2500
#            n_cores <-  96
#            n_chains <- 4
#            scaling_factor_for_parallel <- 1.3333333333333
#            N_sim <- 300
#            # N_scenarios_per_sim <-  ( 3*4 + 3*(3)) + ( 3*4 + 3*(3)) + ( 6*4 + 6*(3))
#            N_prior_param_combos <- 6
#            N_toggle_known_zeros <- 2
#            N_Se_Sp_combos <- 2
#            N_toggle_CD_in_both_classes_or_only_diseased <- 2
#            N_toggle_CD_mat_type <- 2
#            N_CD_struct_combos <-  N_toggle_CD_in_both_classes_or_only_diseased * N_toggle_CD_mat_type ; N_CD_struct_combos
#            N_DGMs_except_CI <- N_Se_Sp_combos * N_CD_struct_combos ; N_DGMs_except_CI
#            N_DGMs_CI <- N_Se_Sp_combos
#            N_DGMs <- N_DGMs_except_CI + N_DGMs_CI ; N_DGMs
#            N_scenarios_per_sim <- (N_DGMs_CI) + (N_DGMs_except_CI * N_toggle_known_zeros * N_prior_param_combos)    ; N_scenarios_per_sim 
#            if (n_chains == 4) seconds_per_sim <-   60 * 3.00 # need to check
#            if (n_chains == 8) seconds_per_sim <-   60 * 1.80 
#            mins_per_sim <- seconds_per_sim / 60 ; mins_per_sim * N_sim
#            hours_per_sim <- mins_per_sim / 60 ; hours_per_sim * N_sim
#            hours_for_all_N_sims <- hours_per_sim * N_sim; hours_for_all_N_sims
#            hours_for_all_N_sims_and_all_scenarios <- hours_for_all_N_sims * N_scenarios_per_sim ; hours_for_all_N_sims_and_all_scenarios
#            total_days_for_given_N_serial <- hours_for_all_N_sims_and_all_scenarios / 24 ; total_days_for_given_N_serial
#            total_days_for_given_N_parallel_unadjusted <- total_days_for_given_N_serial / (n_cores/n_chains) ; total_days_for_given_N_parallel_unadjusted
#            total_days_for_given_N_parallel_adjusted_3 <- total_days_for_given_N_parallel_unadjusted * scaling_factor_for_parallel ; total_days_for_given_N_parallel_adjusted_3
#            
#            
#            n_cores <-  96
#            n_chains <-  4
#            scaling_factor_for_parallel <- 1.3333333333333
#            N_sim <- 300
#            # N_scenarios_per_sim <-  ( 3*4 + 3*(3)) + ( 3*4 + 3*(3)) + ( 6*4 + 6*(3))
#            N_prior_param_combos <-  4
#            N_toggle_known_zeros <- 2
#            N_Se_Sp_combos <- 2 
#            N_toggle_CD_in_both_classes_or_only_diseased <- 2
#            N_toggle_CD_mat_type <- 2
#            N_CD_struct_combos <-  N_toggle_CD_in_both_classes_or_only_diseased * N_toggle_CD_mat_type ; N_CD_struct_combos
#            N_DGMs_except_CI <- N_Se_Sp_combos * N_CD_struct_combos ; N_DGMs_except_CI
#            N_DGMs_CI <- N_Se_Sp_combos
#            N_DGMs <- N_DGMs_except_CI + N_DGMs_CI ; N_DGMs
#            N_scenarios_per_sim <- (N_DGMs_CI) + (N_DGMs_except_CI * N_toggle_known_zeros * N_prior_param_combos)    ; N_scenarios_per_sim 
#            if (n_chains == 4) seconds_per_sim <-   60 * 3.00 # need to check
#            if (n_chains == 8) seconds_per_sim <-   60 * 1.80 
#            mins_per_sim <- seconds_per_sim / 60 ; mins_per_sim * N_sim
#            hours_per_sim <- mins_per_sim / 60 ; hours_per_sim * N_sim
#            hours_for_all_N_sims <- hours_per_sim * N_sim; hours_for_all_N_sims
#            hours_for_all_N_sims_and_all_scenarios <- hours_for_all_N_sims * N_scenarios_per_sim ; hours_for_all_N_sims_and_all_scenarios
#            total_days_for_given_N_serial <- hours_for_all_N_sims_and_all_scenarios / 24 ; total_days_for_given_N_serial
#            total_days_for_given_N_parallel_unadjusted <- total_days_for_given_N_serial / (n_cores/n_chains) ; total_days_for_given_N_parallel_unadjusted
#            total_days_for_given_N_parallel_adjusted_4 <- total_days_for_given_N_parallel_unadjusted * scaling_factor_for_parallel ; total_days_for_given_N_parallel_adjusted_4
#            
#            }
#            #
#            
#            total_days_for_given_N_parallel_adjusted_1 + total_days_for_given_N_parallel_adjusted_2 + total_days_for_given_N_parallel_adjusted_3 + total_days_for_given_N_parallel_adjusted_4
#            
#            
#            
#            
#            
#            b_vec <- c(0.55,  1.15, 0.75, 0.90, 1.20)
#            
#            Sigma_from_bs <- diag(5) + t(t(b_vec)) %*% (t(b_vec))
#            
#            Omega_from_bs <- round(cov2cor(Sigma_from_bs), 2) ; Omega_from_bs
#            
#            min(Omega_from_bs)
#            max(Omega_from_bs[Omega_from_bs != 1])
#            
           
           
           
           
           
#                 
#                 
#                 {
#                   
#                   # first work out the model-predicted / expected correlations and table probabilities, then deviance and loo-IC to add to DF. 
#                   
#                   
#                   N_sims_for_model_fit_estimates <- 1000
#                   predicted_table_probs_vec_cumul <- 0
#                   predicted_correlations_vec_cumul <- 0
#                   
#                   predicted_table_probs_array <- array(dim = c(N_sims_for_model_fit_estimates, 2^n_tests))
#                   predicted_correlations_vec_array <- array(dim = c(N_sims_for_model_fit_estimates,  choose(n_tests, 2)))
#                   
#                   for (ii in 1:N_sims_for_model_fit_estimates) {
#                     
#                     d_ind <- sort(rbinom(n= N, size = 1, prob = prev))
#                     n_pos <- sum(d_ind)
#                     
#                     
#                     n_neg <- N - sum(d_ind)
#                     latent_results_neg <- LaplacesDemon::rmvn(n = n_neg, mu = qnorm(1 - Sp), Sigma = as.matrix(Matrix::forceSymmetric(as.matrix(posterior_Omega_nd))))
#                     latent_results_pos <- LaplacesDemon::rmvn(n = n_pos, mu = qnorm(Se), Sigma = as.matrix(Matrix::forceSymmetric(as.matrix(posterior_Omega_d))))
#                     latent_results <- rbind(latent_results_neg, latent_results_pos)
#                     results_neg <- ifelse(latent_results_neg > 0, 1, 0)
#                     results_pos <- ifelse(latent_results_pos > 0, 1, 0)
#                     results <- rbind(results_neg, results_pos)
#                     y_predicted <- results
#                     
#                     df_predicted <- tibble(results,latent_results,d_ind)
#                     df_predicted_pos <- filter(df_predicted, d_ind == 1)
#                     df_predicted_neg <- filter(df_predicted, d_ind == 0)
#                     
#                     
#                     predicted_correlations <- array(dim = c(n_tests, n_tests))
#                     
#                     for (i in 2:n_tests) {
#                       for (j in 1:(i-1)) {
#                         predicted_correlations[i, j] <- cor(y_predicted[, i], y_predicted[, j])
#                         predicted_correlations[j, i] <-  predicted_correlations[i, j]
#                       }
#                     }
#                     
#                     
#                     predicted_correlations_vec <- predicted_correlations[upper.tri(predicted_correlations )]
#                     
#                     predicted_table <- table(y_predicted[, 1], y_predicted[, 2], y_predicted[, 3], y_predicted[, 4], y_predicted[, 5])
#                     predicted_table_probs_vec <- c(unlist(round(prop.table(predicted_table), 4)))
#                     
#                     predicted_table_probs_array[ii, ] <-  predicted_table_probs_vec
#                     predicted_correlations_vec_array[ii, ]  <-   predicted_correlations_vec  # - true_correlations_observed_vec
#                     
#                     
#                     
#                   }
#                   
#                   
#                   predicted_table_probs_stats <- apply(predicted_table_probs_array, 2 , quantile, probs = c(0.025, 0.50, 0.975)) 
#                   predicted_correlations_stats <- apply(predicted_correlations_vec_array, 2 , quantile, probs = c(0.025, 0.50, 0.975)) 
#                   
#                   
#                   predicted_table_probs_vec_median  <- predicted_table_probs_stats[2,]
#                   predicted_correlations_vec_median <-  predicted_correlations_stats[2,]
#                   
#                   predicted_table_probs_vec_lower  <- predicted_table_probs_stats[1,]
#                   predicted_correlations_vec_lower <-  predicted_correlations_stats[1,]
#                   
#                   predicted_table_probs_vec_upper  <- predicted_table_probs_stats[3,]
#                   predicted_correlations_vec_upper <-  predicted_correlations_stats[3,]
#                   
#                   
#                   predicted_expected_cell_counts_median <- predicted_table_probs_vec_median * N
#                   predicted_expected_cell_counts_lower <- predicted_table_probs_vec_lower * N
#                   predicted_expected_cell_counts_upper <- predicted_table_probs_vec_upper * N
#                   
#                   deviance_per_cell_median <- 2 * observed_cell_counts_list[[df_i]]  * log( observed_cell_counts_list[[df_i]] / predicted_expected_cell_counts_median )
#                   deviance_per_cell_lower <- 2 * observed_cell_counts_list[[df_i]]  * log( observed_cell_counts_list[[df_i]] / predicted_expected_cell_counts_lower )
#                   deviance_per_cell_upper <- 2 * observed_cell_counts_list[[df_i]]  * log( observed_cell_counts_list[[df_i]] / predicted_expected_cell_counts_upper )
#                   
#                   deviance_median <- sum(deviance_per_cell_median[!(is.infinite(deviance_per_cell_median))], na.rm = TRUE)
#                   deviance_lower <- sum(deviance_per_cell_lower[!(is.infinite(deviance_per_cell_lower))], na.rm = TRUE)
#                   deviance_upper <- sum(deviance_per_cell_upper[!(is.infinite(deviance_per_cell_upper))], na.rm = TRUE)
#                   
#                   loo_ic_mean <- loo_ic_est_lower <- loo_ic_est_upper <- NA
#                   
#                   estimates_medians <- c(posterior_medians, 
#                                          predicted_correlations_vec_median, predicted_table_probs_vec_median, deviance_median, loo_ic_mean)
#                   
#                   estimates_lower  <- c(posterior_lower,
#                                         predicted_correlations_vec_lower, predicted_table_probs_vec_lower, deviance_lower, loo_ic_est_lower)
#                   
#                   estimates_upper <- c(posterior_upper,
#                                        predicted_correlations_vec_upper, predicted_table_probs_vec_upper, deviance_upper, loo_ic_est_upper)
#                   
#                   param_type <- c(rep("Corr_nd", n_corrs/2), rep("Corr_d", n_corrs/2),  rep("Sp", n_tests), rep("Se", n_tests), rep("prev", n_pops), 
#                                   rep("y_corrs", choose(n_tests, 2)), rep("table_probs", 2^n_tests), "Deviance_est", "loo_ic")
#                   param_seq <- c(seq(from = 1, by = 1, length =  n_corrs/2), seq(from = 1, by = 1, length =  n_corrs/2) , seq(from = 1, by = 1, length =  n_tests)  , seq(from = 1, by = 1, length =  n_tests)  , seq(from = 1, by = 1, length =  n_pops) ,
#                                  seq(from = 1, by = 1, length =  choose(n_tests, 2)) ,  seq(from = 1, by = 1, length =  2^n_tests), 1, 1)
#                   n_params_main <- length(estimates_medians)
#                   
#                   
#                   # append results to DF
#                   df_segment_1 <- tibble(N = N, 
#                                          DGM = DGP, 
#                                          df_sim_seed = df_i,
#                                          algorithm = "custom_HMC", 
#                                          model_type = model_type,
#                                          corr_force_positive = corr_force_positive,
#                                          prior_lkj_skewed_diseased_a = prior_lkj_skewed_diseased[1],
#                                          prior_lkj_skewed_diseased_b = prior_lkj_skewed_diseased[2],
#                                          prior_lkj_skewed_non_diseased_a = prior_lkj_skewed_non_diseased[1],
#                                          prior_lkj_skewed_non_diseased_b = prior_lkj_skewed_non_diseased[2],
#                                          tailored_corr_priors = tailored_corr_priors,
#                                          prior_a_mean_nd_test_1 = prior_a_mean[1,1,1],
#                                          prior_a_mean_nd_test_2 = prior_a_mean[1,2,1],
#                                          prior_a_mean_nd_test_3 = prior_a_mean[1,3,1],
#                                          prior_a_mean_nd_test_4 = prior_a_mean[1,4,1],
#                                          prior_a_mean_nd_test_5 = prior_a_mean[1,5,1],
#                                          prior_a_mean_d_test_1 = prior_a_mean[2,1,1],
#                                          prior_a_mean_d_test_2 = prior_a_mean[2,2,1],
#                                          prior_a_mean_d_test_3 = prior_a_mean[2,3,1],
#                                          prior_a_mean_d_test_4 = prior_a_mean[2,4,1],
#                                          prior_a_mean_d_test_5 = prior_a_mean[2,5,1],
#                                          prior_a_sd_nd_test_1 = prior_a_sd[1,1,1],
#                                          prior_a_sd_nd_test_2 = prior_a_sd[1,2,1],
#                                          prior_a_sd_nd_test_3 = prior_a_sd[1,3,1],
#                                          prior_a_sd_nd_test_4 = prior_a_sd[1,4,1],
#                                          prior_a_sd_nd_test_5 = prior_a_sd[1,5,1],
#                                          prior_a_sd_d_test_1 = prior_a_sd[2,1,1],
#                                          prior_a_sd_d_test_2 = prior_a_sd[2,2,1],
#                                          prior_a_sd_d_test_3 = prior_a_sd[2,3,1],
#                                          prior_a_sd_d_test_4 = prior_a_sd[2,4,1],
#                                          prior_a_sd_d_test_5 = prior_a_sd[2,5,1])
#                   
#                   
#                   df_segment_2 <-  df_segment_1 %>% 
#                     slice(rep(1:n(), each = n_params_main)) %>%
#                     mutate(estimates_medians = estimates_medians, 
#                            estimates_lower = estimates_lower, 
#                            estimates_upper = estimates_upper, 
#                            true_estimates_observed = true_estimates_observed_list[[df_i]],
#                            true_estimates  = true_estimates,
#                            param_type = param_type,
#                            param_seq = param_seq,
#                            diff_observed_median = true_estimates_observed_list[[df_i]] - estimates_medians,
#                            diff_observed_lower = true_estimates_observed_list[[df_i]] - estimates_lower,
#                            diff_observed_upper = true_estimates_observed_list[[df_i]] - estimates_upper,
#                            abs_diff_observed_median = abs(true_estimates_observed_list[[df_i]] - estimates_medians),
#                            abs_diff_observed_lower = abs(true_estimates_observed_list[[df_i]] - estimates_lower),
#                            abs_diff_observed_upper = abs(true_estimates_observed_list[[df_i]] - estimates_upper),
#                            corr_force_positive = ifelse(model_type == "LT", 1, corr_force_positive))
#                   
#                #    View(df_segment_2)
#                   
#                   
#                   
#                   file_name <- paste("df_sim", 
#                                      "N", N/1000, 
#                                      "DGP", DGP,
#                                      "df_seed", df_i,
#                                      "custom_HMC", 
#                                      model_type, 
#                                      "pos_corr", corr_force_positive,
#                                      "pi_lkj_d_a", prior_lkj_skewed_diseased[1],
#                                      "pi_lkj_d_b", prior_lkj_skewed_diseased[2],
#                                      "pi_lkj_nd_a", prior_lkj_skewed_non_diseased[1],
#                                      "pi_lkj_nd_b", prior_lkj_skewed_non_diseased[2],
#                                      "tailored_pi", tailored_corr_priors,
#                                      "pi_a_mu_nd", prior_a_mean[1,1,1], prior_a_mean[1,2,1],  prior_a_mean[1,3,1], prior_a_mean[1,4,1], prior_a_mean[1,5,1],
#                                      "pi_a_mu_d",  prior_a_mean[2,1,1], prior_a_mean[2,2,1],  prior_a_mean[2,3,1], prior_a_mean[2,4,1], prior_a_mean[2,5,1],
#                                      "pi_a_sds_nd",  prior_a_sd[1,1,1], prior_a_sd[1,2,1],  prior_a_sd[1,3,1], prior_a_sd[1,4,1], prior_a_sd[1,5,1],
#                                      "pi_a_sds_d",   prior_a_sd[2,1,1], prior_a_sd[2,2,1],  prior_a_sd[2,3,1], prior_a_sd[2,4,1], prior_a_sd[2,5,1], sep = "_")
#                   
#                   
#                   file_name
#                   
#                   
#                   
#                   saveRDS(df_segment_2, file = file_name)
#                   
#                   
#                   
#                   
#                   
#                   
#                 }
# 
# 
# }
# 
# 
# 
# 
# 
# 
# 
# View(df_segment_2)
# df_sim_results_avg_by_model_and_priors_etc <- df_segment_2 %>%
#   group_by(N, 
#            DGM,
#     #       df_sim_seed,
#            algorithm, 
#            model_type,
#            corr_force_positive,
#            prior_lkj_skewed_diseased_a,
#            prior_lkj_skewed_diseased_b,
#            prior_lkj_skewed_non_diseased_a,
#            prior_lkj_skewed_non_diseased_b,
#            tailored_corr_priors,
#            prior_a_mean_nd_test_1,
#            prior_a_mean_nd_test_2,
#            prior_a_mean_nd_test_3,
#            prior_a_mean_nd_test_4,
#            prior_a_mean_nd_test_5,
#            prior_a_mean_d_test_1,
#            prior_a_mean_d_test_2,
#            prior_a_mean_d_test_3,
#            prior_a_mean_d_test_4,
#            prior_a_mean_d_test_5,
#            prior_a_sd_nd_test_1,
#            prior_a_sd_nd_test_2,
#            prior_a_sd_nd_test_3,
#            prior_a_sd_nd_test_4,
#            prior_a_sd_nd_test_5,
#            prior_a_sd_d_test_1,
#            prior_a_sd_d_test_2,
#            prior_a_sd_d_test_3,
#            prior_a_sd_d_test_4,
#            prior_a_sd_d_test_5, 
#            param_type)  %>%
#   dplyr::summarise(mean_abs_diff_observed_median = mean(abs_diff_observed_median, na.rm=TRUE))
# 
#  View(df_sim_results_avg_by_model_and_priors_etc)
# 
# 
# df_sim_results_avg_by_model_and_priors_etc <-  df_sim_results_avg_by_model_and_priors_etc  %>%
#   arrange(param_type)
# 
# df_sim_results_avg_by_model_and_priors_etc <- df_sim_results_avg_by_model_and_priors_etc %>%
#   ungroup()  %>%
#   mutate(num = rep(1, length(df_sim_results_avg_by_model_and_priors_etc$N)))
# 
#  
# 
# ### df_segment_3 <-  df_segment_2 %>%  slice(rep(1:n(), each = 1000)) 
# 
# 
# # View(df_segment_2)
# 
# 
# 
# # 
# # ggplot(data = filter(df_segment_2, param_type %in% c("Se", "Sp", "prev", "Corr_d", "Corr_nd"), df_sim_seed == 123), 
# #        aes(x = param_seq, y = diff_observed_median, shape = model_type , 
# #                                 colour = interaction(corr_force_positive,
# #                                                      prior_lkj_skewed_diseased_a,
# #                                                      prior_lkj_skewed_diseased_b, 
# #                                                      prior_lkj_skewed_non_diseased_a, 
# #                                                      prior_lkj_skewed_non_diseased_b, 
# #                                                      tailored_corr_priors, 
# #                                                      sep = ":") ))  + 
# #   #  geom_line(size = 1.0) + 
# #   geom_point(size = 3.5) + 
# #   geom_point(data =  filter(df_sim_results_avg_by_model_and_priors_etc, param_type %in% c("Se", "Sp", "prev", "Corr_d", "Corr_nd"), df_sim_seed == 123), 
# #              aes(x =   num, y = mean_abs_diff_observed_median,   shape = model_type, colour = interaction(corr_force_positive,
# #                                                                                                                                                prior_lkj_skewed_diseased_a,
# #                                                                                                                                                prior_lkj_skewed_diseased_b,
# #                                                                                                                                                prior_lkj_skewed_non_diseased_a,
# #                                                                                                                                                prior_lkj_skewed_non_diseased_b,
# #                                                                                                                                                tailored_corr_priors,
# #                                                                                                                                                sep = ":")),
# #              size = 10,   stroke = 3)   +
# #   facet_wrap(~ param_type + DGM , scales = "free", ncol = 5)  + 
# #   geom_hline(yintercept = 0) +
# #   theme_bw() + 
# #   ylab(" Diff (true - estimate) or mean abs. diff for average points") + 
# #   xlab("Parameter") + 
# #   theme_bw(base_size = 20) + 
# #   guides(color=guide_legend(title="Model properties"))   
# # 
# # 
# 
# 
# 
# 
# 
# ggplot(data = filter(df_sim_results_avg_by_model_and_priors_etc, param_type %in% c("Se", "Sp", "prev", "Corr_d", "Corr_nd")), 
#        aes(x = num, y = mean_abs_diff_observed_median, shape = model_type , 
#            colour = interaction(corr_force_positive,
#                                 prior_lkj_skewed_diseased_a,
#                                 prior_lkj_skewed_diseased_b, 
#                                 prior_lkj_skewed_non_diseased_a, 
#                                 prior_lkj_skewed_non_diseased_b, 
#                                 tailored_corr_priors, 
#                                 sep = ":") ))  + 
#   geom_point(size = 3.5) + 
#   facet_wrap(~ param_type + DGM , scales = "free", ncol = 5)  + 
#   geom_hline(yintercept = 0) +
#   theme_bw() + 
#   ylab(" Diff (true - estimate) or mean abs. diff for average points") + 
#   xlab("Parameter") + 
#   theme_bw(base_size = 20) + 
#   guides(color=guide_legend(title="Model properties"))   
# 
# 
# 
# 
# 
# 
# # ---------------------------------------------  correlation residual plot
# 
# 
# df_segment_for_table_prob_resid <- df_segment_2 %>%
#   dplyr::filter(param_type == "table_probs" ) 
# 
# 
# ggplot(data = df_segment_for_table_prob_resid, aes(x = param_seq, y = diff_observed_median, linetype = model_type , 
#                                                    colour = interaction(corr_force_positive,
#                                                                         prior_lkj_skewed_diseased_a,
#                                                                         prior_lkj_skewed_diseased_b, 
#                                                                         prior_lkj_skewed_non_diseased_a, 
#                                                                         prior_lkj_skewed_non_diseased_b, 
#                                                                         tailored_corr_priors, 
#                                                                         sep = ":") ))  + 
#   #  geom_line(size = 1.0) + 
#   geom_point(size = 3.5) + 
#   geom_errorbar(aes(ymin=diff_observed_lower, ymax=diff_observed_upper, width=0.2,    colour = interaction(corr_force_positive,
#                                                                                                            prior_lkj_skewed_diseased_a,
#                                                                                                            prior_lkj_skewed_diseased_b, 
#                                                                                                            prior_lkj_skewed_non_diseased_a, 
#                                                                                                            prior_lkj_skewed_non_diseased_b, 
#                                                                                                            tailored_corr_priors, 
#                                                                                                            sep = ":")  )) + 
#   facet_wrap(~ param_type, scales = "free")  + 
#   geom_hline(yintercept = 0) +
#   theme_bw() + 
#   ylab(" Diff (true - estimate) or mean abs. diff for average points") + 
#   xlab("Parameter") + 
#   theme_bw(base_size = 20) + 
#   guides(color=guide_legend(title="Model properties"))   
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# # 
# # ## --------------------------- Wang et al (2017) - mini simulation study (fitted to data generated from Wang et al TLCM 5-test "FEM2" model - see paper)
# # y_for_jags_hw <- tibble(y) %>% 
# #   mutate(Population = rep(1, N)) %>%
# #   mutate(t_1 = y[,1],
# #          t_2 = y[,2],
# #          t_3 = y[,3],
# #          t_4 = y[,4],
# #          t_5 = y[,5]) %>%
# #   select(- y)
# # 
# # sp[1] ~  dbeta(113.25, 0.42) # Wang et al Sp prios for tests 1-3
# # 
# # 
# # # jags_hw_outs <- template_huiwalter( y_for_jags_hw,
# # #                                     covariance = T,
# # #                                         cov_as_cor = TRUE,
# # #                                     # cov_as_cor = FALSE,
# # #                                     outfile = "jags_HW_model_Wang_et_al_data_1.txt")
# # 
# #  
# #  
# # 
# # 
# # {
# #   jags_hw_result_N_Wang <- run.jags(
# #     #"jags_HW_model_N_1000.txt", 
# #     "jags_HW_model_Wang_et_al_data_1.txt", 
# #     monitor = c("se", "sp"), 
# #     n.chains = 48, 
# #     method = "parallel", 
# #     burnin = 5000, 
# #     sample = 10000)
# #   
# #   jags_hw_result_N_Wang
# # }
# #  
# #  
# # 
# # 
# # 
# # 
# # 
# # Se_true_Wang <-       c(0.60, 0.70, 0.80, 0.65, 0.68) 
# # Sp_true_Wang <-       c(0.99, 0.99, 0.99, 0.75, 0.65)
# # 
# # 
# # Se_LC_MVP_Wang  <-  (c(0.62, 0.70, 0.86, 0.66, 0.67)) # custom alg or Stan
# # Sp_LC_MVP_Wang  <-  (c(0.98, 0.96, 1.00, 0.74, 0.64)) # custom alg or Stan
# # 
# # Se_LC_MVP_force_positive_corr_Wang  <-  (c(0.59, 0.68, 0.79, 0.65, 0.66)) # custom alg or Stan
# # Sp_LC_MVP_force_positive_corr_Wang  <-  (c(1.00, 1.00, 1.00, 0.75, 0.64)) # custom alg or Stan
# # 
# # 
# # Se_LC_LT_Wang <- c(0.58, 0.69, 0.80, 0.65, 0.67) # Stan
# # Sp_LC_LT_Wang <- c(0.98, 1, 1, 0.75, 0.65) # Stan
# # 
# # Se_FEM2_Wang  <-  round((c(0.55, 0.635, 0.72, 0.614, 0.666)), 2)
# # Sp_FEM2_Wang  <-  round((c(0.998, 0.998, 0.998, 0.756, 0.67)), 2)
# # 
# # Se_Denwood_TLCM_Wang <- jags_hw_result_N_Wang$summaries[1:n_tests,2] 
# # Sp_Denwood_TLCM_Wang <- jags_hw_result_N_Wang$summaries[(n_tests + 1):(2 * n_tests),2] 
# # 
# # par(mfrow = c(2, 1))
# # plot(round( (Se_LC_MVP_Wang/Se_true_Wang) - 1, 2)*100, ylim = c(-50, 50), col = "green", main = "% diff between true and model-estimated value, Se , \nWang et al data simulated under TLCM ", cex = 2, pch = 19 ) ; 
# # points(round(Se_LC_MVP_force_positive_corr_Wang/Se_true_Wang - 1 , 2)*100, col = "green", cex = 2, pch = 21)  ; 
# # points(round(Se_LC_LT_Wang/Se_true_Wang - 1 , 2)*100, col = "blue", cex = 2, pch = 19)  ; 
# # points(round(Se_FEM2_Wang/Se_true_Wang - 1 , 2)*100, col = "red", cex = 2, pch = 19)  ; 
# # points(round(Se_Denwood_TLCM_Wang/Se_true_Wang - 1 , 2)*100, col = "orange", cex = 2, pch = 19)  ; 
# # abline(a=0, b=0)
# # 
# # plot(round( Sp_LC_MVP_Wang/Sp_true_Wang - 1  , 2)*100, ylim = c(-50, 50), col = "green", main = "% diff between true and model-estimated value, Sp , \nWang et al data simulated under TLCM ", cex = 2, pch = 19) ; sum(abs(Sp_LC_MVP_Wang - Sp_true_Wang))
# # points(round(Sp_LC_MVP_force_positive_corr_Wang/Se_true_Wang - 1 , 2)*100, col = "green", cex = 2, pch = 21)  ; 
# # points(round(Sp_LC_LT_Wang/Sp_true_Wang - 1 , 2)*100, col = "blue", cex = 2, pch = 19)  ; 
# # points(round(  Sp_FEM2_Wang/Sp_true_Wang - 1, 2)*100, col = "red", cex = 2, pch = 19) ; sum(abs(Sp_FEM2_Wang - Sp_true_Wang))
# # points(round(  Sp_Denwood_TLCM_Wang/Sp_true_Wang - 1, 2)*100, col = "orange", cex = 2, pch = 19) ; sum(abs(Sp_Denwood_TLCM_Wang - Sp_true_Wang))
# # abline(a=0, b=0)
# # 
# # 
# # 
# # 
# # Se_abs_diff_LC_MVP_Wang <- sum(abs(Se_LC_MVP_Wang / Se_true_Wang - 1)*100)  ; Se_abs_diff_LC_MVP_Wang
# # Se_abs_diff_LC_MVP_force_positive_corr_Wang <- sum(abs(Se_LC_MVP_force_positive_corr_Wang / Se_true_Wang - 1)*100)  ; Se_abs_diff_LC_MVP_force_positive_corr_Wang
# # Se_abs_diff_LC_LT_Wang <- sum(abs(Se_LC_LT_Wang / Se_true_Wang - 1)*100)  ; Se_abs_diff_LC_LT_Wang
# # Se_abs_diff_FEM2_Wang <- sum(abs(Se_FEM2_Wang/Se_true_Wang - 1)*100) ; Se_abs_diff_FEM2_Wang
# # Se_abs_diff_Denwood_TLCM_Wang <- sum(abs(Se_Denwood_TLCM_Wang/Se_true_Wang - 1)*100) ; Se_abs_diff_Denwood_TLCM_Wang
# # 
# # Sp_abs_diff_LC_MVP_Wang <- sum(abs(Sp_LC_MVP_Wang / Sp_true_Wang - 1 )*100) ; Sp_abs_diff_LC_MVP_Wang
# # Sp_abs_diff_LC_MVP_force_positive_corr_Wang <- sum(abs(Sp_LC_MVP_force_positive_corr_Wang / Sp_true_Wang - 1 )*100) ; Sp_abs_diff_LC_MVP_force_positive_corr_Wang
# # Sp_abs_diff_LC_LT_Wang <- sum(abs(Sp_LC_LT_Wang / Sp_true_Wang - 1 )*100) ; Sp_abs_diff_LC_LT_Wang
# # Sp_abs_diff_FEM2_Wang <- sum(abs(Sp_FEM2_Wang / Sp_true_Wang - 1)*100) ; Sp_abs_diff_FEM2_Wang
# # Sp_abs_diff_Denwood_TLCM_Wang <- sum(abs(Sp_Denwood_TLCM_Wang / Sp_true_Wang - 1)*100) ; Sp_abs_diff_Denwood_TLCM_Wang
# # 
# # round(Se_abs_diff_LC_MVP_Wang, 2)   ;  round(Se_abs_diff_LC_LT_Wang, 2) ;  round(Se_abs_diff_LC_LT_Wang, 2) ;  round(Se_abs_diff_FEM2_Wang, 2) ;  round(Se_abs_diff_Denwood_TLCM_Wang, 2)   # Se diffs  - LOWER = BETTER
# # round(Sp_abs_diff_LC_MVP_Wang, 2)  ;  round(Se_abs_diff_LC_LT_Wang, 2)  ; round(Sp_abs_diff_LC_LT_Wang, 2) ;  round(Sp_abs_diff_FEM2_Wang, 2) ;  round(Sp_abs_diff_Denwood_TLCM_Wang, 2)  # Sp diffs - LOWER = BETTER
# # 
# # Se_abs_diff_LC_MVP_Wang + Sp_abs_diff_LC_MVP_Wang # overall (Se + Sp)  - LOWER = BETTER
# # Se_abs_diff_LC_MVP_force_positive_corr_Wang + Sp_abs_diff_LC_MVP_force_positive_corr_Wang # overall (Se + Sp)  - LOWER = BETTER
# # Se_abs_diff_LC_LT_Wang + Sp_abs_diff_LC_LT_Wang # overall (Se + Sp)  - LOWER = BETTER
# # Se_abs_diff_FEM2_Wang + Sp_abs_diff_FEM2_Wang  # overall (Se + Sp)  - LOWER = BETTER
# # Se_abs_diff_Denwood_TLCM_Wang + Sp_abs_diff_Denwood_TLCM_Wang 
# # 
# # 
# # 
# # 
# 
# 
# 
# 
# 
           
           
           
           
           
   
           
           
           
           
           
