

###  -------------  Stan - function to run LC_MVP model -------------------------------------------------------------------------------------------------------------------

# ## debug:
# # 
# {
#     computer <- "Local_HPC"
#     run_type <- "ps3"
#     ##
#     BayesMVP_model_obj = BayesMVP_LC_MVP_model_using_manual_grad_obj
#     BayesMVP_settings_list = pilot_study_target_ESS_list
#     global_list = global_list
#     ##
#     Stan_data_list = Stan_data_list
#     # ##
#     manual_gradients = TRUE
#     SIMD_vect_type = "AVX512"
#     # ##
#     save_output_directory =    pilot_study_target_ESS_list$output_path
#     save_full_output <- TRUE
#     compute_nested_rhat <- FALSE
#     save_log_lik_trace <- FALSE
#     compute_transformed_parameters <- FALSE
#     compute_generated_quantities <- TRUE
#     ##
#     output_path <-   pilot_study_target_ESS_list$output_path 
#     ##
#     # save_output_directory =  BayesMVP_pilot_study_list$output_path
#     # save_full_output = TRUE
#     # compute_nested_rhat = NULL
#     # save_log_lik_trace = FALSE
#     # compute_transformed_parameters = TRUE
#     # compute_generated_quantities = TRUE
#     # ##
# }


R_fn_run_BayesMVP_model_LC_MVP <- function(    computer,
                                               run_type,
                                               ##
                                               BayesMVP_model_obj,
                                               BayesMVP_settings_list,
                                               global_list, 
                                               ##
                                               Stan_data_list,
                                               ##
                                               manual_gradients,
                                               SIMD_vect_type,
                                               ##
                                               save_output_directory = NULL,
                                               save_full_output = FALSE,
                                               compute_nested_rhat = NULL,
                                               save_log_lik_trace = FALSE,
                                               compute_transformed_parameters = TRUE,
                                               compute_generated_quantities = TRUE
                                               )  {
  
       ## Start timer:
       tictoc::tic("timer_outer")
      
       gc(reset = TRUE) 
       
       {
           ## Important settings:
           N_vec <- BayesMVP_settings_list$N_vec
           n_runs <- BayesMVP_settings_list$n_runs
           start_index <- BayesMVP_settings_list$start_index
           ##
           n_superchains <- pilot_study_target_ESS_list$n_superchains
           n_chains_burnin_vec  <-  pilot_study_target_ESS_list$n_chains_burnin_vec 
           n_chains_sampling_vec  <-  pilot_study_target_ESS_list$n_chains_sampling_vec 
           ##
           n_chains_sampling_total_combos <- length(n_chains_sampling_vec)
           #
           n_burnin_vec  <-  pilot_study_target_ESS_list$n_burnin_vec 
           n_iter_vec  <-  pilot_study_target_ESS_list$n_iter_vec 
           ##
           #### Load BayesMVP settings/data stuff:
           partitioned_HMC <- BayesMVP_settings_list$partitioned_HMC
           diffusion_HMC <- BayesMVP_settings_list$diffusion_HMC
           sample_nuisance <- BayesMVP_settings_list$sample_nuisance
           ##
           adapt_delta <- BayesMVP_settings_list$adapt_delta
           learning_rate = BayesMVP_settings_list$learning_rate
           ##
           metric_shape_main = BayesMVP_settings_list$metric_shape_main
           metric_type_main = BayesMVP_settings_list$metric_type_main
           ##
           clip_iter = BayesMVP_settings_list$clip_iter
           ##
           interval_width_main = BayesMVP_settings_list$interval_width_main
           ##
           tau_mult = BayesMVP_settings_list$tau_mult
           ##
           ratio_M_us = BayesMVP_settings_list$ratio_M_us
           ratio_M_main = BayesMVP_settings_list$ratio_M_main
           ##
           force_autodiff = BayesMVP_settings_list$force_autodiff
           force_PartialLog = BayesMVP_settings_list$force_PartialLog
           multi_attempts = BayesMVP_settings_list$multi_attempts
           ##
       }
       
       
       {
         #### Stan_data_list <- BayesMVP_settings_list$Stan_data_list ## for Stan models
         
         # model_args_list$prev_prior_a <- model_args_list$prior_p_alpha
         # model_args_list$prev_prior_b <- model_args_list$prior_p_beta
         model_args_list$n_class <- 2
   
         ## Extract quantities needed from the global list:
         # Model_settings_list <-  global_list$Model_settings_list
         # n_tests <- Model_settings_list$n_tests
         # n_class <- Model_settings_list$n_class
         # n_pops <- Model_settings_list$n_pops
         # n_covariates_per_outcome_mat <- Model_settings_list$n_covariates_per_outcome_mat
         # overflow_threshold <- Model_settings_list$overflow_threshold
         # underflow_threshold <- Model_settings_list$underflow_threshold
         # Phi_type <- Model_settings_list$Phi_type 
         # ##
         # priors_list <-  global_list$priors_list
         # corr_force_positive <- priors_list$prior_Omega_list$corr_force_positive
         # a_priori_known_corrs <- priors_list$prior_Omega_list$a_priori_known_corrs
         ##
         N_sample_sizes_vec <- global_list$N_sample_sizes_vec
         y_binary_list <- global_list$data_sim_outs$y_binary_list
         ##
         # n_covariates_max_nd <- Model_settings_list$n_covariates_max_nd
         # n_covariates_max_d <-  Model_settings_list$n_covariates_max_d
         # n_covariates_max <-    Model_settings_list$n_covariates_max
         # ##
         # prior_beta_mean <- array(dim = c(n_class, n_tests, n_covariates_max))
         # prior_beta_mean[,,1] <-  t(priors_list$prior_beta_list$prior_beta_mean_vec)
         # prior_beta_sd <- array(dim = c(n_class, n_tests, n_covariates_max))
         # prior_beta_sd[,,1] <-  t(priors_list$prior_beta_list$prior_beta_sd_vec)
         # ##
         prior_LKJ <- priors_list$prior_Omega_list$prior_lkj_cholesky_eta
         # ##
         # prior_prev_alpha <- priors_list$prior_prev_list$prior_prev_alpha
         # prior_prev_beta <- priors_list$prior_prev_list$prior_prev_beta
         ##
         # ## "Data" which depens on N:
         # df_index <- which(N_sample_sizes_vec == N_sample_size_of_dataset)
         # N <- N_sample_sizes_vec[df_index]
       }
       
       
       
       ## Set key variables :
       if (manual_gradients == TRUE) { 
         
             BayesMVP_Model_type <- "LC_MVP"
             vect_type <- SIMD_vect_type
             model_args_list$vect_type <- vect_type
             
       } else { 
         
             BayesMVP_Model_type <- "Stan"
             if  (SIMD_vect_type != "Stan") { 
               stop("ERROR: For Stan models run via BayesMVP, the SIMD vectorisation type ('vect_type') must be set to 'Stan'")
             } else { 
               vect_type <- "Stan"
               model_args_list$vect_type <- vect_type
             }
             
       }

 
       
       # #### Load BayesMVP MCMC params:
       # n_chains_burnin <- BayesMVP_settings_list$n_chains_burnin
       # n_chains_sampling <- BayesMVP_settings_list$n_chains_sampling
       # n_superchains <- BayesMVP_settings_list$n_superchains
       ##

       # 
       # 
       # N_sample_sizes_vec <- global_list$N_sample_sizes_vec
       # ## total_num_datasets <- length(N_sample_sizes_vec)
       # ## Get data frame index (e.g. for this study if N = 500 then df_index = 1,)
       # df_index <- which(N_sample_sizes_vec == N_sample_size_of_dataset)
       ##
       # seed <- MCMC_seed
       # n_burnin <- n_burnin
       # n_iter <- n_iter
       ##
       n_params_main <- global_list$Model_settings_list$n_params_main
       # print(paste("df_index = ", df_index))
       # ##
       # n_nuisance <- prod(dim(global_list$initial_values_list$inits_u_list[[df_index]]))
       # n_params <- n_params_main + n_nuisance
       # index_nuisance <- 1:n_nuisance
       # index_main_params <- (n_nuisance + 1):n_params
       n_nuisance_to_track <- 10 # set to some small number (< 10) if don't care about making inference on nuisance params (which is most of the time!)
       
      # if (partitioned_HMC == TRUE) {
       ##   sample_nuisance <- TRUE
      # } 
       # if (partitioned_HMC == FALSE) {
       #   sample_nuisance <- FALSE
       # }
       
       # print(paste("model_args_list = "))
       # print(str(model_args_list))
 
   
       # if (BayesMVP_Model_type == "Stan") { 
       #   model_args_list <- NULL
       # } else { 
       #   Stan_data_list <- NULL
       # }
 
 
       {
         ## Set inits for params with DO NOT depend on N outside the loop:
         inits_Omega_unc_col_one_raw <- global_list$initial_values_list$inits_Omega_list$inits_Omega_unc_col_one_raw
         inits_Omega_unc_off_raw <- global_list$initial_values_list$inits_Omega_list$inits_Omega_unc_off_raw
         inits_beta_vec_raw <- global_list$initial_values_list$inits_beta_list$inits_beta_vec
         inits_prev_unc <- global_list$initial_values_list$inits_prev_list$inits_prev_unc
       }
       
       
       ## chunking (uses ps1 + ps2 results!):
       pilot_study_parallel_scaling_comp_list <-  pilot_study_target_ESS_list$pilot_study_parallel_scaling_comp_list 
           
       
       df_index <- 1
       
     ## -----------------------
     for (df_index in start_index:length(N_vec[!is.na(N_vec)]))  {
           
           {
               N <- BayesMVP_settings_list$N_vec[df_index]
               n_nuisance <- N * n_tests
               y <- y_binary_list[[df_index]]
               pop_group <- rep(1, N)
               ## Print:
               print(paste("N = ", N))
               print(paste("n_nuisance = ", n_nuisance))
               
               ## Set inits for u's here (as dimension of u's vary with N!!)
               inits_u_raw <- global_list$initial_values_list$inits_u_list[[df_index]]
               ## Make Stan / BayesMVP inits list:
               ## Note: For the LC_MVP (even with manual gradients!) BayesMVP uses the same structure as the Stan LC_MVP model for initial values !!
               stan_or_BayesMVP_inits_list <- list(  "u_raw" = inits_u_raw,
                                                     "col_one_raw" = inits_Omega_unc_col_one_raw,
                                                     "off_raw" = inits_Omega_unc_off_raw,
                                                     "beta_vec" = inits_beta_vec_raw,
                                                     "p_raw" = array(inits_prev_unc))
               ##
               init_lists_per_chain <- rep(list(stan_or_BayesMVP_inits_list), n_chains_burnin) 
               ##
               Stan_data_list_given_current_N <- Stan_data_list[[df_index]]
               ##  print(str(Stan_data_list_given_current_N))
               ##
               ## Chunking (uses ps1 + ps2 results!):
               opt_N_chunks <- pilot_study_parallel_scaling_comp_list$opt_n_chunks_at_opt_n_threads[[as.character(computer)]][[as.character(N)]]
               model_args_list$num_chunks <- opt_N_chunks
               
           }

             
       ## -----------------------
       for (k in (1:length(n_chains_burnin_vec))) {
         
                   ##
                   n_chains_burnin <- n_chains_burnin_vec[k]
                   ## -----------------------
                   for (kk in (1:length(n_chains_sampling_vec))) {
                     
                         ##
                         n_chains_sampling <- n_chains_sampling_vec[kk]
                         
                         ## -----------------------
                         for (n_burn_index in (1:length(n_burnin_vec))) {
                           
                               ##
                               n_burnin <- n_burnin_vec[n_burn_index]
                               
                               ## -----------------------
                               for (n_iter_index in (1:length(n_iter_vec))) {
                                 
                                     ##
                                     n_iter <- n_iter_vec[n_iter_index]
                                     ##
                                     df_list_alg_efficiency_per_run <- list()
                                     ## iii <- 1 ## for testing 
                                     ## -----------------------
                                     for (iii in 1:n_runs) {
                                       
                                           ## Set seed for the iii-th run:
                                           seed <- iii
                       
                                           ## Set timer for the iii-th run:
                                           tictoc::tic("timer")
                                             
                                           #### ---- Run model ------------------------------------------------------------:
                                           model_samples <-  BayesMVP_model_obj$sample(   Model_type = BayesMVP_Model_type,
                                                                                          partitioned_HMC = partitioned_HMC,
                                                                                          diffusion_HMC = diffusion_HMC,
                                                                                          parallel_method = "RcppParallel",
                                                                                          vect_type = vect_type,
                                                                                          ##
                                                                                          seed = seed,
                                                                                          ##
                                                                                          n_chains_burnin = n_chains_burnin,
                                                                                          init_lists_per_chain = init_lists_per_chain,
                                                                                          ##
                                                                                          n_chains_sampling = n_chains_sampling,
                                                                                          n_superchains = n_superchains,
                                                                                          ##
                                                                                          n_burnin = n_burnin,
                                                                                          n_iter = n_iter,
                                                                                          ## Some other arguments:
                                                                                          y = y,
                                                                                          N = N,
                                                                                          n_params_main = n_params_main,
                                                                                          n_nuisance = n_nuisance,
                                                                                          ##
                                                                                          model_args_list = model_args_list,
                                                                                          Stan_data_list = Stan_data_list_given_current_N,
                                                                                          ##
                                                                                          sample_nuisance = TRUE,
                                                                                          ##
                                                                                          force_autodiff = force_autodiff,
                                                                                          force_PartialLog = force_PartialLog,
                                                                                          multi_attempts = multi_attempts,
                                                                                          ##
                                                                                          adapt_delta = adapt_delta,
                                                                                          learning_rate = learning_rate,
                                                                                          ##
                                                                                          metric_shape_main = metric_shape_main,
                                                                                          metric_type_main = metric_type_main,
                                                                                          ##
                                                                                          tau_mult = tau_mult,
                                                                                          ##
                                                                                          clip_iter = clip_iter,
                                                                                          ##
                                                                                          interval_width_main = interval_width_main,
                                                                                          ##
                                                                                          ratio_M_us = ratio_M_us,
                                                                                          ratio_M_main = ratio_M_main,
                                                                                          ##
                                                                                          n_nuisance_to_track = n_nuisance_to_track)
                                           
                                           
                                           # #### ---- Model run time ("inner" timer) - using tictoc:
                                           # {
                                           #   print(tictoc::toc(log = TRUE))
                                           #   log.txt <- tictoc::tic.log(format = TRUE)
                                           #   tictoc::tic.clearlog()
                                           #   time_stan_total_inc_csv_inner <- unlist(log.txt)
                                           #   ##
                                           #   extract_numeric_string <- str_extract(time_stan_total_inc_csv_inner, "\\d+\\.\\d+")   
                                           #   time_BayesMVP_total_inc_csv_inner_numeric <- as.numeric(extract_numeric_string)
                                           # }
                                           
                                           gc(reset = TRUE)
                                           
                                           # ## debug: 
                                           # save_full_output = FALSE
                                           # compute_nested_rhat = NULL
                                           # save_log_lik_trace = FALSE
                                           # compute_transformed_parameters = TRUE
                                           # compute_generated_quantities = TRUE
                                           
                                           #### model_fit$init_object$model_args_list
                                           
                                      
                                           
                                           #### --- MODEL RESULTS SUMMARY + DIAGNOSTICS -------------------------------------------------------------
                                           # after fitting, call the "summary()" method to compute + extract e.g. model summaries + traces + plotting methods 
                                           # model_fit <- model_samples$summary() # to call "summary()" w/ default options 
                                           require(bridgestan)
                                           model_fit <- model_samples$summary(save_log_lik_trace = save_log_lik_trace, 
                                                                              compute_nested_rhat = compute_nested_rhat,
                                                                              compute_transformed_parameters = compute_transformed_parameters,
                                                                              compute_generated_quantities = compute_generated_quantities)
                                           
                                           # extract # divergences + % of sampling iterations which have divergences
                                           divs <- model_fit$get_divergences()
                                           n_divs <- divs$n_divs
                                           pct_divs <- divs$pct_divs
                                           ##
                                           require(dplyr)
                                           # nice summary tibble for main parameters, includes ESS/Rhat, etc
                                           try({  
                                             BayesMVP_Stan_like_summary_tibble_main <- model_fit$get_summary_main() %>% print(n = 50) 
                                           }, silent = TRUE)
                                           # nice summary tibble for transformed parameters, includes ESS/Rhat, etc
                                           try({  
                                             BayesMVP_Stan_like_summary_tibble_tp <- model_fit$get_summary_transformed() %>% print(n = 150) 
                                           }, silent = TRUE)
                                           # nice summary tibble for generated quantities, includes ESS/Rhat, etc (for LC-MVP this includes Se/Sp/prevalence)
                                           try({  
                                             BayesMVP_Stan_like_summary_tibble_gq <- model_fit$get_summary_generated_quantities () %>% print(n = 150) 
                                           }, silent = TRUE)
                                           ##
                                           require(posterior)  
                                           ## first extract the trace array object (note: already in a posterior-compatible format!)
                                           posterior_draws <- model_fit$get_posterior_draws()
                                           ##
                                           model_efficiency_metrics <- model_fit$get_efficiency_metrics()
                                           ##
                                           time_burnin <- model_efficiency_metrics$time_burnin  ; time_burnin 
                                           time_sampling <- model_efficiency_metrics$time_sampling ; time_sampling
                                           time_total_MCMC <- model_efficiency_metrics$time_total_MCMC  ; time_total_MCMC
                                           time_total_inc_summaries <- model_efficiency_metrics$time_total_inc_summaries ; time_total_inc_summaries # note this includes time to compute R-hat, etc 
                                           ##
                                           Min_ESS_main_params <- model_efficiency_metrics$Min_ESS_main   ; Min_ESS_main_params
                                           Min_ESS_per_sec_sampling <- model_efficiency_metrics$Min_ESS_per_sec_samp ; Min_ESS_per_sec_sampling
                                           Min_ESS_per_sec_overall <- model_efficiency_metrics$Min_ESS_per_sec_total ; Min_ESS_per_sec_overall
                                           ##
                                           Min_ESS_per_grad <- model_efficiency_metrics$Min_ESS_per_grad_sampling ; Min_ESS_per_grad
                                           grad_evals_per_sec <- model_efficiency_metrics$grad_evals_per_sec ; grad_evals_per_sec
                                           ##
                                           est_time_to_100_ESS <- model_efficiency_metrics$est_time_to_100_ESS_inc_summaries ; est_time_to_100_ESS
                                           est_time_to_1000_ESS <- model_efficiency_metrics$est_time_to_1000_ESS_inc_summaries ; est_time_to_1000_ESS
                                           est_time_to_10000_ESS <- model_efficiency_metrics$est_time_to_10000_ESS_inc_summaries; est_time_to_10000_ESS
                                           
                                           # est_time_5000_ESS <- model_fit$time_to_target_ESS(target_ESS = 5000) ; est_time_5000_ESS
                                           # est_time_5000_ESS
                                           
                                           ##
                                           # log_lik_trace <- model_fit$get_log_lik_trace()
                                           # str(log_lik_trace) # will be NULL unless you specify  "save_log_lik_trace = TRUE" in the "$summary()" method
                                           
                                           
                                           # #### Extract trace array:
                                           # BayesMVP_draws_array <- model$draws()
                                           
                                           ## model_fit$get_posterior_draws()
                                           
                                           ## Extract model traces:
                                           traces <- model_fit$get_all_traces()
                                           ## Trace for main:
                                           BayesMVP_draws_array_main <- traces$traces_as_arrays$trace_params_main
                                           ## str(BayesMVP_draws_array_main)
                                           
                                           
                                           #### ---- Re-format as a list (for main_params ONLY!):
                                           BayesMVP_trace_main_params_0 <- list()
                                           for (i in 1:n_params_main) {
                                             BayesMVP_trace_main_params_0[[i]] <- BayesMVP_draws_array_main[,,i]
                                           }
                                           ## str(BayesMVP_trace_main_params_0)
                                           ## Format:
                                           BayesMVP_trace_main_params <- list()
                                           for (i in 1:n_params_main) {
                                             mat <- array(data = BayesMVP_trace_main_params_0[[i]], dim = c(n_iter, n_chains_sampling))
                                             BayesMVP_trace_main_params[[i]] <- mat
                                           }
                                           str(BayesMVP_trace_main_params)
                                           
                                           #### ---- Compute split-ESS using BayesMVP:
                                           ## Min ESS:
                                           Min_ESS <- Min_ESS_main_params
                                           ## Print:
                                           print(paste("ESS (min) = ", Min_ESS))
                                           
                                           
                                           #### ---- Compute split-Rhat using BayesMVP:
                                           if (!(is.null(model_efficiency_metrics$Max_nested_rhat_main))) {
                                             Max_rhat <- round(model_efficiency_metrics$Max_rhat_main, 3) 
                                             ## Print:
                                             print(paste("rhat (max) = ", round(Max_rhat, 3)))
                                           } else { 
                                             Max_rhat <- NULL
                                           }
                                           
                                           #### ---- Compute nested R-hat (if enabled):
                                           if (!(is.null(model_efficiency_metrics$Max_nested_rhat_main))) {
                                             Max_nested_rhat <- round(model_efficiency_metrics$Max_nested_rhat_main, 3) 
                                             ## Print:
                                             print(paste("rhat_nested (max) = ", round(Max_nested_rhat, 3)))
                                           } else { 
                                             Max_nested_rhat <- NULL
                                           }
                                           
                                           #### ---- run time (total)
                                           print(paste("seed = ", seed))
                                           ##
                                           ## Times:
                                           total_time_seconds <- time_total_inc_summaries #  cmdstanr_model_out_timers$total
                                           total_time_mins <- total_time_seconds / 60
                                           total_time_hours <- total_time_mins / 60
                                           ##
                                           print(paste("total time =", round(total_time_seconds, 0), "seconds")) # in seconds
                                           print(paste("total time =", floor(total_time_mins), "minutes and ", round(((total_time_mins - floor(total_time_mins))*60), 0), "seconds"))    # in minutes
                                           print(round(total_time_mins, 3 ))
                                           print(paste("total time =", floor(total_time_hours), "hours and ", round(((total_time_hours - floor(total_time_hours))*60), 0), "minutes"))    # in hours
                                           ##
                                           #### Burnin times:
                                           burnin_time_seconds <- time_burnin  ##  max(cmdstanr_model_out_timers$chains$sampling)
                                           burnin_time_mins <- burnin_time_seconds / 60
                                           burnin_time_hours <- burnin_time_mins / 60
                                           ##
                                           #### ---- run time (post-burnin / sampling ONLY)
                                           pb_time_seconds <- time_sampling ##  max(cmdstanr_model_out_timers$chains$sampling)
                                           pb_time_mins <- pb_time_seconds / 60
                                           pb_time_hours <- pb_time_mins / 60
                                           ##
                                           print(paste("Sampling (post-burnin) time =", round(pb_time_seconds, 0), "seconds")) # in seconds
                                           print(paste("Sampling (post-burnin) time =", floor(pb_time_mins), "minutes and ", round(((pb_time_mins - floor(pb_time_mins))*60), 0), "seconds"))    # in minutes
                                           print(round(pb_time_mins, 3 ))
                                           print(paste("Sampling (post-burnin) time =", floor(pb_time_hours), "hours and ", round(((pb_time_hours - floor(pb_time_hours))*60), 0), "minutes"))    # in hours
                                           
                                           #### ----- Min ESS / sec (using total time)
                                           Min_ESS_per_sec_total_time <- Min_ESS / total_time_seconds
                                           print(paste("Min ESS / sec (total time) = ", signif(Min_ESS_per_sec_total_time, 5)))
                                           ##
                                           #### ----- Min ESS / sec (using post-burnin / sampling time)
                                           Min_ESS_per_sec_pb_time <- Min_ESS / pb_time_seconds
                                           print(paste("Min ESS / sec (sampling time only) = ", signif(Min_ESS_per_sec_pb_time, 5)))
                                           ##
                                           
                                           ## ----- Get MCMC / HMC info: 
                                           MCMC_info <- model_fit$get_HMC_info()
                                           ##
                                           diffusion_HMC <- MCMC_info$diffusion_HMC
                                           partitioned_HMC <- MCMC_info$partitioned_HMC
                                           ## For main params:
                                           eps_main <- MCMC_info$eps_main
                                           tau_main <- MCMC_info$tau_main
                                           L_main <-  tau_main / eps_main
                                           n_grad_evals_sampling_main <- L_main * n_iter * n_chains_sampling
                                           Min_ess_per_grad_main_samp <-  Min_ESS / n_grad_evals_sampling_main
                                           ##
                                           n_chains_sampling <-MCMC_info$n_chains_sampling
                                           n_chains_burnin <-MCMC_info$n_chains_burnin
                                           ##
                                           LR_main <- MCMC_info$LR_main
                                           adapt_delta <- MCMC_info$adapt_delta
                                           ##
                                           metric_type_main <- MCMC_info$metric_type_main
                                           metric_shape_main <- MCMC_info$metric_shape_main
                                           
                                           ## For nuisance params:
                                           if (partitioned_HMC == TRUE) {
                                                     LR_us <- MCMC_info$LR_us
                                                     ##
                                                     eps_us <-  MCMC_info$eps_us
                                                     tau_us <- MCMC_info$tau_us
                                                     L_us <-  tau_us / eps_us
                                                     n_grad_evals_sampling_us <- L_us * n_iter * n_chains_sampling
                                                     ##
                                                     metric_type_nuisance <- MCMC_info$metric_type_nuisance
                                                     metric_shape_nuisance <- MCMC_info$metric_shape_nuisance
                                           } else { 
                                                     LR_us <- NULL
                                                     ##
                                                     eps_us <-  NULL
                                                     tau_us <- NULL
                                                     L_us <- NULL
                                                     n_grad_evals_sampling_us <- NULL
                                                     ##
                                                     metric_type_nuisance <- NULL
                                                     metric_shape_nuisance <- NULL
                                           }
                                           
                                           # model_fit$get_efficiency_metrics()
                                           # model_efficiency_metrics
                                           
                                           
                                           #### ----- Min ESS / grad (uses post-burnin / sampling time ONLY)
                                           mean_L_burnin  <-  L_main ##  (mean(c(2^model$sampler_diagnostics(inc_warmup = TRUE)[1:n_burnin,,1] - 1)))  # mean L (total)
                                           print(paste("Mean L across chains (burnin)  = ", mean_L_burnin))
                                           ##
                                           mean_L_post_burnin  <-  L_main ## (mean(c(2^model$sampler_diagnostics()[,,1] - 1))) # mean # of Leapfrog steps (sampling only)
                                           print(paste("Mean L across chains (post-burnin) = ", mean_L_post_burnin))
                                           ##
                                           L_burn_means_per_chain <-  c()
                                           L_samp_means_per_chain <-  c()
                                           ##
                                           ## 
                                           for (i in 1:n_chains_burnin) {
                                             L_burn_means_per_chain[i]    <-  mean_L_burnin ## mean ( (  2^model$sampler_diagnostics(inc_warmup = TRUE)[1:n_burnin,,1] - 1  )[ ,i,1]) 
                                           }
                                           for (i in 1:n_chains_sampling) {
                                             L_samp_means_per_chain[i]    <-  mean_L_post_burnin ##  mean ( (  2^model$sampler_diagnostics()[,,1] - 1  )[1:n_iter,i,1]) 
                                           }
                                           # ##
                                           # max_of_mean_Ls_per_chain_burn <-      max(L_burn_means_per_chain)
                                           # max_of_mean_Ls_per_chain_samp <-      max(L_samp_means_per_chain)
                                           # ##
                                           # print(paste("Max L across chains (burnin)  = ", max_of_mean_Ls_per_chain_burn))
                                           # print(paste("Max L across chains (post-burnin) = ", max_of_mean_Ls_per_chain_samp))
                                           # ##
                                           # mean_eps_post_burnin  <-   (mean(c(model$sampler_diagnostics()[,,5]))) # mean # of Leapfrog steps (sampling only)
                                           # print(paste("Mean step-size across chains (post-burnin) = ", signif(mean_eps_post_burnin, 4)))
                                           ##
                                           ## total # of gradient evals
                                           total_gradient_evals <-      (mean_L_post_burnin*n_chains_sampling*n_iter + mean_L_burnin*n_chains_burnin*n_burnin)    
                                           total_gradient_evals_pb <-   (mean_L_post_burnin*n_chains_sampling*n_iter)
                                           ##
                                           min_ess_per_grad_eval <- Min_ESS / total_gradient_evals ; min_ess_per_grad_eval
                                           min_ess_per_grad_eval_pb <- Min_ESS / total_gradient_evals_pb ; min_ess_per_grad_eval
                                           ##
                                           print(paste("ESS/grad (total) = ", signif(1000 * min_ess_per_grad_eval, 3)))
                                           print(paste("ESS/grad (sampling) = ", signif(1000 * min_ess_per_grad_eval_pb, 3)))
                                           print(paste("grad/sec (sampling) = ",  signif(Min_ESS_per_sec_pb_time  / (1000 * min_ess_per_grad_eval_pb), 3)  ))
                                           
                                           #### ---- Divergences:
                                           total_divs <- n_divs
                                           pct_divs <- pct_divs
                                           print(paste("total_divs = ", total_divs))
                                           print(paste("pct_divs = ", pct_divs))
                                           
                                           
                                           
                                           #### ---- Model run time ("outer" timer) - using tictoc:
                                           {
                                             print(tictoc::toc(log = TRUE))
                                             log.txt <- tictoc::tic.log(format = TRUE)
                                             tictoc::tic.clearlog()
                                             time_total_inc_summaries_outer <- unlist(log.txt)
                                             ##
                                             extract_numeric_string <- str_extract(time_total_inc_summaries_outer, "\\d+\\.\\d+")   
                                             time_BayesMVP_total_inc_summaries_outer <- as.numeric(extract_numeric_string)
                                           }
                                           
                                           
                                           #### ------ Save result of SINGLE (iii-th) run:
                                           if (BayesMVP_Model_type == "LC_MVP") {
                                                     
                                                     # bool_to_int <- function(bool) { 
                                                     #   if (bool == FALSE) { 
                                                     #     bool_as_int <- 0
                                                     #   } else { 
                                                     #     bool_as_int <- 1
                                                     #   }
                                                     # }
                                                     # 
                                                     # metric_type_main_to_save <- ifelse(metric_type_main == "Hessian", "HESS", "EMP")
                                                     # metric_shape_main_to_save <- ifelse(metric_shape_main == "dense", "DENSE", "DIAG")
                                                     # 
                                                     # BayesMVP_BASELINE_file_name_string <- paste0(    "run_type", run_type, "_",
                                                     #                                                  "PC", computer, "_",
                                                     #                                                  "N",  N, "_",
                                                     #                                                  "seed", seed, "_",  
                                                     #                                                  "Mod", BayesMVP_Model_type, "_",   
                                                     #                                                  "diffHMC", bool_to_int(diffusion_HMC), "_",   
                                                     #                                                  "partHMC", bool_to_int(partitioned_HMC), "_",   
                                                     #                                                  "SIMD", vect_type, "_",
                                                     #                                                  ##
                                                     #                                                  "forceAD",  bool_to_int(force_autodiff), "_",
                                                     #                                                  "forcePLOG",  bool_to_int(force_PartialLog), "_",
                                                     #                                                  "multiA",  bool_to_int(multi_attempts), "_",
                                                     #                                                  ##
                                                     #                                                  "priorLKJ", prior_LKJ[1],"and", prior_LKJ[2], "_",  
                                                     #                                                  "posCOR", as.integer(corr_force_positive),  "_",  
                                                     #                                                  ##
                                                     #                                                  "burn_chains", n_chains_burnin, "_",
                                                     #                                                  "pb_chains", n_chains_sampling, "_",
                                                     #                                                  "burn", n_burnin, "_",
                                                     #                                                  "samp", n_iter, "_",
                                                     #                                                  "ad", adapt_delta, "_",
                                                     #                                                  "LR", learning_rate, "_", 
                                                     #                                                  "clip", clip_iter, "_", 
                                                     #                                                  "Mmain", metric_shape_main_to_save, metric_type_main_to_save)
                                             
                                             
                                                     BayesMVP_BASELINE_file_name_string <- R_fn_make_BayesMVP_BASELINE_file_name_string(  run_type = run_type,
                                                                                                                                          computer = computer,
                                                                                                                                          N = N,
                                                                                                                                          seed = seed,
                                                                                                                                          BayesMVP_Model_type = BayesMVP_Model_type,
                                                                                                                                          diffusion_HMC = diffusion_HMC,
                                                                                                                                          partitioned_HMC = partitioned_HMC,
                                                                                                                                          vect_type = vect_type,
                                                                                                                                          force_autodiff = force_autodiff,
                                                                                                                                          force_PartialLog = force_PartialLog,
                                                                                                                                          multi_attempts = multi_attempts,
                                                                                                                                          prior_LKJ = prior_LKJ,
                                                                                                                                          corr_force_positive = corr_force_positive,
                                                                                                                                          n_chains_burnin = n_chains_burnin,
                                                                                                                                          n_chains_sampling = n_chains_sampling,
                                                                                                                                          n_burnin = n_burnin,
                                                                                                                                          n_iter = n_iter,
                                                                                                                                          adapt_delta = adapt_delta,
                                                                                                                                          learning_rate = learning_rate,
                                                                                                                                          clip_iter = clip_iter,
                                                                                                                                          metric_type_main = metric_type_main,
                                                                                                                                          metric_shape_main = metric_shape_main)
                                                      
                                                     df_list_alg_efficiency_for_run_iii <- tibble(   software = "BayesMVP", 
                                                                                                     device = computer, 
                                                                                                     run_type = run_type, 
                                                                                                     N = N, 
                                                                                                     run_number = iii, 
                                                                                                     seed = seed, 
                                                                                                     model = BayesMVP_Model_type,
                                                                                                     ##
                                                                                                     diffusion_HMC = diffusion_HMC,
                                                                                                     partitioned_HMC = partitioned_HMC,
                                                                                                     ##
                                                                                                     vect_type = vect_type,
                                                                                                     ##
                                                                                                     force_autodiff = force_autodiff,
                                                                                                     force_PartialLog = force_PartialLog,
                                                                                                     multi_attempts = multi_attempts,
                                                                                                     ##
                                                                                                     prior_LKJ_1 = prior_LKJ[1],
                                                                                                     prior_LKJ_2 = prior_LKJ[2],
                                                                                                     corr_force_positive = corr_force_positive,
                                                                                                     ##
                                                                                                     n_chains_burnin = n_chains_burnin,
                                                                                                     n_chains_sampling = n_chains_sampling,
                                                                                                     n_burnin = n_burnin,
                                                                                                     n_iter = n_iter,
                                                                                                     adapt_delta = adapt_delta,
                                                                                                     LR = learning_rate,
                                                                                                     clip_iter = clip_iter,
                                                                                                     metric_shape_main = metric_shape_main,
                                                                                                     metric_type_main = metric_type_main,
                                                                                                     ##
                                                                                                     n_divs = n_divs,
                                                                                                     pct_divs = pct_divs,
                                                                                                     ##
                                                                                                     Max_rhat = Max_rhat,
                                                                                                     Max_nested_rhat = Max_nested_rhat,
                                                                                                     ##
                                                                                                     Min_ESS = Min_ESS,
                                                                                                     ##
                                                                                                     time_BayesMVP_total_inc_summaries_outer = time_BayesMVP_total_inc_summaries_outer,
                                                                                                     ##
                                                                                                     time_burnin = time_burnin,
                                                                                                     time_sampling = time_sampling,
                                                                                                     time_total_MCMC = time_total_MCMC,
                                                                                                     time_total_inc_summaries = time_total_inc_summaries,
                                                                                                     ##
                                                                                                     total_time_seconds = total_time_seconds,
                                                                                                     total_time_mins = total_time_mins,
                                                                                                     total_time_hours = total_time_hours,
                                                                                                     ##
                                                                                                     burnin_time_seconds = burnin_time_seconds,
                                                                                                     burnin_time_mins = burnin_time_mins,
                                                                                                     burnin_time_hours = burnin_time_hours,
                                                                                                     ##
                                                                                                     pb_time_seconds = pb_time_seconds,
                                                                                                     pb_time_mins = pb_time_mins,
                                                                                                     pb_time_hours = pb_time_hours,
                                                                                                     ##
                                                                                                     Min_ESS_per_sec_total_time = Min_ESS_per_sec_total_time,
                                                                                                     Min_ESS_per_sec_pb_time = Min_ESS_per_sec_pb_time,
                                                                                                     ##
                                                                                                     total_gradient_evals = total_gradient_evals,
                                                                                                     total_gradient_evals_pb = total_gradient_evals_pb,
                                                                                                     ##
                                                                                                     min_ess_per_grad_eval = min_ess_per_grad_eval,
                                                                                                     min_ess_per_grad_eval_pb = min_ess_per_grad_eval_pb,
                                                                                                     ##
                                                                                                     mean_L_burnin = mean_L_burnin,
                                                                                                     mean_L_post_burnin = mean_L_post_burnin,
                                                                                                     ##
                                                                                                     eps_main = eps_main,
                                                                                                     eps_us = eps_us,
                                                                                                     ##
                                                                                                     tau_main = tau_main,
                                                                                                     tau_us = tau_us)
                                                                                                     # ##
                                                                                                     # max_IQR = NA_real_,
                                                                                                     # max_IQR_index =NA_real_,
                                                                                                     # max_Range = NA_real_,
                                                                                                     # max_Range_index = NA_real_, 
                                                     
                                                     ## Add parameter estimates to DF:
                                                   
                                                    
                                                     {
                                                         for (t in 1:n_tests) { 
                                                            ## Save Se estimates:
                                                            row <- dplyr::filter(BayesMVP_Stan_like_summary_tibble_gq, parameter == paste0("Se_bin", ".", t))
                                                            df_list_alg_efficiency_for_run_iii[[as.character(paste0("Se", t, "_", "Median"))]] <-  row$`50%`
                                                            df_list_alg_efficiency_for_run_iii[[as.character(paste0("Se", t, "_", "Mean"))]] <-  row$mean
                                                            df_list_alg_efficiency_for_run_iii[[as.character(paste0("Se", t, "_", "lower95"))]] <- row$`2.5%`
                                                            df_list_alg_efficiency_for_run_iii[[as.character(paste0("Se", t, "_", "upper95"))]] <- row$`97.5%`
                                                            ## Save Sp estimates:
                                                            row <- dplyr::filter(BayesMVP_Stan_like_summary_tibble_gq, parameter == paste0("Sp_bin", ".", t))
                                                            df_list_alg_efficiency_for_run_iii[[as.character(paste0("Sp", t, "_", "Median"))]] <- row$`50%`
                                                            df_list_alg_efficiency_for_run_iii[[as.character(paste0("Sp", t, "_", "Mean"))]] <- row$mean
                                                            df_list_alg_efficiency_for_run_iii[[as.character(paste0("Sp", t, "_", "lower95"))]] <- row$`2.5%`
                                                            df_list_alg_efficiency_for_run_iii[[as.character(paste0("Sp", t, "_", "upper95"))]] <- row$`97.5%`
                                                         }
                                                         ## Save prev estimate:
                                                         row <-    dplyr::filter(BayesMVP_Stan_like_summary_tibble_gq, parameter == paste0("p", ".", "1"))
                                                         df_list_alg_efficiency_for_run_iii[[as.character(paste0("p", "_", "Median"))]] <- row$`50%`
                                                         df_list_alg_efficiency_for_run_iii[[as.character(paste0("p", "_", "Mean"))]] <- row$mean
                                                         df_list_alg_efficiency_for_run_iii[[as.character(paste0("p", "_", "lower95"))]] <- row$`2.5%`
                                                         df_list_alg_efficiency_for_run_iii[[as.character(paste0("p", "_", "upper95"))]] <- row$`97.5%`
                                                     }
                                                   
                                           }
                                           
                                           df_list_alg_efficiency_per_run[[iii]] <- df_list_alg_efficiency_for_run_iii
                                           
                                           ## 1. ------  save KEY SUMMARY info:
                                           ## Make list:
                                           BayesMVP_key_info_list <- list(  "BayesMVP_efficiency_info_tibble" = df_list_alg_efficiency_for_run_iii,
                                                                            "BayesMVP_summary_table_main_only_unconstrained" = BayesMVP_Stan_like_summary_tibble_main,
                                                                            "BayesMVP_summary_table_Se_Sp_prev" = BayesMVP_Stan_like_summary_tibble_gq,
                                                                            "BayesMVP_trace_main_params" = BayesMVP_trace_main_params)
                                           ## File name and path:
                                           file_name <- paste0("BayesMVP_key_info_", BayesMVP_BASELINE_file_name_string)
                                           file_path <- file.path(save_output_directory, file_name)
                                           ## save RDS file:
                                           saveRDS(object = BayesMVP_key_info_list, file = file_path)
                                           
                                           
                                           # ## Helper R function to remove "log_lik" parameter from trace array 
                                           # R_fn_remove_log_lik_from_array <- function(arr) {
                                           #   
                                           #       # Get the array
                                           #       arr <- model_samples$model_fit_object$traces$traces_as_arrays$trace_transformed_params
                                           #       format(object.size(arr), units = "MB")
                                           #       
                                           #       # Find which parameters don't contain "log_lik"
                                           #       keep_params <- !grepl("log_lik", dimnames(arr)[[3]])
                                           #       
                                           #       # Create new array without the log_lik parameters
                                           #       arr_filtered <- arr[, , keep_params, drop = FALSE]
                                           #       
                                           #       # Replace the original array
                                           #       model_samples$model_fit_object$traces$traces_as_arrays$trace_transformed_params <- arr_filtered
                                           #       arr <-   model_samples$model_fit_object$traces$traces_as_arrays$trace_transformed_params 
                                           #       
                                           #       return(arr)
                                           #   
                                           # }
                                           # 
                                           # array_containing_log_lik <-  model_samples$model_fit_object$traces$traces_as_arrays$trace_transformed_params
                                           # model_samples$model_fit_object$traces$traces_as_arrays$trace_transformed_params <- R_fn_remove_log_lik_from_array(array_containing_log_lik)
                                           
                                           ## 2. ------  save FULL BayesMVP output 
                                           ## WARNING: file may be large - but should be ** MUCH ** smaller than Stan ** UNLESS ** you track lots of nuisance parameters!!
                                           if (save_full_output == TRUE) {
                                                 ## File name and path:
                                                 file_name <- paste0("BayesMVP_FULL_output_", BayesMVP_BASELINE_file_name_string)
                                                 file_path <- file.path(save_output_directory, file_name)
                                                 ## save RDS file:
                                                 #### model$save_object(file = file_path)
                                                 model_samples$result$result[[6]] <- NULL ## replace large log_lik tract w/ NULL!
                                                 ##
                                                 BayesMVP_full_output_list <- list( ## model_samples = model_samples,
                                                                                    model_fit = model_fit)
                                                 saveRDS(object = BayesMVP_full_output_list, file = file_path)
                                           }
                                           
                                           # # Check sizes of components
                                           # format(object.size(model_samples), units = "MB")
                                           # format(object.size(model_fit), units = "MB")
                                           # 
                                           # # If model_samples is big, check its components
                                           # lapply(model_samples, function(x) format(object.size(x), units = "MB"))
                                           # lapply(model_fit, function(x) format(object.size(x), units = "MB"))
                                          #  
                                          #  str(model_fit) ## $summary_object$ $traces$traces_as_arrays$trace_transformed_params
                                          # str( model_samples$model_fit_object)
                                          # 
                                          # tr <- model_fit$get_all_traces()
                                          # str(tr)
                                          #  
                                          # model_samples$model_fit_object <- NULL
                                          # 
                                          # model_samples$result$result <- NULL
                                          #  
                                          #  model_samples$model_fit_object$all_param_outs_trace <- NULL
                                          #  
                                          #  str(model_samples$model_fit_object$all_param_outs_trace)
                                          #  
                                          #  str(model_samples$model_fit_object$traces$traces_as_arrays$trace_transformed_params)
                                          #  lapply(model_samples$model_fit_object$traces$traces_as_arrays$trace_transformed_params, function(x) format(object.size(x), units = "MB"))
     
 
                                         
                                     } ## // end of "n_runs" / iii loop
                                     
                                     try({  
                                       beepr::beep("ready")
                                     })
                                     
                                     ## 1. ------  save EFFICIENCY / ALGORITHM INFO:
                                     df_alg_efficiency_all_runs <- tibble(data.table::rbindlist(df_list_alg_efficiency_per_run))
                                     ## save tibble of all runs:
                                     {
                                         ## File name and path:
                                         file_name <- paste0("BayesMVP_EFF_info", "_", 
                                                             "n_runs", n_runs, "_", 
                                                             BayesMVP_BASELINE_file_name_string)
                                         file_path <- file.path(save_output_directory, file_name)
                                         ## save RDS file:
                                         saveRDS(object = df_alg_efficiency_all_runs, file = file_path)
                                     }
                                     
                               } ## // end of "n_iter_vec" loop
                           
                         } ## // end of "n_burnin_vec" loop
                     
                   } ## // end of "n_chains_sampling_vec" loop
         
       } ## // end of "n_chains_burnin_vec" loop
       
     } ## // end of N loop
                           
       
       
}




            # #### ------ SAVE KEY OUTPUTS:
            # BayesMVP_file_name_string <- paste0(  "run_type", run_type, "_",
            #                                       "computer_", computer, "_",
            #                                       "seed", seed, "_",  
            #                                       "Mod", BayesMVP_Model_type, "_",    
            #                                       "priorLKJ", prior_LKJ[1],"and", prior_LKJ[2], "_",  
            #                                       "posCOR", corr_force_positive,  "_",  
            #                                       "N",  N, "_",
            #                                       "burn_chains_", n_chains_burnin, "_",
            #                                       "pb_chains_", n_chains_sampling, "_",
            #                                       "burn_", n_burnin, "_",
            #                                       "samp_", n_iter, "_",
            #                                       "ad_", adapt_delta, "_",
            #                                       "LR_main_", LR_main, "_", 
            #                                       "clip_", clip_iter, 
            #                                       "M_main_", metric_shape_main, metric_type_main)
            #                                       
            #                                 
            # 
            # ## 1. ------  save BayesMVP output 
            # ## WARNING: file may be large - but should be ** MUCH ** smaller than Stan ** UNLESS ** you track lots of nuisance parameters!!
            # if (save_full_output == TRUE) {
            #   ## File name and path:
            #   file_name <- paste0("BayesMVP_FULL_output_", BayesMVP_file_name_string)
            #   file_path <- file.path(save_output_directory, file_name)
            #   ## save RDS file:
            #   #### model$save_object(file = file_path) 
            #   BayesMVP_full_output_list <- list( model_samples = model_samples,
            #                                      model_fit = model_fit)
            #   saveRDS(object = BayesMVP_full_output_list, file = file_path)
            # }
            

            
            # ##  -----  save key efficiency info:
            # ## outputs to save:
            # BayesMVP_efficiency_info_list <- list(  paste("total_divs = "), total_divs,
            #                                     paste("pct_divs = "), pct_divs,
            #                                     ##
            #                                     paste("time_BayesMVP_total_inc_summaries_outer = "), time_BayesMVP_total_inc_summaries_outer,
            #                                     ##
            #                                     paste("time_burnin = "), time_burnin,
            #                                     paste("time_sampling = "), time_sampling,
            #                                     paste("time_total_MCMC = "), time_total_MCMC,
            #                                     paste("time_total_inc_summaries = "), time_total_inc_summaries,
            #                                     ## paste("time_BayesMVP_total_inc_summaries = "), time_BayesMVP_total_inc_summaries,
            #                                     #### paste("cmdstanr_model_out_timers = "), cmdstanr_model_out_timers,
            #                                     ##
            #                                     paste("total_time_seconds = "), total_time_seconds,
            #                                     paste("total_time_mins = "), total_time_mins,
            #                                     paste("total_time_hours = "), total_time_hours,
            #                                     ##
            #                                     paste("burnin_time_seconds = "), burnin_time_seconds,
            #                                     paste("burnin_time_mins = "), burnin_time_mins,
            #                                     paste("burnin_time_hours = "), burnin_time_hours,
            #                                     ##
            #                                     paste("pb_time_seconds = "), pb_time_seconds,
            #                                     paste("pb_time_mins = "), pb_time_mins,
            #                                     paste("pb_time_hours = "), pb_time_hours,
            #                                     ##
            #                                     paste("Min_ESS_per_sec_total_time = "), Min_ESS_per_sec_total_time,
            #                                     paste("Min_ESS_per_sec_pb_time = "), Min_ESS_per_sec_pb_time,
            #                                     ##
            #                                     # paste("total time =", round(total_time_seconds, 0), "seconds"),
            #                                     # paste("total time =", floor(total_time_mins), "minutes and ", round(((total_time_mins - floor(total_time_mins))*60), 0), "seconds"),
            #                                     # paste("total time =", floor(total_time_hours), "hours and ", round(((total_time_hours - floor(total_time_hours))*60), 0), "minutes"),
            #                                     # paste("Sampling (post-burnin) time =", round(pb_time_seconds, 0), "seconds"),
            #                                     # paste("Sampling (post-burnin) time =", floor(pb_time_mins), "minutes and ", round(((pb_time_mins - floor(pb_time_mins))*60), 0), "seconds"),
            #                                     # paste("Sampling (post-burnin) time =", floor(pb_time_hours), "hours and ", round(((pb_time_hours - floor(pb_time_hours))*60), 0), "minutes"),
            #                                     # paste("Min ESS / sec (total time) = ", round(Min_ESS_per_sec_total_time, 5)),
            #                                     # paste("Min ESS / sec (sampling time only) = ", round(Min_ESS_per_sec_pb_time, 5)),
            #                                     ##
            #                                     paste("stan_total_gradient_evals = "), stan_total_gradient_evals,
            #                                     paste("stan_total_gradient_evals_pb = "), stan_total_gradient_evals_pb,
            #                                     ##
            #                                     paste("stan_min_ess_per_grad_eval = "), stan_min_ess_per_grad_eval,
            #                                     paste("stan_min_ess_per_grad_eval_pb = "), stan_min_ess_per_grad_eval_pb,
            #                                     ##
            #                                     paste("Min_ESS = "), Min_ESS,
            #                                     paste("Max_rhat = "), Max_rhat,
            #                                     paste("Max_nested_rhat = "), Max_nested_rhat,
            #                                     ##
            #                                     ##
            #                                     # paste("L_burn_means_per_chain = "), L_burn_means_per_chain,
            #                                     # paste("max_of_mean_Ls_per_chain_burn = "), max_of_mean_Ls_per_chain_burn,
            #                                     paste("mean_L_during_burnin = "), mean_L_burnin,
            #                                     ##
            #                                     # paste("L_samp_means_per_chain = "), L_samp_means_per_chain,
            #                                     # paste("max_of_mean_Ls_per_chain_samp = "), max_of_mean_Ls_per_chain_samp,
            #                                     paste("mean_L_post_burnin = "), mean_L_post_burnin,
            #                                     ##
            #                                     paste("mean_eps_pb_main = "), eps_main,
            #                                     paste("mean_eps_pb_us = "), eps_us,
            #                                     ##
            #                                     paste("mean_tau_pb_main = "), tau_main,
            #                                     paste("mean_tau_pb_us = "), tau_us
            # )
            # ##
            # ## 2. ------  save:  key info:
            # ## Make list:
            # BayesMVP_key_info_list <- list(  "BayesMVP_efficiency_info_list" = BayesMVP_efficiency_info_list,
            #                                  "BayesMVP_summary_table_main_only_unconstrained" = BayesMVP_Stan_like_summary_tibble_main,
            #                                  "BayesMVP_summary_table_Se_Sp_prev" = BayesMVP_Stan_like_summary_tibble_gq,
            #                                  "BayesMVP_trace_main_params" = BayesMVP_trace_main_params)
            # ## File name and path:
            # file_name <- paste0("BayesMVP_key_info_", BayesMVP_file_name_string)
            # file_path <- file.path(save_output_directory, file_name)
            # ## save RDS file:
            # saveRDS(object = BayesMVP_key_info_list, file = file_path)
            
 
 



            




