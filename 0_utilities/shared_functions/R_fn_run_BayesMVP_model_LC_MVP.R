

###  -------------  Stan - function to run LC_MVP model -------------------------------------------------------------------------------------------------------------------

## debug:
# 
# BayesMVP_model_obj = BayesMVP_model_obj
# BayesMVP_settings_list = BayesMVP_pilot_study_list
# global_list = global_list
# Stan_data_list = Stan_data_list
# ##
# manual_gradients = manual_gradients
# SIMD_vect_type = "Stan"
# partitioned_HMC = TRUE
# diffusion_HMC = TRUE
# ##
# N_sample_size_of_dataset = N
# run_number = i
# save_output_directory =  BayesMVP_pilot_study_list$output_path
# save_full_output = TRUE
# compute_nested_rhat = NULL
# save_log_lik_trace = FALSE
# compute_transformed_parameters = TRUE
# compute_generated_quantities = TRUE
# ##
# MCMC_seed = i
# n_chains_burnin = n_chains_burnin
# n_chains_sampling = n_chains_sampling
# n_superchains = n_superchains
# n_iter = n_iter
# n_burnin = n_burnin


R_fn_run_BayesMVP_model_LC_MVP <- function(    computer,
                                               run_type,
                                               BayesMVP_model_obj,
                                               BayesMVP_settings_list,
                                               global_list, 
                                               Stan_data_list,
                                               ##
                                               manual_gradients,
                                               SIMD_vect_type,
                                               partitioned_HMC,
                                               diffusion_HMC,
                                               ##
                                               N_sample_size_of_dataset,
                                               run_number, 
                                               save_output_directory = NULL,
                                               save_full_output = FALSE,
                                               compute_nested_rhat = NULL,
                                               save_log_lik_trace = FALSE,
                                               compute_transformed_parameters = TRUE,
                                               compute_generated_quantities = TRUE,
                                               ##
                                               MCMC_seed,
                                               n_chains_burnin,
                                               n_chains_sampling,
                                               n_superchains = NULL,
                                               n_iter = 1000,
                                               n_burnin = 500
                                               )  {
  
       ## Start timer:
       tictoc::tic("timer_outer")
      
       gc(reset = TRUE) 
  
       
       #### Load BayesMVP data stuff:
       model_args_list <- BayesMVP_settings_list$model_args_list
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
       ## "Data" which depens on N:
       df_index <- which(N_sample_sizes_vec == N_sample_size_of_dataset)
       N <- N_sample_sizes_vec[df_index]
       y <- y_binary_list[[df_index]]
       pop_group <- rep(1, N)
       
       
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
       adapt_delta <- BayesMVP_settings_list$adapt_delta
       learning_rate = BayesMVP_settings_list$learning_rate
       metric_shape_main = BayesMVP_settings_list$metric_shape_main
       metric_type_main = BayesMVP_settings_list$metric_type_main
       clip_iter = BayesMVP_settings_list$clip_iter
       interval_width_main = BayesMVP_settings_list$interval_width_main
       tau_mult = BayesMVP_settings_list$tau_mult
       ratio_M_us = BayesMVP_settings_list$ratio_M_us
       ratio_M_main = BayesMVP_settings_list$ratio_M_main
       force_autodiff = BayesMVP_settings_list$force_autodiff
       force_PartialLog = BayesMVP_settings_list$force_PartialLog
       multi_attempts = BayesMVP_settings_list$multi_attempts
       # 
       # 
       # N_sample_sizes_vec <- global_list$N_sample_sizes_vec
       # ## total_num_datasets <- length(N_sample_sizes_vec)
       # ## Get data frame index (e.g. for this study if N = 500 then df_index = 1,)
       # df_index <- which(N_sample_sizes_vec == N_sample_size_of_dataset)
       ##
       seed <- MCMC_seed
       iter_warmup <- n_burnin
       iter_sampling <- n_iter
       ##
       n_params_main <- global_list$Model_settings_list$n_params_main
       print(paste("df_index = ", df_index))
       n_nuisance <- prod(dim(global_list$initial_values_list$inits_u_list[[df_index]]))
       n_params <- n_params_main + n_nuisance
       index_nuisance <- 1:n_nuisance
       index_main_params <- (n_nuisance + 1):n_params
       ##
       ## Set inits:
       inits_u_raw <- global_list$initial_values_list$inits_u_list[[df_index]]
       inits_Omega_unc_col_one_raw <- global_list$initial_values_list$inits_Omega_list$inits_Omega_unc_col_one_raw
       inits_Omega_unc_off_raw <- global_list$initial_values_list$inits_Omega_list$inits_Omega_unc_off_raw
       inits_beta_vec_raw <- global_list$initial_values_list$inits_beta_list$inits_beta_vec
       inits_prev_unc <- global_list$initial_values_list$inits_prev_list$inits_prev_unc
       
       ## Make Stan / BayesMVP inits list:
       ## Note: For the LC_MVP (even with manual gradients!) BayesMVP uses the same structure as the Stan LC_MVP model for initial values !!
       stan_or_BayesMVP_inits_list <- list(  "u_raw" = inits_u_raw,
                                             "col_one_raw" = inits_Omega_unc_col_one_raw,
                                             "off_raw" = inits_Omega_unc_off_raw,
                                             "beta_vec" = inits_beta_vec_raw,
                                             "p_raw" = array(inits_prev_unc))
       
       init_lists_per_chain <- rep(list(stan_or_BayesMVP_inits_list), n_chains_burnin) 
       
       ##
       n_nuisance_to_track <- 10 # set to some small number (< 10) if don't care about making inference on nuisance params (which is most of the time!)
       
      # if (partitioned_HMC == TRUE) {
         sample_nuisance <- TRUE
      # } 
       # if (partitioned_HMC == FALSE) {
       #   sample_nuisance <- FALSE
       # }
       
       print(paste("model_args_list = "))
       print(str(model_args_list))
       ##
       print(paste("N = ", N))
       ##
       print(paste("y = "))
       print(str(y))
       
       
   
       # if (BayesMVP_Model_type == "Stan") { 
       #   model_args_list <- NULL
       # } else { 
       #   Stan_data_list <- NULL
       # }
       
        print(str(Stan_data_list))
       
       Stan_data_list_given_current_N <- Stan_data_list[[df_index]]
       print(str(Stan_data_list_given_current_N))
       
       #### ---- Run model ------------------------------------------------------------:
       model_samples <-  BayesMVP_model_obj$sample(   Model_type = BayesMVP_Model_type,
                                                      partitioned_HMC = partitioned_HMC,
                                                      diffusion_HMC = diffusion_HMC,
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
                                                      ## Some other SAMPLER / MCMC arguments:
                                                      sample_nuisance = sample_nuisance,
                                                      force_autodiff = force_autodiff,
                                                      force_PartialLog = force_PartialLog,
                                                      multi_attempts = multi_attempts,
                                                      adapt_delta = adapt_delta,
                                                      learning_rate = learning_rate,
                                                      metric_shape_main = metric_shape_main,
                                                      metric_type_main = metric_type_main,
                                                      tau_mult = tau_mult,
                                                      clip_iter = clip_iter,
                                                      interval_width_main = interval_width_main,
                                                      ratio_M_us = ratio_M_us,
                                                      ratio_M_main = ratio_M_main,
                                                      parallel_method = "RcppParallel",
                                                      vect_type = vect_type,
                                                      n_nuisance_to_track = n_nuisance_to_track)
       
       
       # model_samples <-  BayesMVP_model_obj$sample(   partitioned_HMC = partitioned_HMC,
       #                                       diffusion_HMC = diffusion_HMC,
       #                                       ##
       #                                       seed = 1,
       #                                       n_burnin = n_burnin,
       #                                       n_iter = n_iter,
       #                                       n_chains_sampling = n_chains_sampling,
       #                                       n_superchains = n_superchains,
       #                                       ## Some other arguments:
       #                                       Stan_data_list = list(),
       #                                       y = y,
       #                                       N = N,
       #                                       n_params_main = n_params_main,
       #                                       n_nuisance = n_nuisance,
       #                                       ##
       #                                       init_lists_per_chain = init_lists_per_chain,
       #                                       n_chains_burnin = n_chains_burnin,
       #                                       model_args_list = model_args_list,
       #                                       ## Some other SAMPLER / MCMC arguments:
       #                                       sample_nuisance = TRUE,
       #                                       ##
       #                                       force_autodiff = FALSE,
       #                                       force_PartialLog = FALSE,
       #                                       multi_attempts = FALSE,
       #                                       ##
       #                                       adapt_delta = 0.80,
       #                                       learning_rate = 0.05,
       #                                       metric_shape_main = "dense",
       #                                       metric_type_main = "Hessian",
       #                                       tau_mult = 2.0,
       #                                       clip_iter = 25,
       #                                       interval_width_main = 50,
       #                                       ratio_M_us = 0.25,
       #                                       ratio_M_main = 0.25,
       #                                       parallel_method = "RcppParallel",
       #                                       #### parallel_method = "OpenMP",
       #                                       ## vect_type = "AVX512",
       #                                       vect_type = "AVX2",
       #                                       ## vect_type = "Stan",
       #                                       n_nuisance_to_track = n_nuisance_to_track)
       
       
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
           BayesMVP_Stan_like_summary_tibble_main <- model_fit$get_summary_main() %>% print(n = 50) 
           # nice summary tibble for transformed parameters, includes ESS/Rhat, etc
           BayesMVP_Stan_like_summary_tibble_tp <- model_fit$get_summary_transformed() %>% print(n = 150) 
           # nice summary tibble for generated quantities, includes ESS/Rhat, etc (for LC-MVP this includes Se/Sp/prevalence)
           BayesMVP_Stan_like_summary_tibble_gq <- model_fit$get_summary_generated_quantities () %>% print(n = 150) 
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
            
   
            # #### ---- Model run time (using tictoc):
            # {
            #   print(tictoc::toc(log = TRUE))
            #   log.txt <- tictoc::tic.log(format = TRUE)
            #   tictoc::tic.clearlog()
            #   time_stan_total_inc_csv <- unlist(log.txt)
            # }
            # 
            # time_stan_total_real_world <- as.numeric(substr(time_stan_total_inc_csv,  8,  14))
            # 
            
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
            mean_L_burnin  <-  L_main ##  (mean(c(2^model$sampler_diagnostics(inc_warmup = TRUE)[1:iter_warmup,,1] - 1)))  # mean L (total)
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
              L_burn_means_per_chain[i]    <-  mean_L_burnin ## mean ( (  2^model$sampler_diagnostics(inc_warmup = TRUE)[1:iter_warmup,,1] - 1  )[ ,i,1]) 
            }
            for (i in 1:n_chains_sampling) {
              L_samp_means_per_chain[i]    <-  mean_L_post_burnin ##  mean ( (  2^model$sampler_diagnostics()[,,1] - 1  )[1:iter_sampling,i,1]) 
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
            stan_total_gradient_evals <-      (mean_L_post_burnin*n_chains_sampling*iter_sampling + mean_L_burnin*n_chains_burnin*iter_warmup)    
            stan_total_gradient_evals_pb <-   (mean_L_post_burnin*n_chains_sampling*iter_sampling)
            ##
            stan_min_ess_per_grad_eval <- Min_ESS / stan_total_gradient_evals ; stan_min_ess_per_grad_eval
            stan_min_ess_per_grad_eval_pb <- Min_ESS / stan_total_gradient_evals_pb ; stan_min_ess_per_grad_eval
            ##
            print(paste("ESS/grad (total) = ", signif(1000 * stan_min_ess_per_grad_eval, 3)))
            print(paste("ESS/grad (sampling) = ", signif(1000 * stan_min_ess_per_grad_eval_pb, 3)))
            print(paste("grad/sec (sampling) = ",  signif(Min_ESS_per_sec_pb_time  / (1000 * stan_min_ess_per_grad_eval_pb), 3)  ))
            
            #### ---- Divergences:
            total_divs <- n_divs
            pct_divs <- pct_divs
            print(paste("total_divs = ", total_divs))
            print(paste("pct_divs = ", pct_divs))
            
            #### ------ SAVE KEY OUTPUTS:
            BayesMVP_file_name_string <- paste0(  "run_type", run_type, "_",
                                                  "computer_", computer, "_",
                                                  "seed", seed, "_",  
                                                  "Mod", BayesMVP_Model_type, "_",    
                                                  "priorLKJ", prior_LKJ[1],"and", prior_LKJ[2], "_",  
                                                  "posCOR", corr_force_positive,  "_",  
                                                  "N",  N, "_",
                                                  "burn_chains_", n_chains_burnin, "_",
                                                  "pb_chains_", n_chains_sampling, "_",
                                                  "burn_", iter_warmup, "_",
                                                  "samp_", iter_sampling, "_",
                                                  "ad_", adapt_delta, "_",
                                                  "LR_main_", LR_main, "_", 
                                                  "clip_", clip_iter, 
                                                  "M_main_", metric_shape_main, metric_type_main)
                                                  
                                            
            
            ## 1. ------  save BayesMVP output 
            ## WARNING: file may be large - but should be ** MUCH ** smaller than Stan ** UNLESS ** you track lots of nuisance parameters!!
            if (save_full_output == TRUE) {
              ## File name and path:
              file_name <- paste0("BayesMVP_FULL_output_", BayesMVP_file_name_string)
              file_path <- file.path(save_output_directory, file_name)
              ## save RDS file:
              #### model$save_object(file = file_path) 
              BayesMVP_full_output_list <- list( model_samples = model_samples,
                                                 model_fit = model_fit)
              saveRDS(object = BayesMVP_full_output_list, file = file_path)
            }
            
            
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
            
            ##  -----  save key efficiency info:
            ## outputs to save:
            BayesMVP_efficiency_info_list <- list(  paste("total_divs = "), total_divs,
                                                paste("pct_divs = "), pct_divs,
                                                ##
                                                paste("time_BayesMVP_total_inc_summaries_outer = "), time_BayesMVP_total_inc_summaries_outer,
                                                ##
                                                paste("time_burnin = "), time_burnin,
                                                paste("time_sampling = "), time_sampling,
                                                paste("time_total_MCMC = "), time_total_MCMC,
                                                paste("time_total_inc_summaries = "), time_total_inc_summaries,
                                                ## paste("time_BayesMVP_total_inc_summaries = "), time_BayesMVP_total_inc_summaries,
                                                #### paste("cmdstanr_model_out_timers = "), cmdstanr_model_out_timers,
                                                ##
                                                paste("total_time_seconds = "), total_time_seconds,
                                                paste("total_time_mins = "), total_time_mins,
                                                paste("total_time_hours = "), total_time_hours,
                                                ##
                                                paste("burnin_time_seconds = "), burnin_time_seconds,
                                                paste("burnin_time_mins = "), burnin_time_mins,
                                                paste("burnin_time_hours = "), burnin_time_hours,
                                                ##
                                                paste("pb_time_seconds = "), pb_time_seconds,
                                                paste("pb_time_mins = "), pb_time_mins,
                                                paste("pb_time_hours = "), pb_time_hours,
                                                ##
                                                paste("Min_ESS_per_sec_total_time = "), Min_ESS_per_sec_total_time,
                                                paste("Min_ESS_per_sec_pb_time = "), Min_ESS_per_sec_pb_time,
                                                ##
                                                # paste("total time =", round(total_time_seconds, 0), "seconds"),
                                                # paste("total time =", floor(total_time_mins), "minutes and ", round(((total_time_mins - floor(total_time_mins))*60), 0), "seconds"),
                                                # paste("total time =", floor(total_time_hours), "hours and ", round(((total_time_hours - floor(total_time_hours))*60), 0), "minutes"),
                                                # paste("Sampling (post-burnin) time =", round(pb_time_seconds, 0), "seconds"),
                                                # paste("Sampling (post-burnin) time =", floor(pb_time_mins), "minutes and ", round(((pb_time_mins - floor(pb_time_mins))*60), 0), "seconds"),
                                                # paste("Sampling (post-burnin) time =", floor(pb_time_hours), "hours and ", round(((pb_time_hours - floor(pb_time_hours))*60), 0), "minutes"),
                                                # paste("Min ESS / sec (total time) = ", round(Min_ESS_per_sec_total_time, 5)),
                                                # paste("Min ESS / sec (sampling time only) = ", round(Min_ESS_per_sec_pb_time, 5)),
                                                ##
                                                paste("stan_total_gradient_evals = "), stan_total_gradient_evals,
                                                paste("stan_total_gradient_evals_pb = "), stan_total_gradient_evals_pb,
                                                ##
                                                paste("stan_min_ess_per_grad_eval = "), stan_min_ess_per_grad_eval,
                                                paste("stan_min_ess_per_grad_eval_pb = "), stan_min_ess_per_grad_eval_pb,
                                                ##
                                                paste("Min_ESS = "), Min_ESS,
                                                paste("Max_rhat = "), Max_rhat,
                                                paste("Max_nested_rhat = "), Max_nested_rhat,
                                                ##
                                                ##
                                                # paste("L_burn_means_per_chain = "), L_burn_means_per_chain,
                                                # paste("max_of_mean_Ls_per_chain_burn = "), max_of_mean_Ls_per_chain_burn,
                                                paste("mean_L_during_burnin = "), mean_L_burnin,
                                                ##
                                                # paste("L_samp_means_per_chain = "), L_samp_means_per_chain,
                                                # paste("max_of_mean_Ls_per_chain_samp = "), max_of_mean_Ls_per_chain_samp,
                                                paste("mean_L_post_burnin = "), mean_L_post_burnin,
                                                ##
                                                paste("mean_eps_pb_main = "), eps_main,
                                                paste("mean_eps_pb_us = "), eps_us,
                                                ##
                                                paste("mean_tau_pb_main = "), tau_main,
                                                paste("mean_tau_pb_us = "), tau_us
            )
            ##
            ## 2. ------  save:  key info:
            ## Make list:
            BayesMVP_key_info_list <- list(  "BayesMVP_efficiency_info_list" = BayesMVP_efficiency_info_list,
                                             "BayesMVP_summary_table_main_only_unconstrained" = BayesMVP_Stan_like_summary_tibble_main,
                                             "BayesMVP_summary_table_Se_Sp_prev" = BayesMVP_Stan_like_summary_tibble_gq,
                                             "BayesMVP_trace_main_params" = BayesMVP_trace_main_params)
            ## File name and path:
            file_name <- paste0("BayesMVP_key_info_", BayesMVP_file_name_string)
            file_path <- file.path(save_output_directory, file_name)
            ## save RDS file:
            saveRDS(object = BayesMVP_key_info_list, file = file_path)
            
            
}





