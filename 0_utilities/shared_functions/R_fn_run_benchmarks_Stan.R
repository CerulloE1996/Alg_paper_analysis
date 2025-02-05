


 

###  -------------  Stan - function to run LC_MVP model -------------------------------------------------------------------------------------------------------------------

 


R_fn_run_Stan_model_LC_MVP <- function(    global_list, 
                                           N_sample_size_of_dataset,
                                           run_number, 
                                           save_full_cmdstanr_output = FALSE,
                                           compute_nested_rhat = NULL,
                                           MCMC_seed,
                                           n_chains,
                                           n_superchains = NULL,
                                           n_iter,
                                           n_burnin,
                                           adapt_delta = 0.80,
                                           max_treedepth = 10,
                                           metric_type = "diag_e")  {
  
       ## Start timer:
       tictoc::tic("timer_outer")
  
       ## Start timer:
       tictoc::tic("timer_inner")
      
       gc(reset = TRUE) 
  
       ## Set key variables :
       N_sample_sizes_vec <- global_list$N_sample_sizes_vec
       ## total_num_datasets <- length(N_sample_sizes_vec)
       ## Get data frame index (e.g. for this study if N = 500 then df_index = 1,)
       df_index <- which(N_sample_sizes_vec == N_sample_size_of_dataset)
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
       ## Make Stan inits list:
       stan_inits_list <- list("u_raw" = inits_u_raw,
                               "col_one_raw" = inits_Omega_unc_col_one_raw,
                               "off_raw" = inits_Omega_unc_off_raw,
                               "beta_vec" = inits_beta_vec_raw,
                               "p_raw" = inits_prev_unc)
       
       #### ---- Run Stan model:
       model <- mod$sample(   data = stan_data_list[[df_index]],
                              seed = MCMC_seed,
                              chains = n_chains,
                              parallel_chains = n_chains,
                              iter_warmup = n_burnin,
                              iter_sampling = n_iter, 
                              refresh = round( ((n_burnin + n_iter)/10), 0),
                              init = rep(list(stan_inits_list), n_chains), 
                              save_warmup = 1, # for some efficiency stats (can turn off for sim. study)
                              metric = metric_type,
                              adapt_delta = adapt_delta, 
                              max_treedepth = max_treedepth)
       
       #### ---- Model run time ("inner" timer) - using tictoc:
       {
         print(tictoc::toc(log = TRUE))
         log.txt <- tictoc::tic.log(format = TRUE)
         tictoc::tic.clearlog()
         time_stan_total_inc_csv_inner <- unlist(log.txt)
       }
       
      
       extract_numeric_string <- str_extract(time_stan_total_inc_csv_inner, "\\d+\\.\\d+")   
       time_stan_total_inc_csv_inner_numeric <- as.numeric(extract_numeric_string)
      
      
       gc(reset = TRUE)
      

    
        {
          
              try({
                cmdstanr_model_out_unc <- model$summary(variables = c("col_one_raw", "off_raw", "beta_vec", "p_raw"), "mean", "median", "sd", "mad",  ~quantile(.x, probs = c(0.025,  0.975) ), "rhat" , "ess_bulk", "ess_tail")
                print(cmdstanr_model_out_unc, n = 100)
                # Min_ESS <- round(min(cmdstanr_model_out_unc$ess_bulk, na.rm=TRUE), 0)
                # print(paste("min ESS = ", Min_ESS))
              })
        
              try({
                cmdstanr_model_out <- model$summary(variables = c( "p", "Se_bin", "Sp_bin"  ), "mean", "median", "sd", "mad",  ~quantile(.x, probs = c(0.025,  0.975) ), "rhat" , "ess_bulk", "ess_tail")
                print(cmdstanr_model_out, n = 100)
              })
            
        }
      
       
        #### Extract trace array:
        stan_draws_array <- model$draws()
        
        
        #### Get main params index:
        offset <- 1 ## BOOKMARK - 1st param is "lp__"
        index_nuisance_adj <- (1 + offset):(n_nuisance + offset)
        index_main_params_adj <- (n_nuisance + offset + 1):(n_params + offset)
        
        
        #### ---- Re-format as a list (for main_params ONLY!):
        Stan_trace_main_params_0 <- list()
        for (i in 1:n_params_main) {
          Stan_trace_main_params_0[[i]] <- stan_draws_array[,,index_main_params_adj[i]]
        }
        ## Format:
        Stan_trace_main_params <- list()
        for (i in 1:n_params_main) {
          mat <- array(data = Stan_trace_main_params_0[[i]], dim = c(n_iter, n_chains))
          Stan_trace_main_params[[i]] <- mat
        }
        
        
        #### ---- Compute split-ESS using BayesMVP:
        Rcpp_outs_split_ESS <- BayesMVP:::Rcpp_compute_MCMC_diagnostics( mcmc_3D_array = Stan_trace_main_params,
                                                                         diagnostic = "split_ESS",
                                                                    n_threads = parallel::detectCores())
        ess_vec <- Rcpp_outs_split_ESS$diagnostics[,1]
        ## Min ESS:
        Min_ESS <- round(min(ess_vec, na.rm = TRUE), 0)
        ## Print:
        print(paste("ESS (min) = ", Min_ESS))
        
        
        #### ---- Compute split-Rhat using BayesMVP:
        Rcpp_outs_split_rhat <- BayesMVP:::Rcpp_compute_MCMC_diagnostics( mcmc_3D_array = Stan_trace_main_params,
                                                                          diagnostic = "split_rhat",
                                                                     n_threads = parallel::detectCores())
        rhats_vec <-  Rcpp_outs_split_rhat$diagnostics[,1]  
        ## Max R-hat:
        Max_rhat <-  round(max(rhats_vec, na.rm = TRUE), 3)
        ## Print:
        print(paste("rhat (max) = ", round(Max_rhat, 3)))
        
        
        #### ---- Compute nested R-hat (if enabled):
        Max_rhat_nested <- "Nested R-hat not computed for this run"
        if (is.null(compute_nested_rhat)) { 
            if (n_chains > 15) {  ## only compute nested R-hat if at least 16 chains are used 
              compute_nested_rhat <- TRUE
            } else { 
              compute_nested_rhat <- TRUE
            }
        }
        
        if (compute_nested_rhat == TRUE) {
            ## Use BayesMVP helper fn to create superchain ID's (for nested R-hat):
            superchain_ids <- BayesMVP:::create_superchain_ids(n_superchains = n_superchains,  
                                                               n_chains = n_chains)
            rhats_nested_vec <- c()
            for (i in 1:n_params_main) {
              rhats_nested_vec[i] <-   posterior::rhat_nested( array(c(stan_draws_array[,,index_main_params_adj[i]]), dim = c(iter_sampling, n_chains)), superchain_ids =superchain_ids )
              #### rhats_vec[i] <-   posterior::rhat( array(c(stan_draws_array[,,index_main_params_adj[i]]), dim = c(iter_sampling, n_chains)) )
            }
            Max_rhat_nested <- round(max(rhats_nested_vec, na.rm = TRUE), 3)
            print(paste("rhat_nested (max) = ", Max_rhat_nested))
        }
        
        
        #### ---- Model run time (using tictoc):
        {
          print(tictoc::toc(log = TRUE))
          log.txt <- tictoc::tic.log(format = TRUE)
          tictoc::tic.clearlog()
          time_stan_total_inc_csv <- unlist(log.txt)
        }
        
        time_stan_total_real_world <- as.numeric(substr(time_stan_total_inc_csv,  8,  14))
        
        
        #### ---- run time (total)
        cmdstanr_model_out_timers <- model$time()
        total_time_seconds <- time_stan_total_real_world #  cmdstanr_model_out_timers$total
        total_time_mins <- total_time_seconds / 60
        total_time_hours <- total_time_mins / 60
        
        print(paste("seed = ", seed))
        
        print(paste("total time =", round(total_time_seconds, 0), "seconds")) # in seconds
        print(paste("total time =", floor(total_time_mins), "minutes and ", round(((total_time_mins - floor(total_time_mins))*60), 0), "seconds"))    # in minutes
        print(round(total_time_mins, 3 ))
        print(paste("total time =", floor(total_time_hours), "hours and ", round(((total_time_hours - floor(total_time_hours))*60), 0), "minutes"))    # in hours
        
        #### ---- run time (post-burnin / sampling ONLY)
        pb_time_seconds <- max(cmdstanr_model_out_timers$chains$sampling)
        pb_time_mins <- pb_time_seconds / 60
        pb_time_hours <- pb_time_mins / 60
        
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
        #### ----- Min ESS / grad (uses post-burnin / sampling time ONLY)
        mean_L  <-   (mean(c(2^model$sampler_diagnostics(inc_warmup = TRUE)[1:iter_warmup,,1] - 1)))  # mean L (total)
        print(paste("Mean L across chains (burnin)  = ", mean_L))
        ##
        mean_L_post_burnin  <-   (mean(c(2^model$sampler_diagnostics()[,,1] - 1))) # mean # of Leapfrog steps (sampling only)
        print(paste("Mean L across chains (post-burnin) = ", mean_L_post_burnin))
        ##
        L_burn_means_per_chain <-  c()
        L_samp_means_per_chain <-  c()
        ##
        for (i in 1:n_chains) {
          L_burn_means_per_chain[i]    <-  mean ( (  2^model$sampler_diagnostics(inc_warmup = TRUE)[1:iter_warmup,,1] - 1  )[ ,i,1]) 
          L_samp_means_per_chain[i]    <-  mean ( (  2^model$sampler_diagnostics()[,,1] - 1  )[1:iter_sampling,i,1]) 
        }
        ##
        max_of_mean_Ls_per_chain_burn <-      max(L_burn_means_per_chain)
        max_of_mean_Ls_per_chain_samp <-      max(L_samp_means_per_chain)
        ##
        print(paste("Max L across chains (burnin)  = ", max_of_mean_Ls_per_chain_burn))
        print(paste("Max L across chains (post-burnin) = ", max_of_mean_Ls_per_chain_samp))
        ##
        mean_eps_post_burnin  <-   (mean(c(model$sampler_diagnostics()[,,5]))) # mean # of Leapfrog steps (sampling only)
        print(paste("Mean step-size across chains (post-burnin) = ", signif(mean_eps_post_burnin, 4)))
        ##
        ## total # of gradient evals
        stan_total_gradient_evals <-   (mean_L*n_chains*(iter_sampling + iter_warmup))   #  n_chains * iter_sampling * stan_n_leapfrogs ; stan_total_gradient_evals
        stan_total_gradient_evals_pb <-   (mean_L_post_burnin*n_chains*iter_sampling)
        ##
        stan_min_ess_per_grad_eval <- Min_ESS / stan_total_gradient_evals ; stan_min_ess_per_grad_eval
        stan_min_ess_per_grad_eval_pb <- Min_ESS / stan_total_gradient_evals_pb ; stan_min_ess_per_grad_eval
        ##
        print(paste("ESS/grad (total) = ", signif(1000 * stan_min_ess_per_grad_eval, 3)))
        print(paste("ESS/grad (sampling) = ", signif(1000 * stan_min_ess_per_grad_eval_pb, 3)))
        print(paste("grad/sec (sampling) = ",  signif(Min_ESS_per_sec_pb_time  / (1000 * stan_min_ess_per_grad_eval_pb), 3)  ))
        
        #### ---- Divergences:
        total_divs <- sum(model$sampler_diagnostics()[,,2])
        print(paste("total_divs = ", total_divs))
        
        #### ------ SAVE KEY OUTPUTS:
        stan_file_name_string <- paste0(  "run_num", run_number, "_",
                                          "seed", seed, "_",  
                                          "Mod", Model_type, "_",     
                                          "PO", prior_only,  "_",    
                                          "priorLKJ", prior_LKJ[1],"and", prior_LKJ[2], "_",  
                                          "posCOR", corr_force_positive,  "_",  
                                          "N",  N, "_",
                                          "chains_",n_chains, "_",
                                          "burn_", iter_warmup, "_",
                                          "samp_", iter_sampling, "_",
                                          "ad_", adapt_delta, "_",
                                          "maxtree_", max_treedepth, "_",
                                          "Mtype", metric_type)
        
        
        ##  -----  save key efficiency info:
        ## outputs to save:
        Stan_efficiency_info_list <- list(  paste("total_divs = "), total_divs,
                                            ##
                                            paste("time_stan_total_inc_csv_inner_numeric = "), time_stan_total_inc_csv_inner_numeric,
                                            paste("time_stan_total_inc_csv_outer_numeric = "), time_stan_total_inc_csv_outer_numeric,
                                            paste("cmdstanr_model_out_timers = "), cmdstanr_model_out_timers,
                                            ##
                                            paste("total_time_seconds = "), total_time_seconds,
                                            paste("total_time_mins = "), total_time_mins,
                                            paste("total_time_hours = "), total_time_hours,
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
                                            paste("Max_rhat_nested = "), Max_rhat_nested,
                                            ##
                                            ##
                                            paste("L_burn_means_per_chain = "), L_burn_means_per_chain,
                                            paste("max_of_mean_Ls_per_chain_burn = "), max_of_mean_Ls_per_chain_burn,
                                            paste("mean_L = "), mean_L,
                                            ##
                                            paste("L_samp_means_per_chain = "), L_samp_means_per_chain,
                                            paste("max_of_mean_Ls_per_chain_samp = "), max_of_mean_Ls_per_chain_samp,
                                            paste("mean_L_post_burnin = "), mean_L_post_burnin,
                                            ##
                                            paste("mean_eps_post_burnin = "), mean_eps_post_burnin)
        ##
        ## 1. ------  save:  key efficiency info  + partial cmdstanr output + main_params trace:
        ## Make list:
        Stan_key_info_list <- list("Stan_efficiency_info_list" = Stan_efficiency_info_list,
                                   "Stan_summary_table_main_only_unconstrained" = cmdstanr_model_out_unc,
                                   "Stan_summary_table_Se_Sp_prev" = cmdstanr_model_out,
                                   "Stan_trace_main_params" = Stan_trace_main_params)
        ## File name:
        file_name <- paste0("Stan_key_info_", stan_file_name_string)
        ## save RDS file:
        saveRDS(object = Stan_key_info_list, file = file_name)
        
        ## 2. ------  save FULL cmdstanr output (WARNING: file may be massive!)
        if (save_full_cmdstanr_output == TRUE) {
            ## File name:
            file_name <- paste0("Stan_FULL_output_", stan_file_name_string)
            ## save RDS file:
            model$save_object(file = file_name)
        }
        

  
}





