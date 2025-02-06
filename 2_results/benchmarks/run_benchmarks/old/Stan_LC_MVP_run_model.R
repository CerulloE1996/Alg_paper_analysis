

# - | ----------  run models using Stan --------------------------------------------------------------------
   
  {
    
      N <- 5000 ## first choose sample size 
      source("Stan_LC_MVP_prepare_data.R") ### source file to prepare Stan data / inits 
      
  }
  


 

# library(bridgestan)
# 
# dummy_data_N <-  100
# dummy_data_vec <- rnorm(dummy_data_N)
# dummy_data <- list(N = dummy_data_N, y = dummy_data_vec )
# # convert data to JSON format (use cmdstanr::write_stan_json NOT jsonlite::tload_and_run_log_prob_grad_all_StanoJSON)
# r_data_list <- dummy_data
# r_data_JSON <- tempfile(fileext = ".json")
# cmdstanr::write_stan_json(r_data_list, r_data_JSON)
# 
# 
# Sys.setenv(STAN_THREADS="true")
# model <- StanModel$new("dummy_stan_model.stan", r_data_JSON, 1234)
# print(paste0("This model's name is ", model$name(), "."))
# print(paste0("This model has ", model$param_num(), " parameters."))
#  
# 
# res <- model$log_density_gradient(1, jacobian = TRUE)
# 
# 
# 
# 
# # now call the function 
# run <- parallel::mcparallel(run_model_from_R(model_so_file,   r_data_JSON,    theta_vec))
# res <- parallel::mccollect(run)
# res
# 
# tic()
# run <-  (run_model_from_R(model_so_file,   r_data_JSON,    theta_vec))
# toc()
# res
# 
# run <- parallel::mcparallel( load_and_run_log_prob_grad_all_Stan(so_file = model_so_file,
#                                                                  param_names = list("hello"),
#                                                                  param_dims = list(list(1)),
#                                     params = theta_vec,
#                                     data_from_r_json = r_data_JSON,
#                                     use_json = TRUE,
#                                     random_seed = 123))
# res <- parallel::mccollect(run)
# res


### Sys.setenv(LD_LIBRARY_PATH = "/home/enzocerullo/bridgestan/src/")
# corpcor(Sigma_d)
# corpcor::cor2pcor(Sigma_d)





###  -------------  Stan - run models -------------------------------------------------------------------------------------------------------------------




sims_start <- 1
sims_end <-   1



for (df_i in sims_start:sims_end) {
  
  
  
  
  
  {
    
    
    gc(reset = TRUE) 
    
    seed <- df_i
    
    
    # run using NUTS-HMC
    metric_type <- "diag_e"    #  --------------- 
    #  metric_type <- "unit_e"
    
    #   n_chains <-  64  ;  iter_warmup <- 250  ;        iter_sampling <-   800 
    #  n_chains <-  8 ; iter_warmup <- 1000  ;       iter_sampling <-   500 
    
    
    
    n_chains <-  32 ;  iter_sampling <-   500 # for pilot study / tuning / testing
    
    if (N == 5000) {
      n_chains <-  16 ;  iter_sampling <-   250
    }
    
    
    ####    iter_warmup <- 1000
    ###     iter_warmup <- 500
    ### iter_warmup <- 250
    ###     iter_warmup <- 125
    
    
    
    #    iter_warmup <- 1000  # already done pilot runs for this  
    iter_warmup <- 525
    #    iter_warmup <- 275
    #    iter_warmup <- 200 ###
    #    iter_warmup <- 150
    
    
    adapt_delta <- 0.80
    
    max_treedepth <- 10   #     max_treedepth <- 9
    
    if (!(n_chains %in% c(3, 6, 10, 16, 24, 32, 50, 80)))  {
      
          # if (parallel::detectCores() < 17) {  # Laptop (8 cores / 8 chains) 
          #   if (N == 500)   {  iter_warmup = 275    ; iter_sampling = 500  }   # target ESS ~ 1000
          #   if (N == 1000)  {  iter_warmup = 1000   ; iter_sampling = 500  }  # target ESS ~ 1000
          #   if (N == 2500)  {  iter_warmup = 200    ; iter_sampling = 500  } # should be 5450 but time-limited  ; 1090 =  (5450 / 5)  - so target ESS ~ 200
          #   if (N == 5000)  {  iter_warmup = 525    ; iter_sampling = 500  }  # should be 5714 but time-limited  ; 286 =  (5714 / 20)  - so target ESS ~ 50
          # } else { # HPC (96 cores / 64 chains)
          #   if (N == 500)   {  iter_warmup = 200  ; iter_sampling = 500  }  # target ESS ~ 1000
          #   if (N == 1000)  {  iter_warmup = 275  ; iter_sampling = 500  } # target ESS ~ 1000
          #   if (N == 2500)  {  iter_warmup = 200  ; iter_sampling = 500  } # target ESS ~ 1000
          #   if (N == 5000)  {  iter_warmup = 525  ; iter_sampling = 500  }  # should be 714 but time-limited  ; 143 =  (714 / 5)  - so target ESS ~ 200
          # }
          
          max_treedepth <- 10
          adapt_delta <- 0.80
          metric_type <- "diag_e"
          
    }
    
    
    
    if (prior_only == 1)  { 
      iter_warmup = 1000
      iter_sampling = 16000
    }
    
    
    tictoc::tic("timer")
    
    stan_data_list[[df_i]]$n_tests <- n_tests
    #  stan_data_list[[df_i]]$n_tests <- 5
    
    
    
    

    
    
    
    model <- mod$sample(
      # data = stan_data_list[[df_i]],
      data = stan_data_list[[123]],
      seed = seed,
      chains = n_chains,
      parallel_chains = n_chains,
      iter_warmup = iter_warmup,
      iter_sampling = iter_sampling, 
      refresh = round( ((iter_warmup + iter_sampling)/10), 0),
      init = rep(list(init), n_chains), 
      save_warmup = 1, # for some efficiency stats (can turn off for sim. study)
      metric = metric_type,
      adapt_delta = adapt_delta, 
      max_treedepth = max_treedepth)
    
    
    gc(reset = TRUE)
    

    
    {
      
      print(prior_lkj)
      
      
      # view sampler diagnostics (no need to do this as cmdstanr flags divergences + iterations which exceed max_treedepth, and adds additional comp. time)
      #model$cmdstan_diagnose()
      
      
      
      
      
      if (prior_only == 0) {
        
        if (model_type == "LC_MVP") { 
          
          try({
            cmdstanr_model_out <- model$summary(variables = c( "p_raw", "beta" , "Omega"), "mean", "median", "sd", "mad",  ~quantile(.x, probs = c(0.025,  0.975) ), "rhat" , "ess_bulk", "ess_tail")
            print(cmdstanr_model_out, n = 100)
            Min_ESS <- round(min(cmdstanr_model_out$ess_bulk, na.rm=TRUE), 0)
            print(paste("min ESS = ", Min_ESS))
          })
          try({
            cmdstanr_model_out <- model$summary(variables = c( "p", "Se_bin", "Sp_bin"  ), "mean", "median", "sd", "mad",  ~quantile(.x, probs = c(0.025,  0.975) ), "rhat" , "ess_bulk", "ess_tail")
            print(cmdstanr_model_out, n = 100)
          })
          
        } else if (model_type == "LT") { 
          
          try({
            cmdstanr_model_out <- model$summary(variables = c( "p", "Se_mean", "Sp_mean" , "Se_median", "Sp_median"  ), "mean", "median", "sd", "mad",  ~quantile(.x, probs = c(0.025,  0.975) ), "rhat" , "ess_bulk", "ess_tail")
            print(cmdstanr_model_out, n = 100)
          })
          try({
            cmdstanr_model_out <- model$summary(variables = c("p_raw",  "LT_a", "LT_b"), "mean", "median", "sd", "mad",  ~quantile(.x, probs = c(0.025,  0.975) ), "rhat" , "ess_bulk", "ess_tail")
            print(cmdstanr_model_out, n = 100)
            Min_ESS <- round(min(cmdstanr_model_out$ess_bulk, na.rm=TRUE), 0)
            print(paste("min ESS = ", Min_ESS))
          })
          
        }
        
      } else { 
        
        # try({
        #   cmdstanr_model_out <- model$summary(variables = c( "beta_alpha_1", "beta_alpha_2"), "mean", "median", "sd", "mad",  ~quantile(.x, probs = c(0.025,  0.975) ), "rhat" , "ess_bulk", "ess_tail")
        #   print(cmdstanr_model_out, n = 100)
        #   Min_ESS <- round(min(cmdstanr_model_out$ess_bulk, na.rm=TRUE), 2)
        #   print(paste("min ESS = ", Min_ESS))
        # })
        
        try({
          cmdstanr_model_out <- model$summary(variables = c(  "Omega"), "mean", "median", "sd", "mad",  ~quantile(.x, probs = c(0.025,  0.975) ), "rhat" , "ess_bulk", "ess_tail")
          print(cmdstanr_model_out, n = 100)
          Min_ESS <- round(min(cmdstanr_model_out$ess_bulk, na.rm=TRUE), 0)
          print(paste("min ESS = ", Min_ESS))
        })
        
      }
      
      
      
      try({
        cmdstanr_model_out <- model$summary(variables = c(  "n_mild_overflow_pct", "n_severe_overflow_pct", "n_underflow_pct"), "mean", "median", "sd", "mad",  ~quantile(.x, probs = c(0.025,  0.975) ), "rhat" , "ess_bulk", "ess_tail")
        print(cmdstanr_model_out, n = 100)
      })
      
      
      
      
      
      
      
      try({
        
        # Max R-hat / converged?
        cmdstanr_model_out_rhat <- cmdstanr_model_out$rhat[!(is.na(cmdstanr_model_out$rhat))]
        print(round(max(cmdstanr_model_out_rhat), 3)) ; print(any(cmdstanr_model_out_rhat > 1.010)) # if TRUE, not converged (at least 1 r-hat > 1.01)
        
        print(tictoc::toc(log = TRUE))
        log.txt <- tictoc::tic.log(format = TRUE)
        tictoc::tic.clearlog()
        time_stan_total_inc_csv <- unlist(log.txt)
        
        time_stan_total_real_world <- as.numeric(substr(time_stan_total_inc_csv,  8,  14))
        
        # run time (total)
        cmdstanr_model_out_timers <- model$time()
        total_time_seconds <- time_stan_total_real_world #  cmdstanr_model_out_timers$total
        total_time_mins <- total_time_seconds / 60
        total_time_hours <- total_time_mins / 60
        
        print(paste("seed = ", seed))
        
        print(paste("total time =", round(total_time_seconds, 0), "seconds")) # in seconds
        print(paste("total time =", floor(total_time_mins), "minutes and ", round(((total_time_mins - floor(total_time_mins))*60), 0), "seconds"))    # in minutes
        print(round(total_time_mins, 3 ))
        print(paste("total time =", floor(total_time_hours), "hours and ", round(((total_time_hours - floor(total_time_hours))*60), 0), "minutes"))    # in hours
        
        # run time (post-burnin / sampling ONLY)
        pb_time_seconds <- max(cmdstanr_model_out_timers$chains$sampling)
        pb_time_mins <- pb_time_seconds / 60
        pb_time_hours <- pb_time_mins / 60
        
        print(paste("Sampling (post-burnin) time =", round(pb_time_seconds, 0), "seconds")) # in seconds
        print(paste("Sampling (post-burnin) time =", floor(pb_time_mins), "minutes and ", round(((pb_time_mins - floor(pb_time_mins))*60), 0), "seconds"))    # in minutes
        print(round(pb_time_mins, 3 ))
        print(paste("Sampling (post-burnin) time =", floor(pb_time_hours), "hours and ", round(((pb_time_hours - floor(pb_time_hours))*60), 0), "minutes"))    # in hours
        
        # Min ESS / sec (using total time)
        Min_ESS_per_sec_total_time <- Min_ESS / total_time_seconds
        print(paste("Min ESS / sec (total time) = ", signif(Min_ESS_per_sec_total_time, 5)))
        
        # Min ESS / sec (using post-burnin / sampling time)
        Min_ESS_per_sec_pb_time <- Min_ESS / pb_time_seconds
        print(paste("Min ESS / sec (sampling time only) = ", signif(Min_ESS_per_sec_pb_time, 5)))
        
        # # Min ESS / grad (uses post-burnin / sampling time ONLY)
        mean_L  <-   (mean(c(2^model$sampler_diagnostics(inc_warmup = TRUE)[1:iter_warmup,,1] - 1)))  # mean L (total)
        print(paste("Mean L across chains (burnin)  = ", mean_L))
        
        mean_L_post_burnin  <-   (mean(c(2^model$sampler_diagnostics()[,,1] - 1))) # mean # of Leapfrog steps (sampling only)
        print(paste("Mean L across chains (post-burnin) = ", mean_L_post_burnin))
        
        
        
        L_burn_means_per_chain <-  c()
        L_samp_means_per_chain <-  c()
        for (i in 1:n_chains) {
          L_burn_means_per_chain[i]    <-  mean ( (  2^model$sampler_diagnostics(inc_warmup = TRUE)[1:iter_warmup,,1] - 1  )[ ,i,1]) 
          L_samp_means_per_chain[i]    <-  mean ( (  2^model$sampler_diagnostics()[,,1] - 1  )[1:iter_sampling,i,1]) 
        }
        
        max_of_mean_Ls_per_chain_burn <-      max(L_burn_means_per_chain)
        max_of_mean_Ls_per_chain_samp <-      max(L_samp_means_per_chain)
        
        
        print(paste("Max L across chains (burnin)  = ", max_of_mean_Ls_per_chain_burn))
        print(paste("Max L across chains (post-burnin) = ", max_of_mean_Ls_per_chain_samp))
        
        
        mean_eps_post_burnin  <-   (mean(c(model$sampler_diagnostics()[,,5]))) # mean # of Leapfrog steps (sampling only)
        print(paste("Mean step-size across chains (post-burnin) = ", signif(mean_eps_post_burnin, 4)))
        
        # total # of gradient evals
        stan_total_gradient_evals <-   (mean_L*n_chains*(iter_sampling + iter_warmup))   #  n_chains * iter_sampling * stan_n_leapfrogs ; stan_total_gradient_evals
        stan_total_gradient_evals_pb <-   (mean_L_post_burnin*n_chains*iter_sampling)
        
        stan_min_ess_per_grad_eval <- Min_ESS / stan_total_gradient_evals ; stan_min_ess_per_grad_eval
        stan_min_ess_per_grad_eval_pb <- Min_ESS / stan_total_gradient_evals_pb ; stan_min_ess_per_grad_eval
        
        print(paste("ESS/grad (total) = ", signif(1000 * stan_min_ess_per_grad_eval, 3)))
        print(paste("ESS/grad (sampling) = ", signif(1000 * stan_min_ess_per_grad_eval_pb, 3)))
        print(paste("grad/sec (sampling) = ",  signif(Min_ESS_per_sec_pb_time  / (1000 * stan_min_ess_per_grad_eval_pb), 3)  ))
        
        total_divs <- sum(model$sampler_diagnostics()[,,2])
        
        print(paste("total_divs = ", total_divs))
        
      #  print(  round(  ( n_chains * ( mean_L * iter_warmup + ( iter_sampling * (100 / (min_ess / 1    )  )  * mean_L_post_burnin  ) ) ) / 1000   , 0) )
      #  print(  round(  ( n_chains * ( mean_L * iter_warmup + ( iter_sampling * (1000 / (min_ess / 1   )  )  * mean_L_post_burnin  ) ) ) / 1000   , 0) )
        
        
        stan_draws_array <- model$draws()
        
        
        num_elements_Omegas <- n_class * n_tests * n_tests
        
        #  Omega_index_start_for_Sean_param <- n_class*1*n_tests + N*n_tests + 1 + n_class*(n_tests-1) + n_class * choose(n_tests-1, 2) + N  + 2
        #  Omega_index_end_for_Sean_param <- n_class*1*n_tests + N*n_tests + 1 + n_class*(n_tests-1) + n_class * choose(n_tests-1, 2) + N  + 1 + num_elements_Omegas
        
        
        if (model_type == "LT") { 
          as_index <- 2:(n_tests * n_class + 1)
          bs_index <-  seq(from =  (tail(as_index, 1) + 1) ,  by = 1, length = (length(as_index)) )  #   (n_tests * n_class + 2):(n_tests * n_class + 2 + n_class * choose(n_tests, 2) - 1) #  Omega_index_start_for_Sean_param:Omega_index_end_for_Sean_param
          prev_index <-  tail(bs_index, 1) + 1    #   n_class*1*n_tests + N*n_tests + 2 
          
          
          param_index  <- c(coeff_index,  bs_index,  prev_index)
        } else { 
          Omega_index <-  (n_tests * n_class + 2):(n_tests * n_class + 2 + n_class * choose(n_tests, 2) - 1) #  Omega_index_start_for_Sean_param:Omega_index_end_for_Sean_param
          prev_index <-  tail(Omega_index, 1) + 1    #   n_class*1*n_tests + N*n_tests + 2 
          coeff_index <- 2:(n_tests * n_class + 1)
          
          param_index  <- c(coeff_index,  Omega_index,  prev_index)
        }
        
        
        
        superchain_ids = seq(from = 1, to = n_chains, by = 1)
        if (n_chains > 4)  superchain_ids = c(rep(1, n_chains/2), rep(2, n_chains/2))
        if (n_chains > 15)  superchain_ids = c(rep(1, n_chains/4), rep(2, n_chains/4), rep(3, n_chains/4), rep(4, n_chains/4))
        if (n_chains > 47)  superchain_ids = c(rep(1, n_chains/8), rep(2, n_chains/8), rep(3, n_chains/8), rep(4, n_chains/8), 
                                               rep(5, n_chains/8), rep(6, n_chains/8), rep(7, n_chains/8), rep(8, n_chains/8))
        
        stan_draws_array[,, 31]
        
        
        rhats_nested <-  rhats <- c()
        for (i in 1:length(param_index)) {
          rhats_nested[i] <-   posterior::rhat_nested( array(c(stan_draws_array[,,param_index[i]]), dim = c(iter_sampling, n_chains)) , superchain_ids =superchain_ids )
          rhats[i] <-   posterior::rhat( array(c(stan_draws_array[,,param_index[i]]), dim = c(iter_sampling, n_chains)) )
        }
        
        
        {
          i = 13
          plot(posterior::as_draws ( array(c(stan_draws_array[,,param_index[i]]), dim = c(iter_sampling, n_chains))  )[,1])
          for (iii in 1:n_chains) {
            lines(posterior::as_draws ( array(c(stan_draws_array[,,param_index[i]]), dim = c(iter_sampling, n_chains))  )[,iii], col = iii)
          }
        }
        
        print(round(max(rhats_nested, na.rm = TRUE), 3))
        print(round(max(rhats, na.rm = TRUE), 3))
        
        
        # save efficiency summary info only
        file_name <- paste0("efficiency_info_", "seed_", seed, "_",
                            "Mod_", model_type,   
                            "PO_", prior_only,   "_",  
                            prior_lkj[1],"and",  prior_lkj[2],  "priorLKJ_", 
                            prior_param_3, "PP3_",
                            corr_force_positive, "posCOR_",
                            corr_prior_beta, "skewBpi_", 
                            N, "N_", 
                            n_chains, "chains_", 
                            iter_warmup, "burn_",  
                            iter_sampling, "samp_", 
                            adapt_delta, "ad_", 
                            max_treedepth, "maxtree_",
                            metric_type, "Mtype",".RDS")
        
        file_list <- list(
          time_stan_total_real_world, cmdstanr_model_out_timers,
          total_time_seconds,  total_time_mins, total_time_hours,
          pb_time_seconds, pb_time_mins, pb_time_hours,
          Min_ESS_per_sec_total_time,
          Min_ESS_per_sec_pb_time,# 10
          paste("total time =", round(total_time_seconds, 0), "seconds"),
          paste("total time =", floor(total_time_mins), "minutes and ", round(((total_time_mins - floor(total_time_mins))*60), 0), "seconds"),
          paste("total time =", floor(total_time_hours), "hours and ", round(((total_time_hours - floor(total_time_hours))*60), 0), "minutes"),
          paste("Sampling (post-burnin) time =", round(pb_time_seconds, 0), "seconds"),
          paste("Sampling (post-burnin) time =", floor(pb_time_mins), "minutes and ", round(((pb_time_mins - floor(pb_time_mins))*60), 0), "seconds"),
          paste("Sampling (post-burnin) time =", floor(pb_time_hours), "hours and ", round(((pb_time_hours - floor(pb_time_hours))*60), 0), "minutes"),
          paste("Min ESS / sec (total time) = ", round(Min_ESS_per_sec_total_time, 5)),
          paste("Min ESS / sec (sampling time only) = ", round(Min_ESS_per_sec_pb_time, 5)),
          mean_L_post_burnin,
          mean_L, # 20
          stan_min_ess_per_grad_eval * 1000,
          stan_min_ess_per_grad_eval_pb * 1000 ,
          round(max(rhats_nested, na.rm = TRUE), 4), # 23
          round(max(rhats, na.rm = TRUE), 4),  # 24
          round(max(cmdstanr_model_out_rhat), 4),
          Min_ESS,
          mean_eps_post_burnin, 
          (paste("Max L across chains (burnin)  = ", max_of_mean_Ls_per_chain_burn)),
          (paste("Max L across chains (post-burnin) = ", max_of_mean_Ls_per_chain_samp)) , # 29
          (paste("total_divs = ", total_divs))
        )
        
        saveRDS(file_list, file = file_name)
        
        
        # # save partial cmdstanr output - table for main params only
        file_name <- paste0("Summary_param_table_main_", "seed_", seed, "_",   "Mod_", model_type,   "PO_", prior_only,    "_", prior_lkj[1],"and",  prior_lkj[2],  "priorLKJ_",  prior_param_3, "PP3_",  corr_force_positive, "posCOR_", corr_prior_beta, "skewBpi_",    N, "N_", n_chains, "chains_", iter_warmup, "burn_",  iter_sampling, "samp_",
                            adapt_delta, "ad_", max_treedepth, "maxtree_", metric_type, "Mtype")
        saveRDS(cmdstanr_model_out, file = file_name)
        
        
        
        
        # save full cmdstanr output (files may be massive!!!)
        file_name <- paste0("seed_", seed, "_", "Mod_", model_type,  "PO_", prior_only,  "_",  prior_lkj[1], "and",  prior_lkj[2],  "priorLKJ_", prior_param_3, "PP3_",   corr_force_positive, "posCOR_", corr_prior_beta, "skewBpi_",   N, "N_", n_chains, "chains_", iter_warmup, "burn_",  iter_sampling, "samp_",
                            adapt_delta, "ad_", max_treedepth, "maxtree_", metric_type, "Mtype")
        model$save_object(file = file_name)
        
        
        
        
        
        # try({
        # 
        # 
        # {
        # 
        # L_burn_vec <- c()
        # L_samp_vec <- c()
        # R_hat_vec <- c()
        # R_hat_nested_vec <- c()
        # ESS_vec <- c()
        # Time_burn_vec <- c()
        # Time_samp_vec <- c()
        # Samp_ESS_per_sec_vec <- c()
        # Samp_ESS_per_grad_vec <- c()
        # N_grad_evals_relative_for_target_ESS_8_chains <- c()
        # N_grad_evals_relative_for_target_ESS_64_chains <- c()
        # N_iter_for_target_ESS_8_chains <- c()
        # 
        #     n_burnin <- 1000
        # #   n_burnin <- 525
        #  #   n_burnin <- 275
        #   #     n_burnin <- 200
        # 
        #   n_sampling <- 500
        # 
        #   N <- 5000
        # 
        # for (seed_i in 1:5)  {
        # 
        #          ###  seed_1_Mod_MVPPO_0_10and2priorLKJ_0PP3_0posCOR_0skewBpi_5000N_32chains_525burn_500samp_0.8ad_10maxtree_diag_eMtype
        # 
        #           file_name_0 <-   paste0("seed_",
        #                                 seed_i,
        #                                 "_Mod_MVPPO_0_10and2priorLKJ_0PP3_0posCOR_0skewBpi_",
        #                                 N,
        #                                 "N_32chains_",
        #                                 n_burnin,
        #                                 "burn_500samp_0.8ad_10maxtree_diag_eMtype")
        # 
        #           model <-  readRDS(file_name_0)
        # 
        # 
        # 
        #           # # # Min ESS / grad (uses post-burnin / sampling time ONLY)
        #           # mean_L  <-   (mean(c(    2^model$sampler_diagnostics(inc_warmup = TRUE)[1:n_burnin,,1] - 1    )))  # mean L (total)
        #           # print(paste("Mean L across chains (burnin)  = ", mean_L))
        #           #
        #           # mean_L_post_burnin  <-   (max(c(  2^model$sampler_diagnostics()[,,1] - 1  ))) # mean # of Leapfrog steps (sampling only)
        #           # print(paste("Mean L across chains (post-burnin) = ", mean_L_post_burnin))
        # 
        #           L_samp_means_per_chain <-  c()
        #           L_burn_means_per_chain <-  c()
        #           for (i in 1:32) {
        #             L_burn_means_per_chain[i]    <-  mean ( (  2^model$sampler_diagnostics(inc_warmup = TRUE)[1:n_burnin,,1] - 1  )[ ,i,1])
        #             L_samp_means_per_chain[i]    <-  mean ( (  2^model$sampler_diagnostics()[,,1] - 1  )[1:n_sampling,i,1])
        #           }
        #           #
        # 
        #           max_of_mean_Ls_per_chain_burn <-      max(L_burn_means_per_chain)
        #           max_of_mean_Ls_per_chain_samp <-      max(L_samp_means_per_chain)
        # 
        # 
        # 
        #           file_name <-   paste0("efficiency_info_seed_",
        #                                 seed_i,
        #                                 "_Mod_MVPPO_0_10and2priorLKJ_0PP3_0posCOR_0skewBpi_",
        #                                 N,
        #                                 "N_32chains_",
        #                                 n_burnin,
        #                                 "burn_500samp_0.8ad_10maxtree_diag_eMtype.RDS")
        # 
        #              model <-  readRDS(file_name)
        #             {
        #               # print(round(model[[3]], 0))
        #               # print(model[[14]])
        #               # print(model[[17]])
        #               print(paste("| ---------------------------------------------- |"))
        #               L_burn_vec[seed_i] <- print(round(max_of_mean_Ls_per_chain_burn, 0))
        #               L_samp_vec[seed_i] <- print(round(max_of_mean_Ls_per_chain_samp, 0))
        #               R_hat_vec[seed_i] <- print(round(model[[24]], 3) )
        #               R_hat_nested_vec[seed_i] <- print(round(model[[23]], 3) )
        #               print(paste("ESS = ", round(model[[26]], 0)) )
        #               ESS_vec[seed_i] <-  round(model[[26]], 0)
        #               print(paste("burnin time = ", signif( (model[[3]]  -  model[[6]])/60, 3)) )
        #               Time_burn_vec[seed_i] <- signif( (model[[3]]  -  model[[6]])/60, 3 )
        #               print(paste("samp. time = ", signif( model[[6]] / 60, 3)) )
        #               Time_samp_vec[seed_i] <-   signif( model[[6]] / 60, 3 )
        #               print(model[[18]])
        #               Samp_ESS_per_sec_vec[seed_i] <- signif((model[[10]]), 4)
        #               Samp_ESS_per_grad_vec[seed_i] <- print(signif(model[[22]], 3) )
        #               if (N == 500)  target_ESS = 5000
        #               if (N == 1000) target_ESS = 5000
        #               if (N == 2500) target_ESS = 2500
        #               if (N == 5000) target_ESS = 2000
        #               N_grad_evals_relative_for_target_ESS_8_chains[seed_i]  <- ( n_burnin   *  L_burn_vec[seed_i]    +
        #                                                                           n_sampling * L_samp_vec[seed_i] * (target_ESS /  ESS_vec[seed_i]) * (32 / 8)  ) / 1000
        #               N_grad_evals_relative_for_target_ESS_64_chains[seed_i] <- ( n_burnin   *  L_burn_vec[seed_i]    +
        #                                                                           n_sampling * L_samp_vec[seed_i] * (target_ESS /  ESS_vec[seed_i]) * (32 / 64) ) / 1000
        # 
        #               print(signif(     N_grad_evals_relative_for_target_ESS_8_chains[seed_i], 4) )
        #               print(signif(     N_grad_evals_relative_for_target_ESS_64_chains[seed_i], 4) )
        # 
        #               N_iter_for_target_ESS_8_chains[seed_i] <- print(   (32/8) * ceiling(n_sampling * ( target_ESS / ESS_vec[seed_i])))
        # 
        #               print(paste("| ---------------------------------------------- |"))
        #             }
        # 
        #         }
        # 
        #         {
        #         print(paste("| ------------------------------------------------------ |"))
        #         print(paste("| ------ Summary Estimates (means of the runs)  -------- |"))
        #         print(paste("| ------------------------------------------------------ |"))
        #         print(paste0("L_burn = ", round(mean(L_burn_vec), 0), ", SD = (", round(sd(L_burn_vec), 0), ")" ) )
        #         print(paste0("L_samp = ", round(mean(L_samp_vec), 0), ", SD = (", round(sd(L_samp_vec), 0), ")" ) )
        #         print(paste0("R_hat = ", round(mean(R_hat_vec), 3), ", SD = (", round(sd(R_hat_vec), 3), ")" ) )
        #         print(paste0("R_hat_nested = ", round(mean(R_hat_nested_vec), 3), ", SD = (", round(sd(R_hat_nested_vec), 3), ")" ) )
        #         print(paste0("ESS = ", round(mean(ESS_vec), 0), ", SD = (", round(sd(ESS_vec), 0), ")" ) )
        #       #  print(paste0("Time_burn = ", signif(mean(Time_burn_vec), 4), ", SD = (", signif(sd(Time_burn_vec), 4), ")" ) )
        #        # print(paste0("Time_samp = ", signif(mean(Time_samp_vec), 4), ", SD = (", signif(sd(Time_samp_vec), 4), ")" ) )
        #        # print(paste0("Samp_ESS_per_sec = ", signif(mean(Samp_ESS_per_sec_vec), 4), ", SD = (", signif(sd(Samp_ESS_per_sec_vec), 4), ")" ) )
        #         print(paste0("Samp_ESS_per_grad = ", signif(mean(Samp_ESS_per_grad_vec), 4), ", SD = (", signif(sd(Samp_ESS_per_grad_vec), 4), ")" ) )
        #         print(paste0("N_grad_evals_relative_for_target_ESS_8_chains = ", signif(mean(N_grad_evals_relative_for_target_ESS_8_chains), 4), ", SD = (", signif(sd(N_grad_evals_relative_for_target_ESS_8_chains), 4), ")" ) )
        #         print(paste0("N_grad_evals_relative_for_target_ESS_64_chains = ", signif(mean(N_grad_evals_relative_for_target_ESS_64_chains), 4), ", SD = (", signif(sd(N_grad_evals_relative_for_target_ESS_64_chains), 4), ")" ) )
        #         print(paste0("N_iter_for_target_ESS_8_chains = ", round(mean(N_iter_for_target_ESS_8_chains), 0), ", SD = (", round(sd(N_iter_for_target_ESS_8_chains), 0), ")" ) )
        # 
        #         }
        # 
        # }
        # 
        # })
        
        
        
        
        # 
        # const double x_i = -0.3418*log( 1.0/x - 1.0);
        # 
        # 
        # const double exp_x_i = exp(0.33333333333333331483 * log( x_i  +  std::sqrt(  x_i^2  + 1 ) ));
        # const double exp_2x_i = exp_x_i*exp_x_i;
        # return  2.74699999999999988631 * ( (exp_2x_i  - 1.0) / exp_x_i ) ;  //   now do sinh parth part
        # 
        # # model <-  readRDS("Mplus_efficiency_info_seed_1_10_24prior_IW_25000N_64chains_.RDS")
        # # {
        # #   print(model[[2]])
        #   print(round(model[[4]], 0))
        #   print(model[[13]])
        #   print(signif(model[[11]], 3))
        #   print(signif(model[[12]], 3))
        # }
        
        
        
        
        
        
      })
      
    }
    
  }
  
}





