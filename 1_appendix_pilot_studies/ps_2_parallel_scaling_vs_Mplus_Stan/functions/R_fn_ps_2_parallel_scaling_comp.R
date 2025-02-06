

 
# df_index <- 1
# kkk <- 1 
# iii <- 1
# algorithm <- "MD_BayesMVP"

# - | --------- Pilot study - Parallel scalability for: ----------------------------------------------------------------------------------------
## (1) BayesMVP (for various chunks); 
## (2) Mplus ; and 
## (3) Stan (using .stan model file run via BayesMVP R package)


R_fn_RUN_ps_parallel_scaling_comp <- function(     pilot_study_parallel_scaling_comp_list,
                                                   computer,
                                                   algorithm,
                                                   BayesMVP_manual_gradient_model_obj,
                                                   BayesMVP_using_Stan_file_model_obj,
                                                   global_list,
                                                   Stan_data_list,
                                                   Mplus_settings_list,
                                                   output_path) { 
 
    ## Extract list from ps_1 (as lot of overlap with ps_2):
    pilot_study_opt_N_chunks_list <- pilot_study_parallel_scaling_comp_list$pilot_study_opt_N_chunks_list
    ##
    ## Set device
    pilot_study_parallel_scaling_comp_list$device <- computer
    print(paste("device = ", pilot_study_parallel_scaling_comp_list$device))
    ## run type:
    pilot_study_parallel_scaling_comp_list$run_type <- "ps2"
    print(paste("run_type = ", pilot_study_parallel_scaling_comp_list$run_type))
    
    ## Set important variables:
    n_thread_total_combos <- length(pilot_study_opt_N_chunks_list$n_threads_vec)
    ##
    n_max_chunk_combos <-  2 ## pilot_study_parallel_scaling_comp_list$n_max_chunk_combos 
    ##
    N_vec <- pilot_study_parallel_scaling_comp_list$N_vec
    n_runs <- pilot_study_parallel_scaling_comp_list$n_runs
    start_index <- pilot_study_parallel_scaling_comp_list$start_index
    
    ## Make array to store results:
    times_array <- array(dim = c(length(N_vec), n_max_chunk_combos, n_runs, n_thread_total_combos))
    str(times_array)
    dimnames(times_array) <- list(N = N_vec,
                                  n_chunks_index = seq(from = 1, to = n_max_chunk_combos, by = 1),
                                  run_number = seq(from = 1, to = n_runs, by = 1),
                                  n_threads_index =   pilot_study_opt_N_chunks_list$n_threads_vec) 
    str(times_array)
    ##
    ## Set key variables:
    n_class <- global_list$Model_settings_list$n_class
    n_tests <- global_list$Model_settings_list$n_tests
    n_params_main <- global_list$Model_settings_list$n_params_main
    n_corrs <-  global_list$Model_settings_list$n_corrs
    n_covariates_total <-  global_list$Model_settings_list$n_covariates_total
    n_nuisance_to_track <- 10 ## set to small number 
    ##
    pilot_study_parallel_scaling_comp_list$manual_gradients <- TRUE ## Using manual-gradient function !! (AD doesn't have "chunking")
    ## Fixed # of * burnin * chains:
    n_chains_burnin <- min(parallel::detectCores(), 8)
    n_burnin <- 500
    ##
    sample_nuisance <- TRUE
    partitioned_HMC <- FALSE
    diffusion_HMC <- FALSE
    ## Model_type <- "LC_MVP"
    force_autodiff <- force_PartialLog <- FALSE
    multi_attempts <- FALSE
    metric_shape_main <- "dense"
    ##
 
    ## Use "baseline" path length of 16 (with manual-gradient LC_MVP BayesMVP model as the baseline):
    L_main <- 16
    
    if (algorithm == "MD_BayesMVP")  { 
      
          Mplus_ind <- FALSE
          Model_type <- "LC_MVP"
          BayesMVP_model_obj <- BayesMVP_manual_gradient_model_obj
          L_main  <- L_main ## leave as it is for BayesMVP manual-gradient model
      
    } else if (algorithm == "AD_BayesMVP_Stan")  { 
      
          Mplus_ind <- FALSE
          Model_type <- "Stan"
          BayesMVP_model_obj <- BayesMVP_using_Stan_file_model_obj
          L_main <- 4 ## lower for Stan model as it takes much longer (due to autodiff)
      
    } else {  ## Mplus
      
          Mplus_ind <- TRUE
          BayesMVP_model_obj <- NA
      
    }
    
    df_index = 1
    
## -----------------------
for (df_index in start_index:length(pilot_study_parallel_scaling_comp_list$N_vec[!is.na(pilot_study_parallel_scaling_comp_list$N_vec)]))  {
  
      {
        
          N <- pilot_study_parallel_scaling_comp_list$N_vec[df_index]
          n_nuisance <- N * n_tests
          ## Print:
          print(paste("N = ", N))
          print(paste("n_nuisance = ", n_nuisance))
          ##
        
            
          if (Mplus_ind == FALSE) {
                  
                  ## Get Rcpp / C++ lists:
                  Model_args_as_Rcpp_List <- BayesMVP_model_obj$init_object$Model_args_as_Rcpp_List
                  Model_args_as_Rcpp_List$N <- N
                  Model_args_as_Rcpp_List$n_nuisance <- n_nuisance
                  ##
                  EHMC_args_as_Rcpp_List <- BayesMVP:::init_EHMC_args_as_Rcpp_List(diffusion_HMC = diffusion_HMC)
                  ## Edit entries to ensure don't get divergences - but also ensure suitable L chosen:
                  EHMC_args_as_Rcpp_List$eps_main <- 0.0001
                  EHMC_args_as_Rcpp_List$tau_main <-     L_main * EHMC_args_as_Rcpp_List$eps_main 
                  ## Metric Rcpp / C++ list::
                  EHMC_Metric_as_Rcpp_List <- BayesMVP:::init_EHMC_Metric_as_Rcpp_List(   n_params_main = n_params_main, 
                                                                                          n_nuisance = n_nuisance, 
                                                                                          metric_shape_main = metric_shape_main)  
                  
                  if (algorithm == "MD_BayesMVP") {
                    
                      ## Assign SIMD_vect_type:
                      Model_args_as_Rcpp_List$Model_args_strings[c("vect_type",
                                                                   "vect_type_exp", "vect_type_log", "vect_type_lse", "vect_type_tanh", 
                                                                   "vect_type_Phi", "vect_type_log_Phi", "vect_type_inv_Phi", 
                                                                   "vect_type_inv_Phi_approx_from_logit_prob"), ] <-  pilot_study_parallel_scaling_comp_list$SIMD_vect_type 
                  
                      for (c in 1:n_class) {
                        for (t in 1:n_tests) {
                          Model_args_as_Rcpp_List$Model_args_2_later_vecs_of_mats_double[[1]][[c]][[t]] <- matrix(1, nrow = N, ncol = 1)
                        }
                      }
                      
                      n_chunks_vec <- pilot_study_parallel_scaling_comp_list$n_chunks_vecs[[as.character(computer)]][[as.character(N)]]
                      
                  } else { 
                  
                     Stan_data_list_given_current_N <- Stan_data_list[[df_index]]
                     BayesMVP_model_obj$Stan_data_list <- Stan_data_list_given_current_N
                     ##
                     cmdstanr::write_stan_json(data = Stan_data_list_given_current_N, file = Model_args_as_Rcpp_List$json_file_path)
                     ##
                     print(str(Stan_data_list_given_current_N))
                     
                  }
                  
          }
          
          N_iter <- pilot_study_opt_N_chunks_list$n_iter_given_N[[as.character(N)]] ## use same N_iter as ps_1
       
          if (algorithm == "MD_BayesMVP")  { 
            n_chunks_vec <- n_chunks_vec
          } else if   (algorithm == "AD_BayesMVP_Stan")  { 
            n_chunks_vec <- c(1)
          } else {  ## Mplus
            n_chunks_vec <- c(1)
          }
     
          kkk = 1
      
      }
  
  ## -----------------------
  for (kkk in 1:length(n_chunks_vec))  {
    
    {
        ## Set the number of chunks to use in model_args_list:
        num_chunks <- n_chunks_vec[kkk]
        
        if (algorithm == "MD_BayesMVP")  { 
            Model_args_as_Rcpp_List$Model_args_ints[4] <- num_chunks
        }
 
        iii = 1
    }
    
    ## -----------------------
    for (iii in 1:pilot_study_opt_N_chunks_list$n_runs) {
      
           seed <- iii
 
           n_params <- n_params_main + n_nuisance 
           # theta_vec = rep(0.01, n_params) 

           if (computer == "Laptop") { 
             n_threads_chosen_from_ps1 <- pilot_study_parallel_scaling_comp_list$n_threads_vec_for_Laptop 
           } else { 
             n_threads_chosen_from_ps1 <- pilot_study_parallel_scaling_comp_list$n_threads_vec_for_Local_HPC
           }
     
          ##  jj <- 1 ## n_threads_chosen_from_ps1
 
          times <- c()
       ## -----------------------
       for (jj in (1:length(pilot_study_opt_N_chunks_list$n_threads_vec))) {
         
         
       ## Set timer for the iii-th run:
       tictoc::tic("timer")

       ## Set number of ** sampling ** threads / chains:
       n_threads <-  pilot_study_opt_N_chunks_list$n_threads_vec[jj]
       n_chains_sampling <- n_threads
       n_superchains <- n_chains_sampling
       ## Print info:
       if (Mplus_ind == FALSE) {
         print(paste("n_threads = ", n_threads))
         print(paste("n_chains_sampling = ", n_chains_sampling))
       }
       ##
       print(paste("n_params_main = ", n_params_main))
       
       if (Mplus_ind == FALSE) {
     
                   theta_main_vectors_all_chains_input_from_R <- matrix(0.01, ncol = n_chains_sampling, nrow = n_params_main)
                   ## Inits for main:
                   theta_main_vectors_all_chains_input_from_R[ (n_corrs + 1):(n_corrs + n_covariates_total/2) , ] <- rep(-1, n_covariates_total/2)
                   theta_main_vectors_all_chains_input_from_R[ (n_corrs + 1 + n_covariates_total/2):(n_corrs + n_covariates_total), ] <- rep(1, n_covariates_total/2)
                   theta_main_vectors_all_chains_input_from_R[ n_params_main ] =  -0.6931472  # this is equiv to starting val of p = 0.20 -  since: 0.5 * (tanh( -0.6931472) + 1)  = -0.6931472
                   ##
                   index_nuisance = 1:n_nuisance
                   index_main = (n_nuisance + 1):(n_nuisance + n_params_main)
                   ## Inits for nuisance:
                   theta_us_vectors_all_chains_input_from_R <- matrix(0.01, ncol = n_chains_sampling, nrow = n_nuisance)
                   for (kk in 1:n_chains_sampling) {
                     theta_us_vectors_all_chains_input_from_R[, kk] <-  c(global_list$initial_values_list$inits_u_list[[df_index]])
                   }
                   ## Get y:
                   y_binary_list <- global_list$data_sim_outs$y_binary_list
                   df_index <- which(pilot_study_opt_N_chunks_list$N_vec == N)
                   y <- y_binary_list[[df_index]]
                   ##
                   ## str(Model_args_as_Rcpp_List$Model_args_2_later_vecs_of_mats_double)
                   ##
                   ### Call C++ parallel sampling function * directly * (skip costly burn-in phase + out of scope for this paper)
                   RcppParallel::setThreadOptions(numThreads = n_chains_sampling);
                   # ##
                   # print(paste("theta_main_vectors_all_chains_input_from_R = "))
                   # print(str(theta_main_vectors_all_chains_input_from_R))
                   # ##
                   # print(paste("theta_us_vectors_all_chains_input_from_R = "))
                   # print(str(theta_us_vectors_all_chains_input_from_R))
                   ##
                   result <- BayesMVP:::Rcpp_fn_RcppParallel_EHMC_sampling(    n_threads_R = n_chains_sampling,
                                                                               sample_nuisance_R = sample_nuisance,
                                                                               n_nuisance_to_track = n_nuisance_to_track,
                                                                               seed_R = seed,
                                                                               iter_one_by_one = FALSE,
                                                                               n_iter_R = N_iter,
                                                                               partitioned_HMC_R = partitioned_HMC,
                                                                               Model_type_R = Model_type,
                                                                               force_autodiff_R = force_autodiff,
                                                                               force_PartialLog = force_PartialLog,
                                                                               multi_attempts_R = multi_attempts,
                                                                               theta_main_vectors_all_chains_input_from_R = theta_main_vectors_all_chains_input_from_R, # inits stored here
                                                                               theta_us_vectors_all_chains_input_from_R = theta_us_vectors_all_chains_input_from_R,  # inits stored here
                                                                               y =  y,  ## only used in C++ for manual models! (all data passed via Stan_data_list / JSON strings for .stan models!) 
                                                                               Model_args_as_Rcpp_List =  Model_args_as_Rcpp_List,
                                                                               EHMC_args_as_Rcpp_List =   EHMC_args_as_Rcpp_List,
                                                                               EHMC_Metric_as_Rcpp_List = EHMC_Metric_as_Rcpp_List)
       
       } else if (Mplus_ind == TRUE) { 
 
          
          if (algorithm == "Mplus_standard") {  ## i.e., n_threads = n_chains
            
                n_chains <- n_threads  
                n_thin <- 1 ## less thinning for this as it s generally slower than the WCP version
                
                run_model <- TRUE
            
          } else if (algorithm == "Mplus_WCP") {  ## i.e., n_threads > n_chains
      
                if (computer == "Laptop") { 
                  n_chains <- 4 
                } else { 
                  n_chains <- 8
                }
            
                ## NOTE: For WCO, we must have:  n_chains < n_threads, hence:
                ## For Laptop, only run when n_threads is at least 8 (as 8/4 = 2 but 4/4 = 1)1
                ## For Local_HPC, only run when n_threads is at least 16 (as 16/8 = 2 but 8/8 = 1)
                ## Hence:
                min_n_threads_to_run_WCP <- n_chains * 2
                
                n_thin <- 4
                
                if (n_threads > (min_n_threads_to_run_WCP - 1)) {
                  run_model <- TRUE
                } else { 
                  run_model <- FALSE
                }
                
          }
            
           # N_iter <- n_fb_iter * n_thin
           # n_fb_iter <- N_iter / n_thin
         
           ##
           n_fb_iter <- N_iter
           if (algorithm == "Mplus_standard") { 
             n_fb_iter <- 0.5 * n_fb_iter ## half the iterations as standard one is slower!!
           }
           N_total_iter <- n_thin * n_fb_iter
           ##
           print(paste("n_threads (Mplus) = ", n_threads))
           print(paste("n_chains (Mplus) = ", n_chains))
           print(paste("N_total_iter (Mplus) = ", N_total_iter))
           print(paste("n_thin (Mplus) = ", n_thin))
           print(paste("n_fb_iter (Mplus) = ", n_fb_iter))
           ##
           ##
           Mplus_settings_outs <- R_fn_run_Mplus_model_LC_MVP(      run_model = run_model,
                                                                    computer = computer,
                                                                    Mplus_settings_list = Mplus_settings_list,
                                                                    global_list = global_list,
                                                                    N_sample_size_of_dataset = N,
                                                                    run_number = iii,
                                                                    save_full_output = FALSE,
                                                                    save_output_directory =  output_path,
                                                                    compute_nested_rhat = NULL,
                                                                    MCMC_seed = seed,
                                                                    n_chains = n_chains,
                                                                    n_threads = n_threads,
                                                                    n_superchains = n_superchains,
                                                                    n_fb_iter = n_fb_iter,
                                                                    n_thin = n_thin)
 
 
        }  # ----- end of " } else if (Mplus_ind == TRUE) { "
         
         {
           print(tictoc::toc(log = TRUE))
           log.txt <- tictoc::tic.log(format = TRUE)
           tictoc::tic.clearlog()
           timer <- unlist(log.txt)
           time <- as.numeric( substr(start = 0, stop = 100,  strsplit(  strsplit( timer, "[:]")[[1]] , "[s]")[[2]][1] ) )
         }
       
         times[jj] <- time
     
      }  #  ------   end of  jj loop

           print(paste("N = ", N))
           comment(print(iii))
           str(times_array)
           times_array[df_index, kkk, iii, ]  <- print(round(dput(times), 3))
           
    } # ---- end of  " for (iii in 1:pilot_study_opt_N_chunks_list$n_runs) {" loop
        
        {
          print(paste("N = ", N))
          print(paste("num_chunks = ", num_chunks))
          print(paste("n_threads = ", n_threads))
          print( round((times_array[df_index, kkk,,]), 2))
        } 
    
  } #  -----  end of  "  for (kkk in 1:length(n_chunks_vec))  {" loop
  
      
        
        {
            file_name <- paste0("parallel_scalability_ps", "_",
                                "algorithm", algorithm, "_",
                                "N", N, "_",
                                "n_runs", pilot_study_parallel_scaling_comp_list$n_runs)
            ##
            if (pilot_study_parallel_scaling_comp_list$device == "Laptop") { 
              file_name <- paste0("Laptop_", file_name)
            } else { 
              file_name <- paste0("HPC_",    file_name)
            }
            ##
            file_path <- file.path(output_path, file_name)
            ##
            saveRDS(object = times_array[df_index,,,], file = file_path)
        }
        
        
        try({  
          beepr::beep("random")
        })
        
        
}  #  -----  end of  "df_index" loop (for N)
    
    
    
    
}




#
    
    
    
    
     



