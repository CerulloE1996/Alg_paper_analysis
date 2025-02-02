

# -| ------------------------------    Pilot study - Finding the optimal N_{chunks} for BayesMVP --------------------------------------------------------------------


R_fn_run_ps_opt_N_chunks <- function(    pilot_study_opt_N_chunks_list,
                                         computer,
                                         BayesMVP_model_obj,
                                         global_list,
                                         output_path) { 
  
  ## Set device
  pilot_study_opt_N_chunks_list$device <- computer
  print(paste("device = ", pilot_study_opt_N_chunks_list$device))
  ## run type:
  pilot_study_opt_N_chunks_list$run_type <- "ps1"
  print(paste("run_type = ", pilot_study_opt_N_chunks_list$run_type))
  
  ## Set important variables:
  n_thread_total_combos <- length(pilot_study_opt_N_chunks_list$n_threads_vec)
  ##
  n_max_chunk_combos <-  pilot_study_opt_N_chunks_list$n_max_chunk_combos 
  ##
  N_vec <- pilot_study_opt_N_chunks_list$N_vec
  n_runs <- pilot_study_opt_N_chunks_list$n_runs
  start_index <- pilot_study_opt_N_chunks_list$start_index
  
  ## Make array to store results:
  times_array <- array(dim = c(length(N_vec), n_max_chunk_combos, n_runs, n_thread_total_combos))
  str(times_array)
  dimnames(times_array) <- list(N = c(500, 1000, 2500, 5000, 12500, 25000),
                                n_chunks_index = seq(from = 1, to = n_max_chunk_combos, by = 1),
                                run_number = seq(from = 1, to = n_runs, by = 1),
                                n_threads_index =   pilot_study_opt_N_chunks_list$n_threads_vec) 
 
  
  ## Set key variables:
  n_class <- global_list$Model_settings_list$n_class
  n_tests <- global_list$Model_settings_list$n_tests
  n_params_main <- global_list$Model_settings_list$n_params_main
  n_corrs <-  global_list$Model_settings_list$n_corrs
  n_covariates_total <-  global_list$Model_settings_list$n_covariates_total
  n_nuisance_to_track <- 10 ## set to small number 
  ##
  pilot_study_opt_N_chunks_list$manual_gradients <- TRUE ## Using manual-gradient function !! (AD doesn't have "chunking")
  ## Fixed # of * burnin * chains:
  n_chains_burnin <- min(parallel::detectCores(), 8)
  n_burnin <- 500
  ##
  sample_nuisance <- FALSE
  partitioned_HMC <- FALSE
  diffusion_HMC <- FALSE
  Model_type <- "LC_MVP"
  force_autodiff <- force_PartialLog <- FALSE
  multi_attempts <- FALSE
  metric_shape_main <- "dense"
  ##
  
  for (df_index in start_index:length(pilot_study_opt_N_chunks_list$N_vec[!is.na(pilot_study_opt_N_chunks_list$N_vec)]))  {
    
    N <- pilot_study_opt_N_chunks_list$N_vec[df_index]
    n_nuisance <- N * n_tests
    ## Print:
    print(paste("N = ", N))
    print(paste("n_nuisance = ", n_nuisance))
    ##
    ## Get Rcpp / C++ lists:
    Model_args_as_Rcpp_List <- BayesMVP_model_obj$init_object$Model_args_as_Rcpp_List
    Model_args_as_Rcpp_List$N <- N
    Model_args_as_Rcpp_List$n_nuisance <- n_nuisance
    ##
    EHMC_args_as_Rcpp_List <- BayesMVP:::init_EHMC_args_as_Rcpp_List(diffusion_HMC = diffusion_HMC)
    ## Edit entries to ensure don't get divergences - but also ensure suitable L chosen:
    EHMC_args_as_Rcpp_List$eps_main <- 0.0001
    ## Use path length of 16:
    L_main <- 16
    EHMC_args_as_Rcpp_List$tau_main <-     L_main * EHMC_args_as_Rcpp_List$eps_main 
    ## Metric Rcpp / C++ list::
    EHMC_Metric_as_Rcpp_List <- BayesMVP:::init_EHMC_Metric_as_Rcpp_List(   n_params_main = n_params_main, 
                                                                            n_nuisance = n_nuisance, 
                                                                            metric_shape_main = metric_shape_main)  
    ## Assign SIMD_vect_type:
    Model_args_as_Rcpp_List$Model_args_strings[c("vect_type",
                                                 "vect_type_exp", "vect_type_log", "vect_type_lse", "vect_type_tanh", 
                                                 "vect_type_Phi", "vect_type_log_Phi", "vect_type_inv_Phi", 
                                                 "vect_type_inv_Phi_approx_from_logit_prob"), ] <-  pilot_study_opt_N_chunks_list$SIMD_vect_type 
    
    for (c in 1:n_class) {
      for (t in 1:n_tests) {
        Model_args_as_Rcpp_List$Model_args_2_later_vecs_of_mats_double[[1]][[c]][[t]] <- matrix(1, nrow = N, ncol = 1)
      }
    }
 
    n_chunks_vec <- pilot_study_opt_N_chunks_list$n_chunks_vecs[[as.character(N)]]
    N_iter <- pilot_study_opt_N_chunks_list$n_iter_given_N[[as.character(N)]]
    
    for (kkk in 1:length(n_chunks_vec))  {
      
      ## Set the number of chunks to use in model_args_list:
      num_chunks <- n_chunks_vec[kkk]
      
      Model_args_as_Rcpp_List$Model_args_ints[4] <- num_chunks
      ## Using manual-gradient model obj:
      BayesMVP_model_obj <- BayesMVP_LC_MVP_model_using_manual_grad_obj
      
      
      for (iii in 1:pilot_study_opt_N_chunks_list$n_runs) {
        
        seed <- iii
        times <- c()
        
        for (jj in (1:length(pilot_study_opt_N_chunks_list$n_threads_vec))) {
          
          ## Set timer:
          tictoc::tic("timer")
          
          ## Set number of ** sampling ** chains:
          n_threads <- pilot_study_opt_N_chunks_list$n_threads_vec[jj]
          n_chains_sampling <- n_threads
          n_superchains <- n_chains_sampling
          ## Print info:
          print(paste("n_threads = ", n_threads))
          print(paste("n_chains_sampling = ", n_chains_sampling))
          
          print(paste("n_params_main = ", n_params_main))
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
          str(Model_args_as_Rcpp_List$Model_args_2_later_vecs_of_mats_double)
          ##
          ### Call C++ parallel sampling function * directly * (skip costly burn-in phase + out of scope for this paper)
          RcppParallel::setThreadOptions(numThreads = n_chains_sampling);
          ##
          result <- BayesMVP:::Rcpp_fn_RcppParallel_EHMC_sampling(    n_threads_R = n_chains_sampling,
                                                                      sample_nuisance_R = sample_nuisance,
                                                                      n_nuisance_to_track = n_nuisance_to_track,
                                                                      seed_R = seed,
                                                                      iter_one_by_one = FALSE,
                                                                      n_iter_R = n_iter,
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
          
          
          
          print(tictoc::toc(log = TRUE))
          log.txt <- tictoc::tic.log(format = TRUE)
          tictoc::tic.clearlog()
          timer <- unlist(log.txt)
          time <- as.numeric( substr(start = 0, stop = 100,  strsplit(  strsplit( timer, "[:]")[[1]] , "[s]")[[2]][1] ) )
          
          times[jj] <- time
          
        } ## // end of jj (# N_threads) loop
        
        print(paste("N = ", N))
        comment(print(iii))
        times_array[df_index, kkk, iii, ]  <- print(round(dput(times), 3))
        
      } ## // end of iii (# run) loop
      
      {
        print(paste("N = ", N))
        print(paste("num_chunks = ", num_chunks))
        print(paste("n_threads = ", n_threads))
        print( round((times_array[df_index, kkk,,]), 2))
        #  print( round(colMeans(times_array), 2))
        print(signif(times/n_threads, 3))
      } 
      
      
    }
    
    
    
    {
      # pilot_study_opt_N_chunks_list$output_path <- file.path(  getwd(), 
      #                                                         "1_appendix_pilot_studies",
      #                                                         "ps_1_optimizing_N_chunks",
      #                                                         "outputs")
      file_name <- paste0("determining_optimal_N_chunks_ps", "_",
                          "N", N, "_",
                          "n_runs", pilot_study_opt_N_chunks_list$n_runs)
      if (pilot_study_opt_N_chunks_list$device == "Laptop") { 
        file_name <- paste0("Laptop_", file_name)
      } else { 
        file_name <- paste0("HPC_",    file_name)
      }
      
      file_path <- file.path(output_path, file_name)
      
      saveRDS(object = times_array[df_index,,,], file = file_path)
    }
    
    try({  
      beepr::beep("random")
    })
    
    
  }
  
  
  
  
}






















