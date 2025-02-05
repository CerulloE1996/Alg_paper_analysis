

 
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
    
    
    
    
    
# # Plot / table results  for scalability / parallisability pilot study   ------------------------------------------------------------------------------------------------------------
# 
# 
# # if (parallel::detectCores() < 17)  {  # Laptop
# #   
# #   options(mc.cores = 8*2)
# #   cores = c(2, 4, 8) #  c(4,, 8, 16)
# #   
# #   {
# #     n_chunks_vec_N_500   <- c(1, 2)  
# #     n_chunks_vec_N_1000  <- c(1, 2)
# #     n_chunks_vec_N_2500  <- c(1, 5)  
# #     n_chunks_vec_N_5000  <- c(1, 10) 
# #     n_chunks_vec_N_12500 <- c(1, 25) 
# #     n_chunks_vec_N_25000 <- c(1, 125) 
# #   }
# #   
# # } else {  # HPC
# #   
# #   options(mc.cores = 96*1)
# #   cores = c(8, 16, 32, 64)
# #   
# #   {
# #     n_chunks_vec_N_500   <- c(1, 2)
# #     n_chunks_vec_N_1000  <- c(1, 2)
# #     n_chunks_vec_N_2500  <- c(1, 5)
# #     n_chunks_vec_N_5000  <- c(1, 5)
# #     n_chunks_vec_N_12500 <- c(1, 40)
# #     n_chunks_vec_N_25000 <- c(1, 25)
# #     
# #   }
# #   
# # }
# 
# 
# 
# 
# 
# N_vec <- c(500, 1000, 2500, 5000, 12500, 25000)
# algorithm <-  "MD_BayesMVP"
# #   algorithm <-  "AD_Stan"
# #####  algorithm <-  "Mplus_standard"
# #    algorithm <-  "Mplus_WCP"
# 
# 
# {
#   cores = c(8, 16, 32, 64)
#   {
#     n_chunks_vec_N_500   <- c(1, 2)
#     n_chunks_vec_N_1000  <- c(1, 2)
#     n_chunks_vec_N_2500  <- c(1, 5)
#     n_chunks_vec_N_5000  <- c(1, 5)
#     n_chunks_vec_N_12500 <- c(1, 40)
#     n_chunks_vec_N_25000 <- c(1, 25)
#     
#   }
#   
# }
# 
# 
# 
# 
# {
#   
#   N_scalability_pilot_results_for_each_N_HPC <- list()
#   N_scalability_pilot_results_for_each_N_and_each_N_chains_HPC <- list()
#   N_scalability_pilot_results_medians_for_each_N_and_each_N_chains_HPC <- list()
#   N_scalability_pilot_results_SD_for_each_N_and_each_N_chains_HPC <- list()
#   
#   for (dataset_index in 1:length(N_vec)) {
#     
#     {
#       
#       file_name <- "parallel_scalability_pilot_study"
#       file_name <- paste0(file_name, "_N_", N_vec[dataset_index])
#       file_name <- paste0(file_name, "_algorithm_", algorithm)
#       if (sampling_option == 100) file_name <- paste0(file_name, "_test_fn_")
#       file_name <- paste0("HPC_",    file_name)
#       N_scalability_pilot_results_for_each_N_HPC[[dataset_index]] <- readRDS(file_name)
#       
#       n_chunks_combos <-          dim(   N_scalability_pilot_results_for_each_N_HPC[[dataset_index]] )[1]
#       n_runs <-                   dim(   N_scalability_pilot_results_for_each_N_HPC[[dataset_index]] )[2]
#       n_cores_combos <-           dim(   N_scalability_pilot_results_for_each_N_HPC[[dataset_index]] )[3]
#       
#       print(n_cores_combos)
#       
#       N_scalability_pilot_results_for_each_N_HPC # list of dim length(N_vec) = 6, each with dim = 3 (= N_chunks) x 5 ( = N_runs) x 7 (= cores)
#       
#       for (N_cores_index in 1:length(N_vec)) {
#         N_scalability_pilot_results_medians_for_each_N_and_each_N_chains_HPC[[dataset_index]]  <-    apply( N_scalability_pilot_results_for_each_N_HPC[[dataset_index]], c(1, 3), median, na.rm = TRUE)
#         N_scalability_pilot_results_SD_for_each_N_and_each_N_chains_HPC[[dataset_index]]  <-         apply( N_scalability_pilot_results_for_each_N_HPC[[dataset_index]], c(1, 3), sd,     na.rm = TRUE)
#       }
#       
#     }
#     
#   }
#   
#   
# }
# 
# 
# 
# 
# {
#   
#   nrow_df_HPC <-  n_cores_combos * length(N_vec) * n_chunks_combos
#   
#   N_scalability_pilot_study_df_HPC <- tibble(N = rep(NA, nrow_df_HPC),
#                                              N_num = N,
#                                              device = rep("HPC", nrow_df_HPC),
#                                              time_avg = N,
#                                              time_SD = N,
#                                              N_chunks = N, 
#                                              N_chunks_num = N, 
#                                              N_cores = N, 
#                                              N_chains = N, 
#                                              N_iter = N#, 
#                                              # iter_per_second = N, 
#                                              # iter_per_second_adj_for_N = N
#   )
#   
#   
#   ### View(N_scalability_pilot_study_df_HPC)
#   
#   
#   counter <- 1
#   
#   for (dataset_index in 1:length(N_vec)) {
#     for (N_cores_index in 1:n_cores_combos) {
#       for (i in 1:n_chunks_combos) {  # 3 different N_chunks tested
#         
#         N = N_vec[dataset_index]
#         
#         N_scalability_pilot_study_df_HPC$N[counter]   <-      as.character(N_vec[dataset_index])
#         N_scalability_pilot_study_df_HPC$N_num[counter]   <-  as.numeric(N_vec[dataset_index])
#         N_scalability_pilot_study_df_HPC$time_avg[counter]   <-    N_scalability_pilot_results_medians_for_each_N_and_each_N_chains_HPC[[dataset_index]][i, N_cores_index]
#         N_scalability_pilot_study_df_HPC$time_SD[counter]    <-    N_scalability_pilot_results_SD_for_each_N_and_each_N_chains_HPC[[dataset_index]][i, N_cores_index]
#         
#         if (N == 500)     N_chunks = n_chunks_vec_N_500[i]
#         if (N == 1000)    N_chunks = n_chunks_vec_N_1000[i]
#         if (N == 2500)    N_chunks = n_chunks_vec_N_2500[i]
#         if (N == 5000)    N_chunks = n_chunks_vec_N_5000[i]
#         if (N == 12500)   N_chunks = n_chunks_vec_N_12500[i]
#         if (N == 25000)   N_chunks = n_chunks_vec_N_25000[i]
#         
#         N_scalability_pilot_study_df_HPC$N_chunks[counter]    <-    as.character(N_chunks)
#         N_scalability_pilot_study_df_HPC$N_chunks_num[counter]    <-    N_chunks 
#         
#         N_scalability_pilot_study_df_HPC$N_cores[counter]  <- cores[N_cores_index]
#         
#         if (algorithm != "Mplus_WCP") { 
#           N_scalability_pilot_study_df_HPC$N_chains[counter] <- cores[N_cores_index]
#         } else { 
#           if (N_scalability_pilot_study_df_HPC$device[1] == "HPC")     N_scalability_pilot_study_df_HPC$N_chains[counter] <-  8
#           if (N_scalability_pilot_study_df_HPC$device[1] == "Laptop")  N_scalability_pilot_study_df_HPC$N_chains[counter] <-  4
#         }
#         
#         
#         
#         {
#           if (N == 500)     N_iter = 400
#           if (N == 1000)    N_iter = 200
#           if (N == 2500)    N_iter = 80
#           if (N == 5000)    N_iter = 40
#           if (N == 12500)   N_iter = 16
#           if (N == 25000)   N_iter = 8
#           
#           
#           
#           if (algorithm == "Mplus_WCP")   { 
#             N_iter =  30  * N_iter    
#           } else if (algorithm == "Mplus_standard") { 
#             N_iter =   5 * N_iter 
#           } else if (algorithm == "AD_Stan") {  
#             N_iter =    round(N_iter / 2, 0)
#           }
#           
#           # 
#           N_scalability_pilot_study_df_HPC$N_iter[counter] <- N_iter
#         }
#         
#         counter = counter + 1
#         
#       }
#     }
#   }
#   
#   
#   
# }
# 
# 
# 
# 
# 
# # View(N_scalability_pilot_study_df_HPC)
# 
# N_scalability_pilot_study_df_HPC <- filter(N_scalability_pilot_study_df_HPC, !(is.na(N_chunks_num)))
# 
# 
# 
# #  Laptop
# {
#   
#   
#   
#   
#   
#   {
#     
#     cores = c(2, 4, 8)
#     
#     n_chunks_vec_N_500   <- c(1, 2)  
#     n_chunks_vec_N_1000  <- c(1, 2)
#     n_chunks_vec_N_2500  <- c(1, 5)  
#     n_chunks_vec_N_5000  <- c(1, 10) 
#     n_chunks_vec_N_12500 <- c(1, 25) 
#     n_chunks_vec_N_25000 <- c(1, 125) 
#     
#     
#     
#     
#   }
#   
#   
#   
#   N_scalability_pilot_results_for_each_N_Laptop <- list()
#   N_scalability_pilot_results_for_each_N_and_each_N_chains_Laptop <- list()
#   N_scalability_pilot_results_medians_for_each_N_and_each_N_chains_Laptop <- list()
#   N_scalability_pilot_results_SD_for_each_N_and_each_N_chains_Laptop <- list()
#   
#   
#   for (dataset_index in 1:length(N_vec)) {
#     
#     {
#       
#       file_name <- "parallel_scalability_pilot_study"
#       file_name <- paste0(file_name, "_N_", N_vec[dataset_index])
#       file_name <- paste0(file_name, "_algorithm_", algorithm)
#       if (sampling_option == 100) file_name <- paste0(file_name, "_test_fn_")
#       file_name <- paste0("Laptop_",    file_name)
#       N_scalability_pilot_results_for_each_N_Laptop[[dataset_index]] <- readRDS(file_name)
#       
#       n_chunks_combos <-          dim(   N_scalability_pilot_results_for_each_N_Laptop[[dataset_index]] )[1]
#       n_runs <-                   dim(   N_scalability_pilot_results_for_each_N_Laptop[[dataset_index]] )[2]
#       n_cores_combos <-           dim(   N_scalability_pilot_results_for_each_N_Laptop[[dataset_index]] )[3]
#       
#       print(n_chunks_combos)
#       print(n_runs)
#       print(n_cores_combos)
#       
#       N_scalability_pilot_results_for_each_N_Laptop # list of dim length(N_vec) = 6, each with dim = 3 (= N_chunks) x 5 ( = N_runs) x 7 (= cores)
#       
#       for (N_cores_index in 1:length(N_vec)) {
#         N_scalability_pilot_results_medians_for_each_N_and_each_N_chains_Laptop[[dataset_index]]  <-    apply( N_scalability_pilot_results_for_each_N_Laptop[[dataset_index]], c(1, 3), median, na.rm = TRUE)
#         N_scalability_pilot_results_SD_for_each_N_and_each_N_chains_Laptop[[dataset_index]]  <-         apply( N_scalability_pilot_results_for_each_N_Laptop[[dataset_index]], c(1, 3), sd,     na.rm = TRUE)
#       }
#     }
#     
#   }
#   
#   
#   
#   nrow_df_Laptop <- n_cores_combos * length(N_vec) * n_chunks_combos
#   N_scalability_pilot_study_df_Laptop <- tibble(N = rep(NA, nrow_df_Laptop),
#                                                 N_num = N,
#                                                 device = rep("Laptop", nrow_df_Laptop),
#                                                 time_avg = N,
#                                                 time_SD = N,
#                                                 N_chunks = N, 
#                                                 N_chunks_num = N, 
#                                                 N_cores = N, 
#                                                 N_chains = N, 
#                                                 N_iter = N#, 
#                                                 # iter_per_second = N, 
#                                                 # iter_per_second_adj_for_N = N
#   )
#   
#   
#   
#   counter <- 1
#   
#   for (dataset_index in 1:length(N_vec)) {
#     for (N_cores_index in 1:n_cores_combos) {
#       for (i in 1:2) {  # 3 different N_chunks tested
#         
#         
#         N = N_vec[dataset_index]
#         
#         N_scalability_pilot_study_df_Laptop$N[counter]   <-      as.character(N_vec[dataset_index])
#         N_scalability_pilot_study_df_Laptop$N_num[counter]   <-  as.numeric(N_vec[dataset_index])
#         N_scalability_pilot_study_df_Laptop$time_avg[counter]   <-    N_scalability_pilot_results_medians_for_each_N_and_each_N_chains_Laptop[[dataset_index]][i, N_cores_index]
#         N_scalability_pilot_study_df_Laptop$time_SD[counter]    <-    N_scalability_pilot_results_SD_for_each_N_and_each_N_chains_Laptop[[dataset_index]][i, N_cores_index]
#         
#         if (N == 500)     N_chunks = n_chunks_vec_N_500[i]
#         if (N == 1000)    N_chunks = n_chunks_vec_N_1000[i]
#         if (N == 2500)    N_chunks = n_chunks_vec_N_2500[i]
#         if (N == 5000)    N_chunks = n_chunks_vec_N_5000[i]
#         if (N == 12500)   N_chunks = n_chunks_vec_N_12500[i]
#         if (N == 25000)   N_chunks = n_chunks_vec_N_25000[i]
#         
#         N_scalability_pilot_study_df_Laptop$N_chunks[counter]    <-    as.character(N_chunks)
#         N_scalability_pilot_study_df_Laptop$N_chunks_num[counter]    <-    N_chunks 
#         
#         N_scalability_pilot_study_df_Laptop$N_cores[counter]  <- cores[N_cores_index]
#         ###   N_scalability_pilot_study_df_Laptop$N_chains[counter] <- cores[N_cores_index]
#         
#         
#         if (algorithm != "Mplus_WCP") { 
#           N_scalability_pilot_study_df_HPC$N_chains[counter] <- cores[N_cores_index]
#         } else { 
#           if (N_scalability_pilot_study_df_Laptop$device[counter] == "HPC")     N_scalability_pilot_study_df_HPC$N_chains[counter] <-  8
#           if (N_scalability_pilot_study_df_Laptop$device[counter] == "Laptop")  N_scalability_pilot_study_df_HPC$N_chains[counter] <-  4
#         }
#         
#         if (N == 500)     N_iter = 400
#         if (N == 1000)    N_iter = 200
#         if (N == 2500)    N_iter = 80
#         if (N == 5000)    N_iter = 40
#         if (N == 12500)   N_iter = 16
#         if (N == 25000)   N_iter = 8
#         # 
#         
#         if (algorithm == "Mplus_WCP")   { 
#           N_iter =  30  * N_iter    
#         } else if (algorithm == "Mplus_standard") { 
#           N_iter =   5 * N_iter 
#         } else if (algorithm == "AD_Stan") {  
#           N_iter =    roound(N_iter / 2, 0)
#         }
#         
#         
#         
#         N_scalability_pilot_study_df_Laptop$N_iter[counter] <- N_iter
#         
#         counter = counter + 1
#         
#       }
#     }
#   }
#   
#   
#   
# }
# 
# 
# 
# n_chunks_combos
# 
# 
# N_scalability_pilot_study_df_Laptop <- filter(N_scalability_pilot_study_df_Laptop, !(is.na(N_chunks_num)))
# View(N_scalability_pilot_study_df_Laptop)
# 
# 
# 
# {
#   
#   N_scalability_pilot_study_df_HPC_and_Laptop <- rbind(N_scalability_pilot_study_df_HPC,
#                                                        N_scalability_pilot_study_df_Laptop)
#   
#   ### View(N_scalability_pilot_study_df_HPC_and_Laptop)
#   
#   
#   N_scalability_pilot_study_df_HPC_and_Laptop <- dplyr::mutate(N_scalability_pilot_study_df_HPC_and_Laptop, 
#                                                                N_label =  paste0("N = ",  N_scalability_pilot_study_df_HPC_and_Laptop$N))
#   N_scalability_pilot_study_df_HPC_and_Laptop$N_label <- factor( N_scalability_pilot_study_df_HPC_and_Laptop$N_label )
#   N_scalability_pilot_study_df_HPC_and_Laptop$N_label <- factor(N_scalability_pilot_study_df_HPC_and_Laptop$N_label, 
#                                                                 levels = c("N = 500", "N = 1000", "N = 2500", "N = 5000", "N = 12500", "N = 25000"))
#   
#   
#   
#   N_scalability_pilot_study_df_HPC_and_Laptop <- dplyr::mutate(N_scalability_pilot_study_df_HPC_and_Laptop, 
#                                                                N_and_N_iter_label =  paste0(N_label, ", N_{iter} = ", N_iter))
#   N_scalability_pilot_study_df_HPC_and_Laptop$N_and_N_iter_label <- factor( N_scalability_pilot_study_df_HPC_and_Laptop$N_and_N_iter_label )
#   N_scalability_pilot_study_df_HPC_and_Laptop$N_and_N_iter_label <- factor(N_scalability_pilot_study_df_HPC_and_Laptop$N_and_N_iter_label, 
#                                                                            levels = c("N = 500, N_{iter} = 400",
#                                                                                       "N = 1000, N_{iter} = 200", 
#                                                                                       "N = 2500, N_{iter} = 80", 
#                                                                                       "N = 5000, N_{iter} = 40",
#                                                                                       "N = 12500, N_{iter} = 16",
#                                                                                       "N = 25000, N_{iter} = 8"))
#   
#   N_scalability_pilot_study_df_HPC_and_Laptop <-  N_scalability_pilot_study_df_HPC_and_Laptop %>%  
#     mutate(N_chains = ifelse(algorithm == "Mplus_WCP", 4, N_chains))  %>% 
#     print(n = 1000)
#   
#   
#   N_scalability_pilot_study_df_HPC_and_Laptop <- dplyr::mutate(N_scalability_pilot_study_df_HPC_and_Laptop, 
#                                                                N_chains_label =  paste0("N_{chains} = ", N_chains))
#   N_scalability_pilot_study_df_HPC_and_Laptop$N_chains_label <- factor(N_scalability_pilot_study_df_HPC_and_Laptop$N_chains_label, levels = c("N_{chains} = 2",
#                                                                                                                                               "N_{chains} = 4", 
#                                                                                                                                               #"N_{chains} = 6", 
#                                                                                                                                               "N_{chains} = 8", 
#                                                                                                                                               "N_{chains} = 16", 
#                                                                                                                                               "N_{chains} = 32",
#                                                                                                                                               "N_{chains} = 64"
#                                                                                                                                               #"N_{chains} = 96"
#   ))
#   
#   levels(N_scalability_pilot_study_df_HPC_and_Laptop$N_chains_label)
#   ### N_scalability_pilot_study_df_HPC_and_Laptop$n_chains_burnin <- factor(N_scalability_pilot_study_df_HPC_and_Laptop$n_chains_burnin)
#   
#   
#   
#   N_scalability_pilot_study_df_HPC_and_Laptop$N_chunks <- factor( N_scalability_pilot_study_df_HPC_and_Laptop$N_chunks )
#   unique(N_scalability_pilot_study_df_HPC_and_Laptop$N_chunks )
#   N_scalability_pilot_study_df_HPC_and_Laptop$N_chunks <- factor(N_scalability_pilot_study_df_HPC_and_Laptop$N_chunks, 
#                                                                  levels = c("1",
#                                                                             "2", 
#                                                                             # "4", 
#                                                                             "5",
#                                                                             # "8",
#                                                                             "10",
#                                                                             "25",
#                                                                             "40", 
#                                                                             #  "50",
#                                                                             # "100", 
#                                                                             "125"
#                                                                             #"200",
#                                                                             #"250",
#                                                                             #"500"
#                                                                  ))
#   
#   
#   
#   levels(N_scalability_pilot_study_df_HPC_and_Laptop$N_chunks)
#   
#   # n_chunks_vec_N_500   <- c(1, 2, 5)  
#   # n_chunks_vec_N_1000  <- c(1, 2, 8)
#   # n_chunks_vec_N_2500  <- c(1, 2, 20)  
#   # n_chunks_vec_N_5000  <- c(1, 20, 40, 100) 
#   # n_chunks_vec_N_12500 <- c(1, 50, 100, 250) 
#   # n_chunks_vec_N_25000 <- c(1, 125, 200, 500) 
#   # 
#   # n_chunks_vec_N_500   <- c(1, 2, 5)
#   # n_chunks_vec_N_1000  <- c(1, 2, 5)
#   # n_chunks_vec_N_2500  <- c(1, 2, 4)
#   # n_chunks_vec_N_5000  <- c(1, 5,  10,  20)
#   # n_chunks_vec_N_12500 <- c(1, 20, 50,  100)
#   # n_chunks_vec_N_25000 <- c(1, 40, 125, 200)
#   
#   
#   N_scalability_pilot_study_df_HPC_and_Laptop <- N_scalability_pilot_study_df_HPC_and_Laptop %>% dplyr::mutate( N_chunks_group = case_when( N_chunks_num == 1 ~ "1 chunk",
#                                                                                                                                             # N_chunks_num == 2 ~ "2 chunks",
#                                                                                                                                             N_chunks_num %in% c(3:99999) ~ "Optimal N_{chunks}") )
#   
#   N_scalability_pilot_study_df_HPC_and_Laptop$N_chunks_group <- factor( N_scalability_pilot_study_df_HPC_and_Laptop$N_chunks_group )
#   
#   N_scalability_pilot_study_df_HPC_and_Laptop$device <- factor( N_scalability_pilot_study_df_HPC_and_Laptop$device)
#   
# }
# 
# 
# 
# 
# 
# 
# 
# 
# N_scalability_pilot_study_df_HPC_and_Laptop %>% arrange(device, N_num, N_chunks_num, N_chains) %>% print(n = 100)
# 
# 
# View(N_scalability_pilot_study_df_HPC_and_Laptop)
# 
# 
# 
# ### saveRDS(N_scalability_pilot_study_df_HPC_and_Laptop, file  = paste0("N_scalability_pilot_study_df_HPC_and_Laptop_", algorithm))
# 
# 
# 
# # View( 
# #   dplyr::filter(N_scalability_pilot_study_df_HPC_and_Laptop , 
# #                 device == "HPC",
# #                 N_chunks == "1"
# #   )
# #   )
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
# # Plots ----------------------------------------------------------------------------------------------------------------------------
# #  ----------------------------- plot 1 (a)  --------------------------
# if (algorithm == "MD_BayesMVP") {
#   {
#     plot_N_scalability_pilot_study_plot_1a_HPC <-   ggplot(    dplyr::filter(N_scalability_pilot_study_df_HPC_and_Laptop ,
#                                                                              device == "HPC",
#                                                                              #   device == "Laptop",
#                                                                              #  N_chunks == "1",
#                                                                              N_num > 2500
#     ),
#     mapping = aes(x = N_cores,
#                   y =   time_avg,
#                   colour = N_chunks,
#                   #   linetype = device,
#                   shape = N
#     ),
#     ) +
#       geom_point(size = 4) +
#       geom_line(linewidth = 1,           aes(x = N_cores,
#                                              y =  time_avg,
#                                              #   colour = N_chains_label,
#                                              linetype = device
#       )) +
#       theme_bw(base_size = 20) +
#       theme(legend.position = "bottom")  +
#       ylab("Time (seconds)") +
#       xlab("N_{chains}" ) +
#       scale_x_continuous(breaks = c(8, 16, 32, 64, 96)) + 
#       facet_wrap( ~ N_label, 
#                   scales = "free" 
#       )
#     
#     plot_N_scalability_pilot_study_plot_1a_HPC
#   }
#   
#   {
#     
#     plot_N_scalability_pilot_study_plot_1a_Laptop <-   ggplot(    dplyr::filter(N_scalability_pilot_study_df_HPC_and_Laptop ,
#                                                                                 #  device == "HPC",
#                                                                                 device == "Laptop",
#                                                                                 N_num > 2500
#                                                                                 #  N_chunks == "1"
#     ),
#     mapping = aes(x = N_cores,
#                   y =   time_avg,
#                   
#                   colour = N_chunks,
#                   shape = N
#                   #   colour = N_chunks_group,
#                   # linetype = device
#     ),
#     ) +
#       geom_point(size = 4) +
#       geom_line(linewidth = 1,           aes(x = N_cores,
#                                              y =  time_avg,
#                                              #   colour = N_chains_label,
#                                              linetype = device
#       )) +
#       theme_bw(base_size = 20) +
#       theme(legend.position = "bottom")  +
#       ylab("Time (seconds)") +
#       xlab("N_{chains}" ) +
#       scale_x_continuous(breaks = c(2, 4, 8)) +
#       facet_wrap( ~ N_label,
#                   scales = "free"
#       )
#     
#     plot_N_scalability_pilot_study_plot_1a_Laptop
#     
#   }
#   
#   plot_N_scalability_pilot_study_plot_1a_HPC +
#     plot_N_scalability_pilot_study_plot_1a_Laptop +
#     plot_layout(ncol = 1)
#   
#   {
#     plot_scale_factor <- 1
#     plot_width <-  16*plot_scale_factor
#     plot_height <- 2*9*plot_scale_factor
#     }
#   
#   plot_name <- paste0("plot_N_scalability_pilot_study_", algorithm, "_plot_1a.png")
#   png(plot_name ,units = "in", width = plot_width, height=plot_height, res=800)
#   plot_N_scalability_pilot_study_plot_1a_HPC +
#     plot_N_scalability_pilot_study_plot_1a_Laptop +
#     plot_layout(ncol = 1)
#   dev.off()
#   
# } else if (algorithm %in% c("AD_Stan", "Mplus_standard"))  { 
#   
#   plot_N_scalability_pilot_study_plot_1_HPC_and_laptop <-   ggplot(    dplyr::filter(N_scalability_pilot_study_df_HPC_and_Laptop , 
#                                                                                      #  device == "HPC",
#                                                                                      N_chunks == "1"
#                                                                                      
#   ), 
#   mapping = aes(x = N_cores,
#                 y =   time_avg,
#                 colour = N_label,
#                 #   linetype = device
#   ),
#   ) +
#     geom_point(size = 4) +
#     geom_line(linewidth = 1,           aes(x = N_cores,
#                                            y =  time_avg, 
#                                            colour = N_label,
#                                            #   linetype = device
#     )) +
#     theme_bw(base_size = 20) +
#     theme(legend.position = "bottom")  +
#     ylab("Time (seconds)") +
#     xlab("N_{chains}" ) +
#     scale_x_continuous(breaks = c(2, 4, 6, 8, 16, 32, 64, 96)) + 
#     facet_wrap( ~ device,
#                 scales = "free",
#     )
#   
#   plot_N_scalability_pilot_study_plot_1_HPC_and_laptop
#   
#   
#   
#   
#   {
#     plot_scale_factor <- 1
#     plot_width <-  16*plot_scale_factor
#     plot_height <- 1*9*plot_scale_factor
#   }
#   
#   
#   plot_name <- paste0("plot_N_scalability_pilot_study_", algorithm, "_plot_1.png")
#   png(plot_name ,units = "in", width = plot_width, height=plot_height, res=800)
#   plot_N_scalability_pilot_study_plot_1_HPC_and_laptop
#   dev.off()
#   
#   
# } else if (algorithm %in% c("Mplus_WCP"))  {  
#   
#   plot_N_scalability_pilot_study_plot_1_HPC_and_laptop <-   ggplot(    dplyr::filter(N_scalability_pilot_study_df_HPC_and_Laptop , 
#                                                                                      #   device == "HPC",
#                                                                                      N_chunks == "1"
#                                                                                      
#   ), 
#   mapping = aes(x = N_cores,
#                 y =   time_avg,
#                 colour = N_label,
#                 #   linetype = device
#   ),
#   ) +
#     geom_point(size = 4) +
#     geom_line(linewidth = 1,           aes(x = N_cores,
#                                            y =  time_avg, 
#                                            colour = N_label,
#                                            #   linetype = device
#     )) +
#     theme_bw(base_size = 20) +
#     theme(legend.position = "bottom")  +
#     ylab("Time (seconds)") +
#     xlab("N_{cores}" ) +
#     scale_x_continuous(breaks = c(2, 4, 6, 8, 16, 32, 64, 96)) + 
#     facet_wrap( ~ device,
#                 scales = "free",
#     )
#   
#   plot_N_scalability_pilot_study_plot_1_HPC_and_laptop
#   
#   
#   
#   
#   {
#     plot_scale_factor <- 1
#     plot_width <-  16*plot_scale_factor
#     plot_height <- 1*9*plot_scale_factor
#   }
#   
#   
#   plot_name <- paste0("plot_N_scalability_pilot_study_", algorithm, "_plot_1.png")
#   png(plot_name ,units = "in", width = plot_width, height=plot_height, res=800)
#   plot_N_scalability_pilot_study_plot_1_HPC_and_laptop
#   dev.off()
#   
#   
#   
# }
# 
# 
# 
# #  ----------------------------- plot 1 (b) --------------------------
# if (algoorithm == "MD_BayesMVP") {
#   {
#     
#     plot_N_scalability_pilot_study_plot_1b_HPC <-   ggplot(    dplyr::filter(N_scalability_pilot_study_df_HPC_and_Laptop ,
#                                                                              device == "HPC",
#     ),
#     mapping = aes(x = N_cores,
#                   y =   time_avg,
#                   colour = N_label,
#                   linetype = device
#     ),
#     ) +
#       geom_point(size = 4) +
#       geom_line(linewidth = 1,           aes(x = N_cores,
#                                              y =  time_avg,
#       )) +
#       theme_bw(base_size = 20) +
#       theme(legend.position = "bottom")  +
#       ylab("Time (seconds)") +
#       xlab("N_{chains}" ) +
#       scale_x_continuous(breaks = c(8, 16, 32, 64, 96)) +
#       facet_wrap( ~ N_chunks_group,
#                   scales = "free"
#       )
#     plot_N_scalability_pilot_study_plot_1b_HPC
#     
#   }
#   
#   {
#     
#     plot_N_scalability_pilot_study_plot_1b_Laptop <-   ggplot(    dplyr::filter(N_scalability_pilot_study_df_HPC_and_Laptop ,
#                                                                                 #  device == "HPC",
#                                                                                 device == "Laptop",
#                                                                                 #  N_chunks == "1"
#     ),
#     mapping = aes(x = N_cores,
#                   y =   time_avg,
#                   colour = N_label
#     ),
#     ) +
#       geom_point(size = 4) +
#       geom_line(linewidth = 1,           aes(x = N_cores,
#                                              y =  time_avg,
#                                              linetype = device
#       )) +
#       theme_bw(base_size = 20) +
#       theme(legend.position = "bottom")  +
#       ylab("Time (seconds)") +
#       xlab("N_{chains}" ) +
#       scale_x_continuous(breaks = c(2, 4, 6, 8)) +
#       facet_wrap( ~ N_chunks_group,
#                   scales = "free"
#       )
#     plot_N_scalability_pilot_study_plot_1b_Laptop
#     
#   }
#   
#   plot_N_scalability_pilot_study_plot_1b_HPC +
#     plot_N_scalability_pilot_study_plot_1b_Laptop +
#     plot_layout(ncol = 1)
#   
#   {
#     plot_scale_factor <- 1
#     plot_width <-  16*plot_scale_factor
#     plot_height <- 2*9*plot_scale_factor
#     }
#   
#   plot_name <- paste0("plot_N_scalability_pilot_study_", algorithm, "_plot_1b.png")
#   png(plot_name ,units = "in", width = plot_width, height=plot_height, res=800)
#   plot_N_scalability_pilot_study_plot_1b_HPC +
#     plot_N_scalability_pilot_study_plot_1b_Laptop +
#     plot_layout(ncol = 1)
#   dev.off()
#   
# } else if (algorithm %in% c("AD_Stan", "Mplus_standard"))  { 
#   
#   # no plot (1) (b) for Stan
#   
# } else if (algorithm %in% c("Mplus_WCP"))  {  
#   
# }
# 
# 
# #  ----------------------------- plot 1 (a) and (b)  on SAME PANEL --------------------------
# if (algoorithm == "MD_BayesMVP") {
#   
#   {
#     plot_scale_factor <- 1
#     plot_width <-  16*plot_scale_factor
#     plot_height <- (18+9 - 4)*plot_scale_factor
#   }
#   
#   plot_name <- paste0("plot_N_scalability_pilot_study_", algorithm, "_plot_1.png")
#   png(plot_name ,units = "in", width = plot_width, height=plot_height, res=800)
#   wrap_elements(
#     plot_N_scalability_pilot_study_plot_1a_HPC +  # theme(legend.position = "none") +
#       ylab(" " ) +
#       plot_N_scalability_pilot_study_plot_1a_Laptop +  ylab(" " )  +
#       plot_layout(ncol = 1)
#   ) +
#     labs(tag = "Time (seconds)") +
#     theme(
#       plot.tag = element_text(size = rel(2), angle = 90),
#       plot.tag.position = "left"
#     ) +
#     wrap_elements(
#       plot_N_scalability_pilot_study_plot_1b_HPC +  # theme(legend.position = "none") +
#         ylab(" " ) +
#         plot_N_scalability_pilot_study_plot_1b_Laptop +   ylab(" " )   +
#         plot_layout(ncol = 1)
#     ) +
#     labs(tag = "Time (seconds)") +
#     theme(
#       plot.tag = element_text(size = rel(2), angle = 90),
#       plot.tag.position = "left"
#     ) +
#     plot_layout(ncol = 1,
#                 heights = c(1, 0.75))
#   dev.off()
#   
# } else if (algorithm %in% c("AD_Stan", "Mplus_standard"))  { 
#   
#   ## None for Stan (see plot (1) for Stan above)
#   
# } else if (algorithm %in% c("Mplus_WCP"))  {  
#   
# }
# 
# 
# 
# 
# 
# 
# 
# 
# 
# #  ----------------------------- plot 2 (a) --------------------------
# if (algoorithm == "MD_BayesMVP") {
#   
#   View(N_scalability_pilot_study_df_HPC_and_Laptop_2)
#   
#   N_scalability_pilot_study_df_HPC_and_Laptop_2 <-  N_scalability_pilot_study_df_HPC_and_Laptop  %>%
#     group_by(device, N_num, N_chunks_num) %>%
#     mutate(time_perfect_scaling = min(time_avg))  # %>% print(n = 100)
#   
#   N_scalability_pilot_study_df_HPC_and_Laptop_2 <- filter(N_scalability_pilot_study_df_HPC_and_Laptop_2, !(is.na(N_chunks_group)) )
#   
#   {
#     
#     plot_N_scalability_pilot_study_plot_2a_HPC <-   ggplot(    dplyr::filter(N_scalability_pilot_study_df_HPC_and_Laptop_2 ,
#                                                                              device == "HPC",
#                                                                              #   device == "Laptop",
#                                                                              #  N_chunks == "1"
#     ),
#     mapping = aes(x = N_cores,
#                   y =   N_cores / time_avg,
#                   colour = N_chunks_group,
#                   linetype = device
#     ),
#     ) +
#       geom_point(size = 5) +
#       geom_line(linewidth = 1,   linetype = "dashed",   aes(x = N_cores,
#                                                             y =  N_cores / time_perfect_scaling)) +
#       geom_line(linewidth = 2,            aes(x = N_cores,
#                                               y =  N_cores / time_avg)) +
#       theme_bw(base_size = 32) +
#       theme(legend.position = "bottom")  +
#       ylab("Ratio (N_{chains} / second)") +
#       xlab("N_{chains}" ) +
#       scale_x_continuous(breaks = c(8, 16, 32, 64)) +
#       # geom_abline(slope=1, intercept= 0) +
#       facet_wrap( ~ N_label,
#                   scales = "free"
#       )
#     
#     plot_N_scalability_pilot_study_plot_2a_HPC
#     
#   }
#   
#   {
#     
#     plot_N_scalability_pilot_study_plot_2a_Laptop <-   ggplot(    dplyr::filter(N_scalability_pilot_study_df_HPC_and_Laptop_2 ,
#                                                                                 #  device == "HPC",
#                                                                                 device == "Laptop",
#                                                                                 #  N_chunks == "1"
#     ),
#     mapping = aes(x = N_cores,
#                   y =   N_cores / time_avg,
#                   colour = N_chunks_group,
#                   linetype = device
#     ),
#     ) +
#       geom_point(size = 4) +
#       geom_line(linewidth = 1,   linetype = "dashed",   aes(x = N_cores,
#                                                             y =  N_cores / time_perfect_scaling)) +
#       geom_line(linewidth = 2,            aes(x = N_cores,
#                                               y =  N_cores / time_avg)) +
#       theme_bw(base_size = 32) +
#       theme(legend.position = "bottom")  +
#       ylab("Ratio (N_{chains} / second)") +
#       xlab("N_{chains}" ) +
#       scale_x_continuous(breaks = c(2, 4,  8)) +
#       facet_wrap( ~ N_label,
#                   scales = "free"
#       )
#     
#     plot_N_scalability_pilot_study_plot_2a_Laptop
#     
#   }
#   
#   plot_N_scalability_pilot_study_plot_2a_HPC +
#     plot_N_scalability_pilot_study_plot_2a_Laptop +
#     plot_layout(ncol = 1)
#   
#   
#   {
#     plot_scale_factor <- 1
#     plot_width <-  16*plot_scale_factor
#     plot_height <- 2*9*plot_scale_factor
#     }
#   
#   plot_name <- paste0("plot_N_scalability_pilot_study_", algorithm, "_plot_2a.png")
#   png(plot_name ,units = "in", width = plot_width, height=plot_height, res=800)
#   plot_N_scalability_pilot_study_plot_2a_HPC +
#     plot_N_scalability_pilot_study_plot_2a_Laptop +
#     plot_layout(ncol = 1)
#   dev.off()
#   
# } else if (algorithm %in% c("AD_Stan", "Mplus_standard"))  { 
#   
#   
#   N_scalability_pilot_study_df_HPC_and_Laptop_2 <-  N_scalability_pilot_study_df_HPC_and_Laptop  %>%
#     group_by(device, N_num, N_chunks_num) %>%
#     mutate(time_perfect_scaling = min(time_avg))  # %>% print(n = 100)
#   
#   
#   
#   plot_N_scalability_pilot_study_plot_2a_HPC_and_laptop <-   ggplot(    dplyr::filter(N_scalability_pilot_study_df_HPC_and_Laptop_2 , 
#                                                                                       #  device == "HPC",
#                                                                                       N_chunks == "1"
#                                                                                       
#   ), 
#   mapping = aes(x = N_chains,
#                 y =   N_chains / time_avg,
#                 colour = N_label,
#                 # linetype = device
#   ),
#   )    +
#     geom_point(size = 4) +
#     # geom_line(linewidth = 1,           aes(x = N_chains,
#     #                                        y =   N_chains / time_avg,
#     #                                        colour = N_label,
#     #                                   #     linetype = device
#     # )) +
#     geom_line(linewidth = 1,   linetype = "dashed",   aes(x = N_chains,
#                                                           y =  N_chains / time_perfect_scaling)) +
#     geom_line(linewidth = 2,            aes(x = N_chains,
#                                             y =  N_chains / time_avg)) +
#     theme_bw(base_size = 20) +
#     theme(legend.position = "bottom")  +
#     ylab("Ratio (N_{chains} / second)") +
#     xlab("N_{chains}" ) +
#     scale_x_continuous(breaks = c(2, 4, 6, 8, 16, 32, 64, 96)) + 
#     facet_wrap( ~ device,
#                 scales = "free",
#     )
#   
#   plot_N_scalability_pilot_study_plot_2a_HPC_and_laptop
#   
#   
#   
#   
#   {
#     plot_scale_factor <- 1
#     plot_width <-  16*plot_scale_factor
#     plot_height <- 1*9*plot_scale_factor
#   }
#   
#   
#   plot_name <- paste0("plot_N_scalability_pilot_study_", algorithm, "_plot_2a.png")
#   png(plot_name ,units = "in", width = plot_width, height=plot_height, res=800)
#   plot_N_scalability_pilot_study_plot_2a_HPC_and_laptop
#   dev.off()
#   
# } else if (algorithm %in% c("Mplus_WCP"))  {  
#   
#   
#   N_scalability_pilot_study_df_HPC_and_Laptop_2 <-  N_scalability_pilot_study_df_HPC_and_Laptop  %>%
#     group_by(device, N_num, N_chunks_num) %>%
#     mutate(time_perfect_scaling = min(time_avg))  # %>% print(n = 100)
#   
#   
#   
#   plot_N_scalability_pilot_study_plot_2a_HPC_and_laptop <-   ggplot(    dplyr::filter(N_scalability_pilot_study_df_HPC_and_Laptop_2 , 
#                                                                                       # device == "HPC",
#                                                                                       N_chunks == "1"
#                                                                                       
#   ), 
#   mapping = aes(x = N_cores,
#                 y =   N_cores / time_avg,
#                 colour = N_label,
#                 # linetype = device
#   ),
#   )    +
#     geom_point(size = 4) +
#     # geom_line(linewidth = 1,           aes(x = N_chains,
#     #                                        y =   N_chains / time_avg,
#     #                                        colour = N_label,
#     #                                   #     linetype = device
#     # )) +
#     geom_line(linewidth = 1,   linetype = "dashed",   aes(x = N_cores,
#                                                           y =  N_cores / time_perfect_scaling)) +
#     geom_line(linewidth = 2,            aes(x = N_cores,
#                                             y =  N_cores / time_avg)) +
#     theme_bw(base_size = 20) +
#     theme(legend.position = "bottom")  +
#     ylab("Ratio (N_{cores} / second)") +
#     xlab("N_{cores}" ) +
#     scale_x_continuous(breaks = c(2, 4, 6, 8, 16, 32, 64, 96)) + 
#     facet_wrap( ~ device,
#                 scales = "free",
#     )
#   
#   plot_N_scalability_pilot_study_plot_2a_HPC_and_laptop
#   
#   
#   
#   
#   {
#     plot_scale_factor <- 1
#     plot_width <-  16*plot_scale_factor
#     plot_height <- 1*9*plot_scale_factor
#   }
#   
#   
#   plot_name <- paste0("plot_N_scalability_pilot_study_", algorithm, "_plot_2a.png")
#   png(plot_name ,units = "in", width = plot_width, height=plot_height, res=800)
#   plot_N_scalability_pilot_study_plot_2a_HPC_and_laptop
#   dev.off()
#   
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
# 
# 
# 
# 
# 
# #  ----------------------------- plot 2 (b) --------------------------
# # this plot is tto compare pasrallel scaling IGNORING absolute time 
# 
# if (algoorithm == "MD_BayesMVP") {
#   
#   # N_scalability_pilot_study_df_HPC_and_Laptop_2 <-  N_scalability_pilot_study_df_HPC_and_Laptop  %>%
#   #   group_by(device, N_num, N_chunks_num) %>%
#   #   mutate(time_perfect_scaling = min(time_avg))  # %>% print(n = 100)
#   # 
#   # {
#   #   
#   #   plot_N_scalability_pilot_study_plot_2b_HPC <-   ggplot(    dplyr::filter(N_scalability_pilot_study_df_HPC_and_Laptop_2 , 
#   #                                                                           device == "HPC",
#   #                                                                           #   device == "Laptop",
#   #                                                                           #  N_chunks == "1"
#   #   ), 
#   #   mapping = aes(x = N_chains,
#   #                 y =   (N_chains / time_avg) * time_perfect_scaling,
#   #                 colour = N_label,
#   #                 linetype = device
#   #   ),
#   #   ) +
#   #     geom_point(size = 5) +
#   #     geom_line(linewidth = 1,   linetype = "dashed",   aes(x = N_chains,
#   #                                                           y =  (N_chains / time_perfect_scaling) * time_perfect_scaling )) +
#   #     geom_line(linewidth = 2,            aes(x = N_chains,
#   #                                             y =  time_perfect_scaling * N_chains / time_avg)) +
#   #     theme_bw(base_size = 20) +
#   #     theme(legend.position = "bottom")  +
#   #     ylab("Adj. ratio (N_{chains} / sec) * min[time]") +
#   #     xlab("N_{chains}" ) +
#   #     scale_x_continuous(breaks = c(8, 16, 32, 64, 96)) + 
#   #     facet_wrap( ~ N_chunks_group, 
#   #                     scales = "free"
#   #     ) 
#   #   
#   #   plot_N_scalability_pilot_study_plot_2b_HPC
#   #   
#   #   }
#   # 
#   # 
#   # {
#   #   
#   #   plot_N_scalability_pilot_study_plot_2b_Laptop <-   ggplot(    dplyr::filter(N_scalability_pilot_study_df_HPC_and_Laptop_2 , 
#   #                                                                              #  device == "HPC",
#   #                                                                              device == "Laptop",
#   #                                                                              #  N_chunks == "1"
#   #   ), 
#   #                 mapping = aes(x = N_chains,
#   #                               y =   (N_chains / time_avg) * time_perfect_scaling,
#   #                               colour = N_label,
#   #                               linetype = device
#   #                 ),
#   #   ) +
#   #     geom_point(size = 5) +
#   #     geom_line(linewidth = 1,   linetype = "dashed",   aes(x = N_chains,
#   #                                                           y =  (N_chains / time_perfect_scaling) * time_perfect_scaling )) +
#   #     geom_line(linewidth = 2,        colour = "black",     aes(x = N_chains,
#   #                                             y =  time_perfect_scaling * N_chains / time_avg)) +
#   #     theme_bw(base_size = 20) +
#   #     theme(legend.position = "bottom")  +
#   #     ylab("Adj. ratio (N_{chains} / sec) * min[time]") +
#   #     xlab("N_{chains}" ) +
#   #     scale_x_continuous(breaks = c(2, 4, 6, 8)) + 
#   #     facet_wrap( ~ N_chunks_group, 
#   #                 scales = "free"
#   #     ) 
#   #   
#   #   plot_N_scalability_pilot_study_plot_2b_Laptop
#   #   
#   # }
#   # 
#   # plot_N_scalability_pilot_study_plot_2b_HPC + 
#   #   plot_N_scalability_pilot_study_plot_2b_Laptop + 
#   #   plot_layout(ncol = 1)
#   # 
#   # 
#   # { 
#   #   plot_scale_factor <- 4
#   #   plot_width <-  4*plot_scale_factor
#   #   plot_height <- 3*plot_scale_factor
#   #   }
#   # 
#   # 
#   # plot_name <- paste0("plot_N_scalability_pilot_study_", algorithm, "_plot_2b.png")
#   # png(plot_name ,units = "in", width = plot_width, height=plot_height, res=800)
#   # plot_N_scalability_pilot_study_plot_2b_HPC + 
#   #   plot_N_scalability_pilot_study_plot_2b_Laptop + 
#   #   plot_layout(ncol = 1)
#   # dev.off()
#   
#   
# } else if (algorithm %in% c("AD_Stan", "Mplus_standard"))  { 
#   
#   
#   N_scalability_pilot_study_df_HPC_and_Laptop_2 <-  N_scalability_pilot_study_df_HPC_and_Laptop  %>%
#     group_by(device, N_num, N_chunks_num) %>%
#     mutate(time_perfect_scaling = min(time_avg),
#            Adj_scaling_ratio = time_perfect_scaling * (N_chains / time_avg))   %>%
#     filter(!(is.na(time_avg))) %>% 
#     print(n = 100) 
#   
#   
#   plot_N_scalability_pilot_study_plot_2b_HPC_and_laptop <-   ggplot(    dplyr::filter(N_scalability_pilot_study_df_HPC_and_Laptop_2 ,  
#                                                                                       N_chunks == "1"  ), 
#                                                                         mapping = aes(x = N_chains,
#                                                                                       y =  Adj_scaling_ratio,
#                                                                                       colour = N_label,
#                                                                                       # linetype = device
#                                                                         )
#   ) + 
#     geom_point(size = 4) +
#     # geom_line(linewidth = 1,           aes(x = N_chains,
#     #                                        y =  Adj_scaling_ratio,
#     #                                        colour = N_label,
#     #                                        linetype = device
#     # )) +
#     geom_line(linewidth = 1,   linetype = "dashed",  colour = "black",  aes(x = N_chains, 
#                                                                             y =  (N_chains / time_perfect_scaling) * time_perfect_scaling )) +
#     geom_line(linewidth = 2,            aes(x = N_chains,
#                                             y =  time_perfect_scaling * N_chains / time_avg)) +
#     theme_bw(base_size = 20) +
#     theme(legend.position = "bottom")  +
#     ylab("Adj. ratio (N_{chains} / sec) * min[time]") +
#     xlab("N_{chains}" ) +
#     scale_x_continuous(breaks = c(2, 4, 6, 8, 16, 32, 64, 96)) + 
#     facet_wrap( ~ device,
#                 scales = "free",
#     )
#   
#   plot_N_scalability_pilot_study_plot_2b_HPC_and_laptop
#   
#   
#   
#   
#   {
#     plot_scale_factor <- 1
#     plot_width <-  16*plot_scale_factor
#     plot_height <- 1*9*plot_scale_factor
#   }
#   
#   
#   plot_name <- paste0("plot_N_scalability_pilot_study_", algorithm, "_plot_2b.png")
#   png(plot_name ,units = "in", width = plot_width, height=plot_height, res=800)
#   plot_N_scalability_pilot_study_plot_2b_HPC_and_laptop
#   dev.off()
#   
# } else if (algorithm %in% c("Mplus_WCP"))  {  
#   
#   
#   N_scalability_pilot_study_df_HPC_and_Laptop_2 <-  N_scalability_pilot_study_df_HPC_and_Laptop  %>%
#     group_by(device, N_num, N_chunks_num) %>%
#     mutate(time_perfect_scaling = min(time_avg),
#            Adj_scaling_ratio = time_perfect_scaling * (N_cores / time_avg))   %>%
#     filter(!(is.na(time_avg))) %>% 
#     print(n = 100) 
#   
#   
#   plot_N_scalability_pilot_study_plot_2b_HPC_and_laptop <-   ggplot(    dplyr::filter(N_scalability_pilot_study_df_HPC_and_Laptop_2 ,  
#                                                                                       N_chunks == "1" ,
#                                                                                       #  device == "HPC"
#   ), 
#   mapping = aes(x = N_cores,
#                 y =  Adj_scaling_ratio,
#                 colour = N_label,
#                 # linetype = device
#   )
#   ) + 
#     geom_point(size = 4) +
#     # geom_line(linewidth = 1,           aes(x = N_cores,
#     #                                        y =  Adj_scaling_ratio,
#     #                                        colour = N_label,
#     #                                        linetype = device
#     # )) +
#     geom_line(linewidth = 1,   linetype = "dashed",  colour = "black",  aes(x = N_cores, 
#                                                                             y =  (N_cores / time_perfect_scaling) * time_perfect_scaling )) +
#     geom_line(linewidth = 2,            aes(x = N_cores,
#                                             y =  time_perfect_scaling * N_cores / time_avg)) +
#     theme_bw(base_size = 20) +
#     theme(legend.position = "bottom")  +
#     ylab("Adj. ratio (N_{cores} / sec) * min[time]") +
#     xlab("N_{cores}" ) +
#     scale_x_continuous(breaks = c(2, 4, 6, 8, 16, 32, 64, 96)) + 
#     facet_wrap( ~ device,
#                 scales = "free",
#     )
#   
#   plot_N_scalability_pilot_study_plot_2b_HPC_and_laptop
#   
#   
#   
#   
#   {
#     plot_scale_factor <- 1
#     plot_width <-  16*plot_scale_factor
#     plot_height <- 1*9*plot_scale_factor
#   }
#   
#   
#   plot_name <- paste0("plot_N_scalability_pilot_study_", algorithm, "_plot_2b.png")
#   png(plot_name ,units = "in", width = plot_width, height=plot_height, res=800)
#   plot_N_scalability_pilot_study_plot_2b_HPC_and_laptop
#   dev.off()
#   
#   
# }
# 
# 
# #  ----------------------------- plot 2 (a) and (b)  on SAME PANEL --------------------------
# if (algoorithm == "MD_BayesMVP") {
#   
#   # { 
#   #   plot_scale_factor <- 1
#   #   plot_width <-  16*plot_scale_factor
#   #   plot_height <- (18+9 - 4)*plot_scale_factor
#   # }
#   # 
#   # 
#   # 
#   # plot_name <- paste0("plot_N_scalability_pilot_study_", algorithm, "_plot_2.png")
#   # png(plot_name ,units = "in", width = plot_width, height=plot_height, res=800)
#   # wrap_elements(
#   #   plot_N_scalability_pilot_study_plot_2a_HPC +  # theme(legend.position = "none") + 
#   #     ylab(" " ) +
#   #   plot_N_scalability_pilot_study_plot_2a_Laptop +  ylab(" " )  + 
#   #   plot_layout(ncol = 1)
#   #   ) +
#   #     labs(tag = "Ratio (N_{chains} / sec)") +
#   #     theme(
#   #       plot.tag = element_text(size = rel(2), angle = 90),
#   #       plot.tag.position = "left"
#   #     ) + 
#   #   wrap_elements(
#   #   plot_N_scalability_pilot_study_plot_2b_HPC +  # theme(legend.position = "none") +   
#   #     ylab(" " ) + 
#   #   plot_N_scalability_pilot_study_plot_2b_Laptop +   ylab(" " )   + 
#   #   plot_layout(ncol = 1)
#   #   ) +
#   #   labs(tag = "Adj. ratio (N_{chains} / sec) * min[time]") +
#   #   theme(
#   #     plot.tag = element_text(size = rel(2), angle = 90),
#   #     plot.tag.position = "left"
#   #   ) + 
#   # plot_layout(ncol = 1, 
#   #             heights = c(1, 0.60))
#   # dev.off()
#   
# } else if (algorithm %in% c("AD_Stan", "Mplus_standard"))  {
#   
#   {
#     plot_scale_factor <- 1
#     plot_width <-  16*plot_scale_factor
#     plot_height <- (1+9)*plot_scale_factor
#   }
#   
#   
#   
#   plot_name <- paste0("plot_N_scalability_pilot_study_", algorithm, "_plot_2.png")
#   png(plot_name , units = "in", width = plot_width, height=plot_height, res=800)
#   plot_N_scalability_pilot_study_plot_2a_HPC_and_laptop + theme(legend.position = "none") + 
#     plot_N_scalability_pilot_study_plot_2b_HPC_and_laptop + 
#     plot_layout(ncol = 1)
#   dev.off()
#   
# } else if (algorithm %in% c("Mplus_WCP"))  {  
#   
#   {
#     plot_scale_factor <- 4
#     plot_width <-  4*plot_scale_factor
#     plot_height <- (3)*plot_scale_factor
#   }
#   
#   
#   
#   plot_name <- paste0("plot_N_scalability_pilot_study_", algorithm, "_plot_2.png")
#   png(plot_name , units = "in", width = plot_width, height=plot_height, res=800)
#   plot_N_scalability_pilot_study_plot_2a_HPC_and_laptop + theme(legend.position = "none") + 
#     plot_N_scalability_pilot_study_plot_2b_HPC_and_laptop + 
#     plot_layout(ncol = 1)
#   dev.off()
#   
#   
#   
# }
# 
# 
# 
# 
# 
# # Plot(s) comparing Mplus_WCP and Mplus_standard 
# 
# 
# 
# 
# df_scalability_Mplus_standard   <- readRDS("N_scalability_pilot_study_df_HPC_and_Laptop_Mplus_standard")  %>% mutate(Algorithm = "Mplus_standard")  %>% filter(!(is.na(time_avg))) %>% print(n = 1000)
# df_scalability_Mplus_WCP        <- readRDS("N_scalability_pilot_study_df_HPC_and_Laptop_Mplus_WCP")  %>% mutate(Algorithm = "Mplus_WCP") %>% filter(!(is.na(time_avg))) %>% print(n = 1000)
# 
# ### df_scalability_Mplus_WCP <- df_scalability_Mplus_WCP %>% mutate(N_chains = 4)  %>% print(n = 1000)
# 
# df_scalability_Mplus_standard_and_WCP <- rbind(df_scalability_Mplus_standard, df_scalability_Mplus_WCP)
# 
# df_scalability_Mplus_standard_and_WCP$Algorithm <- factor(  df_scalability_Mplus_standard_and_WCP$Algorithm )
# 
# df_scalability_Mplus_standard_and_WCP_2   <-  df_scalability_Mplus_standard_and_WCP  %>%
#   group_by(device, Algorithm, N_num, N_chunks_num) %>%
#   mutate(time_perfect_scaling = min(time_avg),
#          Adj_scaling_ratio = time_perfect_scaling * (N_cores / time_avg))   %>%
#   filter(!(is.na(time_avg))) %>% 
#   mutate(time_avg_adj_for_Mplus_WCP_vs_standard =     ifelse(Algorithm == "Mplus_WCP", time_avg * (N_cores / N_chains), time_avg)) %>% 
#   mutate(time_perfect_adj_for_Mplus_WCP_vs_standard = ifelse(Algorithm == "Mplus_WCP",  (N_cores / N_chains) , time_avg)) %>% 
#   print(n = 100) %>% 
#   filter(N_chains != 2)
# 
# 
# 
# 
# 
# subset_Mplus_standard <-  filter(df_scalability_Mplus_standard_and_WCP_2, N_chunks == 1, N == 12500, device == "Laptop", Algorithm == "Mplus_standard")
# 
# subset_Mplus_WCP <-  filter(df_scalability_Mplus_standard_and_WCP_2, N_chunks == 1, N == 12500, device == "Laptop" , Algorithm == "Mplus_WCP")
# 
# 
# plot_N_scalability_plot_HPC_and_laptop_Mplus_WCP_vs_standard_plot_1 <-   ggplot(    dplyr::filter(df_scalability_Mplus_standard_and_WCP_2 ,  
#                                                                                                   #   N_chunks == "1" ,
#                                                                                                   #   device == "HPC"
# ), 
# mapping = aes(x = N_cores,
#               # y =  Adj_scaling_ratio,
#               y =  time_avg_adj_for_Mplus_WCP_vs_standard,
#               colour = N_label,
#               #  linetype = Algorithm
# )
# ) + 
#   geom_point(size = 4) +
#   # geom_line(linewidth = 1,           aes(x = N_cores,
#   #                                        y =  Adj_scaling_ratio,
#   #                                        colour = N_label,
#   #                                        linetype = device
#   # )) +
#   geom_line(linewidth = 1,   linetype = "dashed",  colour = "black",  aes(x = N_cores,
#                                                                           #   y =  (N_cores / time_perfect_scaling) * time_perfect_scaling )) +
#                                                                           y =   time_perfect_adj_for_Mplus_WCP_vs_standard)) +
#   geom_line(linewidth = 2,          aes(x = N_cores,
#                                         #  y =  time_perfect_scaling * N_cores / time_avg)) +
#                                         y =   time_avg_adj_for_Mplus_WCP_vs_standard)) + 
#   theme_bw(base_size = 20) +
#   theme(legend.position = "bottom")  +
#   ylab("Adj. ratio (N_{cores} / sec) * min[time]") +
#   xlab("N_{cores}" ) +
#   scale_x_continuous(breaks = c(2, 4, 6, 8, 16, 32, 64, 96)) + 
#   facet_wrap( ~ device + Algorithm + N,
#               scales = "free",
#   )
# 
# plot_N_scalability_plot_HPC_and_laptop_Mplus_WCP_vs_standard_plot_1
# 
# 
# 
# 
# {
#   plot_scale_factor <- 1
#   plot_width <-  16*plot_scale_factor
#   plot_height <- 1*9*plot_scale_factor
# }
# 
# 
# plot_name <- paste0("plot_N_scalability_plot_HPC_and_laptop_Mplus_WCP_vs_standard_plot_1")
# png(plot_name ,units = "in", width = plot_width, height=plot_height, res=800)
# plot_N_scalability_plot_HPC_and_laptop_Mplus_WCP_vs_standard_plot_1
# dev.off()
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
# 
# 
# 
# # 
# # 
# # 
# # # stats for table showing best N_{chunks} for each N - for both HPC and laptop
# # 
# # 
# # 
# # N_scalability_pilot_study_df_best_HPC <- N_scalability_pilot_study_df_HPC_and_Laptop %>%
# #   group_by(device, N)  %>%
# #   mutate(best_ind = ifelse(time_avg == min(time_avg), 1, 0)) %>% 
# #   dplyr::filter(best_ind == 1, device == "HPC") %>% 
# #   print(n = 1000)
# # 
# # 
# # 
# # N_scalability_pilot_study_df_best_Laptop <- N_scalability_pilot_study_df_HPC_and_Laptop %>%
# #   group_by(device, N)  %>%
# #   mutate(best_ind = ifelse(time_avg == min(time_avg), 1, 0)) %>% 
# #   dplyr::filter(best_ind == 1, device == "Laptop") %>% 
# #   print(n = 1000)
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# 
# 
# 
# 
# 
# 
# # End of  "Pilot study - Parallel scalability for: (1) BayesMVP (for various chunks); (2) Mplus ; and (3) Stan (using C++ AD function)"     ------------------------------------------------------------------------------------------------------------
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
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # num_chunks <- 10 ; num_chunks
# # chunk_size <- round(N / num_chunks) ; chunk_size
# # 
# # chunk_counter = 0
# # 
# # for (nc in 0:(num_chunks - 1)) {
# #   
# #   for (t in 0:(n_tests - 1)) {
# #     
# #     if (t == 0) {  
# #       if (chunk_counter == 0) {
# #         print(paste("chunk_counter = 0, t = 0:",  
# #                     "theta from:", 0,   "length",n_tests * chunk_size, 
# #                     "then given this, from" , 0, "length", chunk_size))
# #       } else       {
# #         print(paste("chunk_counter > 0, t = 0:",  
# #                     "theta from:", (n_tests * chunk_size * chunk_counter) - 1,   "length",n_tests * chunk_size, 
# #                     "then given this, from" , 0, "length", chunk_size))
# #       }
# #     } else { 
# #       if (chunk_counter == 0) {
# #         print(paste("chunk_counter = 0, t > 0:",  
# #                     "theta from:", 0,   "length",n_tests * chunk_size, 
# #                     "then given this, from" , (t * chunk_size) - 1, "length", chunk_size))
# #       } else       {
# #         print(paste("chunk_counter > 0, t > 0:",  
# #                     "theta from:", (n_tests * chunk_size * chunk_counter) - 1,   "length",n_tests * chunk_size, 
# #                     "then given this, from" , (t * chunk_size) - 1, "length", chunk_size))
# #       }
# #     }
# #     
# #     
# #   }
# #   
# #   chunk_counter = chunk_counter + 1
# #   
# # }
# # 
# # plot(cores, cores / times)
# # points(cores, cores / times, co = "blue" )                         
# # # ----------  grad - both u and main (using STAN AD grad_fn)
# # par(mfrow = c(2, 2))
# # 
# # 
# # # use "dput" 
# # dput(times) 
# # 
# # 
# # 
# # times_N_64000_ad <-  c(43.049, 46.076, 53.425, 74.123, 106.226, 145.479, 180.485)         #  Zen 4, 12 DIMM's
# # times_N_64000_ideal_ad <- rep(times_N_64000_ad[1],  length(cores))
# # 
# # 
# # times_N_16000_ad <-  c(38.694, 41.037, 50.804, 67.304, 92.734, 134.817, 166.817)       #  Zen 4, 12 DIMM's
# # times_N_16000_ideal_ad <- rep(times_N_16000_ad[1],  length(cores))
# # 
# # 
# # times_N_4000_ad <-  c(36.795, 39.98, 47.705, 66.24, 89.351, 131.202, 164.311)        #  Zen 4, 12 DIMM's
# # times_N_4000_ideal_ad <- rep(times_N_4000_ad[1],  length(cores))
# # 
# # 
# # times_N_1000_ad <-  c(33.986, 38.708, 40.123, 49.594, 75.616, 119.415, 152.732)          #  Zen 4, 12 DIMM's
# # times_N_1000_ideal_ad <- rep(times_N_1000_ad[1],  length(cores))
# # 
# # 
# # 
# # div <- cores/4
# # 
# # 
# # # plot(x = cores, y = 1/(times_N_1000_ideal/div), col = "green",  main = "N = 1000, AD", type = "l")
# # # points(x = cores, y = 1/(times_N_1000_ad/div))
# # # lines(x = cores, y = 1/(times_N_1000_ad/div))
# # 
# # 
# # 
# # 
# # # ----------  grad - both u and main (using MANUAL grad_fn) - FULLY VECTORISED  (i.e. chunking with 1 chunk)
# # # use "d" 
# # dput(times)
# # 
# # 
# # 
# # 
# # times_N_64000_vec <-  c(9.708, 10.612, 12.048, 14.938, 21.392, 35.894, 50.145)       #  Zen 4, 12 DIMM's
# # times_N_64000_ideal_vec <- rep(times_N_64000_vec[1],  length(cores))
# # 
# # 
# # times_N_16000_vec <-   c(8.63, 9.618, 10, 11.818, 15.797, 20.467, 25.381)        #  Zen 4, 12 DIMM's ,  -g -O3  -march=znver3   -mtune=native   -fPIC  $(LTO) #  
# # times_N_16000_ideal_vec <- rep(times_N_16000_vec[1],  length(cores))
# # 
# # 
# # times_N_4000_vec <-   c(8.305, 8.52, 8.419, 9.268, 13.454, 16.693, 19.745)        #  Zen 4, 12 DIMM's
# # times_N_4000_ideal_vec <- rep(times_N_4000_vec[1],  length(cores))
# # 
# # 
# # times_N_1000_vec <-  c(7.964, 8.371, 8.798, 8.924, 12.074, 14.559, 16.515)        #  Zen 4, 12 DIMM's
# # times_N_1000_ideal_vec <- rep(times_N_1000_vec[1],  length(cores))
# # 
# # div <- cores/4
# # 
# # 
# # # ----------  grad - both u and main (using MANUAL grad_fn) - VECTORISED + CHUNKING 
# # # use "d" 
# # dput(times)
# # 
# # #  dput_temp_1 <-  dput(times)
# # #  dput_temp_2 <-  dput(times)
# # 
# # 
# # 
# # times_N_64000_vec_chunk <-   c(8.213, 8.703, 9.428, 10.921, 14.241, 18.426, 20.853)       #  Zen 4, 12 DIMM's
# # times_N_64000_ideal_vec_chunk <- rep(times_N_64000_vec_chunk[1],  length(cores))
# # 
# # 
# # times_N_16000_vec_chunk <-   c(7.672,   8.1, 8.615, 9.414, 12.379, 15.261, 17.252)         #  Zen 4, 12 DIMM's,   -g -O3  -march=znver3   -mtune=native   -fPIC  $(LTO) # 
# # times_N_16000_ideal_vec_chunk <- rep(times_N_16000_vec_chunk[1],  length(cores))
# # 
# # times_N_4000_vec_chunk <-    c(7.987, 8.198, 8.988, 8.626, 12.075, 14.385, 16.281)        #  Zen 4, 12 DIMM's
# # times_N_4000_ideal_vec_chunk <- rep(times_N_4000_vec_chunk[1],  length(cores))
# # 
# # times_N_1000_vec_chunk <-   c(7.964, 8.371, 8.798, 8.924, 12.074, 14.559, 16.515)          #  Zen 4, 12 DIMM's
# # times_N_1000_ideal_vec_chunk <- rep(times_N_1000_vec_chunk[1],  length(cores))
# # 
# # 
# # 
# # 
# # 
# # # ----------  grad - both u and main (using MANUAL grad_fn) - NON-VECTORISED
# # # use "d"
# # dput(times)
# # 
# # 
# # times_N_64000_nonvec <-  c(25.124, 27.738, 27.881, 29.218, 37.97, 46.958, 56.494)       #  Zen 4, 12 DIMM's
# # times_N_64000_ideal_nonvec <- rep(times_N_64000_nonvec[1],  length(cores))
# # 
# # 
# # times_N_16000_nonvec <-  c(22.364, 23.353, 23.953, 24.156, 32.556, 37.797, 43.012)        #  Zen 4, 12 DIMM's,   -g -O3  -march=znver3   -mtune=native   -fPIC  $(LTO) #
# # times_N_16000_ideal_nonvec <- rep(times_N_16000_nonvec[1],  length(cores))
# # 
# # times_N_4000_nonvec <-   c(22.067, 22.684, 22.532, 23.551, 31.319, 35.992, 39.799)      #  Zen 4, 12 DIMM's
# # times_N_4000_ideal_nonvec <- rep(times_N_4000_nonvec[1],  length(cores))
# # 
# # times_N_1000_nonvec <-   c(21.964, 22.415, 22.866, 23.033, 30.497, 35.669, 38.824)          #  Zen 4, 12 DIMM's
# # times_N_1000_ideal_nonvec <- rep(times_N_1000_nonvec[1],  length(cores))
# # # 
# # 
# # 
# # 
# # 
# # 
# # #  --------- make general df 
# # n_dfs <- 4
# # n_functions <- 4
# # df_selecting_n_chains <- tibble(n_cores  = rep(rep(cores, n_functions), n_dfs), 
# #                                 function_type = rep( c( rep("AD-NON-VEC", length(cores)) ,        rep("MD-VEC", length(cores)) ,   rep("MD-VEC-CHUNK", length(cores))  ,  rep("MD-NON-VEC", length(cores)) ), n_dfs), 
# #                                 df_size = (c( rep(1000, n_functions * length(cores)), rep(4000, n_functions * length(cores)), rep(16000, n_functions * length(cores)), rep(64000, n_functions * length(cores)) )), 
# #                                 times = c(  c(times_N_1000_ad, times_N_1000_vec, times_N_1000_vec_chunk, times_N_1000_nonvec) , 
# #                                             c(times_N_4000_ad, times_N_4000_vec, times_N_4000_vec_chunk, times_N_4000_nonvec) ,
# #                                             c(times_N_16000_ad, times_N_16000_vec, times_N_16000_vec_chunk, times_N_16000_nonvec) ,
# #                                             c(times_N_64000_ad, times_N_64000_vec, times_N_64000_vec_chunk, times_N_64000_nonvec)  ) , 
# #                                 ideal_times = c(  c(times_N_1000_ideal_ad, times_N_1000_ideal_vec,   times_N_1000_ideal_vec_chunk, times_N_1000_ideal_nonvec) , 
# #                                                   c(times_N_4000_ideal_ad, times_N_4000_ideal_vec,   times_N_4000_ideal_vec_chunk, times_N_4000_ideal_nonvec) ,
# #                                                   c(times_N_16000_ideal_ad, times_N_16000_ideal_vec, times_N_16000_ideal_vec_chunk, times_N_16000_ideal_nonvec) ,
# #                                                   c(times_N_64000_ideal_ad, times_N_64000_ideal_vec, times_N_64000_ideal_vec_chunk, times_N_64000_ideal_nonvec)  ) )
# # 
# # # #  --------- make general df ( just VEC vs NON-VEC for MD )
# # # n_dfs <- 6
# # # n_functions <-  2
# # # df_selecting_n_chains <- tibble(n_cores  = rep(rep(cores, n_functions), n_dfs), 
# # #                                 function_type =     rep( c(   rep("MD-VEC", length(cores)) ,  rep("MD-NON-VEC", length(cores)) ), n_dfs), 
# # #                                 df_size = (c( rep(1000, n_functions * length(cores)), rep(4000, n_functions * length(cores)), rep(16000, n_functions * length(cores)), rep(64000, n_functions * length(cores)),  rep(128000, n_functions * length(cores)),  rep(256000, n_functions * length(cores))  )), 
# # #                                 times = c(  c( times_N_1000_vec, times_N_1000_nonvec) , 
# # #                                             c( times_N_4000_vec, times_N_4000_nonvec) ,
# # #                                             c( times_N_16000_vec, times_N_16000_nonvec) ,
# # #                                             c( times_N_64000_vec, times_N_64000_nonvec)  , 
# # #                                             c( times_N_128000_vec, times_N_128000_nonvec)  , 
# # #                                             c( times_N_256000_vec, times_N_256000_nonvec)  ) )
# # 
# # 
# # 
# # df_selecting_n_chains <- df_selecting_n_chains %>% mutate(div = n_cores/4,
# #                                                           # eff = 1/(times/div)
# #                                                           eff = n_cores / times,
# #                                                           scaling_ratio = n_cores /  ( times/ ideal_times) )
# # 
# # 
# # require(ggplot2)
# # 
# # #  df_selecting_n_chains <- filter(df_selecting_n_chains, function_type != "AD-NON-VEC")
# # 
# # # ---------  plot trends 
# # 
# # ggplot(data = df_selecting_n_chains, aes(x = n_cores, y = times, colour = function_type))  + 
# #   geom_line() + 
# #   geom_point() + 
# #   facet_wrap(~ df_size, scales = "free")  + 
# #   # facet_wrap(~ df_size)  +
# #   theme_bw() + 
# #   ylab("Time (adjusted for N)") + 
# #   xlab("Number of parallel cores (total = 64 cores / 128 threads)")
# # 
# # 
# # 
# # 
# # 
# # df_selecting_n_chains_2 <- dplyr::filter(df_selecting_n_chains, function_type != "AD-NON-VEC")
# # 
# # 
# # #  tiff("Figure_1_MD_vec_nonvec_chunk_efficiency.tif" ,units = "in", width = 10, height=8, res=800, compression = "lzw")
# # png("Figure_1_MD_vec_nonvec_chunk_efficiency.png" ,units = "in", width = 10, height=8, res=800)
# # 
# # ggplot(data = df_selecting_n_chains_2, aes(x = n_cores, y = eff, colour = function_type))  + 
# #   geom_line(size = 1.0) + 
# #   geom_point(size = 3.5) + 
# #   facet_wrap(~ df_size, scales = "free")  + 
# #   #facet_wrap(~ df_size)  +
# #   theme_bw() + 
# #   ylab("Efficiency ( #threads / time - adjusted for N)") + 
# #   xlab("Number of parallel cores (total = 64 cores / 128 threads)") + 
# #   theme_bw(base_size = 20) + 
# #   guides(color=guide_legend(title="Function type")) 
# # 
# # 
# # dev.off()
# # 
# # 
# # df_selecting_n_chains_3 <- dplyr::filter(df_selecting_n_chains, function_type != "MD-VEC", function_type != "MD-NON-VEC")
# # 
# # 
# # #  tiff("Figure_1_MD_vec_nonvec_chunk_efficiency.tif" ,units = "in", width = 10, height=8, res=800, compression = "lzw")
# # png("Figure_1_AD_vs_MD_chunk_efficiency_scaling_ratio.png" ,units = "in", width = 10, height=8, res=800)
# # 
# # g_1 <- ggplot(data = df_selecting_n_chains_3, aes(x = n_cores, y = scaling_ratio, colour = function_type))  + 
# #   geom_line(size = 1.0) + 
# #   geom_point(size = 3.5) + 
# #   facet_wrap(~ df_size, scales = "free")  + 
# #   #facet_wrap(~ df_size)  +
# #   theme_bw() + 
# #   #  ylab("Efficiency ( #threads / time - adjusted for N)") + 
# #   ylab(" Scaling ratio (higher is better) ") + 
# #   xlab("Number of parallel cores (total = 64 cores / 128 threads)") + 
# #   theme_bw(base_size = 20) + 
# #   guides(color=guide_legend(title="Function type")) 
# # g_1
# # 
# # dev.off()
# # 
# # png("Figure_1_AD_vs_MD_chunk_efficiency.png" ,units = "in", width = 10, height=8, res=800)
# # 
# # g_2 <- ggplot(data = df_selecting_n_chains_3, aes(x = n_cores, y = eff, colour = function_type))  + 
# #   geom_line(size = 1.0) + 
# #   geom_point(size = 3.5) + 
# #   facet_wrap(~ df_size, scales = "free")  + 
# #   #facet_wrap(~ df_size)  +
# #   theme_bw() + 
# #   ylab("Efficiency ( #threads / time - adjusted for N)") + 
# #   # ylab(" Scaling ratio (higher is better) ") + 
# #   xlab("Number of parallel cores (total = 64 cores / 128 threads)") + 
# #   theme_bw(base_size = 20) + 
# #   guides(color=guide_legend(title="Function type")) 
# # g_2
# # 
# # dev.off()
# # 
# # 
# # require(patchwork)
# # 
# # png("Figure_1_AD_vs_MD_chunk_efficiency_efficiency_and_scaling.png" ,units = "in", width = 18, height=8, res=800)
# # 
# # g_1 +   theme(legend.position="none") + g_2
# # 
# # dev.off()
# # 
# # # # # ----------   lp-only - using VECTORISED grad_fn - w/ only the containers needed to calc. lp (not grad containers)  
# # # 
# # # # use "d" 
# # # dput(times)
# # # 
# # # 
# # # # times_N_1024000 <-  c(                                 )   ; times_N_1024000[5]  / times_N_1024000[1]
# # # # times_N_1024000_ideal <- rep(times_N_1024000[1],  8)
# # # 
# # # # times_N_256000 <-  c(                             )   ; times_N_256000[5]  / times_N_256000[1]
# # # # times_N_256000_ideal <- rep(times_N_256000[1],  8)
# # # 
# # # 
# # # times_N_64000 <- c(      3.508, 3.595, 4.054, 4.714, 5.425, 6.369, 7.302, 8.562             )   ; times_N_64000[5]  / times_N_64000[1]      #  Zen 4, 12 DIMM's
# # # times_N_64000_ideal <- rep(times_N_64000[1],  8)
# # # 
# # # 
# # # times_N_16000 <- c(     2.216, 2.258, 2.48, 2.636, 2.928, 3.238, 3.833, 4.607           )   ; times_N_16000[5]  / times_N_16000[1]      #  Zen 4, 12 DIMM's
# # # times_N_16000_ideal <- rep(times_N_16000[1],  8)
# # # 
# # # 
# # # times_N_4000 <- c(      1.929, 1.958, 2.223, 2.355, 2.766, 2.616, 3.5, 3.618          )   ; times_N_4000[5]  / times_N_4000[1]      #  Zen 4, 12 DIMM's
# # # times_N_4000_ideal <- rep(times_N_4000[1],  8)
# # # 
# # # 
# # # times_N_1000 <- c(         1.729, 1.75, 2.173, 2.27, 2.045, 2.223, 2.431, 2.395          )   ; times_N_1000[5]  / times_N_1000[1]      #  Zen 4, 12 DIMM's
# # # times_N_1000_ideal <- rep(times_N_1000[1],  8)
# # # 
# # # cores <- c(4, 8, 16, 24, 32, 40, 48, 56)
# # # div <- cores/4
# # # 
# # # 
# # # 
# # # plot(x = cores, y = 1/(times_N_1000_ideal/div), col = "green",  main = "N = 1000, MD, lp-only, vectorised", type = "l")
# # # points(x = cores, y = 1/(times_N_1000/div))
# # # lines(x = cores, y = 1/(times_N_1000/div))
# # # 
# # # plot(x = cores, y = 1/(times_N_4000_ideal/div), col = "green",  main = "N = 4000, MD, lp-only, vectorised", type = "l")
# # # points(x = cores, y = 1/(times_N_4000/div))
# # # lines(x = cores, y = 1/(times_N_4000/div))
# # # 
# # # 
# # # plot(x = cores, y = 1/(times_N_16000_ideal/div), col = "green",  main = "N = 16,000, MD, lp-only, vectorised", type = "l")
# # # points(x = cores, y = 1/(times_N_16000/div))
# # # lines(x = cores, y = 1/(times_N_16000/div))
# # # 
# # # plot(x = cores, y = 1/(times_N_64000_ideal/div), col = "green",  main = "N = 64,000, MD, lp-only, vectorised", type = "l")
# # # points(x = cores, y = 1/(times_N_64000/div))
# # # lines(x = cores, y = 1/(times_N_64000/div))
# # # 
# # # # 
# # # # plot(x = cores, y = 1/(times_N_256000_ideal/div), col = "green",  main = "N = 256,000, MD, lp-only, vectorised", type = "l")
# # # # points(x = cores, y = 1/(times_N_256000/div))
# # # # lines(x = cores, y = 1/(times_N_256000/div))
# # # # 
# # # # plot(x = cores, y = 1/(times_N_1024000_ideal/div), col = "green",  main = "N = 1 million, MD, lp-only, vectorised", type = "l")
# # # # points(x = cores, y = 1/(times_N_1024000/div))
# # # # lines(x = cores, y = 1/(times_N_1024000/div))
# # # 
# # # 
# # # 
# # # # # ----------   lp-only - using NON-VECTORISED grad_fn - w/ only the containers needed to calc. lp (not grad containers)  
# # # 
# # # # use "d" 
# # # dput(times)
# # # 
# # # 
# # # # times_N_1024000 <-  c(                                 )   ; times_N_1024000[5]  / times_N_1024000[1]
# # # # times_N_1024000_ideal <- rep(times_N_1024000[1],  8)
# # # #
# # # # times_N_256000 <-  c(                            )   ; times_N_256000[5]  / times_N_256000[1]
# # # # times_N_256000_ideal <- rep(times_N_256000[1],  8)
# # # 
# # # 
# # # times_N_64000 <- c(        2.403, 2.489, 2.894, 3.055, 3.335, 3.74, 4.137, 4.736               )   ; times_N_64000[5]  / times_N_64000[1]      #  Zen 4, 12 DIMM's
# # # times_N_64000_ideal <- rep(times_N_64000[1],  8)
# # # 
# # # 
# # # times_N_16000 <- c(      1.931, 1.976, 2.27, 2.42, 2.448, 2.591, 3.05, 3.129              )   ; times_N_16000[5]  / times_N_16000[1]      #  Zen 4, 12 DIMM's
# # # times_N_16000_ideal <- rep(times_N_16000[1],  8)
# # # 
# # # 
# # # times_N_4000 <- c(        1.842, 1.896, 2.183, 2.296, 2.031, 2.363, 2.748, 2.835                 )   ; times_N_4000[5]  / times_N_4000[1]      #  Zen 4, 12 DIMM's
# # # times_N_4000_ideal <- rep(times_N_4000[1],  8)
# # # 
# # # 
# # # times_N_1000 <- c(         1.87, 1.967, 2.135, 2.294, 2.493, 2.479, 2.624, 2.725                   )   ; times_N_1000[5]  / times_N_1000[1]      #  Zen 4, 12 DIMM's
# # # times_N_1000_ideal <- rep(times_N_1000[1],  8)
# # # 
# # # cores <- c(4, 8, 16, 24, 32, 40, 48, 56)
# # # div <- cores/4
# # # 
# # # 
# # # 
# # # plot(x = cores, y = 1/(times_N_1000_ideal/div), col = "green",  main = "N = 1000, MD, lp-only, NON-vectorised", type = "l")
# # # points(x = cores, y = 1/(times_N_1000/div))
# # # lines(x = cores, y = 1/(times_N_1000/div))
# # # 
# # # plot(x = cores, y = 1/(times_N_4000_ideal/div), col = "green",  main = "N = 4000, MD, lp-only, NON-vectorised", type = "l")
# # # points(x = cores, y = 1/(times_N_4000/div))
# # # lines(x = cores, y = 1/(times_N_4000/div))
# # # 
# # # 
# # # plot(x = cores, y = 1/(times_N_16000_ideal/div), col = "green",  main = "N = 16,000, MD, lp-only, NON-vectorised", type = "l")
# # # points(x = cores, y = 1/(times_N_16000/div))
# # # lines(x = cores, y = 1/(times_N_16000/div))
# # # 
# # # plot(x = cores, y = 1/(times_N_64000_ideal/div), col = "green",  main = "N = 64,000, MD, lp-only, NON-vectorised", type = "l")
# # # points(x = cores, y = 1/(times_N_64000/div))
# # # lines(x = cores, y = 1/(times_N_64000/div))
# # # 
# # # # 
# # # # plot(x = cores, y = 1/(times_N_256000_ideal/div), col = "green",  main = "N = 256,000, MD, lp-only, NON-vectorised", type = "l")
# # # # points(x = cores, y = 1/(times_N_256000/div))
# # # # lines(x = cores, y = 1/(times_N_256000/div))
# # # # 
# # # # plot(x = cores, y = 1/(times_N_1024000_ideal/div), col = "green",  main = "N = 1 million, MD, lp-only, vectorised", type = "l")
# # # # points(x = cores, y = 1/(times_N_1024000/div))
# # # # lines(x = cores, y = 1/(times_N_1024000/div))
# # # 
# # 
# # 
# # 
# # par(mfrow = c(2,2))
# # 
# # times_N_1000 <- c(         1.729, 1.75, 2.173, 2.27, 2.045, 2.223, 2.431, 2.395          )   ; times_N_1000[5]  / times_N_1000[8]      #  Zen 4, 12 DIMM's
# # times_N_1000_ideal <- rep(times_N_1000[1],  8)
# # plot(x = cores, y = 1/(times_N_1000_ideal/div), col = "green",  main = "N = 1000, MD, lp-only, NON-vectorised = BLUE", type = "l")
# # points(x = cores, y = 1/(times_N_1000/div), col = "green" )
# # lines(x = cores, y = 1/(times_N_1000/div), col = "green")
# # 
# # times_N_1000 <- c(         1.87, 1.967, 2.135, 2.294, 2.493, 2.479, 2.624, 2.725                   )   ; times_N_1000[5]  / times_N_1000[8]      #  Zen 4, 12 DIMM's - NON-VEC
# # times_N_1000_ideal <- rep(times_N_1000[1],  8)
# # lines(x = cores, y = 1/(times_N_1000_ideal/div), col = "blue",  main = "N = 1000, MD, lp-only, vectorised = BLUE ", type = "l")
# # points(x = cores, y = 1/(times_N_1000/div) ,  col = "blue" )
# # lines(x = cores, y = 1/(times_N_1000/div),  col = "blue")
# # 
# # 
# # 
# # 
# # times_N_4000 <- c(      1.929, 1.958, 2.223, 2.355, 2.766, 2.616, 3.5, 3.618          )   ; times_N_4000[8]  / times_N_4000[1]      #  Zen 4, 12 DIMM's
# # times_N_4000_ideal <- rep(times_N_4000[1],  8)
# # plot(x = cores, y = 1/(times_N_4000_ideal/div), col = "green",  main = "N = 4000, MD, lp-only, NON-vectorised = BLUE ", type = "l")
# # points(x = cores, y = 1/(times_N_4000/div), col = "green")
# # lines(x = cores, y = 1/(times_N_4000/div), col = "green")
# # 
# # times_N_4000 <- c(        1.842, 1.896, 2.183, 2.296, 2.031, 2.363, 2.748, 2.835                 )   ; times_N_4000[8]  / times_N_4000[1]      #  Zen 4, 12 DIMM's - NON-VEC
# # times_N_4000_ideal <- rep(times_N_4000[1],  8)
# # lines(x = cores, y = 1/(times_N_4000_ideal/div), col = "blue",  main = "N = 4000, MD, lp-only, vectorised", type = "l")
# # points(x = cores, y = 1/(times_N_4000/div) ,  col = "blue"  )
# # lines(x = cores, y = 1/(times_N_4000/div) ,  col = "blue" )
# # 
# # 
# # 
# # 
# # times_N_16000 <- c(     2.216, 2.258, 2.48, 2.636, 2.928, 3.238, 3.833, 4.607           )   ; times_N_16000[8]  / times_N_16000[1]      #  Zen 4, 12 DIMM's
# # times_N_16000_ideal <- rep(times_N_16000[1],  8)
# # plot(x = cores, y = 1/(times_N_16000_ideal/div), col = "green",  main = "N = 16,000, MD, lp-only, NON-vectorised = BLUE ", type = "l")
# # points(x = cores, y = 1/(times_N_16000/div), col = "green")
# # lines(x = cores, y = 1/(times_N_16000/div), col = "green")
# # 
# # times_N_16000 <- c(      1.931, 1.976, 2.27, 2.42, 2.448, 2.591, 3.05, 3.129              )   ; times_N_16000[8]  / times_N_16000[1]      #  Zen 4, 12 DIMM's - NON-VEC
# # times_N_16000_ideal <- rep(times_N_16000[1],  8)
# # lines(x = cores, y = 1/(times_N_16000_ideal/div), col = "blue",  main = "N = 16,000, MD, lp-only, vectorised = BLUE ", type = "l")
# # points(x = cores, y = 1/(times_N_16000/div) ,  col = "blue" )
# # lines(x = cores, y = 1/(times_N_16000/div) , col = "blue" )
# # 
# # 
# # 
# # 
# # 
# # times_N_64000 <- c(      3.508, 3.595, 4.054, 4.714, 5.425, 6.369, 7.302, 8.562             )   ; times_N_64000[8]  / times_N_64000[1]      #  Zen 4, 12 DIMM's
# # times_N_64000_ideal <- rep(times_N_64000[1],  8)
# # plot(x = cores, y = 1/(times_N_64000_ideal/div), col = "green",  main = "N = 64,000, MD, lp-only, NON-vectorised = BLUE ", type = "l")
# # points(x = cores, y = 1/(times_N_64000/div), col = "green")
# # lines(x = cores, y = 1/(times_N_64000/div), col = "green")
# # 
# # times_N_64000 <- c(        2.403, 2.489, 2.894, 3.055, 3.335, 3.74, 4.137, 4.736               )   ; times_N_64000[8]  / times_N_64000[1]      #  Zen 4, 12 DIMM's   - NON-VEC
# # times_N_64000_ideal <- rep(times_N_64000[1],  8)
# # lines(x = cores, y = 1/(times_N_64000_ideal/div), col = "blue",  main = "N = 64,000, MD, lp-only, vectorised", type = "l")
# # points(x = cores, y = 1/(times_N_64000/div) ,  col = "blue" )
# # lines(x = cores, y = 1/(times_N_64000/div), col = "blue")
# # 
# # 
# # 
# # 
# # {
# #   
# #   fn_version = 1
# #   
# #   tic()
# #   
# #   
# #   outs_manual_grad <-   lcmMVPbetav3::fn_log_posterior_full_binary_only_GHK_manual_diff_loop_test(theta =  theta_vec,
# #                                                                                                   y = y,
# #                                                                                                   lp_and_grad_args,
# #                                                                                                   version = fn_version,
# #                                                                                                   n_iter = 50)
# #   
# #   
# #   toc()
# #   
# # }
# # 
# # #      lcmMVPbetav3::
# # 
# # 
# # 
# # 
# # {
# #   
# #   
# #   tic()
# #   
# #   
# #   fn_log_posterior_full_binary_only_GHK_manual_diff_AF_loop_test(theta =  theta_vec,
# #                                                                  y = y,
# #                                                                  lp_and_grad_args, 
# #                                                                  n_iter = 10)
# #   
# #   
# #   
# #   # for (i in 1:500)
# #   #   # lcmMVPbetav3::fn_log_posterior_full_binary_only_GHK_manual_diff_stan
# #   #   # fn_log_posterior_full_binary_only_GHK_manual_diff_AF
# #   #   #  fn_log_posterior_full_binary_only_GHK_manual_diff_stan
# #   #   outs_manual_grad <-     fn_log_posterior_full_binary_only_GHK_manual_diff_AF(theta =  theta_vec,
# #   #                                                                               exclude_priors = FALSE,
# #   #                                                                               lkj_prior_method = 2,
# #   #                                                                               #  exclude_priors =FALSE,
# #   #                                                                               # grad_main = FALSE,
# #   #                                                                               grad_main = TRUE,
# #   #                                                                               grad_nuisance = TRUE,
# #   #                                                                               #grad_nuisance = FALSE,
# #   #                                                                               CI = CI,
# #   #                                                                               homog_corr = homog_corr,
# #   #                                                                               rough_approx = rough_approx,
# #   #                                                                               lkj_cholesky_eta = prior_lkj,
# #   #                                                                               lkj_cholesky = mvr_cholesky,
# #   #                                                                               prior_coeffs_mean = prior_mean_vec,
# #   #                                                                               prior_coeffs_sd = prior_sd_vec,
# #   #                                                                               #    class_ind = class_ind - 1,
# #   #                                                                               n_class = n_class,
# #   #                                                                               n_tests = n_tests,
# #   #                                                                                   X = X,
# #   #                                                                               y = y)
# #   
# #   toc()
# #   
# # }
# # 
# # 
# # 
# # tail(outs_manual_grad[,1], 50)
# # outs_manual_grad[1,2]
# # 
# # 
# # u_array = outs_manual_grad[[1]]   ;   u_array
# # prob_1 = outs_manual_grad[[2]]   ;   prob_1
# # Z_std_norm = outs_manual_grad[[3]]   ;   Z_std_norm
# # Phi_Z =outs_manual_grad[[4]]   ;   Phi_Z
# # Bound_Z =outs_manual_grad[[5]]   ;   Bound_Z
# # Bound_U_Bound_Phi_Z_1 =outs_manual_grad[[6]]   ;   Bound_U_Bound_Phi_Z_1
# # prob_n_1 =outs_manual_grad[[7]]   ;   prob_n_1
# # log_posterior_1 =outs_manual_grad[[8]]   ;   log_posterior_1 ; sum(log_posterior_1)
# # 
# # inc_array =outs_manual_grad[[10]]   ;   inc_array
# # y1_array =outs_manual_grad[[11]]   ;   y1_array
# # lp_array =outs_manual_grad[[9]]   ;   lp_array
# # af_prev =outs_manual_grad[[12]]   ;   af_prev
# # log_prev =outs_manual_grad[[13]]    ;   log_prev
# # y_sign =  outs_manual_grad[[14]]    ;   y_sign
# # u_grad_1 = outs_manual_grad[[15]]    ;   u_grad_1
# # beta_grad_1 = outs_manual_grad[[16]]    ;   beta_grad_1
# # 
# # grad_z_mat_1 =    outs_manual_grad[[17]]    ;   grad_z_mat_1
# # grad_term_mat_1 =    outs_manual_grad[[18]]    ;   grad_term_mat_1
# # derivs_chain_container_vec_array_1  =    outs_manual_grad[[19]]    ;   derivs_chain_container_vec_array_1
# # common_grad_term_1_1 =    outs_manual_grad[[20]]    ;   common_grad_term_1_1
# # 
# # {
# #   n_params <- N * n_tests * n_class + 43
# #   theta_vec <- rnorm(n = n_params, mean = 0, sd = 0.05)
# #   homog_corr <- F
# #   rough_approx <- T
# #   prior_lkj <- 6
# #   mvr_cholesky <- T
# #   n_us <- - N * n_tests * n_class
# # }
# # 
# # 
# # 
# # 
# # 
# # 
# # {
# #   
# #   
# #   tic()
# #   for (i in 1:50)  
# #     # lcmMVPbetav3::fn_log_posterior_full_binary_only_GHK_manual_diff_stan
# #     # fn_log_posterior_full_binary_only_GHK_manual_diff_AF
# #     #  fn_log_posterior_full_binary_only_GHK_manual_diff_stan
# #     outs_manual_grad_AF <-     fn_log_posterior_full_binary_only_GHK_manual_diff_AF(theta =  theta_vec,
# #                                                                                     exclude_priors = FALSE,
# #                                                                                     lkj_prior_method = 2,
# #                                                                                     grad_main = TRUE,
# #                                                                                     grad_nuisance = TRUE,
# #                                                                                     CI = CI,
# #                                                                                     homog_corr = homog_corr,
# #                                                                                     rough_approx = rough_approx,
# #                                                                                     lkj_cholesky_eta = prior_lkj,
# #                                                                                     lkj_cholesky = mvr_cholesky,
# #                                                                                     prior_coeffs_mean = prior_mean_vec,
# #                                                                                     prior_coeffs_sd = prior_sd_vec,
# #                                                                                     n_class = n_class,
# #                                                                                     n_tests = n_tests,
# #                                                                                     X = X,
# #                                                                                     y = y
# #                                                                                     #    Eigen_y = y
# #     )
# #   toc()
# # }
# # 
# # 
# # tail(outs_manual_grad_AF[,1], 50)
# # outs_manual_grad_AF[1,2]
# # 
# # outs_manual_grad_AF[[3]] 
# # outs_manual_grad_AF[[4]] 
# # outs_manual_grad_AF[[5]] 
# # outs_manual_grad_AF[[6]]
# # outs_manual_grad_AF[[7]]
# # 
# # outs_manual_grad_AF[[4]]  +      outs_manual_grad_AF[[1]][2,2]
# # 
# # outs_manual_grad_AF[[1]][2,1]
# # outs_manual_grad_AF[[1]][2,2]
# # 
# # u_array = outs_manual_grad_AF[[1]]   ;   u_array # good
# # prob = outs_manual_grad_AF[[2]]   ;   prob # good
# # Z_std_norm = outs_manual_grad_AF[[3]]   ;   Z_std_norm # good
# # Phi_Z =outs_manual_grad_AF[[4]]   ;   Phi_Z # good
# # Bound_Z =outs_manual_grad_AF[[5]]   ;   Bound_Z # good
# # Bound_U_Bound_Phi_Z =outs_manual_grad_AF[[6]]   ;   Bound_U_Bound_Phi_Z  # good
# # prob_n =outs_manual_grad_AF[[7]]   ;   prob_n  # good
# # log_posterior =outs_manual_grad_AF[[8]]   ;   log_posterior ; sum(log_posterior)
# # 
# # inc_array =outs_manual_grad_AF[[10]]   ;   inc_array
# # y1_array =outs_manual_grad_AF[[11]]   ;   y1_array
# # lp_array =outs_manual_grad_AF[[9]]   ;   lp_array
# # af_prev =outs_manual_grad_AF[[12]]   ;   af_prev
# # log_prev =outs_manual_grad_AF[[13]]    ;   log_prev
# # y_sign =  outs_manual_grad_AF[[14]]    ;   y_sign
# # u_grad = outs_manual_grad_AF[[15]]    ;   u_grad
# # beta_grad = outs_manual_grad_AF[[16]]    ;   beta_grad
# # 
# # Z_std_norm[[1]]  -  Z_std_norm_1[[1]]
# # Phi_Z[[1]]  - Phi_Z_1[[1]]
# # Bound_Z[[1]]  - Bound_Z_1[[1]] 
# # Bound_U_Bound_Phi_Z[[1]]  - Bound_U_Bound_Phi_Z_1[[1]]
# # prob_n   - prob_n_1
# # log_posterior[[1]]  - log_posterior_1[[1]]
# # 
# # round(u_grad - u_grad_1, 7)
# # 
# # grad_z_mat =    outs_manual_grad_AF[[17]]    ;   grad_z_mat
# # grad_term_mat =    outs_manual_grad_AF[[18]]    ;   grad_term_mat
# # derivs_chain_container_vec_array =    outs_manual_grad_AF[[19]]    ;   derivs_chain_container_vec_array
# # common_grad_term_1 =    outs_manual_grad_AF[[20]]    ;   common_grad_term_1
# # 
# # outs_manual_grad_AF[[21]] 
# # 
# # sum(prob[[1]] - prob_1[[1]])
# # sum(grad_z_mat - grad_z_mat_1)
# # sum(grad_term_mat - grad_term_mat_1)
# # sum(derivs_chain_container_vec_array - derivs_chain_container_vec_array_1)
# # 
# # common_grad_term_1_1[[1]] - common_grad_term_1[[1]]
# # 
# # sum( outs_manual_grad_AF[[2]]  )
# # 
# # 
# # c <- 2
# # t <- 6
# # i <- 2
# # 
# # beta_grad
# # beta_grad_1
# # sum(common_grad_term_1[[c]][, t] *   rowSums(  derivs_chain_container_vec_array  ) )
# # 
# # beta_grad_array(c, t) =        (  af::matmul( af::transpose( common_grad_term_1[c](af::span, t)  )   ,    af::sum(  fn_AF_block(  derivs_chain_container_vec_array, 0,  0, N, i + 2)  , 1) )  ) ; 
# # 
# # 
# # 
# # i = 0 
# # step = n_tests * n_class;
# # #   for (int t = 0; t < n_tests; t++) {
# # for (t in 0:(n_tests - 1)) {
# #   
# #   #   for (int c = 0; c < n_class; c++) {
# #   for (c in 0:(n_class - 1)) {
# #     print(i)
# #     end_index = n_tests * N * n_class - (n_tests * n_class - (i + 1))  - 1;
# #     print(end_index)
# #     
# #     print(  head(  seq(from = i, to = end_index, by = step) , 10)  )
# #     
# #     
# #     i = i + 1;
# #     
# #   } 
# #   
# # }  
# # 
# # n_tests * N * n_class 
# # 
# # 
# # 
# # 
# # ratios <- c(0.15, 0.6, 1.25, 2.0, 4.4, 4.0, 4.9)
# # N_sample <- c(1, 4, 8, 16, 64, 128, 256) * 1000
# # 
# # plot(y = ratios, x = N_sample, xlab = "Sample size (NOTE: N_obs = N * n_tests, where n_tests = 6)", ylab = "ratio", type = "l", col = "blue")
# # points(y = ratios, x = N_sample, col = "blue")
# # abline(h = 1, col = "green")
# # 
# # df_CPU_vs_GPU  <- tibble(ratio = ratios, N = N_sample)
# # N_sample_factor <- as.factor(N_sample)
# # df_CPU_vs_GPU_factor <- tibble(ratio = ratios, N = N_sample_factor)
# # 
# # require(ggplot2)
# # ggplot(data = df_CPU_vs_GPU_factor, aes(x = N, y = ratio)) + 
# #   geom_point(size = 4, colour = "blue") + 
# #   geom_line(group = N, colour = "blue") + 
# #   ylab("Relative speed (CPU-only vs CPU+GPU)") + 
# #   xlab("N /Sample size (NOTE: N_obs = N * n_tests, where n_tests = 6)")
# # 
# # 
# # 
# #  
# # 
# # 
# # 
# # 
# # 
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
