

#### -------  Main function for - "optimising the number of chunks for BayesMVP manual-gradients model" ) ----------------------------------------------------

## Make list for pilot study: 
pilot_study_opt_N_chunks_list <- list()

## Set device
pilot_study_opt_N_chunks_list$device <- computer
print(paste("device = ", pilot_study_opt_N_chunks_list$device))
## run type:
pilot_study_opt_N_chunks_list$run_type <- "ps1"
print(paste("run_type = ", pilot_study_opt_N_chunks_list$run_type))

if (pilot_study_opt_N_chunks_list$device == "Laptop")  { 
  pilot_study_opt_N_chunks_list$n_total_threads <- 8*2 ## SMT * enabled * on Laptop
  options(mc.cores = n_total_threads)
  pilot_study_opt_N_chunks_list$n_threads_vec <- c(2, 4, 8, 16)
  pilot_study_opt_N_chunks_list$SIMD_vect_type <- "AVX2"
} else { 
  pilot_study_opt_N_chunks_list$n_total_threads <- 96 ## SMT * disabled * on local_HPC
  options(mc.cores = n_total_threads)
  pilot_study_opt_N_chunks_list$n_threads_vec <- c(4, 8, 16, 32, 64)
  pilot_study_opt_N_chunks_list$SIMD_vect_type <- "AVX512"
}

n_thread_total_combos <- length(pilot_study_opt_N_chunks_list$n_threads_vec)



times <- c()

 




{
  ##
  pilot_study_opt_N_chunks_list$n_chunks_vec_N_500   <-  c(1, 2, 4,  5,   10) # ~ 3 mins
  pilot_study_opt_N_chunks_list$n_chunks_vec_N_1000  <-  c(1, 2, 4,  5,   10) # ~ 3 mins
  pilot_study_opt_N_chunks_list$n_chunks_vec_N_2500  <-  c(1, 2, 4,  5,   10,  20,  25) # ~ 4 mins
  pilot_study_opt_N_chunks_list$n_chunks_vec_N_5000  <- c(1, 2, 4,  5,   10,  20,  25,  40,  50) # ~ 5 mins
  pilot_study_opt_N_chunks_list$n_chunks_vec_N_12500 <- c(1, 5, 10, 20, 25,  40,  50,  100, 125,  200,  250) # ~ 6 mins
  pilot_study_opt_N_chunks_list$n_chunks_vec_N_25000 <- c(1, 5, 10, 20, 25,  40,  50,  100, 125,  200,  250, 400, 500) # ~ 7 mins
  ##
  n_max_chunk_combos <- length(pilot_study_opt_N_chunks_list$n_chunks_vec_N_25000)
}





pilot_study_opt_N_chunks_list$n_runs <- 10
pilot_study_opt_N_chunks_list$N_vec <- BayesMVP_pilot_study_list$N_sample_sizes_vec_for_BayesMVP

pilot_study_opt_N_chunks_list

start_index <- 1
iii <- 1
kkk <- 1
jj <- 1
dataset_index <- 1

N_vec <- pilot_study_opt_N_chunks_list$N_vec
n_runs <- pilot_study_opt_N_chunks_list$n_runs

## Make array to store results:
times_array <- array(dim = c(length(N_vec), n_max_chunk_combos, n_runs, n_thread_total_combos))
str(times_array)
dimnames(times_array) <- list(N = c(500, 1000, 2500, 5000, 12500, 25000),
                              n_chunks_index = seq(from = 1, to = n_max_chunk_combos, by = 1),
                              run_number = seq(from = 1, to = n_runs, by = 1),
                              n_threads_index =   pilot_study_opt_N_chunks_list$n_threads_vec) 
##   "n_chunks_index", "run_number", "n_threads_index")

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
n_chains_burnin <- 8
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
  
        ## Get Rcpp / C++ lists:
        Model_args_as_Rcpp_List <- BayesMVP_model_obj$init_object$Model_args_as_Rcpp_List
        EHMC_args_as_Rcpp_List <- init_EHMC_args_as_Rcpp_List(diffusion_HMC = diffusion_HMC)
        ## Edit entries to ensure don't get divergences - but also ensure suitable L chosen:
        EHMC_args_as_Rcpp_List$eps_main <- 0.0001
        ## Use path length of 16:
        L_main <- 16
        EHMC_args_as_Rcpp_List$tau_main <-     L_main * EHMC_args_as_Rcpp_List$eps_main 
        ## Metric Rcpp / C++ list::
        EHMC_Metric_as_Rcpp_List <- init_EHMC_Metric_as_Rcpp_List(   n_params_main = n_params_main, 
                                                                     n_nuisance = n_nuisance, 
                                                                     metric_shape_main = metric_shape_main)  
  
        N <- pilot_study_opt_N_chunks_list$N_vec[df_index]
        Model_args_as_Rcpp_List$N <- N
        ##
        for (c in 1:n_class) {
          for (t in 1:n_tests) {
            Model_args_as_Rcpp_List$Model_args_2_later_vecs_of_mats_double[[1]][[c]][[t]] <- matrix(1, nrow = N, ncol = 1)
          }
        }
        ##
        n_nuisance <- N * n_tests
        Model_args_as_Rcpp_List$n_nuisance <- n_nuisance
        ## Print:
        print(paste("N = ", N))
        print(paste("n_nuisance = ", n_nuisance))
        
        if (N == 500) { 
          n_chunks_vec <- pilot_study_opt_N_chunks_list$n_chunks_vec_N_500
          N_iter <- 400 
        }
        if (N == 1000) { 
          n_chunks_vec <- pilot_study_opt_N_chunks_list$n_chunks_vec_N_1000
          N_iter <- 200 
        }
        if (N == 2500) { 
          n_chunks_vec <- pilot_study_opt_N_chunks_list$n_chunks_vec_N_2500
          N_iter <- 80 
        }
        if (N == 5000) { 
          n_chunks_vec <- pilot_study_opt_N_chunks_list$n_chunks_vec_N_5000
          N_iter <- 40 
        }
        if (N == 12500) { 
          n_chunks_vec <- pilot_study_opt_N_chunks_list$n_chunks_vec_N_12500
          N_iter <- 16 
        }
        if (N == 25000) { 
          n_chunks_vec <- pilot_study_opt_N_chunks_list$n_chunks_vec_N_25000
          N_iter <- 8 
        }
        ##
        n_iter <- N_iter
        ##
        

  
  for (kkk in 1:length(n_chunks_vec))  {
    
    # n_params <- n_params_main + n_nuisance 
    # theta_vec = rep(0.01, n_params) 
    
    ## Set the number of chunks to use in model_args_list:
    num_chunks <- n_chunks_vec[kkk]
    ## BayesMVP_pilot_study_list$model_args_list$num_chunks <- num_chunks
    Model_args_as_Rcpp_List$Model_args_ints[4] <- num_chunks
    ## Using manual-gradient model obj:
    BayesMVP_model_obj <- BayesMVP_LC_MVP_model_using_manual_grad_obj
    
    
    for (iii in 1:pilot_study_opt_N_chunks_list$n_runs) {
      
              seed <- iii

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
              }
              
              print(paste("N = ", N))
              comment(print(iii))
              times_array[df_index, kkk, iii, ]  <- print(round(dput(times), 3))
            }
            
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
        pilot_study_opt_N_chunks_list$output_path <- file.path(  getwd(), 
                                                                "1_appendix_pilot_studies",
                                                                "ps_1_optimizing_N_chunks",
                                                                "outputs")
        file_name <- paste0("determining_optimal_N_chunks_ps", 
                            "_N_", N, 
                            "n_runs_", pilot_study_opt_N_chunks_list$n_runs)
        if (pilot_study_opt_N_chunks_list$device == "Laptop") { 
           file_name <- paste0("Laptop_", file_name)
        } else { 
           file_name <- paste0("HPC_",    file_name)
        }
     
        file_path <- file.path(pilot_study_opt_N_chunks_list$output_path, file_name)
        
        saveRDS(object = times_array[df_index,,,], file = file_path)
  }
  
  try({  
    beepr::beep("random")
  })
  
  
}













# Plot / table results  for N_chunks pilot study   ------------------------------------------------------------------------------------------------------------


N_vec <- c(500, 1000, 2500, 5000, 12500, 25000)

#  HPC
N_chunks_pilot_results_for_each_N_HPC <- list()
N_chunks_pilot_results_medians_for_each_N_HPC <- list()
N_chunks_pilot_results_SD_for_each_N_HPC <- list()

for (dataset_index in 1:length(N_vec)) {
  
      {
          pilot_study_opt_N_chunks_list$output_path <- file.path(  getwd(), 
                                                                   "1_appendix_pilot_studies",
                                                                   "ps_1_optimizing_N_chunks",
                                                                   "outputs")
          file_name <- paste0("determining_optimal_N_chunks_ps", "_N_", N)
          file_name <- paste0("HPC_",    file_name)
          ## Path:
          file_path <- file.path(pilot_study_opt_N_chunks_list$output_path, file_name)
          ## Load in data: 
          N_chunks_pilot_results_for_each_N_HPC[[dataset_index]] <- readRDS(file_path)
      }
      
      
      N_chunks_pilot_results_medians_for_each_N_HPC[[dataset_index]]   <- apply(   N_chunks_pilot_results_for_each_N_HPC[[dataset_index]],   c(1), median, na.rm = TRUE)
      N_chunks_pilot_results_medians_for_each_N_HPC[[dataset_index]]   <-  N_chunks_pilot_results_medians_for_each_N_HPC[[dataset_index]][!(is.na( N_chunks_pilot_results_medians_for_each_N_HPC[[dataset_index]]))]
      
      N_chunks_pilot_results_SD_for_each_N_HPC[[dataset_index]]   <- apply(N_chunks_pilot_results_for_each_N_HPC[[dataset_index]],   c(1), sd, na.rm = TRUE)
      N_chunks_pilot_results_SD_for_each_N_HPC[[dataset_index]] <-  N_chunks_pilot_results_SD_for_each_N_HPC[[dataset_index]][!(is.na( N_chunks_pilot_results_SD_for_each_N_HPC[[dataset_index]]))]
      
      # make into tibble columns for use with rbindlist
      N_chunks_pilot_results_medians_for_each_N_HPC[[dataset_index]] <- tibble(  N_chunks_pilot_results_medians_for_each_N_HPC[[dataset_index]] )
      N_chunks_pilot_results_SD_for_each_N_HPC[[dataset_index]] <- tibble(  N_chunks_pilot_results_SD_for_each_N_HPC[[dataset_index]] )
  
}


# Laptop
N_chunks_pilot_results_for_each_N_Laptop <- list()
N_chunks_pilot_results_medians_for_each_N_Laptop <- list()
N_chunks_pilot_results_SD_for_each_N_Laptop <- list()

for (dataset_index in 1:length(N_vec)) {
  
      {
        pilot_study_opt_N_chunks_list$output_path <- file.path(  getwd(), 
                                                                 "1_appendix_pilot_studies",
                                                                 "ps_1_optimizing_N_chunks",
                                                                 "outputs")
        file_name <- paste0("determining_optimal_N_chunks_ps", "_N_", N)
        file_name <- paste0("Laptop_",    file_name)
        ## Path:
        file_path <- file.path(pilot_study_opt_N_chunks_list$output_path, file_name)
        ## Load in data: 
        N_chunks_pilot_results_for_each_N_HPC[[dataset_index]] <- readRDS(file_path)
      }
      
      
      N_chunks_pilot_results_medians_for_each_N_Laptop[[dataset_index]]   <- apply(   N_chunks_pilot_results_for_each_N_Laptop[[dataset_index]],   c(1), median, na.rm = TRUE)
      N_chunks_pilot_results_medians_for_each_N_Laptop[[dataset_index]]   <-  N_chunks_pilot_results_medians_for_each_N_Laptop[[dataset_index]][!(is.na( N_chunks_pilot_results_medians_for_each_N_Laptop[[dataset_index]]))]
      
      
      N_chunks_pilot_results_SD_for_each_N_Laptop[[dataset_index]]   <- apply(N_chunks_pilot_results_for_each_N_Laptop[[dataset_index]],   c(1), sd, na.rm = TRUE)
      N_chunks_pilot_results_SD_for_each_N_Laptop[[dataset_index]] <-  N_chunks_pilot_results_SD_for_each_N_Laptop[[dataset_index]][!(is.na( N_chunks_pilot_results_SD_for_each_N_Laptop[[dataset_index]]))]
      
      # make into tibble columns for use with rbindlist
      N_chunks_pilot_results_medians_for_each_N_Laptop[[dataset_index]] <- tibble(  N_chunks_pilot_results_medians_for_each_N_Laptop[[dataset_index]] )
      N_chunks_pilot_results_SD_for_each_N_Laptop[[dataset_index]] <- tibble(  N_chunks_pilot_results_SD_for_each_N_Laptop[[dataset_index]] )
      
}




# N_chunks_pilot_results_medians_N_500   <- apply(N_chunks_pilot_results_N_500,   c(1), median, na.rm = TRUE)


N_vec_for_df <- c(rep(500,   nrow(N_chunks_pilot_results_medians_for_each_N_HPC[[1]])), 
                  rep(1000,  nrow(N_chunks_pilot_results_medians_for_each_N_HPC[[2]])), 
                  rep(2500,  nrow(N_chunks_pilot_results_medians_for_each_N_HPC[[3]])), 
                  rep(5000,  nrow(N_chunks_pilot_results_medians_for_each_N_HPC[[4]])), 
                  rep(12500, nrow(N_chunks_pilot_results_medians_for_each_N_HPC[[5]])), 
                  rep(25000, nrow(N_chunks_pilot_results_medians_for_each_N_HPC[[6]]))
)


times_avg_vec_for_df_HPC <-   data.table::rbindlist(N_chunks_pilot_results_medians_for_each_N_HPC)[[1]]
times_SD_vec_for_df_HPC  <-   data.table::rbindlist(N_chunks_pilot_results_SD_for_each_N_HPC)[[1]]


times_avg_vec_for_df_Laptop <-   data.table::rbindlist(N_chunks_pilot_results_medians_for_each_N_Laptop)[[1]]
times_SD_vec_for_df_Laptop  <-   data.table::rbindlist(N_chunks_pilot_results_SD_for_each_N_Laptop)[[1]]





{
  n_chunks_vec_N_500   <- c(1) # c(1, 2, 4,  5,  8,   10) # ~ 3 mins
  n_chunks_vec_N_1000  <- c(1)# c(1, 2, 4,  5,  8,   10) # ~ 3 mins
  n_chunks_vec_N_2500  <- c(1) # c(1, 2, 4,  5,  8,   10,  20,  25) # ~ 4 mins
  n_chunks_vec_N_5000  <- c(1, 2,  5,  8,   10,  20,  25,  40,  50) # ~ 5 mins
  n_chunks_vec_N_12500 <- c(1, 5, 10, 20, 25,  40,  50,  100, 125,  200,  250) # ~ 6 mins
  n_chunks_vec_N_25000 <- c(1, 5, 10, 20, 25,  40,  50,  100, 125,  200,  250, 400, 500) # ~ 7 mins
}

N_chunks_vec_for_df_both_HPC_and_Laptop  <- c(n_chunks_vec_N_500,
                                              n_chunks_vec_N_1000,
                                              n_chunks_vec_N_2500,
                                              n_chunks_vec_N_5000,
                                              n_chunks_vec_N_12500,
                                              n_chunks_vec_N_25000
)

N_iter_pilot_study_df_both_HPC_and_Laptop  <- c(   rep(400,   nrow(N_chunks_pilot_results_medians_for_each_N_HPC[[1]])), 
                                                   rep(200,   nrow(N_chunks_pilot_results_medians_for_each_N_HPC[[2]])), 
                                                   rep(80,    nrow(N_chunks_pilot_results_medians_for_each_N_HPC[[3]])), 
                                                   rep(40,    nrow(N_chunks_pilot_results_medians_for_each_N_HPC[[4]])), 
                                                   rep(16,    nrow(N_chunks_pilot_results_medians_for_each_N_HPC[[5]])), 
                                                   rep(8,     nrow(N_chunks_pilot_results_medians_for_each_N_HPC[[6]]))
)



length(N_vec_for_df)
length(N_iter_pilot_study_df_both_HPC_and_Laptop)
length(times_avg_vec_for_df_HPC)
length(times_SD_vec_for_df_HPC)
length(N_chunks_vec_for_df_both_HPC_and_Laptop)



N_chunks_pilot_study_df_HPC <- tibble(N = factor(N_vec_for_df), 
                                      device = factor(rep("HPC", length(N_iter_pilot_study_df_both_HPC_and_Laptop))), 
                                      N_num = N_vec_for_df,
                                      time_avg = times_avg_vec_for_df_HPC,
                                      time_SD = times_SD_vec_for_df_HPC,
                                      N_chunks_num = N_chunks_vec_for_df_both_HPC_and_Laptop,
                                      N_chunks = factor(N_chunks_vec_for_df_both_HPC_and_Laptop),
                                      N_iter = N_iter_pilot_study_df_both_HPC_and_Laptop,
                                      iter_per_second = N_iter /  time_avg,
                                      iter_per_second_adj_for_N = N_num * iter_per_second
)

length(times_avg_vec_for_df_Laptop)
length(times_SD_vec_for_df_Laptop)

N_chunks_pilot_study_df_Laptop <- tibble(N = factor(N_vec_for_df), 
                                         device = factor(rep("Laptop", length(N_iter_pilot_study_df_both_HPC_and_Laptop))), 
                                         N_num = N_vec_for_df,
                                         time_avg = times_avg_vec_for_df_Laptop,
                                         time_SD = times_SD_vec_for_df_Laptop,
                                         N_chunks_num = N_chunks_vec_for_df_both_HPC_and_Laptop,
                                         N_chunks = factor(N_chunks_vec_for_df_both_HPC_and_Laptop),
                                         N_iter = N_iter_pilot_study_df_both_HPC_and_Laptop,
                                         iter_per_second = N_iter /  time_avg,
                                         iter_per_second_adj_for_N = N_num * iter_per_second
)


N_chunks_pilot_study_df_both_HPC_and_Laptop <- rbind(N_chunks_pilot_study_df_HPC, 
                                                     N_chunks_pilot_study_df_Laptop)








N_chunks_pilot_study_df_both_HPC_and_Laptop <- dplyr::mutate(N_chunks_pilot_study_df_both_HPC_and_Laptop, 
                                                             N_label =  paste0("N = ",  N_chunks_pilot_study_df_both_HPC_and_Laptop$N))
N_chunks_pilot_study_df_both_HPC_and_Laptop$N_label <- factor( N_chunks_pilot_study_df_both_HPC_and_Laptop$N_label )
N_chunks_pilot_study_df_both_HPC_and_Laptop$N_label <- factor(N_chunks_pilot_study_df_both_HPC_and_Laptop$N_label, 
                                                              levels = c("N = 500", "N = 1000", "N = 2500", "N = 5000", "N = 12500", "N = 25000"))



N_chunks_pilot_study_df_both_HPC_and_Laptop <- dplyr::mutate(N_chunks_pilot_study_df_both_HPC_and_Laptop, 
                                                             N_and_N_iter_label =  paste0(N_label, ", N_{iter} = ", N_iter))
N_chunks_pilot_study_df_both_HPC_and_Laptop$N_and_N_iter_label <- factor( N_chunks_pilot_study_df_both_HPC_and_Laptop$N_and_N_iter_label )
N_chunks_pilot_study_df_both_HPC_and_Laptop$N_and_N_iter_label <- factor(N_chunks_pilot_study_df_both_HPC_and_Laptop$N_and_N_iter_label, 
                                                                         levels = c("N = 500, N_{iter} = 400",
                                                                                    "N = 1000, N_{iter} = 200", 
                                                                                    "N = 2500, N_{iter} = 80", 
                                                                                    "N = 5000, N_{iter} = 40",
                                                                                    "N = 12500, N_{iter} = 16",
                                                                                    "N = 25000, N_{iter} = 8"))

# 
# N_chunks_pilot_study_df_both_HPC_and_Laptop$N_chains_label <- factor(N_chunks_pilot_study_df_both_HPC_and_Laptop$N_chains_label, levels = c( "N_chains = 4", "N_chains = 8", "N_chains = 16", "N_chains = 32", "N_chains = 64" ))
# N_chunks_pilot_study_df_both_HPC_and_Laptop$n_chains_burnin <- factor(N_chunks_pilot_study_df_both_HPC_and_Laptop$n_chains_burnin)
# 




# 
# #  ---- plot 1
# {
#   
#   
#   
#   
#   plot_N_chunks_pilot_study_HPC_plot_0 <-   ggplot(    dplyr::filter(N_chunks_pilot_study_df_both_HPC_and_Laptop, N_chunks_num != 1), 
#                                                        mapping = aes(x = (N_chunks),
#                                                                    #   y = Time,
#                                                                     # y = Time / N_iter,
#                                                                    y =  iter_per_second_adj_for_N,
#                                                                      colour = N,
#                                                                      group = N
#                                                        ),
#   ) +
#     geom_point(size = 4) + 
#     # geom_errorbar(size = 1, width = 0.02, aes(x = N_chunks, y = time_for_1000_ESS_mean, 
#     #                                           ymin = time_for_1000_ESS_mean -  1.96 * time_for_1000_ESS_SD,
#     #                                           ymax = time_for_1000_ESS_mean +  1.96 * time_for_1000_ESS_SD)) + 
#     geom_line(size = 2) + 
#     theme_bw(base_size = 20) +
#     theme(legend.position = "bottom")  + 
#     ylab("Time (non-adjusted)") + 
#     xlab("N_{chunks}" ) + 
#     scale_y_continuous(breaks = seq(from = 0, to = 50, by = 2))
#   
#   plot_N_chunks_pilot_study_HPC_plot_0
#   
#   
#   
#   
# }
# 




{
  plot_scale_factor <- 3
  plot_width <-  4*plot_scale_factor
  plot_height <- 3*plot_scale_factor
}

# png("Figure_N_chunks_pilot_study_plot_0.png" ,units = "in", width = plot_width, height=plot_height, res=800)
# plot_N_chunks_pilot_study_HPC
# dev.off()




#  ---- plot 2
{
  plot_N_chunks_pilot_study_HPC_plot_1_N_500_1000_2500 <-   ggplot(    dplyr::filter(N_chunks_pilot_study_df_both_HPC_and_Laptop, 
                                                                                     #  N_chunks_num != 1
                                                                                     N %in%(c(500:2500)) 
  ), 
  mapping = aes(x =  (N_chunks),
                y = time_avg,
                # y = Time / N_iter,
                # y =  iter_per_second_adj_for_N,
                colour = N,
                group = device
  ),
  ) +
    geom_point(size = 4) + 
    geom_errorbar(size = 1, width = 0.02, aes(x = N_chunks,
                                              y = time_avg, 
                                              ymin = time_avg -  1.96 * time_SD,
                                              ymax = time_avg +  1.96 * time_SD,
                                              colour = N,
                                              linetype = device
    )) +
    geom_line(size = 1,                aes(x = N_chunks,
                                           y = time_avg, 
                                           colour = N,
                                           linetype = device
    )) +
    theme_bw(base_size = 32) +
    theme(legend.position = "bottom")  + 
    ylab("Time (secomds)") + 
    xlab("N_{chunks}" ) + 
    #scale_y_continuous(breaks = seq(from = 0, to = 10, by = 1)) + 
    #   scale_x_continuous(breaks = seq(from = 0, to = 500, by = 1)) +
    facet_wrap( ~ N_and_N_iter_label, 
                scales = "free"
    ) + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
  plot_N_chunks_pilot_study_HPC_plot_1_N_500_1000_2500
}




{
  plot_N_chunks_pilot_study_HPC_plot_1_N_5000_12500_25000 <-   ggplot(    dplyr::filter(N_chunks_pilot_study_df_both_HPC_and_Laptop, 
                                                                                        N_chunks_num != 1,
                                                                                        N %in%(c(5000:25000)) 
  ), 
  mapping = aes(x =  (N_chunks),
                y = time_avg,
                # y = Time / N_iter,
                # y =  iter_per_second_adj_for_N,
                colour = N,
                group = device
  ),
  ) +
    geom_point(size = 4) + 
    geom_errorbar(size = 1, width = 0.02, aes(x = N_chunks,
                                              y = time_avg, 
                                              ymin = time_avg -  1.96 * time_SD,
                                              ymax = time_avg +  1.96 * time_SD,
                                              colour = N,
                                              linetype = device
    )) +
    geom_line(size = 1,                aes(x = N_chunks,
                                           y = time_avg, 
                                           colour = N,
                                           linetype = device
    )) +
    # geom_line(size = 2) + 
    theme_bw(base_size = 32) +
    theme(legend.position = "bottom")  + 
    ylab("Time (secomds)") + 
    xlab("N_{chunks}" ) + 
    #scale_y_continuous(breaks = seq(from = 0, to = 10, by = 1)) + 
    #   scale_x_continuous(breaks = seq(from = 0, to = 500, by = 1)) +
    scale_color_manual(values = c("5000" = "orange", "12500" = "black", "25000" = "purple")) +  
    facet_wrap( ~ N_and_N_iter_label, 
                scales = "free"
    ) + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
  plot_N_chunks_pilot_study_HPC_plot_1_N_5000_12500_25000
}



plot_N_chunks_pilot_study_HPC_plot_1_N_500_1000_2500 + 
  plot_N_chunks_pilot_study_HPC_plot_1_N_5000_12500_25000 + 
  plot_layout(ncol = 1)



{
  plot_scale_factor <- 3
  plot_width <-  4*plot_scale_factor
  plot_height <- 3*plot_scale_factor
  }

png("Figure_N_chunks_pilot_study_plot_1.png" ,units = "in", width = plot_width, height=plot_height, res=800)
plot_N_chunks_pilot_study_HPC_plot_1_N_500_1000_2500 + 
  plot_N_chunks_pilot_study_HPC_plot_1_N_5000_12500_25000 + 
  plot_layout(ncol = 1)
dev.off()
# 








# stats for table showing best N_{chunks} for each N - for both HPC and laptop



N_chunks_pilot_study_df_best_HPC <- N_chunks_pilot_study_df_both_HPC_and_Laptop %>%
  group_by(device, N)  %>%
  mutate(best_ind = ifelse(time_avg == min(time_avg), 1, 0)) %>% 
  dplyr::filter(best_ind == 1, device == "HPC") %>% 
  print(n = 1000)



N_chunks_pilot_study_df_best_Laptop <- N_chunks_pilot_study_df_both_HPC_and_Laptop %>%
  group_by(device, N)  %>%
  mutate(best_ind = ifelse(time_avg == min(time_avg), 1, 0)) %>% 
  dplyr::filter(best_ind == 1, device == "Laptop") %>% 
  print(n = 1000)









{
  plot_scale_factor <- 3
  plot_width <-  4*plot_scale_factor
  plot_height <- 3*plot_scale_factor
  }

# png("Figure_N_chunks_pilot_study_plot_0.png" ,units = "in", width = plot_width, height=plot_height, res=800)
# plot_N_chunks_pilot_study_HPC
# dev.off()









#  ---- plot 2
{
  
  speed_for_fastest_N_chunk_for_N_500 <- max(dplyr::filter(N_chunks_pilot_study_df_2, N == 500)$iter_per_second_adj_for_N)
  
  N_chunks_pilot_study_df_2 <- N_chunks_pilot_study_df %>%
    dplyr::mutate( speed_ratio_relative_to_fastest_N_500  =   iter_per_second_adj_for_N / speed_for_fastest_N_chunk_for_N_500 )
  
  
  N_chunks_pilot_study_df_2$speed_ratio_relative_to_fastest_N_500
  
  
  
  plot_N_chunks_pilot_study_HPC_plot_0 <-   ggplot(    dplyr::filter(N_chunks_pilot_study_df_2), 
                                                       mapping = aes(
                                                         # x = log(N_chunks_num),
                                                         x =  (N_chunks),
                                                         y = speed_ratio_relative_to_fastest_N_500,
                                                         colour = N,
                                                         group = N
                                                       ),
  ) +
    geom_point(size = 4) + 
    geom_line(size = 2) + 
    theme_bw(base_size = 20) +
    theme(legend.position = "bottom")  + 
    ylab("speed_ratio_relative_to_fastest_N_500") + 
    xlab("log N_{chunks}" ) + 
    geom_hline(yintercept = 1,   linetype="dashed", color = "red",    size = 1) 
  #scale_y_continuous(breaks = seq(from = 0, to = 50, by = 2))
  
  plot_N_chunks_pilot_study_HPC_plot_0
  
  
  
  
}




{
  plot_scale_factor <- 3
  plot_width <-  4*plot_scale_factor
  plot_height <- 3*plot_scale_factor
}

# png("Figure_N_chunks_pilot_study_plot_0.png" ,units = "in", width = plot_width, height=plot_height, res=800)
# plot_N_chunks_pilot_study_HPC
# dev.off()
# 





#  ---- plot 3
{
  
  
  
  
  plot_N_chunks_pilot_study_HPC_plot_1 <-   ggplot(    dplyr::filter(N_chunks_pilot_study_df, N_chunks_num != 1), 
                                                       mapping = aes(x = log(N_chunks_num),
                                                                     y = iter_per_second_adj_for_N,
                                                                     colour = N,
                                                                     group = N
                                                       ),
  ) +
    geom_point(size = 4) + 
    geom_line(size = 2) + 
    # geom_line(     filter(N_chunks_pilot_study_df, N_chunks_num != 1), 
    #              mapping = aes(x = N_chunks,
    #                  y = iter_per_second,
    #                  colour = N
    # )) + 
    # geom_line(size = 2.5,
    #            filter(N_chunks_pilot_study_df,
    #                   N_chunks_num != 1
    #            ), 
    #            aes(x =  N_chunks, 
    #                y = iter_per_second,
    #                group = N
    #                )
    #           )  + 
    # scale_color_manual(values = c("max[Range] (threshold = 0.75)" = "blue", "max[IQR] (threshold = 0.50)" = "red")) +
    theme_bw(base_size = 20) +
    # geom_line() + 
    theme(legend.position = "bottom")  + 
    ylab("Speed (adjusted for N)") + 
    xlab("N_{chunks}" )
  # facet_wrap( ~ N, 
  #             scales = "free",
  #             ncol = 3
  #)
  
  
  plot_N_chunks_pilot_study_HPC_plot_1
  
  
  
  
}


{
  plot_scale_factor <- 3
  plot_width <-  4*plot_scale_factor
  plot_height <- 3*plot_scale_factor
}

# png("Figure_N_chunks_pilot_study_plot_1.png" ,units = "in", width = plot_width, height=plot_height, res=800)
# plot_N_chunks_pilot_study_HPC
# dev.off()





#  ---- plot 4
{
  
  
  
  
  plot_N_chunks_pilot_study_HPC_plot_2 <-   ggplot(filter(N_chunks_pilot_study_df,
                                                          N_chunks_num != 1), 
                                                   aes(x = log(N_chunks_num),
                                                       y = Time)) +
    geom_point() + 
    geom_point(size = 2.5, aes(x =  N_chunks, y = Time), position = position_jitter(width = 0, height = 0, seed = 123))  + 
    # scale_color_manual(values = c("max[Range] (threshold = 0.75)" = "blue", "max[IQR] (threshold = 0.50)" = "red")) +
    theme_bw(base_size = 16) +
    # geom_line() + 
    theme(legend.position = "bottom")  + 
    ylab("Max Range (blue) and IQR (red)") + 
    xlab("Min ESS") + 
    facet_wrap( ~ N, 
                scales = "free",
                ncol = 3
    )
  
  
  plot_N_chunks_pilot_study_HPC_plot_2
  
  
  
  
}


{
  plot_scale_factor <- 3
  plot_width <-  4*plot_scale_factor
  plot_height <- 3*plot_scale_factor
}

png("Figure_N_chunks_pilot_study_plot_2.png" ,units = "in", width = plot_width, height=plot_height, res=800)
plot_N_chunks_pilot_study_HPC_plot_2
dev.off()







