


# N <- 500
algorithm = "AD_BayesMVP_Stan"


R_fn_make_df_ps_par_scaling_given_comp_and_algorithm <- function(   pilot_study_parallel_scaling_comp_list, 
                                                                    computer, 
                                                                    algorithm) { 
  
  
        ## Extract list from ps1:
        pilot_study_opt_N_chunks_list <- pilot_study_parallel_scaling_comp_list$pilot_study_opt_N_chunks_list
  
        ## Make big tibble:
        {
          df_index_counter <- 0
          df_opt_N_chunks_ps_for_each_N_list <- list()
          for (N in pilot_study_parallel_scaling_comp_list$N_vec) { 
            
                print(paste0("N = ", N))
                ## Increment counter:
                df_index_counter <- df_index_counter + 1
                ##
                if (algorithm == "MD_BayesMVP") {
                  n_chunks_vec <- pilot_study_parallel_scaling_comp_list$n_chunks_vecs[[as.character(computer)]][[as.character(N)]]
                  n_rows <- length(n_chunks_vec)
                } else { 
                  n_chunks_vec <- NA
                  n_rows <- 1
                }
                ##
                n_iter_given_N <- pilot_study_opt_N_chunks_list$n_iter_given_N[[as.character(N)]]
                n_runs <- pilot_study_parallel_scaling_comp_list$n_runs
                ##
                if (computer == "Laptop") {
                  n_threads_vec <- pilot_study_opt_N_chunks_list$n_threads_vec_for_Laptop 
                } else { 
                  n_threads_vec <- pilot_study_opt_N_chunks_list$n_threads_vec_for_Local_HPC
                }
                ##
                n_thread_total_combos <- length(n_threads_vec)
                ##
                df_given_N_for_each_num_threads <- list()
                for (n_threads_index in 1:n_thread_total_combos) {
                      n_threads <- n_threads_vec[n_threads_index]
                      df_given_N_for_each_num_threads[[n_threads_index]] <- dplyr::tibble(  algorithm = rep(algorithm, n_rows), 
                                                                                            df_index = rep(df_index_counter, n_rows),
                                                                                            N = rep(N, n_rows),
                                                                                            n_chunks = n_chunks_vec,
                                                                                            n_threads = rep(n_threads, n_rows),
                                                                                            n_iter = rep(n_iter_given_N, n_rows)
                      )
                }
                ## Now bind all into a single big df:
                df_given_N <- tibble(data.table::rbindlist(df_given_N_for_each_num_threads)) ## %>% print(n = 100)
                ##
                n_rows <- nrow(df_given_N)
                df_given_N_for_each_run_list <- list()
                for (run in 1:n_runs) {
                     df_given_N_for_each_run_list[[run]] <- df_given_N %>% dplyr::mutate(run_number = rep(run, n_rows))
                }
                ## Now bind all into a single big df:
                df_given_N_all_runs <- tibble(data.table::rbindlist(df_given_N_for_each_run_list)) ## %>% print(n = 100)
                df_opt_N_chunks_ps_for_each_N_list[[df_index_counter]] <- df_given_N_all_runs
            
          }
          ## Then bind to get one big dataset to store all results:
          df_all_runs <- tibble(data.table::rbindlist(df_opt_N_chunks_ps_for_each_N_list))  
          n_rows <- nrow(df_all_runs)
          df_all_runs <- df_all_runs %>% dplyr::mutate(device = rep(computer, n_rows)) 
          df_all_runs ## %>% print(n = 1000)
        }
        
        
          return(df_all_runs)
  
}








R_fn_make_df_ps_par_scaling_given_algorithm <- function( pilot_study_parallel_scaling_comp_list, 
                                                         algorithm) { 
        
            ## DF for HPC:
            df_all_runs_for_Local_HPC <- R_fn_make_df_ps_par_scaling_given_comp_and_algorithm(  pilot_study_parallel_scaling_comp_list = pilot_study_parallel_scaling_comp_list, 
                                                                                                computer = "Local_HPC", 
                                                                                                algorithm = algorithm)
            
            ## df for Laptop:
            df_all_runs_for_Local_Laptop <- R_fn_make_df_ps_par_scaling_given_comp_and_algorithm(  pilot_study_parallel_scaling_comp_list = pilot_study_parallel_scaling_comp_list, 
                                                                                                   computer = "Laptop", 
                                                                                                   algorithm = algorithm)
                    
            ## merge the df's:
            df_list <- list(df_all_runs_for_Local_HPC, 
                            df_all_runs_for_Local_Laptop)
            df_all_runs_all_devices <- tibble(data.table::rbindlist(df_list))
            df_all_runs_all_devices ## %>% print(n = 500)
            
            return(df_all_runs_all_devices)
    
}
  
  




 

R_fn_make_df_ps_par_scaling <- function(pilot_study_parallel_scaling_comp_list) { 
  
        ## DF for BayesMVP manual-gradient model:
        df_all_runs_for_BayesMVP_MD <- R_fn_make_df_ps_par_scaling_given_algorithm(  pilot_study_parallel_scaling_comp_list = pilot_study_parallel_scaling_comp_list, 
                                                                                     algorithm = "MD_BayesMVP")
        
        ## DF for BayesMVP using .stan file / Stan model:
        df_all_runs_for_AD_BayesMVP_Stan <- R_fn_make_df_ps_par_scaling_given_algorithm(  pilot_study_parallel_scaling_comp_list = pilot_study_parallel_scaling_comp_list, 
                                                                                          algorithm = "AD_BayesMVP_Stan")
        
        ## DF for Mplus (standard Gibbs alg.):
        df_all_runs_for_Mplus_standard <- R_fn_make_df_ps_par_scaling_given_algorithm(  pilot_study_parallel_scaling_comp_list = pilot_study_parallel_scaling_comp_list, 
                                                                                        algorithm = "Mplus_standard")
        
        ## DF for Mplus (standard Gibbs alg. w/ WCP):
        df_all_runs_for_Mplus_WCP <- R_fn_make_df_ps_par_scaling_given_algorithm(  pilot_study_parallel_scaling_comp_list = pilot_study_parallel_scaling_comp_list, 
                                                                                   algorithm = "Mplus_WCP")
        
       
        
        ## merge the df's:
        df_list <- list(df_all_runs_for_BayesMVP_MD, 
                        df_all_runs_for_AD_BayesMVP_Stan,
                        df_all_runs_for_Mplus_standard,
                        df_all_runs_for_Mplus_WCP)
        ##
        df_all_runs_all_devices <- tibble(data.table::rbindlist(df_list))
        df_all_runs_all_devices ## %>% print(n = 500)
        
        return(df_all_runs_all_devices)
  
}





# 
# # 
# tibble_all_runs_skeleton <- R_fn_make_df_ps_par_scaling(pilot_study_parallel_scaling_comp_list = pilot_study_parallel_scaling_comp_list)
# N <- 2500
dataset_index <- 1
n_chunks_index <- 1
n_runs_index <- 1
n_threads_index <- 1
algorithm <- "AD_BayesMVP_Stan"
 algorithm_index <- 2


R_fn_add_res_to_df_ps_par_scaling_helper_fn <- function(  pilot_study_parallel_scaling_comp_list, 
                                                          tibble_skeleton,
                                                          computer,
                                                          algorithm
) {
  
  {
    if (computer == "Laptop") { 
      n_threads_vec <- pilot_study_parallel_scaling_comp_list$pilot_study_opt_N_chunks_list$n_threads_vec_for_Laptop 
    } else { 
      n_threads_vec <- pilot_study_parallel_scaling_comp_list$pilot_study_opt_N_chunks_list$n_threads_vec_for_Local_HPC
    }     
    ##
    n_thread_total_combos <- length(n_threads_vec)
    ##
    row_index <- 0 
    ##
  }
  
  {
    ##
    for (dataset_index in 1:length(N_vec)) {
      
      N <- N_vec[dataset_index]
      ## Load the RDS file:
      {
        output_path <- pilot_study_parallel_scaling_comp_list$output_path
        ##
        file_name <- paste0("parallel_scalability_ps", "_",
                            "algorithm", algorithm, "_",
                            "N", N, "_",
                            "n_runs", pilot_study_parallel_scaling_comp_list$n_runs)
        ##
        if (computer == "Laptop") { 
          file_name <- paste0("Laptop_",    file_name)
        } else { 
          attempt_success <- FALSE
          try({ 
            file_name <- paste0("HPC_",    file_name)
            attempt_success <- TRUE
          })
          if (attempt_success == FALSE) {
            file_name <- paste0("Local_HPC_",    file_name)
            attempt_success <- TRUE
          }
        }   
        ## Path:
        file_path <- file.path(output_path, file_name)
        ## Load file: 
        array_given_N <- readRDS(file_path)
      }
      
      ##
      if (algorithm == "MD_BayesMVP") {
          n_chunks_vec_given_N <- pilot_study_parallel_scaling_comp_list$n_chunks_vecs[[as.character(computer)]][[as.character(N)]]
          n_chunks_combos_given_N <- length(n_chunks_vec_given_N)
      } else { 
        n_chunks_vec_given_N <- c(1)
        n_chunks_combos_given_N <-  1
      }
      ##
      for (n_runs_index in 1:n_runs) {
        ##
        run_number_given_N <- n_runs_index
        ##
        for (n_threads_index in 1:n_thread_total_combos) {
          ##
          for (n_chunks_index in 1:n_chunks_combos_given_N)  {
            ##
            row_index <- row_index + 1
            ##
            time_sec <- array_given_N[n_chunks_index, n_runs_index, n_threads_index]
            ##
            tibble_skeleton[row_index, ]$time_sec <- time_sec
          }
        }
      }
    }
  }
  
  return(tibble_skeleton)
  
}


 






R_fn_add_res_to_df_ps_par_scaling <- function(   pilot_study_parallel_scaling_comp_list,
                                                 output_path,
                                                 tibble_all_runs_skeleton
) { 
  
        ## 
        {
            tibble_all_runs_skeleton_Laptop <- tibble_all_runs_skeleton %>% dplyr::filter(device == "Laptop")   %>% 
              dplyr::group_by(algorithm, N, n_threads, n_chunks) %>% 
              print(n = 100)
            ##
            tibble_all_runs_skeleton_HPC <- tibble_all_runs_skeleton %>% dplyr::filter(device == "Local_HPC") %>% 
              dplyr::group_by(algorithm, N, n_threads, n_chunks) %>% 
              print(n = 100)
            ##
            ## Extract list from ps1:
            pilot_study_opt_N_chunks_list <- pilot_study_parallel_scaling_comp_list$pilot_study_opt_N_chunks_list
            ##
            ## Important variables:
            tibble_all_runs_skeleton_HPC$time_sec <- rep(NA, nrow(tibble_all_runs_skeleton_HPC))
            tibble_all_runs_skeleton_Laptop$time_sec <- rep(NA, nrow(tibble_all_runs_skeleton_Laptop))
            ##
            n_max_chunk_combos <- pilot_study_opt_N_chunks_list$n_max_chunk_combos
            N_vec <- pilot_study_opt_N_chunks_list$N_vec
            ##
            n_runs <- pilot_study_parallel_scaling_comp_list$n_runs
            ##
            output_path <- pilot_study_parallel_scaling_comp_list$output_path
            ##
            algorithm_vec <- c("MD_BayesMVP", 
                               "AD_BayesMVP_Stan",
                               "Mplus_standard",
                               "Mplus_WCP")
            ##
        }

        ##
        {
            ## ---- For Laptop:
            ##
            df_list_per_algorithm <- list()
            ##
            ##
            for (algorithm_index in 1:length(algorithm_vec)) {
              ##
              algorithm_char <- as.character(algorithm_vec[algorithm_index])
              tibble_all_runs_skeleton_Laptop_given_algorithm <- tibble_all_runs_skeleton_Laptop %>% dplyr::filter(algorithm == algorithm_char)
              ##
              ## For laptop:
              tibble_all_runs_skeleton_Laptop_given_algorithm_2 <- R_fn_add_res_to_df_ps_par_scaling_helper_fn(  pilot_study_parallel_scaling_comp_list = pilot_study_parallel_scaling_comp_list,
                                                                                                                 tibble_skeleton = tibble_all_runs_skeleton_Laptop_given_algorithm,
                                                                                                                 computer = "Laptop", 
                                                                                                                 algorithm = algorithm_char)
              ##
              df_list_per_algorithm[[algorithm_index]] <- tibble_all_runs_skeleton_Laptop_given_algorithm_2
            }
            ## bind the tibbles:
            tibble_all_runs_skeleton_Laptop <- tibble(data.table::rbindlist(df_list_per_algorithm))
        }
  
        {
            ## ---- For HPC:
            ##
            df_list_per_algorithm <- list()
            ##
            ##
            for (algorithm_index in 1:length(algorithm_vec)) {
              ##
              algorithm_char <- as.character(algorithm_vec[algorithm_index])
              tibble_all_runs_skeleton_HPC_given_algorithm <- tibble_all_runs_skeleton_HPC %>% dplyr::filter(algorithm == algorithm_char)
              ##
              ## For laptop:
              tibble_all_runs_skeleton_HPC_given_algorithm_2 <- R_fn_add_res_to_df_ps_par_scaling_helper_fn(   pilot_study_parallel_scaling_comp_list = pilot_study_parallel_scaling_comp_list,
                                                                                                               tibble_skeleton = tibble_all_runs_skeleton_HPC_given_algorithm,
                                                                                                               computer = "Local_HPC", 
                                                                                                               algorithm = algorithm_char)
              ##
              df_list_per_algorithm[[algorithm_index]] <- tibble_all_runs_skeleton_HPC_given_algorithm_2
            }
            ## bind the tibbles:
            tibble_all_runs_skeleton_HPC <- tibble(data.table::rbindlist(df_list_per_algorithm))
        }
      
      ## Combine DF's:
      tibble_list <- list(tibble_all_runs_skeleton_Laptop, 
                          tibble_all_runs_skeleton_HPC)
      tibble_all_runs_skeleton <- tibble(data.table::rbindlist(tibble_list))
      
      tibble_all_runs_skeleton %>% filter(algorithm == "AD_BayesMVP_Stan", device == "Local_HPC")
      
      return(tibble_all_runs_skeleton)
  
}
 






# 
# tibble_all_runs <- R_fn_add_res_to_df_ps_par_scaling(  pilot_study_parallel_scaling_comp_list = pilot_study_parallel_scaling_comp_list,
#                                                        output_path = pilot_study_parallel_scaling_comp_list$output_path,
#                                                        tibble_all_runs_skeleton = tibble_all_runs_skeleton)
# 
# 
# 
# 
# 
# 
# tibble_all_runs
# 
# 
# ## Make + save plots (+ key tables/tibbles):
# ##
# filter(tibble_all_runs, device == "Local_HPC", algorithm == "MD_BayesMVP") %>% print(n = 500)          
# filter(tibble_all_runs, device == "Local_HPC", algorithm == "AD_BayesMVP_Stan") %>% print(n = 500)   
# filter(tibble_all_runs, device == "Local_HPC", algorithm == "Mplus_standard") %>% print(n = 500)   
# filter(tibble_all_runs, device == "Local_HPC", algorithm == "Mplus_WCP") %>% print(n = 500)   
# ##
# filter(tibble_all_runs, device == "Laptop", algorithm == "MD_BayesMVP") %>% print(n = 500)   
# filter(tibble_all_runs, device == "Laptop", algorithm == "AD_BayesMVP_Stan") %>% print(n = 500)   
# filter(tibble_all_runs, device == "Laptop", algorithm == "Mplus_standard") %>% print(n = 500)   
# filter(tibble_all_runs, device == "Laptop", algorithm == "Mplus_WCP") %>% print(n = 500)   
# 
# 
# 
# 


















