




R_fn_make_df_ps_opt_N_chunks_given_computer <- function(  pilot_study_opt_N_chunks_list, 
                                                          computer) { 
  
        ## Make big tibble:
        {
          df_index_counter <- 0
          df_opt_N_chunks_ps_for_each_N_list <- list()
          for (N in pilot_study_opt_N_chunks_list$N_vec) { 
            
            print(paste0("N = ", N))
            ## Increment counter:
            df_index_counter <- df_index_counter + 1
            ##
            n_rows <- length(pilot_study_opt_N_chunks_list$n_chunks_vecs[[as.character(N)]])
            n_chunks_vec <- pilot_study_opt_N_chunks_list$n_chunks_vecs[[as.character(N)]]
            n_iter_given_N <- pilot_study_opt_N_chunks_list$n_iter_given_N[[as.character(N)]]
            
            n_runs <- pilot_study_opt_N_chunks_list$n_runs
            n_threads_vec <- pilot_study_opt_N_chunks_list$n_threads_vec
            n_thread_total_combos <- length(pilot_study_opt_N_chunks_list$n_threads_vec)
            ##
            df_given_N_for_each_num_threads <- list()
            for (n_threads_index in 1:n_thread_total_combos) {
              n_threads <- n_threads_vec[n_threads_index]
              df_given_N_for_each_num_threads[[n_threads_index]] <- dplyr::tibble(  df_index = rep(df_index_counter, n_rows),
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
          ## df_all_runs %>% print(n = 100)
        }
        
        
          return(df_all_runs)
  
}








R_fn_make_df_ps_opt_N_chunks <- function(    pilot_study_opt_N_chunks_list,
                                             n_threads_vec_for_Local_HPC,
                                             n_threads_vec_for_Laptop
) { 
        
            ## DF for HPC:
            pilot_study_opt_N_chunks_list$n_threads_vec <- n_threads_vec_for_Local_HPC
            df_all_runs_for_Local_HPC <- R_fn_make_df_ps_opt_N_chunks_given_computer( pilot_study_opt_N_chunks_list = pilot_study_opt_N_chunks_list, 
                                                                                      computer = "HPC")
            
            ## df for Laptop:
            pilot_study_opt_N_chunks_list$n_threads_vec <- n_threads_vec_for_Laptop
            df_all_runs_for_Local_Laptop <- R_fn_make_df_ps_opt_N_chunks_given_computer( pilot_study_opt_N_chunks_list = pilot_study_opt_N_chunks_list, 
                                                                                         computer = "Laptop")
            
            ## merge the df's:
            df_list <- list(df_all_runs_for_Local_HPC, df_all_runs_for_Local_Laptop)
            df_all_runs_all_devices <- tibble(data.table::rbindlist(df_list))
            df_all_runs_all_devices %>% print(n = 500)
            
            return(df_all_runs_all_devices)
    
}
  
  
  
 
 


R_fn_add_res_to_df_ps_opt_N_chunks_helper_fn <- function( pilot_study_opt_N_chunks_list, 
                                                          tibble_skeleton,
                                                          computer
) {
  
        {
          if (computer == "Laptop") { 
            n_threads_vec <- pilot_study_opt_N_chunks_list$n_threads_vec_for_Laptop 
          } else { 
            n_threads_vec <- pilot_study_opt_N_chunks_list$n_threads_vec_for_Local_HPC
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
              pilot_study_opt_N_chunks_list$output_path <- file.path(  getwd(), 
                                                                       "1_appendix_pilot_studies",
                                                                       "ps_1_optimizing_N_chunks_and_N_threads",
                                                                       "outputs")
              file_name <- paste0("determining_optimal_N_chunks_ps", "_N", N)
              if (computer == "Laptop") { 
                    file_name <- paste0("Laptop_",    file_name, "_n_runs", n_runs)
              } else { 
                    attempt_success <- FALSE
                    try({ 
                      file_name <- paste0("HPC_",    file_name, "_n_runs", n_runs)
                      attempt_success <- TRUE
                    })
                    if (attempt_success == FALSE) {
                      file_name <- paste0("Local_HPC_",    file_name, "_n_runs", n_runs)
                      attempt_success <- TRUE
                    }
              }   
              ## Path:
              file_path <- file.path(pilot_study_opt_N_chunks_list$output_path, file_name)
              ## Load file: 
              array_given_N <- readRDS(file_path)
            }
            ##
            n_chunks_vec_given_N <- pilot_study_opt_N_chunks_list$n_chunks_vecs[[as.character(N)]]
            n_chunks_combos_given_N <- length(n_chunks_vec_given_N)
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






 

R_fn_add_res_to_df_ps_opt_N_chunks <- function(    pilot_study_opt_N_chunks_list,
                                                   output_path,
                                                   tibble_skeleton
) { 
  
  
      ## 
      {
        tibble_all_runs_skeleton_Laptop <- tibble_all_runs_skeleton %>% dplyr::filter(device == "Laptop")   %>% 
          dplyr::group_by(N, n_threads, n_chunks) %>% 
          print(n = 10)
        ##
        tibble_all_runs_skeleton_HPC <- tibble_all_runs_skeleton %>% dplyr::filter(device == "HPC") %>% 
          dplyr::group_by(N, n_threads, n_chunks) %>% 
          print(n = 10)
        ##
        ## Important variables:
        tibble_all_runs_skeleton_HPC$time_sec <- rep(NA, nrow(tibble_all_runs_skeleton_HPC))
        tibble_all_runs_skeleton_Laptop$time_sec <- rep(NA, nrow(tibble_all_runs_skeleton_Laptop))
        ##
        n_max_chunk_combos <- pilot_study_opt_N_chunks_list$n_max_chunk_combos
        N_vec <- pilot_study_opt_N_chunks_list$N_vec
        ##
        n_runs <- pilot_study_opt_N_chunks_list$n_runs
        ##
        output_path <- pilot_study_opt_N_chunks_list$output_path
      }
      
      ## For laptop:
      tibble_all_runs_skeleton_Laptop <- R_fn_add_res_to_df_ps_opt_N_chunks_helper_fn( pilot_study_opt_N_chunks_list = pilot_study_opt_N_chunks_list,
                                                                                       tibble_skeleton = tibble_all_runs_skeleton_Laptop,
                                                                                       computer = "Laptop")
      
      ## For Local_HPC:
      tibble_all_runs_skeleton_HPC <- R_fn_add_res_to_df_ps_opt_N_chunks_helper_fn(    pilot_study_opt_N_chunks_list = pilot_study_opt_N_chunks_list,
                                                                                       tibble_skeleton = tibble_all_runs_skeleton_HPC,
                                                                                       computer = "HPC")
  

      
      ## bind the tibbles:
      tibble_list <- list(tibble_all_runs_skeleton_Laptop, 
                          tibble_all_runs_skeleton_HPC)
      tibble_all_runs_skeleton <- tibble(data.table::rbindlist(tibble_list))
      
      return(tibble_all_runs_skeleton)
  
  
}









































