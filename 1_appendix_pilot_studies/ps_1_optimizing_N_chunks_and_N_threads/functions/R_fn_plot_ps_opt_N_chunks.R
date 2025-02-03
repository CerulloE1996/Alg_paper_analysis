


# -| ------------------------------    Pilot study - Finding the optimal N_{chunks} for BayesMVP --------------------------------------------------------------------
 



R_fn_ps_opt_N_chunks_plots <- function(     tibble_all_runs,
                                            pilot_study_opt_N_chunks_list,
                                            global_list,
                                            output_path) { 
      
      # ## Set device
      # pilot_study_opt_N_chunks_list$device <- computer
      # print(paste("device = ", pilot_study_opt_N_chunks_list$device))
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
      
      
      tibble_all_runs_avg <-  tibble_all_runs %>%
                              dplyr::group_by(df_index, N, n_chunks, n_threads, n_iter, device) %>%
                              dplyr::summarise( time_mean = mean(time_sec, na.rm = TRUE),
                                                time_median = median(time_sec, na.rm = TRUE),
                                                time_SD = sd(time_sec, na.rm = TRUE),
                                                .groups = 'drop')
      
      
      {
          tibble_all_runs_avg <- dplyr::mutate(tibble_all_runs_avg,  N_label =  paste0("N = ",  tibble_all_runs_avg$N))
          tibble_all_runs_avg$N_label <- factor( tibble_all_runs_avg$N_label )
          tibble_all_runs_avg$N_label <- factor( tibble_all_runs_avg$N_label, 
                                                 levels = c("N = 500", 
                                                            "N = 1000", 
                                                            "N = 2500", 
                                                            "N = 5000", 
                                                            "N = 12500", 
                                                            "N = 25000"))
          ##
          tibble_all_runs_avg <- dplyr::mutate( tibble_all_runs_avg, 
                                                N_and_N_iter_label =  paste0(N_label, ", N_{iter} = ", n_iter))
          tibble_all_runs_avg$N_and_N_iter_label <- factor( tibble_all_runs_avg$N_and_N_iter_label )
          tibble_all_runs_avg$N_and_N_iter_label <- factor(tibble_all_runs_avg$N_and_N_iter_label, 
                                                                                   levels = c("N = 500, N_{iter} = 400",
                                                                                              "N = 1000, N_{iter} = 200", 
                                                                                              "N = 2500, N_{iter} = 80", 
                                                                                              "N = 5000, N_{iter} = 40",
                                                                                              "N = 12500, N_{iter} = 16",
                                                                                              "N = 25000, N_{iter} = 8"))
          
          tibble_all_runs_avg <-  tibble_all_runs_avg %>%
                                  dplyr::group_by(N, device) %>%
                                  dplyr::mutate( time_at_min_n_threads = first(time_mean[n_threads == min(n_threads)]) ) %>%
                                  dplyr::mutate( perfect_scaling_time  = time_at_min_n_threads ) %>%
                                  dplyr::ungroup() %>% 
                                  print(n = 1000)
          
          tibble_all_runs_avg <-  tibble_all_runs_avg %>% dplyr::mutate(  iter_per_second = n_iter /  time_mean,
                                                                          iter_per_second_adj_for_N = N * iter_per_second,
                                                                          n_threads_num = as.numeric(n_threads), 
                                                                          n_threads = factor(n_threads_num),
                                                                          n_chunks_num = as.numeric(n_chunks),
                                                                          n_chunks = factor(n_chunks_num),
                                                                          eff = n_threads_num / time_mean,
                                                                          scaling_ratio =  n_threads_num /  (time_mean/perfect_scaling_time),
                                                                          # total_n_iter_across_all_threads_per_sec = total_n_iter_across_all_threads / time_mean,
                                                                          # total_n_iter_across_all_threads_per_sec_recip = 1.0 / total_n_iter_across_all_threads_per_sec
                                                                          )
        }
      
      ##  ----------------- Make + save "table 1(a)"  ------------------------------------------------------------------------------------------------------------------
      {
        message(paste("Tibble/table showing the optimal N_{chunks} for each N and each N_{threads}, for the local HPC:"))
        ##
        tibble_1_showing_optimal_n_chunks_per_N_for_HPC <- tibble_all_runs_avg %>%
          group_by(device, N, n_threads)  %>%
          mutate(best_ind = ifelse(time_mean == min(time_mean), 1, 0)) %>% 
          dplyr::filter(best_ind == 1, device == "HPC") %>% 
          print(n = 1000)
        ## Save tibble:
        file_name <- file.path(output_path, "tibble_1_showing_optimal_n_chunks_per_N_for_HPC")
        saveRDS(object = tibble_1_showing_optimal_n_chunks_per_N_for_HPC, file = file_name)
      }
      
      ##  ----------------- Make + save "table 1(b)"  ------------------------------------------------------------------------------------------------------------------
      {
        message(paste("Tibble/table showing the optimal N_{chunks} for each N and each N_{threads}, for the Laptop::"))
        ##
        tibble_1_showing_optimal_n_chunks_per_N_for_Laptop <- tibble_all_runs_avg %>%
          group_by(device, N, n_threads)  %>%
          mutate(best_ind = ifelse(time_mean == min(time_mean), 1, 0)) %>% 
          dplyr::filter(best_ind == 1, device == "Laptop") %>% 
          print(n = 1000)
        ## Save tibble:
        file_name <- file.path(output_path, "tibble_1_showing_optimal_n_chunks_per_N_for_Laptop")
        saveRDS(object = tibble_1_showing_optimal_n_chunks_per_N_for_Laptop, file = file_name)
      }
      
      
      
      
      ##  ----------------- Make + save "plot 1"  ------------------------------------------------------------------------------------------------------------------
      ## NOTE: This is a plot of N_{chunks} vs. time (in sec) for all N at ** ALL ** OF THE TESTED N_{threads} for each device 
      ## (64 for HPC, and either 8 or 16 for Laptop). 
      plot_1_outs <- R_fn_plot_ps_N_chunks_make_ggplot_1(    tibble_all_runs_avg = tibble_all_runs_avg,
                                                             device = "HPC",
                                                             save_plot = TRUE,
                                                             plot_overall_scale_factor = 4,
                                                             plot_width_scale_factor = 4,
                                                             plot_height_scale_factor = 3, 
                                                             plot_PPI = 400,
                                                             output_path =     pilot_study_opt_N_chunks_list$output_path )
      ##
      plot_panel_1_opt_N_chunks_at_the_every_tested_thread_count  <- plot_1_outs$plot
      plot_panel_1_opt_N_chunks_at_the_every_tested_thread_count
 
      
      ##  ----------------- Make + save "table 2(a)"  ------------------------------------------------------------------------------------------------------------------
      {
        message(paste("Tibble/table showing the optimal N_{chunks} ** and ** optimal N_{threads} combination for each N, for the local HPC:"))
        ##
        tibble_2_showing_optimal_n_chunks_and_n_threads_combo_per_N_for_HPC <- tibble_all_runs_avg %>%
          group_by(device, N)  %>%
          mutate(best_ind = ifelse(eff == max(eff), 1, 0)) %>% 
          dplyr::filter(best_ind == 1, device == "HPC") %>% 
          print(n = 1000)
        ## Save tibble:
        file_name <- file.path(output_path, "tibble_2_showing_optimal_n_chunks_and_n_threads_combo_per_N_for_HPC")
        saveRDS(object = tibble_2_showing_optimal_n_chunks_and_n_threads_combo_per_N_for_HPC, file = file_name)
      }
      
      ##  ----------------- Make + save "table 2(b)"  ------------------------------------------------------------------------------------------------------------------
      {
        message(paste("Tibble/table showing the optimal N_{chunks} ** and ** optimal N_{threads} combination for each N, for the local Laptop:"))
        ##
        tibble_2_showing_optimal_n_chunks_and_n_threads_combo_per_N_for_Laptop <- tibble_all_runs_avg %>%
          group_by(device, N)  %>%
          mutate(best_ind = ifelse(eff == max(eff), 1, 0)) %>% 
          dplyr::filter(best_ind == 1, device == "Laptop") %>% 
          print(n = 1000)
        ## Save tibble:
        file_name <- file.path(output_path, "tibble_2_showing_optimal_n_chunks_and_n_threads_combo_per_N_for_Laptop")
        saveRDS(object = tibble_2_showing_optimal_n_chunks_and_n_threads_combo_per_N_for_Laptop, file = file_name)
      }
 
      
      chosen_n_threads_HPC <- 64
      chosen_n_threads_Laptop <- 16
      
      ##  ----------------- Make + save "plot 2"  ------------------------------------------------------------------------------------------------------------------
      ## NOTE: This is a plot of N_{chunks} vs. time (in sec) for all N at the ** CHOSEN ** (i.e., the "optimal") # OF THREADS for each device 
      ## (64 for HPC, and either 8 or 16 for Laptop). 
      plot_2_outs <- R_fn_plot_ps_N_chunks_make_ggplot_2(    tibble_all_runs_avg = tibble_all_runs_avg,
                                                        chosen_n_threads_HPC = chosen_n_threads_HPC,
                                                        chosen_n_threads_Laptop = chosen_n_threads_Laptop,
                                                        save_plot = TRUE,
                                                        plot_overall_scale_factor = 4,
                                                        plot_width_scale_factor = 4,
                                                        plot_height_scale_factor = 3, 
                                                        plot_PPI = 400,
                                                        output_path =     pilot_study_opt_N_chunks_list$output_path )
      ##
      df_subset_at_chosen_n_threads  <- plot_2_outs$df_subset_at_chosen_n_threads
      tibble_all_runs_avg_subset_1  <-  plot_2_outs$tibble_all_runs_avg_subset_1
      tibble_all_runs_avg_subset_2  <-  plot_2_outs$tibble_all_runs_avg_subset_2
      plot_panel_2_opt_N_chunks_at_the_opt_thread_count  <- plot_2_outs$plot
      plot_panel_2_opt_N_chunks_at_the_opt_thread_count
      
  
          
      return(list( ## Outputs for part 1:
                   tibble_1_showing_optimal_n_chunks_per_N_for_HPC = tibble_1_showing_optimal_n_chunks_per_N_for_HPC,
                   tibble_1_showing_optimal_n_chunks_per_N_for_Laptop = tibble_1_showing_optimal_n_chunks_per_N_for_Laptop,
                   plot_panel_1_opt_N_chunks_at_the_every_tested_thread_count = plot_panel_1_opt_N_chunks_at_the_every_tested_thread_count, 
                   ## Outputs for part 2:
                   tibble_2_showing_optimal_n_chunks_and_n_threads_combo_per_N_for_HPC = tibble_2_showing_optimal_n_chunks_and_n_threads_combo_per_N_for_HPC,
                   tibble_2_showing_optimal_n_chunks_and_n_threads_combo_per_N_for_Laptop = tibble_2_showing_optimal_n_chunks_and_n_threads_combo_per_N_for_Laptop,
                   plot_panel_2_opt_N_chunks_at_the_opt_thread_count = plot_panel_2_opt_N_chunks_at_the_opt_thread_count))
                  
                 
  
 

}

































