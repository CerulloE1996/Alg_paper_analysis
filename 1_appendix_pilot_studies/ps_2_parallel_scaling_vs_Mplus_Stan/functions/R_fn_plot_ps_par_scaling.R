


# -| ------------------------------    Pilot study - Finding the optimal N_{chunks} for BayesMVP --------------------------------------------------------------------
 



R_fn_ps_par_scaling_plots <- function(      tibble_all_runs,
                                            pilot_study_parallel_scaling_comp_list,
                                            global_list,
                                            output_path,
                                            plot_1_overall_scale_factor,
                                            plot_1_width_scale_factor,
                                            plot_1_height_scale_factor, 
                                            plot_1_PPI,
                                            plot_2_overall_scale_factor,
                                            plot_2_width_scale_factor,
                                            plot_2_height_scale_factor, 
                                            plot_2_PPI) { 
      
 
  
      
      tibble_all_runs_avg <-  tibble_all_runs %>%
                              dplyr::group_by(device, algorithm, df_index, N, n_iter, n_threads, n_chunks) %>%
                              dplyr::summarise( time_mean = mean(time_sec, na.rm = TRUE),
                                                time_median = median(time_sec, na.rm = TRUE),
                                                time_SD = sd(time_sec, na.rm = TRUE),
                                                .groups = 'drop')
      
      tibble_all_runs_avg %>% print(n = 1000)
      
      
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
                                                                          scaling_ratio =  perfect_scaling_time * eff
                                                                          # total_n_iter_across_all_threads_per_sec = total_n_iter_across_all_threads / time_mean,
                                                                          # total_n_iter_across_all_threads_per_sec_recip = 1.0 / total_n_iter_across_all_threads_per_sec
                                                                          )
          
          tibble_all_runs_avg <- tibble_all_runs_avg %>% dplyr::mutate( algorithm = ifelse((algorithm == "MD_BayesMVP") & (n_chunks_num != 1), 
                                                                                           "MD_BayesMVP_chunk", 
                                                                                           as.character(algorithm)))
          tibble_all_runs_avg$algorithm <- factor(tibble_all_runs_avg$algorithm)
          tibble_all_runs_avg$algorithm
          
          tibble_all_runs_avg %>% dplyr::filter(algorithm == "Mplus_WCP")
          tibble_all_runs_avg <- tibble_all_runs_avg %>% dplyr::filter( !((algorithm == "Mplus_WCP") & (time_mean < 0.01)) ) %>% print(n=100)
          
      }
      
      
      filter(tibble_all_runs_avg, algorithm %in% c("MD_BayesMVP", "MD_BayesMVP_chunk")) %>% print(n = 1000)
      
      # ##  ----------------- Make + save "table 1(a)"  ------------------------------------------------------------------------------------------------------------------
      # {
      #   message(paste("Tibble/table showing the optimal N_{chunks} for each N and each N_{threads}, for the local HPC:"))
      #   ##
      #   tibble_1_showing_optimal_n_chunks_per_N_for_HPC <-  tibble_all_runs_avg %>%
      #                                                       dplyr::filter(device == "Local_HPC", N == 500) %>% 
      #                                                       # group_by(device, N, n_threads)  %>%
      #                                                       # mutate(best_ind = ifelse(time_mean == min(time_mean), 1, 0)) %>% 
      #                                                       # dplyr::filter(best_ind == 1, device == "Local_HPC") %>% 
      #                                                       print(n = 1000)
      #   ## Save tibble:
      #   file_name <- file.path(output_path, "tibble_1_showing_optimal_n_chunks_per_N_for_HPC")
      #   saveRDS(object = tibble_1_showing_optimal_n_chunks_per_N_for_HPC, file = file_name)
      # }
      # 
      # ##  ----------------- Make + save "table 1(b)"  ------------------------------------------------------------------------------------------------------------------
      # {
      #   message(paste("Tibble/table showing the optimal N_{chunks} for each N and each N_{threads}, for the Laptop::"))
      #   ##
      #   tibble_1_showing_optimal_n_chunks_per_N_for_Laptop <- tibble_all_runs_avg %>%
      #     group_by(device, N, n_threads)  %>%
      #     mutate(best_ind = ifelse(time_mean == min(time_mean), 1, 0)) %>% 
      #     dplyr::filter(best_ind == 1, device == "Laptop") %>% 
      #     print(n = 1000)
      #   ## Save tibble:
      #   file_name <- file.path(output_path, "tibble_1_showing_optimal_n_chunks_per_N_for_Laptop")
      #   saveRDS(object = tibble_1_showing_optimal_n_chunks_per_N_for_Laptop, file = file_name)
      # }
      # 
      
      
      
      ##  ----------------- Make + save "plot 1"  ------------------------------------------------------------------------------------------------------------------
      ## NOTE: This is a plot of N_{chunks} vs. time (in sec) for all N at ** ALL ** OF THE TESTED N_{threads} for each device 
      ## (64 for HPC, and either 8 or 16 for Laptop). 
      plot_1_outs <- R_fn_ps_par_scaling_ggplot_1(     tibble_all_runs_avg = tibble_all_runs_avg,
                                                       save_plot = TRUE,
                                                       plot_overall_scale_factor = plot_1_overall_scale_factor,
                                                       plot_width_scale_factor =   plot_1_width_scale_factor,
                                                       plot_height_scale_factor =  plot_1_height_scale_factor, 
                                                       plot_PPI = plot_1_PPI,
                                                       output_path =     pilot_study_parallel_scaling_comp_list$output_path )
      ##
      plot_panel_1_paralell_scaling_comparison  <- plot_1_outs$plot
 
      
      # ##  ----------------- Make + save "table 2(a)"  ------------------------------------------------------------------------------------------------------------------
      # {
      #   message(paste("Tibble/table showing the optimal N_{chunks} ** and ** optimal N_{threads} combination for each N, for the local HPC:"))
      #   ##
      #   tibble_2_showing_optimal_n_chunks_and_n_threads_combo_per_N_for_HPC <- tibble_all_runs_avg %>%
      #     group_by(device, N)  %>%
      #     mutate(best_ind = ifelse(eff == max(eff), 1, 0)) %>% 
      #     dplyr::filter(best_ind == 1, device == "HPC") %>% 
      #     print(n = 1000)
      #   ## Save tibble:
      #   file_name <- file.path(output_path, "tibble_2_showing_optimal_n_chunks_and_n_threads_combo_per_N_for_HPC")
      #   saveRDS(object = tibble_2_showing_optimal_n_chunks_and_n_threads_combo_per_N_for_HPC, file = file_name)
      # }
      # 
      # ##  ----------------- Make + save "table 2(b)"  ------------------------------------------------------------------------------------------------------------------
      # {
      #   message(paste("Tibble/table showing the optimal N_{chunks} ** and ** optimal N_{threads} combination for each N, for the local Laptop:"))
      #   ##
      #   tibble_2_showing_optimal_n_chunks_and_n_threads_combo_per_N_for_Laptop <- tibble_all_runs_avg %>%
      #     group_by(device, N)  %>%
      #     mutate(best_ind = ifelse(eff == max(eff), 1, 0)) %>% 
      #     dplyr::filter(best_ind == 1, device == "Laptop") %>% 
      #     print(n = 1000)
      #   ## Save tibble:
      #   file_name <- file.path(output_path, "tibble_2_showing_optimal_n_chunks_and_n_threads_combo_per_N_for_Laptop")
      #   saveRDS(object = tibble_2_showing_optimal_n_chunks_and_n_threads_combo_per_N_for_Laptop, file = file_name)
      # }
      # 
      # 
      chosen_n_threads_HPC <- 64
      chosen_n_threads_Laptop <- 16
      # 
      ##  ----------------- Make + save "plot 2"  ------------------------------------------------------------------------------------------------------------------
      ## NOTE: This is a plot of N_{chunks} vs. time (in sec) for all N at the ** CHOSEN ** (i.e., the "optimal") # OF THREADS for each device
      ## (64 for HPC, and either 8 or 16 for Laptop).
      plot_2_outs <- R_fn_ps_par_scaling_ggplot_2(      tibble_all_runs_avg = tibble_all_runs_avg,
                                                        chosen_n_threads_HPC = chosen_n_threads_HPC,
                                                        chosen_n_threads_Laptop = chosen_n_threads_Laptop,
                                                        save_plot = TRUE,
                                                        plot_overall_scale_factor = plot_2_overall_scale_factor,
                                                        plot_width_scale_factor =   plot_2_width_scale_factor,
                                                        plot_height_scale_factor =  plot_2_height_scale_factor, 
                                                        plot_PPI = plot_2_PPI,
                                                        output_path =     pilot_study_parallel_scaling_comp_list$output_path )
      
      plot_panel_2_paralell_scaling_comparison_at_chosen_n_threads  <- plot_2_outs$plot
      ##
      df_subset_at_chosen_n_threads  <- plot_2_outs$df_subset_at_chosen_n_threads
      ##
      
  
          
      return(list( ## Outputs for part 1:
                   # tibble_1_showing_optimal_n_chunks_per_N_for_HPC = tibble_1_showing_optimal_n_chunks_per_N_for_HPC,
                   # tibble_1_showing_optimal_n_chunks_per_N_for_Laptop = tibble_1_showing_optimal_n_chunks_per_N_for_Laptop,
                   plot_panel_1_paralell_scaling_comparison = plot_panel_1_paralell_scaling_comparison,
                   # ## Outputs for part 2:
                   # tibble_2_showing_optimal_n_chunks_and_n_threads_combo_per_N_for_HPC = tibble_2_showing_optimal_n_chunks_and_n_threads_combo_per_N_for_HPC,
                   # tibble_2_showing_optimal_n_chunks_and_n_threads_combo_per_N_for_Laptop = tibble_2_showing_optimal_n_chunks_and_n_threads_combo_per_N_for_Laptop,
                   df_subset_at_chosen_n_threads = df_subset_at_chosen_n_threads,
                   plot_panel_2_paralell_scaling_comparison_at_chosen_n_threads = plot_panel_2_paralell_scaling_comparison_at_chosen_n_threads
                   ))
                  
                 
  
 

}

































