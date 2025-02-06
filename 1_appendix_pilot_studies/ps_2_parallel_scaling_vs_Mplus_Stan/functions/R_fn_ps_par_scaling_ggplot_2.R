


# -| ------------------------------   Plot for finding the optimal N_{chunks} and N_{threads} ** combo ** for each N, for BayesMVP  MVP-LC model--------------------------------------------------------------------

R_fn_ps_par_scaling_ggplot_2 <- function(       tibble_all_runs_avg, 
                                                chosen_n_threads_HPC,
                                                chosen_n_threads_Laptop, 
                                                save_plot = TRUE,
                                                plot_overall_scale_factor,
                                                plot_width_scale_factor,
                                                plot_height_scale_factor,
                                                plot_PPI,
                                                output_path) {
  
  
           df_subset_at_chosen_n_threads <- dplyr::filter(tibble_all_runs_avg, 
                                                 ((device == "Laptop") & (n_threads == chosen_n_threads_Laptop)) |
                                                 ((device == "Local_HPC")    & (n_threads == chosen_n_threads_HPC)))
           ##
           df_subset_at_chosen_n_threads_Laptop <- df_subset_at_chosen_n_threads %>%
             dplyr::filter(device == "Laptop")
           ##
           df_subset_at_chosen_n_threads_HPC <- df_subset_at_chosen_n_threads %>%
             dplyr::filter(device == "Local_HPC")
           ##
    
            ##  ---------------------------------- "plot 2"
            ##
            { ##  -------- plot 2: panel (a) ---- Panel for Laptop
              ##
              # tibble_all_runs_avg_subset_1 <- dplyr::filter(df_subset_at_chosen_n_threads, N %in%(c(500, 1000, 2500)))
              # tibble_all_runs_avg_subset_2 <- dplyr::filter(df_subset_at_chosen_n_threads, N %in%(c(5000, 12500, 25000)))
              ##
              plot_panel_1 <-   ggplot(    df_subset_at_chosen_n_threads,
                                                                   mapping = aes(x =  N,
                                                                                 y = scaling_ratio,
                                                                                 colour = algorithm,
                                                                                 group = algorithm
                                                                                 ),
              ) +
                geom_point(size = 4) +
                geom_line(size = 2) + 
                # geom_errorbar(size = 1, width = 0.02, aes(x = N,
                #                                           y = scaling_ratio,
                #                                           ymin = scaling_ratio -  1.96 * time_SD,
                #                                           ymax = scaling_ratio +  1.96 * time_SD,
                #                                           colour = N_label,
                #                                           linetype = device
                # )) +
                # geom_line(size = 1,                aes(x = N,
                #                                        y = scaling_ratio, 
                #                                        colour = algorithm,
                #                                        linetype = device
                # )) +
                theme_bw(base_size = 16) +
                theme(legend.position = "bottom")  + 
                ylab("scaling_ratio") + 
                xlab("N" ) + 
                facet_wrap( ~ device, scales = "free") + 
                theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
                scale_x_continuous( breaks = c(500, 1000, 2500, 5000, 12500, 25000),       
                                    trans = 'log10')
              
                plot_panel_1
              
            }
            
          
          
          # 
          #  { ## -------- plot 2: panel (b) ----  Panel for HPC
          # 
          #    plot_panel_2 <-   ggplot(  df_subset_at_chosen_n_threads_HPC,
          #                               mapping = aes(x =  N,
          #                                             y = scaling_ratio,
          #                                             colour = algorithm,
          #                                             group = algorithm
          #                               ),
          #     ) +
          #       geom_point(size = 4) +
          #       geom_line(size = 2) + 
          #       # geom_errorbar(size = 1, width = 0.02, aes(x = N,
          #       #                                           y = scaling_ratio,
          #       #                                           ymin = scaling_ratio -  1.96 * time_SD,
          #       #                                           ymax = scaling_ratio +  1.96 * time_SD,
          #       #                                           colour = N_label,
          #       #                                           linetype = device
          #       # )) +
          #       # geom_line(size = 1,                aes(x = N,
          #       #                                        y = scaling_ratio,
          #       #                                        colour = N_label,
          #       #                                        linetype = device
          #       # )) +
          #       # geom_line(size = 2) +
          #       theme_bw(base_size = 16) +
          #       theme(legend.position = "bottom")  +
          #       ylab("scaling_ratio") +
          #       xlab("N" ) +
          #       #### facet_wrap( ~ N_and_N_iter_label, scales = "free") +
          #       theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
          # 
          #       plot_panel_2
          # 
          # }
          
          
          
           plot <- plot_panel_1  ### +  plot_panel_2 + plot_layout(ncol = 2)
           plot
           ## plot <- plot_N_chunks_pilot_study_HPC_plot_1
           
          ## save plot:
          {
            {
              plot_width <-  plot_width_scale_factor  * plot_overall_scale_factor
              plot_height <- plot_height_scale_factor * plot_overall_scale_factor
            }
            ##
            file_name <- file.path(output_path, "Figure_parallel_scaling_ps_plot_2.png")
            ##
            png(filename = file_name, 
                units = "in", 
                width = plot_width, 
                height =  plot_height, 
                res = plot_PPI)
            ##
            print(plot)
            ##
            dev.off()
            ##
          }
          
          
          
          return(list(df_subset_at_chosen_n_threads = df_subset_at_chosen_n_threads,
                      # tibble_all_runs_avg_subset_1 = tibble_all_runs_avg_subset_1,
                      # tibble_all_runs_avg_subset_2 = tibble_all_runs_avg_subset_2,
                      plot = plot))
  
}








