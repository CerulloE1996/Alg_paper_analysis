


# -| ------------------------------   Plot for finding the optimal N_{chunks} and N_{threads} ** combo ** for each N, for BayesMVP  MVP-LC model--------------------------------------------------------------------

R_fn_plot_ps_N_chunks_make_ggplot_2 <- function(tibble_all_runs_avg, 
                                                chosen_n_threads_HPC,
                                                chosen_n_threads_Laptop, 
                                                save_plot = TRUE,
                                                plot_overall_scale_factor = 4,
                                                plot_width_scale_factor = 4,
                                                plot_height_scale_factor = 3,
                                                plot_PPI = 400,
                                                output_path) {
  
  
           df_subset_at_chosen_n_threads <- dplyr::filter(tibble_all_runs_avg, 
                                                 ((device == "Laptop") & (n_threads == chosen_n_threads_Laptop)) |
                                                 ((device == "HPC")    & (n_threads == chosen_n_threads_HPC)))
  
          ##  ---------------------------------- "plot 2"
          ##
          { ##  -------- plot 2: panel (a) ---- Panel for N = {500, 1000, 2500}
            ##
            tibble_all_runs_avg_subset_1 <- dplyr::filter(df_subset_at_chosen_n_threads, N %in%(c(500, 1000, 2500)))
            tibble_all_runs_avg_subset_2 <- dplyr::filter(df_subset_at_chosen_n_threads, N %in%(c(5000, 12500, 25000)))
            ##
            plot_N_chunks_pilot_study_HPC_plot_1_N_500_1000_2500 <-   ggplot(    tibble_all_runs_avg_subset_1,
                                                                                 mapping = aes(x =  (n_chunks),
                                                                                               y = time_mean,
                                                                                               # y = Time / N_iter,
                                                                                               # y =  iter_per_second_adj_for_N,
                                                                                               colour = N_label,
                                                                                               group = device
                                                                                 ),
            ) +
              geom_point(size = 4) +
              geom_errorbar(size = 1, width = 0.02, aes(x = n_chunks,
                                                        y = time_mean,
                                                        ymin = time_mean -  1.96 * time_SD,
                                                        ymax = time_mean +  1.96 * time_SD,
                                                        colour = N_label,
                                                        linetype = device
              )) +
              geom_line(size = 1,                aes(x = n_chunks,
                                                     y = time_mean, 
                                                     colour = N_label,
                                                     linetype = device
              )) +
              theme_bw(base_size = 16) +
              theme(legend.position = "bottom")  + 
              ylab("Time (sec)") + 
              xlab("N_{chunks}" ) + 
              #scale_y_continuous(breaks = seq(from = 0, to = 10, by = 1)) + 
              #   scale_x_continuous(breaks = seq(from = 0, to = 500, by = 1)) +
              facet_wrap( ~ N_and_N_iter_label, scales = "free") + 
              theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
            
            plot_N_chunks_pilot_study_HPC_plot_1_N_500_1000_2500
          }
          
          
          
          
           { ## -------- plot 2: panel (b) ----  Panel for N = {5000, 12500, 25000}
              
              plot_N_chunks_pilot_study_HPC_plot_1_N_5000_12500_25000 <-   ggplot(  tibble_all_runs_avg_subset_2,  
                                                                                    mapping = aes(x =  (n_chunks),
                                                                                                  y = time_mean,
                                                                                                  # y = Time / N_iter,
                                                                                                  # y =  iter_per_second_adj_for_N,
                                                                                                  colour = N_label,
                                                                                                  group = device
                                                                                    ),
              ) +
                geom_point(size = 4) + 
                geom_errorbar(size = 1, width = 0.02, aes(x = n_chunks,
                                                          y = time_mean, 
                                                          ymin = time_mean -  1.96 * time_SD,
                                                          ymax = time_mean +  1.96 * time_SD,
                                                          colour = N_label,
                                                          linetype = device
                )) +
                geom_line(size = 1,                aes(x = n_chunks,
                                                       y = time_mean, 
                                                       colour = N_label,
                                                       linetype = device
                )) +
                # geom_line(size = 2) + 
                theme_bw(base_size = 16) +
                theme(legend.position = "bottom")  + 
                ylab("Time (sec)") + 
                xlab("N_{chunks}" ) + 
                #  scale_y_continuous(breaks = seq(from = 0, to = 10, by = 1)) + 
                #  scale_x_continuous(breaks = seq(from = 0, to = 500, by = 1)) +
                #  scale_color_manual(values = c("5000" = "orange", "12500" = "black", "25000" = "purple")) +  
                facet_wrap( ~ N_and_N_iter_label, scales = "free") + 
                theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
              
              plot_N_chunks_pilot_study_HPC_plot_1_N_5000_12500_25000
            
          }
          
          
          
           plot <- plot_N_chunks_pilot_study_HPC_plot_1_N_500_1000_2500 +  plot_N_chunks_pilot_study_HPC_plot_1_N_5000_12500_25000 + plot_layout(ncol = 1)
           plot
          
          
          ## save plot:
          {
            {
              plot_width <-  plot_width_scale_factor* plot_overall_scale_factor
              plot_height <- plot_height_scale_factor*plot_overall_scale_factor
            }
            ##
            file_name <- file.path(output_path, "Figure_N_chunks_pilot_study_plot_1.png")
            ##
            png(filename = file_name, 
                units = "in", 
                width = plot_width, 
                height =  plot_height, 
                res = plot_PPI)
            ##
            plot
            ##
            dev.off()
            ##
          }
          
          
          
          return(list(df_subset_at_chosen_n_threads = df_subset_at_chosen_n_threads,
                      tibble_all_runs_avg_subset_1 = tibble_all_runs_avg_subset_1,
                      tibble_all_runs_avg_subset_2 = tibble_all_runs_avg_subset_2,
                      plot = plot))
  
}








