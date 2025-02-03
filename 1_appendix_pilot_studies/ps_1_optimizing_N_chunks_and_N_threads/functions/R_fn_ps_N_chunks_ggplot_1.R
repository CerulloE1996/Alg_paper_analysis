


# -| ------------------------------    Pilot study - Finding the optimal N_{chunks} (for each N_{thread} and N) for BayesMVP --------------------------------------------------------------------

R_fn_plot_ps_N_chunks_make_ggplot_1 <- function(tibble_all_runs_avg, 
                                                device,
                                                save_plot = TRUE,
                                                plot_overall_scale_factor = 4,
                                                plot_width_scale_factor = 4,
                                                plot_height_scale_factor = 3,
                                                plot_PPI = 400,
                                                output_path) {
 
  
          ##  ---------------------------------- "plot 1"
          ## 
          tibble_all_runs_avg_initial_subset_HPC <-    dplyr::filter(tibble_all_runs_avg, device ==  "HPC")
          tibble_all_runs_avg_initial_subset_Laptop <- dplyr::filter(tibble_all_runs_avg, device ==  "Laptop")
          # ##
          # tibble_all_runs_avg_subset_N_500_HPC    <- dplyr::filter(tibble_all_runs_avg_initial_subset_HPC, N == 500)
          # tibble_all_runs_avg_subset_N_1000_HPC   <- dplyr::filter(tibble_all_runs_avg_initial_subset_HPC, N == 1000)
          # tibble_all_runs_avg_subset_N_2500_HPC   <- dplyr::filter(tibble_all_runs_avg_initial_subset_HPC, N == 2500)
          # tibble_all_runs_avg_subset_N_5000_HPC   <- dplyr::filter(tibble_all_runs_avg_initial_subset_HPC, N == 5000)
          # tibble_all_runs_avg_subset_N_12500_HPC  <- dplyr::filter(tibble_all_runs_avg_initial_subset_HPC, N == 12500)
          # tibble_all_runs_avg_subset_N_25000_HPC  <- dplyr::filter(tibble_all_runs_avg_initial_subset_HPC, N == 25000)
          ##
          { ##  -------- plot 1: panel (a) ---- Panel for N = {500, 1000, 2500}
                print(paste("Plot for HPC data"))
                plot_panel_HPC <-   ggplot(    tibble_all_runs_avg_initial_subset_HPC,
                                                                                     mapping = aes(x =  (n_chunks),
                                                                                                   y = eff,
                                                                                                   colour =  n_threads,
                                                                                                   group  =  n_threads,
                                                                                                   shape = device
                                                                                     ),
                ) +
                  geom_point(size = 4) +
                  geom_line(size = 1,                aes(x = n_chunks,
                                                         y = eff, 
                                                         colour =  n_threads,
                                                         linetype =  n_threads
                  )) +
                  theme_bw(base_size = 16) +
                  theme(legend.position = "bottom")  + 
                  ylab("n_threads / time") + 
                  xlab("N_{chunks}" ) + 
                  facet_wrap(~ N_and_N_iter_label, scales = "free") + 
                  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
                
                  plot_panel_HPC
            
          }
          
          
          
          
          { ## -------- plot 1: panel (b) ----  Panel for N = {5000, 12500, 25000}
            
                  print(paste("Plot for Laptop data"))
                  plot_panel_Laptop <-   ggplot(  tibble_all_runs_avg_initial_subset_Laptop,  
                                                                                      mapping = aes(x =  (n_chunks),
                                                                                                    y = eff,
                                                                                                    colour =  n_threads,
                                                                                                    group  =  n_threads,
                                                                                                    shape = device 
                                                                                      ),
                ) +
                  geom_point(size = 4) + 
                  geom_line(size = 1,                aes(x = n_chunks,
                                                         y = eff, 
                                                         colour =  n_threads,
                                                         linetype =  n_threads
                  )) +
                  theme_bw(base_size = 16) +
                  theme(legend.position = "bottom")  + 
                  ylab("n_threads / time") + 
                  xlab("N_{chunks}" ) + 
                  #  scale_y_continuous(breaks = seq(from = 0, to = 10, by = 1)) + 
                  #  scale_x_continuous(breaks = seq(from = 0, to = 500, by = 1)) +
                  #  scale_color_manual(values = c("5000" = "orange", "12500" = "black", "25000" = "purple")) +  
                  facet_wrap(~ N_and_N_iter_label, scales = "free") + 
                  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
                
                  plot_panel_Laptop
            
          }
          
          
          
           plot <-  plot_panel_HPC + plot_panel_Laptop + plot_layout(ncol = 1)
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
          
          
          
          return(list( tibble_all_runs_avg_initial_subset_HPC = tibble_all_runs_avg_initial_subset_HPC,
                       tibble_all_runs_avg_initial_subset_Laptop = tibble_all_runs_avg_initial_subset_Laptop,
                      plot = plot))
  
}








