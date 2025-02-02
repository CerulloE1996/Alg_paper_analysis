



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







