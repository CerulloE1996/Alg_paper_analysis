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






