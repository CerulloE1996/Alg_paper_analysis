 



  
R_fn_ps3_target_ESS_make_tibbles <-  function(  pilot_study_target_ESS_list,
                                              
                                              
                                                BayesMVP_Model_type,
                                                diffusion_HMC,
                                                partitioned_HMC,
                                                vect_type, 
                                                force_autodiff, 
                                                force_PartialLog, 
                                                multi_attempts, 
                                                prior_LKJ, 
                                                corr_force_positive, 
                                                n_chains_burnin, 
                                                n_chains_sampling,
                                                n_burnin,
                                                n_iter,
                                                adapt_delta,
                                                learning_rate,
                                                clip_iter, 
                                                metric_type_main,
                                                metric_shape_main
) {
     
  
  computer <- "Local_HPC"
  run_type <- "ps3"

  n_runs <- pilot_study_target_ESS_list$n_runs
  
  
  diffusion_HMC <- pilot_study_target_ESS_list$diffusion_HMC
  partitioned_HMC <- pilot_study_target_ESS_list$partitioned_HMC
  ##
  force_autodiff <- pilot_study_target_ESS_list$force_autodiff
  force_PartialLog <- pilot_study_target_ESS_list$force_PartialLog
  multi_attempts <- pilot_study_target_ESS_list$multi_attempts
  
  for (seed in 1:n_runs) {
    for (N in N_vec) {
        BayesMVP_BASELINE_file_name_string <- R_fn_make_BayesMVP_BASELINE_file_name_string(  run_type = run_type,
                                                                                             computer = computer,
                                                                                             N = N,
                                                                                             seed = seed,
                                                                                             BayesMVP_Model_type = BayesMVP_Model_type,
                                                                                             diffusion_HMC = diffusion_HMC,
                                                                                             partitioned_HMC = partitioned_HMC,
                                                                                             vect_type = vect_type,
                                                                                             force_autodiff = force_autodiff,
                                                                                             force_PartialLog = force_PartialLog,
                                                                                             multi_attempts = multi_attempts,
                                                                                             prior_LKJ = prior_LKJ,
                                                                                             corr_force_positive = corr_force_positive,
                                                                                             n_chains_burnin = n_chains_burnin,
                                                                                             n_chains_sampling = n_chains_sampling,
                                                                                             n_burnin = n_burnin,
                                                                                             n_iter = n_iter,
                                                                                             adapt_delta = adapt_delta,
                                                                                             learning_rate = learning_rate,
                                                                                             clip_iter = clip_iter,
                                                                                             metric_type_main = metric_type_main,
                                                                                             metric_shape_main = metric_shape_main)
  
    }
  }
  
  
           ## Helper fn:
           Range <- function(x) {
               return(max(x) - min(x))
           }
             
           ## Compute IQR and Range for all "important test accuracy" parameters (i.e., Se, Sp, and disease prevalence):
           iqr_vec <- Range_vec <- rep(NA, 11)
           counter <- 0
           {
                 ## For Se estimates:
                 for (t in 1:n_tests) { 
                     counter <- counter + 1
                     param_name <- paste0("Se", t, "_", "Mean")
                     param_vec <- c(df_alg_efficiency_all_runs[[param_name]]) * 100
                     iqr_vec[counter] <-    IQR(param_vec)
                     Range_vec[counter] <-  Range(param_vec)
                     names(iqr_vec)[counter] <-   param_name
                     names(Range_vec)[counter] <- param_name
                 }
                 ## For Sp estimates:
                 for (t in 1:n_tests) { 
                     counter <- counter + 1
                     param_name <- paste0("Sp", t, "_", "Mean")
                     param_vec <- c(df_alg_efficiency_all_runs[[param_name]]) * 100
                     iqr_vec[counter] <-  IQR(param_vec)
                     Range_vec[counter] <-  Range(param_vec)
                     names(iqr_vec)[counter] <-   param_name
                     names(Range_vec)[counter] <- param_name
                 }
                 ## For p (disease prevalence) estimate:
                 {
                     counter <- counter + 1
                     param_name <- paste0("p", "_", "Mean")
                     param_vec <- c(df_alg_efficiency_all_runs[[param_name]]) * 100
                     iqr_vec[counter] <-  IQR(param_vec)
                     Range_vec[counter] <-  Range(param_vec)
                     names(iqr_vec)[counter] <-   param_name
                     names(Range_vec)[counter] <- param_name
                 }
             
             {
                 # ## Print:
                 # print(paste("iqr_vec = ", iqr_vec))
                 # print(paste("Range_vec = ", Range_vec))
                 ## Compute summaries across the 10 runs:
                 Max_IQR <- max(iqr_vec)
                 Max_Range <- max(Range_vec)
                 ## Print:
                 message(paste("Max_IQR = ", Max_IQR))
                 message(paste("Max_IQR_param= ")) ;  print(which(iqr_vec == Max_IQR))
                 message(paste("Max_Range = ", Max_Range))
                 message(paste("Max_Range_param= ")) ;  print(which(Range_vec == Max_Range))
                 ##
                 n_iter <- mean(df_alg_efficiency_all_runs$n_iter)
                 Min_ESS_avg <- round(mean(df_alg_efficiency_all_runs$Min_ESS), 0)
                 message(paste(paste0("Min_ESS_avg (@ ", n_iter, " iter) = ",  Min_ESS_avg)))
             }
             
           
           }
           
}
           
       
     
                         
                           # {
                           #   for (t in 1:n_tests) { 
                           #     ## Save Se estimates:
                           #     row <- dplyr::filter(BayesMVP_Stan_like_summary_tibble_gq, parameter == paste0("Se_bin", ".", t))
                           #     df_list_alg_efficiency_for_run_iii[[as.character(paste0("Se", t, "_", "Median"))]] <-  row$`50%`
                           #     df_list_alg_efficiency_for_run_iii[[as.character(paste0("Se", t, "_", "Mean"))]] <-  row$mean
                           #     df_list_alg_efficiency_for_run_iii[[as.character(paste0("Se", t, "_", "lower95"))]] <- row$`2.5%`
                           #     df_list_alg_efficiency_for_run_iii[[as.character(paste0("Se", t, "_", "upper95"))]] <- row$`97.5%`
                           #     ## Save Sp estimates:
                           #     row <- dplyr::filter(BayesMVP_Stan_like_summary_tibble_gq, parameter == paste0("Sp_bin", ".", t))
                           #     df_list_alg_efficiency_for_run_iii[[as.character(paste0("Sp", t, "_", "Median"))]] <- row$`50%`
                           #     df_list_alg_efficiency_for_run_iii[[as.character(paste0("Sp", t, "_", "Mean"))]] <- row$mean
                           #     df_list_alg_efficiency_for_run_iii[[as.character(paste0("Sp", t, "_", "lower95"))]] <- row$`2.5%`
                           #     df_list_alg_efficiency_for_run_iii[[as.character(paste0("Sp", t, "_", "upper95"))]] <- row$`97.5%`
                           #   }
                           #   ## Save prev estimate:
                           #   row <-    dplyr::filter(BayesMVP_Stan_like_summary_tibble_gq, parameter == paste0("p", ".", "1"))
                           #   df_list_alg_efficiency_for_run_iii[[as.character(paste0("p", "_", "Median"))]] <- row$`50%`
                           #   df_list_alg_efficiency_for_run_iii[[as.character(paste0("p", "_", "Mean"))]] <- row$mean
                           #   df_list_alg_efficiency_for_run_iii[[as.character(paste0("p", "_", "lower95"))]] <- row$`2.5%`
                           #   df_list_alg_efficiency_for_run_iii[[as.character(paste0("p", "_", "upper95"))]] <- row$`97.5%`
                           # }
         
         # ##
         # max_IQR = NA_real_,
         # max_IQR_index =NA_real_,
         # max_Range = NA_real_,
         # max_Range_index = NA_real_, 
                       
             
 








            




