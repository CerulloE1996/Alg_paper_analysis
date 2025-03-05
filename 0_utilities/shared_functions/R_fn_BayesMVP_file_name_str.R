



R_fn_make_BayesMVP_BASELINE_file_name_string <- function(  run_type,
                                                           computer, 
                                                           N, 
                                                           seed, 
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
  
  
 

          ## Helper function:
          bool_to_int <- function(bool) { 
            if (bool == FALSE) { 
              bool_as_int <- 0
            } else { 
              bool_as_int <- 1
            }
          }
          
          metric_type_main_to_save <- ifelse(metric_type_main == "Hessian", "HESS", "EMP")
          metric_shape_main_to_save <- ifelse(metric_shape_main == "dense", "DENSE", "DIAG")
          
          
          
          BayesMVP_BASELINE_file_name_string <- paste0(    "run_type", run_type, "_",
                                                           "PC", computer, "_",
                                                           "N",  N, "_",
                                                           "seed", seed, "_",  
                                                           "Mod", BayesMVP_Model_type, "_",   
                                                           "diffHMC", bool_to_int(diffusion_HMC), "_",   
                                                           "partHMC", bool_to_int(partitioned_HMC), "_",   
                                                           "SIMD", vect_type, "_",
                                                           ##
                                                           "forceAD",  bool_to_int(force_autodiff), "_",
                                                           "forcePLOG",  bool_to_int(force_PartialLog), "_",
                                                           "multiA",  bool_to_int(multi_attempts), "_",
                                                           ##
                                                           "priorLKJ", prior_LKJ[1],"and", prior_LKJ[2], "_",  
                                                           "posCOR", as.integer(corr_force_positive),  "_",  
                                                           ##
                                                           "burn_chains", n_chains_burnin, "_",
                                                           "pb_chains", n_chains_sampling, "_",
                                                           "burn", n_burnin, "_",
                                                           "samp", n_iter, "_",
                                                           "ad", adapt_delta, "_",
                                                           "LR", learning_rate, "_", 
                                                           "clip", clip_iter, "_", 
                                                           "Mmain", metric_shape_main_to_save, metric_type_main_to_save)
          
          
          
          return(BayesMVP_BASELINE_file_name_string)
          
          
}