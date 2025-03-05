

#### ------- R function to compile Stan model  ------------------------------------------------------------------------------------------------------------------------------
     
    
# # #### For testing:
# # # BayesMVP_Model_type = "LC_MVP"
# # BayesMVP_Model_type = "Stan"
# # ##
# # global_list = global_list
# # ## Stan_data_list = NULL
# # Stan_model_file_path = NULL
# N_sample_size_of_dataset = 500
# # sample_nuisance = TRUE
# # set_custom_CXX_CPP_flags_if_using_Stan_model = TRUE
# # CCACHE_PATH = " "
# # # user_home_dir = NULL
# # # user_BayesMVP_dir = NULL
# # CXX_COMPILER_PATH = NULL
# # CPP_COMPILER_PATH = NULL
# # MATH_FLAGS = NULL
# # AVX_FLAGS = NULL
# # FMA_FLAGS = NULL
# # n_chains_burnin = 8
# 
# 


R_fn_compile_BayesMVP_model <- function(  BayesMVP_Model_type, 
                                          set_custom_optimised_CXX_CPP_flags = NULL,
                                          cmdstan_cpp_flags_if_using_Stan_model = NULL,
                                          global_list,
                                          Stan_data_list = NULL,
                                          Stan_model_file_path = NULL,
                                          N_sample_size_of_dataset,
                                          sample_nuisance,
                                          CCACHE_PATH = " ", 
                                          # user_home_dir = NULL,
                                          # user_BayesMVP_dir = NULL,
                                          CXX_COMPILER_PATH = NULL,
                                          CPP_COMPILER_PATH = NULL,
                                          MATH_FLAGS = NULL,
                                          FMA_FLAGS = NULL,
                                          AVX_FLAGS = NULL,
                                          n_chains_burnin = min(8, parallel::detectCores()) ## Dummy/can change later but needed for compilation
                                          )  {


          N_sample_sizes_vec <- global_list$N_sample_sizes_vec
          ## total_num_datasets <- length(N_sample_sizes_vec)
          ## Get data frame index (e.g. for this study if N = 500 then df_index = 1,)
          N <- N_sample_size_of_dataset
          df_index <- which(N_sample_sizes_vec == N_sample_size_of_dataset)
          ##
          n_params_main <- global_list$Model_settings_list$n_params_main
          print(paste("df_index = ", df_index))
          n_nuisance <- prod(dim(global_list$initial_values_list$inits_u_list[[df_index]]))
          n_params <- n_params_main + n_nuisance
          index_nuisance <- 1:n_nuisance
          index_main_params <- (n_nuisance + 1):n_params
          ##
          ## Set inits:
          inits_u_raw <- global_list$initial_values_list$inits_u_list[[df_index]]
          inits_Omega_unc_col_one_raw <- global_list$initial_values_list$inits_Omega_list$inits_Omega_unc_col_one_raw
          inits_Omega_unc_off_raw <- global_list$initial_values_list$inits_Omega_list$inits_Omega_unc_off_raw
          inits_beta_vec_raw <- global_list$initial_values_list$inits_beta_list$inits_beta_vec
          inits_prev_unc <- global_list$initial_values_list$inits_prev_list$inits_prev_unc
          ## Make Stan (and therefore BayesMVP) inits list:
          stan_inits_list <- list("u_raw" = inits_u_raw,
                                  "col_one_raw" = inits_Omega_unc_col_one_raw,
                                  "off_raw" = inits_Omega_unc_off_raw,
                                  "beta_vec" = inits_beta_vec_raw,
                                  "p_raw" = array(data = inits_prev_unc))
          
          # ##
          # N_sample_sizes_vec <- global_list$N_sample_sizes_vec
          # y_binary_list <- global_list$data_sim_outs$y_binary_list
          # ##
          # ## "Data" which depens on N:
          # N <- N_sample_sizes_vec[df_index]
          # y <- y_binary_list[[df_index]]
          # pop_group <- rep(1, N)
          
          ## Extract quantities needed from the global list:
          Model_settings_list <-  global_list$Model_settings_list
          n_tests <- Model_settings_list$n_tests
          n_class <- Model_settings_list$n_class
          n_pops <- Model_settings_list$n_pops
          n_covariates_per_outcome_mat <- Model_settings_list$n_covariates_per_outcome_mat
          overflow_threshold <- Model_settings_list$overflow_threshold
          underflow_threshold <- Model_settings_list$underflow_threshold
          Phi_type_int <- Model_settings_list$Phi_type_int
          ##
          priors_list <-  global_list$priors_list
          corr_force_positive <- priors_list$prior_Omega_list$corr_force_positive
          a_priori_known_corrs <- priors_list$prior_Omega_list$a_priori_known_corrs
          ##
          N_sample_sizes_vec <- global_list$N_sample_sizes_vec
          y_binary_list <- global_list$data_sim_outs$y_binary_list
          ##
          # n_covariates_max_nd <- Model_settings_list$n_covariates_max_nd
          # n_covariates_max_d <-  Model_settings_list$n_covariates_max_d
          # n_covariates_max <-    Model_settings_list$n_covariates_max
          ##
          # prior_beta_mean <- array(dim = c(n_class, n_tests, n_covariates_max))
          # prior_beta_mean[,,1] <-  t(priors_list$prior_beta_list$prior_beta_mean_vec)
          # prior_beta_sd <- array(dim = c(n_class, n_tests, n_covariates_max))
          # prior_beta_sd[,,1] <-  t(priors_list$prior_beta_list$prior_beta_sd_vec)
          ##
          prior_LKJ <- priors_list$prior_Omega_list$prior_lkj_cholesky_eta
          ##
          prior_prev_alpha <- priors_list$prior_prev_list$prior_prev_alpha
          prior_prev_beta <- priors_list$prior_prev_list$prior_prev_beta
          ##
          ## "Data" which depens on N:
          N <- N_sample_sizes_vec[df_index]
          y <- y_binary_list[[df_index]]
          pop_group <- rep(1, N)
          
          # ## Uses same data as Stan models so just extract from Stan_data_list (also this ensures priors/"data") for Stan 
          # ## (or dummy Stan) models is in correct format!
          # N <- Stan_data_list[[df_index]]$N
          # y <- Stan_data_list[[df_index]]$y
          # n_tests <- Stan_data_list[[df_index]]$n_tests
          # n_pops <- Stan_data_list[[df_index]]$n_pops
          # pop <- Stan_data_list[[df_index]]$pop
          # ##
          # n_pops <- Stan_data_list[[df_index]]$n_pops
          # n_pops <- Stan_data_list[[df_index]]$n_pops
          # n_pops <- Stan_data_list[[df_index]]$n_pops
          # n_pops <- Stan_data_list[[df_index]]$n_pops
          # n_pops <- Stan_data_list[[df_index]]$n_pops
          # ##
          # n_pops <- Stan_data_list[[df_index]]$n_pops
          # ##
          # n_pops <- Stan_data_list[[df_index]]$n_pops
          # n_pops <- Stan_data_list[[df_index]]$n_pops
          # ##
          # n_pops <- Stan_data_list[[df_index]]$n_pops
          # n_pops <- Stan_data_list[[df_index]]$n_pops
          ##
          n_pops <- Stan_data_list[[df_index]]$n_pops
          ##
          prior_beta_mean <- Stan_data_list[[df_index]]$prior_beta_mean
          prior_beta_sd <- Stan_data_list[[df_index]]$prior_beta_sd
          ##
          prior_LKJ <- Stan_data_list[[df_index]]$prior_LKJ
          ##
          prior_p_alpha <- Stan_data_list[[df_index]]$prior_p_alpha
          prior_p_beta <- Stan_data_list[[df_index]]$prior_p_beta
          ##
          n_pops <- Stan_data_list[[df_index]]$n_pops
          n_pops <- Stan_data_list[[df_index]]$n_pops
          fully_vectorised <- Stan_data_list[[df_index]]$fully_vectorised
          
          
          
          
          
          if (BayesMVP_Model_type != "Stan") { 
             if (set_custom_optimised_CXX_CPP_flags == TRUE) { 
                stop("'cmdstan_cpp_flags_if_using_Stan_model' must be set to FALSE if not using a Stan model")
             }
          }
          ##
          if (set_custom_optimised_CXX_CPP_flags == TRUE) { 
 
                   ## cmdstan_cpp_flags_if_using_Stan_model  <- cmdstan_cpp_flags_if_using_Stan_model
                    
          } else if (set_custom_optimised_CXX_CPP_flags == FALSE) { 
            
                  if (BayesMVP_Model_type == "Stan") { 
                    warning("using Default cmdstan flags to compile Stan model")
                  }
                  cmdstan_cpp_flags_if_using_Stan_model  <- NULL
                  
          }
    
          ###  -----------  Compile + initialise the model using "MVP_model$new(...)"  -------------------------------------------------------------
          init_lists_per_chain <- rep(list(stan_inits_list), n_chains_burnin) 
          n_chunks <- NULL
          ##
       
        
          
          if (BayesMVP_Model_type == "Stan") {
            
                      model_args_list <- NULL
                
                      ## Compile BayesMVP model:
                      model_obj <- BayesMVP::MVP_model$new(   Model_type =  "Stan",
                                                              Stan_cpp_flags = cmdstan_cpp_flags_if_using_Stan_model,
                                                              y = y, ## Dummy/can change later but needed for compilation
                                                              N = N, ## Dummy/can change later but needed for compilation
                                                              ##  X = NULL,
                                                              ##  model_args_list = model_args_list, # this arg is only needed for BUILT-IN (not Stan) models
                                                              Stan_data_list = Stan_data_list[[df_index]], ## Dummy/can change later but needed for compilation
                                                              Stan_model_file_path = Stan_model_file_path, 
                                                              init_lists_per_chain = init_lists_per_chain, ## Dummy/can change later but needed for compilation
                                                              sample_nuisance = sample_nuisance, ## Dummy/can change later but needed for compilation
                                                              n_chains_burnin = n_chains_burnin, ## Dummy/can change later but needed for compilation
                                                              n_params_main = n_params_main, ## Dummy/can change later but needed for compilation
                                                              n_nuisance = n_nuisance) ## Dummy/can change later but needed for compilation
                  
          } else if (BayesMVP_Model_type == "LC_MVP") {
            
                      Stan_data_list <- NULL
                
                      num_chunks <-  BayesMVP:::find_num_chunks_MVP(N, n_tests)
                
                      ## make model_args_list (note: Stan models don't need this)
                      model_args_list  <- list(       lkj_cholesky_eta = prior_LKJ,
                                                      n_covariates_per_outcome_mat = n_covariates_per_outcome_mat,  
                                                      #X = X, # only needed if want to include covariates
                                                      num_chunks =   num_chunks,
                                                      prior_coeffs_mean_mat = prior_beta_mean,
                                                      prior_coeffs_sd_mat =    prior_beta_sd, 
                                                      prev_prior_a = prior_p_alpha,  
                                                      prev_prior_b = prior_p_beta,
                                                      corr_force_positive = corr_force_positive)
                
                      ## Compile BayesMVP model:
                      model_obj <- BayesMVP::MVP_model$new(   Model_type = "LC_MVP",
                                                              y = y, ## Dummy/can change later but needed for compilation
                                                              N = N, ## Dummy/can change later but needed for compilation
                                                              model_args_list = model_args_list, # this arg is only needed for BUILT-IN (not Stan) models 
                                                              init_lists_per_chain = init_lists_per_chain, ## Dummy/can change later but needed for compilation
                                                              sample_nuisance = sample_nuisance, ## Dummy/can change later but needed for compilation
                                                              n_chains_burnin = n_chains_burnin, ## Dummy/can change later but needed for compilation
                                                              n_params_main = n_params_main, ## Dummy/can change later but needed for compilation
                                                              n_nuisance = n_nuisance) ## Dummy/can change later but needed for compilation
 
          }
          
                    return(list(model_obj = model_obj, 
                                model_args_list = model_args_list,
                                Stan_data_list = Stan_data_list,
                                init_lists_per_chain = init_lists_per_chain))
                              
                    
                    
                    
                    
              
                    
}

 

# str(Stan_data_list[[df_index]])
# 
# Stan_data_list[[df_index]]$prior_p_alpha
# 
# ## set bs environment variable (otherwise it'll try downloading it even if already installed...)
# bs_path <- BayesMVP:::bridgestan_path()
# Sys.setenv(BRIDGESTAN = bs_path)
# 
# source(normalizePath("C:\\Users\\enzoc\\Documents\\Work\\PhD_work\\R_packages\\BayesMVP\\inst\\BayesMVP\\R\\R_fns_JSON_helpers.R"))
# source(normalizePath("C:\\Users\\enzoc\\Documents\\Work\\PhD_work\\R_packages\\BayesMVP\\inst\\BayesMVP\\R\\R_fn_init_bs_model.R"))
# source(normalizePath("C:\\Users\\enzoc\\Documents\\Work\\PhD_work\\R_packages\\BayesMVP\\inst\\BayesMVP\\R\\R_fns_init_hard_coded_models.R"))
# source(normalizePath("C:\\Users\\enzoc\\Documents\\Work\\PhD_work\\R_packages\\BayesMVP\\inst\\BayesMVP\\R\\R_fns_init_model.R"))
# source(normalizePath("C:\\Users\\enzoc\\Documents\\Work\\PhD_work\\R_packages\\BayesMVP\\inst\\BayesMVP\\R\\R_fns_init_inits.R"))
# source(normalizePath("C:\\Users\\enzoc\\Documents\\Work\\PhD_work\\R_packages\\BayesMVP\\inst\\BayesMVP\\R\\R_fn_init_model_and_vals.R"))
# #
# # prev_prior_a = Stan_data_list[[df_index]]$prior_p_alpha
# #
# #  model_obj$init_object$theta_main_vectors_all_chains_input_from_R
# 
# str(init_lists_per_chain)
# 
# Sys.getenv("R_ARCH")
# Sys.setenv(R_ARCH="/x64")  # For 64-bit
# 
#  
# init_lists_per_chain <- rep(list(stan_inits_list), n_chains_burnin) 
# 
# outs =  initialise_model(  Model_type = "LC_MVP",
#                                ##
#                                Stan_data_list = NULL,
#                                Stan_model_file_path = NULL,
#                                Stan_cpp_flags = cmdstan_cpp_flags,
#                                Stan_cpp_user_header = NULL,
#                                ##
#                                compile = FALSE,
#                                force_recompile = FALSE,
#                                cmdstanr_model_fit_obj = NULL,
#                                ##
#                                y = y,
#                                N = N,
#                                n_params_main = n_params_main,
#                                n_nuisance = n_nuisance,
#                                init_lists_per_chain = init_lists_per_chain,
#                                sample_nuisance = TRUE,
#                                model_args_list = model_args_list, 
#                                n_chains_burnin = n_chains_burnin)
# 
# 
#  # bs_model <- outs$bs_model
#  # Stan_model_file_path <- outs$Stan_model_file_path
#  # 
#  # outs$inits_unconstrained_vec_per_chain
#  # 
#  # dll_path <- transform_stan_path(Stan_model_file_path)#
#  # normalizePath(dll_path)
# # 
#  Stan_model_file_path <- system.file("stan_models/LC_MVP_bin_PartialLog_v5.stan", package = "BayesMVP")
#  Stan_model_file_path <- system.file("stan_models/PO_LC_MVP_bin.stan", package = "BayesMVP")
#  
#  # init_model( Model_type = "Stan",
#  #             Stan_data_list = Stan_data_list[[df_index]],
#  #             Stan_model_file_path = Stan_model_file_path,
#  #             compile = TRUE,
#  #             force_recompile = FALSE,
#  #             cmdstanr_model_fit_obj = NULL,
#  #             y = y,
#  #             N = N,
#  #             n_params_main = n_params_main,
#  #             n_nuisance = n_nuisance,
#  #             init_lists_per_chain = init_lists_per_chain,
#  #             sample_nuisance = TRUE,
#  #             ## model_args_list = model_args_list,
#  #             Stan_cpp_user_header = NULL,
#  #             Stan_cpp_flags = NULL,
#  #             n_chains_burnin = n_chains_burnin)
#  
#  outs =  initialise_model(  Model_type = "Stan",
#                             ##
#                             Stan_data_list = Stan_data_list[[df_index]],
#                             Stan_model_file_path = Stan_model_file_path,
#                             Stan_cpp_flags = cmdstan_cpp_flags,
#                             Stan_cpp_user_header = NULL,
#                             ##
#                             compile = FALSE,
#                             force_recompile = FALSE,
#                             cmdstanr_model_fit_obj = NULL,
#                             ##
#                             y = y,
#                             N = N,
#                             n_params_main = n_params_main,
#                             n_nuisance = n_nuisance,
#                             init_lists_per_chain = init_lists_per_chain,
#                             sample_nuisance = TRUE,
#                             model_args_list = model_args_list,
#                             n_chains_burnin = n_chains_burnin)
# 
# 
#     
#     
#     
#     
#     
#     
#     
#     

    




 