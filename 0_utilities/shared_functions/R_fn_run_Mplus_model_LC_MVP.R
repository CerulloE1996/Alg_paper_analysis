






#  --- | --------- R function to fun Mplus LC-MVP model (NOT GHK param. - this only done using Stan or BayesMVP)  --------------------------------------------------------------------

R_fn_run_Mplus_model_LC_MVP <- function(     computer,
                                             run_type,
                                             Mplus_settings_list,
                                             global_list, 
                                             N_sample_size_of_dataset,
                                             run_number, 
                                             save_full_output = TRUE,
                                             save_output_directory = NULL,
                                             compute_nested_rhat = NULL,
                                             MCMC_seed,
                                             n_chains,
                                             n_threads,
                                             n_superchains = NULL,
                                             n_fb_iter,
                                             n_thin)  {
  
    require(MplusAutomation)
  
    ## Start timer:
    tictoc::tic("timer_outer")
    
    ## Start timer:
    tictoc::tic("timer_inner")
    
    
    ##
    Mplus_run_type <- Mplus_settings_list$Mplus_run_type
    ## Mplus has restriction that n_iter = n_burnin:
    n_iter <- round(0.5 * n_fb_iter * n_thin)
    n_burnin <- n_iter
    ##
      
    if (n_threads > n_chains) { 
      message(paste("NOTE: n_threads > n_chains - so using within-chain parallelisation (WCP)"))
    }
    
    ## Extract quantities needed from the global list:
    seed <- MCMC_seed
    Model_settings_list <-  global_list$Model_settings_list
    n_tests <- Model_settings_list$n_tests
    n_class <- Model_settings_list$n_class
    n_pops <- Model_settings_list$n_pops
    n_covariates_per_outcome_mat <- Model_settings_list$n_covariates_per_outcome_mat
    ##
    priors_list <-  global_list$priors_list
    ##
    N_sample_sizes_vec <- global_list$N_sample_sizes_vec
    y_binary_list <- global_list$data_sim_outs$y_binary_list
    ##
    n_covariates_max_nd <- Model_settings_list$n_covariates_max_nd
    n_covariates_max_d <-  Model_settings_list$n_covariates_max_d
    n_covariates_max <-    Model_settings_list$n_covariates_max
    ##
    prior_beta_mean <- array(dim = c(n_class, n_tests, n_covariates_max))
    prior_beta_mean[,,1] <-  t(priors_list$prior_beta_list$prior_beta_mean_vec)
    prior_beta_sd <- array(dim = c(n_class, n_tests, n_covariates_max))
    prior_beta_sd[,,1] <-  t(priors_list$prior_beta_list$prior_beta_sd_vec)
    ##
    prior_LKJ <- priors_list$prior_Omega_list$prior_lkj_cholesky_eta
    ##
    prior_prev_alpha <- priors_list$prior_prev_list$prior_prev_alpha
    prior_prev_beta <- priors_list$prior_prev_list$prior_prev_beta
    ##
    N <- N_sample_size_of_dataset
    ##
    y_binary_list <- global_list$data_sim_outs$y_binary_list
    y <- y_binary_list[[dataset_index]]
    ##
    ## Turn into format for Mplus:
    df <- data.frame(y) %>% 
      dplyr::rename( u1 = X1,
                     u2 = X2, 
                     u3 = X3,
                     u4 = X4,
                     u5 = X5)
   
    ##
    message(paste("Priors for LC-MVP (using Mplus w/ Metropolis-within-Gibbs MCMC algorithm:"))
    ##
    if ((prior_LKJ[1] == 12) && (prior_LKJ[2] == 3))  {
      
          # prior_IW_d <-  10   # 10 => w/ prior-only model (N=2) get ~ (-0.67, 0.67) interval -  approx. equiv. to LKJ(2)
          prior_IW_d <-  11   # 11 -> like LKJ(3)
          # prior_IW_nd <- 24 # 24 ->  w/ prior-only model (N=2) get ~ (-0.41, 0.41) interval -  approx. equiv. to LKJ(10)
          prior_IW_nd <- 30 # 30 -> like LKJ(12)
          ##
          message(paste("Priors for correlations / Omega are set to be approx Omega_d ~ LKJ(3) and Omega_nd ~ LKJ(12) - in order to match Stan/BayesMVP algorithms"))
          message(paste("prior_IW_d = ", prior_IW_d))
          message(paste("prior_IW_nd = ", prior_IW_nd))
          
    } else {
      
          stop("Prior_LKJ must be c(12, 3) to approximately match Stan/BayesMVP")
      
    }
    
  
    
    {
      
      #### tictoc::tic("mplus timer")
      
      fit_mplus <- MplusAutomation::mplusObject(TITLE = "Bayesian LCM-MVP - for mixed binary and/or ordinal data (up to 10 categories)",
                               #   DATA = "FILE = Mplus.dat;",
                               USEVARIABLES = "u1 u2 u3 u4 u5;",
                               VARIABLE = "  
                                           CATEGORICAL = u1-u5 ;
                                           CLASSES = C(2);", 
                               #    MONTECARLO = paste("SEED = ", seed, ";"), 
                               ANALYSIS = paste0("ESTIMATOR = BAYES;",  "\n",
                                                 " CHAINS = ", n_chains, ";",   "\n",
                                                 " PROCESSORS = ", n_threads, ";",   "\n",
                                                 " TYPE = MIXTURE;",   "\n", 
                                                 " FBITERATIONS = ", n_fb_iter, ";",  "\n",
                                                 " THIN = ", n_thin, ";",     "\n",
                                                 " STSEED  =  ",  seed, ";",   "\n",
                                                 " OPTSEED  =  ", seed, ";",   "\n",
                                                 " MCSEED  =  ",  seed, ";",   "\n",
                                                 " BSEED  =  ",   seed, ";"
                               ),   
                               MODEL = "%OVERALL%
                                            !%C#1% ! 
                                            [C#1*-1] (p31);
                                            
                                            u1-u5 WITH u1-u5*0 (p1-p10); 
                                            
                                            [u1$1-u5$1*-1] (p11-p15); 
                                            
                                            %C#2% !  
                                              
                                            u1-u5 WITH u1-u5*0 (p16-p25);
                                            
                                            [u1$1-u5$1*+1] (p26-p30);",
                               # OUTPUT = "   SAMPSTAT MODINDICES (0) STANDARDIZED
                               # 
                               # RESIDUAL TECH1 TECH2 TECH3 TECH4
                               # 
                               # TECH5 FSCOEF FSDET CINTERVAL PATTERNS; ",
                               # MODELPRIORS = paste("p13-p42  ~ IW(0, 13);"), 
                               MODELPRIORS = paste("p1-p10   ~ IW(0.0001,", prior_IW_d, ");", # for LC 1 - DISEASED class 
                                                   "p16-p25  ~ IW(0.0001,", prior_IW_nd, ");", 
                                                   
                                                   "   
                                                       !!  p31 ~ D(5, 10);   
                                                         p31 ~ D(", prior_prev_alpha, ", ", prior_prev_beta, "); 
                                                       !!!! p31 ~ D(1, 1); 
                                                    
                                                          p11 ~ N(", prior_beta_mean[2, 1, 1], ", ", prior_beta_sd[2, 1, 1]^2, "); 
                                                          p12-p15 ~ N(0, 1); ! diffuse for index tests
                        
                                                            !!!! p26 ~ N(-2.10, 0.0625); 
                                                            p26 ~ N(", prior_beta_mean[1, 1, 1], ", ", prior_beta_sd[1, 1, 1]^2, ");
                                                          p27-p30 ~ N(0, 1);  ! diffuse for index tests"),  
                               SAVEDATA = "bparameters = bparam.dat;",
                               rdata = data.frame(df),
                               quiet = FALSE
      )
      
      res_plus <- MplusAutomation::mplusModeler( fit_mplus,
                                                 modelout = paste0("mplus_model_seed_", seed, "_N_", N, ".inp"), 
                                                 writeData = "always",
                                                 # Mplus_command = "/opt/mplusdemo/",
                                                 run = 1)
      
      MplusAutomation::get_results(res_plus, "summaries")
      
      #### ---- Model run time ("inner" timer) - using tictoc:
      {
        print(tictoc::toc(log = TRUE))
        log.txt <- tictoc::tic.log(format = TRUE)
        tictoc::tic.clearlog()
        time_stan_total_inc_csv_inner <- unlist(log.txt)
        ##
        extract_numeric_string <- str_extract(time_stan_total_inc_csv_inner, "\\d+\\.\\d+")   
        time_mplus_total_inc_csv_inner_numeric <- as.numeric(extract_numeric_string)
      }
      
      
      #### if (seed == 10)   beepr::beep("random") # make sound to know model has finished running 
      
    }
    
    
    {
      
            mplus_posterior_samples <- MplusAutomation::get_bparameters(res_plus)$valid_draw
            mplus_posterior_samp_array <- array(dim = c(n_chains, dim(mplus_posterior_samples[[1]])[1], dim(mplus_posterior_samples[[1]])[2] - 2))
            #  str(mplus_posterior_samp_array)
            ##
            for (i in 1:n_chains) {
              mplus_posterior_samp_array[i, , ] <- mplus_posterior_samples[[i]][,3:(dim(mplus_posterior_samples[[1]])[2])]
            }
            
            
            #### ---- Re-format as a list (for main_params ONLY!):
            mplus_trace_list <- list()
            for (i in 1:n_params_main) {
              ## Take the * transpose * since n_rows must equal n_iter and n_cols must equal n_chains:
              mplus_trace_list[[i]] <- t(mplus_posterior_samp_array[,,i])
            }
            #### str(mplus_trace_list)
            
            
            #### ---- Compute split-ESS using BayesMVP:
            Rcpp_outs_split_ESS <- BayesMVP:::Rcpp_compute_MCMC_diagnostics( mcmc_3D_array = mplus_trace_list,
                                                                             diagnostic = "split_ESS",
                                                                             n_threads = parallel::detectCores())
            ess_vec <- Rcpp_outs_split_ESS$diagnostics[,1]
            ## Min ESS:
            Min_ESS <- round(min(ess_vec, na.rm = TRUE), 0)
            ## Print:
            print(paste("ESS (min) = ", Min_ESS))
          
            
            #### ---- Compute split-Rhat using BayesMVP:
            Rcpp_outs_split_rhat <- BayesMVP:::Rcpp_compute_MCMC_diagnostics( mcmc_3D_array = mplus_trace_list,
                                                                              diagnostic = "split_rhat",
                                                                              n_threads = parallel::detectCores())
            rhats_vec <-  Rcpp_outs_split_rhat$diagnostics[,1]  
            ## Max R-hat:
            Max_rhat <-  round(max(rhats_vec, na.rm = TRUE), 3)
            ## Print:
            print(paste("rhat (max) = ", round(Max_rhat, 3)))
            
            
            #### ---- Compute nested R-hat (if enabled):
            Max_rhat_nested <- "Nested R-hat not computed for this run"
            if (is.null(compute_nested_rhat)) { 
              if (n_chains > 15) {  ## only compute nested R-hat if at least 16 chains are used 
                compute_nested_rhat <- TRUE
              } else { 
                compute_nested_rhat <- TRUE
              }
            }
            ##
            if (compute_nested_rhat == TRUE) {
              ## Use BayesMVP helper fn to create superchain ID's (for nested R-hat):
              superchain_ids <- BayesMVP:::create_superchain_ids(n_superchains = n_superchains,  
                                                                 n_chains = n_chains)
              rhats_nested_vec <- c()
              for (i in 1:n_params_main) {
                rhats_nested_vec[i] <-   posterior::rhat_nested( array(c(mplus_trace_list[[i]]), dim = c(n_iter, n_chains)), superchain_ids = superchain_ids )
                #### rhats_vec[i] <-   posterior::rhat( array(c(stan_draws_array[,,index_main_params_adj[i]]), dim = c(iter_sampling, n_chains)) )
              }
              Max_rhat_nested <- round(max(rhats_nested_vec, na.rm = TRUE), 3)
              print(paste("rhat_nested (max) = ", Max_rhat_nested))
            }
            ##
            ess_vec <- unique(ess_vec) ; length(ess_vec)
            rhats_vec <- unique(rhats_vec) ; length(rhats_vec)
            rhats_nested_vec <- unique(rhats_nested_vec) ; length(rhats_nested_vec)
            ##
            mplus_min_ess <- min(ess_vec, na.rm = TRUE) 
            mplus_max_rhat <- max(rhats_vec, na.rm = TRUE)
            mplus_max_n_rhat <- max(rhats_nested_vec, na.rm = TRUE)
            ##
            ##
            mplus_time_total <-  time_mplus_total_inc_csv_inner_numeric # time (total)
            ##
            mplus_ess_per_sec_total <- mplus_min_ess / mplus_time_total
          
    }
    





    {
            
            
            #### n_iter <- dim(mplus_posterior_samples[[1]])[1]
            
            mplus_posterior_samp_array_merged <- array(dim = c(n_chains * n_iter, dim(mplus_posterior_samp_array)[3] ))
            
            
            
            i_start = 1
            i_end = n_iter
            
            for (kk in 1:n_chains) {
              
                  mplus_posterior_samp_array_merged[i_start:i_end, ] = mplus_posterior_samp_array[kk, , ]
                  
                  i_start = i_start + n_iter
                  i_end = i_end + n_iter
                  
            }
            
            # n_covariates <- sum(global_list$Model_settings_list$n_covariates_per_outcome_mat) #### BOOKMARK
            # 
            # mplus_D_pos_means <- mplus_D_neg_means <-   rep(0, (n_covariates + 1)*n_tests )
            # mplus_prev_means   <- rep(0, n_class - 1)
            # 
            # for (kk in 1:n_chains) {
            #   mplus_mean <- c()
            #   for (param in 1:(dim(mplus_posterior_samples[[kk]])[2] - 2)) {
            #     mplus_mean[param] <- mean(pnorm(mplus_posterior_samp_array_merged[, param]))
            #   }
            #   
            #   #   round(mplus_mean, 2)
            #   mplus_D_pos <- mplus_mean[21:25]
            #   mplus_D_neg <- mplus_mean[26:30]
            #   mplus_prev <-  mplus_mean[31]
            #   
            #   mplus_D_pos_means = mplus_D_pos_means + mplus_D_pos
            #   mplus_D_neg_means = mplus_D_neg_means + mplus_D_neg
            #   mplus_prev_means = mplus_prev_means + mplus_prev
            #   
            # }
            # 
            # mplus_D_pos_means <- mplus_D_pos_means / n_chains
            # mplus_D_neg_means <- mplus_D_neg_means / n_chains
            # mplus_prev_means <- mplus_prev_means / n_chains
            # 
            # print(round(1 - mplus_D_pos_means, 3))
            # print(round(mplus_D_neg_means, 3))
            # print(round(mplus_prev_means, 3))
      
    }
    






    ## ----- Print info:
    { 
      
                print(paste0("seed = ", seed))
                ##
                print(paste0("Min ESS = ", round(mplus_min_ess, 0)))
                print(paste0("Max R-hat = ", round(mplus_max_rhat, 3)))
                print(paste0("Max nR-hat = ", round(mplus_max_n_rhat, 3)))
                ##
                #### print(paste0("Time (total) = ", round(mplus_time_total, 0), " seconds")) ## BOOKMARK
                print(paste0("Min ESS / sec (total) = ", round(mplus_ess_per_sec_total , 3)))
                print(paste0("Min ESS / sec (sampling only)  = ", round(2 *mplus_ess_per_sec_total , 3)))
                print(paste0("Bin or Ord? = ",  if (max(y) > 1) { print("Ord") } else { print("Bin")} ))
                ##
                print(paste("N = ", N))
                ##
                print(paste("n_threads = ", n_threads))
                print(paste("N_chains = ", n_chains))
                ##
                print(paste("N_iter (pb) = ",  0.5 * n_fb_iter * n_thin ))
                
    }
    
    #### ---- Model run time ("outer" timer) - using tictoc:
    {
      print(tictoc::toc(log = TRUE))
      log.txt <- tictoc::tic.log(format = TRUE)
      tictoc::tic.clearlog()
      time_stan_total_inc_csv_outer <- unlist(log.txt)
      ##
      extract_numeric_string <- str_extract(time_stan_total_inc_csv_outer, "\\d+\\.\\d+")   
      time_mplus_total_inc_csv_outer_numeric <- as.numeric(extract_numeric_string)
    }
    
    mplus_time_total <- time_mplus_total_inc_csv_outer_numeric
      
    ## ----- Save file:
    {
                
                #  file_name <- paste0("Mplus_", "efficiency_info_", "seed_", seed, "_",  prior_IW_d, "_", prior_IW_nd,  "prior_IW_", N, "N_", n_chains, "chains_", ".RDS")
                
      Mplus_file_name_string <- paste0(   "run_type", run_type, "_",
                                          "computer_", computer, "_",
                                          "seed_", seed, "_", 
                                          "priorIW", prior_IW_d, "_",   prior_IW_nd,  "_",
                                          "N", N, "_",
                                          "n_threads", n_threads, "_",
                                          "n_chains", n_chains, "_",
                                          "n_fb_iter", n_fb_iter, "_",
                                          "n_thin", n_thin, "_",
                                          ".RDS")
                
      
                
                
                total_time_seconds <- mplus_time_total
                total_time_mins <- total_time_seconds / 60
                total_time_hours <- total_time_mins / 60
                
                pb_time_seconds <- mplus_time_total / 2
                pb_time_mins <- pb_time_seconds / 60
                pb_time_hours <- pb_time_mins / 60
                
                Min_ESS_per_sec_total_time <- mplus_ess_per_sec_total
                Min_ESS_per_sec_pb_time <- Min_ESS_per_sec_total_time * 2
                
                file_list <- list(
                  mplus_max_rhat, round(max(mplus_max_rhat),3),
                  mplus_max_n_rhat,
                  mplus_min_ess,
                  ##
                  total_time_seconds,  total_time_mins, total_time_hours,
                  pb_time_seconds, pb_time_mins, pb_time_hours,
                  Min_ESS_per_sec_total_time,
                  Min_ESS_per_sec_pb_time,
                  paste("total time =", round(mplus_time_total, 0), "seconds"),
                  paste("total time =", floor(total_time_mins), "minutes and ", round(((total_time_mins - floor(total_time_mins))*60), 0), "seconds"),
                  paste("total time =", floor(total_time_hours), "hours and ", round(((total_time_hours - floor(total_time_hours))*60), 0), "minutes"),
                  paste("Sampling (post-burnin) time =", round(pb_time_seconds, 0), "seconds"),
                  paste("Sampling (post-burnin) time =", floor(pb_time_mins), "minutes and ", round(((pb_time_mins - floor(pb_time_mins))*60), 0), "seconds"),
                  paste("Sampling (post-burnin) time =", floor(pb_time_hours), "hours and ", round(((pb_time_hours - floor(pb_time_hours))*60), 0), "minutes"),
                  paste("Min ESS / sec (total time) = ", round(Min_ESS_per_sec_total_time, 3)),
                  paste("Min ESS / sec (sampling time only) = ", round(Min_ESS_per_sec_pb_time, 3)), 
                  mplus_trace_list
                )
                # ## Set file name and path:
                # file_path <- file.path(save_output_directory, file_name)
                # ## Save as RDS:
                # saveRDS(object = file_list, file = file_path)
          
                ## File name and path:
                file_name <- paste0("Mplus_outs_", Mplus_file_name_string)
                file_path <- file.path(save_output_directory, file_name)
                ## save RDS file:
                saveRDS(object = file_list, file = file_path)
                
    }
  
  
}








 



