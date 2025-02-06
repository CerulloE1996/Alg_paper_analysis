
    ##  ---- Author: Enzo Cerullo
    ##  ---- Date created: 24/01/2025 (this is the revised version - the original version of this script was created in June 2024)
    ##  ---- Local HPC specs:
    ##  BOOKMARK
    ##  ---- Laptop (aka "normal computer") specs:
    ##  BOOKMARK



    #### ------- Set working directory (wd) - change if needed:  -----------------------------------------------------------------------s--------- 
    {
      
        os <- .Platform$OS.type
        n_threads <- parallel::detectCores()
        
        if (os == "windows") { 
          ## Set to your Windows dir (if using Windows):
          setwd("C:\\users\\enzoc\\Documents\\Work\\PhD_work\\Alg_paper_analysis") 
        } else { 
          ## Set to your Linux dir (if using Linux):
          if (n_threads > 16) setwd("/home/enzocerullo/Documents/Work/PhD_work/Alg_paper_analysis")   ## Local-HPC 
          else  setwd("/home/enzo/Documents/Work/PhD_work/Alg_paper_analysis")    ## Laptop
        }
        
        if (n_threads > 16) { 
          computer <- "Local_HPC"
        } else { 
          computer <- "Laptop"
        }
        
        
        
    }
    
    #### ------- Source necessary R config / function files:  ----------------------------------------------------------------------------------- 
    {
        source(file.path(getwd(), "0_utilities/shared_configs/load_R_packages.R"))
        source(file.path(getwd(), "0_utilities/shared_functions/R_fn_load_data_binary_LC_MVP_sim.R"))
    }
    
    #### ------- Set options:  ------------------------------------------------------------------------------------------------------------------
    {
        options(scipen = 999)
        options(max.print = 1000000000)
        options(warning.length = 8000)
        n_total_threads <- parallel::detectCores() 
        print(paste("Total threads detected = ", n_total_threads))
        options(mc.cores = n_total_threads)
    }
    
    #### ------- Make global list for storage: --------------------------------------------------------------------------------------------------
    global_list <- list()
    
    #### ------- Simulate data: ----------------------------------------------------------------------------------------------------------------- 
    {
        N_sample_sizes_vec <- c(500, 1000, 2500, 5000, 12500, 25000)
        N_datasets <- length(N_sample_sizes_vec)
        
        data_sim_outs <- simulate_binary_LC_MVP_data(    N_vec = N_sample_sizes_vec,
                                                         seed = 123,
                                                         DGP = 5)
        {
            ## Extract key quantities:
            y_binary_list <- data_sim_outs$y_binary_list
            ##
            Sigma_nd_true_observed_list <- data_sim_outs$Sigma_nd_true_observed_list
            Sigma_d_true_observed_list <-  data_sim_outs$Sigma_d_true_observed_list
            ##
            Phi_Se_observed_list <- data_sim_outs$Phi_Se_observed_list
            Phi_Fp_observed_list <- data_sim_outs$Phi_Fp_observed_list
            ##
            prev_true_observed_list <- data_sim_outs$prev_true_observed_list
            Se_true_observed_list <- data_sim_outs$Se_true_observed_list
            Sp_true_observed_list <- data_sim_outs$Sp_true_observed_list
            ##
            true_correlations_observed_vec_list <- data_sim_outs$true_correlations_observed_vec_list
            observed_table_probs_list <- data_sim_outs$observed_table_probs_list
            true_estimates_observed_list <- data_sim_outs$true_estimates_observed_list
            observed_cell_counts_list <- data_sim_outs$observed_cell_counts_list
        }
        
        ## Add to global list
        global_list$N_sample_sizes_vec <- N_sample_sizes_vec
        global_list$N_datasets <- N_datasets
        global_list$data_sim_outs <- data_sim_outs
        
    }
    
    
    #### ------- Set essential model settings: --------------------------------------------------------------------------------------------------
    {
        Model_settings_list <- list()
        ##
        Model_settings_list$Model_type <- "LC_MVP" ## This paper focuses on the LC-MVP (latent class multivariate probit) model only. 
        Model_settings_list$n_tests <- 5
        Model_settings_list$n_class <- 2
        Model_settings_list$n_pops <- 1 ## Just one "population" assumed in study (multiple pops. not yet in BayesMVP-manual models - coming in future)
        ##
        Model_settings_list$n_covariates <- 1 ## Intercept-only / no covariates for this paper (fairly common in test accuracy research)
        Model_settings_list$n_covariates_per_outcome_mat <- array(1, dim = c(Model_settings_list$n_class, Model_settings_list$n_tests))
        Model_settings_list$n_covariates_total <- sum(Model_settings_list$n_covariates_per_outcome_mat)
        Model_settings_list$n_covariates_max_nd <- max(Model_settings_list$n_covariates_per_outcome_mat[1, ])
        Model_settings_list$n_covariates_max_d <- max(Model_settings_list$n_covariates_per_outcome_mat[2, ])
        Model_settings_list$n_covariates_max <- max(Model_settings_list$n_covariates_per_outcome_mat)
        ##
        Model_settings_list$n_corrs_nd <- choose(Model_settings_list$n_tests, 2)
        Model_settings_list$n_corrs_d <-  choose(Model_settings_list$n_tests, 2)
        Model_settings_list$n_corrs <- Model_settings_list$n_corrs_nd + Model_settings_list$n_corrs_d
        ##
        Model_settings_list$n_prev_params <- Model_settings_list$n_pops
        ##
        Model_settings_list$n_params_main <-   Model_settings_list$n_corrs + Model_settings_list$n_covariates_total + Model_settings_list$n_prev_params
        ## Settings for overflow/underflow thresholds (found by ad-hoc experimentation):
        Model_settings_list$overflow_threshold <- +5
        Model_settings_list$underflow_threshold <- -5
        ##
        Model_settings_list$Phi_type <- "Phi" ; Model_settings_list$Phi_type_int <- 1
        Model_settings_list$nuisance_transformation <- "Phi" ## this transformation is more in line with the guassian assumption of unc_u which diffusion-pathspace HMC neeeds. 
        ##
        Model_settings_list$corr_param <- "Sean"
        
        ## Add to global list
        global_list$"Model_settings_list" <- Model_settings_list
    }
    
 
    #### ------- Set priors: --------------------------------------------------------------------------------------------------------------------
    {
        ## Intercepts/coefficient(s) priors [diffuse]:
        prior_beta_mean_vec <- matrix(data = 0, ncol = Model_settings_list$n_class, nrow = Model_settings_list$n_tests)
        prior_beta_sd_vec <- matrix(data = 1, ncol = Model_settings_list$n_class, nrow = Model_settings_list$n_tests)
        ##
        ## Informative priors for reference tests:
        prior_beta_mean_vec[1,1] =  -2.10 ;    prior_beta_sd_vec[1,1] = 0.450
        prior_beta_mean_vec[1,2] = +0.40 ;   prior_beta_sd_vec[1,2] = 0.375
        ## Put in a list:
        prior_beta_list <- list("prior_beta_mean_vec" = prior_beta_mean_vec,
                                "prior_beta_sd_vec" = prior_beta_sd_vec)
        
        ## Correlation priors [more informative in non-diseased class / class 1, more informative in diseased):
        a_priori_known_corrs <- 0
        corr_force_positive <- 0 ## Not forcing corrs. to be positive throughout entire study (as impossible to do in Mplus!)
        #### tailored_corr_priors <- FALSE
        prior_lkj_cholesky_eta = c(12, 3) ## c(10, 2)
        ## Put in a list:
        prior_Omega_list <- list("a_priori_known_corrs" = a_priori_known_corrs,
                                 "corr_force_positive" = corr_force_positive,
                                 "prior_lkj_cholesky_eta" = prior_lkj_cholesky_eta)
        
        ## Prevelance priors:
        prior_prev_alpha <- 5
        prior_prev_beta  <- 10
        ## Put in a list:
        prior_prev_list <- list("prior_prev_alpha" = prior_prev_alpha,
                                "prior_prev_beta" = prior_prev_beta)
        
        ## Put all in one big list:
        priors_list <- list("prior_beta_list" = prior_beta_list,
                            "prior_Omega_list" = prior_Omega_list,
                            "prior_prev_list" = prior_prev_list)
        
        ## Add to global list
        global_list$"priors_list" <- priors_list
        
    }
    
    
    #### ------- Set initial MCMC values (for BayesMVP + Stan + Mplus): -------------------------------------------------------------------------
    {
        n_tests <- Model_settings_list$n_tests
        n_class <- Model_settings_list$n_class
        #### Now we set the initial values for the MCMC algorithms/software we will be comparing and benchmarking. 
        #### We make initial values the same (or as close to "equivelent" as possible) between different algorithms/software. 
        ##
        ## Inits for the * unconstrained * nuisance / high-dimensional latent-variable params for GHK param. of LC_MVP model.
        ## NOTE: This is only relevant for BayesMVP and Stan (NOT Mplus - this does not use the GHK parameterisation of the MVP/LC_MVP):
        {
            inits_u_list <- list()
            for (dataset_index in 1:N_datasets) {
              N <- N_sample_sizes_vec[dataset_index]
              inits_u_unc <- array(0.01, dim = c(N, n_tests))
              inits_u_list[[dataset_index]] <- array(0.01, dim = c(N, n_tests))
            }
        }

        ## Inits for Intercepts/coefficient(s) [NOTE: unc. scale same as actual scale for these as these params are unconstrained !!]
        {
            inits_beta_mat <- matrix(nrow = n_class, ncol = Model_settings_list$n_tests)
            inits_beta_mat[1, ] <- -1.0   # tests 1-T, class 1 (D-)
            inits_beta_mat[2, ] <- +1.0   # tests 1-T, class 2 (D-)
            ## For both BayesMVP * and * Stan, we have the unc. beta vector with the beta in class 1 (D-) first, and then class 2 (D+ after):
            inits_beta_vec <-  c(t(inits_beta_mat))
            ## Now put all in a list:
            inits_beta_list <- list("inits_beta_mat" = inits_beta_mat,
                                    "inits_beta_vec" = inits_beta_vec)
        } 

          
        ## Inits for corr params:
        {
            T_choose_2 <- choose(n_tests, 2)
            Tm1_choose_2 <- choose(Model_settings_list$n_tests - 1, 2)
            ##
            init_TxT_mat <- matrix(ncol = n_tests, nrow = Model_settings_list$n_tests)
            init_TxT_mat[lower.tri(init_TxT_mat)] <- 0.01
            inits_Omega_unc_mat_nd <- init_TxT_mat
            inits_Omega_unc_mat_d <-  inits_Omega_unc_mat_nd
            inits_Omega_unc_mat <- list()
            inits_Omega_unc_mat[[1]] <- inits_Omega_unc_mat_nd
            inits_Omega_unc_mat[[2]] <- inits_Omega_unc_mat_d
            ## Specifically for Stan model:
            inits_Omega_unc_col_one_raw <- array(dim = c(n_class, n_tests - 1))
            inits_Omega_unc_col_one_raw_nd <- rep(0.01, n_tests - 1)
            inits_Omega_unc_col_one_raw_d <- inits_Omega_unc_col_one_raw_nd
            inits_Omega_unc_col_one_raw[1, ] <- inits_Omega_unc_col_one_raw_nd
            inits_Omega_unc_col_one_raw[2, ] <- inits_Omega_unc_col_one_raw_d
            ##
            inits_Omega_unc_off_raw = array(dim = c(n_class, Tm1_choose_2 - a_priori_known_corrs))
            inits_Omega_unc_off_raw_nd = rep(0.01, Tm1_choose_2 - a_priori_known_corrs)
            inits_Omega_unc_off_raw_d <- inits_Omega_unc_off_raw_nd
            inits_Omega_unc_off_raw[1, ] <- inits_Omega_unc_off_raw_nd
            inits_Omega_unc_off_raw[2, ] <- inits_Omega_unc_off_raw_d
            ## Now put all in a list:
            inits_Omega_list <- list(    "inits_Omega_unc_mat_nd" = inits_Omega_unc_mat_nd,
                                         "inits_Omega_unc_mat_d" = inits_Omega_unc_mat_d,
                                         "inits_Omega_unc_mat" = inits_Omega_unc_mat,
                                         ##
                                         "inits_Omega_unc_off_raw_nd" = inits_Omega_unc_off_raw_nd,
                                         "inits_Omega_unc_off_raw_d" = inits_Omega_unc_off_raw_d,
                                         "inits_Omega_unc_off_raw" = inits_Omega_unc_off_raw,
                                         ##
                                         "inits_Omega_unc_col_one_raw_nd" = inits_Omega_unc_col_one_raw_nd,
                                         "inits_Omega_unc_col_one_raw_d" = inits_Omega_unc_col_one_raw_d,
                                         "inits_Omega_unc_col_one_raw" = inits_Omega_unc_col_one_raw)
            #### BOOKMARK: Add Corr-scale equivelent inits here ???
        }
        
        
        ## Inits for disease prevalence (prev):
        {
            inits_prev_prob_scale <- 0.20
            ## Now, since using this transformation:
            ## prev_prob_scale <- (tanh(prev_unc) + 1)/2, we have:
            inits_prev_unc <- atanh( 2*inits_prev_prob_scale - 1 ) 
            ## Now put all in a list:
            inits_prev_list <- list("inits_prev_unc" = inits_prev_unc,
                                    "inits_prev_prob_scale" = inits_prev_prob_scale)
           
        }
        
        ## Now put all the initial values in one big list:
        {
           initial_values_list <- list("inits_u_list" =     inits_u_list,
                                       "inits_beta_list" =  inits_beta_list,
                                       "inits_Omega_list" = inits_Omega_list,
                                       "inits_prev_list" =  inits_prev_list) 
        }
      
      ## Add to global list
      global_list$"initial_values_list" <- initial_values_list
      
    }
    
    
    
    
    #### -------   Prepare data for Stan model(s)    -----------------------------------------------------------------------------------------------------------------------------
    source(file.path(getwd(), "0_utilities/shared_functions/R_fn_Prep_Stan_binary_data.R"))
    {
        ##
        Stan_data_list <- R_fn_prep_Stan_binary_data(global_list = global_list)
        ## outputs list is lists, where each outer-most list is for each of the different N:
        str(Stan_data_list)
        
        Stan_data_list_save <- Stan_data_list
        
        
     
        
        #### -------   Set C++ flags for Stan model(s) [e.g. enabling AVX-512 on local HPC and AVX2 on laptop] + compile the Stan model(s)   -----------------------------------------
        #### source(file.path(getwd(), "0_utilities/shared_configs/Prep_Stan_compile_model.R"))
        source(file.path(getwd(), "0_utilities/shared_functions/R_fn_compile_Stan_model.R"))
        ##
        ## Compile Stan model using custom optimised C++ flags (to make "fairer" comparison with BayesMVP):
        Stan_model_file_path <-   file.path(getwd(), "0_utilities/stan_models/LC_MVP_bin_PartialLog_v5.stan")
        # Stan_model_file_path <-  "/home/enzo/Documents/Work/PhD_work/R_packages/BayesMVP/inst/BayesMVP/inst/stan_models/LC_MVP_bin_PartialLog_v5.stan"
        # Stan_model_file_path <-  "/home/enzo/Documents/Work/PhD_work/R_packages/BayesMVP/inst/BayesMVP/inst/stan_models/PO_LC_MVP_bin.stan"
        ##
        Stan_settings_list <- list()
        ##
        Stan_settings_list$custom_cpp_user_header_file_path <- NULL
        ##
        Stan_settings_list$set_custom_optimised_CXX_CPP_flags <- TRUE
        ##
        {
          if (os == "unix") { 
              Stan_settings_list$cpp_flags$CXX_COMPILER_PATH <- "/opt/AMD/aocc-compiler-5.0.0/bin/clang++"
              Stan_settings_list$cpp_flags$CPP_COMPILER_PATH <- "/opt/AMD/aocc-compiler-5.0.0/bin/clang"
          } else if (os == "windows") { 
              Stan_settings_list$cpp_flags$CXX_COMPILER_PATH <- "g++"
              Stan_settings_list$cpp_flags$CPP_COMPILER_PATH <- "gcc"
          }
          Stan_settings_list$cpp_flags$MATH_FLAGS <- "-fno-math-errno -fno-signed-zeros -fno-trapping-math"
          ## Stan_settings_list$MATH_FLAGS <- "-fno-math-errno"
          ##
          Stan_settings_list$cpp_flags$THREAD_FLAGS <- "-D_REENTRANT"
          ##
          Stan_settings_list$cpp_flags$FMA_FLAGS <- "-mfma"
          ##
          if (computer == "Local_HPC") { 
              Stan_settings_list$cpp_flags$AVX_FLAGS <- "-mavx -mavx2 -mavx512f -mavx512vl -mavx512dq"
          } else if (computer == "Laptop") { 
              Stan_settings_list$cpp_flags$AVX_FLAGS <- "-mavx -mavx2"
          }
          
          
        }
    }
 
    
    stan_compile_LC_MVP_model_outs_list <- R_fn_compile_Stan_model(  force_recompile = FALSE,
                                                                     Stan_model_file_path = Stan_model_file_path, 
                                                                     custom_cpp_user_header_file_path = NULL, 
                                                                     set_custom_optimised_CXX_CPP_flags = Stan_settings_list$set_custom_optimised_CXX_CPP_flags, 
                                                                     CXX_COMPILER_PATH = Stan_settings_list$cpp_flags$CXX_COMPILER_PATH,
                                                                     CPP_COMPILER_PATH = Stan_settings_list$cpp_flags$CPP_COMPILER_PATH,
                                                                     MATH_FLAGS = Stan_settings_list$cpp_flags$MATH_FLAGS,
                                                                     FMA_FLAGS = Stan_settings_list$cpp_flags$FMA_FLAGS,
                                                                     AVX_FLAGS = Stan_settings_list$cpp_flags$AVX_FLAGS,
                                                                     THREAD_FLAGS = Stan_settings_list$cpp_flags$THREAD_FLAGS,
                                                                     )
    
    # ## Make sure cmdstan is installed with the relevant C++ flags (using the "cpp_options" argument) for a "fair" comparison to BayesMVP:
    # cmdstanr::install_cmdstan( overwrite = TRUE, 
    #                            cpp_options =  stan_compile_LC_MVP_model_outs_list$cmdstan_cpp_flags)
                                                                   
    # 
    # -- Compiling C++ code ---
    #   /opt/AMD/aocc-compiler-4.2.0/bin/clang++ 
    #   -fPIC -Wno-deprecated-declarations 
    # -std=c++17 -D_REENTRANT -Wno-sign-compare -Wno-ignored-attributes   
    # -I stan/lib/stan_math/lib/tbb_2020.3/include 
    # -O3 
    # -I src 
    # -I stan/src 
    # -I stan/lib/rapidjson_1.1.0/
    #   -I lib/CLI11-1.9.1/ 
    #   -I stan/lib/stan_math/ 
    #   -I stan/lib/stan_math/lib/eigen_3.4.0 
    # -I stan/lib/stan_math/lib/boost_1.84.0 
    # -I stan/lib/stan_math/lib/sundials_6.1.1/include 
    # -I stan/lib/stan_math/lib/sundials_6.1.1/src/sundials 
    # -DBOOST_DISABLE_ASSERTS        
    # -c -include-pch stan/src/stan/model/model_header.hpp.gch/model_header_16_0.hpp.gch -x c++ -o /tmp/RtmpfrhjR7/model-3194761439f3e.o /tmp/RtmpfrhjR7/model-3194761439f3e.hpp
    # 
    # --- Linking model ---
    #   /opt/AMD/aocc-compiler-4.2.0/bin/clang++
    #   -fPIC -Wno-deprecated-declarations -std=c++17 
    # -D_REENTRANT -Wno-sign-compare -Wno-ignored-attributes      -I stan/lib/stan_math/lib/tbb_2020.3/include    -O3 -I src -I stan/src -I stan/lib/rapidjson_1.1.0/ -I lib/CLI11-1.9.1/ -I stan/lib/stan_math/ -I stan/lib/stan_math/lib/eigen_3.4.0 -I stan/lib/stan_math/lib/boost_1.84.0 -I stan/lib/stan_math/lib/sundials_6.1.1/include -I stan/lib/stan_math/lib/sundials_6.1.1/src/sundials    -DBOOST_DISABLE_ASSERTS               -Wl,-L,"/home/enzo/.cmdstan/cmdstan-2.35.0/stan/lib/stan_math/lib/tbb"   -Wl,-rpath,"/home/enzo/.cmdstan/cmdstan-2.35.0/stan/lib/stan_math/lib/tbb"      /tmp/RtmpfrhjR7/model-3194761439f3e.o src/cmdstan/main.o  -lpthread     -ltbb   stan/lib/stan_math/lib/sundials_6.1.1/lib/libsundials_nvecserial.a stan/lib/stan_math/lib/sundials_6.1.1/lib/libsundials_cvodes.a stan/lib/stan_math/lib/sundials_6.1.1/lib/libsundials_idas.a stan/lib/stan_math/lib/sundials_6.1.1/lib/libsundials_kinsol.a  stan/lib/stan_math/lib/tbb/libtbb.so.2 -o /tmp/RtmpfrhjR7/model-3194761439f3e
    # rm /tmp/RtmpfrhjR7/model-3194761439f3e.hpp /tmp/RtmpfrhjR7/model-3194761439f3
    # 
    # 
                                                                   
    
    # ## Check flags used:
    # stan_compile_LC_MVP_model_outs_list$FLAGS_STANDARD_MACROS
    # stan_compile_LC_MVP_model_outs_list$FLAGS_CUSTOM_MACROS
    ## Print the Stan model:
    Stan_LC_MVP_model_obj <- stan_compile_LC_MVP_model_outs_list$mod
 
    
    #### -------   Run Stan (NUTS-HMC) Pilot Study (Note: All pilot studies were run on my local HPC) ----------------------------------------------------------------------------------
    source(file.path(getwd(), "0_utilities/shared_functions/R_fn_run_Stan_model_LC_MVP.R"))
    ##
    {
        ## Set Stan pilot study parameters:
        Stan_pilot_study_list <- list()
        ##
        Stan_pilot_study_list$output_path <- file.path( getwd(), 
                                                        "1_appendix_pilot_studies",
                                                        "ps_6_determining_basic_MCMC_settings_Stan",
                                                        "outputs")
        ##
        Stan_pilot_study_list$n_runs_per_N <- 5
        Stan_pilot_study_list$n_runs <- 5
        ## Set # chains and # superchains 
        ## NOTE: We ran all pilot studies on a local HPC with 96 cores, and we used 32 chains/threads for each models run. 
        ## Hence, we set n_chains to min(32, parallel::detectcores()) to accommodate "normal" PC's with less cores. 
        Stan_pilot_study_list$n_chains <- 8## min(64, parallel::detectCores())
        Stan_pilot_study_list$n_superchains <- round(Stan_pilot_study_list$n_chains / 8) # E.g. if we have 32 cores then we have 4 superchsins so each superchain has 8 chains (since 4x8 = 32)
        ##
        Stan_pilot_study_list$n_iter <- 500
        ##
        Stan_pilot_study_list$n_burnin_vec <- c(150, 200, 265, 525, 1000)
        ##
        Stan_pilot_study_list$adapt_delta <- 0.80
        Stan_pilot_study_list$max_treedepth <- 10
        Stan_pilot_study_list$metric_type <- "diag_e"
        ##
    }
    
    # #   n_chains <-  64  ;  iter_warmup <- 250  ;        iter_sampling <-   800 
    # #  n_chains <-  8 ; iter_warmup <- 1000  ;       iter_sampling <-   500 
    #
    # n_chains <-  32 ;  iter_sampling <-   500 # for pilot study / tuning / testing
    # 
    # if (N == 5000) {
    #   n_chains <-  16 ;  iter_sampling <-   250
    # }
    
    
    # BOOKMARK - need to double-check how long the burnin for bayesMVP is!! (e.g. is 500 actually 500 or is it 525?! - almost certain it's the latter but double-check!!!!!)
    # #    iter_warmup <- 1000  # already done pilot runs for this  
    # #    iter_warmup <- 525
    # #    iter_warmup <- 275
    # #    iter_warmup <- 200 ###
    # #    iter_warmup <- 150
    
    #  #### These "iter_warmup" are based on the STAN PILOT STUDY RESULTS:
    # if (!(n_chains %in% c(3, 6, 10, 16, 24, 32, 50, 80)))  {
    #   
    #       # if (parallel::detectCores() < 17) {  # Laptop (8 cores / 8 chains) 
    #       #   if (N == 500)   {  iter_warmup = 275    ; iter_sampling = 500  }   # target ESS ~ 1000
    #       #   if (N == 1000)  {  iter_warmup = 1000   ; iter_sampling = 500  }  # target ESS ~ 1000
    #       #   if (N == 2500)  {  iter_warmup = 200    ; iter_sampling = 500  } # should be 5450 but time-limited  ; 1090 =  (5450 / 5)  - so target ESS ~ 200
    #       #   if (N == 5000)  {  iter_warmup = 525    ; iter_sampling = 500  }  # should be 5714 but time-limited  ; 286 =  (5714 / 20)  - so target ESS ~ 50
    #       # } else { # HPC (96 cores / 64 chains)
    #       #   if (N == 500)   {  iter_warmup = 200  ; iter_sampling = 500  }  # target ESS ~ 1000
    #       #   if (N == 1000)  {  iter_warmup = 275  ; iter_sampling = 500  } # target ESS ~ 1000
    #       #   if (N == 2500)  {  iter_warmup = 200  ; iter_sampling = 500  } # target ESS ~ 1000
    #       #   if (N == 5000)  {  iter_warmup = 525  ; iter_sampling = 500  }  # should be 714 but time-limited  ; 143 =  (714 / 5)  - so target ESS ~ 200
    #       # }
    #       
    # }
    
    ## For Stan, due to time constraints we "only" did N = {500, 1000, 2500, 5000} (i.e., excluding the 2 very large datasets):
    Stan_pilot_study_list$N_sample_sizes_vec_for_Stan <- global_list$N_sample_sizes_vec[1:4] 
    ##
    str(Stan_data_list)
    
    # for (N in Stan_pilot_study_list$N_sample_sizes_vec_for_Stan) {
    #   
    #     df_index <- which(Stan_pilot_study_list$N_sample_sizes_vec_for_Stan == N)
    #     Stan_data_list_given_current_N <- Stan_data_list[[df_index]]
    #   
    #     for (n_burnin in Stan_pilot_study_list$n_burnin_vec) {
    #       
    #         for (i in 1:Stan_pilot_study_list$n_runs) {
    #               # 
    #               # ## For testing:
    #               # {
    #               #     Stan_model_obj = Stan_LC_MVP_model_obj
    #               #     global_list = global_list
    #               #     N_sample_size_of_dataset = N
    #               #     run_number = i
    #               #     save_full_cmdstanr_output = FALSE
    #               #     compute_nested_rhat = NULL
    #               #     MCMC_seed = MCMC_seed
    #               #     n_chains = n_chains
    #               #     n_superchains = NULL
    #               #     n_iter = n_iter
    #               #     n_burnin = n_burnin
    #               #     adapt_delta = 0.80
    #               #     max_treedepth = 10
    #               #     metric_type = "diag_e"
    #               # }
    #           
    #               stan_model_outs <- R_fn_run_Stan_model_LC_MVP( Stan_model_obj = Stan_LC_MVP_model_obj,
    #                                                              Stan_data_list_given_current_N = Stan_data_list_given_current_N,
    #                                                              global_list = global_list,
    #                                                              N_sample_size_of_dataset = N,
    #                                                              run_number = i,
    #                                                              save_full_cmdstanr_output = FALSE,
    #                                                              save_output_directory =  Stan_pilot_study_list$output_path,
    #                                                              compute_nested_rhat = NULL,
    #                                                              MCMC_seed = i,
    #                                                              n_chains = Stan_pilot_study_list$n_chains,
    #                                                              n_superchains = Stan_pilot_study_list$n_superchains,
    #                                                              n_iter = Stan_pilot_study_list$n_iter,
    #                                                              n_burnin = n_burnin,
    #                                                              adapt_delta =  Stan_pilot_study_list$adapt_delta,
    #                                                              max_treedepth =  Stan_pilot_study_list$max_treedepth,
    #                                                              metric_type =  Stan_pilot_study_list$metric_type)
    #         
    #         }
    #       
    #     }
    #   
    # }
                                                   
    
    #### ---------------------------------------------------------------------------------------------------------------------------------------------------- 
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    #### -------   Run Mplus (Metropolis-within-Gibbs) Pilot Study ------------------------------------------------------------------------------------------
    ##  NOTE: All pilot studies were run on my local HPC
    ##
    source(file.path(getwd(), "0_utilities/shared_functions/R_fn_run_Mplus_model_LC_MVP.R"))
    {
        ## Set Mplus pilot study parameters:
        Mplus_pilot_study_list <- list()
        ##
        Mplus_pilot_study_list$Mplus_run_type <- "Mplus_pilot" 
        ##
        Mplus_pilot_study_list$output_path <- file.path(getwd(), 
                                                        "1_appendix_pilot_studies",
                                                        "ps_5_determining_basic_MCMC_settings_Mplus",
                                                        "outputs")
        ##
        Mplus_pilot_study_list$n_runs_per_N <- 5
        Mplus_pilot_study_list$n_runs <- Mplus_pilot_study_list$n_runs_per_N
        ##
        Mplus_pilot_study_list$WCP <- TRUE
        ##
        Mplus_pilot_study_list$n_threads_if_local_HPC_AMD_EPYC <- 64 
        Mplus_pilot_study_list$n_threads <- min(parallel::detectCores(), Mplus_pilot_study_list$n_threads_if_local_HPC_AMD_EPYC)
        Mplus_pilot_study_list$deficit_ratio <- round(Mplus_pilot_study_list$n_threads_if_local_HPC_AMD_EPYC/Mplus_pilot_study_list$n_threads)
        ##
        if (Mplus_pilot_study_list$WCP == TRUE) {  ## , If using within-chain parallelisation (WCP)
          
                Mplus_pilot_study_list$n_chains <- ceil(8/Mplus_pilot_study_list$deficit_ratio)
                Mplus_pilot_study_list$n_superchains <- ceil(Mplus_pilot_study_list$n_chains / 8)
                ##   
                Mplus_pilot_study_list$n_fb_iter = 5000
                Mplus_pilot_study_list$n_thin = 1
          
        } else {     
                
                Mplus_pilot_study_list$n_chains <- Mplus_pilot_study_list$n_threads 
                Mplus_pilot_study_list$n_superchains <- ceil(Mplus_pilot_study_list$n_chains / 8)
                ##
                Mplus_pilot_study_list$n_fb_iter = 5000
                Mplus_pilot_study_list$n_thin = 1
          
        }
        ##
        # if (Mplus_pilot_study_list$WCP == TRUE) { 
        #   ## Set # chains and # superchains 
        #   ## NOTE: We ran all pilot studies on a local HPC with 96 cores, and we used 32 chains/threads for each models run. 
        #   ## Hence, we set n_chains to min(32, parallel::detectcores()) to accommodate "normal" PC's with less cores. 
        #   Mplus_pilot_study_list$n_chains <- min(64, parallel::detectCores())
        #   Mplus_pilot_study_list$n_superchains <- ceil(Mplus_pilot_study_list$n_chains / 8) # E.g. if we have 32 cores then we have 4 superchsins so each superchain has 8 chains (since 4x8 = 32)
        #   ##
        #   Mplus_pilot_study_list$n_fb_iter <- 500
        #   ##
        # } else { 
        #   
        # }
        
        { 
              print(paste("n_threads = ", Mplus_pilot_study_list$n_threads))
              print(paste("N_chains = ", Mplus_pilot_study_list$n_chains))
              print(paste("N_iter (pb) = ",  0.5 * Mplus_pilot_study_list$n_fb_iter * Mplus_pilot_study_list$n_thin ))
            
        }
          
        ##
        Mplus_pilot_study_list$N_sample_sizes_vec_for_Mplus <- global_list$N_sample_sizes_vec[1:6]  ## BOOKMARK: All 6 N's for Mplus (???)
        ##
    
    }
    
    
    for (N in Mplus_pilot_study_list$N_sample_sizes_vec_for_Mplus) {
        
        for (i in 1:Mplus_pilot_study_list$n_runs) {
                
                # ## For testing: 
                #### Mplus_pilot_study_list$n_fb_iter <- 50
                # {
                #   global_list = global_list
                #   N_sample_size_of_dataset = N
                #   run_number = run_number
                #   save_full_output = TRUE
                #   compute_nested_rhat = NULL
                #   MCMC_seed = MCMC_seed
                #   n_chains = n_chains
                #   n_superchains = NULL
                # }
                
                Mplus_model_outs <- R_fn_run_Mplus_model_LC_MVP(     run_model = FALSE,
                                                                     Mplus_settings_list = Mplus_pilot_study_list,
                                                                     global_list = global_list,
                                                                     N_sample_size_of_dataset = N,
                                                                     run_number = i,
                                                                     save_full_output = TRUE,
                                                                     save_output_directory =  Mplus_pilot_study_list$output_path,
                                                                     compute_nested_rhat = NULL,
                                                                     MCMC_seed = i,
                                                                     n_chains = Mplus_pilot_study_list$n_chains,
                                                                     n_threads = Mplus_pilot_study_list$n_threads,
                                                                     n_superchains = Mplus_pilot_study_list$n_superchains,
                                                                     n_fb_iter = Mplus_pilot_study_list$n_fb_iter,
                                                                     n_thin = Mplus_pilot_study_list$n_thin)
          
        }
        
      
      
    }
    
    
    # n_threads = 8
    # n_fb_iter = 1000
    
    # 
    # 
    # n_fb_iter = n_fb_iter / 10
    # 
    # seed   =      10
    # 
    # n_chains = 32
    
    
    #### ---------------------------------------------------------------------------------------------------------------------------------------------------- 
    
    
    
    #### -------   Run Mplus (Metropolis-within-Gibbs)  FINAL BENCHMARKS  ------------------------------------------------------------------------------------------
    ##  NOTE: All pilot studies were run on my local HPC
    ##
    source(file.path(getwd(), "0_utilities/shared_functions/R_fn_run_Mplus_model_LC_MVP.R"))
    
        {
            
            if (parallel::detectCores() < 17) { ## i.e. if on Laptop (which has 8 cores / 16 total threads)
              
                n_threads  = 8
                n_chains = 4
                
            } else { ## i.e. if on local HPC
              
                n_threads <- 64
                n_chains <- 8
                
            }
            
            
            ## Now set the appropriate n_fb_iter & n_thin in order to approximately reach the target ESS. 
            ## The values of n_fb_iter and n_thin are based on the Mplus pilot study (see above). 
            if (n_threads == 64) { # HPC
              
                  if (N == 500)     {   n_fb_iter = 55000  ;  n_thin = 2   }   
                  if (N == 1000)    {   n_fb_iter = 47500  ;  n_thin = 4   }  
                  if (N == 2500)    {   n_fb_iter = 70000  ;  n_thin = 2  }  
                  if (N == 5000)    {   n_fb_iter = 50000  ;  n_thin = 2  }   
                  if (N == 12500)   {   n_fb_iter = 10000  ;  n_thin = 9  }   
                  if (N == 25000)   {   n_fb_iter = 9600   ;  n_thin = 10  }   
              
            } else  {  # Laptop 
              
                  if (N == 500)     {   n_fb_iter = 55000  ;  n_thin = 2  }    
                  if (N == 1000)    {   n_fb_iter = 47500  ;  n_thin = 4  }    
                  if (N == 2500)    {   n_fb_iter = 35000  ;  n_thin = 2  }    
                  if (N == 5000)    {   n_fb_iter = 25000  ;  n_thin = 2  }     
                  if (N == 12500)   {   n_fb_iter = 1000   ;  n_thin = 36 }    
                  if (N == 25000)   {   n_fb_iter = 1000   ;  n_thin = 48 }     
              
            }  
            
            
            print(paste("N = ", N))
            print(paste("n_threads = ", n_threads))
            print(paste("N_chains = ", n_chains))
            print(paste("N_iter (pb) = ",  0.5 * n_fb_iter * n_thin ))
            
    }  

    #### ---------------------------------------------------------------------------------------------------------------------------------------------------- 
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
  
    #### -------  Compile BayesMVP (using Stan model / .stan file!) model + Set custom C++ flags for Stan model    ---------------------------------------------- 
    source(file.path(getwd(), "0_utilities/shared_functions/R_fn_compile_BayesMVP_model.R"))
    {
      ##
      ## Compile Stan model using custom optimised C++ flags (to make "fairer" comparison with BayesMVP):
      #### Stan_model_file_path <-   file.path(getwd(), "0_utilities/stan_models/LC_MVP_bin_PartialLog_v5.stan")
      Stan_model_file_path <-  system.file(package = "BayesMVP", file = "stan_models/LC_MVP_bin_PartialLog_v5.stan" )
      ##
      Stan_data_list <- R_fn_prep_Stan_binary_data(global_list = global_list)
      ## outputs list is lists, where each outer-most list is for each of the different N:
      str(Stan_data_list)
      
      ## Use same cpp flags to compile the Stan model as we used for Stan pilot study / Stan models:
      set_custom_optimised_CXX_CPP_flags <- TRUE
      cmdstan_cpp_flags_if_using_Stan_model <-  Stan_settings_list$cpp_flags
      
      BayesMVP_compile_LC_MVP_model_using_Stan_file_outs_list <- R_fn_compile_BayesMVP_model( BayesMVP_Model_type = "Stan", 
                                                                                              set_custom_optimised_CXX_CPP_flags = set_custom_optimised_CXX_CPP_flags,
                                                                                              cmdstan_cpp_flags_if_using_Stan_model = Stan_settings_list$cpp_flags,
                                                                                              global_list = global_list,
                                                                                              Stan_data_list = Stan_data_list, 
                                                                                              Stan_model_file_path = Stan_model_file_path,
                                                                                              N_sample_size_of_dataset = 500, ## Dummy/can change later but needed for compilation
                                                                                              sample_nuisance = TRUE, ## Dummy/can change later but needed for compilation
                                                                                              CCACHE_PATH = "/usr/bin/ccache")
      
      str(Stan_data_list)
      
      # ## Check flags used:
      # stan_compile_LC_MVP_model_outs_list$FLAGS_STANDARD_MACROS
      # stan_compile_LC_MVP_model_outs_list$FLAGS_CUSTOM_MACROS
      ## Print the Stan model:
      BayesMVP_LC_MVP_model_using_Stan_file_obj <- BayesMVP_compile_LC_MVP_model_using_Stan_file_outs_list$model_obj
      #### model_args_list <- BayesMVP_compile_LC_MVP_model_using_Stan_file_outs_list$model_args_list
      Stan_data_list <- BayesMVP_compile_LC_MVP_model_using_Stan_file_outs_list$Stan_data_list
      init_lists_per_chain <- BayesMVP_compile_LC_MVP_model_using_Stan_file_outs_list$init_lists_per_chain
    }
    
    #### -------  Compile BayesMVP (using MANUAL-GRADIENTS LC_MVP model, i.e., BUILT-IN MODEL)   ---------------------------------------------------------------- 
    source(file.path(getwd(), "0_utilities/shared_functions/R_fn_compile_BayesMVP_model.R"))
    {
      ##
      # ## For debug:
      # BayesMVP_Model_type = "LC_MVP"
      # set_custom_CXX_CPP_flags_if_using_Stan_model = FALSE
      # global_list = global_list
      # Stan_data_list = Stan_data_list
      # N_sample_size_of_dataset = 500
      # sample_nuisance = TRUE
      # CCACHE_PATH = "/usr/bin/ccache"
    
      ## NOTE:  manual-gradient model so use the flags used during installation
      BayesMVP_compile_LC_MVP_model_using_manual_grad_outs_list <- R_fn_compile_BayesMVP_model( BayesMVP_Model_type = "LC_MVP", 
                                                                                                set_custom_optimised_CXX_CPP_flags = FALSE, 
                                                                                                cmdstan_cpp_flags_if_using_Stan_model = NULL,
                                                                                                global_list = global_list,
                                                                                                Stan_data_list = Stan_data_list, 
                                                                                                N_sample_size_of_dataset = 500, ## Dummy/can change later but needed for compilation
                                                                                                sample_nuisance = TRUE, ## Dummy/can change later but needed for compilation
                                                                                                CCACHE_PATH = "/usr/bin/ccache")
      
      # ## Check flags used:
      # stan_compile_LC_MVP_model_outs_list$FLAGS_STANDARD_MACROS
      # stan_compile_LC_MVP_model_outs_list$FLAGS_CUSTOM_MACROS
      ## Print the Stan model:
      BayesMVP_LC_MVP_model_using_manual_grad_obj <- BayesMVP_compile_LC_MVP_model_using_manual_grad_outs_list$model_obj
      model_args_list <- BayesMVP_LC_MVP_model_using_manual_grad_obj$model_args_list
      #### Stan_data_list <- BayesMVP_LC_MVP_model_using_manual_grad_obj$Stan_data_list
      init_lists_per_chain <- BayesMVP_LC_MVP_model_using_manual_grad_obj$init_lists_per_chain
    }
    
    #### -------   Run BayesMVP (NUTS-HMC) Pilot Study (Note: All pilot studies were run on my local HPC) --------------------------------------------------------- 
    source(file.path(getwd(), "0_utilities/shared_functions/R_fn_run_BayesMVP_model_LC_MVP.R"))
    ##
    {
        ## Set Stan pilot study parameters:
        BayesMVP_pilot_study_list <- list()
        ##
        ## BayesMVP_pilot_study_list$Stan_data_list <- Stan_data_list
        BayesMVP_pilot_study_list$model_args_list <- model_args_list
        ##
        BayesMVP_pilot_study_list$output_path <- file.path( getwd(), 
                                                        "1_appendix_pilot_studies",
                                                        "ps_7_determining_basic_MCMC_settings_BayesMVP",
                                                        "outputs")
        ##
        BayesMVP_pilot_study_list$n_runs_per_N <- 5
        BayesMVP_pilot_study_list$n_runs <- 5
        ## Set # chains and # superchains 
        ## NOTE: We ran all pilot studies on a local HPC with 96 cores, and we used 32 chains/threads for each models run. 
        ## Hence, we set n_chains to min(32, parallel::detectcores()) to accommodate "normal" PC's with less cores. 
        BayesMVP_pilot_study_list$n_chains_sampling <- min(64, parallel::detectCores()/2)
        BayesMVP_pilot_study_list$n_chains_burnin <- min(8, parallel::detectCores())
        BayesMVP_pilot_study_list$n_superchains <- round(BayesMVP_pilot_study_list$n_chains_sampling / 8) # E.g. if we have 32 cores then we have 4 superchsins so each superchain has 8 chains (since 4x8 = 32)
        ##
        BayesMVP_pilot_study_list$n_iter <- 500
        ##
        BayesMVP_pilot_study_list$n_burnin_vec <- c(250, 500, 1000)
        ##
        BayesMVP_pilot_study_list$adapt_delta <- 0.80
        BayesMVP_pilot_study_list$learning_rate <- 0.05
        ##
        BayesMVP_pilot_study_list$metric_shape_main <- "dense"
        BayesMVP_pilot_study_list$metric_type_main <- "Hessian"
        BayesMVP_pilot_study_list$clip_iter <- 25
        BayesMVP_pilot_study_list$interval_width_main <- 50
        BayesMVP_pilot_study_list$tau_mult <- 2.0
        BayesMVP_pilot_study_list$ratio_M_us <- 0.25
        BayesMVP_pilot_study_list$ratio_M_main <- 0.25
        BayesMVP_pilot_study_list$force_autodiff <- FALSE
        BayesMVP_pilot_study_list$force_PartialLog <- FALSE
        BayesMVP_pilot_study_list$multi_attempts <- TRUE
    }
    
 
    
    ## For Stan, due to time constraints we "only" did N = {500, 1000, 2500, 5000} (i.e., excluding the 2 very large datasets):
    BayesMVP_pilot_study_list$N_sample_sizes_vec_for_BayesMVP <- global_list$N_sample_sizes_vec[1:6] 
    ## Fixed:
    n_iter <-    BayesMVP_pilot_study_list$n_iter
    n_chains_burnin <-    BayesMVP_pilot_study_list$n_chains_burnin
    n_chains_sampling <-    BayesMVP_pilot_study_list$n_chains_sampling
    n_superchains <-    BayesMVP_pilot_study_list$n_superchains
    ##
    manual_gradients <- TRUE ## this doesn't work yet!! --- BOOKMARK
    ##    manual_gradients <- FALSE ## this works!
    ## 
     # 
     # str(BayesMVP_pilot_study_list)
     # 
     # BayesMVP_pilot_study_list$model_args_list
     
     
                                       
                                               
                                               
                                               
    # for (N in BayesMVP_pilot_study_list$N_sample_sizes_vec_for_BayesMVP) {
    #   
    #   ## Stan_data_list_given_current_N <- Stan_data_list[[df_index]]
    #   
    #   for (n_burnin in BayesMVP_pilot_study_list$n_burnin_vec) {
    #     
    #     for (i in 1:BayesMVP_pilot_study_list$n_runs) {
    #       
    #       # ## For testing: 
    #       # {
    #       #     global_list = global_list
    #       #     N_sample_size_of_dataset = N
    #       #     run_number = run_number
    #       #     save_full_cmdstanr_output = FALSE
    #       #     compute_nested_rhat = NULL
    #       #     MCMC_seed = MCMC_seed
    #       #     n_chains = n_chains
    #       #     n_superchains = NULL
    #       #     n_iter = n_iter
    #       #     n_burnin = n_burnin
    #       #     adapt_delta = 0.80
    #       #     max_treedepth = 10
    #       #     metric_type = "diag_e"
    #       # }
    #       
    #       if (manual_gradients == TRUE) { 
    #         BayesMVP_model_obj <- BayesMVP_LC_MVP_model_using_manual_grad_obj
    #       } else if (manual_gradients == FALSE) { ## Using Stan model file
    #         BayesMVP_model_obj <- BayesMVP_LC_MVP_model_using_Stan_file_obj
    #       }
    #       
    #       n_burnin = 500 ## 250 burnin not working for u's !! probably need high LR ?! 
    #       
    #       run_type <- "misc"
    #       
    #       if (computer == "Laptop")  { 
    #         SIMD_vect_type <- "AVX2"
    #       } else { 
    #         SIMD_vect_type <- "AVX512"
    #       }
    #       
    #       ## BayesMVP_LC_MVP_model_using_Stan_file_obj
    #       ## BayesMVP_LC_MVP_model_using_manual_grad_obj
    #       BayesMVP_model_outs <- R_fn_run_BayesMVP_model_LC_MVP( computer = computer,
    #                                                              run_type = run_type,
    #                                                              BayesMVP_model_obj = BayesMVP_model_obj,
    #                                                              BayesMVP_settings_list = BayesMVP_pilot_study_list,
    #                                                              global_list = global_list,
    #                                                              Stan_data_list = Stan_data_list,
    #                                                              ##
    #                                                              manual_gradients = manual_gradients,
    #                                                              SIMD_vect_type = SIMD_vect_type,
    #                                                              partitioned_HMC = TRUE,
    #                                                              diffusion_HMC = TRUE,
    #                                                              ##
    #                                                              N_sample_size_of_dataset = N,
    #                                                              run_number = i,
    #                                                              save_output_directory =  BayesMVP_pilot_study_list$output_path,
    #                                                              save_full_output = TRUE,
    #                                                              compute_nested_rhat = TRUE,
    #                                                              save_log_lik_trace = FALSE,
    #                                                              compute_transformed_parameters = TRUE,
    #                                                              compute_generated_quantities = TRUE,
    #                                                              ##
    #                                                              MCMC_seed = i,
    #                                                              n_chains_burnin = n_chains_burnin,
    #                                                              n_chains_sampling = n_chains_sampling,
    #                                                              n_superchains = n_superchains,
    #                                                              n_iter = n_iter,
    #                                                              n_burnin = n_burnin)
    #                                                        
    #       
    #     }
    #     
    #   }
    #   
    # }
    # 
    
    
    
    
    
    #### -------  Pilot study #1 - "optimising the number of chunks and number of threads for BayesMVP manual-gradients model" ) ---------------------------------
    source(file.path(getwd(),   "1_appendix_pilot_studies",
                                "ps_1_optimizing_N_chunks_and_N_threads",
                                "functions", 
                                "R_fn_run_ps_1_opt_N_chunks.R"))
    ##
    pilot_study_opt_N_chunks_list <- list()
    ##
    n_threads_vec_for_Laptop <-    c(2, 4, 8, 16)
    pilot_study_opt_N_chunks_list$n_threads_vec_for_Laptop <- n_threads_vec_for_Laptop
    n_threads_vec_for_Local_HPC <- c(4, 8, 16, 32, 64)
    pilot_study_opt_N_chunks_list$n_threads_vec_for_Local_HPC <- n_threads_vec_for_Local_HPC
    ##
    if (computer == "Laptop")  { 
      pilot_study_opt_N_chunks_list$n_total_threads <- 8*2 ## SMT * enabled * on Laptop
      options(mc.cores = n_total_threads)
      pilot_study_opt_N_chunks_list$n_threads_vec <- n_threads_vec_for_Laptop
      pilot_study_opt_N_chunks_list$SIMD_vect_type <- "AVX2"
    } else { 
      pilot_study_opt_N_chunks_list$n_total_threads <- 96 ## SMT * disabled * on local_HPC
      options(mc.cores = n_total_threads)
      pilot_study_opt_N_chunks_list$n_threads_vec <- n_threads_vec_for_Local_HPC
      pilot_study_opt_N_chunks_list$SIMD_vect_type <- "AVX512"
    }
    ## Set output directory:
    pilot_study_opt_N_chunks_list$output_path <- file.path(  getwd(),
                                                            "1_appendix_pilot_studies",
                                                            "ps_1_optimizing_N_chunks_and_N_threads",
                                                            "outputs")
    ## Set other variables: 
    pilot_study_opt_N_chunks_list$n_runs <- 10
    pilot_study_opt_N_chunks_list$start_index <- 1 
    pilot_study_opt_N_chunks_list$N_vec <- BayesMVP_pilot_study_list$N_sample_sizes_vec_for_BayesMVP
    ##
    {
      ##
      pilot_study_opt_N_chunks_list$n_chunks_vecs <- list()
      ##
      pilot_study_opt_N_chunks_list$n_chunks_vecs$`500` <-   c(1, 2, 4, 5, 10)
      pilot_study_opt_N_chunks_list$n_chunks_vecs$`1000` <-  c(1, 2, 4, 5, 10)
      pilot_study_opt_N_chunks_list$n_chunks_vecs$`2500` <-  c(1, 2, 4, 5, 10, 20,  25)
      pilot_study_opt_N_chunks_list$n_chunks_vecs$`5000` <-  c(1, 2, 4, 5, 10, 20,  25, 40,  50)
      pilot_study_opt_N_chunks_list$n_chunks_vecs$`12500` <- c(1, 5, 10, 20, 25, 40, 50, 100, 125, 200, 250)
      pilot_study_opt_N_chunks_list$n_chunks_vecs$`25000` <- c(1, 5, 10, 20, 25, 40, 50, 100, 125, 200, 250, 400, 500)
      ##
      pilot_study_opt_N_chunks_list$n_max_chunk_combos <- length(pilot_study_opt_N_chunks_list$n_chunks_vecs$`25000`)
      ## n_iter for each N:
      pilot_study_opt_N_chunks_list$n_iter_given_N$`500` <-   400 
      pilot_study_opt_N_chunks_list$n_iter_given_N$`1000` <-  200 
      pilot_study_opt_N_chunks_list$n_iter_given_N$`2500` <-  80 
      pilot_study_opt_N_chunks_list$n_iter_given_N$`5000` <-  40 
      pilot_study_opt_N_chunks_list$n_iter_given_N$`12500` <- 16 
      pilot_study_opt_N_chunks_list$n_iter_given_N$`25000` <- 8 
    }
    ##
    # ## Run the pilot study (if not already run and/or don't have results/output files):
    # if (computer == "Laptop") {
    #   run_ps_opt_N_chunks_outs <- R_fn_run_ps_opt_N_chunks( pilot_study_opt_N_chunks_list = pilot_study_opt_N_chunks_list,
    #                                                         computer = "Laptop",
    #                                                         BayesMVP_model_obj = BayesMVP_LC_MVP_model_using_manual_grad_obj,
    #                                                         global_list = global_list,
    #                                                         output_path  = pilot_study_opt_N_chunks_list$output_path)
    # } else if (computer == "Local_HPC") { 
    #   run_ps_opt_N_chunks_outs <- R_fn_run_ps_opt_N_chunks( pilot_study_opt_N_chunks_list = pilot_study_opt_N_chunks_list,
    #                                                         computer = "Local_HPC",
    #                                                         BayesMVP_model_obj = BayesMVP_LC_MVP_model_using_manual_grad_obj,
    #                                                         global_list = global_list,
    #                                                         output_path  = pilot_study_opt_N_chunks_list$output_path)
    # }
    ##
    ## Make big tibble (skeleton to store results):
    ##
    path_ps1 <- file.path(getwd(), "1_appendix_pilot_studies", "ps_1_optimizing_N_chunks_and_N_threads")
    source(file.path(path_ps1, "functions", "R_fn_make_df_ps_1_opt_N_chunks.R"))
    source(file.path(path_ps1, "functions", "R_fn_run_ps_1_opt_N_chunks.R"))
    source(file.path(path_ps1, "functions", "R_fn_ps_N_chunks_ggplot_1.R"))
    source(file.path(path_ps1, "functions", "R_fn_ps_N_chunks_ggplot_2.R"))
    source(file.path(path_ps1, "functions", "R_fn_plot_ps_opt_N_chunks.R"))
    ##
    ## Run the function:
    ##
    tibble_all_runs_skeleton <- R_fn_make_df_ps_opt_N_chunks(    pilot_study_opt_N_chunks_list = pilot_study_opt_N_chunks_list,
                                                                 n_threads_vec_for_Local_HPC = n_threads_vec_for_Local_HPC,
                                                                 n_threads_vec_for_Laptop = n_threads_vec_for_Laptop)
    ## Add outputs to list:
    pilot_study_opt_N_chunks_list$tibble_all_runs_skeleton <- tibble_all_runs_skeleton
    ##
    ## Add the results (stored as R arrays in RDS files) to the big tibble:
    ##
    tibble_all_runs <- R_fn_add_res_to_df_ps_opt_N_chunks( pilot_study_opt_N_chunks_list = pilot_study_opt_N_chunks_list,
                                                           output_path = pilot_study_opt_N_chunks_list$output_path,
                                                           tibble_skeleton = tibble_all_runs_skeleton)
    ## Add outputs to list:
    pilot_study_opt_N_chunks_list$tibble_all_runs <- tibble_all_runs
    ##
    ## Make + save plots (+ key tables/tibbles):
    ##
    Figure_PPI <- 400
    plots_and_table_outs <- R_fn_ps_opt_N_chunks_plots(tibble_all_runs = tibble_all_runs,
                                                       pilot_study_opt_N_chunks_list = pilot_study_opt_N_chunks_list,
                                                       global_list = global_list,
                                                       output_path = pilot_study_opt_N_chunks_list$output_path,
                                                       plot_1_overall_scale_factor = 4,
                                                       plot_1_width_scale_factor = 4,
                                                       plot_1_height_scale_factor = 3, 
                                                       plot_1_PPI = Figure_PPI,
                                                       plot_2_overall_scale_factor = 4,
                                                       plot_2_width_scale_factor = 4,
                                                       plot_2_height_scale_factor = 3, 
                                                       plot_2_PPI = Figure_PPI)
    ## Add outputs to list:
    pilot_study_opt_N_chunks_list$plots_and_table_outs <- plots_and_table_outs
    ##
    ## Print table/tibble 1:                                                
    plots_and_table_outs$tibble_1_showing_optimal_n_chunks_per_N_for_HPC %>% print(n = 100)
    plots_and_table_outs$tibble_1_showing_optimal_n_chunks_per_N_for_Laptop %>% print(n = 100)
    ## Print table/tibble 2:  
    plots_and_table_outs$tibble_2_showing_optimal_n_chunks_and_n_threads_combo_per_N_for_HPC %>% print(n = 100)
    plots_and_table_outs$tibble_2_showing_optimal_n_chunks_and_n_threads_combo_per_N_for_Laptop %>% print(n = 100)
    ## Print plot panel 1:
    plots_and_table_outs$plot_panel_1_opt_N_chunks_at_the_every_tested_thread_count
    ## Print plot panel 2:
    plots_and_table_outs$plot_panel_2_opt_N_chunks_at_the_opt_thread_count
     
    ###
    ### Now, based on table 1 and plot 1 above - we can choose the optimal number of threads to use for the local HPC and laptop, for each N
    # chosen_n_threads_Laptop = 8,
    # chosen_n_threads_HPC = 64,
    # ##
    # tibble_showing_optimal_n_chunks_per_N_for_HPC
    # tibble_showing_optimal_n_chunks_per_N_for_Laptop
   

    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    #### -------  Pilot study #2 - "Comparing parallel scaling for BayesMVP manual-gradients LC-MVP model (with and without chunking) to Stan and Mplus" --------
    source(file.path(getwd(), 
                     "1_appendix_pilot_studies",
                     "ps_2_parallel_scaling_vs_Mplus_Stan",
                     "functions", 
                     "R_fn_ps_2_parallel_scaling_comp.R"))
    ##
    ## Make initial list:
    pilot_study_parallel_scaling_comp_list <- list()
    ##
    ## First add the ps_1 list to the ps_2 list as there is much overlap:
    pilot_study_parallel_scaling_comp_list$pilot_study_opt_N_chunks_list <- pilot_study_opt_N_chunks_list
    ##
    pilot_study_parallel_scaling_comp_list$output_path <- file.path(   getwd(),
                                                                       "1_appendix_pilot_studies",
                                                                       "ps_2_parallel_scaling_vs_Mplus_Stan",
                                                                       "outputs")
    ##
    pilot_study_parallel_scaling_comp_list$opt_n_threads_vec_for_Laptop <-    16
    pilot_study_parallel_scaling_comp_list$opt_n_threads_vec_for_Local_HPC <- 64
    ##
    if (computer == "Laptop")  { 
      pilot_study_parallel_scaling_comp_list$n_total_threads <- 8*2 ## SMT * enabled * on Laptop
      options(mc.cores = n_total_threads)
      pilot_study_parallel_scaling_comp_list$n_threads_vec <- pilot_study_parallel_scaling_comp_list$n_threads_vec_for_Laptop
      pilot_study_parallel_scaling_comp_list$SIMD_vect_type <- "AVX2"
    } else { 
      pilot_study_parallel_scaling_comp_list$n_total_threads <- 96 ## SMT * disabled * on local_HPC
      options(mc.cores = n_total_threads)
      pilot_study_parallel_scaling_comp_list$n_threads_vec <- pilot_study_parallel_scaling_comp_list$n_threads_vec_for_Local_HPC
      pilot_study_parallel_scaling_comp_list$SIMD_vect_type <- "AVX512"
    }
    ##

    
    ## Set other variables: 
    pilot_study_parallel_scaling_comp_list$n_runs <- 10
    pilot_study_parallel_scaling_comp_list$start_index <- 1
    pilot_study_parallel_scaling_comp_list$N_vec <- BayesMVP_pilot_study_list$N_sample_sizes_vec_for_BayesMVP
    ##
    ## Only run the manual-gradient BayesMVP LC_MVP model at the optimal # of chunks found from ps1 + 1 chunk (as a reference/"baseline"), for each N:
    {
      ##
      N_vec <- pilot_study_opt_N_chunks_list$N_vec
      ## Local_HPC tibble from ps1 needed:
      tibble_2_showing_optimal_n_chunks_and_n_threads_combo_per_N_for_HPC <- 
                    pilot_study_opt_N_chunks_list$plots_and_table_outs$tibble_2_showing_optimal_n_chunks_and_n_threads_combo_per_N_for_HPC 
      ## Laptop tibble from ps1 needed:
      tibble_2_showing_optimal_n_chunks_and_n_threads_combo_per_N_for_Laptop <- 
        pilot_study_opt_N_chunks_list$plots_and_table_outs$tibble_2_showing_optimal_n_chunks_and_n_threads_combo_per_N_for_Laptop 
      ##
      pilot_study_parallel_scaling_comp_list$opt_n_chunks_at_opt_n_threads <- list()
      pilot_study_parallel_scaling_comp_list$n_chunks_vecs <- list()
      ##
      N <- 500
      for (N in N_vec) {
              ##
              N_current <- N
              ##
              ## First for the Local_HPC:
              subset_temp_given_N <- tibble_2_showing_optimal_n_chunks_and_n_threads_combo_per_N_for_HPC %>%
                                     dplyr::filter(near(N, N_current))##, n_threads == pilot_study_parallel_scaling_comp_list$opt_n_threads_vec_for_Local_HPC)
              ##
              opt_n_chunks_given_N <- as.numeric(as.character(subset_temp_given_N$n_chunks))
              ##
              pilot_study_parallel_scaling_comp_list$opt_n_chunks_at_opt_n_threads$Local_HPC[[as.character(N_current)]] <- opt_n_chunks_given_N
              ##
              temp <- pilot_study_parallel_scaling_comp_list$opt_n_chunks_at_opt_n_threads$Local_HPC[[as.character(N_current)]]
              pilot_study_parallel_scaling_comp_list$n_chunks_vecs$Local_HPC[[as.character(N_current)]] <-   c(1, temp)
              ##
              ## Then for the laptop:
              subset_temp_given_N <- tibble_2_showing_optimal_n_chunks_and_n_threads_combo_per_N_for_Laptop %>%
                                     dplyr::filter(near(N, N_current))#'# , n_threads == pilot_study_parallel_scaling_comp_list$opt_n_threads_vec_for_Laptop)
              ##
              opt_n_chunks_given_N <- as.numeric(as.character(subset_temp_given_N$n_chunks))
              ##
              pilot_study_parallel_scaling_comp_list$opt_n_chunks_at_opt_n_threads$Laptop[[as.character(N_current)]] <- opt_n_chunks_given_N
              ##
              temp <- pilot_study_parallel_scaling_comp_list$opt_n_chunks_at_opt_n_threads$Laptop[[as.character(N_current)]]
              pilot_study_parallel_scaling_comp_list$n_chunks_vecs$Laptop[[as.character(N_current)]] <-   c(1, temp)
              ##
      }
      ## Check for Local_HPC:
      pilot_study_parallel_scaling_comp_list$opt_n_chunks_at_opt_n_threads$Local_HPC
      pilot_study_parallel_scaling_comp_list$n_chunks_vecs$Local_HPC
      ## Check for Laptop:
      pilot_study_parallel_scaling_comp_list$opt_n_chunks_at_opt_n_threads$Laptop
      pilot_study_parallel_scaling_comp_list$n_chunks_vecs$Laptop
      ##
      pilot_study_parallel_scaling_comp_list$n_max_chunk_combos <- 2
      ##
      ## n_iter for each N (same as in ps_1!)
      pilot_study_parallel_scaling_comp_list$n_iter_given_N <-      pilot_study_opt_N_chunks_list$n_iter_given_N
      ##
    }
    
    
    # ## ---- Run the pilot study (if not already run and/or don't have results/output files):
    # {
    #   ##
    #   ## algorithm <- "MD_BayesMVP"
    #   ## algorithm <- "AD_BayesMVP_Stan"
    #   ##  algorithm <- "Mplus_WCP"
    #   algorithm <- "Mplus_standard"
    #   ##
    #   path_ps2 <- file.path(getwd(), "1_appendix_pilot_studies", "ps_2_parallel_scaling_vs_Mplus_Stan")
    #   source(file.path(path_ps2, "functions", "R_fn_ps_2_parallel_scaling_comp.R"))
    #   ##
    #   if (computer == "Laptop") {
    #     run_ps_opt_N_chunks_outs <- R_fn_RUN_ps_parallel_scaling_comp(  pilot_study_parallel_scaling_comp_list = pilot_study_parallel_scaling_comp_list,
    #                                                                     computer = "Laptop",
    #                                                                     algorithm = algorithm,
    #                                                                     BayesMVP_manual_gradient_model_obj = BayesMVP_LC_MVP_model_using_manual_grad_obj,
    #                                                                     BayesMVP_using_Stan_file_model_obj = BayesMVP_LC_MVP_model_using_Stan_file_obj,
    #                                                                     global_list = global_list,
    #                                                                     Stan_data_list = Stan_data_list,
    #                                                                     Mplus_settings_list = Mplus_settings_list,
    #                                                                     output_path  = pilot_study_parallel_scaling_comp_list$output_path)
    #   } else if (computer == "Local_HPC") {
    #     run_ps_opt_N_chunks_outs <- R_fn_RUN_ps_parallel_scaling_comp(  pilot_study_parallel_scaling_comp_list = pilot_study_parallel_scaling_comp_list,
    #                                                                     computer = "Local_HPC",
    #                                                                     algorithm = algorithm,
    #                                                                     BayesMVP_manual_gradient_model_obj = BayesMVP_LC_MVP_model_using_manual_grad_obj,
    #                                                                     BayesMVP_using_Stan_file_model_obj = BayesMVP_LC_MVP_model_using_Stan_file_obj,
    #                                                                     global_list = global_list,
    #                                                                     Stan_data_list = Stan_data_list,
    #                                                                     Mplus_settings_list = Mplus_settings_list,
    #                                                                     output_path  = pilot_study_parallel_scaling_comp_list$output_path)
    #   }
    # }
  
    ## ---- results:
    # ## Make big tibble (skeleton to store results):
    path_ps2 <- file.path(getwd(), "1_appendix_pilot_studies", "ps_2_parallel_scaling_vs_Mplus_Stan")
    source(file.path(path_ps2, "functions", "R_fn_make_df_ps_2_par_scaling.R"))
    source(file.path(path_ps2, "functions", "R_fn_ps_par_scaling_ggplot_1.R"))
    source(file.path(path_ps2, "functions", "R_fn_ps_par_scaling_ggplot_2.R"))
    source(file.path(path_ps2, "functions", "R_fn_plot_ps_par_scaling.R"))
    ##
    ## Run the function:
    ##
    tibble_all_runs_skeleton <- R_fn_make_df_ps_par_scaling(pilot_study_parallel_scaling_comp_list = pilot_study_parallel_scaling_comp_list)
    ## Add outputs to list:
    pilot_study_parallel_scaling_comp_list$tibble_all_runs_skeleton <- tibble_all_runs_skeleton
    ##
    ## Add the results (stored as R arrays in RDS files) to the big tibble:
    ##
    tibble_all_runs <- R_fn_add_res_to_df_ps_par_scaling(  pilot_study_parallel_scaling_comp_list = pilot_study_parallel_scaling_comp_list,
                                                           output_path = pilot_study_parallel_scaling_comp_list$output_path,
                                                           tibble_all_runs_skeleton = tibble_all_runs_skeleton)
    ## Add outputs to list:
    pilot_study_parallel_scaling_comp_list$tibble_all_runs <- tibble_all_runs
    ##
    ## Make + save plots (+ key tables/tibbles):
    ##
    plots_and_table_outs <- R_fn_ps_par_scaling_plots( tibble_all_runs = tibble_all_runs,
                                                       pilot_study_parallel_scaling_comp_list = pilot_study_parallel_scaling_comp_list,
                                                       global_list = global_list,
                                                       output_path = pilot_study_parallel_scaling_comp_list$output_path, 
                                                       plot_1_overall_scale_factor = 4,
                                                       plot_1_width_scale_factor = 4,
                                                       plot_1_height_scale_factor = 3,
                                                       plot_1_PPI = 400,
                                                       plot_2_overall_scale_factor = 1,
                                                       plot_2_width_scale_factor = 16,
                                                       plot_2_height_scale_factor = 9,
                                                       plot_2_PPI = 400)
    ## Add outputs to list:
    pilot_study_parallel_scaling_comp_list$plots_and_table_outs <- plots_and_table_outs
    ##
    # ## Print table/tibble 1:                                                
    # plots_and_table_outs$tibble_1_showing_optimal_n_chunks_per_N_for_HPC %>% print(n = 100)
    # plots_and_table_outs$tibble_1_showing_optimal_n_chunks_per_N_for_Laptop %>% print(n = 100)
    # ## Print table/tibble 2:  
    # plots_and_table_outs$tibble_2_showing_optimal_n_chunks_and_n_threads_combo_per_N_for_HPC %>% print(n = 100)
    # plots_and_table_outs$tibble_2_showing_optimal_n_chunks_and_n_threads_combo_per_N_for_Laptop %>% print(n = 100)
    ## Print plot panel 1:
    plots_and_table_outs$plot_panel_1_paralell_scaling_comparison
    ## Print plot panel 2:
    plots_and_table_outs$plot_panel_2_paralell_scaling_comparison_at_chosen_n_threads
    
    df_subset_at_chosen_n_threads <- plots_and_table_outs$df_subset_at_chosen_n_threads
    
    
    df_subset_at_chosen_n_threads
    
 
    
    
    plot_1_overall_scale_factor = 4
    plot_1_width_scale_factor = 4
    plot_1_height_scale_factor = 3
    plot_1_PPI = 400
    plot_2_overall_scale_factor = 1
    plot_2_width_scale_factor = 16
    plot_2_height_scale_factor = 9
    plot_2_PPI = 400
    
    
    
    
    
    
 
    
    
    
    
    
    
    
    
    
    
    
    
    

    
    
    