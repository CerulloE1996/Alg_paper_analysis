


{
  
    # Set working direcory ---------------
    try({  setwd("/home/enzocerullo/Documents/Work/PhD_work/")   }, silent = TRUE)
    try({  setwd("/home/enzo/Documents/Work/PhD_work/")    }, silent = TRUE)
    #  options(repos = c(CRAN = "http://cran.rstudio.com"))
    
    # options -------------------------------------------------------------------------
    #  totalCores = 8
    rstan::rstan_options(auto_write = TRUE)
    options(scipen = 999)
    options(max.print = 1000000000)
  #  rstan_options(auto_write = TRUE)
    options(mc.cores = parallel::detectCores() / 2)
  
}

 

{
  
  
  ## Load in dependencies needed for BayesMVP (and other)
  require(Rcpp)
  require(RcppEigen)
  require(RcppParallel)
  require(RcppZiggurat)
  require(RcppClock)
  require(rstan)
  require(BH) 
  require(devtools)
  require(githubinstall)
  require(sitmo)  # maybe need this
  require(dqrng) # maybe need this
  require(beepr)
  require(plyr)
  require(dplyr)
  require(ggplot2)
  require(patchwork)
  require(tictoc) 
  require(remotes)  # maybe need this
  require(cmdstanr) 
  require(jsonlite)
  require(expm)

  
  ## Now load in BayesMVP 
  require(BayesMVP)

  ## Now load in other packages needed for analysis / algorithm comparisons etc
  require(MplusAutomation)
  require(bridgestan)

 
  require(Rfast)   # maybe need this
  require(LaplacesDemon)  # maybe need this
  require(TruncatedNormal)  # maybe need this
  require(rockchalk)   # maybe need this
  
  # 
  # remotes::install_github("https://github.com/roualdes/bridgestan",
  #                  #       subdir="~/Documents/Work/PhD_work/R_packages/BayesMVP",
  #                      force = TRUE)
  # # # # # # Install the cmdstanr package from CRAN
  # # we recommend running this in a fresh R session or restarting your current session
  # install.packages("cmdstanr", repos = c('https://stan-dev.r-universe.dev', getOption("repos")))
  #  remotes::install_github("stan-dev/cmdstanr",force = TRUE)
  # # # Load cmdstanr package
   library(cmdstanr)
  # # 
  # # # # Install the latest version of CmdStan
  # install_cmdstan(cores = 16, overwrite = TRUE, cpp_options = list("STAN_MODEL_LDFLAGS" = "-shared",
  #                                              #  "LDFLAGS" = "-shared" ,
  #                                                 "CXXFLAGS" = "-fPIC"))
  #                                                # "CXX" = "/opt/AMD/aocc-compiler-4.2.0/bin/clang++") )
  # #               
  # # 

  # install.packages("StanHeaders")
  
}

list.files("~/Documents/Work/PhD_work/R_packages/BayesMVP/src", pattern = "\\.so$")
readLines("~/Documents/Work/PhD_work/R_packages/BayesMVP/NAMESPACE")

devtools::clean_dll("~/Documents/Work/PhD_work/R_packages/BayesMVP")



Rcpp::compileAttributes( "~/Documents/Work/PhD_work/R_packages/BayesMVP")
options(buildtools.check = function(action) TRUE )   #
devtools::install(pkg = "~/Documents/Work/PhD_work/R_packages/BayesMVP") 

#  

# 
rm(list = ls())
# gc()
# 
# setTimeLimit(cpu = Inf, elapsed = Inf)
# memory.limit(size = 16000)
# Sys.setenv(R_TIMEOUT = 3600)
# rstan_options(threads_per_chain = 1)
# 
# export LD_LIBRARY_PATH=/usr/lib/gcc/x86_64-linux-gnu/11/:$LD_LIBRARY_PATH

dyn.load("/usr/local/lib/libbridgestan.so")
# 
# Sys.setenv(CPPFLAGS = "-I/home/enzocerullo/R/x86_64-pc-linux-gnu-library/4.1/StanHeaders/include/")
# 
# 
Sys.setenv(CXXFLAGS = paste(Sys.getenv("CXXFLAGS"), "-I/home/enzocerullo/cmdstan/stan/src", sep=" "))
Sys.setenv(CXXFLAGS = paste(Sys.getenv("CXXFLAGS"), "-I/home/enzocerullo/cmdstan/stan/lib/stan_math", sep=" "))
Sys.setenv(CXXFLAGS = paste(Sys.getenv("CXXFLAGS"), "-I/home/enzocerullo/cmdstan/stan/src/stan/model", sep=" ")) 
Sys.setenv(CXXFLAGS = paste(Sys.getenv("CXXFLAGS"), "-I/home/enzocerullo/cmdstan/stan/src/stan/services", sep=" ")) 

Sys.setenv(PKG_CPPFLAGS = paste(Sys.getenv("PKG_CPPFLAGS"), "-I/home/enzocerullo/cmdstan/stan/src/stan/model", sep=" ")) 

 
setwd("/home/enzocerullo/Documents/Work/PhD_work/R_packages/BayesMVP/src")

flags <- paste("g++ -shared    -o LC_MVP_bin_PartialLog_v4_model.so LC_MVP_bin_PartialLog_v4.cpp -L/home/enzocerullo/Documents/Work/PhD_work/R_packages/BayesMVP/lib -lbridgestan -lpthread -lm",
               " -O3  -march=native  -mtune=native   -fPIC    -D_REENTRANT  -DSTAN_THREADS -pthread -Wl,--no-undefined",
               " -L/home/enzocerullo/Documents/Work/PhD_work/R_packages/BayesMVP/lib   -lbridgestan -lpthread -lm",
               "-I/home/enzocerullo/cmdstan/stan/lib/stan_math/lib/boost_1.84.0/   -I/home/enzocerullo/cmdstan/stan/src/stan/services ",
               "-I/home/enzocerullo/cmdstan/stan/src  -I/home/enzocerullo/cmdstan/stan/lib/stan_math",
               "-I/home/enzocerullo/cmdstan/stan/lib/stan_math/lib/eigen_3.4.0/ ",
               #"-I/home/enzocerullo/cmdstan/stan/lib/stan_math/lib/tbb  ",
               "-I/home/enzocerullo/bridgestan/stan/lib/stan_math/lib/tbb_2020.3/include",
             #  "-I/home/enzocerullo/bridgestan/stan/lib/stan_math/lib/tbb_2020.3/src/old/test_task_scheduler_observer_v3.cpp
               "-L/home/enzocerullo/stan/lib/stan_math/lib/tbb_2020.3/lib",
               "-I/home/enzocerullo/bridgestan/stan/lib/stan_math/lib/sundials_6.1.1/include ")
               
               
  
  
  
  system(flags)
  

Sys.setenv(CPPFLAGS = paste(Sys.getenv("CPPFLAGS"), "-I/home/enzocerullo/R/x86_64-pc-linux-gnu-library/4.1/StanHeaders/include", sep=" "))
Sys.setenv(PKG_CPPFLAGS = paste(Sys.getenv("PKG_CPPFLAGS"), "-I/home/enzocerullo/R/x86_64-pc-linux-gnu-library/4.1/StanHeaders/include", sep=" "))
Sys.setenv(PKG_CXXFLAGS = paste(Sys.getenv("PKG_CXXFLAGS"), "-I/home/enzocerullo/R/x86_64-pc-linux-gnu-library/4.1/StanHeaders/include", sep=" "))
# 
# Sys.setenv(CPPFLAGS = paste(Sys.getenv("CPPFLAGS"), "-I/usr/include/rapidjson", sep=" "))
# 
  Sys.getenv("CPPFLAGS")
 
  Sys.getenv("CXXFLAGS")
  Sys.getenv("PKG_CPPFLAGS")
  Sys.getenv("PKG_CXXFLAGS")
  
  Sys.setenv(CXXFLAGS = paste(Sys.getenv("CXXFLAGS"), "-I/home/enzocerullo/.bridgestan/bridgestan-2.5.0/src", sep=" "))
  Sys.setenv(CPPFLAGS = paste(Sys.getenv("CPPFLAGS"), "-I/home/enzocerullo/.bridgestan/bridgestan-2.5.0/src", sep=" "))
  Sys.setenv(PKG_CPPFLAGS = paste(Sys.getenv("PKG_CPPFLAGS"), "-I/home/enzocerullo/.bridgestan/bridgestan-2.5.0/src", sep=" "))
  Sys.setenv(PKG_CXXFLAGS = paste(Sys.getenv("PKG_CXXFLAGS"), "-I/home/enzocerullo/.bridgestan/bridgestan-2.5.0/src", sep=" "))

# Sys.setenv(CPPFLAGS = paste(Sys.getenv("CPPFLAGS"), "-I/home/enzocerullo/R/x86_64-pc-linux-gnu-library/4.1/StanHeaders/include", sep=" "))
# Sys.setenv(PKG_CPPFLAGS = paste(Sys.getenv("PKG_CPPFLAGS"), "-I/home/enzocerullo/R/x86_64-pc-linux-gnu-library/4.1/StanHeaders/include", sep=" "))
# Sys.setenv(PKG_CXXFLAGS = paste(Sys.getenv("PKG_CXXFLAGS"), "-I/home/enzocerullo/R/x86_64-pc-linux-gnu-library/4.1/StanHeaders/include", sep=" "))
# 
# # 

  system.file(package = "StanHeaders")
  system.file(package = "bridgestan")
  

# 
# Sys.setenv(CXXFLAGS = "")
# Sys.setenv(CPPFLAGS = "")
# Sys.setenv(PKG_CPPFLAGS = "")
# Sys.setenv(PKG_CXXFLAGS = "")


{
  colourise <- function(text, fg = "black", bg = NULL) {
    term <- Sys.getenv()["TERM"]
    colour_terms <- c("xterm-color","xterm-256color", "screen", "screen-256color")
    
    if(rcmd_running() || !any(term %in% colour_terms, na.rm = TRUE)) {
      return(text)
    }
    
    col_escape <- function(col) {
      paste0("\033[", col, "m")
    }
    
    col <- .fg_colours[tolower(fg)]
    if (!is.null(bg)) {
      col <- paste0(col, .bg_colours[tolower(bg)], sep = ";")
    }
    
    init <- col_escape(col)
    reset <- col_escape("0")
    paste0(init, text, reset)
  }
  
  .fg_colours <- c(
    "black" = "0;30",
    "blue" = "0;34",
    "green" = "0;32",
    "cyan" = "0;36",
    "red" = "0;31",
    "purple" = "0;35",
    "brown" = "0;33",
    "light gray" = "0;37",
    "dark gray" = "1;30",
    "light blue" = "1;34",
    "light green" = "1;32",
    "light cyan" = "1;36",
    "light red" = "1;31",
    "light purple" = "1;35",
    "yellow" = "1;33",
    "white" = "1;37"
  )
  
  .bg_colours <- c(
    "black" = "40",
    "red" = "41",
    "green" = "42",
    "brown" = "43",
    "blue" = "44",
    "purple" = "45",
    "cyan" = "46",
    "light gray" = "47"
  )
  
  rcmd_running <- function() {
    nchar(Sys.getenv('R_TESTS')) != 0
  }
}




# {
#   
#   
#   # -| ------------------  Simulated dataset (binary) --------------------------------------------
#   
#   
  ###  for sim only
  DGP <- 5

  N <- 500
  #     N <- 2500

  ## for algorithm paper:
  y_master_list_seed_123_datasets <- list()


  {
    DGP <- 5 ;    N <- 500   ### for sim study too
    DGP <- 5 ;    N <- 1000
    DGP <- 5 ;    N <- 2500  ### for sim study too

    DGP <- 5 ;    N <- 5000
    DGP <- 5 ;   N <- 12500
    DGP <- 5 ;   N <- 25000

  }
  ###  DGP <- 5 ;   N <- 50000




  N_datasets <- 123
  #  N_datasets <- 500

  n_tests <- 5


#   
  for (N in c(500, 1000, 2500, 5000, 12500, 25000))
  {

    y_list <- list()

    Sigma_nd_true_observed_list <- list()
    Sigma_d_true_observed_list <- list()

    prev_true_observed_list <- list()
    Se_true_observed_list <- list()
    Sp_true_observed_list <- list()

    true_correlations_observed_vec_list <- list()

    observed_table_probs_list <- list()
    true_estimates_observed_list <- list()

    observed_cell_counts_list <- list()





    Se_set_1 <- c(0.60, 0.55, 0.60, 0.65, 0.70)
    Sp_set_1 <- c(0.99, 0.95, 0.90, 0.90, 0.85)

    Se_set_2 <- c(0.925, 0.86, 0.87, 0.91, 0.86)
    Sp_set_2 <- c(0.95,  0.81, 0.70, 0.67, 0.85)


    Sigma_CI <- diag(n_tests)


    b_vec <- c(0.36,  1.10, 1.10, 1.25, 1.50)
    Sigma_from_bs <- diag(5) + t(t(b_vec)) %*% (t(b_vec))
    Omega_from_bs <- round(cov2cor(Sigma_from_bs), 2) ; Omega_from_bs
    Sigma_varied <- Omega_from_bs


    Sigma_highly_varied <- matrix(c(1,  0,     0,        0,        0,
                                    0,  1,     0.50,     0.25,     0,
                                    0,  0.50,  1,        0.40,     0.40,
                                    0,  0.25,  0.40,     1,        0.70,
                                    0,  0,     0.40,     0.70,     1),
                                  n_tests, n_tests)


    if (DGP == 1) { # Conditional independence (CI)

      true_Fp_vec <-        1 - Sp_set_1
      true_Se_vec <-            Se_set_1

      Sigma_d <- Sigma_CI
      Sigma_nd <-  Sigma_CI

    } else if (DGP == 2) { # CD in D+ group, CI in D- group. CD in D+ group is quite uniform and relatively high

      true_Fp_vec <-        1 - Sp_set_1
      true_Se_vec <-            Se_set_1

      Sigma_d <- Sigma_highly_varied
      Sigma_nd <-  Sigma_CI

    } else if ( DGP == 3) {  # CD in D+ group, CI in D- group. CD in D+ group is very NON-uniform and relatively high, but test 1 is uncorrelated to all other tests and test 2 is uncorrelated to test 5, only weakly correlated to test 4 but strongly correlated to test 3. Tests 3,4,5 all correlated (w/ correlation between 0.50-0.80).

      true_Fp_vec <-        1 - Sp_set_1
      true_Se_vec <-            Se_set_1

      Sigma_d <- Sigma_varied
      Sigma_nd <-  Sigma_CI

    } else if (DGP == 4) {  # CD in both groups - with CD in D+ group 2x that of CD in D- group. CD pattern is the same as that in DGP 3 (i.e. NON-uniform pattern).

      true_Fp_vec <-        1 - Sp_set_1
      true_Se_vec <-            Se_set_1

      Sigma_d <- Sigma_varied
      Sigma_nd <-  0.5 * Sigma_varied
      diag(Sigma_nd) <- rep(1, n_tests)


    } else if (DGP == 5) {

      true_Fp_vec <-        1 - Sp_set_1
      true_Se_vec <-            Se_set_1

      Sigma_d <- Sigma_highly_varied
      Sigma_nd <-  0.5 * Sigma_highly_varied
      diag(Sigma_nd) <- rep(1, n_tests)

    } else if (DGP == 6) {

      true_Fp_vec <-        1 - Sp_set_2
      true_Se_vec <-            Se_set_2

      Sigma_d <- Sigma_varied
      Sigma_nd <-  0.5 * Sigma_varied
      diag(Sigma_nd) <- rep(1, n_tests)

    } else if (DGP == 7) {

      true_Fp_vec <-        1 - Sp_set_2
      true_Se_vec <-            Se_set_2

      Sigma_d <- Sigma_varied
      Sigma_nd <-  1 * Sigma_varied

    }



    L_Sigma_d  = (chol(Sigma_d)) # PD check
    L_Sigma_nd  = (chol(Sigma_nd)) # PD check

    eigen(Sigma_d)
    eigen(Sigma_nd)

    #  nearPD(Sigma_d)




    for (ii in 1:N_datasets) {
      #  for (ii in 1:N_datasets) {

      df_sim_seed <- ii

      set.seed(df_sim_seed, kind = "L'Ecuyer-CMRG")




      true_prev <- 0.20 # low-ish prevalence
      # true_prev <- 0.40 # high (relatively) prevalence (same prev. used in Wang et al, 2017)

      d_ind <- sort(rbinom(n= N, size = 1, prob = true_prev))
      n_pos <- sum(d_ind)


      n_neg <- N - sum(d_ind)
      latent_results_neg <- LaplacesDemon::rmvn(n = n_neg, mu = qnorm(true_Fp_vec), Sigma = Sigma_nd)
      latent_results_pos <- LaplacesDemon::rmvn(n = n_pos, mu = qnorm(true_Se_vec), Sigma = Sigma_d)
      latent_results <- rbind(latent_results_neg, latent_results_pos)
      results_neg <- ifelse(latent_results_neg > 0, 1, 0)
      results_pos <- ifelse(latent_results_pos > 0, 1, 0)
      results <- rbind(results_neg, results_pos)
      y <- results

      df <- dplyr::tibble(results,latent_results,d_ind)
      df_pos <- dplyr::filter(df, d_ind == 1)
      df_neg <- dplyr::filter(df, d_ind == 0)



      Sigma_nd_true_observed <- Sigma_d_true_observed <- array(dim = c(n_tests, n_tests))
      observed_correlations <- array(dim = c(n_tests, n_tests))

      for (i in 2:n_tests) {
        for (j in 1:(i-1)) {
          Sigma_nd_true_observed[i, j] <- cor(df_neg$latent_results[,i], df_neg$latent_results[,j])
          Sigma_nd_true_observed[j, i] <-  Sigma_nd_true_observed[i, j]
          Sigma_d_true_observed[i, j] <- cor(df_pos$latent_results[,i], df_pos$latent_results[,j])
          Sigma_d_true_observed[j, i] <-  Sigma_d_true_observed[i, j]
          observed_correlations[i, j] <- cor(y[, i], y[, j])
          observed_correlations[j, i] <-  observed_correlations[i, j]
        }
      }


      prev_observed <-  print(round(sum(d_ind)/N, 3))

      # Se
      Phi_Se_observed_vec <- c()
      for (i in 1:n_tests) {
        Phi_Se_observed_vec[i] <- round(qnorm(sum(df_pos$results[,i])/nrow(df_pos)),2)
      }
      print(Phi_Se_observed_vec)
      print(round(pnorm(Phi_Se_observed_vec), 3))

      # Fp
      Phi_Sp_observed_vec <- c()
      for (i in 1:n_tests) {
        Phi_Sp_observed_vec[i] <-  round(  qnorm( 1 - ((nrow(df_neg) - sum(df_neg$results[,i]))/nrow(df_neg))  ),  2)
      }
      print(Phi_Sp_observed_vec)
      print(round(pnorm(-Phi_Sp_observed_vec), 3))

      print(paste("N = ", N))

      # saveRDS(object = y, file = paste0("y_Data_Bin_N_", N, "DGP_", DGP)  )
      # saveRDS(object = df, file = paste0("df_Data_Bin_N_", N, "DGP_", DGP)  )


      # print(Sigma_nd)
      # print(Sigma_d)


      prev_true_observed  <-  print(round(sum(d_ind)/N, 3))
      Se_true_observed <-     print(round(pnorm(Phi_Se_observed_vec), 3))
      Sp_true_observed <-    print(round(pnorm(-Phi_Sp_observed_vec), 3))

      true_correlations_observed_vec <- observed_correlations[upper.tri(observed_correlations )]

      obs_table <- table(y[, 1], y[, 2], y[, 3], y[, 4], y[, 5])
      observed_table_probs_vec <- c(unlist(round(prop.table(obs_table), 4)))

      observed_cell_counts_list[[ii]] <- observed_table_probs_vec * N

      true_estimates_observed <-  c(Sigma_nd_true_observed[upper.tri(Sigma_nd_true_observed )],  Sigma_d_true_observed[upper.tri(Sigma_d_true_observed )], Sp_true_observed,  Se_true_observed, prev_true_observed ,
                                    true_correlations_observed_vec, observed_table_probs_vec, NA, NA)
      true_estimates  <-  c(Sigma_nd[upper.tri(Sigma_nd )],  Sigma_d[upper.tri(Sigma_d )], 1 - true_Fp_vec,  true_Se_vec, true_prev  ,
                            rep(NA, length(true_correlations_observed_vec)), rep(NA, length(observed_table_probs_vec)), NA, NA)


      # make lists for simulation study

      y_list[[ii]] <- y

      Sigma_nd_true_observed_list[[ii]] <- Sigma_nd_true_observed
      Sigma_d_true_observed_list[[ii]] <- Sigma_d_true_observed

      prev_true_observed_list[[ii]] <- prev_true_observed
      Se_true_observed_list[[ii]] <- Se_true_observed
      Sp_true_observed_list[[ii]] <- Sp_true_observed

      true_correlations_observed_vec_list[[ii]] <- true_correlations_observed_vec

      observed_table_probs_list[[ii]] <- observed_table_probs_vec
      true_estimates_observed_list[[ii]] <- true_estimates_observed



    }

    if (N == 500)   y_master_list_seed_123_datasets[[1]] <- y_list[[123]]
    if (N == 1000)  y_master_list_seed_123_datasets[[2]] <- y_list[[123]]
    if (N == 2500)  y_master_list_seed_123_datasets[[3]] <- y_list[[123]]
    if (N == 5000)  y_master_list_seed_123_datasets[[4]] <- y_list[[123]]
    if (N == 12500) y_master_list_seed_123_datasets[[5]] <- y_list[[123]]
    if (N == 25000) y_master_list_seed_123_datasets[[6]] <- y_list[[123]]

  }


  # assess sparsity in D- class
  {
    print((sum(df_neg[,1]$results[,1]) / length(df_neg[,1]$results[,1]) ) *100)
    print((sum(df_neg[,1]$results[,2]) / length(df_neg[,1]$results[,1]) ) *100)
    print((sum(df_neg[,1]$results[,3]) / length(df_neg[,1]$results[,1]) ) *100)
    print((sum(df_neg[,1]$results[,4]) / length(df_neg[,1]$results[,1]) ) *100)
    print((sum(df_neg[,1]$results[,5]) / length(df_neg[,1]$results[,1]) ) *100)
  }


  prev_true_observed_list[[123]]
  Se_true_observed_list[[123]]


# saveRDS(file = "MVP_LC_bin_data_1.RDS", 
#         object = list(y_list, 
#         Sigma_nd_true_observed_list,
#         Sigma_d_true_observed_list,
#         prev_true_observed_list,
#         Se_true_observed_list,
#         Sp_true_observed_list,
#         true_correlations_observed_vec_list,
#         observed_table_probs_list,
#         true_estimates_observed_list))


# {
# stuff <- readRDS("MVP_LC_bin_data_1.RDS")
# 
# y_list <- stuff[[1]]
# Sigma_nd_true_observed_list <- stuff[[2]]
# Sigma_d_true_observed_list <- stuff[[3]]
# prev_true_observed_list <- stuff[[4]]
# Se_true_observed_list <- stuff[[5]]
# Sp_true_observed_list <- stuff[[6]]
# true_correlations_observed_vec_list <- stuff[[7]]
# observed_table_probs_list <- stuff[[8]]
# true_estimates_observed_list <- stuff[[9]]
#  
# }
# 
# y <- y_list[[1]]
  
#   #  # | -----------------  Simulated dataset (binary + ordinal) --------------------------------------------

#  N <- 1000
#  
#  ##### N <- 2000  ###
#  N <- 4000
#  #  # # #  N <- 8000   ###
#  N <- 16000
#  #
#  #     N <- 32000    ###
#  #     ####
#  N <- 64000    ###
#  #     ####
#  #   #    N <- 128000    ###
#  #  # # #
#  # ###     N <- 256000    ###
# 
#  
#  
#   # {
#   #   set.seed(123)
#   # 
#   # 
#   #   # #     n_tests <- 6
#   #   # #     
#   #   #   
#   #   #   
#   #     # newest DGP (all within 95% prior interval of LKJ(6) prior)
#   #         rho12_d <- 0.10
#   #         rho13_d <- 0.10
#   #         rho14_d <- 0.10
#   #         rho15_d <- 0.10
#   #         rho16_d <- 0.10
#   #         rho23_d <- 0.30
#   #         rho24_d <- 0.30
#   #         rho25_d <- 0.30
#   #         rho26_d <- 0.30
#   #         rho34_d <- 0.30
#   #         rho35_d <- 0.30
#   #         rho36_d <- 0.30
#   #         rho45_d <- 0.50
#   #         rho46_d <- 0.50
#   #         rho56_d <- 0.50
#   # 
#   #         rho12_nd <- 0.05
#   #         rho13_nd <- 0.05
#   #         rho14_nd <- 0.05
#   #         rho15_nd <- 0.05
#   #         rho16_nd <- 0.05
#   #         rho23_nd <- 0.15
#   #         rho24_nd <- 0.15
#   #         rho25_nd <- 0.15
#   #         rho26_nd <- 0.15
#   #         rho34_nd <- 0.15
#   #         rho35_nd <- 0.15
#   #         rho36_nd <- 0.15
#   #         rho45_nd <- 0.25
#   #         rho46_nd <- 0.25
#   #         rho56_nd <- 0.25
#   #     #
#   # 
#   # 
#   # 
#   # 
#   #   Sigma_d <- matrix(c(1,rho12_d,rho13_d,rho14_d,rho15_d,rho16_d,
#   #                       rho12_d,1,rho23_d,rho24_d,rho25_d,rho26_d,
#   #                       rho13_d,rho23_d,1,rho34_d,rho35_d,rho36_d,
#   #                       rho14_d,rho24_d,rho34_d,1,rho45_d,rho46_d,
#   #                       rho15_d,rho25_d,rho35_d,rho45_d,1,rho56_d,
#   #                       rho16_d,rho26_d,rho36_d,rho46_d,rho56_d,1),  n_tests, n_tests)
#   # 
#   #   #L_Sigma_d  = (chol(Sigma_d))
#   # 
#   #   Sigma_nd <- matrix(c(1,rho12_nd,rho13_nd,rho14_nd,rho15_nd,rho16_nd,
#   #                        rho12_nd,1,rho23_nd,rho24_nd,rho25_nd,rho26_nd,
#   #                        rho13_nd,rho23_nd,1,rho34_nd,rho35_nd,rho36_nd,
#   #                        rho14_nd,rho24_nd,rho34_nd,1,rho45_nd,rho46_nd,
#   #                        rho15_nd,rho25_nd,rho35_nd,rho45_nd,1,rho56_nd,
#   #                        rho16_nd,rho26_nd,rho36_nd,rho46_nd,rho56_nd,1),  n_tests, n_tests)
#   #   #L_Sigma_nd  = (chol(Sigma_nd))
#   # 
#   #   eigen(Sigma_d)
#   #   eigen(Sigma_nd)
#   # 
#   # 
#   #   # CI
#   #   #Sigma_d <- Sigma_nd <- diag(1, n_tests)
#   # 
#   # 
#   #   d_ind <- sort(rbinom(n= N, size = 1, prob = 0.20))
#   #   n_pos <- sum(d_ind)
#   #   n_neg <- N - sum(d_ind)
#   #   latent_data_neg <- LaplacesDemon::rmvn(n = n_neg, mu = c(-1.25, -0.8, -0.8, -0.8, -0.8, -0.8), Sigma = Sigma_nd)
#   #   latent_data_pos <- LaplacesDemon::rmvn(n = n_pos, mu = c(1,    0.85, 0.85, 0.85, 0.85, 0.85), Sigma = Sigma_d)
#   # 
#   #   # binary results (first 3 tests)
#   #   results_binary_neg <- ifelse(latent_data_neg[,1:3] > 0, 1, 0)
#   #   results_binary_pos <- ifelse(latent_data_pos[,1:3] > 0, 1, 0)
#   # 
#   #   # ordinal results (last 3 tests)
#   #   results_ordinal_neg <- ifelse(latent_data_neg[,4:6] < -2.5, 1,
#   #                                 ifelse( ((latent_data_neg[,4:6] > -2.5) & (latent_data_neg[,4:6] < -2)), 2,
#   #                                         ifelse( ((latent_data_neg[,4:6] > -2) & (latent_data_neg[,4:6] < -1.5)), 3,
#   #                                                 ifelse( ((latent_data_neg[,4:6] > -1.5) & (latent_data_neg[,4:6] < -1)), 4,
#   #                                                         ifelse( ((latent_data_neg[,4:6] > -1)   & (latent_data_neg[,4:6] < -0.5)), 5,
#   #                                                                 ifelse( ((latent_data_neg[,4:6] > -0.5)   & (latent_data_neg[,4:6] < 0)), 6,
#   #                                                                         ifelse( ((latent_data_neg[,4:6] >  0)   & (latent_data_neg[,4:6] < 0.5)), 7,
#   #                                                                                 ifelse( ((latent_data_neg[,4:6] >  0.5)   & (latent_data_neg[,4:6] < 1)), 8,
#   #                                                                                         ifelse( ((latent_data_neg[,4:6] >  1)   & (latent_data_neg[,4:6] < 1.5)), 9,
#   #                                                                                                 ifelse( ((latent_data_neg[,4:6] >  1.5)   & (latent_data_neg[,4:6] < 2)), 10,
#   #                                                                                                         ifelse( ((latent_data_neg[,4:6] >  2) ), 11, 0)))))))))))
#   # 
#   #   results_ordinal_pos <- ifelse(latent_data_pos[,4:6] < -2.5, 1,
#   #                                 ifelse( ((latent_data_pos[,4:6] > -2.5) & (latent_data_pos[,4:6] < -2)), 2,
#   #                                         ifelse( ((latent_data_pos[,4:6] > -2) & (latent_data_pos[,4:6] < -1.5)), 3,
#   #                                                 ifelse( ((latent_data_pos[,4:6] > -1.5) & (latent_data_pos[,4:6] < -1)), 4,
#   #                                                         ifelse( ((latent_data_pos[,4:6] > -1)   & (latent_data_pos[,4:6] < -0.5)), 5,
#   #                                                                 ifelse( ((latent_data_pos[,4:6] > -0.5)   & (latent_data_pos[,4:6] < 0)), 6,
#   #                                                                         ifelse( ((latent_data_pos[,4:6] >  0)   & (latent_data_pos[,4:6] < 0.5)), 7,
#   #                                                                                 ifelse( ((latent_data_pos[,4:6] >  0.5)   & (latent_data_pos[,4:6] < 1)), 8,
#   #                                                                                         ifelse( ((latent_data_pos[,4:6] >  1)   & (latent_data_pos[,4:6] < 1.5)), 9,
#   #                                                                                                 ifelse( ((latent_data_pos[,4:6] >  1.5)   & (latent_data_pos[,4:6] < 2)), 10,
#   #                                                                                                         ifelse( ((latent_data_pos[,4:6] >  2) ), 11, 0)))))))))))
#   # 
#   #   min(results_ordinal_neg)
#   #   max((results_ordinal_neg))
#   # 
#   # 
#   #   results_pos <- cbind(results_binary_pos, results_ordinal_pos)
#   #   results_neg <- cbind(results_binary_neg, results_ordinal_neg)
#   # 
#   #   results <- rbind(results_neg, results_pos)
#   #   latent_results <- rbind(latent_data_neg, latent_data_pos )
#   # 
#   #   y <- results
#   #   df <- tibble(results,latent_results,d_ind)
#   #   df_pos <- filter(df, d_ind == 1)
#   #   df_neg <- filter(df, d_ind == 0)
#   # 
#   #   nrow(df_pos)
#   # 
#   # 
#   #   round(sum(d_ind)/N,2)
#   # 
#   #   # Se, binary
#   #   round(sum(df_pos$results[,1])/nrow(df_pos),2)
#   #   round(sum(df_pos$results[,2])/nrow(df_pos),2)
#   #   round(sum(df_pos$results[,3])/nrow(df_pos),2)
#   # 
#   # 
#   # 
#   # 
#   #   # Se, ordinal
#   #   # test 4
#   #   Se_t4_true <- c()
#   #   Se_t5_true <- c()
#   #   Se_t6_true <- c()
#   #   Sp_t4_true <- c()
#   #   Sp_t5_true <- c()
#   #   Sp_t6_true <- c()
#   # 
#   #   for (i in 1:10) {
#   #     print(round(sum( df_pos$results[,4] > i )/nrow(df_pos),2))
#   #     Se_t4_true[i] <- round(sum( df_pos$results[,4] > i )/nrow(df_pos),2)
#   #   }
#   # 
#   # 
#   #   # test 5
#   #   for (i in 1:10) {
#   #     print(round(sum( df_pos$results[,5] > i )/nrow(df_pos),2))
#   #     Se_t5_true[i] <- round(sum( df_pos$results[,5] > i )/nrow(df_pos),2)
#   #   }
#   # 
#   #   # test 6
#   #   for (i in 1:10) {
#   #     print(round(sum( df_pos$results[,6] > i )/nrow(df_pos),2))
#   #     Se_t6_true[i] <- round(sum( df_pos$results[,6] > i )/nrow(df_pos),2)
#   #   }
#   # 
#   # 
#   # 
#   #   # Sp, binary
#   #   round((nrow(df_neg) - sum(df_neg$results[,1]))/nrow(df_neg),2)
#   #   round((nrow(df_neg) - sum(df_neg$results[,2]))/nrow(df_neg),2)
#   #   round((nrow(df_neg) - sum(df_neg$results[,3]))/nrow(df_neg),2)
#   # 
#   #   # Sp, ordinal
#   #   # test 4
#   #   for (i in 1:10) {
#   #     print(1 - round(sum( df_neg$results[,4] > i )/(nrow(df_neg)),2))
#   #     Sp_t4_true[i] <- 1 - round(sum( df_neg$results[,4] > i )/(nrow(df_neg)),2)
#   #   }
#   # 
#   # 
#   #   # test 5
#   #   for (i in 1:10) {
#   #     print(1 - round(sum( df_neg$results[,5] > i )/(nrow(df_neg)),2))
#   #     Sp_t5_true[i] <- 1 - round(sum( df_neg$results[,5] > i )/(nrow(df_neg)),2)
#   #   }
#   # 
#   #   # test 6
#   #   for (i in 1:10) {
#   #     print(1 - round(sum( df_neg$results[,6] > i )/(nrow(df_neg)),2))
#   #     Sp_t6_true[i] <- 1 - round(sum( df_neg$results[,6] > i )/(nrow(df_neg)),2)
#   #   }
#   # 
#   # 
#   # 
#   #   for (n in 1:N) {
#   #     for (t in 1:n_tests) {
#   #       if (y[n, t] == 11) {
#   #         y[n, t] <- 10
#   #       }
#   #     }
#   #   }
#   # 
#   # 
#   #   y_ord <- y
#   #   
#   #   saveRDS(object = y, file = paste0("y_Data_Ord_N_", N)  )
#   #   saveRDS(object = df, file = paste0("df_Data_Ord_N_", N)  )
#   # 
#   # }
# 
# 
# 
# 
#   {
#     
#     n_binary_tests <- 3
#     
#     y <- readRDS( paste0("y_Data_Ord_N_", N) ) ##############  
#     df <- readRDS( paste0("df_Data_Ord_N_", N) ) ############## 
#     df_pos <- filter(df, d_ind == 1)
#     df_neg <- filter(df, d_ind == 0)
#     
#     nrow(df_pos)
#     sum(df_pos$results[,1] == 0)
#     sum(df_pos$results[,2] == 0)
#     sum(df_pos$results[,3] == 0)
#     sum(df_pos$results[,4] == 0)
#     sum(df_pos$results[,5] == 0)
#     sum(df_pos$results[,6] == 0)
#     
#     # data.frame(table(data.frame(results)))
#     
#     cor(df$latent_results[,1], df$latent_results[,2])
#     cor(df$latent_results[,1], df$latent_results[,3])
#     cor(df$latent_results[,1], df$latent_results[,4])
#     cor(df$latent_results[,1], df$latent_results[,5])
#     cor(df$latent_results[,1], df$latent_results[,6])
#     
#     cor(df_pos$latent_results[,1], df_pos$latent_results[,2])
#     cor(df_pos$latent_results[,1], df_pos$latent_results[,3])
#     cor(df_pos$latent_results[,1], df_pos$latent_results[,4])
#     cor(df_pos$latent_results[,1], df_pos$latent_results[,5])
#     cor(df_pos$latent_results[,1], df_pos$latent_results[,6])
#     
#     cor(df_neg$latent_results[,1], df_neg$latent_results[,2])
#     cor(df_neg$latent_results[,1], df_neg$latent_results[,3])
#     cor(df_neg$latent_results[,1], df_neg$latent_results[,4])
#     cor(df_neg$latent_results[,1], df_neg$latent_results[,5])
#     cor(df_neg$latent_results[,1], df_neg$latent_results[,6])
#     
#     #print(round(sum(d_ind)/N,2))
#     
#     # Se
#     Phi_Se_observed_vec <- c()
#     for (i in 1:n_binary_tests) {
#       Phi_Se_observed_vec[i] <- round(qnorm(sum(df_pos$results[,i])/nrow(df_pos)),2)
#     }
#     print(Phi_Se_observed_vec)
#     
#     
#     # Fp
#     Phi_Sp_observed_vec <- c()
#     for (i in 1:n_binary_tests) {
#       Phi_Sp_observed_vec[i] <-  round(  qnorm( 1 - ((nrow(df_neg) - sum(df_neg$results[,i]))/nrow(df_neg))  ),  2)
#     }
#     print(Phi_Sp_observed_vec)
#     
#     print(paste("N = ", N))
#     
#   }
#   
#   
#   
# 
#   
# #   y_bin <- y
#  
#  # 
#  # 
# 
# #  
# 

{
  
  
  n_tests <- n_tests
  n_class <- 2
  n_covariatres <- 0
  prior_mean_vec <- array(0,  dim = c(n_tests*(n_covariatres + 1), n_class))
  prior_mean_vec
  
  
  
  prior_sd_vec  <- array(0, dim = c(n_tests*(n_covariatres + 1), n_class))
  prior_sd_vec
  
  
  n_ord_tests <- 3
  max_threshold <- 9
  prior_ind_dir_alpha <- array(1, dim = c(n_class, n_ord_tests, max_threshold + 1))
  
  group <- rep(1, N)
  prior_prev <- array(1, dim = c(length(unique(group)), n_class))
  
  
  
  initial_prev <- array(dim = c(length(unique(group)), n_class))
  
  for (g in 1:length(unique(group))) { 
    if (n_class == 2) { 
      initial_prev[g,2] <- ifelse(0 %in% y[,1], sum(y[,1])/nrow(y), 0.1)
      initial_prev[g,1] <- 1 - initial_prev[g,2]
    } else {
      for (c in 1:n_class) { 
        initial_prev[g,c] <- 1/n_class
        
      }
    }
  }
  
  
  
  samples <- rep(NA, 10000)
  
  sort(samples)[250]
  sort(samples)[9750]
  
  
  
  initial_prev <- array(dim = c(length(unique(group)), n_class))
  
  
  
  n_chains <- 4
  n_iter <- 200
  n_burnin <- 200
  n_thin <- 1
  n_iter / n_thin
  n_class <- 2
  
  n_covariates <- 0
  initial_beta <- array(0.001, dim = c(n_class, n_tests, n_covariates+1))
  initial_beta[1,,1] <- -1
  initial_beta[2,,1] <- 1
  
  
  
  CI_class <-  c(1,0) # if want to assume CI for subset of classes (1 = yes, 0 = no)
  CI_test_pair_d <- matrix(0,nrow = n_tests, ncol = n_tests)
  CI_test_pair_d[n_tests, 1:n_tests] <- rep(1, n_tests)
  CI_test_pair_d[1:n_tests, n_tests] <- rep(1, n_tests)
  CI_test_pair_nd <- matrix(1, nrow = n_tests, ncol = n_tests)
  CI_test_pair <-  array(c(CI_test_pair_nd, CI_test_pair_d), dim = c(n_tests, n_tests, n_class)) # if want to assume CI for subset of test-pairs in each class (1 = yes, 0 = no)
  
  
  group <- array(1, dim = c(N, 1))
  
  X <- array(1, dim = c(N, 1))
  
  initial_prev <- array(dim = c(length(unique(group)), n_class))
  
  for (g in 1:length(unique(group))) { 
    if (n_class == 2) { 
      initial_prev[g,2] <- ifelse(0 %in% y[,1], sum(y[,1])/nrow(y), 0.1)
      initial_prev[g,1] <- 1 - initial_prev[g,2]
    } else {
      for (c in 1:n_class) { 
        initial_prev[g,c] <- 1/n_class
        
      }
    }
  }
  
  seed=123
  corr_W <- array(0 , dim = c( n_tests, n_tests,n_class)) 
  MH_corr = T
  
  
  
  
  
  MH_cutpoints = T
  induced_dir = F
  update_prog = F
  
  group = rep(1, N)
  intercept_only <- T
  f = c(10,10)
  n_class = n_class
  
  CI = T
  latent_scale = 1
  prior_ind_dir = 0
  
  perfect_gs = T
  CI_class = rep(0, n_class) #if want to assume CI for subset of classes 
  CI_test_pair =  array(0, dim = c(n_tests, n_tests, n_class))  # if want to assume CI for subset of test-pairs 
  prior_Omega = array(diag(rep(1, n_tests)), dim = c(n_tests, n_tests, n_class)) # prior corr matrix for each class
  #   prior_mean_vec = prior_mean_vec,
  #   prior_sd_vec  = prior_sd_vec,
  prior_mean_vec = array(c(rep(-1,3),rep(-1,3), rep(1,3), rep(1,3)), dim = c(n_tests*(n_covariates + 1), n_class))
  prior_sd_vec = array(c(rep(1,3), rep(1,3)), dim = c(n_tests*(n_covariates + 1), n_class))
  prior_ind_dir_alpha = prior_ind_dir_alpha <- array(1, dim = c(n_class, n_ord_tests, max_threshold + 1))
  
  
  vectorised_cuts = F
  
 
  
}




# mat <- matrix(rnorm(N * n_tests), nrow = N, ncol = n_tests)
# 
# outs <- AF_block_fn_check(AF_mat = mat, 
#                   row_index_start = 0, 
#                   col_index_start = 0,
#                   row_segment_length = N, 
#                   col_segment_length = 3)
# 
# 
# outs[[1]] - outs[[2]]
# 
# 
# mat_col_vec <- array(mat[,1], dim = c(length(mat[,1]) , 1))
# fn_AF_segment(mat_col_vec, index_start = 0, segment_length = 10)
# head(mat_col_vec, 10)
# 
# mat_row_vec <- array(mat[1,], dim = c(1, length(mat[1,])))
# fn_AF_segment(mat_row_vec, index_start = 0, segment_length = 3)
# head(c(mat_row_vec), 3)



# 
# 
{
  
  
  n_params_main <- (n_class - 1)  + n_class * choose(n_tests, 2) + n_class * (n_covariates + 1)  * n_tests 
  n_us <- n_class * N * n_tests 
  n_params <- n_params_main + n_us 
  
  
  
  n_corrs <- n_class * 0.5 * n_tests * (n_tests - 1)
  n_coeffs <- n_class * n_tests * 1
  
  n_params <- N * n_tests * n_class + 43
  theta_vec <- rnorm(n = n_params, mean = 0, sd = 0.05)
  homog_corr <- F
  rough_approx <- T
  prior_lkj <- 6
  mvr_cholesky <- T
  n_us <-  N * n_tests * n_class
  
  exclude_priors = FALSE
  lkj_prior_method = 1
  grad_main = TRUE
  #  grad_main = FALSE
  grad_nuisance = TRUE
  #   grad_main = FALSE
  #     grad_nuisance = FALSE
  CI = CI
  rough_approx = FALSE
  homog_corr = homog_corr
  lkj_cholesky = mvr_cholesky
  lkj_cholesky_eta = prior_lkj
  prior_coeffs_mean = prior_mean_vec
  prior_coeffs_sd = prior_sd_vec
  n_class = n_class
  n_tests = n_tests
  
}


  
  
 







# -| ------------------------------    Pilot study - Finding the optimal N_{chunks} for BayesMVP ----------- ----------------------------------------------------------



# cores <- c(1, 2, 4, 6, 8)
# cores <- c(1, 32,  64, 96)
# cores <- c(8, 64)



if (parallel::detectCores() < 17)  { 
  options(mc.cores = 8*1)
  cores = 8
} else { 
  options(mc.cores = 96*1)
  cores = 64
}



times <- c()




{
  
  
  denom <-  N/1000
  N_iter <- round(1000 / denom ) ; N_iter
  ####   N_iter <-   N_iter       * 15
  N_iter <-   N_iter       * 3 # for MD 
  #   N_iter <-   N_iter  / 2 # AD
  N_iter <-   N_iter * 5  / 2 # for "inferior" param
  N_iter <- N_iter / 10
  
  N_iter <- round( N_iter / 10 , 0)
  
  
  
}




grad = rep(0, n_params)
M_inv_us_vec_if_Euclidean = rep(1, n_us)





{
  n_chunks_vec_N_500   <-  c(1) # c(1, 2, 4,  5,   10) # ~ 3 mins
  n_chunks_vec_N_1000  <-  c(1) #  c(1, 2, 4,  5,   10) # ~ 3 mins
  n_chunks_vec_N_2500  <-  c(1) # c(1, 2, 4,  5,   10,  20,  25) # ~ 4 mins
  n_chunks_vec_N_5000  <- c(1, 2, 4,  5,   10,  20,  25,  40,  50) # ~ 5 mins
  n_chunks_vec_N_12500 <- c(1, 5, 10, 20, 25,  40,  50,  100, 125,  200,  250) # ~ 6 mins
  n_chunks_vec_N_25000 <- c(1, 5, 10, 20, 25,  40,  50,  100, 125,  200,  250, 400, 500) # ~ 7 mins
}


n_runs <- 10

start_index <- 1
 N_vec <- c(500, 1000, 2500, 5000, 12500, 25000)
#N_vec <- c(500, 1000, 2500, NA, NA, NA)
# N_vec <- c(1000, 2500, 5000, 12500, 25000)

times_array <- array(dim = c(length(N_vec), length(n_chunks_vec_N_25000), n_runs, length(cores)))





for (dataset_index in start_index:length(N_vec[!is.na(N_vec)]))  {
  
  N <- N_vec[dataset_index]
  
  if (N == 500)   n_chunks_vec = n_chunks_vec_N_500
  if (N == 1000)  n_chunks_vec = n_chunks_vec_N_1000
  if (N == 2500)  n_chunks_vec = n_chunks_vec_N_2500
  if (N == 5000)  n_chunks_vec = n_chunks_vec_N_5000
  if (N == 12500) n_chunks_vec = n_chunks_vec_N_12500
  if (N == 25000) n_chunks_vec = n_chunks_vec_N_25000
  
  
  
  for (kkk in 1:length(n_chunks_vec))  {
    
    num_chunks <- n_chunks_vec[kkk]
    
    {
      
      
      
      
      
      try({
        stopCluster(cl)
      }, silent = TRUE)
      
      
      
      cl <- parallel::makeCluster(max(cores), outfile="") # FORK only works with linux
      #    cl <- makeForkCluster(n_chains, outfile="")  # FORK only works with linux 
      doParallel::registerDoParallel(cl)
      
      comb <- function(x, ...) {
        lapply(seq_along(x),
               function(i) c(x[[i]], lapply(list(...), function(y) y[[i]])))
      }
      
      
      seed = 123
      set.seed(seed)
      doRNG::registerDoRNG(seed = seed)
      
      
      
    }
    
    
    
    {
      N_iter_2 <- 1
      if (N == 500)   {     N_iter = 400 ;        }
      if (N == 1000)  {     N_iter = 200 ;        }
      if (N == 2500)  {     N_iter = 80  ;        }
      if (N == 5000)  {     N_iter = 40  ;        }
      if (N == 12500) {     N_iter = 16  ;        }
      if (N == 25000) {     N_iter = 8   ;        }
    }
    
    
    N_iter <- N_iter 
    
    
    n_params_main <- (n_class - 1)  + n_class * choose(n_tests, 2) + n_class * (n_covariates + 1)  * n_tests 
    n_us <- 1 * N * n_tests 
    n_params <- n_params_main + n_us 
    theta_vec = rep(0.01, n_params) 
    
    
    
    
    theta_vec[   (n_us + n_corrs + 1):(n_us + n_corrs + n_coeffs/2)  ] <- rep(-1, n_coeffs/2)
    theta_vec[(n_us + n_corrs + 1 + n_coeffs/2):(n_us + n_corrs + n_coeffs)] <- rep(1, n_coeffs/2)
    #  log_posterior_iter_0 <- log_posterior[kk]
    
    theta_vec[n_params] =  -0.6931472  # this is equiv to starting val of p = 0.20!  since: 0.5 * (tanh( -0.6931472) + 1)  = -0.6931472
    
    index_us = 1:n_us
    index_main = (n_us+1):(n_us + n_params_main)
    
    
    
    for (iii in 1:n_runs) {
      
      
      
      for (jj in (1:length(cores))) {
        
        if (N_iter_2 > 1) theta_trace <- array(dim = c(N_iter_2, n_params))
        
        print(cores[jj])
        
        n_chains <- cores[jj]
        
        
        
        
        tictoc::tic("timer")
        
        
        {
          #  theta_vec <- round(theta_vec, 5)
          
          rf <-  doRNG::`%dorng%`( 
            foreach::foreach(kk = 1:n_chains,    .packages = c("Rcpp",
                                                               "Boom",
                                                               "BayesMVP") ),   {
                                                                                               
                                                                                                                                                      
                                                                                                      
                                                                   eps =  0.0000000001
                                                                   
                                                                    
                                                                   other_lp_and_grad_args[[9]] = num_chunks
                                                                single_iter_outs <-   BayesMVP::Rcpp_fn_post_burnin_HMC_post_adaptation_phase_float_big_version( Model_type = "MVP_LC",
                                                                                                                                                                 theta_main_array =  theta_vec[index_main],
                                                                                                                                                                 theta_us_array = theta_vec[index_us],
                                                                                                                                                                 log_lik_array = rep(0, N),
                                                                                                                                                                 y = y_master_list_seed_123_datasets[[dataset_index]],
                                                                                                                                                                 X = X_list,
                                                                                                                                                                 other_args = other_lp_and_grad_args,
                                                                                                                                                                 tau_jittered = FALSE,
                                                                                                                                                                 n_iter = N_iter,
                                                                                                                                                                 n_chain_for_loading_bar = kk,
                                                                                                                                                                 tau = eps * 20,
                                                                                                                                                                 eps = eps,
                                                                                                                                                                 log_posterior = 10000,
                                                                                                                                                                 M_inv_us_array =  1 /  rep(1, n_us),
                                                                                                                                                                 M_dense_main = M_dense_main,
                                                                                                                                                                 M_inv_dense_main = M_inv_dense_main,
                                                                                                                                                                 M_inv_dense_main_chol = M_inv_dense_main_chol)
                                                                                                                                                      
                                         
                                                                 
                                                                 
                                                                 
                                                                 
                                                               }
          )
          
          
        }
        print(tictoc::toc(log = TRUE))
        log.txt <- tictoc::tic.log(format = TRUE)
        tictoc::tic.clearlog()
        timer <- unlist(log.txt)
        time <- as.numeric( substr(start = 0, stop = 100,  strsplit(  strsplit( timer, "[:]")[[1]] , "[s]")[[2]][1] ) )
        
        times[jj] <- time
      }
      
      # try({
      #   stopCluster(cl)
      # }, silent = TRUE)
      
      print(paste("N = ", N))
      comment(print(iii))
      times_array[dataset_index, kkk, iii, ]  <- print(round(dput(times), 3))
      
    }
    
    
    # try({
    #   stopCluster(cl)
    # }, silent = TRUE)
    
    
    
    {
      print(paste("N = ", N))
      print(paste("num_chunks = ", num_chunks))
      print(paste("cores = ", cores))
      print( round((times_array[dataset_index, kkk,,]), 2))
      #  print( round(colMeans(times_array), 2))
      print(signif(times/cores, 3))
    } 
    
    
  }
  
  
  
  {
    file_name <- "determining_optimal_N_chunks_pilot_study"
    file_name <- paste0(file_name, "_N_", N)
    if (parallel::detectCores() < 17)  file_name <- paste0("Laptop_", file_name)
    else                               file_name <- paste0("HPC_",    file_name)
    
    saveRDS(times_array[dataset_index,,,], file = file_name)
  }
  
  try({  
    beepr::beep("random")
  })
  
  
} 









# Plot / table results  for N_chunks pilot study   ------------------------------------------------------------------------------------------------------------


N_vec <- c(500, 1000, 2500, 5000, 12500, 25000)

#  HPC
N_chunks_pilot_results_for_each_N_HPC <- list()
N_chunks_pilot_results_medians_for_each_N_HPC <- list()
N_chunks_pilot_results_SD_for_each_N_HPC <- list()

for (dataset_index in 1:length(N_vec)) {
  {
    file_name <- "determining_optimal_N_chunks_pilot_study"
    file_name <- paste0(file_name, "_N_", N_vec[dataset_index])
    file_name <- paste0("HPC_",    file_name)
    N_chunks_pilot_results_for_each_N_HPC[[dataset_index]] <- readRDS(file_name)
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
    file_name <- "determining_optimal_N_chunks_pilot_study"
    file_name <- paste0(file_name, "_N_", N_vec[dataset_index])
    file_name <- paste0("Laptop_", file_name)
    N_chunks_pilot_results_for_each_N_Laptop[[dataset_index]] <- readRDS(file_name)
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










# End of "N_chunks" pilot study   ------------------------------------------------------------------------------------------------------------






# - | -----------------------------    Pilot study - Parallel scalability for: (1) BayesMVP (for various chunks); (2) Mplus ; and (3) Stan (using C++ AD function)   ----------- ----------------------------------------------------------


{
  
  unloadNamespace("BayesMVP")
  require(BayesMVP)
  
  times <- c()
  
  if (parallel::detectCores() < 17)  {  # Laptop
    
            options(mc.cores = 8*2)
            cores = c(2, 4, 8) #  c(4,, 8, 16)
            
            {
              n_chunks_vec_N_500   <- c(1, 2)  
              n_chunks_vec_N_1000  <- c(1, 2)
              n_chunks_vec_N_2500  <- c(1, 5)  
              n_chunks_vec_N_5000  <- c(1, 10) 
              n_chunks_vec_N_12500 <- c(1, 25) 
              n_chunks_vec_N_25000 <- c(1, 125) 
            }
    
  } else {  # HPC
            
              options(mc.cores = 96*1)
              cores = c(8, 16, 32, 64)
            
            {
              n_chunks_vec_N_500   <- c(1, 2)
              n_chunks_vec_N_1000  <- c(1, 2)
              n_chunks_vec_N_2500  <- c(1, 5)
              n_chunks_vec_N_5000  <- c(1, 5)
              n_chunks_vec_N_12500 <- c(1, 40)
              n_chunks_vec_N_25000 <- c(1, 25)
              
            }
    
  }
  
  
  
  
  ##  cores = c(2, 4, 6, 8)
  
  
  grad = rep(0, n_params)
  M_inv_us_vec_if_Euclidean = rep(1, n_us)
  
  
  
  # cores = c(1, 8)
  # n_chunks_vec_N_5000  <- c(10)
  
  
  n_runs <- 3
  
  start_index <- 1
  N_vec <- c(500, 1000, 2500, 5000, 12500, 25000)
  #   N_vec <- c(NA, NA, NA, NA, 12500, 25000)
  #   N_vec <- c(500, NA, NA, NA, NA, NA)
  #   N_vec <- c( 12500)
  ###  start_index <-  4
  end_index <- 6
  
  
  # start_index <- 6
  #    N_vec <- c(NA, NA, NA, NA, NA, 25000)
  # end_index <- 6
  
  
  times_array <- array(dim = c(length(N_vec), length(n_chunks_vec_N_25000), n_runs, length(cores)))
  
  algorithm <- "MD_BayesMVP" ;  test_fn = TRUE
  #    algorithm <- "MD_BayesMVP" ;  test_fn = FALSE
  #   algorithm <- "AD_Stan"
  #   algorithm <- "Mplus_standard"
  #  algorithm <- "Mplus_WCP"
  
  
}



for (dataset_index in start_index:end_index)  {
  
  
  
  N <- N_vec[dataset_index]
  
  if (N == 500)   n_chunks_vec = n_chunks_vec_N_500
  if (N == 1000)  n_chunks_vec = n_chunks_vec_N_1000
  if (N == 2500)  n_chunks_vec = n_chunks_vec_N_2500
  if (N == 5000)  n_chunks_vec = n_chunks_vec_N_5000
  if (N == 12500) n_chunks_vec = n_chunks_vec_N_12500
  if (N == 25000) n_chunks_vec = n_chunks_vec_N_25000
  
  
  if (algorithm == "MD_BayesMVP")  { 
    n_chunks_vec = n_chunks_vec
  } else if   (algorithm == "AD_Stan")  { 
    n_chunks_vec = c(1)
  } else {  ## Mplus
    n_chunks_vec = c(1)
  }
  
  
  
  for (kkk in 1:length(n_chunks_vec))  {
    
    num_chunks <- n_chunks_vec[kkk]
    
    {
      
      
      
      
      
      try({
        stopCluster(cl)
      }, silent = TRUE)
      
      
      seed = 123
      set.seed(seed)
      
      
      
      if (algorithm == "MD_BayesMVP")  { 
        other_lp_and_grad_args[[41]] <- FALSE # sampling_option = 11
        Mplus_ind = FALSE
        sampling_option = 11
      } else if   (algorithm == "AD_Stan")  { 
        # sampling_option = 10
        other_lp_and_grad_args[[41]] <- TRUE
        sampling_option = 10
        Mplus_ind = FALSE
      } else {  ## Mplus
        Mplus_ind = TRUE
      }
      
      if (test_fn == TRUE) sampling_option = 100
      
      
      
      if (Mplus_ind == FALSE ) {   
        
        cl <- parallel::makeCluster(max(cores), outfile="") # FORK only works with linux
        #    cl <- makeForkCluster(n_chains, outfile="")  # FORK only works with linux 
        doParallel::registerDoParallel(cl)
        
        comb <- function(x, ...) {
          lapply(seq_along(x),
                 function(i) c(x[[i]], lapply(list(...), function(y) y[[i]])))
        }
        
        
        
        doRNG::registerDoRNG(seed = seed)
        
      }
      
      
      
    }
    
    
    
    {
      N_iter_2 <- 1
      if (N == 500)   {     N_iter = 400 ;        }
      if (N == 1000)  {     N_iter = 200 ;        }
      if (N == 2500)  {     N_iter = 80  ;        }
      if (N == 5000)  {     N_iter = 40  ;        }
      if (N == 12500) {     N_iter = 16  ;        }
      if (N == 25000) {     N_iter = 8   ;        }
    }
    
    
    if (algorithm == "MD_BayesMVP")  { 
      N_iter <- N_iter 
      n_Leapfrog <- 40
    } else if   (algorithm == "AD_Stan")  { 
      N_iter <- round(N_iter / 2, 0)
      n_Leapfrog <- 10
    } else {  ## Mplus
      N_iter <- N_iter 
    }
    
    
    
    
    
    n_params_main <- (n_class - 1)  + n_class * choose(n_tests, 2) + n_class * (n_covariates + 1)  * n_tests 
    n_us <- 1 * N * n_tests 
    n_params <- n_params_main + n_us 
    theta_vec = rep(0.01, n_params) 
    
    
    
    
    theta_vec[   (n_us + n_corrs + 1):(n_us + n_corrs + n_coeffs/2)  ] <- rep(-1, n_coeffs/2)
    theta_vec[(n_us + n_corrs + 1 + n_coeffs/2):(n_us + n_corrs + n_coeffs)] <- rep(1, n_coeffs/2)
    #  log_posterior_iter_0 <- log_posterior[kk]
    
    theta_vec[n_params] =  -0.6931472  # this is equiv to starting val of p = 0.20!  since: 0.5 * (tanh( -0.6931472) + 1)  = -0.6931472
    
    index_us = 1:n_us
    index_main = (n_us+1):(n_us + n_params_main)
    
    
    for (jj in (1:length(cores))) {
      
      for (iii in 1:n_runs) {
        
        
        if (N_iter_2 > 1) theta_trace <- array(dim = c(N_iter_2, n_params))
        
        print(paste("cores = ", cores[jj]))
        
        n_chains <- cores[jj]
        
        

        
        
        tictoc::tic("timer")
        
        
        if (Mplus_ind == FALSE ) {   
          {
            #  theta_vec <- round(theta_vec, 5)
            
            rf <-  doRNG::`%dorng%`( 
              foreach::foreach(kk = 1:n_chains,    .packages = c("Rcpp",
                                                                 "Boom",
                                                                 "BayesMVP") ),   {
                                                                   
                                                                   
                                                   # tic()
                                                   for (i in 1:N_iter_2) {
                                                     
                                                     
                                                     
                                                     eps =  0.0000000001
                                                     other_lp_and_grad_args[[9]] = num_chunks
                                                     single_iter_outs <-   BayesMVP::Rcpp_fn_post_burnin_HMC_post_adaptation_phase_float_big_version( Model_type = Model_type,
                                                                                                                          theta_main_array =  theta_vec[index_main],
                                                                                                                          theta_us_array = theta_vec[index_us],
                                                                                                                          log_lik_array = rep(0, N),
                                                                                                                          y = y_master_list_seed_123_datasets[[dataset_index]],
                                                                                                                          X = X_list,
                                                                                                                          other_args = other_lp_and_grad_args,
                                                                                                                          tau_jittered = FALSE,
                                                                                                                          n_iter = N_iter,
                                                                                                                          n_chain_for_loading_bar = kk,
                                                                                                                          tau = eps * n_Leapfrog,
                                                                                                                          eps = eps,
                                                                                                                          log_posterior = 10000,
                                                                                                                          M_inv_us_array =  1 /  rep(1, n_us),
                                                                                                                          M_dense_main = M_dense_main,
                                                                                                                          M_inv_dense_main = M_inv_dense_main,
                                                                                                                          M_inv_dense_main_chol = M_inv_dense_main_chol)
                                                                                                                                                      

                                                     
                                                     
                                                     
                                                   }
 
                                                                   
                                                                   
                                                                   
                                                                   
                                      }
            )
            
            
          }
          
        } else {  ### if using Mplus 
          
          
          
          if (algorithm == "Mplus_standard") { 
            
            n_cores <- cores[jj]
            n_chains <- cores[jj]
            
          } else if ((algorithm == "Mplus_WCP") && (parallel::detectCores() > 16)) { 
            
            n_cores <- cores[jj]
            n_chains <- 8 
            
          }
          
          
          
          
          {
            
            
            
            y_for_Mplus <-  y_master_list_seed_123_datasets[[dataset_index]] 
            
            df <- data.frame(y_for_Mplus) %>% 
              dplyr::rename( u1 = X1,
                             u2 = X2, 
                             u3 = X3,
                             u4 = X4,
                             u5 = X5)
            
            
            prior_IW_d <- 10   # w/ prior-only model (N=2) get ~ (-0.67, 0.67) interval -  approx. equiv. to LKJ(2)
            prior_IW_nd <- 24 # w/ prior-only model (N=2) get ~ (-0.41, 0.41) interval -  approx. equiv. to LKJ(10)
            
            
          }
          
          
          
          
          
          {
            
            
            
            {
              N_iter_2 <- 1
              if (N == 500)   {     N_iter = 400 ;        }
              if (N == 1000)  {     N_iter = 200 ;        }
              if (N == 2500)  {     N_iter = 80  ;        }
              if (N == 5000)  {     N_iter = 40  ;        }
              if (N == 12500) {     N_iter = 16  ;        }
              if (N == 25000) {     N_iter = 8   ;        }
            }
            # 
            
            fb_iter =   5 * N_iter   ; n_thin = 1 ; fb_iter * n_thin
            
            print(paste("N = ", N))
            print(paste("n_cores = ", n_cores))
            print(paste("N_chains = ", n_chains))
            print(paste("N_iter (pb) = ",  0.5 * fb_iter * n_thin ))
            
          }
          
          
          if (algorithm == "Mplus_WCP")   { 
            fb_iter =  30  * N_iter   ; n_thin = 1 ; fb_iter * n_thin
          }
          
          
          { 
            #  tictoc::tic("mplus timer")
            
            
            fit_mplus <- mplusObject(TITLE = "Bayesian LCM-MVP - for mixed binary and/or ordinal data (up to 10 categories)",
                                     #   DATA = "FILE = Mplus.dat;",
                                     USEVARIABLES = "u1 u2 u3 u4 u5;",
                                     VARIABLE = "  
                                       CATEGORICAL = u1-u5 ;
                                       CLASSES = C(2);", 
                                     #    MONTECARLO = paste("SEED = ", seed, ";"), 
                                     ANALYSIS = paste0("ESTIMATOR = BAYES;",  "\n",
                                                       " CHAINS = ", n_chains, ";",   "\n",
                                                       " PROCESSORS = ", n_cores, ";",   "\n",
                                                       " TYPE = MIXTURE;",   "\n", 
                                                       " FBITERATIONS = ", fb_iter, ";",  "\n",
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
                                                 p31 ~ D(5, 10);  ! equiv to p ~ Beta(3, 9)  
                                                ! p31 ~ D(1, 1); 
                                                
                                                      p11 ~ N(+0.40, 0.140625); 
                                                      p12-p15 ~ N(0, 1);   
                    
                                                       p26 ~ N(-2.10, 0.0625); 
                                                      p27-p30 ~ N(0, 1);  "), 
                                     SAVEDATA = "bparameters = bparam.dat;",
                                     rdata = data.frame(df),
                                     quiet = FALSE
            )
            
            res_plus <- mplusModeler(fit_mplus,
                                     modelout = paste0("mplus_model_seed_", seed, "_N_", N, ".inp"), 
                                     writeData = "always",
                                     # Mplus_command = "/opt/mplusdemo/",
                                     run = 1)
            
            get_results(res_plus, "summaries")
            
            # print(tictoc::toc(log = TRUE))
            # log.txt <- tictoc::tic.log(format = TRUE)
            # tictoc::tic.clearlog()
            # time_total <- unlist(log.txt)
            
            
            ##  if (seed == 10)   beepr::beep("random") # make sound to know model has finished running 
            
          }
          
          
          
          
          
        }
        
        
        
        print(tictoc::toc(log = TRUE))
        log.txt <- tictoc::tic.log(format = TRUE)
        tictoc::tic.clearlog()
        timer <- unlist(log.txt)
        time <- as.numeric( substr(start = 0, stop = 100,  strsplit(  strsplit( timer, "[:]")[[1]] , "[s]")[[2]][1] ) )
        
        times[jj] <- time
        
        
        times_array[dataset_index, kkk, iii, jj]  <- print(round( times[jj], 3) )
        
        
        
        {
          
          print(paste("N = ", N))
          print(paste("num_chunks = ", num_chunks))
          print(paste("n_chunks_vec = ",    n_chunks_vec))
          print(paste("cores = ", cores))
          print(paste("replication = ", iii))
          ## print( round((times_array[dataset_index, kkk,,]), 2))
          #  print( round(colMeans(times_array), 2))
          ##  print(signif(times/cores, 3))
        } 
        
        
        
        
      }  #  ------   end of N_chains loop
      
      # try({
      #   stopCluster(cl)
      # }, silent = TRUE)
      
      
      
    } # ---- end of N_runs loop
    
    
    # try({
    #   stopCluster(cl)
    # }, silent = TRUE)
    
    
    
    {
      print(paste("N = ", N))
      print(paste("num_chunks = ", num_chunks))
      print(paste("n_chunks_vec = ",    n_chunks_vec))
      print(paste("cores = ", cores))
      ## print( round((times_array[dataset_index, kkk,,]), 2))
      #  print( round(colMeans(times_array), 2))
      ##  print(signif(times/cores, 3))
    }
    
    
  } #  -----  end of N_chunks loop
  
  
  
  {
    file_name <- "parallel_scalability_pilot_study"
    file_name <- paste0(file_name, "_N_", N)
    file_name <- paste0(file_name, "_algorithm_", algorithm)
    if (sampling_option == 100) file_name <- paste0(file_name, "_test_fn_")
    if (parallel::detectCores() < 17)  file_name <- paste0("Laptop_", file_name)
    else                               file_name <- paste0("HPC_",    file_name)
    
    saveRDS(times_array[dataset_index,,,], file = file_name)
  }
  
  try({  
    beepr::beep("random")
  })
  
  
} 











# Plot / table results  for scalability / parallisability pilot study   ------------------------------------------------------------------------------------------------------------
 

# if (parallel::detectCores() < 17)  {  # Laptop
#   
#   options(mc.cores = 8*2)
#   cores = c(2, 4, 8) #  c(4,, 8, 16)
#   
#   {
#     n_chunks_vec_N_500   <- c(1, 2)  
#     n_chunks_vec_N_1000  <- c(1, 2)
#     n_chunks_vec_N_2500  <- c(1, 5)  
#     n_chunks_vec_N_5000  <- c(1, 10) 
#     n_chunks_vec_N_12500 <- c(1, 25) 
#     n_chunks_vec_N_25000 <- c(1, 125) 
#   }
#   
# } else {  # HPC
#   
#   options(mc.cores = 96*1)
#   cores = c(8, 16, 32, 64)
#   
#   {
#     n_chunks_vec_N_500   <- c(1, 2)
#     n_chunks_vec_N_1000  <- c(1, 2)
#     n_chunks_vec_N_2500  <- c(1, 5)
#     n_chunks_vec_N_5000  <- c(1, 5)
#     n_chunks_vec_N_12500 <- c(1, 40)
#     n_chunks_vec_N_25000 <- c(1, 25)
#     
#   }
#   
# }
 




N_vec <- c(500, 1000, 2500, 5000, 12500, 25000)
algorithm <-  "MD_BayesMVP"
#   algorithm <-  "AD_Stan"
#####  algorithm <-  "Mplus_standard"
#    algorithm <-  "Mplus_WCP"

 
{
  cores = c(8, 16, 32, 64)
  {
    n_chunks_vec_N_500   <- c(1, 2)
    n_chunks_vec_N_1000  <- c(1, 2)
    n_chunks_vec_N_2500  <- c(1, 5)
    n_chunks_vec_N_5000  <- c(1, 5)
    n_chunks_vec_N_12500 <- c(1, 40)
    n_chunks_vec_N_25000 <- c(1, 25)
    
  }
  
}




{
  
  N_scalability_pilot_results_for_each_N_HPC <- list()
  N_scalability_pilot_results_for_each_N_and_each_N_chains_HPC <- list()
  N_scalability_pilot_results_medians_for_each_N_and_each_N_chains_HPC <- list()
  N_scalability_pilot_results_SD_for_each_N_and_each_N_chains_HPC <- list()
  
  for (dataset_index in 1:length(N_vec)) {
    
    {
      
      file_name <- "parallel_scalability_pilot_study"
      file_name <- paste0(file_name, "_N_", N_vec[dataset_index])
      file_name <- paste0(file_name, "_algorithm_", algorithm)
      if (sampling_option == 100) file_name <- paste0(file_name, "_test_fn_")
      file_name <- paste0("HPC_",    file_name)
      N_scalability_pilot_results_for_each_N_HPC[[dataset_index]] <- readRDS(file_name)
      
      n_chunks_combos <-          dim(   N_scalability_pilot_results_for_each_N_HPC[[dataset_index]] )[1]
      n_runs <-                   dim(   N_scalability_pilot_results_for_each_N_HPC[[dataset_index]] )[2]
      n_cores_combos <-           dim(   N_scalability_pilot_results_for_each_N_HPC[[dataset_index]] )[3]
      
      print(n_cores_combos)
      
      N_scalability_pilot_results_for_each_N_HPC # list of dim length(N_vec) = 6, each with dim = 3 (= N_chunks) x 5 ( = N_runs) x 7 (= cores)
      
      for (N_cores_index in 1:length(N_vec)) {
        N_scalability_pilot_results_medians_for_each_N_and_each_N_chains_HPC[[dataset_index]]  <-    apply( N_scalability_pilot_results_for_each_N_HPC[[dataset_index]], c(1, 3), median, na.rm = TRUE)
        N_scalability_pilot_results_SD_for_each_N_and_each_N_chains_HPC[[dataset_index]]  <-         apply( N_scalability_pilot_results_for_each_N_HPC[[dataset_index]], c(1, 3), sd,     na.rm = TRUE)
      }
      
    }
    
  }
  
  
}



 
{
  
  nrow_df_HPC <-  n_cores_combos * length(N_vec) * n_chunks_combos
  
  N_scalability_pilot_study_df_HPC <- tibble(N = rep(NA, nrow_df_HPC),
                                             N_num = N,
                                             device = rep("HPC", nrow_df_HPC),
                                             time_avg = N,
                                             time_SD = N,
                                             N_chunks = N, 
                                             N_chunks_num = N, 
                                             N_cores = N, 
                                             N_chains = N, 
                                             N_iter = N#, 
                                             # iter_per_second = N, 
                                             # iter_per_second_adj_for_N = N
  )
  
  
  ### View(N_scalability_pilot_study_df_HPC)
  
  
  counter <- 1
  
  for (dataset_index in 1:length(N_vec)) {
    for (N_cores_index in 1:n_cores_combos) {
      for (i in 1:n_chunks_combos) {  # 3 different N_chunks tested
        
        N = N_vec[dataset_index]
        
        N_scalability_pilot_study_df_HPC$N[counter]   <-      as.character(N_vec[dataset_index])
        N_scalability_pilot_study_df_HPC$N_num[counter]   <-  as.numeric(N_vec[dataset_index])
        N_scalability_pilot_study_df_HPC$time_avg[counter]   <-    N_scalability_pilot_results_medians_for_each_N_and_each_N_chains_HPC[[dataset_index]][i, N_cores_index]
        N_scalability_pilot_study_df_HPC$time_SD[counter]    <-    N_scalability_pilot_results_SD_for_each_N_and_each_N_chains_HPC[[dataset_index]][i, N_cores_index]
        
        if (N == 500)     N_chunks = n_chunks_vec_N_500[i]
        if (N == 1000)    N_chunks = n_chunks_vec_N_1000[i]
        if (N == 2500)    N_chunks = n_chunks_vec_N_2500[i]
        if (N == 5000)    N_chunks = n_chunks_vec_N_5000[i]
        if (N == 12500)   N_chunks = n_chunks_vec_N_12500[i]
        if (N == 25000)   N_chunks = n_chunks_vec_N_25000[i]
        
        N_scalability_pilot_study_df_HPC$N_chunks[counter]    <-    as.character(N_chunks)
        N_scalability_pilot_study_df_HPC$N_chunks_num[counter]    <-    N_chunks 
        
        N_scalability_pilot_study_df_HPC$N_cores[counter]  <- cores[N_cores_index]
        
        if (algorithm != "Mplus_WCP") { 
          N_scalability_pilot_study_df_HPC$N_chains[counter] <- cores[N_cores_index]
        } else { 
          if (N_scalability_pilot_study_df_HPC$device[1] == "HPC")     N_scalability_pilot_study_df_HPC$N_chains[counter] <-  8
          if (N_scalability_pilot_study_df_HPC$device[1] == "Laptop")  N_scalability_pilot_study_df_HPC$N_chains[counter] <-  4
        }
        
        
        
        {
          if (N == 500)     N_iter = 400
          if (N == 1000)    N_iter = 200
          if (N == 2500)    N_iter = 80
          if (N == 5000)    N_iter = 40
          if (N == 12500)   N_iter = 16
          if (N == 25000)   N_iter = 8
          
          
          
          if (algorithm == "Mplus_WCP")   { 
            N_iter =  30  * N_iter    
          } else if (algorithm == "Mplus_standard") { 
            N_iter =   5 * N_iter 
          } else if (algorithm == "AD_Stan") {  
            N_iter =    round(N_iter / 2, 0)
          }
          
          # 
          N_scalability_pilot_study_df_HPC$N_iter[counter] <- N_iter
        }
        
        counter = counter + 1
        
      }
    }
  }
  
  
  
}





 # View(N_scalability_pilot_study_df_HPC)

 N_scalability_pilot_study_df_HPC <- filter(N_scalability_pilot_study_df_HPC, !(is.na(N_chunks_num)))



#  Laptop
{
  
  
 
 
  
  {
    
    cores = c(2, 4, 8)
    
    n_chunks_vec_N_500   <- c(1, 2)  
    n_chunks_vec_N_1000  <- c(1, 2)
    n_chunks_vec_N_2500  <- c(1, 5)  
    n_chunks_vec_N_5000  <- c(1, 10) 
    n_chunks_vec_N_12500 <- c(1, 25) 
    n_chunks_vec_N_25000 <- c(1, 125) 
    
    
    
    
  }
  
  
  
  N_scalability_pilot_results_for_each_N_Laptop <- list()
  N_scalability_pilot_results_for_each_N_and_each_N_chains_Laptop <- list()
  N_scalability_pilot_results_medians_for_each_N_and_each_N_chains_Laptop <- list()
  N_scalability_pilot_results_SD_for_each_N_and_each_N_chains_Laptop <- list()
  
  
  for (dataset_index in 1:length(N_vec)) {
    
    {
      
      file_name <- "parallel_scalability_pilot_study"
      file_name <- paste0(file_name, "_N_", N_vec[dataset_index])
      file_name <- paste0(file_name, "_algorithm_", algorithm)
      if (sampling_option == 100) file_name <- paste0(file_name, "_test_fn_")
      file_name <- paste0("Laptop_",    file_name)
      N_scalability_pilot_results_for_each_N_Laptop[[dataset_index]] <- readRDS(file_name)
      
      n_chunks_combos <-          dim(   N_scalability_pilot_results_for_each_N_Laptop[[dataset_index]] )[1]
      n_runs <-                   dim(   N_scalability_pilot_results_for_each_N_Laptop[[dataset_index]] )[2]
      n_cores_combos <-           dim(   N_scalability_pilot_results_for_each_N_Laptop[[dataset_index]] )[3]
      
      print(n_chunks_combos)
      print(n_runs)
      print(n_cores_combos)
      
      N_scalability_pilot_results_for_each_N_Laptop # list of dim length(N_vec) = 6, each with dim = 3 (= N_chunks) x 5 ( = N_runs) x 7 (= cores)
      
      for (N_cores_index in 1:length(N_vec)) {
        N_scalability_pilot_results_medians_for_each_N_and_each_N_chains_Laptop[[dataset_index]]  <-    apply( N_scalability_pilot_results_for_each_N_Laptop[[dataset_index]], c(1, 3), median, na.rm = TRUE)
        N_scalability_pilot_results_SD_for_each_N_and_each_N_chains_Laptop[[dataset_index]]  <-         apply( N_scalability_pilot_results_for_each_N_Laptop[[dataset_index]], c(1, 3), sd,     na.rm = TRUE)
      }
    }
    
  }
  
  
  
  nrow_df_Laptop <- n_cores_combos * length(N_vec) * n_chunks_combos
  N_scalability_pilot_study_df_Laptop <- tibble(N = rep(NA, nrow_df_Laptop),
                                                N_num = N,
                                                device = rep("Laptop", nrow_df_Laptop),
                                                time_avg = N,
                                                time_SD = N,
                                                N_chunks = N, 
                                                N_chunks_num = N, 
                                                N_cores = N, 
                                                N_chains = N, 
                                                N_iter = N#, 
                                                # iter_per_second = N, 
                                                # iter_per_second_adj_for_N = N
  )
  
  
  
  counter <- 1
  
  for (dataset_index in 1:length(N_vec)) {
    for (N_cores_index in 1:n_cores_combos) {
      for (i in 1:2) {  # 3 different N_chunks tested
        
        
        N = N_vec[dataset_index]
        
        N_scalability_pilot_study_df_Laptop$N[counter]   <-      as.character(N_vec[dataset_index])
        N_scalability_pilot_study_df_Laptop$N_num[counter]   <-  as.numeric(N_vec[dataset_index])
        N_scalability_pilot_study_df_Laptop$time_avg[counter]   <-    N_scalability_pilot_results_medians_for_each_N_and_each_N_chains_Laptop[[dataset_index]][i, N_cores_index]
        N_scalability_pilot_study_df_Laptop$time_SD[counter]    <-    N_scalability_pilot_results_SD_for_each_N_and_each_N_chains_Laptop[[dataset_index]][i, N_cores_index]
        
        if (N == 500)     N_chunks = n_chunks_vec_N_500[i]
        if (N == 1000)    N_chunks = n_chunks_vec_N_1000[i]
        if (N == 2500)    N_chunks = n_chunks_vec_N_2500[i]
        if (N == 5000)    N_chunks = n_chunks_vec_N_5000[i]
        if (N == 12500)   N_chunks = n_chunks_vec_N_12500[i]
        if (N == 25000)   N_chunks = n_chunks_vec_N_25000[i]
        
        N_scalability_pilot_study_df_Laptop$N_chunks[counter]    <-    as.character(N_chunks)
        N_scalability_pilot_study_df_Laptop$N_chunks_num[counter]    <-    N_chunks 
        
        N_scalability_pilot_study_df_Laptop$N_cores[counter]  <- cores[N_cores_index]
        ###   N_scalability_pilot_study_df_Laptop$N_chains[counter] <- cores[N_cores_index]
        
        
        if (algorithm != "Mplus_WCP") { 
          N_scalability_pilot_study_df_HPC$N_chains[counter] <- cores[N_cores_index]
        } else { 
          if (N_scalability_pilot_study_df_Laptop$device[counter] == "HPC")     N_scalability_pilot_study_df_HPC$N_chains[counter] <-  8
          if (N_scalability_pilot_study_df_Laptop$device[counter] == "Laptop")  N_scalability_pilot_study_df_HPC$N_chains[counter] <-  4
        }
        
        if (N == 500)     N_iter = 400
        if (N == 1000)    N_iter = 200
        if (N == 2500)    N_iter = 80
        if (N == 5000)    N_iter = 40
        if (N == 12500)   N_iter = 16
        if (N == 25000)   N_iter = 8
        # 
        
        if (algorithm == "Mplus_WCP")   { 
          N_iter =  30  * N_iter    
        } else if (algorithm == "Mplus_standard") { 
          N_iter =   5 * N_iter 
        } else if (algorithm == "AD_Stan") {  
          N_iter =    roound(N_iter / 2, 0)
        }
        
 
        
        N_scalability_pilot_study_df_Laptop$N_iter[counter] <- N_iter
        
        counter = counter + 1
        
      }
    }
  }
  
  
  
}

 
 
 n_chunks_combos
 

 N_scalability_pilot_study_df_Laptop <- filter(N_scalability_pilot_study_df_Laptop, !(is.na(N_chunks_num)))
  View(N_scalability_pilot_study_df_Laptop)



{
  
  N_scalability_pilot_study_df_HPC_and_Laptop <- rbind(N_scalability_pilot_study_df_HPC,
                                                       N_scalability_pilot_study_df_Laptop)
  
  ### View(N_scalability_pilot_study_df_HPC_and_Laptop)
  
  
  N_scalability_pilot_study_df_HPC_and_Laptop <- dplyr::mutate(N_scalability_pilot_study_df_HPC_and_Laptop, 
                                                               N_label =  paste0("N = ",  N_scalability_pilot_study_df_HPC_and_Laptop$N))
  N_scalability_pilot_study_df_HPC_and_Laptop$N_label <- factor( N_scalability_pilot_study_df_HPC_and_Laptop$N_label )
  N_scalability_pilot_study_df_HPC_and_Laptop$N_label <- factor(N_scalability_pilot_study_df_HPC_and_Laptop$N_label, 
                                                                levels = c("N = 500", "N = 1000", "N = 2500", "N = 5000", "N = 12500", "N = 25000"))
  
  
  
  N_scalability_pilot_study_df_HPC_and_Laptop <- dplyr::mutate(N_scalability_pilot_study_df_HPC_and_Laptop, 
                                                               N_and_N_iter_label =  paste0(N_label, ", N_{iter} = ", N_iter))
  N_scalability_pilot_study_df_HPC_and_Laptop$N_and_N_iter_label <- factor( N_scalability_pilot_study_df_HPC_and_Laptop$N_and_N_iter_label )
  N_scalability_pilot_study_df_HPC_and_Laptop$N_and_N_iter_label <- factor(N_scalability_pilot_study_df_HPC_and_Laptop$N_and_N_iter_label, 
                                                                           levels = c("N = 500, N_{iter} = 400",
                                                                                      "N = 1000, N_{iter} = 200", 
                                                                                      "N = 2500, N_{iter} = 80", 
                                                                                      "N = 5000, N_{iter} = 40",
                                                                                      "N = 12500, N_{iter} = 16",
                                                                                      "N = 25000, N_{iter} = 8"))
  
  N_scalability_pilot_study_df_HPC_and_Laptop <-  N_scalability_pilot_study_df_HPC_and_Laptop %>%  
    mutate(N_chains = ifelse(algorithm == "Mplus_WCP", 4, N_chains))  %>% 
    print(n = 1000)
  
  
  N_scalability_pilot_study_df_HPC_and_Laptop <- dplyr::mutate(N_scalability_pilot_study_df_HPC_and_Laptop, 
                                                               N_chains_label =  paste0("N_{chains} = ", N_chains))
  N_scalability_pilot_study_df_HPC_and_Laptop$N_chains_label <- factor(N_scalability_pilot_study_df_HPC_and_Laptop$N_chains_label, levels = c("N_{chains} = 2",
                                                                                                                                              "N_{chains} = 4", 
                                                                                                                                              #"N_{chains} = 6", 
                                                                                                                                              "N_{chains} = 8", 
                                                                                                                                              "N_{chains} = 16", 
                                                                                                                                              "N_{chains} = 32",
                                                                                                                                              "N_{chains} = 64"
                                                                                                                                              #"N_{chains} = 96"
                                                                                                                                              ))
  
  levels(N_scalability_pilot_study_df_HPC_and_Laptop$N_chains_label)
  ### N_scalability_pilot_study_df_HPC_and_Laptop$n_chains_burnin <- factor(N_scalability_pilot_study_df_HPC_and_Laptop$n_chains_burnin)
  
  
  
  N_scalability_pilot_study_df_HPC_and_Laptop$N_chunks <- factor( N_scalability_pilot_study_df_HPC_and_Laptop$N_chunks )
  unique(N_scalability_pilot_study_df_HPC_and_Laptop$N_chunks )
  N_scalability_pilot_study_df_HPC_and_Laptop$N_chunks <- factor(N_scalability_pilot_study_df_HPC_and_Laptop$N_chunks, 
                                                                 levels = c("1",
                                                                            "2", 
                                                                            # "4", 
                                                                            "5",
                                                                            # "8",
                                                                            "10",
                                                                            "25",
                                                                            "40", 
                                                                           #  "50",
                                                                            # "100", 
                                                                            "125"
                                                                            #"200",
                                                                            #"250",
                                                                            #"500"
                                                                           ))
  
 
  
  levels(N_scalability_pilot_study_df_HPC_and_Laptop$N_chunks)
  
  # n_chunks_vec_N_500   <- c(1, 2, 5)  
  # n_chunks_vec_N_1000  <- c(1, 2, 8)
  # n_chunks_vec_N_2500  <- c(1, 2, 20)  
  # n_chunks_vec_N_5000  <- c(1, 20, 40, 100) 
  # n_chunks_vec_N_12500 <- c(1, 50, 100, 250) 
  # n_chunks_vec_N_25000 <- c(1, 125, 200, 500) 
  # 
  # n_chunks_vec_N_500   <- c(1, 2, 5)
  # n_chunks_vec_N_1000  <- c(1, 2, 5)
  # n_chunks_vec_N_2500  <- c(1, 2, 4)
  # n_chunks_vec_N_5000  <- c(1, 5,  10,  20)
  # n_chunks_vec_N_12500 <- c(1, 20, 50,  100)
  # n_chunks_vec_N_25000 <- c(1, 40, 125, 200)
  
  
  N_scalability_pilot_study_df_HPC_and_Laptop <- N_scalability_pilot_study_df_HPC_and_Laptop %>% dplyr::mutate( N_chunks_group = case_when( N_chunks_num == 1 ~ "1 chunk",
                                                                                                                                           # N_chunks_num == 2 ~ "2 chunks",
                                                                                                                                            N_chunks_num %in% c(3:99999) ~ "Optimal N_{chunks}") )
  
  N_scalability_pilot_study_df_HPC_and_Laptop$N_chunks_group <- factor( N_scalability_pilot_study_df_HPC_and_Laptop$N_chunks_group )
  
  N_scalability_pilot_study_df_HPC_and_Laptop$device <- factor( N_scalability_pilot_study_df_HPC_and_Laptop$device)
  
}








N_scalability_pilot_study_df_HPC_and_Laptop %>% arrange(device, N_num, N_chunks_num, N_chains) %>% print(n = 100)


View(N_scalability_pilot_study_df_HPC_and_Laptop)



### saveRDS(N_scalability_pilot_study_df_HPC_and_Laptop, file  = paste0("N_scalability_pilot_study_df_HPC_and_Laptop_", algorithm))



# View( 
#   dplyr::filter(N_scalability_pilot_study_df_HPC_and_Laptop , 
#                 device == "HPC",
#                 N_chunks == "1"
#   )
#   )











# Plots ----------------------------------------------------------------------------------------------------------------------------
#  ----------------------------- plot 1 (a)  --------------------------
if (algorithm == "MD_BayesMVP") {
  {
    plot_N_scalability_pilot_study_plot_1a_HPC <-   ggplot(    dplyr::filter(N_scalability_pilot_study_df_HPC_and_Laptop ,
                                                                             device == "HPC",
                                                                             #   device == "Laptop",
                                                                             #  N_chunks == "1",
                                                                             N_num > 2500
    ),
    mapping = aes(x = N_cores,
                  y =   time_avg,
                  colour = N_chunks,
                  #   linetype = device,
                  shape = N
    ),
    ) +
      geom_point(size = 4) +
      geom_line(linewidth = 1,           aes(x = N_cores,
                                             y =  time_avg,
                                             #   colour = N_chains_label,
                                             linetype = device
      )) +
      theme_bw(base_size = 20) +
      theme(legend.position = "bottom")  +
      ylab("Time (seconds)") +
      xlab("N_{chains}" ) +
      scale_x_continuous(breaks = c(8, 16, 32, 64, 96)) + 
      facet_wrap( ~ N_label, 
                  scales = "free" 
      )
    
    plot_N_scalability_pilot_study_plot_1a_HPC
  }
  
  {
    
    plot_N_scalability_pilot_study_plot_1a_Laptop <-   ggplot(    dplyr::filter(N_scalability_pilot_study_df_HPC_and_Laptop ,
                                                                                #  device == "HPC",
                                                                                device == "Laptop",
                                                                                N_num > 2500
                                                                                #  N_chunks == "1"
    ),
    mapping = aes(x = N_cores,
                  y =   time_avg,
                  
                  colour = N_chunks,
                  shape = N
                  #   colour = N_chunks_group,
                  # linetype = device
    ),
    ) +
      geom_point(size = 4) +
      geom_line(linewidth = 1,           aes(x = N_cores,
                                             y =  time_avg,
                                             #   colour = N_chains_label,
                                             linetype = device
      )) +
      theme_bw(base_size = 20) +
      theme(legend.position = "bottom")  +
      ylab("Time (seconds)") +
      xlab("N_{chains}" ) +
      scale_x_continuous(breaks = c(2, 4, 8)) +
      facet_wrap( ~ N_label,
                  scales = "free"
      )
    
    plot_N_scalability_pilot_study_plot_1a_Laptop
    
  }
  
  plot_N_scalability_pilot_study_plot_1a_HPC +
    plot_N_scalability_pilot_study_plot_1a_Laptop +
    plot_layout(ncol = 1)
  
  {
    plot_scale_factor <- 1
    plot_width <-  16*plot_scale_factor
    plot_height <- 2*9*plot_scale_factor
    }
  
  plot_name <- paste0("plot_N_scalability_pilot_study_", algorithm, "_plot_1a.png")
  png(plot_name ,units = "in", width = plot_width, height=plot_height, res=800)
  plot_N_scalability_pilot_study_plot_1a_HPC +
    plot_N_scalability_pilot_study_plot_1a_Laptop +
    plot_layout(ncol = 1)
  dev.off()
  
} else if (algorithm %in% c("AD_Stan", "Mplus_standard"))  { 
  
  plot_N_scalability_pilot_study_plot_1_HPC_and_laptop <-   ggplot(    dplyr::filter(N_scalability_pilot_study_df_HPC_and_Laptop , 
                                                                                     #  device == "HPC",
                                                                                     N_chunks == "1"
                                                                                     
  ), 
  mapping = aes(x = N_cores,
                y =   time_avg,
                colour = N_label,
                #   linetype = device
  ),
  ) +
    geom_point(size = 4) +
    geom_line(linewidth = 1,           aes(x = N_cores,
                                           y =  time_avg, 
                                           colour = N_label,
                                           #   linetype = device
    )) +
    theme_bw(base_size = 20) +
    theme(legend.position = "bottom")  +
    ylab("Time (seconds)") +
    xlab("N_{chains}" ) +
    scale_x_continuous(breaks = c(2, 4, 6, 8, 16, 32, 64, 96)) + 
    facet_wrap( ~ device,
                scales = "free",
    )
  
  plot_N_scalability_pilot_study_plot_1_HPC_and_laptop
  
  
  
  
  {
    plot_scale_factor <- 1
    plot_width <-  16*plot_scale_factor
    plot_height <- 1*9*plot_scale_factor
  }
  
  
  plot_name <- paste0("plot_N_scalability_pilot_study_", algorithm, "_plot_1.png")
  png(plot_name ,units = "in", width = plot_width, height=plot_height, res=800)
  plot_N_scalability_pilot_study_plot_1_HPC_and_laptop
  dev.off()
  
  
} else if (algorithm %in% c("Mplus_WCP"))  {  
  
  plot_N_scalability_pilot_study_plot_1_HPC_and_laptop <-   ggplot(    dplyr::filter(N_scalability_pilot_study_df_HPC_and_Laptop , 
                                                                                     #   device == "HPC",
                                                                                     N_chunks == "1"
                                                                                     
  ), 
  mapping = aes(x = N_cores,
                y =   time_avg,
                colour = N_label,
                #   linetype = device
  ),
  ) +
    geom_point(size = 4) +
    geom_line(linewidth = 1,           aes(x = N_cores,
                                           y =  time_avg, 
                                           colour = N_label,
                                           #   linetype = device
    )) +
    theme_bw(base_size = 20) +
    theme(legend.position = "bottom")  +
    ylab("Time (seconds)") +
    xlab("N_{cores}" ) +
    scale_x_continuous(breaks = c(2, 4, 6, 8, 16, 32, 64, 96)) + 
    facet_wrap( ~ device,
                scales = "free",
    )
  
  plot_N_scalability_pilot_study_plot_1_HPC_and_laptop
  
  
  
  
  {
    plot_scale_factor <- 1
    plot_width <-  16*plot_scale_factor
    plot_height <- 1*9*plot_scale_factor
  }
  
  
  plot_name <- paste0("plot_N_scalability_pilot_study_", algorithm, "_plot_1.png")
  png(plot_name ,units = "in", width = plot_width, height=plot_height, res=800)
  plot_N_scalability_pilot_study_plot_1_HPC_and_laptop
  dev.off()
  
  
  
}



#  ----------------------------- plot 1 (b) --------------------------
if (algoorithm == "MD_BayesMVP") {
  {
    
    plot_N_scalability_pilot_study_plot_1b_HPC <-   ggplot(    dplyr::filter(N_scalability_pilot_study_df_HPC_and_Laptop ,
                                                                             device == "HPC",
    ),
    mapping = aes(x = N_cores,
                  y =   time_avg,
                  colour = N_label,
                  linetype = device
    ),
    ) +
      geom_point(size = 4) +
      geom_line(linewidth = 1,           aes(x = N_cores,
                                             y =  time_avg,
      )) +
      theme_bw(base_size = 20) +
      theme(legend.position = "bottom")  +
      ylab("Time (seconds)") +
      xlab("N_{chains}" ) +
      scale_x_continuous(breaks = c(8, 16, 32, 64, 96)) +
      facet_wrap( ~ N_chunks_group,
                  scales = "free"
      )
    plot_N_scalability_pilot_study_plot_1b_HPC
    
  }
  
  {
    
    plot_N_scalability_pilot_study_plot_1b_Laptop <-   ggplot(    dplyr::filter(N_scalability_pilot_study_df_HPC_and_Laptop ,
                                                                                #  device == "HPC",
                                                                                device == "Laptop",
                                                                                #  N_chunks == "1"
    ),
    mapping = aes(x = N_cores,
                  y =   time_avg,
                  colour = N_label
    ),
    ) +
      geom_point(size = 4) +
      geom_line(linewidth = 1,           aes(x = N_cores,
                                             y =  time_avg,
                                             linetype = device
      )) +
      theme_bw(base_size = 20) +
      theme(legend.position = "bottom")  +
      ylab("Time (seconds)") +
      xlab("N_{chains}" ) +
      scale_x_continuous(breaks = c(2, 4, 6, 8)) +
      facet_wrap( ~ N_chunks_group,
                  scales = "free"
      )
    plot_N_scalability_pilot_study_plot_1b_Laptop
    
  }
  
  plot_N_scalability_pilot_study_plot_1b_HPC +
    plot_N_scalability_pilot_study_plot_1b_Laptop +
    plot_layout(ncol = 1)
  
  {
    plot_scale_factor <- 1
    plot_width <-  16*plot_scale_factor
    plot_height <- 2*9*plot_scale_factor
    }
  
  plot_name <- paste0("plot_N_scalability_pilot_study_", algorithm, "_plot_1b.png")
  png(plot_name ,units = "in", width = plot_width, height=plot_height, res=800)
  plot_N_scalability_pilot_study_plot_1b_HPC +
    plot_N_scalability_pilot_study_plot_1b_Laptop +
    plot_layout(ncol = 1)
  dev.off()
  
} else if (algorithm %in% c("AD_Stan", "Mplus_standard"))  { 
  
  # no plot (1) (b) for Stan
  
} else if (algorithm %in% c("Mplus_WCP"))  {  
  
}


#  ----------------------------- plot 1 (a) and (b)  on SAME PANEL --------------------------
if (algoorithm == "MD_BayesMVP") {
  
  {
    plot_scale_factor <- 1
    plot_width <-  16*plot_scale_factor
    plot_height <- (18+9 - 4)*plot_scale_factor
  }
  
  plot_name <- paste0("plot_N_scalability_pilot_study_", algorithm, "_plot_1.png")
  png(plot_name ,units = "in", width = plot_width, height=plot_height, res=800)
  wrap_elements(
    plot_N_scalability_pilot_study_plot_1a_HPC +  # theme(legend.position = "none") +
      ylab(" " ) +
      plot_N_scalability_pilot_study_plot_1a_Laptop +  ylab(" " )  +
      plot_layout(ncol = 1)
  ) +
    labs(tag = "Time (seconds)") +
    theme(
      plot.tag = element_text(size = rel(2), angle = 90),
      plot.tag.position = "left"
    ) +
    wrap_elements(
      plot_N_scalability_pilot_study_plot_1b_HPC +  # theme(legend.position = "none") +
        ylab(" " ) +
        plot_N_scalability_pilot_study_plot_1b_Laptop +   ylab(" " )   +
        plot_layout(ncol = 1)
    ) +
    labs(tag = "Time (seconds)") +
    theme(
      plot.tag = element_text(size = rel(2), angle = 90),
      plot.tag.position = "left"
    ) +
    plot_layout(ncol = 1,
                heights = c(1, 0.75))
  dev.off()
  
} else if (algorithm %in% c("AD_Stan", "Mplus_standard"))  { 
  
  ## None for Stan (see plot (1) for Stan above)
  
} else if (algorithm %in% c("Mplus_WCP"))  {  
  
}









#  ----------------------------- plot 2 (a) --------------------------
if (algoorithm == "MD_BayesMVP") {
  
  View(N_scalability_pilot_study_df_HPC_and_Laptop_2)
  
  N_scalability_pilot_study_df_HPC_and_Laptop_2 <-  N_scalability_pilot_study_df_HPC_and_Laptop  %>%
    group_by(device, N_num, N_chunks_num) %>%
    mutate(time_perfect_scaling = min(time_avg))  # %>% print(n = 100)
  
  N_scalability_pilot_study_df_HPC_and_Laptop_2 <- filter(N_scalability_pilot_study_df_HPC_and_Laptop_2, !(is.na(N_chunks_group)) )
  
  {
    
    plot_N_scalability_pilot_study_plot_2a_HPC <-   ggplot(    dplyr::filter(N_scalability_pilot_study_df_HPC_and_Laptop_2 ,
                                                                             device == "HPC",
                                                                             #   device == "Laptop",
                                                                             #  N_chunks == "1"
    ),
    mapping = aes(x = N_cores,
                  y =   N_cores / time_avg,
                  colour = N_chunks_group,
                  linetype = device
    ),
    ) +
      geom_point(size = 5) +
      geom_line(linewidth = 1,   linetype = "dashed",   aes(x = N_cores,
                                                            y =  N_cores / time_perfect_scaling)) +
      geom_line(linewidth = 2,            aes(x = N_cores,
                                              y =  N_cores / time_avg)) +
      theme_bw(base_size = 32) +
      theme(legend.position = "bottom")  +
      ylab("Ratio (N_{chains} / second)") +
      xlab("N_{chains}" ) +
      scale_x_continuous(breaks = c(8, 16, 32, 64)) +
      # geom_abline(slope=1, intercept= 0) +
      facet_wrap( ~ N_label,
                  scales = "free"
      )
    
    plot_N_scalability_pilot_study_plot_2a_HPC
    
    }
  
  {
    
    plot_N_scalability_pilot_study_plot_2a_Laptop <-   ggplot(    dplyr::filter(N_scalability_pilot_study_df_HPC_and_Laptop_2 ,
                                                                                #  device == "HPC",
                                                                                device == "Laptop",
                                                                                #  N_chunks == "1"
    ),
    mapping = aes(x = N_cores,
                  y =   N_cores / time_avg,
                  colour = N_chunks_group,
                  linetype = device
    ),
    ) +
      geom_point(size = 4) +
      geom_line(linewidth = 1,   linetype = "dashed",   aes(x = N_cores,
                                                            y =  N_cores / time_perfect_scaling)) +
      geom_line(linewidth = 2,            aes(x = N_cores,
                                              y =  N_cores / time_avg)) +
      theme_bw(base_size = 32) +
      theme(legend.position = "bottom")  +
      ylab("Ratio (N_{chains} / second)") +
      xlab("N_{chains}" ) +
      scale_x_continuous(breaks = c(2, 4,  8)) +
      facet_wrap( ~ N_label,
                  scales = "free"
      )
    
    plot_N_scalability_pilot_study_plot_2a_Laptop
    
  }
  
  plot_N_scalability_pilot_study_plot_2a_HPC +
    plot_N_scalability_pilot_study_plot_2a_Laptop +
    plot_layout(ncol = 1)
  
  
  {
    plot_scale_factor <- 1
    plot_width <-  16*plot_scale_factor
    plot_height <- 2*9*plot_scale_factor
    }
  
  plot_name <- paste0("plot_N_scalability_pilot_study_", algorithm, "_plot_2a.png")
  png(plot_name ,units = "in", width = plot_width, height=plot_height, res=800)
  plot_N_scalability_pilot_study_plot_2a_HPC +
    plot_N_scalability_pilot_study_plot_2a_Laptop +
    plot_layout(ncol = 1)
  dev.off()
  
} else if (algorithm %in% c("AD_Stan", "Mplus_standard"))  { 
  
  
  N_scalability_pilot_study_df_HPC_and_Laptop_2 <-  N_scalability_pilot_study_df_HPC_and_Laptop  %>%
    group_by(device, N_num, N_chunks_num) %>%
    mutate(time_perfect_scaling = min(time_avg))  # %>% print(n = 100)
  
  
  
  plot_N_scalability_pilot_study_plot_2a_HPC_and_laptop <-   ggplot(    dplyr::filter(N_scalability_pilot_study_df_HPC_and_Laptop_2 , 
                                                                                      #  device == "HPC",
                                                                                      N_chunks == "1"
                                                                                      
  ), 
  mapping = aes(x = N_chains,
                y =   N_chains / time_avg,
                colour = N_label,
                # linetype = device
  ),
  )    +
    geom_point(size = 4) +
    # geom_line(linewidth = 1,           aes(x = N_chains,
    #                                        y =   N_chains / time_avg,
    #                                        colour = N_label,
    #                                   #     linetype = device
    # )) +
    geom_line(linewidth = 1,   linetype = "dashed",   aes(x = N_chains,
                                                          y =  N_chains / time_perfect_scaling)) +
    geom_line(linewidth = 2,            aes(x = N_chains,
                                            y =  N_chains / time_avg)) +
    theme_bw(base_size = 20) +
    theme(legend.position = "bottom")  +
    ylab("Ratio (N_{chains} / second)") +
    xlab("N_{chains}" ) +
    scale_x_continuous(breaks = c(2, 4, 6, 8, 16, 32, 64, 96)) + 
    facet_wrap( ~ device,
                scales = "free",
    )
  
  plot_N_scalability_pilot_study_plot_2a_HPC_and_laptop
  
  
  
  
  {
    plot_scale_factor <- 1
    plot_width <-  16*plot_scale_factor
    plot_height <- 1*9*plot_scale_factor
  }
  
  
  plot_name <- paste0("plot_N_scalability_pilot_study_", algorithm, "_plot_2a.png")
  png(plot_name ,units = "in", width = plot_width, height=plot_height, res=800)
  plot_N_scalability_pilot_study_plot_2a_HPC_and_laptop
  dev.off()
  
} else if (algorithm %in% c("Mplus_WCP"))  {  
  
  
  N_scalability_pilot_study_df_HPC_and_Laptop_2 <-  N_scalability_pilot_study_df_HPC_and_Laptop  %>%
    group_by(device, N_num, N_chunks_num) %>%
    mutate(time_perfect_scaling = min(time_avg))  # %>% print(n = 100)
  
  
  
  plot_N_scalability_pilot_study_plot_2a_HPC_and_laptop <-   ggplot(    dplyr::filter(N_scalability_pilot_study_df_HPC_and_Laptop_2 , 
                                                                                      # device == "HPC",
                                                                                      N_chunks == "1"
                                                                                      
  ), 
  mapping = aes(x = N_cores,
                y =   N_cores / time_avg,
                colour = N_label,
                # linetype = device
  ),
  )    +
    geom_point(size = 4) +
    # geom_line(linewidth = 1,           aes(x = N_chains,
    #                                        y =   N_chains / time_avg,
    #                                        colour = N_label,
    #                                   #     linetype = device
    # )) +
    geom_line(linewidth = 1,   linetype = "dashed",   aes(x = N_cores,
                                                          y =  N_cores / time_perfect_scaling)) +
    geom_line(linewidth = 2,            aes(x = N_cores,
                                            y =  N_cores / time_avg)) +
    theme_bw(base_size = 20) +
    theme(legend.position = "bottom")  +
    ylab("Ratio (N_{cores} / second)") +
    xlab("N_{cores}" ) +
    scale_x_continuous(breaks = c(2, 4, 6, 8, 16, 32, 64, 96)) + 
    facet_wrap( ~ device,
                scales = "free",
    )
  
  plot_N_scalability_pilot_study_plot_2a_HPC_and_laptop
  
  
  
  
  {
    plot_scale_factor <- 1
    plot_width <-  16*plot_scale_factor
    plot_height <- 1*9*plot_scale_factor
  }
  
  
  plot_name <- paste0("plot_N_scalability_pilot_study_", algorithm, "_plot_2a.png")
  png(plot_name ,units = "in", width = plot_width, height=plot_height, res=800)
  plot_N_scalability_pilot_study_plot_2a_HPC_and_laptop
  dev.off()
  
  
  
}












#  ----------------------------- plot 2 (b) --------------------------
# this plot is tto compare pasrallel scaling IGNORING absolute time 

if (algoorithm == "MD_BayesMVP") {
  
  # N_scalability_pilot_study_df_HPC_and_Laptop_2 <-  N_scalability_pilot_study_df_HPC_and_Laptop  %>%
  #   group_by(device, N_num, N_chunks_num) %>%
  #   mutate(time_perfect_scaling = min(time_avg))  # %>% print(n = 100)
  # 
  # {
  #   
  #   plot_N_scalability_pilot_study_plot_2b_HPC <-   ggplot(    dplyr::filter(N_scalability_pilot_study_df_HPC_and_Laptop_2 , 
  #                                                                           device == "HPC",
  #                                                                           #   device == "Laptop",
  #                                                                           #  N_chunks == "1"
  #   ), 
  #   mapping = aes(x = N_chains,
  #                 y =   (N_chains / time_avg) * time_perfect_scaling,
  #                 colour = N_label,
  #                 linetype = device
  #   ),
  #   ) +
  #     geom_point(size = 5) +
  #     geom_line(linewidth = 1,   linetype = "dashed",   aes(x = N_chains,
  #                                                           y =  (N_chains / time_perfect_scaling) * time_perfect_scaling )) +
  #     geom_line(linewidth = 2,            aes(x = N_chains,
  #                                             y =  time_perfect_scaling * N_chains / time_avg)) +
  #     theme_bw(base_size = 20) +
  #     theme(legend.position = "bottom")  +
  #     ylab("Adj. ratio (N_{chains} / sec) * min[time]") +
  #     xlab("N_{chains}" ) +
  #     scale_x_continuous(breaks = c(8, 16, 32, 64, 96)) + 
  #     facet_wrap( ~ N_chunks_group, 
  #                     scales = "free"
  #     ) 
  #   
  #   plot_N_scalability_pilot_study_plot_2b_HPC
  #   
  #   }
  # 
  # 
  # {
  #   
  #   plot_N_scalability_pilot_study_plot_2b_Laptop <-   ggplot(    dplyr::filter(N_scalability_pilot_study_df_HPC_and_Laptop_2 , 
  #                                                                              #  device == "HPC",
  #                                                                              device == "Laptop",
  #                                                                              #  N_chunks == "1"
  #   ), 
  #                 mapping = aes(x = N_chains,
  #                               y =   (N_chains / time_avg) * time_perfect_scaling,
  #                               colour = N_label,
  #                               linetype = device
  #                 ),
  #   ) +
  #     geom_point(size = 5) +
  #     geom_line(linewidth = 1,   linetype = "dashed",   aes(x = N_chains,
  #                                                           y =  (N_chains / time_perfect_scaling) * time_perfect_scaling )) +
  #     geom_line(linewidth = 2,        colour = "black",     aes(x = N_chains,
  #                                             y =  time_perfect_scaling * N_chains / time_avg)) +
  #     theme_bw(base_size = 20) +
  #     theme(legend.position = "bottom")  +
  #     ylab("Adj. ratio (N_{chains} / sec) * min[time]") +
  #     xlab("N_{chains}" ) +
  #     scale_x_continuous(breaks = c(2, 4, 6, 8)) + 
  #     facet_wrap( ~ N_chunks_group, 
  #                 scales = "free"
  #     ) 
  #   
  #   plot_N_scalability_pilot_study_plot_2b_Laptop
  #   
  # }
  # 
  # plot_N_scalability_pilot_study_plot_2b_HPC + 
  #   plot_N_scalability_pilot_study_plot_2b_Laptop + 
  #   plot_layout(ncol = 1)
  # 
  # 
  # { 
  #   plot_scale_factor <- 4
  #   plot_width <-  4*plot_scale_factor
  #   plot_height <- 3*plot_scale_factor
  #   }
  # 
  # 
  # plot_name <- paste0("plot_N_scalability_pilot_study_", algorithm, "_plot_2b.png")
  # png(plot_name ,units = "in", width = plot_width, height=plot_height, res=800)
  # plot_N_scalability_pilot_study_plot_2b_HPC + 
  #   plot_N_scalability_pilot_study_plot_2b_Laptop + 
  #   plot_layout(ncol = 1)
  # dev.off()
  
  
} else if (algorithm %in% c("AD_Stan", "Mplus_standard"))  { 
  
  
  N_scalability_pilot_study_df_HPC_and_Laptop_2 <-  N_scalability_pilot_study_df_HPC_and_Laptop  %>%
    group_by(device, N_num, N_chunks_num) %>%
    mutate(time_perfect_scaling = min(time_avg),
           Adj_scaling_ratio = time_perfect_scaling * (N_chains / time_avg))   %>%
    filter(!(is.na(time_avg))) %>% 
    print(n = 100) 
  
  
  plot_N_scalability_pilot_study_plot_2b_HPC_and_laptop <-   ggplot(    dplyr::filter(N_scalability_pilot_study_df_HPC_and_Laptop_2 ,  
                                                                                      N_chunks == "1"  ), 
                                                                        mapping = aes(x = N_chains,
                                                                                      y =  Adj_scaling_ratio,
                                                                                      colour = N_label,
                                                                                      # linetype = device
                                                                        )
  ) + 
    geom_point(size = 4) +
    # geom_line(linewidth = 1,           aes(x = N_chains,
    #                                        y =  Adj_scaling_ratio,
    #                                        colour = N_label,
    #                                        linetype = device
    # )) +
    geom_line(linewidth = 1,   linetype = "dashed",  colour = "black",  aes(x = N_chains, 
                                                                            y =  (N_chains / time_perfect_scaling) * time_perfect_scaling )) +
    geom_line(linewidth = 2,            aes(x = N_chains,
                                            y =  time_perfect_scaling * N_chains / time_avg)) +
    theme_bw(base_size = 20) +
    theme(legend.position = "bottom")  +
    ylab("Adj. ratio (N_{chains} / sec) * min[time]") +
    xlab("N_{chains}" ) +
    scale_x_continuous(breaks = c(2, 4, 6, 8, 16, 32, 64, 96)) + 
    facet_wrap( ~ device,
                scales = "free",
    )
  
  plot_N_scalability_pilot_study_plot_2b_HPC_and_laptop
  
  
  
  
  {
    plot_scale_factor <- 1
    plot_width <-  16*plot_scale_factor
    plot_height <- 1*9*plot_scale_factor
  }
  
  
  plot_name <- paste0("plot_N_scalability_pilot_study_", algorithm, "_plot_2b.png")
  png(plot_name ,units = "in", width = plot_width, height=plot_height, res=800)
  plot_N_scalability_pilot_study_plot_2b_HPC_and_laptop
  dev.off()
  
} else if (algorithm %in% c("Mplus_WCP"))  {  
  
  
  N_scalability_pilot_study_df_HPC_and_Laptop_2 <-  N_scalability_pilot_study_df_HPC_and_Laptop  %>%
    group_by(device, N_num, N_chunks_num) %>%
    mutate(time_perfect_scaling = min(time_avg),
           Adj_scaling_ratio = time_perfect_scaling * (N_cores / time_avg))   %>%
    filter(!(is.na(time_avg))) %>% 
    print(n = 100) 
  
  
  plot_N_scalability_pilot_study_plot_2b_HPC_and_laptop <-   ggplot(    dplyr::filter(N_scalability_pilot_study_df_HPC_and_Laptop_2 ,  
                                                                                      N_chunks == "1" ,
                                                                                      #  device == "HPC"
  ), 
  mapping = aes(x = N_cores,
                y =  Adj_scaling_ratio,
                colour = N_label,
                # linetype = device
  )
  ) + 
    geom_point(size = 4) +
    # geom_line(linewidth = 1,           aes(x = N_cores,
    #                                        y =  Adj_scaling_ratio,
    #                                        colour = N_label,
    #                                        linetype = device
    # )) +
    geom_line(linewidth = 1,   linetype = "dashed",  colour = "black",  aes(x = N_cores, 
                                                                            y =  (N_cores / time_perfect_scaling) * time_perfect_scaling )) +
    geom_line(linewidth = 2,            aes(x = N_cores,
                                            y =  time_perfect_scaling * N_cores / time_avg)) +
    theme_bw(base_size = 20) +
    theme(legend.position = "bottom")  +
    ylab("Adj. ratio (N_{cores} / sec) * min[time]") +
    xlab("N_{cores}" ) +
    scale_x_continuous(breaks = c(2, 4, 6, 8, 16, 32, 64, 96)) + 
    facet_wrap( ~ device,
                scales = "free",
    )
  
  plot_N_scalability_pilot_study_plot_2b_HPC_and_laptop
  
  
  
  
  {
    plot_scale_factor <- 1
    plot_width <-  16*plot_scale_factor
    plot_height <- 1*9*plot_scale_factor
  }
  
  
  plot_name <- paste0("plot_N_scalability_pilot_study_", algorithm, "_plot_2b.png")
  png(plot_name ,units = "in", width = plot_width, height=plot_height, res=800)
  plot_N_scalability_pilot_study_plot_2b_HPC_and_laptop
  dev.off()
  
  
}


#  ----------------------------- plot 2 (a) and (b)  on SAME PANEL --------------------------
if (algoorithm == "MD_BayesMVP") {
  
  # { 
  #   plot_scale_factor <- 1
  #   plot_width <-  16*plot_scale_factor
  #   plot_height <- (18+9 - 4)*plot_scale_factor
  # }
  # 
  # 
  # 
  # plot_name <- paste0("plot_N_scalability_pilot_study_", algorithm, "_plot_2.png")
  # png(plot_name ,units = "in", width = plot_width, height=plot_height, res=800)
  # wrap_elements(
  #   plot_N_scalability_pilot_study_plot_2a_HPC +  # theme(legend.position = "none") + 
  #     ylab(" " ) +
  #   plot_N_scalability_pilot_study_plot_2a_Laptop +  ylab(" " )  + 
  #   plot_layout(ncol = 1)
  #   ) +
  #     labs(tag = "Ratio (N_{chains} / sec)") +
  #     theme(
  #       plot.tag = element_text(size = rel(2), angle = 90),
  #       plot.tag.position = "left"
  #     ) + 
  #   wrap_elements(
  #   plot_N_scalability_pilot_study_plot_2b_HPC +  # theme(legend.position = "none") +   
  #     ylab(" " ) + 
  #   plot_N_scalability_pilot_study_plot_2b_Laptop +   ylab(" " )   + 
  #   plot_layout(ncol = 1)
  #   ) +
  #   labs(tag = "Adj. ratio (N_{chains} / sec) * min[time]") +
  #   theme(
  #     plot.tag = element_text(size = rel(2), angle = 90),
  #     plot.tag.position = "left"
  #   ) + 
  # plot_layout(ncol = 1, 
  #             heights = c(1, 0.60))
  # dev.off()
  
} else if (algorithm %in% c("AD_Stan", "Mplus_standard"))  {
  
  {
    plot_scale_factor <- 1
    plot_width <-  16*plot_scale_factor
    plot_height <- (1+9)*plot_scale_factor
  }
  
  
  
  plot_name <- paste0("plot_N_scalability_pilot_study_", algorithm, "_plot_2.png")
  png(plot_name , units = "in", width = plot_width, height=plot_height, res=800)
  plot_N_scalability_pilot_study_plot_2a_HPC_and_laptop + theme(legend.position = "none") + 
    plot_N_scalability_pilot_study_plot_2b_HPC_and_laptop + 
    plot_layout(ncol = 1)
  dev.off()
  
} else if (algorithm %in% c("Mplus_WCP"))  {  
  
  {
    plot_scale_factor <- 4
    plot_width <-  4*plot_scale_factor
    plot_height <- (3)*plot_scale_factor
  }
  
  
  
  plot_name <- paste0("plot_N_scalability_pilot_study_", algorithm, "_plot_2.png")
  png(plot_name , units = "in", width = plot_width, height=plot_height, res=800)
  plot_N_scalability_pilot_study_plot_2a_HPC_and_laptop + theme(legend.position = "none") + 
    plot_N_scalability_pilot_study_plot_2b_HPC_and_laptop + 
    plot_layout(ncol = 1)
  dev.off()
  
  
  
}





# Plot(s) comparing Mplus_WCP and Mplus_standard 




df_scalability_Mplus_standard   <- readRDS("N_scalability_pilot_study_df_HPC_and_Laptop_Mplus_standard")  %>% mutate(Algorithm = "Mplus_standard")  %>% filter(!(is.na(time_avg))) %>% print(n = 1000)
df_scalability_Mplus_WCP        <- readRDS("N_scalability_pilot_study_df_HPC_and_Laptop_Mplus_WCP")  %>% mutate(Algorithm = "Mplus_WCP") %>% filter(!(is.na(time_avg))) %>% print(n = 1000)

### df_scalability_Mplus_WCP <- df_scalability_Mplus_WCP %>% mutate(N_chains = 4)  %>% print(n = 1000)

df_scalability_Mplus_standard_and_WCP <- rbind(df_scalability_Mplus_standard, df_scalability_Mplus_WCP)

df_scalability_Mplus_standard_and_WCP$Algorithm <- factor(  df_scalability_Mplus_standard_and_WCP$Algorithm )

df_scalability_Mplus_standard_and_WCP_2   <-  df_scalability_Mplus_standard_and_WCP  %>%
  group_by(device, Algorithm, N_num, N_chunks_num) %>%
  mutate(time_perfect_scaling = min(time_avg),
         Adj_scaling_ratio = time_perfect_scaling * (N_cores / time_avg))   %>%
  filter(!(is.na(time_avg))) %>% 
  mutate(time_avg_adj_for_Mplus_WCP_vs_standard =     ifelse(Algorithm == "Mplus_WCP", time_avg * (N_cores / N_chains), time_avg)) %>% 
  mutate(time_perfect_adj_for_Mplus_WCP_vs_standard = ifelse(Algorithm == "Mplus_WCP",  (N_cores / N_chains) , time_avg)) %>% 
  print(n = 100) %>% 
  filter(N_chains != 2)





subset_Mplus_standard <-  filter(df_scalability_Mplus_standard_and_WCP_2, N_chunks == 1, N == 12500, device == "Laptop", Algorithm == "Mplus_standard")

subset_Mplus_WCP <-  filter(df_scalability_Mplus_standard_and_WCP_2, N_chunks == 1, N == 12500, device == "Laptop" , Algorithm == "Mplus_WCP")


plot_N_scalability_plot_HPC_and_laptop_Mplus_WCP_vs_standard_plot_1 <-   ggplot(    dplyr::filter(df_scalability_Mplus_standard_and_WCP_2 ,  
                                                                                                  #   N_chunks == "1" ,
                                                                                                  #   device == "HPC"
), 
mapping = aes(x = N_cores,
              # y =  Adj_scaling_ratio,
              y =  time_avg_adj_for_Mplus_WCP_vs_standard,
              colour = N_label,
              #  linetype = Algorithm
)
) + 
  geom_point(size = 4) +
  # geom_line(linewidth = 1,           aes(x = N_cores,
  #                                        y =  Adj_scaling_ratio,
  #                                        colour = N_label,
  #                                        linetype = device
  # )) +
  geom_line(linewidth = 1,   linetype = "dashed",  colour = "black",  aes(x = N_cores,
                                                                          #   y =  (N_cores / time_perfect_scaling) * time_perfect_scaling )) +
                                                                          y =   time_perfect_adj_for_Mplus_WCP_vs_standard)) +
  geom_line(linewidth = 2,          aes(x = N_cores,
                                        #  y =  time_perfect_scaling * N_cores / time_avg)) +
                                        y =   time_avg_adj_for_Mplus_WCP_vs_standard)) + 
  theme_bw(base_size = 20) +
  theme(legend.position = "bottom")  +
  ylab("Adj. ratio (N_{cores} / sec) * min[time]") +
  xlab("N_{cores}" ) +
  scale_x_continuous(breaks = c(2, 4, 6, 8, 16, 32, 64, 96)) + 
  facet_wrap( ~ device + Algorithm + N,
              scales = "free",
  )

plot_N_scalability_plot_HPC_and_laptop_Mplus_WCP_vs_standard_plot_1




{
  plot_scale_factor <- 1
  plot_width <-  16*plot_scale_factor
  plot_height <- 1*9*plot_scale_factor
}


plot_name <- paste0("plot_N_scalability_plot_HPC_and_laptop_Mplus_WCP_vs_standard_plot_1")
png(plot_name ,units = "in", width = plot_width, height=plot_height, res=800)
plot_N_scalability_plot_HPC_and_laptop_Mplus_WCP_vs_standard_plot_1
dev.off()




























# stats for table showing best N_{chunks} for each N - for both HPC and laptop



N_scalability_pilot_study_df_best_HPC <- N_scalability_pilot_study_df_HPC_and_Laptop %>%
  group_by(device, N)  %>%
  mutate(best_ind = ifelse(time_avg == min(time_avg), 1, 0)) %>% 
  dplyr::filter(best_ind == 1, device == "HPC") %>% 
  print(n = 1000)



N_scalability_pilot_study_df_best_Laptop <- N_scalability_pilot_study_df_HPC_and_Laptop %>%
  group_by(device, N)  %>%
  mutate(best_ind = ifelse(time_avg == min(time_avg), 1, 0)) %>% 
  dplyr::filter(best_ind == 1, device == "Laptop") %>% 
  print(n = 1000)


















# End of  "Pilot study - Parallel scalability for: (1) BayesMVP (for various chunks); (2) Mplus ; and (3) Stan (using C++ AD function)"     ------------------------------------------------------------------------------------------------------------













































num_chunks <- 10 ; num_chunks
chunk_size <- round(N / num_chunks) ; chunk_size

chunk_counter = 0

for (nc in 0:(num_chunks - 1)) {
  
  for (t in 0:(n_tests - 1)) {
    
    if (t == 0) {  
      if (chunk_counter == 0) {
        print(paste("chunk_counter = 0, t = 0:",  
                    "theta from:", 0,   "length",n_tests * chunk_size, 
                    "then given this, from" , 0, "length", chunk_size))
      } else       {
        print(paste("chunk_counter > 0, t = 0:",  
                    "theta from:", (n_tests * chunk_size * chunk_counter) - 1,   "length",n_tests * chunk_size, 
                    "then given this, from" , 0, "length", chunk_size))
      }
    } else { 
      if (chunk_counter == 0) {
        print(paste("chunk_counter = 0, t > 0:",  
                    "theta from:", 0,   "length",n_tests * chunk_size, 
                    "then given this, from" , (t * chunk_size) - 1, "length", chunk_size))
      } else       {
        print(paste("chunk_counter > 0, t > 0:",  
                    "theta from:", (n_tests * chunk_size * chunk_counter) - 1,   "length",n_tests * chunk_size, 
                    "then given this, from" , (t * chunk_size) - 1, "length", chunk_size))
      }
    }
    
    
  }
  
  chunk_counter = chunk_counter + 1
  
}

plot(cores, cores / times)
points(cores, cores / times, co = "blue" )                         
# ----------  grad - both u and main (using STAN AD grad_fn)
par(mfrow = c(2, 2))


# use "dput" 
dput(times) 



times_N_64000_ad <-  c(43.049, 46.076, 53.425, 74.123, 106.226, 145.479, 180.485)         #  Zen 4, 12 DIMM's
times_N_64000_ideal_ad <- rep(times_N_64000_ad[1],  length(cores))


times_N_16000_ad <-  c(38.694, 41.037, 50.804, 67.304, 92.734, 134.817, 166.817)       #  Zen 4, 12 DIMM's
times_N_16000_ideal_ad <- rep(times_N_16000_ad[1],  length(cores))


times_N_4000_ad <-  c(36.795, 39.98, 47.705, 66.24, 89.351, 131.202, 164.311)        #  Zen 4, 12 DIMM's
times_N_4000_ideal_ad <- rep(times_N_4000_ad[1],  length(cores))


times_N_1000_ad <-  c(33.986, 38.708, 40.123, 49.594, 75.616, 119.415, 152.732)          #  Zen 4, 12 DIMM's
times_N_1000_ideal_ad <- rep(times_N_1000_ad[1],  length(cores))



div <- cores/4


# plot(x = cores, y = 1/(times_N_1000_ideal/div), col = "green",  main = "N = 1000, AD", type = "l")
# points(x = cores, y = 1/(times_N_1000_ad/div))
# lines(x = cores, y = 1/(times_N_1000_ad/div))




# ----------  grad - both u and main (using MANUAL grad_fn) - FULLY VECTORISED  (i.e. chunking with 1 chunk)
# use "d" 
dput(times)




times_N_64000_vec <-  c(9.708, 10.612, 12.048, 14.938, 21.392, 35.894, 50.145)       #  Zen 4, 12 DIMM's
times_N_64000_ideal_vec <- rep(times_N_64000_vec[1],  length(cores))


times_N_16000_vec <-   c(8.63, 9.618, 10, 11.818, 15.797, 20.467, 25.381)        #  Zen 4, 12 DIMM's ,  -g -O3  -march=znver3   -mtune=native   -fPIC  $(LTO) #  
times_N_16000_ideal_vec <- rep(times_N_16000_vec[1],  length(cores))


times_N_4000_vec <-   c(8.305, 8.52, 8.419, 9.268, 13.454, 16.693, 19.745)        #  Zen 4, 12 DIMM's
times_N_4000_ideal_vec <- rep(times_N_4000_vec[1],  length(cores))


times_N_1000_vec <-  c(7.964, 8.371, 8.798, 8.924, 12.074, 14.559, 16.515)        #  Zen 4, 12 DIMM's
times_N_1000_ideal_vec <- rep(times_N_1000_vec[1],  length(cores))

div <- cores/4


# ----------  grad - both u and main (using MANUAL grad_fn) - VECTORISED + CHUNKING 
# use "d" 
dput(times)

#  dput_temp_1 <-  dput(times)
#  dput_temp_2 <-  dput(times)



times_N_64000_vec_chunk <-   c(8.213, 8.703, 9.428, 10.921, 14.241, 18.426, 20.853)       #  Zen 4, 12 DIMM's
times_N_64000_ideal_vec_chunk <- rep(times_N_64000_vec_chunk[1],  length(cores))


times_N_16000_vec_chunk <-   c(7.672,   8.1, 8.615, 9.414, 12.379, 15.261, 17.252)         #  Zen 4, 12 DIMM's,   -g -O3  -march=znver3   -mtune=native   -fPIC  $(LTO) # 
times_N_16000_ideal_vec_chunk <- rep(times_N_16000_vec_chunk[1],  length(cores))

times_N_4000_vec_chunk <-    c(7.987, 8.198, 8.988, 8.626, 12.075, 14.385, 16.281)        #  Zen 4, 12 DIMM's
times_N_4000_ideal_vec_chunk <- rep(times_N_4000_vec_chunk[1],  length(cores))

times_N_1000_vec_chunk <-   c(7.964, 8.371, 8.798, 8.924, 12.074, 14.559, 16.515)          #  Zen 4, 12 DIMM's
times_N_1000_ideal_vec_chunk <- rep(times_N_1000_vec_chunk[1],  length(cores))





# ----------  grad - both u and main (using MANUAL grad_fn) - NON-VECTORISED
# use "d"
dput(times)


times_N_64000_nonvec <-  c(25.124, 27.738, 27.881, 29.218, 37.97, 46.958, 56.494)       #  Zen 4, 12 DIMM's
times_N_64000_ideal_nonvec <- rep(times_N_64000_nonvec[1],  length(cores))


times_N_16000_nonvec <-  c(22.364, 23.353, 23.953, 24.156, 32.556, 37.797, 43.012)        #  Zen 4, 12 DIMM's,   -g -O3  -march=znver3   -mtune=native   -fPIC  $(LTO) #
times_N_16000_ideal_nonvec <- rep(times_N_16000_nonvec[1],  length(cores))

times_N_4000_nonvec <-   c(22.067, 22.684, 22.532, 23.551, 31.319, 35.992, 39.799)      #  Zen 4, 12 DIMM's
times_N_4000_ideal_nonvec <- rep(times_N_4000_nonvec[1],  length(cores))

times_N_1000_nonvec <-   c(21.964, 22.415, 22.866, 23.033, 30.497, 35.669, 38.824)          #  Zen 4, 12 DIMM's
times_N_1000_ideal_nonvec <- rep(times_N_1000_nonvec[1],  length(cores))
# 





#  --------- make general df 
n_dfs <- 4
n_functions <- 4
df_selecting_n_chains <- tibble(n_cores  = rep(rep(cores, n_functions), n_dfs), 
                                function_type = rep( c( rep("AD-NON-VEC", length(cores)) ,        rep("MD-VEC", length(cores)) ,   rep("MD-VEC-CHUNK", length(cores))  ,  rep("MD-NON-VEC", length(cores)) ), n_dfs), 
                                df_size = (c( rep(1000, n_functions * length(cores)), rep(4000, n_functions * length(cores)), rep(16000, n_functions * length(cores)), rep(64000, n_functions * length(cores)) )), 
                                times = c(  c(times_N_1000_ad, times_N_1000_vec, times_N_1000_vec_chunk, times_N_1000_nonvec) , 
                                            c(times_N_4000_ad, times_N_4000_vec, times_N_4000_vec_chunk, times_N_4000_nonvec) ,
                                            c(times_N_16000_ad, times_N_16000_vec, times_N_16000_vec_chunk, times_N_16000_nonvec) ,
                                            c(times_N_64000_ad, times_N_64000_vec, times_N_64000_vec_chunk, times_N_64000_nonvec)  ) , 
                                ideal_times = c(  c(times_N_1000_ideal_ad, times_N_1000_ideal_vec,   times_N_1000_ideal_vec_chunk, times_N_1000_ideal_nonvec) , 
                                                  c(times_N_4000_ideal_ad, times_N_4000_ideal_vec,   times_N_4000_ideal_vec_chunk, times_N_4000_ideal_nonvec) ,
                                                  c(times_N_16000_ideal_ad, times_N_16000_ideal_vec, times_N_16000_ideal_vec_chunk, times_N_16000_ideal_nonvec) ,
                                                  c(times_N_64000_ideal_ad, times_N_64000_ideal_vec, times_N_64000_ideal_vec_chunk, times_N_64000_ideal_nonvec)  ) )

# #  --------- make general df ( just VEC vs NON-VEC for MD )
# n_dfs <- 6
# n_functions <-  2
# df_selecting_n_chains <- tibble(n_cores  = rep(rep(cores, n_functions), n_dfs), 
#                                 function_type =     rep( c(   rep("MD-VEC", length(cores)) ,  rep("MD-NON-VEC", length(cores)) ), n_dfs), 
#                                 df_size = (c( rep(1000, n_functions * length(cores)), rep(4000, n_functions * length(cores)), rep(16000, n_functions * length(cores)), rep(64000, n_functions * length(cores)),  rep(128000, n_functions * length(cores)),  rep(256000, n_functions * length(cores))  )), 
#                                 times = c(  c( times_N_1000_vec, times_N_1000_nonvec) , 
#                                             c( times_N_4000_vec, times_N_4000_nonvec) ,
#                                             c( times_N_16000_vec, times_N_16000_nonvec) ,
#                                             c( times_N_64000_vec, times_N_64000_nonvec)  , 
#                                             c( times_N_128000_vec, times_N_128000_nonvec)  , 
#                                             c( times_N_256000_vec, times_N_256000_nonvec)  ) )



df_selecting_n_chains <- df_selecting_n_chains %>% mutate(div = n_cores/4,
                                                          # eff = 1/(times/div)
                                                          eff = n_cores / times,
                                                          scaling_ratio = n_cores /  ( times/ ideal_times) )


require(ggplot2)

#  df_selecting_n_chains <- filter(df_selecting_n_chains, function_type != "AD-NON-VEC")

# ---------  plot trends 

ggplot(data = df_selecting_n_chains, aes(x = n_cores, y = times, colour = function_type))  + 
  geom_line() + 
  geom_point() + 
  facet_wrap(~ df_size, scales = "free")  + 
  # facet_wrap(~ df_size)  +
  theme_bw() + 
  ylab("Time (adjusted for N)") + 
  xlab("Number of parallel cores (total = 64 cores / 128 threads)")





df_selecting_n_chains_2 <- dplyr::filter(df_selecting_n_chains, function_type != "AD-NON-VEC")


#  tiff("Figure_1_MD_vec_nonvec_chunk_efficiency.tif" ,units = "in", width = 10, height=8, res=800, compression = "lzw")
png("Figure_1_MD_vec_nonvec_chunk_efficiency.png" ,units = "in", width = 10, height=8, res=800)

ggplot(data = df_selecting_n_chains_2, aes(x = n_cores, y = eff, colour = function_type))  + 
  geom_line(size = 1.0) + 
  geom_point(size = 3.5) + 
  facet_wrap(~ df_size, scales = "free")  + 
  #facet_wrap(~ df_size)  +
  theme_bw() + 
  ylab("Efficiency ( #threads / time - adjusted for N)") + 
  xlab("Number of parallel cores (total = 64 cores / 128 threads)") + 
  theme_bw(base_size = 20) + 
  guides(color=guide_legend(title="Function type")) 


dev.off()


df_selecting_n_chains_3 <- dplyr::filter(df_selecting_n_chains, function_type != "MD-VEC", function_type != "MD-NON-VEC")


#  tiff("Figure_1_MD_vec_nonvec_chunk_efficiency.tif" ,units = "in", width = 10, height=8, res=800, compression = "lzw")
png("Figure_1_AD_vs_MD_chunk_efficiency_scaling_ratio.png" ,units = "in", width = 10, height=8, res=800)

g_1 <- ggplot(data = df_selecting_n_chains_3, aes(x = n_cores, y = scaling_ratio, colour = function_type))  + 
  geom_line(size = 1.0) + 
  geom_point(size = 3.5) + 
  facet_wrap(~ df_size, scales = "free")  + 
  #facet_wrap(~ df_size)  +
  theme_bw() + 
  #  ylab("Efficiency ( #threads / time - adjusted for N)") + 
  ylab(" Scaling ratio (higher is better) ") + 
  xlab("Number of parallel cores (total = 64 cores / 128 threads)") + 
  theme_bw(base_size = 20) + 
  guides(color=guide_legend(title="Function type")) 
g_1

dev.off()

png("Figure_1_AD_vs_MD_chunk_efficiency.png" ,units = "in", width = 10, height=8, res=800)

g_2 <- ggplot(data = df_selecting_n_chains_3, aes(x = n_cores, y = eff, colour = function_type))  + 
  geom_line(size = 1.0) + 
  geom_point(size = 3.5) + 
  facet_wrap(~ df_size, scales = "free")  + 
  #facet_wrap(~ df_size)  +
  theme_bw() + 
  ylab("Efficiency ( #threads / time - adjusted for N)") + 
  # ylab(" Scaling ratio (higher is better) ") + 
  xlab("Number of parallel cores (total = 64 cores / 128 threads)") + 
  theme_bw(base_size = 20) + 
  guides(color=guide_legend(title="Function type")) 
g_2

dev.off()


require(patchwork)

png("Figure_1_AD_vs_MD_chunk_efficiency_efficiency_and_scaling.png" ,units = "in", width = 18, height=8, res=800)

g_1 +   theme(legend.position="none") + g_2

dev.off()

# # # ----------   lp-only - using VECTORISED grad_fn - w/ only the containers needed to calc. lp (not grad containers)  
# 
# # use "d" 
# dput(times)
# 
# 
# # times_N_1024000 <-  c(                                 )   ; times_N_1024000[5]  / times_N_1024000[1]
# # times_N_1024000_ideal <- rep(times_N_1024000[1],  8)
# 
# # times_N_256000 <-  c(                             )   ; times_N_256000[5]  / times_N_256000[1]
# # times_N_256000_ideal <- rep(times_N_256000[1],  8)
# 
# 
# times_N_64000 <- c(      3.508, 3.595, 4.054, 4.714, 5.425, 6.369, 7.302, 8.562             )   ; times_N_64000[5]  / times_N_64000[1]      #  Zen 4, 12 DIMM's
# times_N_64000_ideal <- rep(times_N_64000[1],  8)
# 
# 
# times_N_16000 <- c(     2.216, 2.258, 2.48, 2.636, 2.928, 3.238, 3.833, 4.607           )   ; times_N_16000[5]  / times_N_16000[1]      #  Zen 4, 12 DIMM's
# times_N_16000_ideal <- rep(times_N_16000[1],  8)
# 
# 
# times_N_4000 <- c(      1.929, 1.958, 2.223, 2.355, 2.766, 2.616, 3.5, 3.618          )   ; times_N_4000[5]  / times_N_4000[1]      #  Zen 4, 12 DIMM's
# times_N_4000_ideal <- rep(times_N_4000[1],  8)
# 
# 
# times_N_1000 <- c(         1.729, 1.75, 2.173, 2.27, 2.045, 2.223, 2.431, 2.395          )   ; times_N_1000[5]  / times_N_1000[1]      #  Zen 4, 12 DIMM's
# times_N_1000_ideal <- rep(times_N_1000[1],  8)
# 
# cores <- c(4, 8, 16, 24, 32, 40, 48, 56)
# div <- cores/4
# 
# 
# 
# plot(x = cores, y = 1/(times_N_1000_ideal/div), col = "green",  main = "N = 1000, MD, lp-only, vectorised", type = "l")
# points(x = cores, y = 1/(times_N_1000/div))
# lines(x = cores, y = 1/(times_N_1000/div))
# 
# plot(x = cores, y = 1/(times_N_4000_ideal/div), col = "green",  main = "N = 4000, MD, lp-only, vectorised", type = "l")
# points(x = cores, y = 1/(times_N_4000/div))
# lines(x = cores, y = 1/(times_N_4000/div))
# 
# 
# plot(x = cores, y = 1/(times_N_16000_ideal/div), col = "green",  main = "N = 16,000, MD, lp-only, vectorised", type = "l")
# points(x = cores, y = 1/(times_N_16000/div))
# lines(x = cores, y = 1/(times_N_16000/div))
# 
# plot(x = cores, y = 1/(times_N_64000_ideal/div), col = "green",  main = "N = 64,000, MD, lp-only, vectorised", type = "l")
# points(x = cores, y = 1/(times_N_64000/div))
# lines(x = cores, y = 1/(times_N_64000/div))
# 
# # 
# # plot(x = cores, y = 1/(times_N_256000_ideal/div), col = "green",  main = "N = 256,000, MD, lp-only, vectorised", type = "l")
# # points(x = cores, y = 1/(times_N_256000/div))
# # lines(x = cores, y = 1/(times_N_256000/div))
# # 
# # plot(x = cores, y = 1/(times_N_1024000_ideal/div), col = "green",  main = "N = 1 million, MD, lp-only, vectorised", type = "l")
# # points(x = cores, y = 1/(times_N_1024000/div))
# # lines(x = cores, y = 1/(times_N_1024000/div))
# 
# 
# 
# # # ----------   lp-only - using NON-VECTORISED grad_fn - w/ only the containers needed to calc. lp (not grad containers)  
# 
# # use "d" 
# dput(times)
# 
# 
# # times_N_1024000 <-  c(                                 )   ; times_N_1024000[5]  / times_N_1024000[1]
# # times_N_1024000_ideal <- rep(times_N_1024000[1],  8)
# #
# # times_N_256000 <-  c(                            )   ; times_N_256000[5]  / times_N_256000[1]
# # times_N_256000_ideal <- rep(times_N_256000[1],  8)
# 
# 
# times_N_64000 <- c(        2.403, 2.489, 2.894, 3.055, 3.335, 3.74, 4.137, 4.736               )   ; times_N_64000[5]  / times_N_64000[1]      #  Zen 4, 12 DIMM's
# times_N_64000_ideal <- rep(times_N_64000[1],  8)
# 
# 
# times_N_16000 <- c(      1.931, 1.976, 2.27, 2.42, 2.448, 2.591, 3.05, 3.129              )   ; times_N_16000[5]  / times_N_16000[1]      #  Zen 4, 12 DIMM's
# times_N_16000_ideal <- rep(times_N_16000[1],  8)
# 
# 
# times_N_4000 <- c(        1.842, 1.896, 2.183, 2.296, 2.031, 2.363, 2.748, 2.835                 )   ; times_N_4000[5]  / times_N_4000[1]      #  Zen 4, 12 DIMM's
# times_N_4000_ideal <- rep(times_N_4000[1],  8)
# 
# 
# times_N_1000 <- c(         1.87, 1.967, 2.135, 2.294, 2.493, 2.479, 2.624, 2.725                   )   ; times_N_1000[5]  / times_N_1000[1]      #  Zen 4, 12 DIMM's
# times_N_1000_ideal <- rep(times_N_1000[1],  8)
# 
# cores <- c(4, 8, 16, 24, 32, 40, 48, 56)
# div <- cores/4
# 
# 
# 
# plot(x = cores, y = 1/(times_N_1000_ideal/div), col = "green",  main = "N = 1000, MD, lp-only, NON-vectorised", type = "l")
# points(x = cores, y = 1/(times_N_1000/div))
# lines(x = cores, y = 1/(times_N_1000/div))
# 
# plot(x = cores, y = 1/(times_N_4000_ideal/div), col = "green",  main = "N = 4000, MD, lp-only, NON-vectorised", type = "l")
# points(x = cores, y = 1/(times_N_4000/div))
# lines(x = cores, y = 1/(times_N_4000/div))
# 
# 
# plot(x = cores, y = 1/(times_N_16000_ideal/div), col = "green",  main = "N = 16,000, MD, lp-only, NON-vectorised", type = "l")
# points(x = cores, y = 1/(times_N_16000/div))
# lines(x = cores, y = 1/(times_N_16000/div))
# 
# plot(x = cores, y = 1/(times_N_64000_ideal/div), col = "green",  main = "N = 64,000, MD, lp-only, NON-vectorised", type = "l")
# points(x = cores, y = 1/(times_N_64000/div))
# lines(x = cores, y = 1/(times_N_64000/div))
# 
# # 
# # plot(x = cores, y = 1/(times_N_256000_ideal/div), col = "green",  main = "N = 256,000, MD, lp-only, NON-vectorised", type = "l")
# # points(x = cores, y = 1/(times_N_256000/div))
# # lines(x = cores, y = 1/(times_N_256000/div))
# # 
# # plot(x = cores, y = 1/(times_N_1024000_ideal/div), col = "green",  main = "N = 1 million, MD, lp-only, vectorised", type = "l")
# # points(x = cores, y = 1/(times_N_1024000/div))
# # lines(x = cores, y = 1/(times_N_1024000/div))
# 



par(mfrow = c(2,2))

times_N_1000 <- c(         1.729, 1.75, 2.173, 2.27, 2.045, 2.223, 2.431, 2.395          )   ; times_N_1000[5]  / times_N_1000[8]      #  Zen 4, 12 DIMM's
times_N_1000_ideal <- rep(times_N_1000[1],  8)
plot(x = cores, y = 1/(times_N_1000_ideal/div), col = "green",  main = "N = 1000, MD, lp-only, NON-vectorised = BLUE", type = "l")
points(x = cores, y = 1/(times_N_1000/div), col = "green" )
lines(x = cores, y = 1/(times_N_1000/div), col = "green")

times_N_1000 <- c(         1.87, 1.967, 2.135, 2.294, 2.493, 2.479, 2.624, 2.725                   )   ; times_N_1000[5]  / times_N_1000[8]      #  Zen 4, 12 DIMM's - NON-VEC
times_N_1000_ideal <- rep(times_N_1000[1],  8)
lines(x = cores, y = 1/(times_N_1000_ideal/div), col = "blue",  main = "N = 1000, MD, lp-only, vectorised = BLUE ", type = "l")
points(x = cores, y = 1/(times_N_1000/div) ,  col = "blue" )
lines(x = cores, y = 1/(times_N_1000/div),  col = "blue")




times_N_4000 <- c(      1.929, 1.958, 2.223, 2.355, 2.766, 2.616, 3.5, 3.618          )   ; times_N_4000[8]  / times_N_4000[1]      #  Zen 4, 12 DIMM's
times_N_4000_ideal <- rep(times_N_4000[1],  8)
plot(x = cores, y = 1/(times_N_4000_ideal/div), col = "green",  main = "N = 4000, MD, lp-only, NON-vectorised = BLUE ", type = "l")
points(x = cores, y = 1/(times_N_4000/div), col = "green")
lines(x = cores, y = 1/(times_N_4000/div), col = "green")

times_N_4000 <- c(        1.842, 1.896, 2.183, 2.296, 2.031, 2.363, 2.748, 2.835                 )   ; times_N_4000[8]  / times_N_4000[1]      #  Zen 4, 12 DIMM's - NON-VEC
times_N_4000_ideal <- rep(times_N_4000[1],  8)
lines(x = cores, y = 1/(times_N_4000_ideal/div), col = "blue",  main = "N = 4000, MD, lp-only, vectorised", type = "l")
points(x = cores, y = 1/(times_N_4000/div) ,  col = "blue"  )
lines(x = cores, y = 1/(times_N_4000/div) ,  col = "blue" )




times_N_16000 <- c(     2.216, 2.258, 2.48, 2.636, 2.928, 3.238, 3.833, 4.607           )   ; times_N_16000[8]  / times_N_16000[1]      #  Zen 4, 12 DIMM's
times_N_16000_ideal <- rep(times_N_16000[1],  8)
plot(x = cores, y = 1/(times_N_16000_ideal/div), col = "green",  main = "N = 16,000, MD, lp-only, NON-vectorised = BLUE ", type = "l")
points(x = cores, y = 1/(times_N_16000/div), col = "green")
lines(x = cores, y = 1/(times_N_16000/div), col = "green")

times_N_16000 <- c(      1.931, 1.976, 2.27, 2.42, 2.448, 2.591, 3.05, 3.129              )   ; times_N_16000[8]  / times_N_16000[1]      #  Zen 4, 12 DIMM's - NON-VEC
times_N_16000_ideal <- rep(times_N_16000[1],  8)
lines(x = cores, y = 1/(times_N_16000_ideal/div), col = "blue",  main = "N = 16,000, MD, lp-only, vectorised = BLUE ", type = "l")
points(x = cores, y = 1/(times_N_16000/div) ,  col = "blue" )
lines(x = cores, y = 1/(times_N_16000/div) , col = "blue" )





times_N_64000 <- c(      3.508, 3.595, 4.054, 4.714, 5.425, 6.369, 7.302, 8.562             )   ; times_N_64000[8]  / times_N_64000[1]      #  Zen 4, 12 DIMM's
times_N_64000_ideal <- rep(times_N_64000[1],  8)
plot(x = cores, y = 1/(times_N_64000_ideal/div), col = "green",  main = "N = 64,000, MD, lp-only, NON-vectorised = BLUE ", type = "l")
points(x = cores, y = 1/(times_N_64000/div), col = "green")
lines(x = cores, y = 1/(times_N_64000/div), col = "green")

times_N_64000 <- c(        2.403, 2.489, 2.894, 3.055, 3.335, 3.74, 4.137, 4.736               )   ; times_N_64000[8]  / times_N_64000[1]      #  Zen 4, 12 DIMM's   - NON-VEC
times_N_64000_ideal <- rep(times_N_64000[1],  8)
lines(x = cores, y = 1/(times_N_64000_ideal/div), col = "blue",  main = "N = 64,000, MD, lp-only, vectorised", type = "l")
points(x = cores, y = 1/(times_N_64000/div) ,  col = "blue" )
lines(x = cores, y = 1/(times_N_64000/div), col = "blue")




{
  
  fn_version = 1
  
  tic()
  
  
  outs_manual_grad <-   lcmMVPbetav3::fn_log_posterior_full_binary_only_GHK_manual_diff_loop_test(theta =  theta_vec,
                                                                                                  y = y,
                                                                                                  lp_and_grad_args,
                                                                                                  version = fn_version,
                                                                                                  n_iter = 50)
  
  
  toc()
  
}

#      lcmMVPbetav3::




{
  
  
  tic()
  
  
  fn_log_posterior_full_binary_only_GHK_manual_diff_AF_loop_test(theta =  theta_vec,
                                                                 y = y,
                                                                 lp_and_grad_args, 
                                                                 n_iter = 10)
  
  
  
  # for (i in 1:500)
  #   # lcmMVPbetav3::fn_log_posterior_full_binary_only_GHK_manual_diff_stan
  #   # fn_log_posterior_full_binary_only_GHK_manual_diff_AF
  #   #  fn_log_posterior_full_binary_only_GHK_manual_diff_stan
  #   outs_manual_grad <-     fn_log_posterior_full_binary_only_GHK_manual_diff_AF(theta =  theta_vec,
  #                                                                               exclude_priors = FALSE,
  #                                                                               lkj_prior_method = 2,
  #                                                                               #  exclude_priors =FALSE,
  #                                                                               # grad_main = FALSE,
  #                                                                               grad_main = TRUE,
  #                                                                               grad_nuisance = TRUE,
  #                                                                               #grad_nuisance = FALSE,
  #                                                                               CI = CI,
  #                                                                               homog_corr = homog_corr,
  #                                                                               rough_approx = rough_approx,
  #                                                                               lkj_cholesky_eta = prior_lkj,
  #                                                                               lkj_cholesky = mvr_cholesky,
  #                                                                               prior_coeffs_mean = prior_mean_vec,
  #                                                                               prior_coeffs_sd = prior_sd_vec,
  #                                                                               #    class_ind = class_ind - 1,
  #                                                                               n_class = n_class,
  #                                                                               n_tests = n_tests,
  #                                                                                   X = X,
  #                                                                               y = y)
  
  toc()
  
}



tail(outs_manual_grad[,1], 50)
outs_manual_grad[1,2]


u_array = outs_manual_grad[[1]]   ;   u_array
prob_1 = outs_manual_grad[[2]]   ;   prob_1
Z_std_norm = outs_manual_grad[[3]]   ;   Z_std_norm
Phi_Z =outs_manual_grad[[4]]   ;   Phi_Z
Bound_Z =outs_manual_grad[[5]]   ;   Bound_Z
Bound_U_Bound_Phi_Z_1 =outs_manual_grad[[6]]   ;   Bound_U_Bound_Phi_Z_1
prob_n_1 =outs_manual_grad[[7]]   ;   prob_n_1
log_posterior_1 =outs_manual_grad[[8]]   ;   log_posterior_1 ; sum(log_posterior_1)

inc_array =outs_manual_grad[[10]]   ;   inc_array
y1_array =outs_manual_grad[[11]]   ;   y1_array
lp_array =outs_manual_grad[[9]]   ;   lp_array
af_prev =outs_manual_grad[[12]]   ;   af_prev
log_prev =outs_manual_grad[[13]]    ;   log_prev
y_sign =  outs_manual_grad[[14]]    ;   y_sign
u_grad_1 = outs_manual_grad[[15]]    ;   u_grad_1
beta_grad_1 = outs_manual_grad[[16]]    ;   beta_grad_1

grad_z_mat_1 =    outs_manual_grad[[17]]    ;   grad_z_mat_1
grad_term_mat_1 =    outs_manual_grad[[18]]    ;   grad_term_mat_1
derivs_chain_container_vec_array_1  =    outs_manual_grad[[19]]    ;   derivs_chain_container_vec_array_1
common_grad_term_1_1 =    outs_manual_grad[[20]]    ;   common_grad_term_1_1

{
  n_params <- N * n_tests * n_class + 43
  theta_vec <- rnorm(n = n_params, mean = 0, sd = 0.05)
  homog_corr <- F
  rough_approx <- T
  prior_lkj <- 6
  mvr_cholesky <- T
  n_us <- - N * n_tests * n_class
}






{
  
  
  tic()
  for (i in 1:50)  
    # lcmMVPbetav3::fn_log_posterior_full_binary_only_GHK_manual_diff_stan
    # fn_log_posterior_full_binary_only_GHK_manual_diff_AF
    #  fn_log_posterior_full_binary_only_GHK_manual_diff_stan
    outs_manual_grad_AF <-     fn_log_posterior_full_binary_only_GHK_manual_diff_AF(theta =  theta_vec,
                                                                                    exclude_priors = FALSE,
                                                                                    lkj_prior_method = 2,
                                                                                    grad_main = TRUE,
                                                                                    grad_nuisance = TRUE,
                                                                                    CI = CI,
                                                                                    homog_corr = homog_corr,
                                                                                    rough_approx = rough_approx,
                                                                                    lkj_cholesky_eta = prior_lkj,
                                                                                    lkj_cholesky = mvr_cholesky,
                                                                                    prior_coeffs_mean = prior_mean_vec,
                                                                                    prior_coeffs_sd = prior_sd_vec,
                                                                                    n_class = n_class,
                                                                                    n_tests = n_tests,
                                                                                    X = X,
                                                                                    y = y
                                                                                    #    Eigen_y = y
    )
  toc()
}


tail(outs_manual_grad_AF[,1], 50)
outs_manual_grad_AF[1,2]

outs_manual_grad_AF[[3]] 
outs_manual_grad_AF[[4]] 
outs_manual_grad_AF[[5]] 
outs_manual_grad_AF[[6]]
outs_manual_grad_AF[[7]]

outs_manual_grad_AF[[4]]  +      outs_manual_grad_AF[[1]][2,2]

outs_manual_grad_AF[[1]][2,1]
outs_manual_grad_AF[[1]][2,2]

u_array = outs_manual_grad_AF[[1]]   ;   u_array # good
prob = outs_manual_grad_AF[[2]]   ;   prob # good
Z_std_norm = outs_manual_grad_AF[[3]]   ;   Z_std_norm # good
Phi_Z =outs_manual_grad_AF[[4]]   ;   Phi_Z # good
Bound_Z =outs_manual_grad_AF[[5]]   ;   Bound_Z # good
Bound_U_Bound_Phi_Z =outs_manual_grad_AF[[6]]   ;   Bound_U_Bound_Phi_Z  # good
prob_n =outs_manual_grad_AF[[7]]   ;   prob_n  # good
log_posterior =outs_manual_grad_AF[[8]]   ;   log_posterior ; sum(log_posterior)

inc_array =outs_manual_grad_AF[[10]]   ;   inc_array
y1_array =outs_manual_grad_AF[[11]]   ;   y1_array
lp_array =outs_manual_grad_AF[[9]]   ;   lp_array
af_prev =outs_manual_grad_AF[[12]]   ;   af_prev
log_prev =outs_manual_grad_AF[[13]]    ;   log_prev
y_sign =  outs_manual_grad_AF[[14]]    ;   y_sign
u_grad = outs_manual_grad_AF[[15]]    ;   u_grad
beta_grad = outs_manual_grad_AF[[16]]    ;   beta_grad

Z_std_norm[[1]]  -  Z_std_norm_1[[1]]
Phi_Z[[1]]  - Phi_Z_1[[1]]
Bound_Z[[1]]  - Bound_Z_1[[1]] 
Bound_U_Bound_Phi_Z[[1]]  - Bound_U_Bound_Phi_Z_1[[1]]
prob_n   - prob_n_1
log_posterior[[1]]  - log_posterior_1[[1]]

round(u_grad - u_grad_1, 7)

grad_z_mat =    outs_manual_grad_AF[[17]]    ;   grad_z_mat
grad_term_mat =    outs_manual_grad_AF[[18]]    ;   grad_term_mat
derivs_chain_container_vec_array =    outs_manual_grad_AF[[19]]    ;   derivs_chain_container_vec_array
common_grad_term_1 =    outs_manual_grad_AF[[20]]    ;   common_grad_term_1

outs_manual_grad_AF[[21]] 

sum(prob[[1]] - prob_1[[1]])
sum(grad_z_mat - grad_z_mat_1)
sum(grad_term_mat - grad_term_mat_1)
sum(derivs_chain_container_vec_array - derivs_chain_container_vec_array_1)

common_grad_term_1_1[[1]] - common_grad_term_1[[1]]

sum( outs_manual_grad_AF[[2]]  )


c <- 2
t <- 6
i <- 2

beta_grad
beta_grad_1
sum(common_grad_term_1[[c]][, t] *   rowSums(  derivs_chain_container_vec_array  ) )

beta_grad_array(c, t) =        (  af::matmul( af::transpose( common_grad_term_1[c](af::span, t)  )   ,    af::sum(  fn_AF_block(  derivs_chain_container_vec_array, 0,  0, N, i + 2)  , 1) )  ) ; 



i = 0 
step = n_tests * n_class;
#   for (int t = 0; t < n_tests; t++) {
for (t in 0:(n_tests - 1)) {
  
  #   for (int c = 0; c < n_class; c++) {
  for (c in 0:(n_class - 1)) {
    print(i)
    end_index = n_tests * N * n_class - (n_tests * n_class - (i + 1))  - 1;
    print(end_index)
    
    print(  head(  seq(from = i, to = end_index, by = step) , 10)  )
    
    
    i = i + 1;
    
  } 
  
}  

n_tests * N * n_class 




ratios <- c(0.15, 0.6, 1.25, 2.0, 4.4, 4.0, 4.9)
N_sample <- c(1, 4, 8, 16, 64, 128, 256) * 1000

plot(y = ratios, x = N_sample, xlab = "Sample size (NOTE: N_obs = N * n_tests, where n_tests = 6)", ylab = "ratio", type = "l", col = "blue")
points(y = ratios, x = N_sample, col = "blue")
abline(h = 1, col = "green")

df_CPU_vs_GPU  <- tibble(ratio = ratios, N = N_sample)
N_sample_factor <- as.factor(N_sample)
df_CPU_vs_GPU_factor <- tibble(ratio = ratios, N = N_sample_factor)

require(ggplot2)
ggplot(data = df_CPU_vs_GPU_factor, aes(x = N, y = ratio)) + 
  geom_point(size = 4, colour = "blue") + 
  geom_line(group = N, colour = "blue") + 
  ylab("Relative speed (CPU-only vs CPU+GPU)") + 
  xlab("N /Sample size (NOTE: N_obs = N * n_tests, where n_tests = 6)")



# CT dataset [T = 6, all binary, N ~ 4500] (from Hadgu and Qu, 1998) --------------------------------------------
Syva_DFA <- c(0,1,rep(0,18),rep(1,8),0,rep(1,6))
Syva_EIA <- c(0,0,1,rep(0,10),rep(1,7),rep(0,5),rep(1,4),0,rep(1,5))
Abbott_EIA <- c(0,0,0,1,rep(0,6),1,1,1,0,0,0,1,1,1,1,0,0,0,0,1,0,0,0,1,1,0,1,1,1,1)
GenProbe <- c(0,0,0,0,1,0,0,0,1,1,0,1,1,0,1,1,0,0,0,1,0,0,1,1,0,0,0,1,1,1,1,0,1,1,1)
Sanofi_EIA <- c(rep(0,5), 1,0,1,0,1,0,0,1,0,0,1,0,0,1,0,0,1,0,1,0,0,1,0,1,1,1,1,0,1,1)
Culture <- c(rep(0,6), rep(1,10), 0, rep(1,16), 0,1)
Freq <- c(4328, 7,9,14,16,15,17,3,4,2,1,1,2,2,5,1,5,1,1,1,5,2,1,4,1,6,1,6,6,7,9,2,9,2,87)


df_ct <- data.frame(Syva_DFA, Syva_EIA, Abbott_EIA, GenProbe, Sanofi_EIA, Culture, Freq)
df_ct_2 <- as.data.frame(lapply(df_ct, rep, df_ct$Freq))
nrow(df_ct_2)

#set.seed(123)
#df_ct_2 <- sample_n(df_ct_2, size = 500) # if want to analyse random subset of data

y <- df_ct_2[,1:6]
nrow(y)






#  --- | --------------------------------------------------  Run models using MPlus  --------------------------------------------------------------------
# install.packages("MplusAutomation")
require(MplusAutomation)




{
  
  N <- 5000
  
  if (N == 500)    y_for_Mplus <-  y_master_list_seed_123_datasets[[1]]
  if (N == 1000)   y_for_Mplus <-  y_master_list_seed_123_datasets[[2]]
  if (N == 2500)    y_for_Mplus <-  y_master_list_seed_123_datasets[[3]]
  if (N == 5000)    y_for_Mplus <-  y_master_list_seed_123_datasets[[4]]
  if (N == 12500)    y_for_Mplus <-  y_master_list_seed_123_datasets[[5]]
  if (N == 25000)    y_for_Mplus <-  y_master_list_seed_123_datasets[[6]]
  
  
  
  
  
  df <- data.frame(y_for_Mplus) %>% 
    dplyr::rename( u1 = X1,
                   u2 = X2, 
                   u3 = X3,
                   u4 = X4,
                   u5 = X5)
  
  
  tibble(df)
  
  
  
  
# prior_IW_d <-  10   # 10 => w/ prior-only model (N=2) get ~ (-0.67, 0.67) interval -  approx. equiv. to LKJ(2)
  prior_IW_d <-  11   # 11 -> like LKJ(3)
  
 # prior_IW_nd <- 24 # 24 ->  w/ prior-only model (N=2) get ~ (-0.41, 0.41) interval -  approx. equiv. to LKJ(10)
  prior_IW_nd <- 30 # 30 -> like LKJ(12)
  
  
}


 ### df <- df[c(1, 5, 7, 12, 36),]





#  mplus_run_type <- "mplus_pilot" 
mplus_run_type <- "final" 


if (mplus_run_type == "final") {
  
  {
    
    n_cores <- 64 
    n_chains <- 8 

    if (parallel::detectCores() < 17) { 
          n_cores  = 8 
          n_chains = 4
    }
    
    
    if (n_cores == 64) { # HPC: 64 cores + 8 chains
      
      if (N == 500)     {   fb_iter = 55000  ;  n_thin = 2   }   
      if (N == 1000)    {   fb_iter = 47500  ;  n_thin = 4   }  
      if (N == 2500)    {   fb_iter = 70000  ;  n_thin = 2  }  
      if (N == 5000)    {   fb_iter = 50000  ;  n_thin = 2  }   
      if (N == 12500)   {   fb_iter = 10000  ;  n_thin = 9  }   
      if (N == 25000)   {   fb_iter = 9600   ;  n_thin = 10  }   
      
    } else if (n_cores == 8) {  # Laptop: 8 cores + 4 chains
      
      if (N == 500)     {   fb_iter = 55000  ;  n_thin = 2  }    
      if (N == 1000)    {   fb_iter = 47500  ;  n_thin = 4  }    
      if (N == 2500)    {   fb_iter = 35000  ;  n_thin = 2  }    
      if (N == 5000)    {   fb_iter = 25000  ;  n_thin = 2  }     
      if (N == 12500)   {   fb_iter = 1000   ;  n_thin = 36 }    
      if (N == 25000)   {   fb_iter = 1000   ;  n_thin = 48 }     
      
    } else if (n_cores == 32) { 
      
      fb_iter = 5000 ; n_thin = 10
      
    }
    
    
  }
  
  print(paste("N = ", N))
  print(paste("n_cores = ", n_cores))
  print(paste("N_chains = ", n_chains))
  print(paste("N_iter (pb) = ",  0.5 * fb_iter * n_thin ))
  
} else { 
  
  n_chains <- 8 ;    n_cores <- 64 
  # n_chains <- 64 ;    n_cores <- 64 
  
  # n_chains <- 8 ;    n_cores <- 32 
  
  
  if (n_chains == 8) {
    
    fb_iter = 5000 ; n_thin = 10
    
  } else { 
    
    fb_iter = 5000 ; n_thin = 1
    
  }
  
  
  print(paste("N = ", N))
  print(paste("n_cores = ", n_cores))
  print(paste("N_chains = ", n_chains))
  print(paste("N_iter (pb) = ",  0.5 * fb_iter * n_thin ))
  
}

# n_cores = 8
# fb_iter = 1000

# 
# 
# fb_iter = fb_iter / 10
# 
# seed   =      10
# 
# n_chains = 32

for (seed in 1:3)  {
{ 
  tictoc::tic("mplus timer")
  
  
  fit_mplus <- mplusObject(TITLE = "Bayesian LCM-MVP - for mixed binary and/or ordinal data (up to 10 categories)",
                           #   DATA = "FILE = Mplus.dat;",
                           USEVARIABLES = "u1 u2 u3 u4 u5;",
                           VARIABLE = "  
                                       CATEGORICAL = u1-u5 ;
                                       CLASSES = C(2);", 
                           #    MONTECARLO = paste("SEED = ", seed, ";"), 
                           ANALYSIS = paste0("ESTIMATOR = BAYES;",  "\n",
                                             " CHAINS = ", n_chains, ";",   "\n",
                                             " PROCESSORS = ", n_cores, ";",   "\n",
                                             " TYPE = MIXTURE;",   "\n", 
                                             " FBITERATIONS = ", fb_iter, ";",  "\n",
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
                                                 p31 ~ D(5, 10);  ! equiv to p ~ Beta(3, 9)  
                                                ! p31 ~ D(1, 1); 
                                                
                                                      p11 ~ N(+0.40, 0.140625); 
                                                      p12-p15 ~ N(0, 1);   
                    
                                                       p26 ~ N(-2.10, 0.0625); 
                                                      p27-p30 ~ N(0, 1);  "), 
                           SAVEDATA = "bparameters = bparam.dat;",
                           rdata = data.frame(df),
                           quiet = FALSE
  )
  
  res_plus <- mplusModeler(fit_mplus,
                           modelout = paste0("mplus_model_seed_", seed, "_N_", N, ".inp"), 
                           writeData = "always",
                           # Mplus_command = "/opt/mplusdemo/",
                           run = 1)
  
  get_results(res_plus, "summaries")
  
  print(tictoc::toc(log = TRUE))
  log.txt <- tictoc::tic.log(format = TRUE)
  tictoc::tic.clearlog()
  time_total <- unlist(log.txt)
  
  
  if (seed == 10)   beepr::beep("random") # make sound to know model has finished running 
  
}






{
  mplus_posterior_samples <- get_bparameters(res_plus)$valid_draw
  # str(mplus_posterior_samples)
  
  # str(mplus_posterior_samples[[1]])
  
  
  mplus_posterior_samp_array <- array(dim = c(n_chains, dim(mplus_posterior_samples[[1]])[1], dim(mplus_posterior_samples[[1]])[2] - 2))
  #  str(mplus_posterior_samp_array)
  
  for (i in 1:n_chains) {
    mplus_posterior_samp_array[i, , ] <- mplus_posterior_samples[[i]][,3:(dim(mplus_posterior_samples[[1]])[2])]
  }
  
  superchain_ids = seq(from = 1, to = n_chains, by = 1)
  if (n_chains > 4)  superchain_ids = c(rep(1, n_chains/2), rep(2, n_chains/2))
  if (n_chains == 32)  superchain_ids = c(rep(1, n_chains/4), rep(2, n_chains/4), rep(3, n_chains/4), rep(4, n_chains/4))
  if (n_chains > 47)  superchain_ids = c(rep(1, n_chains/4), rep(2, n_chains/4), rep(3, n_chains/4), rep(4, n_chains/4))
  
  mplus_rhat <- mplus_ess <- mplus_n_rhat  <- c()
  for (param in 1:(dim(mplus_posterior_samples[[1]])[2] - 2)) {
    mplus_rhat[param] <- round(rstan::Rhat(t(mplus_posterior_samp_array[,,param])) , 4)
    mplus_n_rhat[param] <- round(posterior::rhat_nested(t(mplus_posterior_samp_array[,,param]), superchain_ids = superchain_ids ) , 4)
    mplus_ess[param] <- round(rstan::ess_bulk(t(mplus_posterior_samp_array[,,param])) , 1)
  }
  
  mplus_rhat <- unique(mplus_rhat) ; length(mplus_rhat)
  mplus_ess <- unique(mplus_ess) ; length(mplus_ess)
  mplus_n_rhat <- unique(mplus_n_rhat) ; length(mplus_n_rhat)
  
  sort(mplus_rhat)
  sort(mplus_ess)
  sort(mplus_n_rhat)
  
  mplus_max_rhat <- max(mplus_rhat, na.rm = TRUE)
  mplus_min_ess <- min(mplus_ess, na.rm = TRUE) 
  mplus_max_n_rhat <- max(mplus_n_rhat, na.rm = TRUE)
  
  mplus_time_total <-  as.numeric(substr(start = 0, stop = 8,      strsplit(time_total, "[:]")[[1]][2]     ))  # time (total)
  
  mplus_ess_per_sec_total <- mplus_min_ess / mplus_time_total
}






{
  
  
  n_iter <- dim(mplus_posterior_samples[[1]])[1]
  
  mplus_posterior_samp_array_merged <- array(dim = c(n_chains * n_iter, dim(mplus_posterior_samp_array)[3] ))
  
  
  
  i_start = 1
  i_end = n_iter
  
  for (kk in 1:n_chains) {
    
    mplus_posterior_samp_array_merged[i_start:i_end, ] = mplus_posterior_samp_array[kk, , ]
    
    i_start = i_start + n_iter
    i_end = i_end + n_iter
  }
  
  
  mplus_D_pos_means <- mplus_D_neg_means <-   rep(0, (n_covariates + 1)*n_tests )
  mplus_prev_means   <- rep(0, n_class - 1)
  
  for (kk in 1:n_chains) {
    mplus_mean <- c()
    for (param in 1:(dim(mplus_posterior_samples[[kk]])[2] - 2)) {
      mplus_mean[param] <- mean(pnorm(mplus_posterior_samp_array_merged[, param]))
    }
    
    #   round(mplus_mean, 2)
    mplus_D_pos <- mplus_mean[21:25]
    mplus_D_neg <- mplus_mean[26:30]
    mplus_prev <-  mplus_mean[31]
    
    mplus_D_pos_means = mplus_D_pos_means + mplus_D_pos
    mplus_D_neg_means = mplus_D_neg_means + mplus_D_neg
    mplus_prev_means = mplus_prev_means + mplus_prev
    
  }
  
  mplus_D_pos_means <- mplus_D_pos_means / n_chains
  mplus_D_neg_means <- mplus_D_neg_means / n_chains
  mplus_prev_means <- mplus_prev_means / n_chains
  
  print(round(1 - mplus_D_pos_means, 3))
  print(round(mplus_D_neg_means, 3))
  print(round(mplus_prev_means, 3))
  
}








{
  print(paste0("seed = ", seed))
  print(paste0("Max nR-hat = ", round(mplus_max_n_rhat, 3)))
  print(paste0("Max R-hat = ", round(mplus_max_rhat, 3)))
  print(paste0("Min ESS = ", round(mplus_min_ess, 0)))
  print(paste0("Time (total) = ", round(mplus_time_total, 0), " seconds"))
  print(paste0("Min ESS / sec (total) = ", round(mplus_ess_per_sec_total , 3)))
  print(paste0("Min ESS / sec (sampling only)  = ", round(2 *mplus_ess_per_sec_total , 3)))
  print(paste0("Bin or Ord? = ",  if (max(y) > 1) { print("Ord") } else { print("Bin")} ))
  print(paste("N = ", N))
  print(paste("n_cores = ", n_cores))
  print(paste("N_chains = ", n_chains))
  print(paste("N_iter (pb) = ",  0.5 * fb_iter * n_thin ))
  
  # save efficiency summary info only 
  #  file_name <- paste0("Mplus_", "efficiency_info_", "seed_", seed, "_",  prior_IW_d, "_", prior_IW_nd,  "prior_IW_", N, "N_", n_chains, "chains_", ".RDS")
  
  file_name <- paste0("Mplus_",
                      "efficiency_info_", 
                      "seed_", seed, "_", 
                      prior_IW_d, "_",   prior_IW_nd,  "prior_IW_", 
                      N, "N_",
                      n_cores, "N_cores_", 
                      n_chains, "N_chains_",
                      fb_iter, "fb_iter_",
                      n_thin, "N_thin_",
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
    mplus_posterior_samples
  )
  
  saveRDS(file_list, file = file_name)
}




}

}










# getSavedata_Bparams(res_plus, discardBurnin = TRUE)
# 
# readModels(target = "/home/enzo/Documents/Work/PhD_Chapter_1_work/BayesLCM/R/mplus_model_1.out", 
#            what="bparameters")$bparameters 


# readRDS("Mplus_efficiency_info_seed_6_10_24prior_IW_12500N_8chains_.RDS")




ESS <- c(307, 324, 264, 220, 284, 252, 306, 328, 274, 278)
total_times <- c(162, 131, 153, 144, 162, 128, 145, 133, 125, 120) / 60
samp_times <- total_times  / 2


ESS <- c(95, 92, 87, 119, 137, 52, 97, 75, 98, 123)
total_times <- c(1166, 1169, 1237, 1135, 1030, 1142, 1142, 1057, 1247, 1102)/60
samp_times <- total_times  / 2


mean(ESS / (samp_times * 2 *  60)) ;  sd(ESS / (samp_times * 2* 60))
mean(ESS / (samp_times   *  60)) ;  sd(ESS / (samp_times  *  60))

mean((100 / ESS) * samp_times) * 2 ;  sd((100 / ESS) * samp_times) * 2
mean((1000 / ESS) * samp_times) * 2 ;  sd((1000 / ESS) * samp_times) * 2



mean(10000 * (1000 / ESS))
# -------------------------------------------------------------------------

slow_sum <- function(x,b,c) {
  xx <- x
  p <- progressr::progressor(along = xx)
  sum <- 0
  for (i in seq_along(x)) {
    Sys.sleep(0.1)
    print(p(message = sprintf("Adding %g", xx[i])))
    sum <- sum + x[i]
    print(xx[i])
    
  }
  # sum
}

with_progress(yy <- slow_sum(1:100, b=10,c=1))

# | -------------    run binary model (no covariates) using custom PX-Gibbs algorithm on CT binary dataset - EXAMPLE 1-------------------------------------------------------------------------

# run w/ multiple chains
n_chains <- 4
n_iter <- 2000
n_burnin <- 2000
n_thin <- 2
iter / n_thin
n_class <- 2
P <- 1

CI_class <-  c(1,0) # if want to assume CI for subset of classes (1 = yes, 0 = no)
CI_test_pair_d <- matrix(0,nrow = n_tests, ncol = n_tests)
CI_test_pair_d[6, 1:n_tests] <- rep(1, n_tests)
CI_test_pair_d[1:n_tests, 6] <- rep(1, n_tests)
CI_test_pair_nd <- matrix(1, nrow = n_tests, ncol = n_tests)
CI_test_pair <-  array(c(CI_test_pair_nd, CI_test_pair_d), dim = c(n_tests, n_tests, n_class)) # if want to assume CI for subset of test-pairs in each class (1 = yes, 0 = no)

# run model
models <-     BayesLCM_run(y = y, 
                           X = X, 
                           n_burnin = n_burnin, 
                           n_iter = n_iter, 
                           # n_thin = n_thin,
                           seed = 123, 
                           f = c(6,6), 
                           CI = FALSE,  
                           CI_class = CI_class,
                           CI_test_pair = CI_test_pair,
                           n_chains = n_chains )




# extract and print model summaries (inc. R-hat model sampler diagnostic)
summaries <- BayesLCM_summary(models, dp = 2)

print(summaries$summary,n=100)
print(summaries$summary_probs,n=100)

# model sampler diagnostics plots
samples <- BayesLCM_samples(models)

params_data_ggmcmc_section <- dplyr::filter(samples, Parameter_num %in% c(1:12))

# density plots
ggs_density(params_data_ggmcmc_section) + 
  facet_wrap(~ Parameter, ncol = 4,scales = "free")



#traceplots
ggs_traceplot(params_data_ggmcmc_section)


# autocorrelation
ggs_autocorrelation(params_data_ggmcmc_section, nLags = 500) + 
  facet_wrap(~ Parameter, ncol = 4, scales = "free")




#traceplot
ts.plot(fit.AlbertChib$prev)
ts.plot(fit.AlbertChib$Omega_d[,1,2])
ts.plot(fit.AlbertChib$Omega_d[,1,3])
ts.plot(fit.AlbertChib$Omega_d[,1,4])
ts.plot(fit.AlbertChib$Omega_d[,1,5])
ts.plot(fit.AlbertChib$Omega_d[,2,5]) #
ts.plot(fit.AlbertChib$Omega_d[,3,5]) #
ts.plot(fit.AlbertChib$Omega_d[,2,6])  #
ts.plot(fit.AlbertChib$Omega_nd[,3,5])
ts.plot(fit.AlbertChib$Omega_nd[,3,1])
ts.plot(fit.AlbertChib$Omega_nd[,1,4])
ts.plot(fit.AlbertChib$Omega_nd[,3,4])
ts.plot(fit.AlbertChib$Omega_nd[,6,5])
ts.plot(fit.AlbertChib$Omega_nd[,6,4])
ts.plot(fit.AlbertChib$beta_d[,1])
ts.plot(fit.AlbertChib$beta_d[,2])
ts.plot(fit.AlbertChib$beta_d[,3])
ts.plot(fit.AlbertChib$beta_d[,4])
ts.plot(fit.AlbertChib$beta_d[,5])
ts.plot(fit.AlbertChib$beta_d[,6])
ts.plot(fit.AlbertChib$beta_nd[,1])
ts.plot(fit.AlbertChib$beta_nd[,2])

# implied prior for correlations (IW(I, f))
m <- 2 # usually set to number of classes (e.g. Asparouhov et al)  or number of tests
f <- 13 # f > m+1 - f if the DOF parameter for the Wishart prior 
f <- 7
f <- 12 # f > m+1 - f if the DOF parameter for the Wishart prior 
f <- 23 # f > m+1 - f if the DOF parameter for the Wishart prior 
sort(rbeta(n=100000, (f - m + 1)/2, (f - m + 1)/2)*2 - 1)[2500]
sort(rbeta(n=100000, (f - m + 1)/2, (f - m + 1)/2)*2 - 1)[97500]


m <- 10
m <- 24
p <- 5
sort(rbeta(n=100000, (m - p + 2)/2, (m - p + 2)/2)*2 - 1)[2500]
sort(rbeta(n=100000, (m - p + 2)/2, (m - p + 2)/2)*2 - 1)[97500]



# compare to LKJ
lkj <- 6
sort(c(rbeta(n=100000,lkj,lkj )*2 - 1))[2500]
sort(c(rbeta(n=100000,lkj,lkj )*2 - 1))[97500]

#autocorrelation plot
par(mfrow=c(2,3))
autocorr.plot(fit.AlbertChib$beta_d[,1:n_tests],lag.max =  1000,auto.layout=F)
par(mfrow=c(2,3))
autocorr.plot(fit.AlbertChib$beta_nd[,1:n_tests],lag.max =  1000,auto.layout=F)
par(mfrow=c(2,3))
autocorr.plot(fit.AlbertChib$Omega_d[,1,2], lag.max =  1000,auto.layout=F)
autocorr.plot(fit.AlbertChib$Omega_d[,1,3], lag.max =  1000,auto.layout=F)
autocorr.plot(fit.AlbertChib$Omega_d[,1,4], lag.max =  1000,auto.layout=F)
autocorr.plot(fit.AlbertChib$Omega_d[,1,5], lag.max =  1000,auto.layout=F)
autocorr.plot(fit.AlbertChib$Omega_d[,1,6], lag.max =  1000,auto.layout=F)
autocorr.plot(fit.AlbertChib$Omega_d[,2,3], lag.max =  1000,auto.layout=F)
autocorr.plot(fit.AlbertChib$Omega_d[,2,4], lag.max =  1000,auto.layout=F)
autocorr.plot(fit.AlbertChib$Omega_d[,2,5], lag.max =  1000,auto.layout=F)
autocorr.plot(fit.AlbertChib$Omega_d[,2,6], lag.max =  1000,auto.layout=F)
autocorr.plot(fit.AlbertChib$Omega_d[,2,1], lag.max =  1000,auto.layout=F)
autocorr.plot(fit.AlbertChib$Omega_nd[,1,3], lag.max =  1000,auto.layout=F)
autocorr.plot(fit.AlbertChib$Omega_nd[,2,3], lag.max =  1000,auto.layout=F)
autocorr.plot(fit.AlbertChib$Omega_nd[,2,4], lag.max =  1000,auto.layout=F)
autocorr.plot(fit.AlbertChib$Omega_nd[,2,5], lag.max =  1000,auto.layout=F)
autocorr.plot(fit.AlbertChib$Omega_nd[,5,6], lag.max =  1000,auto.layout=F)
autocorr.plot(fit.AlbertChib$Omega_nd[,4,6], lag.max =  1000,auto.layout=F)


# Run Ordinal model (epilepsy dataset - EXAMPLE 2 - no covariates) -------------------------------------------

# read in Epilepsy dataset (from Xu et al, 2013) - 3 ordinal, 0 binary, N ~ 300
Freq <- c(181, 1, 1, 56, 5, 16, 1, 1, 10, 3, 5, 2, 2, 2, 1, 2, 4, 1, 18)
Comp <- c(1,1,1,1,1,1,1,1,2,2,2,2,2,2,3,3,3,3,3)
Reader_1 <- c(1,1,1,2,2,3,3,3,1,1,2,3,3,3,1,1,2,3,3)
Reader_2 <- c(1,2,3,1,2,1,2,3,1,3,1,1,2,3,1,2,3,2,3)

data_ep <- tibble(Freq, Comp, Reader_1, Reader_2)

data_ep_orig <- tibble(Freq, Comp-2, Reader_1-2, Reader_2-2)

data_ep_2 <- as.data.frame(lapply(data_ep, rep, data_ep$Freq)) %>% 
  dplyr::select(-Freq)
y <- data_ep_2

N <- nrow(y)

y <- array(dim = c(N, 3))
for (t in 1:3)
  y[,t] <- array(data_ep_2)[[t]]

n_threshold <- c(2,2,2)
max_threshold <- 2
n_bin_tests <- 0
n_ord_tests <- 3

# Run Ordinal model (depression dataset - EXAMPLE 3 - with covariates) - 1 binary, 2 ordinal, N ~ 940 -------------------------------------------

# read in depression dataset
y <- readRDS("depression_data_ordinal_no_covariates.R")
y <- y[-143,]
N <- nrow(y)

y_dichot <- y_array <- y
y_dichot[,2] <- ifelse(y_dichot[,2] > 10, 1, 0)
y_dichot[,3] <- ifelse(y_dichot[,3] > 10, 1, 0)


#  make tibble 
y <- tibble(data.frame(y)) %>% 
  dplyr::rename(MINI = X1,
                PHQ_9 = X2,
                CES_D = X3) 
y

y_dichot <- y

y <- tibble(data.frame(y_dichot)) %>% 
  dplyr::rename(MINI = X1,
                PHQ_9 = X2,
                CES_D = X3) 
y

#  cat. covariates 
X_cat <- readRDS("depression_data_ordinal_cat_covariates.R")
X_cat <- array(X_cat[,2], dim = c(941, 1)) # language
X_cat <- array(X_cat[-143,], dim = c(940,1)) # remove obs. w/ missing age
X_cat

X_cat_groups_array <- X_cat

group <- c(X_cat)


# make tibble
X_cat <- tibble(data.frame(X_cat)) %>%
  dplyr::rename(Language = X_cat)

# use helper function to make tibble contrast matrix (to make it usable with model)
source("BayesLCM_contrast_matrix_fn.R")
X_cat <- BayesLCM_contrast_matrix(X_cat)




# continuous covariate (age)
X_cts <- readRDS("depression_data_ordinal_cts_covariates.R")

# remove obs 143 as has missing age (havent implemented MI yet)
X_cts <- array(c(X_cts[-143, ]), 
               dim = c(940, 1))

# center at mean
X_cts[,1] <- X_cts[,1] - mean(X_cts[,1])

# make tibble
X_cts <- tibble(data.frame(X_cts)) %>%
  dplyr::rename(Age_centered = X_cts)

X_cts

# make design matrices
X_age_and_language <- tibble(cbind(X_cts, X_cat))
n_covariatres <- 3
X_age <- X_age_and_language %>% 
  dplyr::select(Age_centered)
n_covariatres <- 1
X_language <- X_age_and_language %>% 
  dplyr::select(Language_cat_1, Language_cat_2)
n_covariatres <- 2

# design matrix to use for model
X <- X_language


# run w/ multiple chains


# CI_class <-  c(0,0) # if want to assume CI for subset of classes (1 = yes, 0 = no)
# CI_test_pair_d <- matrix(0,nrow = n_tests, ncol = n_tests)
# CI_test_pair_d[1,3] <- CI_test_pair_d[3,1] <- 1
# CI_test_pair_nd <- matrix(0, nrow = n_tests, ncol = n_tests)
# CI_test_pair_nd[1,3] <- CI_test_pair_nd[3,1] <- 1
# CI_test_pair <-  array(c(CI_test_pair_nd, CI_test_pair_d), dim = c(n_tests, n_tests, n_class)) # if want to assume CI for subset of test-pairs in each class (1 = yes, 0 = no)

beta_mean_nd <- c(-1.25, 0, 0,   # tests 1-3, coeff 1 (intercept)
                  # 0, 0, 0,
                  0, 0, 0,
                  0, 0, 0) 

beta_mean_d <- c(1.25, 0, 0,   # tests 1-3, coeff 1 (intercept)
                 #   0, 0, 0, 
                 0, 0, 0,
                 0, 0, 0) 

n_tests <- 6
n_class <- 2
n_covariatres <- 0
prior_mean_vec <- array(c(beta_mean_nd, beta_mean_d), dim = c(n_tests*(n_covariatres + 1), n_class))
prior_mean_vec

prior_mean_vec[1,2] <- 0
prior_mean_vec[1,3] <- 1.25
prior_mean_vec

num1 <- 1
num2 <- 1
beta_var_nd <-  c(1,  1,  1,   # tests 1-3, coeff 1 (intercept)
                  # num1, num2, num2,
                  num1, num2, num2, 
                  num1, num2, num2) 

beta_var_d <- beta_var_nd

prior_sd_vec  <- array(c(beta_var_nd, beta_var_d), dim = c(n_tests*(n_covariatres + 1), n_class))
prior_sd_vec

prior_sd_vec

#prior_mean_vec <- array(0, dim = c(n_tests*(n_covariatres + 1), n_class))
#prior_sd_vec  <- array(1, dim = c(n_tests*(n_covariatres + 1), n_class))

n_ord_tests <- 2
max_threshold <- 17
prior_ind_dir_alpha <- array(1, dim = c(n_class, n_ord_tests, max_threshold + 1))

group <- rep(1, N)
prior_prev <- array(1, dim = c(length(unique(group)), n_class))



initial_prev <- array(dim = c(length(unique(group)), n_class))

for (g in 1:length(unique(group))) { 
  if (n_class == 2) { 
    initial_prev[g,2] <- ifelse(0 %in% y[,1], sum(y[,1])/nrow(y), 0.1)
    initial_prev[g,1] <- 1 - initial_prev[g,2]
  } else {
    for (c in 1:n_class) { 
      initial_prev[g,c] <- 1/n_class
      
    }
  }
}

initial_prev[1,2] <- 0.30
initial_prev[2,2] <- 0.20
initial_prev[3,2] <- 0.55
initial_prev[ ,1] <- 1 - initial_prev[ ,2]



samples <- rep(NA, 10000)

for (i in 1:10000) { 
  samples[i] <- cov2cor(riwish(v =  11,  S = prior_Omega[,,c] ))[1,2]
}

sort(samples)[250]
sort(samples)[9750]

#   cppFunction(depends = "RcppArmadillo",
#               '
#   arma::vec fn_beta_var( arma::mat X2_, arma::mat Omega, arma::mat prior_sd_vec ) {
#     arma::mat Omega_inv = inv(Omega);
#     arma::mat prior_prec_matrix = inv(diag(prior_sd_vec^2))
#     arma::vec res   = inv(  kron(X2_.t() * X2_, Omega_inv ) + prior_prec_matrix ) ;
#     return res;
#   }
# '
#   )

# Rccp function to calculate 

fn_dmvn <-   cppFunction(depends = "RcppArmadillo",
                         '
          arma::mat fn_dmvn( arma::mat XX, arma::mat mean, arma::mat Sigma ) {
            arma::mat Sigma_inv = inv(Sigma);
            int n_tests = XX.n_cols;
            int N = XX.n_rows;
            double det_Sigma = det(Sigma);
            double stuff =  std::pow((2*3.14159265359), (-n_tests*0.5) ) * 1 / (sqrt(det_Sigma)) ;
            arma::mat X_minus_mean = XX - mean;
            arma::mat X_minus_mean_trans = X_minus_mean.t();
            arma::mat res1(N, 1);
             for (int n = 0; n < N; n++) {
               res1(n,0) =   stuff * exp( -0.5 * ((X_minus_mean_trans.col(n).t() * Sigma_inv  *  X_minus_mean.row(n).t() )) ).eval()(0,0) ;
             }
           return res1;
          }
        '
)


tic()
for (i in 1:1000)
  xxxx <- fn_dmvn(Z,  Xbeta[c,,],  Omega[,,c])
toc()

fn_rtnorm <-   cppFunction(depends = c("RcppArmadillo", "RcppDist"),
                           '
          arma::vec fn_rtnorm( arma::vec means, double sd, arma::vec lb, arma::vec ub ) {
            int N = means.n_elem;
            arma::mat res1(N, 1);
             for (int n = 0; n < N; n++) {
               res1(n,0) =   r_truncnorm(means(n), sd, lb(n),  ub(n)); 
             }
           return res1;
          }
        '
)



tic()
for (i in 1:5000) { 
  xxxx <- fn_rtnorm_double_loop(  Xbeta = Xbeta, Omega = Omega, lb=lb, ub=ub )
}
toc()

fn_rtnorm(means = mean_2[c,  ,t], sd = sqrt(sigma_sq[c,t]), lb =  lb[ ,c,t], ub =  ub[ ,c,t])


tic()
for (c in 1:n_class) {
  for (t in 1:n_tests) {
    for (i in 1:5000) {  
      Z_class[c,  ,t] <-  fn_rtnorm(means = mean_2[c,  ,t], sd = sqrt(sigma_sq[c,t]), lb =  lb[ ,c,t], ub =  ub[ ,c,t])
      
      # Z_class[c, 1,t] <- truncnorm::rtruncnorm(n = 1, a = lb[ 1,c,t], b = ub[ 1,c,t], mean =  mean_2[c, 1,t], sd = sqrt(sigma_sq[c,t]))
      #  Z_class[c, ,t] <- truncnorm::rtruncnorm(n = N, a = lb[ ,c,t], b = ub[ ,c,t], mean =  mean_2[c, ,t], sd = sqrt(sigma_sq[c,t]))
    }
  }
}
#
toc()


#y = rbind(y,y,y)
#X = rbind(X,X,X)
#group = rep(group, 3)



# # run model ---------------------------------------------------------------------



initial_prev <- array(dim = c(length(unique(group)), n_class))

for (g in 1:length(unique(group))) { 
  if (n_class == 2) { 
    initial_prev[g,2] <- ifelse(0 %in% y[,1], sum(y[,1])/nrow(y), 0.1)
    initial_prev[g,1] <- 1 - initial_prev[g,2]
  } else {
    for (c in 1:n_class) { 
      initial_prev[g,c] <- 1/n_class
      
    }
  }
}

initial_prev[1,2] <- 0.30
initial_prev[2,2] <- 0.20
initial_prev[3,2] <- 0.55
initial_prev[ ,1] <- 1 - initial_prev[ ,2]


n_chains <- 4
n_iter <- 5000
n_burnin <- 5000
n_thin <- 1
n_iter / n_thin
n_class <- 2

n_covariates <- 0
initial_beta <- array(0, dim = c(n_class, n_tests, n_covariates+1))
initial_beta[1,,1] <- -1
initial_beta[2,,1] <- 1
# initial_beta[1,2,1] <- initial_beta[1,3,1] <- -1
# initial_beta[2,2,1] <- initial_beta[2,3,1] <- 1



CI_class <-  c(1,0) # if want to assume CI for subset of classes (1 = yes, 0 = no)
CI_test_pair_d <- matrix(0,nrow = n_tests, ncol = n_tests)
CI_test_pair_d[6, 1:n_tests] <- rep(1, n_tests)
CI_test_pair_d[1:n_tests, 6] <- rep(1, n_tests)
CI_test_pair_nd <- matrix(1, nrow = n_tests, ncol = n_tests)
CI_test_pair <-  array(c(CI_test_pair_nd, CI_test_pair_d), dim = c(n_tests, n_tests, n_class)) # if want to assume CI for subset of test-pairs in each class (1 = yes, 0 = no)


X<- array(1, dim = c(N, 1))

initial_prev <- array(dim = c(length(unique(group)), n_class))

for (g in 1:length(unique(group))) { 
  if (n_class == 2) { 
    initial_prev[g,2] <- ifelse(0 %in% y[,1], sum(y[,1])/nrow(y), 0.1)
    initial_prev[g,1] <- 1 - initial_prev[g,2]
  } else {
    for (c in 1:n_class) { 
      initial_prev[g,c] <- 1/n_class
      
    }
  }
}

seed=123
corr_W <- array(0 , dim = c( n_tests, n_tests,n_class)) 
MH_corr = T
MH_cutpoints = T
induced_dir = T
update_prog = F
group = rep(1, N)
intercept_only <- T
f = c(10,10)
n_class = n_class
CI = T

perfect_gs = F
CI_class = rep(0, n_class) #if want to assume CI for subset of classes 
CI_test_pair =  array(0, dim = c(n_tests, n_tests, n_class))  # if want to assume CI for subset of test-pairs 
prior_Omega = array(diag(rep(1, n_tests)), dim = c(n_tests, n_tests, n_class)) # prior corr matrix for each class
#   prior_mean_vec = prior_mean_vec,
#   prior_sd_vec  = prior_sd_vec,
prior_mean_vec = array(c(rep(-1,3),rep(-1,3), rep(1,3), rep(1,3)), dim = c(n_tests*(n_covariates + 1), n_class))
prior_sd_vec = array(c(rep(1,3), rep(1,3)), dim = c(n_tests*(n_covariates + 1), n_class))
prior_ind_dir_alpha = prior_ind_dir_alpha <- array(1, dim = c(n_class, n_ord_tests, max_threshold + 1))

models <-     BayesLCM_run(
  y = y, 
  #    y = y_dichot,
  # X = X,
  X = NULL,
  #  y = rbind(y,y,y),
  # X = rbind(X,X,X),
  # group = rep(group, 3),
  intercept_only = TRUE,
  # group = group,
  group = rep(1, N),
  n_burnin = 4000, 
  n_iter = 4000, 
  n_thin = n_thin,
  n_chains = n_chains,
  seed = 123, 
  f = c(10,10), 
  n_class = n_class,
  CI = T,
  perfect_gs = F,
  CI_class = rep(0, n_class), #if want to assume CI for subset of classes 
  CI_test_pair =  array(0, dim = c(n_tests, n_tests, n_class)) , # if want to assume CI for subset of test-pairs 
  prior_Omega = array(diag(rep(1, n_tests)), dim = c(n_tests, n_tests, n_class)) , # prior corr matrix for each class
  prior_mean_vec = prior_mean_vec,
  prior_sd_vec  = prior_sd_vec,
  # prior_mean_vec = array(c(rep(-1,3),rep(0,3), rep(1,3), rep(0,3)), dim = c(n_tests*(n_covariates + 1), n_class)),
  #  prior_sd_vec = array(c(rep(1,3), rep(1,3)), dim = c(n_tests*(n_covariates + 1), n_class)),
  prior_ind_dir_alpha = prior_ind_dir_alpha,
  #  prior_prev = prior_prev,
  # initial_prev = initial_prev,
  initial_beta = initial_beta,
  induced_dir = F,
  anchor_param = T,
  update_prog = TRUE
)



# extract and print model summaries (inc. R-hat model sampler diagnostic)
rm(mean)
summaries <- BayesLCM_summary(models, dp = 2)

print(summaries$summary,n=50)

print(summaries$summary,n=150)

print(summaries$summary_per_chain[[1]],n=50)
print(summaries$summary_per_chain[[2]],n=50)
print(summaries$summary_per_chain[[3]],n=50)
print(summaries$summary_per_chain[[4]],n=50)
print(summaries$summary_per_chain[[5]],n=40)
print(summaries$summary_per_chain[[6]],n=40)
print(summaries$summary_per_chain[[7]],n=40)
print(summaries$summary_per_chain[[8]],n=40)

print(summaries$summary,n=200)

# subset chains (useful when label switching occurs - which happens quite often when > 2 classes)
summaries <- BayesLCM_summary(models, dp = 2, chains_subset = c(2,3))

print(summaries$summary,n=200)



# for multiple groups

# PHQ-9 
# group 3 (intercept)
Sp_PHQ_9 <- round(pnorm(summaries$summary_per_chain[[1]]$median[31:46] - summaries$summary_per_chain[[1]]$median[8]),2)
Se_PHQ_9 <- 1 - round(pnorm(summaries$summary_per_chain[[1]]$median[64:79] - summaries$summary_per_chain[[1]]$median[11]),2)
Sp_PHQ_9 ; Se_PHQ_9


# group 2 (coeff 2)
Sp_PHQ_9 <- round(pnorm(summaries$summary$median[31:46] - summaries$summary$median[8] -  summaries$summary$median[16]),2)
Se_PHQ_9 <- 1 - round(pnorm(summaries$summary$median[64:79] - summaries$summary$median[11] -  summaries$summary$median[22]),2)
Sp_PHQ_9 ; Se_PHQ_9

# group 1 (coeff 1)
Sp_PHQ_9 <- round(pnorm(summaries$summary$median[31:46] - summaries$summary$median[8] - summaries$summary$median[15]),2)
Se_PHQ_9 <- 1 - round(pnorm(summaries$summary$median[64:79] - summaries$summary$median[11] - summaries$summary$median[21]),2)
Sp_PHQ_9 ; Se_PHQ_9

# CES-D
Sp_CES_D <- round(pnorm(summaries$summary$median[47:63] - summaries$summary$median[9]),2)
Se_CES_D <- 1 - round(pnorm(summaries$summary$median[80:96] - summaries$summary$median[12]),2)
Sp_CES_D ; Se_CES_D


# 3 pops, intercept-only 
Sp_PHQ_9 <- round(pnorm(summaries$summary_per_chain[[1]]$median[19:34] - summaries$summary_per_chain[[1]]$median[8]),2)
Se_PHQ_9 <- 1 - round(pnorm(summaries$summary_per_chain[[1]]$median[52:67] - summaries$summary_per_chain[[1]]$median[11]),2)
Sp_PHQ_9 ; Se_PHQ_9



Sp_CES_D <- round(pnorm(summaries$summary_per_chain[[1]]$median[35:51] - summaries$summary_per_chain[[1]]$median[9]),2)
Se_CES_D <- 1 - round(pnorm(summaries$summary_per_chain[[1]]$median[68:84] - summaries$summary_per_chain[[1]]$median[12]),2)
Sp_CES_D ; Se_CES_D



Sp_PHQ_9 <- round(pnorm(summaries$summary$median[19:34] - summaries$summary$median[8]),2)
Se_PHQ_9 <- 1 - round(pnorm(summaries$summary$median[52:67] - summaries$summary$median[11]),2)
Sp_PHQ_9 ; Se_PHQ_9



Sp_CES_D <- round(pnorm(summaries$summary$median[35:51] - summaries$summary$median[9]),2)
Se_CES_D <- 1 - round(pnorm(summaries$summary$median[68:84] - summaries$summary$median[12]),2)
Sp_CES_D ; Se_CES_D







# for 1 group
Sp_PHQ_9 <- round(pnorm(summaries$summary$median[15:30] - summaries$summary$median[4]),2)
Se_PHQ_9 <- 1 - round(pnorm(summaries$summary$median[48:63] - summaries$summary$median[7]),2)
Sp_PHQ_9 ; Se_PHQ_9


Sp_CES_D <- round(pnorm(summaries$summary$median[31:47] - summaries$summary$median[5]),2)
Se_CES_D <- 1 - round(pnorm(summaries$summary$median[64:80] - summaries$summary$median[8]),2)
Sp_CES_D ; Se_CES_D



#  simulated data
Sp_t4_gibbs <- round(pnorm(summaries$summary$median[45:54] - summaries$summary$median[6]),2)
Se_t4_gibbs <- 1 - round(pnorm(summaries$summary$median[75:84] - summaries$summary$median[12]),2)
Sp_t4_gibbs ; Se_t4_gibbs


Sp_t5_gibbs <- round(pnorm(summaries$summary$median[55:64] - summaries$summary$median[7]),2)
Se_t5_gibbs <- 1 - round(pnorm(summaries$summary$median[85:94] - summaries$summary$median[13]),2)
Sp_t5_gibbs ; Se_t5_gibbs

Sp_t6_gibbs <- round(pnorm(summaries$summary$median[65:74] - summaries$summary$median[8]),2)
Se_t6_gibbs <- 1 - round(pnorm(summaries$summary$median[95:104] - summaries$summary$median[14]),2)
Sp_t6_gibbs ; Se_t6_gibbs



# Plots -------------------------------------------------------------------

#dev.off()

par(mfrow = c(1,3))

plot(1 - Sp_t4_true, Se_t4_true)
lines(1 - Sp_t4_Stan, Se_t4_Stan, col = "red")
lines(1 - Sp_t4_gibbs, Se_t4_gibbs, col = "blue")
lines(Se_t4_gibbs, 1 - Sp_t4_gibbs, col = "blue")

plot(1 -  Sp_t5_true,  Se_t5_true)
lines(1 - Sp_t5_Stan,  Se_t5_Stan, col = "red")
lines(1 - Sp_t5_gibbs, Se_t5_gibbs, col = "blue")
lines(Se_t5_gibbs, 1 - Sp_t5_gibbs, col = "blue")

plot(1 -  Sp_t6_true,  Se_t6_true)
lines(1 - Sp_t6_Stan,  Se_t6_Stan, col = "red")
lines(1 - Sp_t6_gibbs, Se_t6_gibbs, col = "blue")
lines(Se_t5_gibbs, 1 - Sp_t5_gibbs, col = "blue")

#par(mfrow = c(1,1))
par(mfrow = c(1,1))

dev.off()

# Se's
plot(Se_t4_true - Se_t4_Stan, ylim = c(-0.1,0.1), col = "red")
abline(h=0)
plot(Se_t4_true - Se_t4_gibbs, ylim = c(-0.1,0.1), col = "blue")
abline(h=0)

# Sp's
plot(Sp_t4_true - Sp_t4_Stan, ylim = c(-0.1,0.1), col = "red")
abline(h=0)
plot(Sp_t4_true - Sp_t4_gibbs, ylim = c(-0.1,0.1), col = "blue")
abline(h=0)

# Se's
plot(Se_t5_true - Se_t5_Stan, ylim = c(-0.1,0.1), col = "red")
abline(h=0)
plot(Se_t5_true - Se_t5_gibbs, ylim = c(-0.1,0.1), col = "blue")
abline(h=0)

# Sp's
plot(Sp_t5_true - Sp_t5_Stan, ylim = c(-0.1,0.1), col = "red")
abline(h=0)
plot(Sp_t5_true - Sp_t5_gibbs, ylim = c(-0.1,0.1), col = "blue")
abline(h=0)

# Se's
plot(Se_t6_true - Se_t6_Stan, ylim = c(-0.1,0.1), col = "red")
abline(h=0)
plot(Se_t6_true - Se_t6_gibbs, ylim = c(-0.1,0.1), col = "blue")
abline(h=0)

# Sp's
plot(Sp_t6_true - Sp_t6_Stan, ylim = c(-0.1,0.1), col = "red")
abline(h=0)
plot(Sp_t6_true - Sp_t6_gibbs, ylim = c(-0.1,0.1), col = "blue")
abline(h=0)

# calculate posterior probabiliies of interest
# for MINI (binary test)

# for PHQ-9 and CES-D (ordinal tests)



# model sampler diagnostics plots
samples <- BayesLCM_samples(models)

params_data_ggmcmc_section <- dplyr::filter(samples, Parameter_num %in% c(1:12))

# density plots
ggs_density(params_data_ggmcmc_section) + 
  facet_wrap(~ Parameter, ncol = 4,scales = "free")



#traceplots
ggs_traceplot(params_data_ggmcmc_section)


# autocorrelation
ggs_autocorrelation(params_data_ggmcmc_section, nLags = 500) + 
  facet_wrap(~ Parameter, ncol = 4, scales = "free")



