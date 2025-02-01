







{
  
  # Set working direcory ---------------
  try({  setwd("/home/enzocerullo/Documents/Work/PhD_work/R_packages/BayesMVP/examples")   }, silent = TRUE)
  try({  setwd("/home/enzocerullo/Documents/Work/PhD_work/R_packages/BayesMVP/examples")    }, silent = TRUE)
  #  options(repos = c(CRAN = "http://cran.rstudio.com"))
  
  # options -------------------------------------------------------------------------
  #  totalCores = 8
  rstan::rstan_options(auto_write = TRUE)
  options(scipen = 999)
  options(max.print = 1000000000)
  #  rstan_options(auto_write = TRUE)
  options(mc.cores = parallel::detectCores() / 2)
  
}



# - |  |  | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ 




{
  
  source("load_R_packages.R")
  source("load_data_binary_LC_MVP_sim.R")
  
}






# - |  |  | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ 


 

{
  
  
  n_covariates <- 1
  n_class = 2
  

{
  
 
  Phi_approx_piecewise <- function(x) { 
    
    y <- ifelse(abs(x) < 5, 
                pnorm(x), 
                plogis(x * 1.702)
    )
    
    return(y)
    
  }
  
  
  
  
      Model_type <- "MVP_LC"
 #     Model_type <- "LT_LC" ; LT_prior_on_mean_accuracy <-  1 
    #  Model_type <- "LT_LC" ; LT_prior_on_mean_accuracy <-  0 #  if 0, prior is set on medians directly (and 1 less Jacobian adjustment needed!)
      #  Model_type <- "MVP_standard"
  
     
        n_covariates_per_outcome_vec <- c(array(1, dim = c(n_tests, 1)))
        
     prior_b_shape_d <-  1.33 ; prior_b_scale_d <-    1.25    # ~ equiv. to  truncated-LKJ(1.5)
     prior_b_shape_nd <- 1.52 ; prior_b_scale_nd <-   0.633 # ~ equiv. to  truncated-LKJ(10)
     
     
  CI <- 0
  
  mvr_cholesky <- 0 # dont even need to use LKJ anymore thanks to generalising it with skewed-LKJ / shifted beta ??!?!?
  
  corr_force_positive <- 0
  
  corr_priors_LKJ_only = FALSE # if false, will use beta priors (as wlel as LKJ but just for Jacobian adjustment)
  # corr_priors_LKJ_only = TRUE  
  
 
        #    prior_lkj_d <- c( 23.15, 17.15    )  ;   prior_lkj_nd <-   c(  2.25, 0.950   )
             prior_lkj_d <- c( 23.15, 23.75    )  ;   prior_lkj_nd <-   c(  4.75, 3.750   )
      
      
  prior_lkj_skewed_diseased     <- prior_lkj_d
  prior_lkj_skewed_non_diseased <- prior_lkj_nd
  
  
  tailored_corr_priors <- FALSE
 # tailored_corr_priors <- TRUE
  
prior_for_skewed_LKJ_a <- array(0, dim = c(n_tests, n_tests, n_class)) # a > b for positive-skew
prior_for_skewed_LKJ_b <- array(0, dim = c(n_tests, n_tests, n_class))

bs_to_set_to_0 = array(0, dim = c(n_class, n_tests)) # for LT model

prior_a_mean <-   array(c(0,rep(0, n_tests-1),
                          0,rep(0, n_tests-1)), dim = c(n_class, n_tests, n_covariates+1))
prior_a_sd  <-    array(c(1,rep(1, n_tests-1),
                          1,rep(1, n_tests-1)), dim = c(n_class, n_tests, n_covariates+1))




if (DGP == 4) {
  prior_a_mean[1,1,1] <- -1.5
  prior_a_mean[2,1,1] <- + 1 
  prior_a_sd[1,1,1] <- 0.25 
  prior_a_sd[2,1,1] <- 0.35 
  
  # now set correlation priors 
  prior_for_skewed_LKJ_a[,,1] <- prior_lkj_skewed_non_diseased[1]
  prior_for_skewed_LKJ_b[,,1] <- prior_lkj_skewed_non_diseased[2]
  
  prior_for_skewed_LKJ_a[,,2] <- prior_lkj_skewed_diseased[1]
  prior_for_skewed_LKJ_b[,,2] <- prior_lkj_skewed_diseased[2]
  
  if (tailored_corr_priors == TRUE) {
    # test 1 is uncorrelated to other tests in BOTH classes (but CD is in both classes), so set this prior tighter for the "tailored" prior:
    prior_for_skewed_LKJ_a[1,,] <- prior_for_skewed_LKJ_a[,1,]  <- 1000 
    prior_for_skewed_LKJ_b[1,,] <- prior_for_skewed_LKJ_b[,1,]  <- 1000 
    
    bs_to_set_to_0[, 1] <- 1
    
    # furthermore, in class 2 (D+), tests 2 and 5 are uncorrelated: # cant really set this to 0 in LT without affecting other stuff 
    prior_for_skewed_LKJ_a[2,5,2] <- prior_for_skewed_LKJ_a[5,2,2]  <- 1000 
    prior_for_skewed_LKJ_b[2,5,2] <- prior_for_skewed_LKJ_b[5,2,2]  <- 1000 
 
 
  }
  
} else   { 
  prior_a_mean[1,1,1] <- - 2.25
  prior_a_mean[1,2,1] <- - 2.25
 # prior_a_mean[1,1,1] <- -  2.33
 #  prior_a_mean[1,2,1] <- - 2.33
  prior_a_sd[1,1,1] <- 0.30 
  prior_a_sd[1,2,1] <- 0.30 
  
  # prior_a_mean[2,1,1] <- + 0.4
  # prior_a_mean[2,2,1] <- + 0.85
  # prior_a_sd[2,1,1] <- 0.4
  # prior_a_sd[2,2,1] <- 0.4
  # 
  
  # now set correlation priors 
  prior_for_skewed_LKJ_a[,,1] <- prior_lkj_skewed_non_diseased[1]
  prior_for_skewed_LKJ_b[,,1] <- prior_lkj_skewed_non_diseased[2]
  
  prior_for_skewed_LKJ_a[,,2] <- prior_lkj_skewed_diseased[1]
  prior_for_skewed_LKJ_b[,,2] <- prior_lkj_skewed_diseased[2]
  
  if (tailored_corr_priors == TRUE) {
    
    if (DGP == 3) { # NON-UNIFORM corr structute in D+
      
      # 0 corr in D- class, so set tight prior around 0:
      prior_for_skewed_LKJ_a[,,1] <-  1000 
      prior_for_skewed_LKJ_b[,,1] <-  1000 
      
      # test 1 is uncorrelated to other tests in BOTH classes (but CD is in both classes), so set this prior tighter for the "tailored" prior:
      prior_for_skewed_LKJ_a[1,,] <- prior_for_skewed_LKJ_a[,1,]  <- 1000 
      prior_for_skewed_LKJ_b[1,,] <- prior_for_skewed_LKJ_b[,1,]  <- 1000 
      
      bs_to_set_to_0[, 1] <- 1
      
      
      # furthermore, in class 2 (D+), tests 2 and 5 are uncorrelated:
      prior_for_skewed_LKJ_a[2,5,2] <- prior_for_skewed_LKJ_a[5,2,2]  <- 1000 
      prior_for_skewed_LKJ_b[2,5,2] <- prior_for_skewed_LKJ_b[5,2,2]  <- 1000 
      
      
    } else if (DGP == 2) {  # UNIFORM corr structute in D+
      
      # 0 corr in D- class, so set tight prior around 0:
      prior_for_skewed_LKJ_a[,,1] <-  1000 
      prior_for_skewed_LKJ_b[,,1] <-  1000 
      
      bs_to_set_to_0[1, ] <- 1
      
    }
    
    
    
  }
  
}



n_pops <- 1


group <- rep(1, N)
 

 
}


#  |  ------------------------------------------------------------------------------------
{
  
  
  options(scipen = 99999)
  options(max.print = 1000000)
 options(mc.cores = 16)
  numerical_diff_e = 0.01
}



{

# ## "non-informative"
  prior_mean_vec = t(prior_a_mean[,,1])
  prior_sd_vec = t(prior_a_sd[,,1])

  


mvr_cholesky <- FALSE
corr_normal_prior_sd <- list()
corr_normal_prior_sd[[1]] <-  array(0.25, dim = c(n_tests, n_tests))
corr_normal_prior_sd[[2]] <-  array(0.25, dim = c(n_tests, n_tests))
 
 
  
  
}


 

 #  corr_param_Archakov <- TRUE ;    use_AD_for_chol_derivs <- TRUE  # this doesn't work 
 #  corr_param_Archakov <- FALSE ;   use_AD_for_chol_derivs <- TRUE  # this seems to work fine
    corr_param_Archakov <- FALSE ;   use_AD_for_chol_derivs <- FALSE # this seems to work fine
 # 
  #  for (df_i in sims_start:sims_end) {
  
  
  df_i <- 123
  
 
  
  prior_only <- FALSE
 

   #     corr_param <- "latent_trait"
        corr_param <- "Chol_Schur"
  ###   corr_param <- "Chol_Nump"
   ###     corr_param <- "Arch"
    #   corr_param <- "Chol_Stan"
        
        if (Model_type == "LT_LC")   corr_param <- "latent_trait"
    
    corr_pos_offset <- 0 
   # corr_pos_offset <- -0.05 
    
    
    
         #   corr_force_positive = FALSE
           corr_force_positive = TRUE

    # 
    # if (corr_force_positive == FALSE) {
    #            lkj_cholesky_eta = c(10, 1.5)
    #      #    lkj_cholesky_eta = c(24, 4)
    # } else if (corr_force_positive == TRUE)  {
    #       #   lkj_cholesky_eta = c(15.5, 1.75)
    #      #     lkj_cholesky_eta = c(24, 5)
    # }
    # #     
    #   
    
 
    
    experiment <- "algorithm_binary"
   # experiment <- "simulation_binary"
    

    
    
    prior_sd_vec[,] = 1
    prior_mean_vec[,] = 0
       
       
       known_values_indicator_list <- known_values_list <- list()
       
       for (c in 1:n_class) {
         known_values_indicator_list[[c]] <- diag(n_tests)
         known_values_list[[c]] <- diag(n_tests)
       }
       
       
          tailored_corr_priors <- FALSE
     #   tailored_corr_priors <- TRUE
       
       if (tailored_corr_priors == TRUE) {
           if (DGP == 4) {
               known_values_indicator_list[[1]][1,2:5] <-    known_values_indicator_list[[1]][2:5, 1] <- 1
               known_values_indicator_list[[1]][2,5] <-    known_values_indicator_list[[1]][5, 2] <- 1
               known_values_indicator_list[[2]][1,2:5] <-    known_values_indicator_list[[2]][2:5, 1] <- 1
               known_values_indicator_list[[2]][2,5] <-    known_values_indicator_list[[2]][5, 2] <- 1
           }
           if (DGP == 3) {
             known_values_indicator_list[[1]][,]   <- 1
             known_values_indicator_list[[1]][,]   <- 1
             known_values_indicator_list[[2]][1,2:5] <-    known_values_indicator_list[[2]][2:5, 1] <- 1
             known_values_indicator_list[[2]][2,5] <-    known_values_indicator_list[[2]][5, 2] <- 1
           }
           if (DGP == 2) {
             known_values_indicator_list[[1]][,]   <- 1
             known_values_indicator_list[[1]][,]   <- 1
           } 
           if (DGP == 1) {
             known_values_indicator_list[[1]][,]   <- 1
             known_values_indicator_list[[1]][,]   <- 1
             known_values_indicator_list[[2]][,]   <- 1
             known_values_indicator_list[[2]][,]   <- 1
           } 
       } else { 
         for (c in 1:n_class) {
           known_values_indicator_list[[c]] <- diag(n_tests)
           known_values_list[[c]] <- diag(n_tests)
         }
       }
       
       
      if (N == 500) {
          L_if_manual = 25
          adapt_interval_width <- round(n_burnin/5, 0)
          adapt_interval_width <- 50
          num_chunks <-  1
      }
      
      if (N == 2500)  {  
        L_if_manual = 60
      #     adapt_interval_width <- round(n_burnin/10, 0)
        adapt_interval_width <- round(n_burnin/5, 0)
        num_chunks <- 1
      }
          
          corr_prior_beta <-  0
          
          
          {
            LT_b_priors_shape <- array(1, dim = c(n_class, n_tests))
            LT_b_priors_scale <- array(1, dim = c(n_class, n_tests))

            
            
              LT_b_priors_shape[1, ] <-    prior_b_shape_nd
              LT_b_priors_scale[1, ] <-    prior_b_scale_nd
              
              # LT prior set 1
            LT_b_priors_shape[2, ] <-       prior_b_shape_d
            LT_b_priors_scale[2, ] <-       prior_b_scale_d
          
              # LT prior set 2
              # .........
              
     
                LT_known_bs_indicator <-  LT_known_bs_values <-  array(0, dim = c(n_class, n_tests))
                
                if (tailored_corr_priors == TRUE) {
                   LT_known_bs_indicator[, 1] <- 1
                   LT_known_bs_values[, 1] <- 0.00001
                }
          }
          
          
          
          if (experiment == "algorithm_binary") { 
            
              # corr_param <- "Chol_Stan"
              corr_param <- "Chol_Schur"
              if (Model_type == "LT_LC")   corr_param <- "latent_trait"
              # lkj_cholesky_eta = c(10, 2)
              lkj_cholesky_eta = c(12, 3)
              corr_force_positive = FALSE
              tailored_corr_priors <- FALSE
              
             

              # if (N == 64000) num_chunks <-  128
              
              df_i = 123

              prior_mean_vec[1,1] =  -2.10 ;    prior_sd_vec[1,1] = 0.45
              prior_mean_vec[1,2] = +0.40 ;   prior_sd_vec[1,2] = 0.375
              
              n_runs <- 3
               
              L_if_manual <- 10
              
            #  num_chunks <-  1
          }
          
          

         

          
          
       if (Model_type == "MVP_LC") {
              beta_prior_mean_vec <- beta_prior_sd_vec <-  list()
              for (c in 1:n_class) {
                beta_prior_mean_vec[[c]] <- array(0.0, dim = c(1, n_tests))
                beta_prior_sd_vec[[c]] <- array(1.0, dim = c(1, n_tests))
              }
              
              beta_prior_mean_vec[[1]][1, 1] <- -2.10
              beta_prior_mean_vec[[2]][1, 1] <- 0.40
              
              beta_prior_sd_vec[[1]][1, 1] <- 0.450
              beta_prior_sd_vec[[2]][1, 1] <- 0.375
              
              n_covariates_per_outcome_vec_1 <- n_covariates_per_outcome_vec
              n_covariates_per_outcome_vec <- list(n_covariates_per_outcome_vec_1, n_covariates_per_outcome_vec_1)
       }
          
          
 
     
}

           






