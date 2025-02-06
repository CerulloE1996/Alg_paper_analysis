

# - | ----------  run models using Stan --------------------------------------------------------------------


# library(polycor)
# library(mvtnorm)
# rho <- matrix(NA, ntest, ntest)
# for(s in 1:ntest){
#   for(t in 1:ntest){
#     rho[s,t] <- polychor(y[,s], y[,t])
#   }
# }
# rho






{
  
  N <-  500
  
  if (N == 500)     dataset_index = 1
  if (N == 1000)    dataset_index = 2
  if (N == 2500)    dataset_index = 3
  if (N == 5000)    dataset_index = 4
  if (N == 12500)   dataset_index = 5
  if (N == 25000)   dataset_index = 6
  
  y = y_master_list_seed_123_datasets[[dataset_index]]
  
  
  N_sims <- 123
  
  
  #  model_type <- "LT" ;  LT_prior_on_mean_accuracy = 1 ;   Hayley_LT_param <- TRUE ######## 
  #  model_type <- "LT" ;  LT_prior_on_mean_accuracy = 0
  prior_b_shape_nd <- 1.52 ; prior_b_scale_nd <-   0.633 # ~ equiv. to  truncated-LKJ(10)
  prior_b_shape_d <-  1.33 ; prior_b_scale_d <-    1.25    # ~ equiv. to  truncated-LKJ(1.5)
  
  
  
  model_type <- "LC_MVP"
  
  
  
   #fully_vectorised <- 0 ;  GHK_comp_method <- 2 ;    handle_numerical_issues <- 0 ;   overflow_threshold <- +2000 ;   underflow_threshold <- -2000   # Goodrich (big for-loop)
   #   fully_vectorised <- 1  ;  GHK_comp_method <- 2 ;       handle_numerical_issues <- 0 ;     overflow_threshold <- +2000 ;    underflow_threshold <- -2000 
#  fully_vectorised <- 1  ;   GHK_comp_method <- 2 ;       handle_numerical_issues <- 1 ;     overflow_threshold <- +0.0001;    underflow_threshold <- -0.0001  ##
   fully_vectorised <- 1  ;   GHK_comp_method <- 2 ;       handle_numerical_issues <- 1 ;     overflow_threshold <- +5 ;    underflow_threshold <- -5  #
  
   # GHK_comp_method <- 1 ;       handle_numerical_issues <- 0 ; overflow_threshold <- 999999 ;    underflow_threshold <- 999999
 
   # Phi_type <- 2 # using Phi_approx and inv_Phi_approx - in  Stan these are slower than Phi() and inv_Phi() !!!!!!!! (as log/exp are slow)
    Phi_type <- 1 # using Phi and inv_Phi
  
  
  #   LT_find_b_priors_using_density <- TRUE
  LT_find_b_priors_using_density <- FALSE
  
  #     corr_param <- "Cholesky" ; prior_lkj <- c(10, 2) ;  corr_prior_beta =  0 ;  corr_force_positive <-  0  ;  prior_param_3 <- 0 ; uniform_indicator <- 0 
  #  corr_param <- "Archakov"
    corr_param <- "Sean" ; prior_lkj <- c(12, 3) ;  corr_prior_beta =  0 ;  corr_force_positive <-  0  ;  prior_param_3 <- 0 ; uniform_indicator <- 0 
  #  corr_param <- "Sean" ; prior_lkj <- c(10, 2) ;  corr_prior_beta =  0 ;  corr_force_positive <-  0  ;  prior_param_3 <- 0 ; uniform_indicator <- 0 
  #   corr_param <- "Sean" ; prior_lkj <- c(23, 7) ;  corr_prior_beta =  0 ;  corr_force_positive <-  0  ;  prior_param_3 <- 0 ; uniform_indicator <- 0 
  
    
  #  corr_param <- "Cholesky"
    
    
  CI <- 0
  
  
  prior_only <-  0
  
  mvr_cholesky <- 0 # dont even need to use LKJ anymore thanks to generalising it with skewed-LKJ / shifted beta ??!?!?
  
  #  corr_force_positive <-  0 ;    prior_lkj <- c(  10, 10) ;    corr_prior_beta =  0 ; prior_param_3 <- 0 ; uniform_indicator <- 0  ; gamma_indicator <- 0 ;  skew_norm_indicator <- 0 
  #  corr_force_positive <-  0 ;    prior_lkj <- c(  24, 24) ;    corr_prior_beta =  0 ; prior_param_3 <- 0  ; uniform_indicator <- 0  ; gamma_indicator <- 0 ;  skew_norm_indicator <- 0 
  #   corr_force_positive <-  1 ;    prior_lkj <- c(  10, 10) ;    corr_prior_beta =  0 ; prior_param_3 <- 0  ; uniform_indicator <- 0  ; gamma_indicator <- 0 ;  skew_norm_indicator <- 0 
  #   corr_force_positive <-  1 ;    prior_lkj <- c(  24, 24) ;    corr_prior_beta =  0 ; prior_param_3 <- 0  ; uniform_indicator <- 0  ; gamma_indicator <- 0 ;  skew_norm_indicator <- 0 
  #   corr_force_positive <-  0 ;    prior_lkj <- c(  23.15, 17.15) ;    corr_prior_beta =  1 ; prior_param_3 <- 0  ; uniform_indicator <- 0  ; gamma_indicator <- 0 ;  skew_norm_indicator <- 0 
  #  corr_force_positive <-  0 ;    prior_lkj <- c(  26.25, 23.75) ;    corr_prior_beta =  1 ; prior_param_3 <- 0  ; uniform_indicator <- 0  ; gamma_indicator <- 0 ;  skew_norm_indicator <- 0 
  # 
  #   corr_force_positive <-  0 ;    prior_lkj <- c(  1.5, 1.5) ;    corr_prior_beta =  0 ; prior_param_3 <- 0  ; uniform_indicator <- 0  ; gamma_indicator <- 0 ;  skew_norm_indicator <- 0 
  #    corr_force_positive <-  0 ;    prior_lkj <- c(  4, 4) ;    corr_prior_beta =  0 ; prior_param_3 <- 0  ; uniform_indicator <- 0  ; gamma_indicator <- 0 ;  skew_norm_indicator <- 0 
  #    corr_force_positive <-  1 ;    prior_lkj <- c(  1.5, 1.5) ;    corr_prior_beta =  0 ; prior_param_3 <- 0  ; uniform_indicator <- 0  ; gamma_indicator <- 0 ;  skew_norm_indicator <- 0 
  #    corr_force_positive <-  1 ;    prior_lkj <- c(  4, 4) ;    corr_prior_beta =  0 ; prior_param_3 <- 0  ; uniform_indicator <- 0  ; gamma_indicator <- 0 ;  skew_norm_indicator <- 0 
  #      corr_force_positive <-  0 ;    prior_lkj <- c(  2.25, 0.95) ;    corr_prior_beta =  1; prior_param_3 <- 0  ; uniform_indicator <- 0  ; gamma_indicator <- 0 ;  skew_norm_indicator <- 0 
  #     corr_force_positive <-  0 ;    prior_lkj <- c(  4.75, 3.75) ;    corr_prior_beta =  1 ; prior_param_3 <- 0   ; uniform_indicator <- 0  ; gamma_indicator <- 0 ;  skew_norm_indicator <- 0
  
  corr_prior_norm = 0
  
  #    prior_lkj <- c( 12, 1.5 ) 
  #    prior_lkj <- c( 24, 4) 
  
  #   prior_lkj <- c(   41.25 ,  41.25  / 1.375 ) 
  # prior_lkj <- c(  37.125 ,  37.125   / 1.375 ) 
  # eta <- 27.5
  #  prior_lkj <- c( eta, eta/1.225) * 0.925
  #  prior_lkj
  
  
  # prior_lkj
  
  
  
  gamma_indicator <-  0
  skew_norm_indicator <- 0
  
  
  #   prior_lkj <- c(1.52,  0.633) ;  corr_prior_beta =  0 ;  corr_force_positive <-  1 ; prior_param_3 <- 3.83 ; uniform_indicator <- 0 ; gamma_indicator <- 0 ; skew_norm_indicator <- 0  # D- LKJ(10)
  #   prior_lkj <- c(1.33,  1.25) ;  corr_prior_beta =  0 ;  corr_force_positive <-  1 ; prior_param_3 <- 0 ; uniform_indicator <- 0 ; gamma_indicator <- 1 ; skew_norm_indicator <- 0  # D+ LKJ(1.5)
  #    prior_lkj <- c(1.59,  0.468) ;  corr_prior_beta =  0 ;  corr_force_positive <-  1 ; prior_param_3 <- 6.32 ; uniform_indicator <- 0 ; gamma_indicator <- 0 ; skew_norm_indicator <- 1  # D+ LKJ(24)
  #    prior_lkj <- c(1.45,  0.881) ;  corr_prior_beta =  0 ;  corr_force_positive <-  1 ; prior_param_3 <- 3.33 ; uniform_indicator <- 0 ; gamma_indicator <- 0 ; skew_norm_indicator <- 1  # D- LKJ(4)
  # #   prior_lkj <- c(1,  1) ;  corr_prior_beta =  0 ;  corr_force_positive <-  1 ; prior_param_3 <- 0 ; uniform_indicator <- 0 ; gamma_indicator <- 1 ; skew_norm_indicator <- 0   
  #    prior_lkj <- c(5, 5) ;  corr_prior_beta =  0 ;  corr_force_positive <-  1 ; prior_param_3 <- 5 ; uniform_indicator <- 1 ; gamma_indicator <- 0 ; skew_norm_indicator <- 0   
  
  ####  prior_lkj <- c(2.28,  3.333) ;  corr_prior_beta =  0 ;  corr_force_positive <-  1
  ####  prior_lkj <- c(2.56,  5.31) ;  corr_prior_beta =  0 ;  corr_force_positive <-  1
  ###    prior_lkj <- c(2.03,  4.80) ;  corr_prior_beta =  0 ;  corr_force_positive <-  1
  ####  prior_lkj <- c( 1, 1) ;  corr_prior_beta =  0 ;  corr_force_positive <-  1
  ####    prior_lkj <- c( 5000, 5000)  ;  corr_prior_beta =  0 ;  corr_force_positive <-  1      # Unif(0, 5) prior
  
  
  #   corr_prior_beta = 1 ;    corr_prior_norm = 1  ;   prior_lkj <- c(   0 , 0.25   )
  
  
  prior_lkj_skewed_diseased <-   prior_lkj
  prior_lkj_skewed_non_diseased <-  prior_lkj
  
  
  
  #  tailored_corr_priors <- TRUE # priors which are more consistent with posteior (should recover parameters better, especially for lower N and in the smaller latent class)
  tailored_corr_priors <- FALSE # priors which are more consistent with posteior (should recover parameters better, especially for lower N and in the smaller latent class)
  
  
  n_class <- 2
  n_covariates <- 0
  n_tests <-   n_tests 
  n_ordinal_tests <- 0
  n_binary_tests <- n_tests
  Thr =   rep(1, n_tests)
  #N <- nrow(y)
  
  prior_a_mean <-   array(c(0,rep(0, n_tests-1),
                            0,rep(0, n_tests-1)), dim = c(n_class, n_tests, n_covariates+1))
  prior_a_sd  <-    array(c(1,rep(1, n_tests-1),
                            1,rep(1, n_tests-1)), dim = c(n_class, n_tests, n_covariates+1))
  
  
  #prior_a_mean[c,t,cov]
  
  
  
  # intercepts / coeffs prior means
  prior_a_mean[1,1,1] <- -2.10
  prior_a_sd[1,1,1] <- 0.45
  
  prior_a_mean[2,1,1] <- +0.40
  prior_a_sd[2,1,1] <-  0.375
  
  
  # prior_a_mean[,,] <- 0
  # prior_a_mean[1,1:3,1] <- - 3.09 # Sp prior mean ~ Wang et al data 
  
  prior_for_skewed_LKJ_a <- array(0, dim = c(n_tests, n_tests, n_class)) # a > b for positive-skew
  prior_for_skewed_LKJ_b <- array(0, dim = c(n_tests, n_tests, n_class))
  
  bs_to_set_to_0 = array(0, dim = c(n_class, n_tests)) # for LT model
  
  
  
  
  
  if (tailored_corr_priors == FALSE) {
    
    prior_for_skewed_LKJ_a <- aperm(prior_for_skewed_LKJ_a, c(3, 1, 2))
    prior_for_skewed_LKJ_b <- aperm(prior_for_skewed_LKJ_b, c(3, 1, 2))
    
  }
  
  
  # prior_a_sd[,,] <- 1
  # prior_a_sd[1,1:3,1] <-  0.69  # Sp prior sd ~ Wang et al data 
  
  
  prior_b_shape <- prior_b_scale <- array(dim = c(n_class, n_tests))
  prior_b_shape[1,] <- prior_b_shape_nd  # D-
  prior_b_scale[1,] <- prior_b_scale_nd  # D-
  prior_b_shape[2,] <- prior_b_shape_d  # D+
  prior_b_scale[2,] <- prior_b_scale_d  # D+
  
  # plot priors for intercepts / coeffs
  MVP_prior_dist_1 <- MVP_prior_dist_2 <- MVP_prior_dist_3 <- c()
  
  for (i in 1:100000) {
    MVP_prior_dist_1[i] <- pnorm(rnorm(n = 1, 0, 1))
    MVP_prior_dist_2[i] <- pnorm(rnorm(n = 1,   prior_a_mean[2,1,1] , prior_a_sd[2,1,1] ))
    MVP_prior_dist_3[i] <- pnorm(rnorm(n = 1,   prior_a_mean[2,2,1] , prior_a_sd[2,2,1] ))
  }
  
  # plot(density( (MVP_prior_dist_1)))
  
  # 
  
  
  # require(sn)
  # 
  # 
  # par(mfrow = c(1, 1))
  # 
  # 
  # 
  # shape = 3.3*1 ; shape
  # scale = 0.143*1  ; scale 
  # 
  # 
  # a <- 0.6
  # shape = 3.3*2.3*a ; shape
  # scale = 0.143*0.80*a  ; 1 / scale 
  #  
  # shape = 4.61*1.2
  # scale =  1/(10.8*0.9)
  # 
  # 
  # shape = 1.47
  # scale =    (0.749/1.47)
  
  
  
  
  {
    
    
    
    #corr_prior_dist_LKJ_standard_1 <-  corr_prior_dist_LKJ_standard_2 <- corr_prior_dist_LKJ_standard_3 <- corr_prior_dist_LKJ_standard_4 <- c()
    corr_prior_dist_LKJ_4 <-  corr_prior_dist_LKJ_6 <- corr_prior_dist_LKJ_8 <- corr_prior_dist_LKJ_10 <- c()
    corr_prior_dist_LKJ_gr0_LKJ_standard_1 <-  corr_prior_dist_LKJ_gr0_LKJ_standard_2 <- corr_prior_dist_LKJ_gr0_LKJ_standard_3 <- corr_prior_dist_LKJ_gr0_LKJ_standard_4 <- c()
    corr_prior_dist_LT_1 <- corr_prior_dist_LT_2 <- corr_prior_dist_LT_3 <-  c()
    corr_prior_dist_gr0_Pareto_1 <- corr_prior_dist_Pareto_2 <- c()
    corr_prior_dist_to_mimic <- c()
    
    dim_choose_2 <- choose(5, 2)
    corr_mtx <- array(1, dim = c(5, 5))
    
    for (i in 1:10^6) {
      
      
      # corrs <- rlkjcorr(n = 1, K = 5, eta = 4)  ;    val <- corrs[1,2]
      # if (any(corrs < 0))      corr_prior_dist_LKJ_4[i] <-  NA
      # else                     corr_prior_dist_LKJ_4[i] <-  val
      
      #
      #           eta = 1.5
      #           lkj_marginal_shape <- eta - 1 + (n_tests/2)
      #           corrs <-  2 * rbeta(n = dim_choose_2, shape1 =  lkj_marginal_shape , shape2 = lkj_marginal_shape  ) - 1
      #           rho <-  corrs[1]
      #
      #
      # #
      # #           corr_mtx <-   lower_tri.assign(corr_mtx, v = corrs)
      # #           corr_mtx <-   upper_tri.assign(corr_mtx, v = corrs)
      # #
      # #           pd_indicator <- 0
      # #
      # #           try({
      # #             L_corr_mtx  =   t(chol(corr_mtx))
      # #             pd_indicator  = 1
      # #           }, silent = TRUE)
      # #
      #
      #
      #
      #          #   if (pd_indicator == 1)   {
      #          if (is.positive.semi.definite( as.matrix(forceSymmetric(corr_mtx))))  {
      #            if (any(corrs < 0))      corr_prior_dist_to_mimic[i] <-  NA
      #            else                     corr_prior_dist_to_mimic[i] <-  rho
      #           } else {
      #             corr_prior_dist_to_mimic[i] <-  NA
      #           }
      
      #
      
      
      # corrs <- rlkjcorr(n = 1, K = 5, eta = 6)  ;    val <- corrs[1,2]
      # if (any(corrs < 0))      corr_prior_dist_LKJ_6[i] <-  NA
      # else                     corr_prior_dist_LKJ_6[i] <-  val
      
      # corrs <- rlkjcorr(n = 1, K = 5, eta = 8)  ;    val <- corrs[1,2]
      # if (any(corrs < 0))      corr_prior_dist_LKJ_8[i] <-  NA
      # else                     corr_prior_dist_LKJ_8[i] <-  val
      
      # corrs <- rlkjcorr(n = 1, K = 5, eta = 10)  ;    val <- corrs[1,2]
      # if (any(corrs < 0))      corr_prior_dist_LKJ_10[i] <-  NA
      # else                     corr_prior_dist_LKJ_10[i] <-  val
      
      # val <-  2 * rbeta(n = 1, shape1 = 4 - 1 + (n_tests/2)   , shape2 = 4  - 1 + (n_tests/2)   ) - 1
      #
      #
      # if  (val > 0 )           corr_prior_dist_LKJ_skewed_1[i] <-  val
      # else                     corr_prior_dist_LKJ_skewed_1[i] <-  NA
      #
      # eta_a <-  4; #    eta_b <- eta_a/1.8
      #
      #
      # eta <- 4
      #
      
      #     #  scale <-  1 ;  shape <- 6  ;  corr_prior_dist_gr0_Pareto_1[i] <-   exp(rlogis(n = 1, location = log(scale), scale = 1/shape)) - 1
      #
      #
      #           b_1 <-   rgamma(n = 1, shape = 1.10, scale = (1.52/1.10)) #    truncdist::rtrunc(n = 1, spec = "gamma", a = 0, b = 4 , shape = shape, scale = scale)
      #           b_2 <-   rgamma(n = 1, shape = 1.10, scale = (1.52/1.10)) #    truncdist::rtrunc(n = 1, spec = "gamma", a = 0, b = 4 , shape = shape, scale = scale)
      #
      #           rho <- (b_1*b_2)/(sqrt(1+b_1^2)*sqrt(1+b_2^2))
      #
      #           #
      #           if ( rho < 1) {
      #             corr_prior_dist_LT_1[i] <-  rho
      #           } else {
      #             corr_prior_dist_LT_1[i] <- NA
      #           }
      
      # # b_1 <- rsn(n = 1, xi = 0.0250 , omega = 0.674 , alpha =  20.1)
      # # b_2 <- rsn(n = 1, xi = 0.0250 , omega = 0.674 , alpha =  20.1)
      #
      # # b_1 <- rsn(n = 1, xi =  1.54  , omega = 2.33  , alpha =   14.8 )
      # # b_2 <- rsn(n = 1, xi =  1.54  , omega = 2.33  , alpha =   14.8 )
      #
      # rho <- (b_1*b_2)/(sqrt(1+b_1^2)*sqrt(1+b_2^2))
      #
      # #
      # if ( rho < 1) {
      #   corr_prior_dist_LT_2[i] <-  rho
      # } else {
      #   corr_prior_dist_LT_2[i] <- NA
      # }
      #
      b_1 <-   rweibull(n = 1, shape =  prior_b_shape_d, scale =   1 / prior_b_scale_d) #    truncdist::rtrunc(n = 1, spec = "gamma", a = 0, b = 4 , shape = shape, scale = scale)
      b_2 <-   rweibull(n = 1, shape =  prior_b_shape_d, scale =   1 / prior_b_scale_d) #    truncdist::rtrunc(n = 1, spec = "gamma", a = 0, b = 4 , shape = shape, scale = scale)
      #
      rho <- (b_1*b_2)/(sqrt(1+b_1^2)*sqrt(1+b_2^2))
      #
      # #
      if ( rho < 1) {
        corr_prior_dist_LT_3[i] <-  rho
      } else {
        corr_prior_dist_LT_3[i] <- NA
      }
      #
      
      
      # b_1_div_5 <-  rbeta(n = 1, 1.90, 20.7)
      # b_1 <- 5 * b_1_div_5
      # b_2_div_5 <-  rbeta(n = 1, 1.90, 20.7)
      # b_2 <- 5 * b_2_div_5
      
      
      # 
      # 
      # 
      # 
      # 
    }
    # 
    # 
    # #   dist_to_mimic <- head(dplyr::filter( df_prior_combined_LKJ_d, model == "MVP", force_positive == 1, prior_param_1 == 1.5)$draws, 100000)
    # 
    #    #  plot(density(corr_prior_dist_LT_1, na.rm = T), col = "green", lwd = 3 )
    #   #
    #     # plot(density(   head(dplyr::filter( df_prior_combined_LKJ_d, model == "MVP", force_positive == 1, prior_param_1 == 1.5)$draws, 100000)) , col = "red", lwd = 3)
    #     # lines(density(corr_prior_dist_LT_2, na.rm = T), col = "blue", lwd = 3 )
    # 
    #   #  plot(density(  dist_to_mimic  ) , col = "black", lwd = 3, ylim = c(0, 6))
    #      plot(density(   corr_prior_dist_to_mimic[!is.na(corr_prior_dist_to_mimic)] ) , col = "black", lwd = 3, ylim = c(0, 6))
    #     # lines(density(  corr_prior_dist_LT_1  ) , col = "red", lwd = 3)  # gamma
    #     # lines(density(corr_prior_dist_LT_2, na.rm = T), col = "blue", lwd = 3 ) # skew-normal
    plot(density(corr_prior_dist_LT_3, na.rm = T), col = "orange", lwd = 3 ) # wb
    # 
    # # plot(density(corr_prior_dist_LKJ_standard_1), col = "green", ylim = c(0, 3), lwd = 3, xlim = c(-2, 2))
    # # #lines(density(corr_prior_dist_LKJ_standard_2), col = "green", lwd = 3)
    # # #lines(density(corr_prior_dist_LKJ_standard_3), col = "green", lwd = 3)
    # # #lines(density(corr_prior_dist_LKJ_standard_4), col = "green", lwd = 3)
    # #
    # # lines(density(corr_prior_dist_LKJ_gr0_LKJ_standard_1), col = "green", lwd = 3, lty = 2)
    # # # lines(density(corr_prior_dist_LKJ_gr0_LKJ_standard_2), col = "green", lwd = 3, lty = 2)
    # # # lines(density(corr_prior_dist_LKJ_gr0_LKJ_standard_3), col = "green", lwd = 3, lty = 2)
    # # #lines(density(corr_prior_dist_LKJ_gr0_LKJ_standard_4), col = "green", lwd = 3, lty = 2)
    # #
    # #
    # #   lines(density(corr_prior_dist_LKJ_skewed_1), col = "blue", lwd = 3)
    # #  # lines(density(corr_prior_dist_LKJ_skewed_2), col = "blue", lwd = 3)
    # # #
    # # #
    # #   lines(density(corr_prior_dist_gr0_Pareto_1), col = "black", lty = 2, lwd = 3)
    # 
    # #
    # 
  }
  # 
  # round(median(corr_prior_dist_to_mimic, na.rm = TRUE), 2) ; round(quantile( corr_prior_dist_to_mimic ,  probs = c(0.025, 0.975), na.rm = TRUE), 2) # prior to copy (black)
  # 
  # corr_prior_dist_to_mimic[!is.na(corr_prior_dist_to_mimic)]
  # 
  # #  
  # # 
  # #   # # round(median(dist_to_mimic), 2) ; round(quantile( dist_to_mimic ,  probs = c(0.025, 0.975)), 2) # prior to copy (black)
  # #   # median(corr_prior_dist_LT_1, na.rm = TRUE) ; round(quantile(corr_prior_dist_LT_1, na.rm = TRUE, probs = c(0.025, 0.975)), 2)   # gamma (red)
  # #   # median(corr_prior_dist_LT_2, na.rm = TRUE) ; round(quantile(corr_prior_dist_LT_2, na.rm = TRUE, probs = c(0.025, 0.975)), 2)   # skew-normal (blue)
  median(corr_prior_dist_LT_3, na.rm = TRUE) ; round(quantile(corr_prior_dist_LT_3, na.rm = TRUE, probs = c(0.025, 0.975)), 2)   # WB (orange)
  # #   # 
  # #    # # # 
  # #  #   median(corr_prior_dist_gr0_Pareto_1) ; round(quantile(corr_prior_dist_gr0_Pareto_1, probs = c(0.025, 0.975)), 2)  
  # # # # 
  # # #   median(corr_prior_dist_LKJ_gr0_LKJ_standard_1, na.rm = TRUE) ; round(quantile(corr_prior_dist_LKJ_gr0_LKJ_standard_1, probs = c(0.025, 0.975), na.rm = TRUE ), 2) 
  # # # # 
  # 
  #   corr_prior_dist_LKJ_skewed_1 <- corr_prior_dist_LKJ_skewed_1[!is.na(corr_prior_dist_LKJ_skewed_1)]
  #   median(corr_prior_dist_LKJ_skewed_1, na.rm = TRUE) ; round(quantile(corr_prior_dist_LKJ_skewed_1,  na.rm = TRUE, probs = c(0.025, 0.975)), 2) 
  # # median(corr_prior_dist_LKJ_skewed_2) ; round(quantile(corr_prior_dist_LKJ_skewed_2, probs = c(0.025, 0.975)), 2) 
  #  #  median(corr_prior_dist_LKJ_standard_1) ; round(quantile(corr_prior_dist_LKJ_standard_1, probs = c(0.025, 0.975)), 2) 
  # 
  #  
  #    
  
  
  
  #rlogis(n = 1, location = 2, scale = 1)
  
  length(corr_prior_dist_LKJ_4)
  corr_prior_dist_LKJ_4 <- corr_prior_dist_LKJ_4[!is.na(corr_prior_dist_LKJ_4)]
  median(corr_prior_dist_LKJ_4, na.rm = TRUE) ; round(quantile(corr_prior_dist_LKJ_4,  na.rm = TRUE, probs = c(0.025, 0.50, 0.975)), 3) 
  
  samps <-  2 * rbeta(n = 10000, shape1 =  1.37, shape2 =   3.46) - 1
  samps <- ifelse(samps > 0 , samps, NA)
  
  samps <-   rbeta(n = 10000, shape1 =  1.37, shape2 =   3.46)  
  quantile(samps, na.rm = T,  probs = c(0.025, 0.50, 0.975))
  
  
  plot(density(  samps , na.rm=T ))
  quantile(samps, na.rm = T,  probs = c(0.025, 0.50, 0.975))
  
  
  
  corr_prior_dist_LKJ_6 <- corr_prior_dist_LKJ_6[!is.na(corr_prior_dist_LKJ_6)]
  median(corr_prior_dist_LKJ_6, na.rm = TRUE) ; round(quantile(corr_prior_dist_LKJ_6,  na.rm = TRUE, probs = c(0.025, 0.975)), 3) 
  
  dens_samps <-  2 * rbeta(n = 10000, shape1 =  -1 + n_tests/2 + 4 - 0.000773, shape2 =  -1 + n_tests/2 + 4 - 0.676) - 1
  dens_samps <- ifelse(dens_samps > 0 , dens_samps, NA)
  
  
  
  corr_prior_dist_LKJ_8 <- corr_prior_dist_LKJ_8[!is.na(corr_prior_dist_LKJ_8)]
  median(corr_prior_dist_LKJ_8, na.rm = TRUE) ; round(quantile(corr_prior_dist_LKJ_8,  na.rm = TRUE, probs = c(0.025, 0.975)), 3) 
  
  dens_samps <-  2 * rbeta(n = 10000, shape1 =  -1 + n_tests/2 + 4 - 0.000773, shape2 =  -1 + n_tests/2 + 4 - 0.676) - 1
  dens_samps <- ifelse(dens_samps > 0 , dens_samps, NA)
  
  # 
  # 
  # corr_prior_dist_LKJ_10 <- corr_prior_dist_LKJ_10[!is.na(corr_prior_dist_LKJ_10)]
  # median(corr_prior_dist_LKJ_10, na.rm = TRUE) ; round(quantile(corr_prior_dist_LKJ_10,  na.rm = TRUE, probs = c(0.025, 0.975)), 3) 
  # 
  # dens_samps <-  2 * rbeta(n = 10000, shape1 =  -1 + n_tests/2 + 4 - 0.000773, shape2 =  -1 + n_tests/2 + 4 - 0.676) - 1
  # dens_samps <- ifelse(dens_samps > 0 , dens_samps, NA)
  
  
  
  
  n_pops <- 1
  
  
  group <- rep(1, N)
  
  
  stan_data_list <- list()
  
  
  n_covs_per_outcome = array(1, dim = c(n_class, n_tests))
  
  
    n_covariates_total_nd  =    (sum( (n_covs_per_outcome[1,])));
    n_covariates_total_d   =     (sum( (n_covs_per_outcome[2,])));
    n_covariates_total  =       n_covariates_total_nd + n_covariates_total_d;
  
    k_choose_2   = (n_tests * (n_tests - 1)) / 2;
    km1_choose_2 =  ((n_tests - 1) * (n_tests - 2)) / 2;
  
  for (i in 1:N_sims) {
    
    
    # if (prior_only == 0 ) { 
    stan_data = list(  N =   (N), # length(corr_prior_dist_LKJ_4), #  N, 
                       D = n_tests, 
                       #  y_marginal_corr_data = corr_prior_dist_LKJ_4,
                       # N =  2000,
                       #  n_corr_mats = 5000,
                       # pd_indicator_vec = pd_indicator_vec,
                       # Omega_data = Omega_data,
                       rough_approx = 0, 
                       prior_param_3 = prior_param_3,
                       uniform_indicator = uniform_indicator,
                       gamma_indicator = gamma_indicator,
                       skew_norm_indicator = skew_norm_indicator,
                       #     rho =   head(dist_to_mimic, 2000), 
                       #  rho =   head( dplyr::filter( df_prior_combined_LKJ_d, model == "MVP", force_positive == 1, prior_param_1 == 4)$draws, 2000) ,
                       #     b_2 = runif(n = 5000, min = 0, max = 5),
                       corr_force_positive = corr_force_positive,
                       mvr_cholesky = mvr_cholesky,
                       n_tests = n_tests,
                       n_binary_tests = n_binary_tests,
                       n_ordinal_tests = n_ordinal_tests, 
                       n_class = 2,
                       vector_classes = seq(1:2),
                       prior_beta_mean =   prior_a_mean ,
                       prior_beta_sd  =   prior_a_sd ,
                       prior_a_mean =   prior_a_mean ,
                       prior_a_sd  =   prior_a_sd ,
                       prior_b_shape = prior_b_shape,
                       prior_b_scale = prior_b_scale,
                       prior_for_corr_a = prior_for_skewed_LKJ_a, # array(4, dim = c(n_class, n_tests, n_tests)), 
                       prior_for_corr_b = prior_for_skewed_LKJ_b, # array(3, dim = c(n_class, n_tests, n_tests)),
                       bs_to_set_to_0 = bs_to_set_to_0, # for latent trait
                       prior_LKJ = prior_lkj, 
                       prior_lkj = prior_lkj, 
                       prior_cons_corr_mean = rep(0, n_class), 
                       prior_cons_corr_sd = rep(0.25, n_class),
                       prior_ind_dirichlet_array =    array(1,dim = c(n_class, n_ordinal_tests, (max(Thr)+1))),
                       prior_anchor_point = array(0, dim = c(n_class, n_ordinal_tests)),
                       Thr =   Thr, # item-specific
                       ###    y = (y_list[[i]]),
                       y = (y_master_list_seed_123_datasets[[dataset_index]]),
                       homog_corr = 0,
                       cons_tests_corr = 0,
                       perfect_gs = 0,
                       prior_only = prior_only, ######
                       alpha_classes = c(1,1),
                       n_pops = 1, #  n_pops,
                       pop = group,
                       prior_alpha_classes = array(1, dim = c(n_pops, n_class)),
                       prior_p_alpha = array(rep(5, n_pops)),
                       prior_p_beta =  array(rep(10, n_pops)),
                       CI = CI,
                       induced_dir = 1,
                       K = 1,
                       x = t(array(rep(1, N), dim = c(N, 1))),
                       prior_cutpoint_sd = 5,
                       corr_pareto_alpha = 2,
                       corr_dist_mean = -1, 
                       corr_dist_sd = 2,
                       boundary_for_rough_approx = 5,
                       lb = -1, 
                       ub = 1,
                       known_num = 0,
                       corr_prior_beta = corr_prior_beta,
                       corr_prior_norm = corr_prior_norm,
                       X_nd = array(1, dim = c(n_tests, 1, N)), 
                       X_d = array(1, dim = c(n_tests, 1, N)), 
                        n_covs_per_outcome = n_covs_per_outcome, 
                       n_covariates_total_nd = n_covariates_total_nd,
                       n_covariates_total_d = n_covariates_total_d,
                       n_covariates_total = n_covariates_total,
                       k_choose_2 = k_choose_2,
                       km1_choose_2 = km1_choose_2,
                       n_covariates_max_nd = 1,
                       n_covariates_max_d = 1,
                       n_covariates_max = 1,
                       handle_numerical_issues = handle_numerical_issues,
                       fully_vectorised = fully_vectorised,
                       GHK_comp_method = GHK_comp_method,
                       overflow_threshold = overflow_threshold,
                       underflow_threshold = underflow_threshold,
                       Phi_type = Phi_type
    )
    
    
    
    if (model_type == "LT") {
      stan_data$prior_a_mean <-  prior_a_mean[,,1]
      stan_data$prior_a_sd <-  prior_a_sd[,,1]
      stan_data$LT_prior_on_mean_accuracy = LT_prior_on_mean_accuracy
    }
    
    
    stan_data_list[[i]] <- stan_data
    
  }
  
  
  
  require(abind)
  Omega_mvp <- array(diag(n_tests), dim = c(n_tests, n_tests, n_class))
  Omega_mvp <- aperm(Omega_mvp, c(3,2,1))
  L_Omega_mvp <- array(0, dim = c(n_tests, n_tests, n_class))
  diag(L_Omega_mvp[,,1]) <- 1
  diag(L_Omega_mvp[,,2]) <- 1
  L_Omega_mvp <- aperm(L_Omega_mvp, c(3,2,1))
  
  # set inits to same as custom sampler for fair comparison (corr's set to 0 or close to 0 e.g. 0.001)
  a_initial <- prior_a_mean
  p_initial <- initial_prev[1,2]
  
  
  a_initial[1,,1] <- - 1
  a_initial[2,,1] <- + 1
  
  # a <- array(c(rep(-1, n_tests), rep(1, n_tests)), dim = c( n_tests, n_class))
  # a <- aperm(a, c(2,1))
  log_diffs <- array(-1, dim = c(n_class, n_ordinal_tests, max(Thr)))
  b <- array(0.01, dim = c(n_class, n_tests))
  C_1 <- array(-2, dim = c(n_class, n_ordinal_tests))
  
  
  # model files  
  if (model_type == "LT") {
              
              if (prior_only == 0 )  {
                
                if (Hayley_LT_param == TRUE) file <- file.path(file = "latent_trait_Hayley_param_v2.stan")  
                else                         file <- file.path(file = "latent_trait_binary_as_MVP_v3.stan")  
                
              }
              
              
              if (prior_only == 1 )   { 
                if (uniform_indicator == 1)  file <- file.path(file = "LC_PO_uniform.stan")
                else   file <- file.path(file = "latent_trait_b_priors_v1.stan")
              }
              #  file <- file.path(file = "latent_trait_binary_v1.stan") 
              mod <- cmdstan_model(file)
              
  } else if (model_type == "LC_MVP") {
    
            if (corr_param == "Cholesky") {
              #   file <- file.path(file = "MVP_binary_Cholesky_v1.stan")
              file <- file.path(file = "MVP_binary_Chol_force_pos_v1.stan")
              file <- file.path(file = "LC_MVP_bin_DefaultCorr_w_stab_v1.stan")
            } else if (corr_param == "Archakov") {
              # file <- file.path(file = "MVP_binary_Arch_et_al_v1.stan")
              if (prior_only == 0 )  file <- file.path(file = "MVP_binary_Arch_force_pos_v1.stan")
              if (prior_only == 1 )  file <- file.path(file = "MVP_binary_Arch_prior_only.stan")
            } else if (corr_param == "Sean") {
              # file <- file.path(file = "MVP_binary_Spinkney_bouds_opt.stan")
              # file <- file.path(file = "MVP_binary_Spinkney_w_num_stab_v2.stan")
             #   file <- file.path(file = "LC_MVP_bin_Sean_w_stab_v5.stan")
              file <- file.path(file = "LC_MVP_bin_PartialLog_v1.stan")
              #   if (prior_only == 1 )  file <- file.path(file = "MVP_binary_Nump_any_intvl_v2_PO.stan")
              if (prior_only == 1 )  file <- file.path(file = "LC_MVP_bin_Sean_w_stab_v5_PO.stan")
            }
            
    
            mod <- cmdstan_model(file)
 
           #  
           #  # find cmdstan path
           #  cmdstanr::cmdstan_path()
           #  
           #  # then navigate to the path, and add this to make/local file (create if not there):
           #  #STAN_MODEL_LDFLAGS = -shared
           #  #CXXFLAGS += -fPIC
           #  
           # 
           # ### mod <- cmdstan_model(file, force_recompile = TRUE)
           #  
           #  mod$compile(force_recompile = TRUE, cpp_options = list("STAN_MODEL_LDFLAGS" = "-shared", 
           #                                                         "LDFLAGS" = "-shared", 
           #                                                         "CXXFLAGS" = " -D_REENTRANT   -fPIC -O3   -march=znver4   -mtune=znver4  -DSTAN_THREADS -pthread -fno-math-errno   -fno-trapping-math   -fno-signed-zeros",
           #                                                         "CXX" = "/opt/AMD/aocc-compiler-4.2.0/bin/clang++"
           #                                                         ))
           #  
           #  # 
           #  # mod <- cmdstan_model("LC_MVP_bin_Sean_w_stab_v5.stan", compile = FALSE, force_recompile = TRUE, dir = "/home/enzocerullo/Documents/Work/PhD_work/")
           #  # cat(mod$hpp_file(), "\n")
           #  # 
           #  # mod <- cmdstan_model("LC_MVP_bin_Sean_w_stab_v5.stan", compile = FALSE, dir = "/home/enzocerullo/Documents/Work/PhD_work/")
           #  # mod$compile(dir = "/home/enzocerullo/Documents/Work/PhD_work/")
           #  
           #  # found fie in temp w/ name: model-15dd8176371fbc.hpp
           #  
           # # rstan:::get_Rcpp_module_def_code("x39LC_MVP_bin_Sean_w_stab_v5_modelx39_namespace")
           #  
           #  
           #  # # ## Compile foo.cpp into a shared object foo.so
           #  # # ## This will need to be generalized for cross platform
           #  # # stan_shared("model-15dd8176371fbc.cpp", "model-15dd8176371fbc.so")
           #  # # ## Load the so
           #  # safe_load_so <- function(so_file) {
           #  #   tryCatch({
           #  #     dyn.load(so_file)
           #  #     message("Shared object loaded successfully")
           #  #   }, error = function(e) {
           #  #     message("Error loading shared object: ", e)
           #  #   })
           #  # }
           #  # 
           #  # 
           #  # result <- mclapply(list("/tmp/RtmpCuYIPp/model-194eb2124b97f6"), safe_load_so, mc.cores = 2)
           #  # 
           #  # 
           #  #  dyn.load('/tmp/RtmpCuYIPp/model-194eb2124b97f6')
           # 
           #  
           #  # find cmdstan path
           #  cmdstanr::cmdstan_path()
            

           # 00000000004b89b0 W double stan::model::log_prob_grad<true, false, stan::model::model_base>(stan::model::model_base const&, std::vector<double, std::allocator<double> >&, std::vector<int, std::allocator<int> >&, std::vector<double, std::allocator<double> >&, std::basic_ostream<char, std::char_traits<char> >*)
            
            #   
            # dyn.load("/home/enzocerullo/Documents/Work/PhD_work/simple.so")
            # simple_function(10)
            #   
            # # compile only to C++ file 
            # mod <- cmdstan_model(file, compile = FALSE)  # Compile = FALSE means we only translate to C++
            # 
            # # Retrieve the path to the generated C++ file
            # cpp_file <- mod$cpp_code()
            # 
            # 
            # 
            # cmdstanr::cmdstan_path()
            # file.exists("/home/enzocerullo/Documents/Work/PhD_work/LC_MVP_bin_Sean_w_stab_v5.so")
            # 
            # n_params <- N*n_tests + 2 * choose(n_tests, 2) + 2*n_tests + 1
            # n_nuisance <-   N*n_tests
            # n_params_main <- n_params - n_nuisance
            # theta_vec <- rep(0.01, n_params)
            # 
            # stan_model <- stan_model("LC_MVP_bin_Sean_PartialLog_v1.stan")
            # 
            # 
            # # Get log probability
            # lp <- rstan::log_prob(stan_model, upars = theta_vec, adjust_transform = TRUE)
            # 
            # # Get gradient of log probability
            # grad <- rstan::grad_log_prob(stan_model, upars = params, adjust_transform = TRUE)
            # 
            # 
            # mod$init_model_methods()
            # mod$grad_log_prob(unconstrained_variables = theta_vec)
            # 
            # 
            # fit_mcmc <- cmdstanr_example("logistic", method = "sample", force_recompile = TRUE)
            # fit_mcmc$init_model_methods()
            # 
            # 
            # 
            
            # safe_test_so <- function(so_file,
            #                          params,
            #                          n_nuisance,
            #                          n_params_main) {
            # 
            #   
            #   lp_grad <- run_stan_model_from_r(so_file = "/home/enzocerullo/Documents/Work/PhD_work/LC_MVP_bin_Sean_PartialLog_v1.so", 
            #                                    params = theta_vec,
            #                                    n_nuisance = n_nuisance, 
            #                                    n_params_main = n_params_main)
            #   
            # }
            # 
            # 
            # result <- mclapply(list(so_file = "/home/enzocerullo/Documents/Work/PhD_work/LC_MVP_bin_Sean_PartialLog_v1.so", 
            #                    params = theta_vec,
            #                    n_nuisance = n_nuisance, 
            #                    n_params_main = n_params_main),
            #                    safe_test_so, mc.cores = 2)
            # 
            # 
            # 
            # 
            # lp_grad <- run_stan_model_from_r(so_file = "/home/enzocerullo/Documents/Work/PhD_work/LC_MVP_bin_Sean_PartialLog_v1.so", 
            #                                  params = theta_vec,
            #                                  n_nuisance = n_nuisance, 
            #                                  n_params_main = n_params_main)
            # 
            # 
            

    
  }
  
  
  if (LT_find_b_priors_using_density == TRUE) {
    file <- file.path(file = "LT_PO_b_finder.stan")
    mod <- cmdstan_model(file)
  }
  
  
  
  # -------------------------------------------------------------------------
  
  
  
  
  # -------------------------------------------------------------------------
  
  
  
  
  
  
  #u_initial <- array(0.001, dim = c(N, n_class, n_tests))
  u_initial <- array(0.01, dim = c(N, n_tests))
  
  km1_choose_2 = 0.5 * (n_tests - 2) * (n_tests - 1)
  known_num = 0
  
  # 
  # matrix[n_class, n_tests] LT_a;
  # matrix<lower=0, upper = 5>[n_class, n_tests] LT_b;
  # vector[n_pops]  p_raw;
  # matrix[N, n_tests] u_raw; // nuisance that absorbs inequality constraints
  
  
  beta_vec_init <- rep(0, n_class * n_tests)
  beta_vec_init[1:n_tests] <- - 1   # tests 1-T, class 1 (D-)
  beta_vec_init[(n_tests + 1):(2*n_tests)] <- + 1   # tests 1-T, class 1 (D+)
  
  init = list(
    u_raw = (u_initial),
    p_raw =  (-0.6931472), # equiv to 0.20 on p
      a = a_initial,
    #beta = a_initial, # [,,1],
    # theta = a_initial[,,1],
    beta_vec = beta_vec_init,
    # Omega_mvr = Omega_mvp,
    # L_Omega_mvr = L_Omega_mvp,
  #  L_Omega = L_Omega_mvp,
    # L_Sigma_2 = L_Omega_mvp,
    # b = b,
    # C_1 = C_1,
    # L_Omega_d_pre = diag(n_tests),
    # L_Omega_nd_pre = diag(n_tests),
    # sigma = rep(1, n_tests),
    # Omega_raw = array(rnorm(mean = 1, sd = 0.0000001, n = choose(n_tests, 2) * n_class), dim = c(n_class, choose(n_tests, 2))),
#    Omega_raw = array(rnorm(mean = 0.1, sd = 0.0000001, n = choose(n_tests, 2) * n_class), dim = c(n_class, choose(n_tests, 2))),
    off_raw = array(0.01, dim = c(n_class, km1_choose_2 - known_num)), 
    col_one_raw =   array(0.01, dim = c(n_class, n_tests - 1))
   # b_raw = array(-3, dim = c(n_class, n_tests))
  )
  
  if (model_type == "LT") {
    a_init <- array(dim = c(n_class, n_tests))
    a_init[1, ] <- -2
    a_init[2, ] <- +2
    b_init <- array(dim = c(n_class, n_tests))
    b_init[1, ] <-  0.25
    b_init[2, ] <- 0.25 
    # init$p <- (p_initial)
    init$LT_a <-  a_init #  a_initial[,,1]
    init$LT_b <-  b_init
  } else { 
    # init$p <- array(p_initial)
    init$a <-  a_initial
  }
  
  
  
  
  
}


# Sys.setenv(LD_LIBRARY_PATH = "/home/enzocerullo/Documents/Work/PhD_work/LC_MVP_bin_Sean_PartialLog_v1")
# Sys.setenv(LD_LIBRARY_PATH = "/home/enzocerullo/Documents/Work/PhD_work/")

# 
# dyn.load("/home/enzocerullo/Documents/Work/PhD_work/LC_MVP_bin_Sean_PartialLog_v1.so")
# dyn.load("/usr/local/lib/libbridgestan.so")
 


Sys.setenv(CXXFLAGS = paste(Sys.getenv("CXXFLAGS"), "-I/home/enzocerullo/bridgestan/src", sep=" "))
Sys.setenv(CPPFLAGS = paste(Sys.getenv("CPPFLAGS"), "-I/home/enzocerullo/bridgestan/src", sep=" "))
Sys.setenv(PKG_CPPFLAGS = paste(Sys.getenv("PKG_CPPFLAGS"), "-I/home/enzocerullo/bridgestan/src", sep=" "))
Sys.setenv(PKG_CXXFLAGS = paste(Sys.getenv("PKG_CXXFLAGS"), "-I/home/enzocerullo/bridgestan/src", sep=" "))



###----- Step 1: you need to make a "libbridgestan.so" file, in Linux can do this using terminal e.g.:
# enzocerullo@pop-os:~/bridgestan$ g++ -shared -o libbridgestan.so bridgestan.o
###----- Step 2: Compile stan model into shared object file (may need to set LDFLAGS), 
# and then put in same directory as the "libbridgestan.so" file. 
### ----- Step 3:, link them together by running: g++ -shared -o libbridgestan.so ./bridgestan.o ./LC_MVP_bin_Sean_PartialLog_v1.so
# (if not in same directory, then link them!)
# Next use dyn.load (make sure you load the Stan shared object model FIRST [and make one for your own model!]):
dyn.load("/home/enzocerullo/Documents/Work/PhD_work/LC_MVP_bin_PartialLog_v4_model.so")

dyn.load("/home/enzocerullo/Documents/Work/PhD_work/R_packages/BayesMVP/src/model_rng.so")
dyn.load("/home/enzocerullo/Documents/Work/PhD_work/R_packages/BayesMVP/src/libbridgestan.so")


dyn.load("LC_MVP_bin_PartialLog_v1_model.so", local = FALSE, now = TRUE)

dyn.load("/home/enzocerullo/Documents/Work/PhD_work/LC_MVP_bin_PartialLog_v3_model.so", local = FALSE, now = TRUE)

Sys.setenv(LD_PRELOAD = "/home/enzocerullo/Documents/Work/PhD_work/R_packages/BayesMVP/src/LC_MVP_bin_PartialLog_v4.so")

dyn.load("/home/enzocerullo/Documents/Work/PhD_work/R_packages/BayesMVP/lib/libbridgestan.so" ,local = FALSE, now = TRUE)

getLoadedDLLs()


dyn.load("/home/enzocerullo/bridgestan/src/libbridgestan.so")

# then, sourceCpp() should work. 
# #### If still doesdnt work, or want to use as part of R package, then add these flags to makeconf or makevars file (adjust paths as appropriate):
# PKG_LIBS +=  /home/enzocerullo/bridgestan/src/libbridgestan.so
# PKG_LIBS +=   /home/enzocerullo/bridgestan/src/LC_MVP_bin_Sean_PartialLog_v1.so

# Sys.setenv(CXXFLAGS="-g")

## model_so_file <- "/home/enzocerullo/Documents/Work/PhD_work/LC_MVP_bin_PartialLog_v1_model.so"
r_data_list <- stan_data_list[[123]]

 


# r_data <- fromJSON(r_data_JSON)

 
# # Before running this, make sure you are in the directory bridgestan/R





# # ------ with cmdstanr
Sys.setenv(STAN_THREADS="true")
file <- file.path(file = "/home/enzocerullo/Documents/Work/PhD_work/LC_MVP_bin_PartialLog_v3.stan")
mod <- cmdstan_model(file,
                     force_recompile = TRUE,
                     compile_model_methods = TRUE,
                  #   compile_hessian_method = TRUE,
                  #   user_header = "/home/enzocerullo/Documents/Work/PhD_work",
                     cpp_options = list(
                      # "STAN_MODEL_LDFLAGS" = "-shared",
                       STAN_THREADS = TRUE
                       # "LDFLAGS" = "-shared",
                    #   "CXXFLAGS" = "-pthread  -DSTAN_THREADS"
                     ))

##cmdstanr::set_cmdstan_path("/home/enzocerullo/.cmdstan/cmdstan-2.33.1")
cmdstanr::cmdstan_path()
cmdstanr::set_cmdstan_path("/home/enzocerullo/cmdstan")
file <- file.path(file = "/home/enzocerullo/Documents/Work/PhD_work/dummy_stan_model.stan")
mod$sample(data = dummy_data)



## ---- STAN METHOD 1A: USING FULL MODEL (SLOWEE) ----------------------------

rstan_model_full <- stan_model("dummy_stan_model.stan")

# then "run" the model but using bad inits so that it fails (just to get the fitted object!)
n_chains <- 2
fit_rstan_full <- rstan::sampling(rstan_model_full,
                                  seed = 123,
                                  init = rep(list(init), n_chains),
                                  data = stan_data_list[[123]],
                                  chains = n_chains,  iter = 400, warmup =200,
                                  #   control = list(adapt_delta = 0.0001, max_treedepth = 1)
)
#


# 
# 
mod$save_hpp_file()


library(bridgestan)

# convert data to JSON format (use cmdstanr::write_stan_json NOT jsonlite::tload_and_run_log_prob_grad_all_StanoJSON)
r_data_list <- stan_data_list[[123]]
r_data_JSON <- tempfile(fileext = ".json")
cmdstanr::write_stan_json(r_data_list, r_data_JSON)

Sys.setenv(STAN_THREADS="true")
model <- StanModel$new("LC_MVP_bin_PartialLog_v4.stan", r_data_JSON, 1234)
print(paste0("This model's name is ", model$name(), "."))
print(paste0("This model has ", model$param_num(), " parameters."))

theta_vec <- rnorm(n_params, sd = 0.01, mean = 0)

res <- model$log_density_gradient(theta_vec, jacobian = TRUE)

# 
# print(paste0("log_density and gradient of Bernoulli model: ",
#              res$val, ", ", res$gradient))
# 


library(bridgestan)

dummy_data_N <-  100
dummy_data_vec <- rnorm(dummy_data_N)
dummy_data <- list(N = dummy_data_N, y = dummy_data_vec )
# convert data to JSON format (use cmdstanr::write_stan_json NOT jsonlite::tload_and_run_log_prob_grad_all_StanoJSON)
r_data_list <- dummy_data
r_data_JSON <- tempfile(fileext = ".json")
cmdstanr::write_stan_json(r_data_list, r_data_JSON)


Sys.setenv(STAN_THREADS="true")
model <- StanModel$new("dummy_stan_model.stan", r_data_JSON, 1234)
print(paste0("This model's name is ", model$name(), "."))
print(paste0("This model has ", model$param_num(), " parameters."))
 

res <- model$log_density_gradient(1, jacobian = TRUE)




# now call the function 
run <- parallel::mcparallel(run_model_from_R(model_so_file,   r_data_JSON,    theta_vec))
res <- parallel::mccollect(run)
res

tic()
run <-  (run_model_from_R(model_so_file,   r_data_JSON,    theta_vec))
toc()
res

run <- parallel::mcparallel( load_and_run_log_prob_grad_all_Stan(so_file = model_so_file,
                                                                 param_names = list("hello"),
                                                                 param_dims = list(list(1)),
                                    params = theta_vec,
                                    data_from_r_json = r_data_JSON,
                                    use_json = TRUE,
                                    random_seed = 123))
res <- parallel::mccollect(run)
res


### Sys.setenv(LD_LIBRARY_PATH = "/home/enzocerullo/bridgestan/src/")
# corpcor(Sigma_d)
# corpcor::cor2pcor(Sigma_d)

###  -------------  Stan - run models -------------------------------------------------------------------------------------------------------------------
# set the seed

# empty_stan_model <- new("stanfit")
# 
# MVP_fn_args_List$rstan_model <- empty_stan_model
# MVP_fn_args_List$rstan_model_fixed_nuisance <- empty_stan_model
# MVP_fn_args_List$rstan_model_fixed_main <- empty_stan_model

### -------- BayesMVP - STAN METHOD 1:  approach using rstan   ------------------------------------------------
  n_params <- N*n_tests + 2 * choose(n_tests, 2) + 2*n_tests + 1
  n_nuisance <-   N*n_tests
  n_params_main <- n_params - n_nuisance
  theta_vec <- rnorm(n_params, mean = 0, sd  = 0.10)
  
  # 
  ## ---- STAN METHOD 1A: USING FULL MODEL (SLOWEE) ----------------------------

  rstan_model_full <- stan_model("LC_MVP_bin_PartialLog_v1.stan")

  # then "run" the model but using bad inits so that it fails (just to get the fitted object!)
  n_chains <- 2
  fit_rstan_full <- rstan::sampling(rstan_model_full,
                                    seed = 123,
                                    init = rep(list(init), n_chains),
                               data = stan_data_list[[123]],
                               chains = n_chains,  iter = 400, warmup =200,
                            #   control = list(adapt_delta = 0.0001, max_treedepth = 1)
  )
  #
  #
  # lp <- rstan::log_prob(fit_rstan_full, upars = theta_vec, adjust_transform = TRUE)  ; lp  # Get log probability
  # grad <- rstan::grad_log_prob(fit_rstan_full, upars = theta_vec, adjust_transform = TRUE) ; grad # Get gradient of log probability
  #
  # MVP_fn_args_List$rstan_model = fit_rstan_full
  #
  # #  parallel::mcparallel
  # tic()
  # res <- parallel::mcparallel(Rcpp_wrapper_compute_full_lp_grad_using_rstan(rstan_model = fit_rstan_full, params = theta_vec))
  # toc()
  #
  # result <- parallel::mccollect(res)
  # result[[1]]
  # 
 
  # ------ with cmdstanr
   file <- file.path(file = "LC_MVP_bin_PartialLog_v2.stan")
   mod <- cmdstan_model(file)
    mod <- cmdstan_model(file, 
                         force_recompile = TRUE, 
                         compile_model_methods = TRUE, 
                         compile_hessian_method = TRUE,
                     #    user_header = "/home/enzocerullo/Documents/Work/PhD_work",
                         cpp_options = list(
                          "STAN_MODEL_LDFLAGS" = "-shared"
                        # "LDFLAGS" = "-shared",
                        # "CXXFLAGS" = "-fPIC"
                         )
                        )
  
  

  
  
  mod$save_hpp_file()
 ##   mod <- cmdstan_model("LC_MVP_bin_PartialLog_v1.stan")
  
  n_chains <- 1
  
  # # make sure cmdstanr path is correct
  # cmdstanr::set_cmdstan_path("/home/enzocerullo/cmdstan")
    cmdstanr::set_cmdstan_path("/home/enzocerullo/.cmdstan/cmdstan-2.34.1")
  # 
   cmdstanr::cmdstan_path()
  
  cmdstan_model <-   mod$sample( data = stan_data_list[[123]],
                          seed = 123,
                          chains = n_chains,
                          parallel_chains = n_chains,
                          iter_warmup = 500,
                          init = rep(list(init), n_chains), 
                          iter_sampling = 500,
                          #save_cmdstan_config=TRUE,
                          output_dir = "/home/enzocerullo/Documents/Work/PhD_work/"
                         # adapt_delta = 0.0001, 
                         # max_treedepth = 1
                          )
  
  model_file <- file.path(cmdstan_path(), "examples", "bernoulli", "bernoulli.stan")
  mod <- cmdstan_model(model_file, force_recompile = TRUE)
  data_list <- list(N = 10, y = c(0, 1, 0, 0, 0, 0, 0, 0, 0, 1))
  fit <- mod$sample(data = data_list, parallel_chains = 4)
  

  
  check_cmdstan_toolchain(fix = TRUE)
  cmdstanr::rebuild_cmdstan()
  
  
  read_cmdstan_csv(cmdstan_model)
  
  cmdstan_model$output(1)
  
  ##cmdstan_model$init_model_methods()
  grad_log_prob <- cmdstan_model$grad_log_prob(unconstrained_variables = theta_vec)
  log_prob <- cmdstan_model$log_prob(unconstrained_variables = theta_vec)

    str(cmdstan_model)
    
    MVP_fn_args_List$cmdstan_model = cmdstan_model
 
    run_with_data("LC_MVP_bin_Sean_PartialLog_v1.stan")
    
    run_model_with_grad
    
    )
    
stan_data$N <- as.integer(stan_data$N)


   res <- parallel::mcparallel( run_model_with_params(so_file = "/home/enzocerullo/Documents/Work/PhD_work/LC_MVP_bin_Sean_PartialLog_v1",
                          data_list = stan_data,
                          params = theta_vec) )
    
    
   res <- parallel::mccollect(res)
   res
  # fit_mcmc <- cmdstanr_example("logistic", method = "sample", force_recompile = TRUE)
  # fit_mcmc$init_model_methods()
  # 
  
  tic()
  res <- parallel::mcparallel(Rcpp_wrapper_compute_full_lp_grad_using_cmdstanr(cmdstan_model_sexp = cmdstan_model, params = theta_vec))
  toc()
  
  result <- parallel::mccollect(res)
  outs <-  result[[1]]
  
  str(outs)
  
  ## ---- STAN METHOD 1B: USING partial MODELs (FASTER) ----------------------------
  # define one model with FIXED (i.e., as data) MAIN params
  rstan_model_fixed_main <-     stan_model("LC_MVP_bin_PartialLog_fixed_main_v1.stan")
  # then "run" the model but using bad inits so that it fails (just to get the fitted object!)   
  data_for_fixed_main <- stan_data_list[[123]]
  data_for_fixed_main$p_raw <-  array(-0.6931472)
  data_for_fixed_main$beta_vec <- array(beta_vec_init)
  data_for_fixed_main$off_raw <- array(0.01, dim = c(n_class, km1_choose_2 - known_num))
  data_for_fixed_main$col_one_raw <- array(0.01, dim = c(n_class, n_tests - 1))
  
  fit_rstan_fixed_main <- rstan::sampling(rstan_model_fixed_main, 
                               data = data_for_fixed_main,
                               chains = 1,  iter = 2, warmup = 1,
                               control = list(adapt_delta = 0.0001, max_treedepth = 1))
  u_vec <- rep(-2, n_nuisance)
  lp <- rstan::log_prob(fit_rstan_fixed_main, upars = u_vec, adjust_transform = TRUE)  ; lp  # Get log probability
  grad <- rstan::grad_log_prob(fit_rstan_fixed_main, upars = u_vec, adjust_transform = TRUE) ; grad # Get gradient of log probability
  
  # define a 2nd model with FIXED (i.e., as data) NUISANCE params  (i.e. grad w.r.t MAIN ONLY)
  rstan_model_fixed_nuisance <- stan_model("LC_MVP_bin_PartialLog_fixed_nuisance_v1.stan")
  # then "run" the model but using bad inits so that it fails (just to get the fitted object!)    
  data_for_fixed_nuisance <- stan_data_list[[123]]
  data_for_fixed_nuisance$u_raw <- u_initial # add nuisance "data"
  
  fit_rstan_fixed_nuisance <- rstan::sampling(rstan_model_fixed_nuisance,
                                              data = data_for_fixed_nuisance, 
                                              chains = 1,  iter = 2, warmup = 1,
                                              control = list(adapt_delta = 0.0001, max_treedepth = 1))
  
  theta_main <- tail(theta_vec, n_params_main)

  lp <- rstan::log_prob(fit_rstan_fixed_nuisance, upars = theta_main, adjust_transform = TRUE)  ; lp  # Get log probability
  grad <- rstan::grad_log_prob(fit_rstan_fixed_nuisance, upars = theta_main, adjust_transform = TRUE) ; grad # Get gradient of log probability
 
  
  
  
  
    mod$init_model_methods()
  mod$grad_log_prob(unconstrained_variables = theta_vec)
  
  
  fit_mcmc <- cmdstanr_example("logistic", method = "sample", force_recompile = TRUE)
  fit_mcmc$init_model_methods()
  
  # Generate some random initial parameters (or any initialization)
  init_params <- rnorm(n_params)
  
  # Create a CmdStanFit object with dummy initial values (not running full sampling)
  dummy_data =  stan_data_list[[123]]
  # fit <- mod$optimize(data = dummy_data, init =  list(init))
  fit <- mod$sample(data = dummy_data, init =  list(init), chains = 1)
  
  log_prob_val <- fit$log_prob(upars = init_params, data = dummy_data, adjust_transform = TRUE)
  gradients <- fit$grad_log_prob(upars = init_params, data = dummy_data, adjust_transform = TRUE)







# start_run <- 1
# n_runs <- 1

# for (seed in start_run:n_runs)



sims_start <- 1
sims_end <-   1



for (df_i in sims_start:sims_end) {
  
  
  
  
  
  {
    
    
    gc(reset = TRUE) 
    
    seed <- df_i
    
    
    # run using NUTS-HMC
    metric_type <- "diag_e"    #  --------------- 
    #  metric_type <- "unit_e"
    
    #   n_chains <-  64  ;  iter_warmup <- 250  ;        iter_sampling <-   800 
    #  n_chains <-  8 ; iter_warmup <- 1000  ;       iter_sampling <-   500 
    
    
    
    n_chains <-  32 ;  iter_sampling <-   500 # for pilot study / tuning / testing
    
    if (N == 5000) {
      n_chains <-  16 ;  iter_sampling <-   250
    }
    
    
    ####    iter_warmup <- 1000
    ###     iter_warmup <- 500
    ### iter_warmup <- 250
    ###     iter_warmup <- 125
    
    
    
    #    iter_warmup <- 1000  # already done pilot runs for this  
    iter_warmup <- 525
    #    iter_warmup <- 275
    #    iter_warmup <- 200 ###
    #    iter_warmup <- 150
    
    
    adapt_delta <- 0.80
    
    max_treedepth <- 10   #     max_treedepth <- 9
    
    if (!(n_chains %in% c(3, 6, 10, 16, 24, 32, 50, 80)))  {
      
          # if (parallel::detectCores() < 17) {  # Laptop (8 cores / 8 chains) 
          #   if (N == 500)   {  iter_warmup = 275    ; iter_sampling = 500  }   # target ESS ~ 1000
          #   if (N == 1000)  {  iter_warmup = 1000   ; iter_sampling = 500  }  # target ESS ~ 1000
          #   if (N == 2500)  {  iter_warmup = 200    ; iter_sampling = 500  } # should be 5450 but time-limited  ; 1090 =  (5450 / 5)  - so target ESS ~ 200
          #   if (N == 5000)  {  iter_warmup = 525    ; iter_sampling = 500  }  # should be 5714 but time-limited  ; 286 =  (5714 / 20)  - so target ESS ~ 50
          # } else { # HPC (96 cores / 64 chains)
          #   if (N == 500)   {  iter_warmup = 200  ; iter_sampling = 500  }  # target ESS ~ 1000
          #   if (N == 1000)  {  iter_warmup = 275  ; iter_sampling = 500  } # target ESS ~ 1000
          #   if (N == 2500)  {  iter_warmup = 200  ; iter_sampling = 500  } # target ESS ~ 1000
          #   if (N == 5000)  {  iter_warmup = 525  ; iter_sampling = 500  }  # should be 714 but time-limited  ; 143 =  (714 / 5)  - so target ESS ~ 200
          # }
          
          max_treedepth <- 10
          adapt_delta <- 0.80
          metric_type <- "diag_e"
          
    }
    
    
    
    if (prior_only == 1)  { 
      iter_warmup = 1000
      iter_sampling = 16000
    }
    
    
    tictoc::tic("timer")
    
    stan_data_list[[df_i]]$n_tests <- n_tests
    #  stan_data_list[[df_i]]$n_tests <- 5
    
    
    
    

    
    
    
    model <- mod$sample(
      # data = stan_data_list[[df_i]],
      data = stan_data_list[[123]],
      seed = seed,
      chains = n_chains,
      parallel_chains = n_chains,
      iter_warmup = iter_warmup,
      iter_sampling = iter_sampling, 
      refresh = round( ((iter_warmup + iter_sampling)/10), 0),
      init = rep(list(init), n_chains), 
      save_warmup = 1, # for some efficiency stats (can turn off for sim. study)
      metric = metric_type,
      adapt_delta = adapt_delta, 
      max_treedepth = max_treedepth)
    
    
    gc(reset = TRUE)
    

    
    {
      
      print(prior_lkj)
      
      
      # view sampler diagnostics (no need to do this as cmdstanr flags divergences + iterations which exceed max_treedepth, and adds additional comp. time)
      #model$cmdstan_diagnose()
      
      
      
      
      
      if (prior_only == 0) {
        
        if (model_type == "LC_MVP") { 
          
          try({
            cmdstanr_model_out <- model$summary(variables = c( "p_raw", "beta" , "Omega"), "mean", "median", "sd", "mad",  ~quantile(.x, probs = c(0.025,  0.975) ), "rhat" , "ess_bulk", "ess_tail")
            print(cmdstanr_model_out, n = 100)
            Min_ESS <- round(min(cmdstanr_model_out$ess_bulk, na.rm=TRUE), 0)
            print(paste("min ESS = ", Min_ESS))
          })
          try({
            cmdstanr_model_out <- model$summary(variables = c( "p", "Se_bin", "Sp_bin"  ), "mean", "median", "sd", "mad",  ~quantile(.x, probs = c(0.025,  0.975) ), "rhat" , "ess_bulk", "ess_tail")
            print(cmdstanr_model_out, n = 100)
          })
          
        } else if (model_type == "LT") { 
          
          try({
            cmdstanr_model_out <- model$summary(variables = c( "p", "Se_mean", "Sp_mean" , "Se_median", "Sp_median"  ), "mean", "median", "sd", "mad",  ~quantile(.x, probs = c(0.025,  0.975) ), "rhat" , "ess_bulk", "ess_tail")
            print(cmdstanr_model_out, n = 100)
          })
          try({
            cmdstanr_model_out <- model$summary(variables = c("p_raw",  "LT_a", "LT_b"), "mean", "median", "sd", "mad",  ~quantile(.x, probs = c(0.025,  0.975) ), "rhat" , "ess_bulk", "ess_tail")
            print(cmdstanr_model_out, n = 100)
            Min_ESS <- round(min(cmdstanr_model_out$ess_bulk, na.rm=TRUE), 0)
            print(paste("min ESS = ", Min_ESS))
          })
          
        }
        
      } else { 
        
        # try({
        #   cmdstanr_model_out <- model$summary(variables = c( "beta_alpha_1", "beta_alpha_2"), "mean", "median", "sd", "mad",  ~quantile(.x, probs = c(0.025,  0.975) ), "rhat" , "ess_bulk", "ess_tail")
        #   print(cmdstanr_model_out, n = 100)
        #   Min_ESS <- round(min(cmdstanr_model_out$ess_bulk, na.rm=TRUE), 2)
        #   print(paste("min ESS = ", Min_ESS))
        # })
        
        try({
          cmdstanr_model_out <- model$summary(variables = c(  "Omega"), "mean", "median", "sd", "mad",  ~quantile(.x, probs = c(0.025,  0.975) ), "rhat" , "ess_bulk", "ess_tail")
          print(cmdstanr_model_out, n = 100)
          Min_ESS <- round(min(cmdstanr_model_out$ess_bulk, na.rm=TRUE), 0)
          print(paste("min ESS = ", Min_ESS))
        })
        
      }
      
      
      
      try({
        cmdstanr_model_out <- model$summary(variables = c(  "n_mild_overflow_pct", "n_severe_overflow_pct", "n_underflow_pct"), "mean", "median", "sd", "mad",  ~quantile(.x, probs = c(0.025,  0.975) ), "rhat" , "ess_bulk", "ess_tail")
        print(cmdstanr_model_out, n = 100)
      })
      
      
      
      
      
      
      
      try({
        
        # Max R-hat / converged?
        cmdstanr_model_out_rhat <- cmdstanr_model_out$rhat[!(is.na(cmdstanr_model_out$rhat))]
        print(round(max(cmdstanr_model_out_rhat), 3)) ; print(any(cmdstanr_model_out_rhat > 1.010)) # if TRUE, not converged (at least 1 r-hat > 1.01)
        
        print(tictoc::toc(log = TRUE))
        log.txt <- tictoc::tic.log(format = TRUE)
        tictoc::tic.clearlog()
        time_stan_total_inc_csv <- unlist(log.txt)
        
        time_stan_total_real_world <- as.numeric(substr(time_stan_total_inc_csv,  8,  14))
        
        # run time (total)
        cmdstanr_model_out_timers <- model$time()
        total_time_seconds <- time_stan_total_real_world #  cmdstanr_model_out_timers$total
        total_time_mins <- total_time_seconds / 60
        total_time_hours <- total_time_mins / 60
        
        print(paste("seed = ", seed))
        
        print(paste("total time =", round(total_time_seconds, 0), "seconds")) # in seconds
        print(paste("total time =", floor(total_time_mins), "minutes and ", round(((total_time_mins - floor(total_time_mins))*60), 0), "seconds"))    # in minutes
        print(round(total_time_mins, 3 ))
        print(paste("total time =", floor(total_time_hours), "hours and ", round(((total_time_hours - floor(total_time_hours))*60), 0), "minutes"))    # in hours
        
        # run time (post-burnin / sampling ONLY)
        pb_time_seconds <- max(cmdstanr_model_out_timers$chains$sampling)
        pb_time_mins <- pb_time_seconds / 60
        pb_time_hours <- pb_time_mins / 60
        
        print(paste("Sampling (post-burnin) time =", round(pb_time_seconds, 0), "seconds")) # in seconds
        print(paste("Sampling (post-burnin) time =", floor(pb_time_mins), "minutes and ", round(((pb_time_mins - floor(pb_time_mins))*60), 0), "seconds"))    # in minutes
        print(round(pb_time_mins, 3 ))
        print(paste("Sampling (post-burnin) time =", floor(pb_time_hours), "hours and ", round(((pb_time_hours - floor(pb_time_hours))*60), 0), "minutes"))    # in hours
        
        # Min ESS / sec (using total time)
        Min_ESS_per_sec_total_time <- Min_ESS / total_time_seconds
        print(paste("Min ESS / sec (total time) = ", signif(Min_ESS_per_sec_total_time, 5)))
        
        # Min ESS / sec (using post-burnin / sampling time)
        Min_ESS_per_sec_pb_time <- Min_ESS / pb_time_seconds
        print(paste("Min ESS / sec (sampling time only) = ", signif(Min_ESS_per_sec_pb_time, 5)))
        
        # # Min ESS / grad (uses post-burnin / sampling time ONLY)
        mean_L  <-   (mean(c(2^model$sampler_diagnostics(inc_warmup = TRUE)[1:iter_warmup,,1] - 1)))  # mean L (total)
        print(paste("Mean L across chains (burnin)  = ", mean_L))
        
        mean_L_post_burnin  <-   (mean(c(2^model$sampler_diagnostics()[,,1] - 1))) # mean # of Leapfrog steps (sampling only)
        print(paste("Mean L across chains (post-burnin) = ", mean_L_post_burnin))
        
        
        
        L_burn_means_per_chain <-  c()
        L_samp_means_per_chain <-  c()
        for (i in 1:n_chains) {
          L_burn_means_per_chain[i]    <-  mean ( (  2^model$sampler_diagnostics(inc_warmup = TRUE)[1:iter_warmup,,1] - 1  )[ ,i,1]) 
          L_samp_means_per_chain[i]    <-  mean ( (  2^model$sampler_diagnostics()[,,1] - 1  )[1:iter_sampling,i,1]) 
        }
        
        max_of_mean_Ls_per_chain_burn <-      max(L_burn_means_per_chain)
        max_of_mean_Ls_per_chain_samp <-      max(L_samp_means_per_chain)
        
        
        print(paste("Max L across chains (burnin)  = ", max_of_mean_Ls_per_chain_burn))
        print(paste("Max L across chains (post-burnin) = ", max_of_mean_Ls_per_chain_samp))
        
        
        mean_eps_post_burnin  <-   (mean(c(model$sampler_diagnostics()[,,5]))) # mean # of Leapfrog steps (sampling only)
        print(paste("Mean step-size across chains (post-burnin) = ", signif(mean_eps_post_burnin, 4)))
        
        # total # of gradient evals
        stan_total_gradient_evals <-   (mean_L*n_chains*(iter_sampling + iter_warmup))   #  n_chains * iter_sampling * stan_n_leapfrogs ; stan_total_gradient_evals
        stan_total_gradient_evals_pb <-   (mean_L_post_burnin*n_chains*iter_sampling)
        
        stan_min_ess_per_grad_eval <- Min_ESS / stan_total_gradient_evals ; stan_min_ess_per_grad_eval
        stan_min_ess_per_grad_eval_pb <- Min_ESS / stan_total_gradient_evals_pb ; stan_min_ess_per_grad_eval
        
        print(paste("ESS/grad (total) = ", signif(1000 * stan_min_ess_per_grad_eval, 3)))
        print(paste("ESS/grad (sampling) = ", signif(1000 * stan_min_ess_per_grad_eval_pb, 3)))
        print(paste("grad/sec (sampling) = ",  signif(Min_ESS_per_sec_pb_time  / (1000 * stan_min_ess_per_grad_eval_pb), 3)  ))
        
        total_divs <- sum(model$sampler_diagnostics()[,,2])
        
        print(paste("total_divs = ", total_divs))
        
      #  print(  round(  ( n_chains * ( mean_L * iter_warmup + ( iter_sampling * (100 / (min_ess / 1    )  )  * mean_L_post_burnin  ) ) ) / 1000   , 0) )
      #  print(  round(  ( n_chains * ( mean_L * iter_warmup + ( iter_sampling * (1000 / (min_ess / 1   )  )  * mean_L_post_burnin  ) ) ) / 1000   , 0) )
        
        
        stan_draws_array <- model$draws()
        
        
        num_elements_Omegas <- n_class * n_tests * n_tests
        
        #  Omega_index_start_for_Sean_param <- n_class*1*n_tests + N*n_tests + 1 + n_class*(n_tests-1) + n_class * choose(n_tests-1, 2) + N  + 2
        #  Omega_index_end_for_Sean_param <- n_class*1*n_tests + N*n_tests + 1 + n_class*(n_tests-1) + n_class * choose(n_tests-1, 2) + N  + 1 + num_elements_Omegas
        
        
        if (model_type == "LT") { 
          as_index <- 2:(n_tests * n_class + 1)
          bs_index <-  seq(from =  (tail(as_index, 1) + 1) ,  by = 1, length = (length(as_index)) )  #   (n_tests * n_class + 2):(n_tests * n_class + 2 + n_class * choose(n_tests, 2) - 1) #  Omega_index_start_for_Sean_param:Omega_index_end_for_Sean_param
          prev_index <-  tail(bs_index, 1) + 1    #   n_class*1*n_tests + N*n_tests + 2 
          
          
          param_index  <- c(coeff_index,  bs_index,  prev_index)
        } else { 
          Omega_index <-  (n_tests * n_class + 2):(n_tests * n_class + 2 + n_class * choose(n_tests, 2) - 1) #  Omega_index_start_for_Sean_param:Omega_index_end_for_Sean_param
          prev_index <-  tail(Omega_index, 1) + 1    #   n_class*1*n_tests + N*n_tests + 2 
          coeff_index <- 2:(n_tests * n_class + 1)
          
          param_index  <- c(coeff_index,  Omega_index,  prev_index)
        }
        
        
        
        superchain_ids = seq(from = 1, to = n_chains, by = 1)
        if (n_chains > 4)  superchain_ids = c(rep(1, n_chains/2), rep(2, n_chains/2))
        if (n_chains > 15)  superchain_ids = c(rep(1, n_chains/4), rep(2, n_chains/4), rep(3, n_chains/4), rep(4, n_chains/4))
        if (n_chains > 47)  superchain_ids = c(rep(1, n_chains/8), rep(2, n_chains/8), rep(3, n_chains/8), rep(4, n_chains/8), 
                                               rep(5, n_chains/8), rep(6, n_chains/8), rep(7, n_chains/8), rep(8, n_chains/8))
        
        stan_draws_array[,, 31]
        
        
        rhats_nested <-  rhats <- c()
        for (i in 1:length(param_index)) {
          rhats_nested[i] <-   posterior::rhat_nested( array(c(stan_draws_array[,,param_index[i]]), dim = c(iter_sampling, n_chains)) , superchain_ids =superchain_ids )
          rhats[i] <-   posterior::rhat( array(c(stan_draws_array[,,param_index[i]]), dim = c(iter_sampling, n_chains)) )
        }
        
        
        {
          i = 13
          plot(posterior::as_draws ( array(c(stan_draws_array[,,param_index[i]]), dim = c(iter_sampling, n_chains))  )[,1])
          for (iii in 1:n_chains) {
            lines(posterior::as_draws ( array(c(stan_draws_array[,,param_index[i]]), dim = c(iter_sampling, n_chains))  )[,iii], col = iii)
          }
        }
        
        print(round(max(rhats_nested, na.rm = TRUE), 3))
        print(round(max(rhats, na.rm = TRUE), 3))
        
        
        # save efficiency summary info only
        file_name <- paste0("efficiency_info_", "seed_", seed, "_",
                            "Mod_", model_type,   
                            "PO_", prior_only,   "_",  
                            prior_lkj[1],"and",  prior_lkj[2],  "priorLKJ_", 
                            prior_param_3, "PP3_",
                            corr_force_positive, "posCOR_",
                            corr_prior_beta, "skewBpi_", 
                            N, "N_", 
                            n_chains, "chains_", 
                            iter_warmup, "burn_",  
                            iter_sampling, "samp_", 
                            adapt_delta, "ad_", 
                            max_treedepth, "maxtree_",
                            metric_type, "Mtype",".RDS")
        
        file_list <- list(
          time_stan_total_real_world, cmdstanr_model_out_timers,
          total_time_seconds,  total_time_mins, total_time_hours,
          pb_time_seconds, pb_time_mins, pb_time_hours,
          Min_ESS_per_sec_total_time,
          Min_ESS_per_sec_pb_time,# 10
          paste("total time =", round(total_time_seconds, 0), "seconds"),
          paste("total time =", floor(total_time_mins), "minutes and ", round(((total_time_mins - floor(total_time_mins))*60), 0), "seconds"),
          paste("total time =", floor(total_time_hours), "hours and ", round(((total_time_hours - floor(total_time_hours))*60), 0), "minutes"),
          paste("Sampling (post-burnin) time =", round(pb_time_seconds, 0), "seconds"),
          paste("Sampling (post-burnin) time =", floor(pb_time_mins), "minutes and ", round(((pb_time_mins - floor(pb_time_mins))*60), 0), "seconds"),
          paste("Sampling (post-burnin) time =", floor(pb_time_hours), "hours and ", round(((pb_time_hours - floor(pb_time_hours))*60), 0), "minutes"),
          paste("Min ESS / sec (total time) = ", round(Min_ESS_per_sec_total_time, 5)),
          paste("Min ESS / sec (sampling time only) = ", round(Min_ESS_per_sec_pb_time, 5)),
          mean_L_post_burnin,
          mean_L, # 20
          stan_min_ess_per_grad_eval * 1000,
          stan_min_ess_per_grad_eval_pb * 1000 ,
          round(max(rhats_nested, na.rm = TRUE), 4), # 23
          round(max(rhats, na.rm = TRUE), 4),  # 24
          round(max(cmdstanr_model_out_rhat), 4),
          Min_ESS,
          mean_eps_post_burnin, 
          (paste("Max L across chains (burnin)  = ", max_of_mean_Ls_per_chain_burn)),
          (paste("Max L across chains (post-burnin) = ", max_of_mean_Ls_per_chain_samp)) , # 29
          (paste("total_divs = ", total_divs))
        )
        
        saveRDS(file_list, file = file_name)
        
        
        # # save partial cmdstanr output - table for main params only
        file_name <- paste0("Summary_param_table_main_", "seed_", seed, "_",   "Mod_", model_type,   "PO_", prior_only,    "_", prior_lkj[1],"and",  prior_lkj[2],  "priorLKJ_",  prior_param_3, "PP3_",  corr_force_positive, "posCOR_", corr_prior_beta, "skewBpi_",    N, "N_", n_chains, "chains_", iter_warmup, "burn_",  iter_sampling, "samp_",
                            adapt_delta, "ad_", max_treedepth, "maxtree_", metric_type, "Mtype")
        saveRDS(cmdstanr_model_out, file = file_name)
        
        
        
        
        # save full cmdstanr output (files may be massive!!!)
        file_name <- paste0("seed_", seed, "_", "Mod_", model_type,  "PO_", prior_only,  "_",  prior_lkj[1], "and",  prior_lkj[2],  "priorLKJ_", prior_param_3, "PP3_",   corr_force_positive, "posCOR_", corr_prior_beta, "skewBpi_",   N, "N_", n_chains, "chains_", iter_warmup, "burn_",  iter_sampling, "samp_",
                            adapt_delta, "ad_", max_treedepth, "maxtree_", metric_type, "Mtype")
        model$save_object(file = file_name)
        
        
        
        
        
        # try({
        # 
        # 
        # {
        # 
        # L_burn_vec <- c()
        # L_samp_vec <- c()
        # R_hat_vec <- c()
        # R_hat_nested_vec <- c()
        # ESS_vec <- c()
        # Time_burn_vec <- c()
        # Time_samp_vec <- c()
        # Samp_ESS_per_sec_vec <- c()
        # Samp_ESS_per_grad_vec <- c()
        # N_grad_evals_relative_for_target_ESS_8_chains <- c()
        # N_grad_evals_relative_for_target_ESS_64_chains <- c()
        # N_iter_for_target_ESS_8_chains <- c()
        # 
        #     n_burnin <- 1000
        # #   n_burnin <- 525
        #  #   n_burnin <- 275
        #   #     n_burnin <- 200
        # 
        #   n_sampling <- 500
        # 
        #   N <- 5000
        # 
        # for (seed_i in 1:5)  {
        # 
        #          ###  seed_1_Mod_MVPPO_0_10and2priorLKJ_0PP3_0posCOR_0skewBpi_5000N_32chains_525burn_500samp_0.8ad_10maxtree_diag_eMtype
        # 
        #           file_name_0 <-   paste0("seed_",
        #                                 seed_i,
        #                                 "_Mod_MVPPO_0_10and2priorLKJ_0PP3_0posCOR_0skewBpi_",
        #                                 N,
        #                                 "N_32chains_",
        #                                 n_burnin,
        #                                 "burn_500samp_0.8ad_10maxtree_diag_eMtype")
        # 
        #           model <-  readRDS(file_name_0)
        # 
        # 
        # 
        #           # # # Min ESS / grad (uses post-burnin / sampling time ONLY)
        #           # mean_L  <-   (mean(c(    2^model$sampler_diagnostics(inc_warmup = TRUE)[1:n_burnin,,1] - 1    )))  # mean L (total)
        #           # print(paste("Mean L across chains (burnin)  = ", mean_L))
        #           #
        #           # mean_L_post_burnin  <-   (max(c(  2^model$sampler_diagnostics()[,,1] - 1  ))) # mean # of Leapfrog steps (sampling only)
        #           # print(paste("Mean L across chains (post-burnin) = ", mean_L_post_burnin))
        # 
        #           L_samp_means_per_chain <-  c()
        #           L_burn_means_per_chain <-  c()
        #           for (i in 1:32) {
        #             L_burn_means_per_chain[i]    <-  mean ( (  2^model$sampler_diagnostics(inc_warmup = TRUE)[1:n_burnin,,1] - 1  )[ ,i,1])
        #             L_samp_means_per_chain[i]    <-  mean ( (  2^model$sampler_diagnostics()[,,1] - 1  )[1:n_sampling,i,1])
        #           }
        #           #
        # 
        #           max_of_mean_Ls_per_chain_burn <-      max(L_burn_means_per_chain)
        #           max_of_mean_Ls_per_chain_samp <-      max(L_samp_means_per_chain)
        # 
        # 
        # 
        #           file_name <-   paste0("efficiency_info_seed_",
        #                                 seed_i,
        #                                 "_Mod_MVPPO_0_10and2priorLKJ_0PP3_0posCOR_0skewBpi_",
        #                                 N,
        #                                 "N_32chains_",
        #                                 n_burnin,
        #                                 "burn_500samp_0.8ad_10maxtree_diag_eMtype.RDS")
        # 
        #              model <-  readRDS(file_name)
        #             {
        #               # print(round(model[[3]], 0))
        #               # print(model[[14]])
        #               # print(model[[17]])
        #               print(paste("| ---------------------------------------------- |"))
        #               L_burn_vec[seed_i] <- print(round(max_of_mean_Ls_per_chain_burn, 0))
        #               L_samp_vec[seed_i] <- print(round(max_of_mean_Ls_per_chain_samp, 0))
        #               R_hat_vec[seed_i] <- print(round(model[[24]], 3) )
        #               R_hat_nested_vec[seed_i] <- print(round(model[[23]], 3) )
        #               print(paste("ESS = ", round(model[[26]], 0)) )
        #               ESS_vec[seed_i] <-  round(model[[26]], 0)
        #               print(paste("burnin time = ", signif( (model[[3]]  -  model[[6]])/60, 3)) )
        #               Time_burn_vec[seed_i] <- signif( (model[[3]]  -  model[[6]])/60, 3 )
        #               print(paste("samp. time = ", signif( model[[6]] / 60, 3)) )
        #               Time_samp_vec[seed_i] <-   signif( model[[6]] / 60, 3 )
        #               print(model[[18]])
        #               Samp_ESS_per_sec_vec[seed_i] <- signif((model[[10]]), 4)
        #               Samp_ESS_per_grad_vec[seed_i] <- print(signif(model[[22]], 3) )
        #               if (N == 500)  target_ESS = 5000
        #               if (N == 1000) target_ESS = 5000
        #               if (N == 2500) target_ESS = 2500
        #               if (N == 5000) target_ESS = 2000
        #               N_grad_evals_relative_for_target_ESS_8_chains[seed_i]  <- ( n_burnin   *  L_burn_vec[seed_i]    +
        #                                                                           n_sampling * L_samp_vec[seed_i] * (target_ESS /  ESS_vec[seed_i]) * (32 / 8)  ) / 1000
        #               N_grad_evals_relative_for_target_ESS_64_chains[seed_i] <- ( n_burnin   *  L_burn_vec[seed_i]    +
        #                                                                           n_sampling * L_samp_vec[seed_i] * (target_ESS /  ESS_vec[seed_i]) * (32 / 64) ) / 1000
        # 
        #               print(signif(     N_grad_evals_relative_for_target_ESS_8_chains[seed_i], 4) )
        #               print(signif(     N_grad_evals_relative_for_target_ESS_64_chains[seed_i], 4) )
        # 
        #               N_iter_for_target_ESS_8_chains[seed_i] <- print(   (32/8) * ceiling(n_sampling * ( target_ESS / ESS_vec[seed_i])))
        # 
        #               print(paste("| ---------------------------------------------- |"))
        #             }
        # 
        #         }
        # 
        #         {
        #         print(paste("| ------------------------------------------------------ |"))
        #         print(paste("| ------ Summary Estimates (means of the runs)  -------- |"))
        #         print(paste("| ------------------------------------------------------ |"))
        #         print(paste0("L_burn = ", round(mean(L_burn_vec), 0), ", SD = (", round(sd(L_burn_vec), 0), ")" ) )
        #         print(paste0("L_samp = ", round(mean(L_samp_vec), 0), ", SD = (", round(sd(L_samp_vec), 0), ")" ) )
        #         print(paste0("R_hat = ", round(mean(R_hat_vec), 3), ", SD = (", round(sd(R_hat_vec), 3), ")" ) )
        #         print(paste0("R_hat_nested = ", round(mean(R_hat_nested_vec), 3), ", SD = (", round(sd(R_hat_nested_vec), 3), ")" ) )
        #         print(paste0("ESS = ", round(mean(ESS_vec), 0), ", SD = (", round(sd(ESS_vec), 0), ")" ) )
        #       #  print(paste0("Time_burn = ", signif(mean(Time_burn_vec), 4), ", SD = (", signif(sd(Time_burn_vec), 4), ")" ) )
        #        # print(paste0("Time_samp = ", signif(mean(Time_samp_vec), 4), ", SD = (", signif(sd(Time_samp_vec), 4), ")" ) )
        #        # print(paste0("Samp_ESS_per_sec = ", signif(mean(Samp_ESS_per_sec_vec), 4), ", SD = (", signif(sd(Samp_ESS_per_sec_vec), 4), ")" ) )
        #         print(paste0("Samp_ESS_per_grad = ", signif(mean(Samp_ESS_per_grad_vec), 4), ", SD = (", signif(sd(Samp_ESS_per_grad_vec), 4), ")" ) )
        #         print(paste0("N_grad_evals_relative_for_target_ESS_8_chains = ", signif(mean(N_grad_evals_relative_for_target_ESS_8_chains), 4), ", SD = (", signif(sd(N_grad_evals_relative_for_target_ESS_8_chains), 4), ")" ) )
        #         print(paste0("N_grad_evals_relative_for_target_ESS_64_chains = ", signif(mean(N_grad_evals_relative_for_target_ESS_64_chains), 4), ", SD = (", signif(sd(N_grad_evals_relative_for_target_ESS_64_chains), 4), ")" ) )
        #         print(paste0("N_iter_for_target_ESS_8_chains = ", round(mean(N_iter_for_target_ESS_8_chains), 0), ", SD = (", round(sd(N_iter_for_target_ESS_8_chains), 0), ")" ) )
        # 
        #         }
        # 
        # }
        # 
        # })
        
        
        
        
        # 
        # const double x_i = -0.3418*log( 1.0/x - 1.0);
        # 
        # 
        # const double exp_x_i = exp(0.33333333333333331483 * log( x_i  +  std::sqrt(  x_i^2  + 1 ) ));
        # const double exp_2x_i = exp_x_i*exp_x_i;
        # return  2.74699999999999988631 * ( (exp_2x_i  - 1.0) / exp_x_i ) ;  //   now do sinh parth part
        # 
        # # model <-  readRDS("Mplus_efficiency_info_seed_1_10_24prior_IW_25000N_64chains_.RDS")
        # # {
        # #   print(model[[2]])
        #   print(round(model[[4]], 0))
        #   print(model[[13]])
        #   print(signif(model[[11]], 3))
        #   print(signif(model[[12]], 3))
        # }
        
        
        
        
        
        
      })
      
    }
    
  }
  
}



N = 500 ; target_ESS = 5000  ; n_chains_target = 8 ;  actual_ESS = 3117  # Laptop
N = 500 ; target_ESS = 5000  ; n_chains_target = 64 ; actual_ESS = 2803  # HPC

N = 1000 ; target_ESS = 5000  ; n_chains_target = 8 ;  actual_ESS = 1368  # Laptop
N = 1000 ; target_ESS = 5000  ; n_chains_target = 64 ; actual_ESS = 1097  # HPC

N = 2500 ; target_ESS = 2500  ; n_chains_target = 8 ;  actual_ESS = 200   # Laptop
N = 2500 ; target_ESS = 2500  ; n_chains_target = 64 ; actual_ESS = 200   # HPC

burn_time =  (1020   -  439 ) / 60
pb_time =    ( 439 ) / 60

1.0 * (burn_time * ifelse(n_chains_target == 8, 1, 1) +   (  pb_time   * (target_ESS/actual_ESS) * (32 / n_chains_target) )  )


#  ESS_target <- 2000       
ESS_target <- 2500

(7.5 - 1.2)   +    ( ((ESS_target/(26.16*1000/1))*1000) / 60 )  # N_burn = 1000, TD = 10
(7.1 - 1.3)  +     ( ((ESS_target/(22.96*1000/1))*1000) / 60 )  # N_burn = 1000, TD = 8

(5.5 - 1.5)  +    ( ((ESS_target/(19.71*1000/1))*1000) / 60 )  # N_burn = 500, TD = 10
(5.3 - 1.3)  +    ( ((ESS_target/(21.90*1000/1))*1000) / 60 )  # N_burn = 500, TD = 8

(4.7 - 1.3)  +    ( ((ESS_target/(22.91*1000/1))*1000) / 60 )  # N_burn = 250, TD = 10
(4.5 - 1.4)  +    ( ((ESS_target/(20.12*1000/1))*1000) / 60 )  # N_burn = 250, TD = 8

(4.7 - 1.3)  +    ( ((ESS_target/(22.91*1000/1))*1000) / 60 )  # N_burn = 150, TD = 10
### (4.5 - 1.4)  +    ( ((ESS_target/(20.12*1000/1))*1000) / 60 )  # N_burn = 150, TD = 8


(4.7 - 1.3)  +    ( ((ESS_target/(22910/10))*1000) / 60 )  # N_burn = 250, TD = 10


#   
# plots for LKJ / skewed-beta priors  -----------------------
prior_lkj


model_type_vec <- c(rep("MVP", 6), rep("LT", 4))
prior_lkj_indicator_vec <- c(rep(1, 2), rep(1, 2), rep(0, 2), 
                             rep(1, 4))
prior_beta_indicator_vec  <-  ifelse(prior_lkj_indicator_vec == 0, 1, 0)


corr_force_positive_vec <- c(rep(0, 2), rep(1, 2), rep(0, 2), 
                             rep(1, 4))


#  prior_lkj <- c(3.30,  6.993)
#    prior_lkj <- c(3.69,  9.091)


prior_param_1_d_vec <- c(1.5, 4,
                         1.5, 4,
                         2.25,  4.75, 
                         1.33, 1.45, 1, 5)
prior_param_2_d_vec <- c(1.5, 4,
                         1.5, 4,
                         0.95,  3.75, 
                         1.25, 0.881, 1, 5)
prior_param_3_d_vec <-  c(0, 0,
                          0, 0,
                          0,  0, 
                          0, 3.33, 0, 5)


prior_param_1_nd_vec <- c(10, 24,
                          10, 24,
                          23.15,  26.25, 
                          1.52, 1.59, 1, 5)
prior_param_2_nd_vec <- c(10, 24,
                          10, 24,
                          17.15, 23.75, 
                          0.633, 0.468, 1, 5)
prior_param_3_nd_vec <-  c(0, 0,
                           0, 0,
                           0,  0, 
                           3.83, 6.32, 0, 5)




df_prior_list_d <- df_prior_list_nd <- list()

for (df_num in 1:length(corr_force_positive_vec)) {
  
  try({
    
    file_name <- paste0("seed_", seed, "_",    "Mod_", model_type_vec[df_num],   "PO_", prior_only,    "_",  prior_param_1_d_vec[df_num], "and",  prior_param_2_d_vec[df_num],  "priorLKJ_", prior_param_3_d_vec[df_num], "PP3_",   corr_force_positive_vec[df_num],   
                        "posCOR_", prior_beta_indicator_vec[df_num], "skewBpi_",  N, "N_", n_chains, "chains_", iter_warmup, "burn_",  iter_sampling, "samp_",
                        adapt_delta, "ad_", max_treedepth, "maxtree_", metric_type, "Mtype")
    
    full_model_out <- readRDS(file_name)
    
    if (model_type_vec[df_num] == "MVP")   corr_draws <- (full_model_out$draws()[,, n_class * choose(n_tests, 2)  + 4 ])
    else   corr_draws <-     (full_model_out$draws()[,, n_class * choose(n_tests, 2)  +   n_tests*n_tests*n_class + 1 ])
    
    
    
    total_sims <- n_chains * 25000
    prior_beta_indicator <- rep(prior_beta_indicator_vec[df_num], total_sims)
    prior_lkj_indicator  <-  ifelse(prior_beta_indicator == 0, 1, 0)
    prior_param_1 <- rep( prior_param_1_d_vec[df_num], total_sims)
    prior_param_2 <- rep(prior_param_2_d_vec[df_num], total_sims)
    prior_param_3 <-  rep(prior_param_3_d_vec[df_num], total_sims)
    force_positive <- rep( corr_force_positive_vec[df_num], total_sims)
    model =  rep( model_type_vec[df_num], total_sims)
    
    df_prior_list_d[[df_num]] <-  tibble(prior_beta_indicator = prior_beta_indicator,
                                         prior_lkj_indicator = prior_lkj_indicator,
                                         prior_param_1 = prior_param_1,
                                         prior_param_2 = prior_param_2,
                                         prior_param_3 = prior_param_3,
                                         force_positive = force_positive,
                                         model = model,
                                         draws = as.numeric(corr_draws[, , 1]))
    
    
    file_name <- paste0("seed_", seed, "_",  "Mod_",  model_type_vec[df_num],   "PO_", prior_only,    "_",  prior_param_1_nd_vec[df_num], "and",  prior_param_2_nd_vec[df_num],  "priorLKJ_",    prior_param_3_nd_vec[df_num], "PP3_",  
                        corr_force_positive_vec[df_num],       "posCOR_", prior_beta_indicator_vec[df_num], "skewBpi_",  N, "N_", n_chains, "chains_", iter_warmup, "burn_",  iter_sampling, "samp_",
                        adapt_delta, "ad_", max_treedepth, "maxtree_", metric_type, "Mtype")
    
    full_model_out <- readRDS(file_name)
    
    
    if (model_type_vec[df_num] == "MVP")   corr_draws <- (full_model_out$draws()[,, n_class * choose(n_tests, 2)  + 4 ])
    else   corr_draws <-     (full_model_out$draws()[,, n_class * choose(n_tests, 2)  +   n_tests*n_tests*n_class + 1 ])
    
    
    total_sims <- n_chains * 25000
    prior_beta_indicator <- rep(prior_beta_indicator_vec[df_num], total_sims)
    prior_lkj_indicator  <-  ifelse(prior_beta_indicator == 0, 1, 0)
    prior_param_1 <- rep( prior_param_1_nd_vec[df_num], total_sims)
    prior_param_2 <- rep(prior_param_2_nd_vec[df_num], total_sims)
    prior_param_3 <-  rep(prior_param_3_nd_vec[df_num], total_sims)
    force_positive <- rep( corr_force_positive_vec[df_num], total_sims)
    model =  rep( model_type_vec[df_num], total_sims)
    
    df_prior_list_nd[[df_num]] <-  tibble(prior_beta_indicator = prior_beta_indicator,
                                          prior_lkj_indicator = prior_lkj_indicator,
                                          prior_param_1 = prior_param_1,
                                          prior_param_2 = prior_param_2,
                                          prior_param_3 = prior_param_3,
                                          force_positive = force_positive,
                                          model = model,
                                          draws = as.numeric(corr_draws[, , 1]))
    
    
  })
}




df_prior_list_d
df_prior_list_nd

require(data.table)
require(ggplot2)
require(patchwork)


# df_prior_list_d[[df_num + 1]] <- tibble(prior_beta_indicator = rep(0, total_sims),
#                                         prior_lkj_indicator =  rep(1, total_sims),
#                                         prior_param_1 =  rep(0, total_sims),
#                                         prior_param_2 =  rep(0, total_sims),
#                                         force_positive =  rep(1, total_sims),
#                                         draws = corr_prior_dist_LT_1)
# 


df_prior_combined_d <-  tibble(data.table::rbindlist(df_prior_list_d))
# 
# df_prior_combined_d <- dplyr::filter(df_prior_combined_d,force_positive == 1, prior_param_1 != 5 )

## LKJ plot
df_prior_combined_LKJ_d  <- dplyr::filter(df_prior_combined_d, prior_lkj_indicator == 1, model == "MVP") %>%
  dplyr::mutate(prior_pars_LKJ = factor(prior_param_1)) %>%
  dplyr::mutate(`corr > 0` = factor(force_positive), 
                prior  = paste0("Omega ~ ", "LKJ(", prior_param_1, ")"))


prior_plots_LKJ_d <- ggplot(df_prior_combined_LKJ_d, aes(x = draws, linetype = `corr > 0` , colour = prior  )) +
  theme_bw() + 
  theme(text  =   element_text(size = 18)) + 
  geom_density(linewidth = 2)  +
  xlim(-1, 1)  #+ 
#  theme(legend.position="none")  
prior_plots_LKJ_d



## beta plot
df_prior_combined_beta_d <- dplyr::filter(df_prior_combined_d, prior_beta_indicator == 1)  %>%
  dplyr::mutate(beta_prior_ratio = factor(paste0("Beta_a_b_ratio = ", prior_param_1 / prior_param_2)),
                prior  = paste0("Omega[i,j] ~ ",  "B(", prior_param_1, ",", prior_param_2, ")"))


prior_plots_skewed_Beta_d <- ggplot(df_prior_combined_beta_d, aes(x = draws , colour = prior   )) +
  theme_bw() + 
  theme(text  =   element_text(size = 18)) + 
  geom_density(linewidth = 2)   +
  xlim(-1, 1)  # + 
#   theme(legend.position="none")  
#  facet_wrap(~ beta_prior_ratio )
prior_plots_skewed_Beta_d



df_prior_combined_nd <-  tibble(data.table::rbindlist(df_prior_list_nd))


## LKJ plot
df_prior_combined_LKJ_nd  <- dplyr::filter(df_prior_combined_nd, prior_lkj_indicator == 1, model == "MVP") %>%
  dplyr::mutate(prior_pars_LKJ = factor(prior_param_1)) %>%
  dplyr::mutate(`corr > 0` = factor(force_positive), 
                prior  = paste0("Omega ~ ",  "LKJ(", prior_param_1, ")"))

prior_plots_LKJ_nd <- ggplot(df_prior_combined_LKJ_nd, aes(x = draws, linetype = `corr > 0` , colour = prior  )) +
  theme_bw() + 
  theme(text  =   element_text(size = 18)) + 
  geom_density(linewidth = 2)  +
  xlim(-1, 1) # + 
# theme(legend.position="none")  
prior_plots_LKJ_nd


## beta plot
df_prior_combined_beta_nd <- dplyr::filter(df_prior_combined_nd, prior_beta_indicator == 1)  %>%
  dplyr::mutate(beta_prior_ratio = factor(paste0("Beta_a_b_ratio = ", prior_param_1 / prior_param_2)),
                prior  = paste0("Omega[i,j] ~ ", "B(", prior_param_1, ",", prior_param_2, ")"))


prior_plots_skewed_Beta_nd <- ggplot(df_prior_combined_beta_nd, aes(x = draws , colour = prior   )) +
  theme_bw() + 
  theme(text  =   element_text(size = 18)) + 
  geom_density(linewidth = 2)   +
  xlim(-1, 1)  

#  facet_wrap(~ beta_prior_ratio )
prior_plots_skewed_Beta_nd


prior_plots_LKJ_d + prior_plots_skewed_Beta_d +  
  prior_plots_LKJ_nd + prior_plots_skewed_Beta_nd + 
  patchwork::plot_layout(ncol = 2)


# Priors plot 1 - plot showing all 6 MVP prior dists    ---------------------------------------------------------------------------------------------------------------------  
png("Figure_all_skewedBeta_and_LKJ_priors.png" ,units = "in", width = 18, height=9, res=800)
prior_plots_LKJ_d + prior_plots_skewed_Beta_d +  
  prior_plots_LKJ_nd + prior_plots_skewed_Beta_nd + 
  patchwork::plot_layout(ncol = 2)
dev.off()
# ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------




## LT_LC prior plot
df_prior_combined_LT_d  <- dplyr::filter(df_prior_combined_d, prior_lkj_indicator == 1, model == "LT") %>%
  dplyr::mutate( `corr > 0` = factor(force_positive))

#  filter(df_prior_combined_LT_d, prior_param_1 == 0.205)
df_prior_combined_LT_d <-  df_prior_combined_LT_d %>% dplyr::mutate( prior = case_when( prior_param_1  == 1.33 &&    prior_param_2 ==  1.25   ~     paste0("b ~ Weibull(", prior_param_1, ",", prior_param_2,  ")" ), 
                                                                                        prior_param_1 ==  1   &&    prior_param_2 ==  1  ~  paste0("b ~ Gamma(", 1, ",", 1, ")"),
                                                                                        prior_param_1 ==  1.45  &&    prior_param_2 ==  0.881   ~    paste0("b ~ Weibull(", prior_param_1, ",", prior_param_2, ")") , 
                                                                                        prior_param_1 ==  5   &&    prior_param_2 ==   5   ~    paste0("b ~ Uniform(", 0, ",", 5, ")") ))

unique(  df_prior_combined_LT_d$prior)
df_prior_combined_LT_d$prior <-  factor(  df_prior_combined_LT_d$prior , levels = c(paste0("b ~ Gamma(", 1, ",", 1, ")"),  
                                                                                    (paste0("b ~ Uniform(", 0, ",", 5, ")")), 
                                                                                    paste0("b ~ Weibull(", 1.45, ",", 0.881, ")"),  
                                                                                    paste0("b ~ Weibull(", 1.33, ",", 1.25, ")")))

unique(  df_prior_combined_LT_d$prior)
# 
# filter(df_prior_combined_LT_d, prior_param_1 == 0.205)

prior_plots_LT_LC_d <- ggplot(df_prior_combined_LT_d, aes(x = draws, linetype =  `corr > 0` , colour = prior  )) +
  theme_bw() + 
  theme(text  =   element_text(size = 18)) + 
  geom_density(linewidth = 2)  +
  xlim(-0.1, 1)     + 
  ggtitle("LT-LC priors (diseased class)")
# theme(legend.position="none")  

prior_plots_LT_LC_d



## LT_LC prior plot
df_prior_combined_LT_nd  <- dplyr::filter(df_prior_combined_nd, prior_lkj_indicator == 1, model == "LT") %>%
  #  dplyr::mutate( prior   = factor(paste0("b ~ Gamma(", prior_param_1, ",", prior_param_2, ")"))) %>%
  #   dplyr::mutate( prior   = expression(paste0(  b^2,   " ~ Gamma(", prior_param_1, ",", prior_param_2, ")"))) %>%
  dplyr::mutate( `corr > 0` = factor(force_positive)) 



df_prior_combined_LT_nd <-  df_prior_combined_LT_nd %>% dplyr::mutate( prior = case_when( prior_param_1 %in% c(1.52, 1.59) ~     (paste0("b ~ Weibull(", prior_param_1, ",", prior_param_2,  ")" )), 
                                                                                          prior_param_1 ==  1   ~  (paste0("b ~ Gamma(", prior_param_1, ",", prior_param_2, ")"))))


unique(  df_prior_combined_LT_nd$prior)
df_prior_combined_LT_nd$prior <-  factor(  df_prior_combined_LT_nd$prior , levels = c(paste0("b ~ Gamma(", 1, ",", 1, ")"),  
                                                                                      (paste0("b ~ SN(", 0.0794, ",",0.418, ",", 6.32,  ")" )),  
                                                                                      (paste0("b ~ SN(", 0.155, ",",0.486, ",", 3.83,  ")" ))))

unique(  df_prior_combined_LT_nd$prior)




prior_plots_LT_LC_nd <- ggplot(df_prior_combined_LT_nd, aes(x = draws, linetype =  `corr > 0` , colour = prior  , parse = TRUE) , parse = TRUE ) +
  theme_bw() + 
  theme(text  =   element_text(size = 18)) + 
  geom_density(linewidth = 2)  +
  xlim(-0.1, 1)  + 
  ggtitle("LT-LC priors (non-diseased class)")
prior_plots_LT_LC_nd


prior_plots_LT_LC_d + prior_plots_LT_LC_nd +  
  patchwork::plot_layout(ncol = 1)

# Priors plot 2    ---------------------------------------------------------------------------------------------------------------------  
png("Figure_all_LT_LC_priors.png" ,units = "in", width = 12, height=12, res=800)
prior_plots_LT_LC_d + prior_plots_LT_LC_nd +  
  patchwork::plot_layout(ncol = 1)
dev.off()
# --------------------------------------------------------------------------------------------------------------------------------------




# ---------D class
df_prior_combined_LT_d <- df_prior_combined_LT_d %>% dplyr::mutate(prior_pars_LKJ = rep( NA, length(df_prior_combined_LT_d$prior_beta_indicator) ))

df_prior_combined_LT_d$prior <- factor(df_prior_combined_LT_d$prior)
df_prior_combined_LKJ_d$prior <- factor(df_prior_combined_LKJ_d$prior)

unique(df_prior_combined_LT_d$prior)
unique(df_prior_combined_LKJ_d$prior)

df_prior_combined_LT_and_MVP_force_pos_compare_d <- rbind( df_prior_combined_LT_d, df_prior_combined_LKJ_d) %>%
  dplyr::filter(prior_param_1 != 1) %>% 
  dplyr::filter(force_positive == 1)

unique(df_prior_combined_LT_and_MVP_force_pos_compare_d$force_positive)






unique(df_prior_combined_LT_and_MVP_force_pos_compare_d$prior)

prior_plots_combined_LT_and_MVP_force_pos_compare_d_1 <- ggplot( dplyr::filter(df_prior_combined_LT_and_MVP_force_pos_compare_d, prior_param_1 %in% c(1.5, 1.845 )), 
                                                                 aes(x = draws, linetype =  model, colour = prior, parse = TRUE )  ) +
  theme_bw() + 
  geom_density(linewidth = 1.5)  +
  theme(text  =   element_text(size = 18)) + 
  xlim(-0.1, 1)  
prior_plots_combined_LT_and_MVP_force_pos_compare_d_1


prior_plots_combined_LT_and_MVP_force_pos_compare_d_2 <- ggplot( dplyr::filter(df_prior_combined_LT_and_MVP_force_pos_compare_d,  prior_param_1 %in% c(4, 0.205 )), 
                                                                 aes(x = draws, linetype =  model, colour = prior, parse = TRUE )  ) +
  theme_bw() + 
  geom_density(linewidth = 1.5)  +
  theme(text  =   element_text(size = 18)) + 
  xlim(-0.1, 1)  
prior_plots_combined_LT_and_MVP_force_pos_compare_d_2


prior_plots_combined_LT_and_MVP_force_pos_compare_d <- 
  prior_plots_combined_LT_and_MVP_force_pos_compare_d_1 + 
  prior_plots_combined_LT_and_MVP_force_pos_compare_d_2 +  
  patchwork::plot_layout(ncol = 1) + 
  plot_annotation(
    title = 'LC-LT vs. LC-MVP "approximately equivelent" priors',
    subtitle = 'Diseased latent class (class 2)',
    # caption = 'NOTE: LC-LT prior prameters were found using a Stan model fitted using the LC-MVP prior densities as a data input'
  ) & 
  theme(text = element_text(size = 18))
prior_plots_combined_LT_and_MVP_force_pos_compare_d

# -----ND  class 
df_prior_combined_LT_nd <- df_prior_combined_LT_nd %>% dplyr::mutate(prior_pars_LKJ = rep( NA, length(df_prior_combined_LT_nd$prior_beta_indicator) ))


df_prior_combined_LT_and_MVP_force_pos_compare_nd <- rbind( df_prior_combined_LT_nd, df_prior_combined_LKJ_nd) %>%
  dplyr::filter(prior_param_1 != 1) %>% 
  dplyr::filter(force_positive == 1)

unique(df_prior_combined_LT_and_MVP_force_pos_compare_nd$force_positive)



prior_plots_combined_LT_and_MVP_force_pos_compare_nd_1 <- ggplot( dplyr::filter(df_prior_combined_LT_and_MVP_force_pos_compare_nd, prior_param_1 %in% c(10, 0.155 )), 
                                                                  aes(x = draws, linetype =  model, colour = prior, parse = TRUE )  ) +
  theme_bw() + 
  geom_density(linewidth = 1.5)  +
  theme(text  =   element_text(size = 18)) + 
  xlim(-0.1, 1)  
prior_plots_combined_LT_and_MVP_force_pos_compare_nd_1


prior_plots_combined_LT_and_MVP_force_pos_compare_nd_2 <- ggplot( dplyr::filter(df_prior_combined_LT_and_MVP_force_pos_compare_nd,  prior_param_1 %in% c(24, 0.0794 )), 
                                                                  aes(x = draws, linetype =  model, colour = prior, parse = TRUE )  ) +
  theme_bw() + 
  geom_density(linewidth = 1.5)  +
  theme(text  =   element_text(size = 18)) + 
  xlim(-0.1, 1)  
prior_plots_combined_LT_and_MVP_force_pos_compare_nd_2

prior_plots_combined_LT_and_MVP_force_pos_compare_nd <-
  prior_plots_combined_LT_and_MVP_force_pos_compare_nd_1 + 
  prior_plots_combined_LT_and_MVP_force_pos_compare_nd_2 +  
  patchwork::plot_layout(ncol = 1) + 
  plot_annotation(
    title = 'LC-LT vs. LC-MVP "approximately equivelent" priors',
    subtitle = 'Non-diseased latent class (class 1)',
    # caption = 'NOTE: LC-LT prior prameters were found using a Stan model fitted using the LC-MVP prior densities as a data input'
  ) & 
  theme(text = element_text(size = 18))
prior_plots_combined_LT_and_MVP_force_pos_compare_nd



# Priors plot 3    ---------------------------------------------------------------------------------------------------------------------  
png("Figure_LT_LC_vs_LC_MVP_approx_equiv_priors.png" ,units = "in", width = 15, height=8, res=1000)
prior_plots_combined_LT_and_MVP_force_pos_compare_d_and_nd_full_panel <-
  prior_plots_combined_LT_and_MVP_force_pos_compare_d_1 + 
  prior_plots_combined_LT_and_MVP_force_pos_compare_nd_1 + 
  prior_plots_combined_LT_and_MVP_force_pos_compare_d_2 +
  prior_plots_combined_LT_and_MVP_force_pos_compare_nd_2 +  
  patchwork::plot_layout(ncol = 2) + 
  plot_annotation(
    title = 'LC-LT vs. LC-MVP "approximately equivelent" priors',
    subtitle = 'Diseased latent class (class 2) is on the left panel and the non-diseased class is on the right',
    # caption = 'NOTE: LC-LT prior prameters were found using a Stan model fitted using the LC-MVP prior densities as a data input'
  ) & 
  theme(text = element_text(size = 18))
prior_plots_combined_LT_and_MVP_force_pos_compare_d_and_nd_full_panel
dev.off()
# --------------------------------------------------------------------------------------------------------------------------------------




#   
{
  
  
  try({
    
    # cmdstanr_model_out <- model$summary(variables = c(  "Omega", "L_Omega_raw_for_positive_constrain" ), "mean", "median", "sd", "mad",  ~quantile(.x, probs = c(0.025, 0.50, 0.975) ), "rhat" , "ess_bulk", "ess_tail")
    # print(cmdstanr_model_out, n = 200)
    
    cmdstanr_model_out <- model$summary(variables = c(  "Omega" ), "mean", "median", "sd", "mad",  ~quantile(.x, probs = c(0.025, 0.50, 0.975) ), "rhat" , "ess_bulk", "ess_tail")
    cmdstanr_model_out$`2.5%` <- round(cmdstanr_model_out$`2.5%`, 3)
    cmdstanr_model_out$`97.5%` <- round(cmdstanr_model_out$`97.5%`, 3)
    print(cmdstanr_model_out, n = 200)
    
    
    #cmdstanr_model_out <- model$summary(variables = c(  "rho" , "rho_2"), "mean", "median", "sd", "mad",  ~quantile(.x, probs = c(0.025, 0.50, 0.975) ), "rhat" , "ess_bulk", "ess_tail")
    # print(cmdstanr_model_out, n = 200)
    
    
    lower = round(cmdstanr_model_out$`2.5%`, 4)
    print(round(mean(lower[lower != 1]), 2))
    # sd(lower[lower != 1])
    
    upper = round(cmdstanr_model_out$`97.5%`, 4)
    print(round(mean(upper[upper != 1]), 2))
    # sd(upper[upper != 1])
    
    med = round(cmdstanr_model_out$`50%`, 3)
    print(round(mean(med[med != 1]), 2))
    # sd(med[med != 1])
    
    prior_lkj
  })
  
  
  try({
    cmdstanr_model_out <- model$summary(variables =  c("p", "Se_bin", "Sp_bin", "Omega_raw_off", "Omega_raw_diagonal"), "mean", "median", "sd", "mad",  ~quantile(.x, probs = c(0.025,  0.975) ), "rhat" , "ess_bulk", "ess_tail")
    print(cmdstanr_model_out, n = 100)
  })
  
  
  
  try({
    cmdstanr_model_out <- model$summary(variables =  c("p", "Se_bin", "Sp_bin"), "mean", "median", "sd", "mad",  ~quantile(.x, probs = c(0.025,  0.975) ), "rhat" , "ess_bulk", "ess_tail")
    print(cmdstanr_model_out, n = 100)
  })
  
  
  
}


prior_lkj




{
  modelr <- rstan::read_stan_csv(model$output_files())  # convert to rstan CSV
  
  # stan_trace(modelr, pars = c("beta", "p"))
  # stan_trace(modelr, pars = c("L_Omega"))
  
  stan_trace(modelr, pars = c("LT_a", "p"))
  
  
  # stan_dens(modelr, pars = c("Se_bin"))
  
  
  stan_dens(modelr, pars = c("Omega"))
}



quantile(rbeta(10000, 3,9), probs = c(0.025, 0.975))
quantile(rbeta(10000, 3,3), probs = c(0.025, 0.975))
quantile(rbeta(10000, 4,4), probs = c(0.025, 0.975))
quantile(rbeta(10000, 5,5), probs = c(0.025, 0.975))
quantile(rbeta(10000, 10,10), probs = c(0.025, 0.975))

{
  
  Se <- round(  (cmdstanr_model_out$median[seq(from = 2, to = n_tests + 1, by = 1)]), 3)
  Sp <- round(  (cmdstanr_model_out$median[seq(from =  n_tests + 2, to = n_tests * 2 + 1, by = 1)]), 3)
  prev <-  round(cmdstanr_model_out$median[1], 3)
  
  Se_lower <- round(  (cmdstanr_model_out$`2.5%`[seq(from = 2, to = n_tests + 1, by = 1)]), 3)
  Sp_lower <- round(  (cmdstanr_model_out$`2.5%`[seq(from =  n_tests + 2, to = n_tests * 2 + 1, by = 1)]), 3)
  prev_lower <-  round(cmdstanr_model_out$`2.5%`[1], 3)
  
  Se_upper <- round(  (cmdstanr_model_out$`97.5%`[seq(from = 2, to = n_tests + 1, by = 1)]), 3)
  Sp_upper <- round(  (cmdstanr_model_out$`97.5%`[seq(from =  n_tests + 2, to = n_tests * 2 + 1, by = 1)]), 3)
  prev_upper <-  round(cmdstanr_model_out$`97.5%`[1], 3)
  
  
  print(cat("Se = ",  Se))
  print(cat("Sp = ", Sp ))
  
  
  
  
  if (model_type == "LT") {
    
    cmdstanr_model_out_2 <- model$summary(variables =  c("Omega"), "mean", "median", "sd", "mad",  ~quantile(.x, probs = c(0.025,  0.975) ), "rhat" , "ess_bulk", "ess_tail")
    
  } else { 
    
    cmdstanr_model_out_2 <- model$summary(variables =  c("Omega"), "mean", "median", "sd", "mad",  ~quantile(.x, probs = c(0.025,  0.975) ), "rhat" , "ess_bulk", "ess_tail")
    
  }
  
  
  LT_Omega_nd_index <- seq(from = 1, to = 2 * n_tests^2, by = 2)  
  
  LT_Omega_nd_vec <-   cmdstanr_model_out_2$median[LT_Omega_nd_index] 
  if (tailored_corr_priors == FALSE)  LT_Omega_nd_vec <- unique(LT_Omega_nd_vec)
  LT_Omega_nd_vec <- LT_Omega_nd_vec[LT_Omega_nd_vec != 1]
  
  LT_Omega_nd_vec_lower <-   cmdstanr_model_out_2$`2.5%`[LT_Omega_nd_index] 
  if (tailored_corr_priors == FALSE)  LT_Omega_nd_vec_lower <- unique(LT_Omega_nd_vec_lower)
  LT_Omega_nd_vec_lower <- LT_Omega_nd_vec_lower[LT_Omega_nd_vec_lower != 1]
  
  LT_Omega_nd_vec_upper  <-   cmdstanr_model_out_2$`97.5%`[LT_Omega_nd_index] 
  if (tailored_corr_priors == FALSE)  LT_Omega_nd_vec_upper <- unique(LT_Omega_nd_vec_upper)
  LT_Omega_nd_vec_upper <- LT_Omega_nd_vec_upper[LT_Omega_nd_vec_upper != 1]
  
  LT_Omega_nd <- LT_Omega_nd_lower <- LT_Omega_nd_upper <- array(dim = c(n_tests, n_tests))
  
  
  counter <- 1 
  
  length_index_m1 <- 0 
  
  for (j in 1:(n_tests - 1)) { 
    
    index <-  (j+1):n_tests - 1  # ; print(index)
    print(index + length_index_m1)
    LT_Omega_nd[j,  index + 1 ] <- LT_Omega_nd_vec[index + length_index_m1]
    LT_Omega_nd_lower[j,  index + 1 ] <- LT_Omega_nd_vec_lower[index + length_index_m1]
    LT_Omega_nd_upper[j,  index + 1 ] <- LT_Omega_nd_vec_upper[index + length_index_m1]
    length_index_m1 <- length_index_m1 + ( length(index) - 1 )
    
  }
  LT_Omega_nd <- round(LT_Omega_nd, 3)
  LT_Omega_nd_upper <- round(LT_Omega_nd_upper, 3)
  LT_Omega_nd_upper <- round(LT_Omega_nd_upper, 3)
  
  
  
  
  LT_Omega_d_index <- seq(from = 2, to = 2 * n_tests^2, by = 2) 
  
  LT_Omega_d_vec <-   cmdstanr_model_out_2$median[LT_Omega_d_index] 
  LT_Omega_d_vec <- unique(LT_Omega_d_vec)
  LT_Omega_d_vec <- LT_Omega_d_vec[LT_Omega_d_vec != 1]
  
  LT_Omega_d_vec_lower <-   cmdstanr_model_out_2$`2.5%`[LT_Omega_d_index] 
  LT_Omega_d_vec_lower <- unique(LT_Omega_d_vec_lower)
  LT_Omega_d_vec_lower <- LT_Omega_d_vec_lower[LT_Omega_d_vec_lower != 1]
  
  LT_Omega_d_vec_upper  <-   cmdstanr_model_out_2$`97.5%`[LT_Omega_d_index] 
  LT_Omega_d_vec_upper <- unique(LT_Omega_d_vec_upper)
  LT_Omega_d_vec_upper <- LT_Omega_d_vec_upper[LT_Omega_d_vec_upper != 1]
  
  LT_Omega_d <- LT_Omega_d_lower <- LT_Omega_d_upper <- array(dim = c(n_tests, n_tests))
  
  
  counter <- 1 
  
  length_index_m1 <- 0 
  
  for (j in 1:(n_tests - 1)) { 
    
    index <-  (j+1):n_tests - 1  # ; print(index)
    print(index + length_index_m1)
    LT_Omega_d[j,  index + 1 ] <- LT_Omega_d_vec[index + length_index_m1]
    LT_Omega_d_lower[j,  index + 1 ] <- LT_Omega_d_vec_lower[index + length_index_m1]
    LT_Omega_d_upper[j,  index + 1 ] <- LT_Omega_d_vec_upper[index + length_index_m1]
    length_index_m1 <- length_index_m1 + ( length(index) - 1 )
    
  }
  LT_Omega_d <- round(LT_Omega_d, 3)
  LT_Omega_d_lower <- round(LT_Omega_d_lower, 3)
  LT_Omega_d_upper <- round(LT_Omega_d_upper, 3)
  
  
  
  
  
  
  {
    
    print(paste("DGP = ", DGP))
    print(cat("Se = ", Se))
    print(cat("Sp = ",  Sp))
    print(cat("prev = ", prev))
    print(paste("corr_force_positive = ", corr_force_positive))
    print(paste("prior_lkj = ", prior_lkj))
    print(paste("model_type = ",model_type))
    print(paste("corr_param = ",corr_param))
    
    
    print(paste("corr mtx. in D- = "))
    print(LT_Omega_nd)
    print(paste("corr mtx. in D+ = "))
    print(LT_Omega_d)
  }
  
  
  sum_abs_Se_diffs <- round(sum(abs(Se_true_observed_list[[df_i]] - Se)), 3)
  sum_abs_Sp_diffs <- round(sum(abs(Sp_true_observed_list[[df_i]] - Sp)), 3)
  sum_abs_prev_diffs <- round(sum(abs(prev_true_observed_list[[df_i]] - prev_observed)), 3)
  
  print(paste("Sum abs. Se diffs = ", sum_abs_Se_diffs))
  print(paste("Sum abs. Sp diffs = ", sum_abs_Sp_diffs))
  print(paste("Sum abs. prev diffs = ", sum_abs_prev_diffs))
  
  print(paste("Sum abs. Se+Sp+prev diffs = ",  sum_abs_Se_diffs + sum_abs_Sp_diffs + sum_abs_prev_diffs))
  
  sum_abs_Omega_d_diffs <- round(sum(abs(Sigma_d_true_observed_list[[df_i]][upper.tri(Sigma_d_true_observed_list[[df_i]])] -  LT_Omega_d[upper.tri(LT_Omega_d )])), 3)
  sum_abs_Omega_nd_diffs <- round(sum(abs(Sigma_nd_true_observed_list[[df_i]][upper.tri(Sigma_nd_true_observed_list[[df_i]])] -  LT_Omega_nd[upper.tri(LT_Omega_nd )])), 3)
  
  print(paste("Sum abs. Corr mtx. diffs (D+) = ", sum_abs_Omega_d_diffs))
  print(paste("Sum abs. Corr mtx. diffs (D-) = ", sum_abs_Omega_nd_diffs))
  
  # loo_ic_out <-  print(model$loo())
  # 
  # loo_ic_mean <- loo_ic_out$estimates[3,1]
  # loo_ic_se  <- loo_ic_out$estimates[3,2]
  # 
  # loo_ic_est_lower <- loo_ic_mean - 2 * loo_ic_se
  # loo_ic_est_upper <- loo_ic_mean + 2 * loo_ic_se
  
}




# Add results to Df ------------------------------------------------------

{
  
  LT_Omega_d[1, 1] <- 1
  LT_Omega_nd[1, 1] <- 1
  for (i in 2:n_tests) {
    LT_Omega_d[i, i] <- 1
    LT_Omega_nd[i, i] <- 1
    for (j in 1:(i-1)) {
      LT_Omega_nd[i, j] <-     LT_Omega_nd[j, i] 
      LT_Omega_d[i, j] <-     LT_Omega_d[j, i] 
    }
  }
  # first work out the model-predicted / expected correlations and table probabilities, then deviance and loo-IC to add to DF. 
  
  
  N_sims_for_model_fit_estimates <- 1000
  predicted_table_probs_vec_cumul <- 0
  predicted_correlations_vec_cumul <- 0
  
  predicted_table_probs_array <- array(dim = c(N_sims_for_model_fit_estimates, 2^n_tests))
  predicted_correlations_vec_array <- array(dim = c(N_sims_for_model_fit_estimates,  choose(n_tests, 2)))
  
  for (ii in 1:N_sims_for_model_fit_estimates) {
    
    d_ind <- sort(rbinom(n= N, size = 1, prob = prev))
    n_pos <- sum(d_ind)
    
    
    n_neg <- N - sum(d_ind)
    latent_results_neg <- LaplacesDemon::rmvn(n = n_neg, mu = qnorm(1 - Sp), Sigma = as.matrix(Matrix::forceSymmetric(LT_Omega_nd)))
    latent_results_pos <- LaplacesDemon::rmvn(n = n_pos, mu = qnorm(Se), Sigma = as.matrix(Matrix::forceSymmetric(LT_Omega_d)))
    latent_results <- rbind(latent_results_neg, latent_results_pos)
    results_neg <- ifelse(latent_results_neg > 0, 1, 0)
    results_pos <- ifelse(latent_results_pos > 0, 1, 0)
    results <- rbind(results_neg, results_pos)
    y_predicted <- results
    
    df_predicted <- tibble(results,latent_results,d_ind)
    df_predicted_pos <- filter(df_predicted, d_ind == 1)
    df_predicted_neg <- filter(df_predicted, d_ind == 0)
    
    
    predicted_correlations <- array(dim = c(n_tests, n_tests))
    
    for (i in 2:n_tests) {
      for (j in 1:(i-1)) {
        predicted_correlations[i, j] <- cor(y_predicted[, i], y_predicted[, j])
        predicted_correlations[j, i] <-  predicted_correlations[i, j]
      }
    }
    
    
    predicted_correlations_vec <- predicted_correlations[upper.tri(predicted_correlations )]
    
    predicted_table <- table(y_predicted[, 1], y_predicted[, 2], y_predicted[, 3], y_predicted[, 4], y_predicted[, 5])
    predicted_table_probs_vec <- c(unlist(round(prop.table(predicted_table), 4)))
    
    predicted_table_probs_array[ii, ] <-  predicted_table_probs_vec
    predicted_correlations_vec_array[ii, ]  <-   predicted_correlations_vec  # - true_correlations_observed_vec
    
  }
  
  
  predicted_table_probs_stats <- apply(predicted_table_probs_array, 2 , quantile, probs = c(0.025, 0.50, 0.975)) 
  predicted_correlations_stats <- apply(predicted_correlations_vec_array, 2 , quantile, probs = c(0.025, 0.50, 0.975)) 
  
  
  predicted_table_probs_vec_median  <- predicted_table_probs_stats[2,]
  predicted_correlations_vec_median <-  predicted_correlations_stats[2,]
  
  predicted_table_probs_vec_lower  <- predicted_table_probs_stats[1,]
  predicted_correlations_vec_lower <-  predicted_correlations_stats[1,]
  
  predicted_table_probs_vec_upper  <- predicted_table_probs_stats[3,]
  predicted_correlations_vec_upper <-  predicted_correlations_stats[3,]
  
  
  
  
  estimates_medians <- c(LT_Omega_nd[upper.tri(LT_Omega_nd )],  LT_Omega_d[upper.tri(LT_Omega_d )], Sp,  Se, prev  , 
                         predicted_correlations_vec_median, predicted_table_probs_vec_median, NA, loo_ic_mean)
  
  estimates_lower  <- c(LT_Omega_nd_lower[upper.tri(LT_Omega_nd_lower )],  LT_Omega_d_lower[upper.tri(LT_Omega_d_lower )], Sp_lower,  Se_lower, prev_upper  , 
                        predicted_correlations_vec_lower, predicted_table_probs_vec_lower, NA, loo_ic_est_lower)
  
  estimates_upper <- c(LT_Omega_d_upper[upper.tri(LT_Omega_d_upper )],  LT_Omega_d_upper[upper.tri(LT_Omega_d_upper )], Sp_upper,  Se_upper, prev_upper  , 
                       predicted_correlations_vec_upper, predicted_table_probs_vec_upper, NA, loo_ic_est_upper)
  
  param_type <- c(rep("Corr_nd", n_corrs/2), rep("Corr_d", n_corrs/2),  rep("Sp", n_tests), rep("Se", n_tests), rep("prev", n_pops), 
                  rep("y_corrs", choose(n_tests, 2)), rep("table_probs", 2^n_tests), "Deviance_est", "loo_ic")
  param_seq <- c(seq(from = 1, by = 1, length =  n_corrs/2), seq(from = 1, by = 1, length =  n_corrs/2) , seq(from = 1, by = 1, length =  n_tests)  , seq(from = 1, by = 1, length =  n_tests)  , seq(from = 1, by = 1, length =  n_pops) ,
                 seq(from = 1, by = 1, length =  choose(n_tests, 2)) ,  seq(from = 1, by = 1, length =  2^n_tests), 1, 1)
  n_params_main <- length(estimates_medians)
  
  
  # append results to DF
  df_segment_1 <- tibble(N = N, 
                         DGM = DGP, 
                         df_sim_seed = df_i,
                         algorithm = "Stan", 
                         model_type = model_type,
                         corr_force_positive = corr_force_positive,
                         prior_lkj_skewed_diseased_a = prior_lkj_skewed_diseased[1],
                         prior_lkj_skewed_diseased_b = prior_lkj_skewed_diseased[2],
                         prior_lkj_skewed_non_diseased_a = prior_lkj_skewed_non_diseased[1],
                         prior_lkj_skewed_non_diseased_b = prior_lkj_skewed_non_diseased[2],
                         tailored_corr_priors = tailored_corr_priors,
                         prior_a_mean_nd_test_1 = prior_a_mean[1,1,1],
                         prior_a_mean_nd_test_2 = prior_a_mean[1,2,1],
                         prior_a_mean_nd_test_3 = prior_a_mean[1,3,1],
                         prior_a_mean_nd_test_4 = prior_a_mean[1,4,1],
                         prior_a_mean_nd_test_5 = prior_a_mean[1,5,1],
                         prior_a_mean_d_test_1 = prior_a_mean[2,1,1],
                         prior_a_mean_d_test_2 = prior_a_mean[2,2,1],
                         prior_a_mean_d_test_3 = prior_a_mean[2,3,1],
                         prior_a_mean_d_test_4 = prior_a_mean[2,4,1],
                         prior_a_mean_d_test_5 = prior_a_mean[2,5,1],
                         prior_a_sd_nd_test_1 = prior_a_sd[1,1,1],
                         prior_a_sd_nd_test_2 = prior_a_sd[1,2,1],
                         prior_a_sd_nd_test_3 = prior_a_sd[1,3,1],
                         prior_a_sd_nd_test_4 = prior_a_sd[1,4,1],
                         prior_a_sd_nd_test_5 = prior_a_sd[1,5,1],
                         prior_a_sd_d_test_1 = prior_a_sd[2,1,1],
                         prior_a_sd_d_test_2 = prior_a_sd[2,2,1],
                         prior_a_sd_d_test_3 = prior_a_sd[2,3,1],
                         prior_a_sd_d_test_4 = prior_a_sd[2,4,1],
                         prior_a_sd_d_test_5 = prior_a_sd[2,5,1])
  
  
  df_segment_2 <-  df_segment_1 %>% 
    slice(rep(1:n(), each = n_params_main)) %>%
    mutate(estimates_medians = estimates_medians, 
           estimates_lower = estimates_lower, 
           estimates_upper = estimates_upper, 
           true_estimates_observed = true_estimates_observed_list[[df_i]],
           true_estimates  = true_estimates,
           param_type = param_type,
           param_seq = param_seq,
           diff_observed_median = true_estimates_observed_list[[df_i]] - estimates_medians,
           diff_observed_lower = true_estimates_observed_list[[df_i]] - estimates_lower,
           diff_observed_upper = true_estimates_observed_list[[df_i]] - estimates_upper,
           abs_diff_observed_median = abs(true_estimates_observed_list[[df_i]] - estimates_medians),
           abs_diff_observed_lower = abs(true_estimates_observed_list[[df_i]] - estimates_lower),
           abs_diff_observed_upper = abs(true_estimates_observed_list[[df_i]] - estimates_upper),
           corr_force_positive = ifelse(model_type == "LT", 1, corr_force_positive))
  
  
  
  
  file_name <- paste("df_sim", 
                     "N", N/1000, 
                     "DGP", DGP,
                     "df_seed", df_i,
                     "Stan", 
                     model_type, 
                     "pos_corr", corr_force_positive,
                     "pi_lkj_d_a", prior_lkj_skewed_diseased[1],
                     "pi_lkj_d_b", prior_lkj_skewed_diseased[2],
                     "pi_lkj_nd_a", prior_lkj_skewed_non_diseased[1],
                     "pi_lkj_nd_b", prior_lkj_skewed_non_diseased[2],
                     "tailored_pi", tailored_corr_priors,
                     "pi_a_mu_nd", prior_a_mean[1,1,1], prior_a_mean[1,2,1],  prior_a_mean[1,3,1], prior_a_mean[1,4,1], prior_a_mean[1,5,1],
                     "pi_a_mu_d",  prior_a_mean[2,1,1], prior_a_mean[2,2,1],  prior_a_mean[2,3,1], prior_a_mean[2,4,1], prior_a_mean[2,5,1],
                     "pi_a_sds_nd",  prior_a_sd[1,1,1], prior_a_sd[1,2,1],  prior_a_sd[1,3,1], prior_a_sd[1,4,1], prior_a_sd[1,5,1],
                     "pi_a_sds_d",   prior_a_sd[2,1,1], prior_a_sd[2,2,1],  prior_a_sd[2,3,1], prior_a_sd[2,4,1], prior_a_sd[2,5,1], sep = "_")
  
  
  file_name
  
  
  
  saveRDS(df_segment_2, file = file_name)
  
  
  
  
  
  
}


}





{
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  View(df_segment_2)
  df_sim_results_avg_by_model_and_priors_etc <- df_segment_2 %>%
    group_by(N, 
             DGM,
             df_sim_seed,
             algorithm, 
             model_type,
             corr_force_positive,
             prior_lkj_skewed_diseased_a,
             prior_lkj_skewed_diseased_b,
             prior_lkj_skewed_non_diseased_a,
             prior_lkj_skewed_non_diseased_b,
             tailored_corr_priors,
             prior_a_mean_nd_test_1,
             prior_a_mean_nd_test_2,
             prior_a_mean_nd_test_3,
             prior_a_mean_nd_test_4,
             prior_a_mean_nd_test_5,
             prior_a_mean_d_test_1,
             prior_a_mean_d_test_2,
             prior_a_mean_d_test_3,
             prior_a_mean_d_test_4,
             prior_a_mean_d_test_5,
             prior_a_sd_nd_test_1,
             prior_a_sd_nd_test_2,
             prior_a_sd_nd_test_3,
             prior_a_sd_nd_test_4,
             prior_a_sd_nd_test_5,
             prior_a_sd_d_test_1,
             prior_a_sd_d_test_2,
             prior_a_sd_d_test_3,
             prior_a_sd_d_test_4,
             prior_a_sd_d_test_5, 
             param_type)  %>%
    dplyr::summarise(mean_abs_diff_observed_median = mean(abs_diff_observed_median, na.rm=TRUE))
  
  View(df_sim_results_avg_by_model_and_priors_etc)
  
  
  df_sim_results_avg_by_model_and_priors_etc <-  df_sim_results_avg_by_model_and_priors_etc  %>%
    arrange(param_type)
  
  df_sim_results_avg_by_model_and_priors_etc <- df_sim_results_avg_by_model_and_priors_etc %>%
    ungroup()  %>%
    mutate(num = rep(1, length(df_sim_results_avg_by_model_and_priors_etc$N)))
  
  
}

### df_segment_3 <-  df_segment_2 %>%  slice(rep(1:n(), each = 1000)) 


# View(df_segment_2)



# 
# ggplot(data = filter(df_segment_2, param_type %in% c("Se", "Sp", "prev", "Corr_d", "Corr_nd"), df_sim_seed == 123), 
#        aes(x = param_seq, y = diff_observed_median, shape = model_type , 
#                                 colour = interaction(corr_force_positive,
#                                                      prior_lkj_skewed_diseased_a,
#                                                      prior_lkj_skewed_diseased_b, 
#                                                      prior_lkj_skewed_non_diseased_a, 
#                                                      prior_lkj_skewed_non_diseased_b, 
#                                                      tailored_corr_priors, 
#                                                      sep = ":") ))  + 
#   #  geom_line(size = 1.0) + 
#   geom_point(size = 3.5) + 
#   geom_point(data =  filter(df_sim_results_avg_by_model_and_priors_etc, param_type %in% c("Se", "Sp", "prev", "Corr_d", "Corr_nd"), df_sim_seed == 123), 
#              aes(x =   num, y = mean_abs_diff_observed_median,   shape = model_type, colour = interaction(corr_force_positive,
#                                                                                                                                                prior_lkj_skewed_diseased_a,
#                                                                                                                                                prior_lkj_skewed_diseased_b,
#                                                                                                                                                prior_lkj_skewed_non_diseased_a,
#                                                                                                                                                prior_lkj_skewed_non_diseased_b,
#                                                                                                                                                tailored_corr_priors,
#                                                                                                                                                sep = ":")),
#              size = 10,   stroke = 3)   +
#   facet_wrap(~ param_type + DGM , scales = "free", ncol = 5)  + 
#   geom_hline(yintercept = 0) +
#   theme_bw() + 
#   ylab(" Diff (true - estimate) or mean abs. diff for average points") + 
#   xlab("Parameter") + 
#   theme_bw(base_size = 20) + 
#   guides(color=guide_legend(title="Model properties"))   
# 
# 





ggplot(data = filter(df_sim_results_avg_by_model_and_priors_etc, param_type %in% c("Se", "Sp", "prev", "Corr_d", "Corr_nd"), df_sim_seed == 123), 
       aes(x = num, y = mean_abs_diff_observed_median, shape = model_type , 
           colour = interaction(corr_force_positive,
                                prior_lkj_skewed_diseased_a,
                                prior_lkj_skewed_diseased_b, 
                                prior_lkj_skewed_non_diseased_a, 
                                prior_lkj_skewed_non_diseased_b, 
                                tailored_corr_priors, 
                                sep = ":") ))  + 
  geom_point(size = 3.5) + 
  facet_wrap(~ param_type + DGM , scales = "free", ncol = 5)  + 
  geom_hline(yintercept = 0) +
  theme_bw() + 
  ylab(" Diff (true - estimate) or mean abs. diff for average points") + 
  xlab("Parameter") + 
  theme_bw(base_size = 20) + 
  guides(color=guide_legend(title="Model properties"))   






# ---------------------------------------------  correlation residual plot


df_segment_for_table_prob_resid <- df_segment_2 %>%
  dplyr::filter(param_type == "table_probs" ) 


ggplot(data = df_segment_for_table_prob_resid, aes(x = param_seq, y = diff_observed_median, linetype = model_type , 
                                                   colour = interaction(corr_force_positive,
                                                                        prior_lkj_skewed_diseased_a,
                                                                        prior_lkj_skewed_diseased_b, 
                                                                        prior_lkj_skewed_non_diseased_a, 
                                                                        prior_lkj_skewed_non_diseased_b, 
                                                                        tailored_corr_priors, 
                                                                        sep = ":") ))  + 
  #  geom_line(size = 1.0) + 
  geom_point(size = 3.5) + 
  geom_errorbar(aes(ymin=diff_observed_lower, ymax=diff_observed_upper, width=0.2,    colour = interaction(corr_force_positive,
                                                                                                           prior_lkj_skewed_diseased_a,
                                                                                                           prior_lkj_skewed_diseased_b, 
                                                                                                           prior_lkj_skewed_non_diseased_a, 
                                                                                                           prior_lkj_skewed_non_diseased_b, 
                                                                                                           tailored_corr_priors, 
                                                                                                           sep = ":")  )) + 
  facet_wrap(~ param_type, scales = "free")  + 
  geom_hline(yintercept = 0) +
  theme_bw() + 
  ylab(" Diff (true - estimate) or mean abs. diff for average points") + 
  xlab("Parameter") + 
  theme_bw(base_size = 20) + 
  guides(color=guide_legend(title="Model properties"))   























# ---------------------------------------------  table probability residual plot





# Se - Se_true_N_1000
# Sp - Sp_true_N_1000
# 
# {
#   print(paste(" | -----------------------------------------------------------------------------------------  | "))
#   
#     if (sum(abs(Se - Se_true_N_1000)) >= sum(abs(Se_CI_N_1000 - Se_true_N_1000))) { 
#       print(paste("CI model fits better (or the same) for Se"))
#       Se_abs_diff  <- ((abs(Se - Se_true_N_1000)) -  (abs(Se_CI_N_1000 - Se_true_N_1000)))
#       Se_abs_diff_total <- sum(abs(Se_abs_diff))
#       print(cat("improvement in abs. diff for Se = ", Se_abs_diff))
#       print(paste("total improvement in abs. diff for Se = ", Se_abs_diff_total))
#     } else { 
#       print(paste("CD model fits better for Se"))
#       Se_abs_diff  <- ((abs(Se - Se_true_N_1000)) -  (abs(Se_CI_N_1000 - Se_true_N_1000)))
#       Se_abs_diff_total <- sum(abs(Se_abs_diff))
#       print(cat("improvement in abs. diff for Se = ", Se_abs_diff))
#       print(paste("total improvement in abs. diff for Se = ", Se_abs_diff_total))
#     }
#     if (sum(abs(Sp - Sp_true_N_1000)) >= sum(abs(Sp_CI_N_1000 - Sp_true_N_1000))) { 
#       print(paste("CI model fits better (or the same) for Sp"))
#       Sp_abs_diff  <- ((abs(Sp - Sp_true_N_1000)) -  (abs(Sp_CI_N_1000 - Sp_true_N_1000)))
#       Sp_abs_diff_total <- sum(abs(Sp_abs_diff))
#       print(cat("improvement in abs. diff for Sp = ", Sp_abs_diff))
#       print(paste("improvement in total abs. diff for Sp = ", Sp_abs_diff_total))
#     } else { 
#       print(paste("CD model fits better for Sp"))
#       Sp_abs_diff  <- ((abs(Sp - Sp_true_N_1000)) -  (abs(Sp_CI_N_1000 - Sp_true_N_1000)))
#       Sp_abs_diff_total <- sum(abs(Sp_abs_diff))
#       print(cat("improvement in abs. diff for Sp = ", Sp_abs_diff))
#       print(paste("improvement in total abs. diff for Sp = ", Sp_abs_diff_total))
#     }
# 
#  #  (sum(abs(Se - Se_true_N_1000)) + (sum(abs(Sp - Sp_true_N_1000)) + (sum(abs(prev - true_prev))
# 
#   
#   
# 
#   
#    
#    print(paste(" | -----------------------------------------------------------------------------------------  | "))
# }


{
  
  # Max R-hat / converged?
  cmdstanr_model_out_rhat <- cmdstanr_model_out$rhat[!(is.na(cmdstanr_model_out$rhat))]
  print(round(max(cmdstanr_model_out_rhat), 3)) ; print(any(cmdstanr_model_out_rhat > 1.010)) # if TRUE, not converged (at least 1 r-hat > 1.01)
  
  # Min ESS?
  cmdstanr_model_out_ess <- cmdstanr_model_out$ess_bulk[!(is.na(cmdstanr_model_out$ess_bulk))]
  Min_ESS <- min(cmdstanr_model_out_ess)
  print(round(Min_ESS, 0))
  
  print(tictoc::toc(log = TRUE))
  log.txt <- tictoc::tic.log(format = TRUE)
  tictoc::tic.clearlog()
  time_stan_total_inc_csv <- unlist(log.txt)
  
  time_stan_total_real_world <- as.numeric(substr(time_stan_total_inc_csv,  8,14))
  
  # run time (total)
  cmdstanr_model_out_timers <- model$time()
  total_time_seconds <- time_stan_total_real_world #  cmdstanr_model_out_timers$total 
  total_time_mins <- total_time_seconds / 60
  total_time_hours <- total_time_mins / 60
  
  print(paste("total time =", round(total_time_seconds, 0), "seconds")) # in seconds
  print(paste("total time =", floor(total_time_mins), "minutes and ", round(((total_time_mins - floor(total_time_mins))*60), 0), "seconds"))    # in minutes
  print(paste("total time =", floor(total_time_hours), "hours and ", round(((total_time_hours - floor(total_time_hours))*60), 0), "minutes"))    # in hours
  
  # run time (post-burnin / sampling ONLY)
  pb_time_seconds <- max(cmdstanr_model_out_timers$chains$sampling)
  pb_time_mins <- pb_time_seconds / 60
  pb_time_hours <- pb_time_mins / 60
  
  print(paste("Sampling (post-burnin) time =", round(pb_time_seconds, 0), "seconds")) # in seconds
  print(paste("Sampling (post-burnin) time =", floor(pb_time_mins), "minutes and ", round(((pb_time_mins - floor(pb_time_mins))*60), 0), "seconds"))    # in minutes
  print(paste("Sampling (post-burnin) time =", floor(pb_time_hours), "hours and ", round(((pb_time_hours - floor(pb_time_hours))*60), 0), "minutes"))    # in hours
  
  # Min ESS / sec (using total time)
  Min_ESS_per_sec_total_time <- Min_ESS / total_time_seconds
  print(paste("Min ESS / sec (total time) = ", round(Min_ESS_per_sec_total_time, 5)))
  
  # Min ESS / sec (using post-burnin / sampling time)
  Min_ESS_per_sec_pb_time <- Min_ESS / pb_time_seconds
  print(paste("Min ESS / sec (sampling time only) = ", round(Min_ESS_per_sec_pb_time, 5)))
  
  # # Min ESS / grad (uses post-burnin / sampling time ONLY) 
  mean_L  <-   (mean(c(2^model$sampler_diagnostics(inc_warmup = TRUE)[,,1] - 1)))  # mean L (total)
  print(paste("Mean L across chains = ", mean_L))
  
  mean_L_post_burnin  <-   (mean(c(2^model$sampler_diagnostics()[,,1] - 1))) # mean # of Leapfrog steps (sampling only)
  print(paste("Mean L across chains (post-burnin) = ", mean_L_post_burnin))
  
  mean_eps_post_burnin  <-   (mean(c(model$sampler_diagnostics()[,,5]))) # mean # of Leapfrog steps (sampling only)
  print(paste("Mean step-size across chains (post-burnin) = ", round(mean_eps_post_burnin, 4)))
  
  # total # of gradient evals
  stan_total_gradient_evals <-   (mean_L*n_chains*(iter_sampling + iter_warmup))   #  n_chains * iter_sampling * stan_n_leapfrogs ; stan_total_gradient_evals
  stan_total_gradient_evals_pb <-   (mean_L_post_burnin*n_chains*iter_sampling) 
  
  stan_min_ess_per_grad_eval <- Min_ESS / stan_total_gradient_evals ; stan_min_ess_per_grad_eval
  stan_min_ess_per_grad_eval_pb <- Min_ESS / stan_total_gradient_evals_pb ; stan_min_ess_per_grad_eval
  
  print(paste("ESS/grad (total) = ", round(1000 * stan_min_ess_per_grad_eval, 3)))
  print(paste("ESS/grad (sampling) = ", round(1000 * stan_min_ess_per_grad_eval_pb, 3)))
  
  print(paste("seed = ", seed))
  
  # save efficiency summary info only 
  file_name <- paste0("efficiency_info_", "seed_", seed, "_",  prior_lkj, "priorLKJ_", N, "N_", n_chains, "chains_", iter_warmup, "burn_",  iter_sampling, "samp_", adapt_delta, "ad_", max_treedepth, "maxtree_", metric_type, "Mtype",".RDS")
  
  file_list <- list(
    cmdstanr_model_out_rhat, round(max(cmdstanr_model_out_rhat),3),
    Min_ESS,
    time_stan_total_real_world, cmdstanr_model_out_timers,
    total_time_seconds,  total_time_mins, total_time_hours,
    pb_time_seconds, pb_time_mins, pb_time_hours,
    Min_ESS_per_sec_total_time,
    Min_ESS_per_sec_pb_time,
    paste("total time =", round(total_time_seconds, 0), "seconds"),
    paste("total time =", floor(total_time_mins), "minutes and ", round(((total_time_mins - floor(total_time_mins))*60), 0), "seconds"),
    paste("total time =", floor(total_time_hours), "hours and ", round(((total_time_hours - floor(total_time_hours))*60), 0), "minutes"),
    paste("Sampling (post-burnin) time =", round(pb_time_seconds, 0), "seconds"),
    paste("Sampling (post-burnin) time =", floor(pb_time_mins), "minutes and ", round(((pb_time_mins - floor(pb_time_mins))*60), 0), "seconds"),
    paste("Sampling (post-burnin) time =", floor(pb_time_hours), "hours and ", round(((pb_time_hours - floor(pb_time_hours))*60), 0), "minutes"),
    paste("Min ESS / sec (total time) = ", round(Min_ESS_per_sec_total_time, 5)),
    paste("Min ESS / sec (sampling time only) = ", round(Min_ESS_per_sec_pb_time, 5)),
    # mean_L_per_chain, 
    mean_L_post_burnin,
    mean_L,
    stan_min_ess_per_grad_eval,
    stan_min_ess_per_grad_eval_pb,
    mean_eps_post_burnin)
  
  saveRDS(file_list, file = file_name)
  
  
  # # save partial cmdstanr output - table for main params only 
  file_name <- paste0("Summary_param_table_main_", "seed_", seed, "_", prior_lkj, "priorLKJ_", N, "N_", n_chains, "chains_", iter_warmup, "burn_",  iter_sampling, "samp_",
                      adapt_delta, "ad_", max_treedepth, "maxtree_", metric_type, "Mtype")
  saveRDS(cmdstanr_model_out, file = file_name)
  
  
  # # save full cmdstanr output (files may be massive!!!)
  # file_name <- paste0("seed_", seed, "_", prior_lkj, "priorLKJ_", N, "N_", n_chains, "chains_", iter_warmup, "burn_",  iter_sampling, "samp_",
  #                     adapt_delta, "ad_", max_treedepth, "maxtree_", metric_type, "Mtype")
  # model$save_object(file = file_name)
  
  
  }
















# 0.094

# Density plots ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------



# mcmc_hist(model$draws("Sigma"), binwidth = 0.025) +
#   ggplot2::labs(subtitle = "Approximate posterior from pathfinder") +
#   ggplot2::xlim(0, 1)




# 
modelr <- rstan::read_stan_csv(model$output_files())  # convert to rstan CSV

# stan_trace(modelr, pars = c("beta", "p"))
# stan_trace(modelr, pars = c("L_Omega"))



# stan_dens(modelr, pars = c("Se_bin"))


stan_dens(modelr, pars = c("Sigma"))


stan_dens(modelr, pars = c("Sigma_2"))

stan_dens(modelr, pars = c("Sigma_gr0_1"))
stan_dens(modelr, pars = c("Sigma_gr0_2"))


stan_dens(modelr, pars = c("Sigma_standard"))





B_mat <- diag(n_tests) - solve(t(chol(Sigma_d))) 
B_mat[1,1] <- B_mat[2,2] <- 1
B_mat %*% t(B_mat)

corpcor::cor2pcor(Sigma_d)

cov2cor(B_mat %*% t(B_mat))


sims <- rmvn(n = 10000, mu = rep(0, n_tests), Sigma = Sigma_d)
cor(sims)




