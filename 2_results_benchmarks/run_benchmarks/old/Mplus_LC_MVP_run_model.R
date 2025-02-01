

## source("re_install_BayesMVP.R")


{
  
  source("load_R_packages.R")
  source("load_data_binary_LC_MVP_sim.R")
  
}

 




#  --- | --------------------------------------------------  Run models using MPlus  --------------------------------------------------------------------
# install.packages("MplusAutomation")
require(MplusAutomation)

S


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















