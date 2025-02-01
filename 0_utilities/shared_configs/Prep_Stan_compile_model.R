

#### ------- Compile Stan model  -------------------------------------------------------------------------
     
    ## First, set the custom C++ flags (e.g. AVX2 / AVX-512, custom AOCC AMD compiler, math flags, etc):
    ## NOTE: THIS WILL ONLY COMPILE IF:
    ## - You are using Linux 

    ## - Your CPU is AMD (NOT Intel)
    ## - You have the AMD AOCC version of the clang C++ compiler installed. 
    ## To compile if any one of the above is not true, please just comment out the custom flags in the 
    ## cmdstanr::cmdstan_model() function!
    
    {
      
      ##  /inst/BayesMVP/inst/stan_models/LC_MVP_bin_w_mnl_cpp_grad_v1.stan
      
      # OR using the version w/ fast math C++ functions:
      user_home_dir <- Sys.getenv("HOME")
      user_BayesMVP_dir <- file.path(user_home_dir, "BayesMVP")
      ### file <- (file.path(user_BayesMVP_dir, "inst/BayesMVP/inst/stan_models/LC_MVP_bin_w_mnl_cpp_grad_v1.stan"))
      file <- (file.path(user_BayesMVP_dir, "stan_models/LC_MVP_bin_w_mnl_cpp_grad_v1.stan"))
      
      ## and then input the path to the corresponding C++ files:
      path_to_cpp_user_header <- file.path(user_BayesMVP_dir, "src/LC_MVP_lp_grad_fn_for_Stan.hpp")
      
      ## Set C++ flags (optional)
      ## CXX_COMPILER <- "/opt/AMD/aocc-compiler-5.0.0/bin/clang++"
      ## CPP_COMPILER <- "/opt/AMD/aocc-compiler-5.0.0/bin/clang"
      CXX_COMPILER <- "g++"
      CPP_COMPILER <- "gcc"
      # CXX_COMPILER <- "clang++"
      # CPP_COMPILER <- "clang"
      ##
      CCACHE <- "/usr/bin/ccache"
      CXX_STD <- "CXX17"
      CPU_BASE_FLAGS <- "-O3  -march=native  -mtune=native"
      FMA_FLAGS <- "-mfma"
      ## AVX Flags (using AVX2 on my laptop and AVX-512 on my local HPC):
      AVX_512_FLAGS <- "-mavx512f -mavx512vl -mavx512dq" ## for AVX-512 - ONLY USE IF YOUR PC SUPPORTS AVX-512 (MANY DONT!) - ELSE COMMENT OUT!
      AVX2_FLAGS <- "-mavx2" ## for AVX2 - ONLY USE IF YOUR PC SUPPORTS AVX2 - ELSE COMMENT OUT!
      AVX1_FLAGS <- "-mavx"
      #
      detect_SIMD_type <- BayesMVP:::detect_vectorization_support()
      ##
      if (detect_SIMD_type == "AVX512") { 
        AVX_FLAGS <- AVX_512_FLAGS
      } else if  (detect_SIMD_type == "AVX2") { 
        AVX_FLAGS <- AVX2_FLAGS
      } else if  (detect_SIMD_type == "AVX") {  
        AVX_FLAGS <- AVX1_FLAGS 
      } else { 
        AVX_FLAGS <- " "  ## No AVX fns at all otherwise
      }
      ##
      CPU_FLAGS <- paste(CPU_BASE_FLAGS, FMA_FLAGS, AVX_FLAGS)
      MATH_FLAGS <- "-fno-math-errno  -fno-signed-zeros  -fno-trapping-math"
      ##
      ## THREAD_FLAGS <- "-pthread -D_REENTRANT -DSTAN_THREADS -DSTAN_MATH_REV_CORE_INIT_CHAINABLESTACK_HPP"
      THREAD_FLAGS <- "-pthread -D_REENTRANT"
      ##THREAD_FLAGS <- " "
      ##
      OTHER_FLAGS <- " "
      ## OTHER_FLAGS <- "-std=c++17"
      OTHER_FLAGS <- paste(OTHER_FLAGS, "-fPIC")  
      OTHER_FLAGS <- paste(OTHER_FLAGS, "-DNDEBUG -fpermissive ")  
      OTHER_FLAGS <- paste(OTHER_FLAGS, "-DBOOST_DISABLE_ASSERTS")
      OTHER_FLAGS <- paste(OTHER_FLAGS, "-Wno-sign-compare -Wno-ignored-attributes -Wno-class-memaccess -Wno-class-varargs") 
      # OTHER_FLAGS <- paste0(OTHER_FLAGHS, " -ftls-model=global-dynamic") # doesnt fix clang issue
      # OTHER_FLAGS <- paste(OTHER_FLAGS, "-fno-gnu-unique") # doesnt fix clang issue
      ##OTHER_FLAGS <- paste(OTHER_FLAGS, "-ftls-model=initial-exec")  # doesnt fix clang issue
      # OTHER_FLAGS <-  " "
      # ##
      # OMP_LIB_PATH = "/opt/AMD/aocc-compiler-5.0.0/lib"
      # OMP_FLAGS = "-fopenmp"
      # OMP_LIB_FLAGS = "-L'OMP_LIB_PATH' -lomp 'OMP_LIB_PATH/libomp.so'"
      # SHLIB_OPENMP_CFLAGS <- OMP_FLAGS
      # SHLIB_OPENMP_CXXFLAGS <- SHLIB_OPENMP_CFLAGS
      # ##
      CMDSTAN_INCLUDE_PATHS <- "-I stan/lib/stan_math/lib/tbb_2020.3/include"
      CMDSTAN_INCLUDE_PATHS <- paste(CMDSTAN_INCLUDE_PATHS, "-I src")
      CMDSTAN_INCLUDE_PATHS <- paste(CMDSTAN_INCLUDE_PATHS, "-I stan/lib/rapidjson_1.1.0/")
      CMDSTAN_INCLUDE_PATHS <- paste(CMDSTAN_INCLUDE_PATHS, "-I lib/CLI11-1.9.1/")
      CMDSTAN_INCLUDE_PATHS <- paste(CMDSTAN_INCLUDE_PATHS, "-I stan/lib/stan_math/")
      CMDSTAN_INCLUDE_PATHS <- paste(CMDSTAN_INCLUDE_PATHS, "-I stan/lib/stan_math/lib/eigen_3.4.0")
      CMDSTAN_INCLUDE_PATHS <- paste(CMDSTAN_INCLUDE_PATHS, "-I stan/lib/stan_math/lib/boost_1.84.0")
      CMDSTAN_INCLUDE_PATHS <- paste(CMDSTAN_INCLUDE_PATHS, "-I stan/lib/stan_math/lib/sundials_6.1.1/include")
      CMDSTAN_INCLUDE_PATHS <- paste(CMDSTAN_INCLUDE_PATHS, "-I stan/lib/stan_math/lib/sundials_6.1.1/src/sundials")
      CMDSTAN_INCLUDE_PATHS <- paste(CMDSTAN_INCLUDE_PATHS, "-I stan/src")
      CMDSTAN_INCLUDE_PATHS <- paste(CMDSTAN_INCLUDE_PATHS, "-I stan/src")
      ##
      ##
      ## ----- Now set "BASE_FLAGS" --------------------
      BASE_FLAGS <- paste(CPU_FLAGS, 
                          ## `SHLIB_OPENMP_CFLAGS`,
                          MATH_FLAGS, 
                          OTHER_FLAGS, 
                          THREAD_FLAGS, 
                          CMDSTAN_INCLUDE_PATHS)
      
      ## Now set standard C++/C flags. 
      CC <-  paste(CCACHE, CPP_COMPILER)
      CXX <- paste(CCACHE, CXX_COMPILER)
      PKG_CPPFLAGS <- BASE_FLAGS
      PKG_CXXFLAGS <- BASE_FLAGS
      CPPFLAGS <- BASE_FLAGS
      CXXFLAGS <- BASE_FLAGS
      CFLAGS <- CPPFLAGS
      
      ## Linking flags to match Stan's
      LINKER_FLAGS <- paste(
        "-Wl,-L,\"$(CMDSTAN_PATH)/stan/lib/stan_math/lib/tbb\"",
        "-Wl,-rpath,\"$(CMDSTAN_PATH)/stan/lib/stan_math/lib/tbb\"",
        ##  "-lpthread",
        "-ltbb"
      )
      
      
      cmdstan_cpp_flags <- list(  paste0("CC = ", CC),
                                  paste0("CXX = ", CXX),
                                  paste0("PKG_CPPFLAGS = ", PKG_CPPFLAGS),
                                  paste0("PKG_CXXFLAGS = ", PKG_CXXFLAGS),
                                  paste0("CPPFLAGS = ", CPPFLAGS),
                                  paste0("CXXFLAGS = ", CXXFLAGS),
                                  paste0("CFLAGS = ", CFLAGS), 
                                  paste0("CXX_STD = ", CXX_STD), 
                                  paste0("LDFLAGS = ", LINKER_FLAGS))
      
      print(BASE_FLAGS)
      print(cmdstan_cpp_flags)
      

      
    }
    
    
    
    



    ## -------------------------------------------------------------------------
    ##
    ## Compile Stan model:
    file <-   file.path(getwd(), "0_utilities/stan_models/LC_MVP_bin_PartialLog_v5.stan")
    ##
    mod <- cmdstanr::cmdstan_model(  file, 
                                     # force_recompile = TRUE,
                                     quiet = FALSE,
                                     user_header = path_to_cpp_user_header,
                                     cpp_options = cmdstan_cpp_flags)
 
   
    # # -------------------------------------------------------------------------
    # u_initial <- array(0.01, dim = c(N, n_tests))
    # 
    # km1_choose_2 = 0.5 * (n_tests - 2) * (n_tests - 1)
    # known_num = 0
    # 
    # beta_vec_init <- rep(0, n_class * n_tests)
    # beta_vec_init[1:n_tests] <- - 1   # tests 1-T, class 1 (D-)
    # beta_vec_init[(n_tests + 1):(2*n_tests)] <- + 1   # tests 1-T, class 1 (D+)
    # 
    # init = list(
    #   u_raw = (u_initial),
    #   p_raw =  (-0.6931472), # equiv to 0.20 on p
    #   beta_vec = beta_vec_init,
    #   off_raw = array(0.01, dim = c(n_class, km1_choose_2 - known_num)),
    #   col_one_raw =   array(0.01, dim = c(n_class, n_tests - 1))
    # )
    # 
   
    
  

    




 