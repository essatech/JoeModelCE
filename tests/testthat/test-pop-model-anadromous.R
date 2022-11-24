test_that("Population model for anadromous species", {

  # library(testthat)
  filename_lc <- system.file("extdata", "life_cycles.csv", package = "JoeModelCE")
  life_cycles <- read.csv(filename_lc)
  expect_true(nrow(life_cycles) > 5)

  life_cycles$Value[which(life_cycles$Name == "SE")]      <- 0.45
  life_cycles$Value[which(life_cycles$Name == "S0")]      <- 0.2
  life_cycles$Value[which(life_cycles$Name == "surv_1")]  <- 0.111
  life_cycles$Value[which(life_cycles$Name == "eps")]     <- 100
  life_cycles$Value[which(life_cycles$Name == "eps_sd")]  <- 0
  life_cycles$Value[which(life_cycles$Name == "SR")]      <- 0.5

  # Setup objects for population model
  pop_mod_setup <- pop_model_setup(life_cycles = life_cycles)

  # Expected density-independent survival
  eggs_per_female <- pop_mod_setup$projection_matrix[1,4]
  expect_true(100*0.45*0.2*0.5 == eggs_per_female)




  #-----------------------------------------------------------------------------
  # COHO SALMON
  #-----------------------------------------------------------------------------
  # https://digitalcommons.humboldt.edu/cgi/viewcontent.cgi?article=1120&context=etd

  filename_lc <- system.file("extdata", "life_cycles_coho.csv", package = "JoeModelCE")
  life_cycles <- read.csv(filename_lc)
  expect_true(nrow(life_cycles) > 5)

  life_cycles$Value[which(life_cycles$Name == "M.rho")]   <- 0
  life_cycles$Value[which(life_cycles$Name == "eps_sd")]  <- 0
  life_cycles$Value[which(life_cycles$Name == "egg_rho")] <- 0


  cr_E <- seq(0.2, 2, by = 0.3)
  cr_0 <- seq(0.2, 2.5, by = 0.3)
  cr_1 <- seq(0.1, 1, by = 0.3)
  cr_2 <- seq(0.1, 1, by = 0.3)
  cr_3 <- seq(0.1, 1, by = 0.3)

  gtest <- expand.grid(cr_E, cr_0, cr_1, cr_2, cr_3)
  colnames(gtest) <- c("cr_E", "cr_0", "cr_1", "cr_2", "cr_3")
  nrow(gtest)

  test_scores <- list()

  #for(i in 1:nrow(gtest)) {
  for(i in 1:1) {

    params <- gtest[i, ]

    life_cycles$Value[which(life_cycles$Name == "cr_E")]  <- params$cr_E
    life_cycles$Value[which(life_cycles$Name == "cr_0")]  <- params$cr_0
    life_cycles$Value[which(life_cycles$Name == "cr_1")]  <- params$cr_1
    life_cycles$Value[which(life_cycles$Name == "cr_2")]  <- params$cr_2
    life_cycles$Value[which(life_cycles$Name == "cr_3")]  <- params$cr_3

















  # Setup objects for population model
  pop_mod_setup <- pop_model_setup(life_cycles = life_cycles)
  pop_mod_setup$projection_matrix

  # Build matrix elements for population model
  pop_mod_mat <- pop_model_matrix_elements(pop_mod_setup = pop_mod_setup)
  # Preview density-independent transition projection_matrix
  A <- pop_mod_mat$projection_matrix
  # Assign nicknames for each stage
  snames <- c("egg_yoy", "juv", "subadult", "adult")
  rownames(A) <- colnames(A) <- snames
  # Simple density-independent lambda estimate
  lambda <- popbio::lambda(A)
  expect_true(class(lambda) == "numeric")
  expect_true(lambda > 1)

  # Set the K.adj (K adjustment prior to pop model run)
  life_histories <- pop_mod_mat$life_histories
  # Mathematical expression of the transition matrix
  life_stages_symbolic <- pop_mod_mat$life_stages_symbolic
  # Mathematical expression of the density matrix
  density_stage_symbolic <- pop_mod_mat$density_stage_symbolic

  # Run simple population projection - project forward through time
  baseline <-
    Projection_DD(
      M.mx = life_stages_symbolic,
      # projection matrix expression
      D.mx = density_stage_symbolic,
      # density-dependence matrix
      H.mx = NULL,
      dat = life_histories,
      # life history data
      K = life_histories$Ka,
      # initial pop size as stage-structure vector
      Nyears = 100,
      # years to run simulation
      p.cat = 0,      # Probability of catastrophe
      CE_df = NULL
    )

  expect_true(!(any(is.na(baseline$pop$N))))

  df <- baseline$pop[which(baseline$pop$year > 5), ]
  #plot(df$year, df$N, type = 'l')

  df <- data.frame(baseline$N)
  df <- df[50:nrow(df), ]

  cms <- colMeans(df)


  score1 <- cms['X4'] / cms['X2']
  score1 <- as.numeric(abs(score1 - 0.05))

  score2 <- cms['X2'] / cms['X1']
  score2 <- as.numeric(abs(score1 - 0.08))

  total_score <- score1 + score2





  params$total_score <- total_score
  params$i <- i
  test_scores[[i]] <- params
  print(i)




}


  out <- do.call("rbind", test_scores)
  out <- out[which(!(is.na(out$total_score))), ]
  out <- out[order(out$total_score),]

  head(out)







})
