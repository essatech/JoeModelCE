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

  #life_cycles$Value[which(life_cycles$Name == "M.rho")]   <- 0
  #life_cycles$Value[which(life_cycles$Name == "eps_sd")]  <- 0
  #life_cycles$Value[which(life_cycles$Name == "egg_rho")] <- 0


  cr_E <- c(0.5, 1, 1.5, 2)
  cr_0 <- c(0.5, 1, 3, 4)
  cr_1 <- c(0.5, 1, 3, 4)
  cr_2 <- c(0.5, 1, 2, 3, 4)
  cr_3 <- c(0.5, 1, 2, 3, 4)
  cr_4 <- c(0.5, 1, 2, 3, 4)

  gtest <- expand.grid(cr_E, cr_0, cr_1, cr_2, cr_3, cr_4)
  colnames(gtest) <- c("cr_E", "cr_0", "cr_1", "cr_2", "cr_3", "cr_4")
  nrow(gtest)

  test_scores <- list()


  i = 2000
  #for(i in 1:nrow(gtest)) {
  for(i in 1:1) {

    print(i)

    params <- gtest[i, ]

    life_cycles$Value[which(life_cycles$Name == "cr_E")]  <- params$cr_E
    life_cycles$Value[which(life_cycles$Name == "cr_0")]  <- params$cr_0
    life_cycles$Value[which(life_cycles$Name == "cr_1")]  <- params$cr_1
    life_cycles$Value[which(life_cycles$Name == "cr_2")]  <- params$cr_2
    life_cycles$Value[which(life_cycles$Name == "cr_3")]  <- params$cr_3
    life_cycles$Value[which(life_cycles$Name == "cr_4")]  <- params$cr_4


    life_pars <- life_cycles
    row.names(life_pars) <- life_pars$Name
    Nstage <- life_pars["Nstage", "Value"]

    survival <-
      life_pars[match(paste("surv", 1:Nstage, sep = "_"), life_pars$Name), "Value"]
    cr <-
      life_pars[match(paste("cr", 1:Nstage, sep = "_"), life_pars$Name), "Value"]
    if (any((cr * survival) > 1)) {
      print("compensation ratios too high")
      next
    }


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

  # expect_true(lambda > 1)

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
      Nyears = 500,
      # years to run simulation
      p.cat = 0,      # Probability of catastrophe
      CE_df = NULL,
      K_adj = FALSE
    )

  expect_true(!(any(is.na(baseline$pop$N))))

  df <- baseline$pop[which(baseline$pop$year > 5), ]
  # plot(df$year, df$N, type = 'l')

  #df2 <- df[which(df$year > 2400), ]
  #plot(df2$year, df2$N, type = 'l')

  df <- data.frame(baseline$N)


  df <- df[25:nrow(df), ]

  cms <- colMeans(df)

  if(cms['X4'] < 10) { next }

  score1 <- cms['X4'] / cms['X2']
  score1 <- as.numeric(abs(score1 - 0.05))

  score2 <- cms['X2'] / cms['X1']
  score2 <- as.numeric(abs(score2 - 0.08))

  total_score <- score1 + score2


  params$total_score <- total_score
  params$N <- as.numeric(cms['X4'])

  params$i <- i
  test_scores[[i]] <- params

}


if(FALSE){
  #=============================================
  out <- do.call("rbind", test_scores)
  out <- out[which(!(is.na(out$total_score))), ]
  out <- out[order(out$total_score),]
  head(out)
  out2 <- out[which(out$N > 850), ]
  out2 <- out2[which(out2$N < 1100), ]

  out2 <- out2[order(out2$total_score, decreasing = TRUE),]
  head(out2, 10)
  cc <- head(out2, 10)
  colMeans(cc)




  out2 <- out2[order(out2$N, decreasing = TRUE),]
  head(out2)
  plot(out2$N, out2$total_score)
  boxplot(out2$N ~ out2$cr_4)
  boxplot(out2$N ~ out2$cr_3)
  boxplot(out2$N ~ out2$cr_2)
  boxplot(out2$N ~ out2$cr_1)
  boxplot(out2$N ~ out2$cr_0)
  boxplot(out2$N ~ out2$cr_E)

  life_cycles$Value[which(life_cycles$Name == "k")]

  #=============================================
}



})
