test_that("Matrix Interaction", {

  # ----------------------------------------
  # JOE MODEL TEST 1
  filename_rm <- system.file("extdata", "./matrix_test/stressor_magnitude_matrix.xlsx", package = "JoeModelCE")
  filename_sr <- system.file("extdata", "./matrix_test/stressor_response_matrix.xlsx", package = "JoeModelCE")
  dose <- StressorMagnitudeWorkbook(filename = filename_rm)
  sr_wb_dat <- StressorResponseWorkbook(filename = filename_sr)
  # Check that matrix is in workbook
  expect_true(all(names(sr_wb_dat$MInt) == c("MInt_AB", "MInt_DE")))
  # Try running the Joe Model with the matrix surface
  MC_sims <- 10
  jmr <- JoeModel_Run(dose = dose, sr_wb_dat = sr_wb_dat, MC_sims = MC_sims)
  expect_true(mean(jmr$ce.df$CE) > 0)
  expect_true(mean(jmr$ce.df$CE) < 1)

  # ----------------------------------------
  # JOE MODEL TEST 2
  filename_sr <- system.file("extdata", "./matrix_test/stressor_response_matrix_AB.xlsx", package = "JoeModelCE")
  sr_wb_dat <- StressorResponseWorkbook(filename = filename_sr)
  # Check that matrix is in workbook
  expect_true(all(names(sr_wb_dat$MInt) == c("MInt_AB")))
  # Try running the Joe Model with the matrix surface
  MC_sims <- 10
  jmr <- JoeModel_Run(dose = dose, sr_wb_dat = sr_wb_dat, MC_sims = MC_sims, adult_sys_cap = FALSE)
  expect_true(mean(jmr$ce.df$CE) >= 0)
  expect_true(mean(jmr$ce.df$CE) <= 1)

  tmp <- jmr$ce.df
  tmp <- tmp[tmp$HUC == 1, ]
  median(tmp$CE) == 0.33
  expect_true(median(tmp$CE) == 0.33)



  # ----------------------------------------
  # POPULATION MODEL TEST 1
  filename_rm <- system.file("extdata", "./matrix_test/stressor_magnitude_matrix.xlsx", package = "JoeModelCE")
  filename_sr <- system.file("extdata", "./matrix_test/stressor_response_matrix_AB.xlsx", package = "JoeModelCE")
  dose <- StressorMagnitudeWorkbook(filename = filename_rm)
  sr_wb_dat <- StressorResponseWorkbook(filename = filename_sr)
  # Load the life cycle parameters
  filename_lc <- system.file("extdata", "life_cycles.csv", package = "JoeModelCE")
  life_cycle_params <- read.csv(filename_lc)

  # Choose target ID
  HUC_ID <- dose$HUC_ID[1]

  # Load in the habitat data optional
  data <- PopulationModel_Run(
    dose = dose,
    sr_wb_dat = sr_wb_dat,
    life_cycle_params = life_cycle_params,
    HUC_ID = HUC_ID,
    n_years = 50,
    MC_sims = 5,
    stressors = NA
  )

  expect_true(!(is.null(data)))
  expect_true(typeof(data) == "list")
  expect_true(all(names(data) == c("ce", "baseline", "MC_sims")))
  expect_true(length((data[["ce"]])) == 5)
  expect_true(length((data[["baseline"]])) == 5)

  wce <- data[["ce"]][[5]]$pop
  woce <- data[["baseline"]][[5]]$pop
  check_1 <- median(wce$N)
  check_2 <- median(woce$N)

  expect_true(check_2 > 5)
  expect_true(check_2 > check_1)





  # ----------------------------------------
  # POPULATION MODEL TEST 2
  filename_rm <- system.file("extdata", "./matrix_test/stressor_magnitude_matrix.xlsx", package = "JoeModelCE")
  filename_sr <- system.file("extdata", "./matrix_test/stressor_response_matrix.xlsx", package = "JoeModelCE")
  dose <- StressorMagnitudeWorkbook(filename = filename_rm)
  sr_wb_dat <- StressorResponseWorkbook(filename = filename_sr)
  # Load the life cycle parameters
  filename_lc <- system.file("extdata", "life_cycles.csv", package = "JoeModelCE")
  life_cycle_params <- read.csv(filename_lc)

  # Choose target ID
  HUC_ID <- dose$HUC_ID[1]

  # Load in the habitat data optional
  data <- PopulationModel_Run(
    dose = dose,
    sr_wb_dat = sr_wb_dat,
    life_cycle_params = life_cycle_params,
    HUC_ID = HUC_ID,
    n_years = 50,
    MC_sims = 5,
    stressors = NA
  )

  expect_true(!(is.null(data)))
  expect_true(typeof(data) == "list")
  expect_true(all(names(data) == c("ce", "baseline", "MC_sims")))
  expect_true(length((data[["ce"]])) == 5)
  expect_true(length((data[["baseline"]])) == 5)

  wce <- data[["ce"]][[5]]$pop
  woce <- data[["baseline"]][[5]]$pop
  check_1 <- median(wce$N)
  check_2 <- median(woce$N)

  expect_true(check_2 > 5)
  expect_true(check_2 > check_1)



})
