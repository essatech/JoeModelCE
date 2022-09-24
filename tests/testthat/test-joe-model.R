test_that("Joe Model Setup", {

  # ----------------------------------------
  # Import of stressor response and magnitude workbook
  filename_rm <- system.file("extdata", "stressor_magnitude_unc_ARTR.xlsx", package = "JoeModelCE")
  filename_sr <- system.file("extdata", "stressor_response_fixed_ARTR.xlsx", package = "JoeModelCE")

  dose <- StressorMagnitudeWorkbook(filename = filename_rm, scenario_worksheet = "natural_unc")
  sr_wb_dat <- StressorResponseWorkbook(filename = filename_sr)


  # ----------------------------------------------------------
  # Run the Basic Joe Model
  # ----------------------------------------------------------

  nsims <- 10

  jmr <- JoeModel_Run(dose = dose, sr_wb_dat = sr_wb_dat, MC_sims = nsims)


  # ----------------------------------------------------------
  # Run tests on outputs

  # Check ce.df object
  # Any missing watersheds?
  diff1 <- setdiff(unique(jmr$ce.df$HUC), unique(dose$HUC_ID))
  expect_true(length(diff1) == 0)

  diff2 <- setdiff(unique(jmr$sc.dose.df$HUC), unique(dose$HUC_ID))
  expect_true(length(diff2) == 0)

  # Correct number of simulations
  check_nsim <- unique(jmr$sc.dose.df$simulation) %>% length()
  expect_true(check_nsim == nsims)

  check_nsim <- unique(jmr$ce.df$simulation) %>% length()
  expect_true(check_nsim == nsims)

  # That CE values for watershed make sense
  expect_true(min(jmr$ce.df$CE) == 0)
  expect_true(max(jmr$ce.df$CE) <= 1 & max(jmr$ce.df$CE) >= 0.5)
  expect_true(sd(jmr$ce.df$CE) > 0)

  # That System Capacity Values Make Sense
  expect_true(min(jmr$sc.dose.df$sys.cap) == 0)
  # expect_true(max(jmr$sc.dose.df$sys.cap) == 1)
  expect_true(sd(jmr$sc.dose.df$sys.cap) > 0)




  # ----------------------------------------------------------
  # Re-Run the Joe Model - with partail variable subsets
  # ----------------------------------------------------------






})
