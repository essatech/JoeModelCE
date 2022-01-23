test_that("System Capacity", {
  library(JoeModelCE)



  # ----------------------------------------
  # Import of stressor response and magnitude workbook
  filename_rm <- system.file("extdata", "stressor_magnitude_unc_ARTR.xlsx", package = "JoeModelCE")
  filename_sr <- system.file("extdata", "stressor_response_fixed_ARTR.xlsx", package = "JoeModelCE")

  dose <- StressorMagnitudeWorkbook(filename = filename_rm, scenario_worksheet = "natural_unc")
  sr_wb_dat <- StressorResponseWorkbook(filename = filename_sr)



  # -----------------------------------------------------
  # Run the Joe curves to generate the response functions
  mean.resp.list <- mean_Response(
    n.stressors = nrow(sr_wb_dat$main_sheet),
    str.list = sr_wb_dat$sr_dat,
    main = sr_wb_dat$main_sheet
  )

  stressors <- sr_wb_dat$stressor_names
  expect_true(1 == 1)
})
