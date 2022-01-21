test_that("Mean Response", {

  library(JoeModelCE)

  # ----------------------------------------
  # Import of stressor response and magnitude workbook
  dose <- StressorMagnitudeWorkbook(filename = "./inst/stressor_magnitude_unc_ARTR.xlsx", scenario_worksheet = "natural_unc")
  sr_wb_dat <- StressorResponseWorkbook(filename = "./inst/stressor-response_fixed_ARTR.xlsx")


  # -----------------------------------------------------
  # Run the Joe curves to generate the response functions
  mean.resp.list <- mean_Response(
    n.stressors = nrow(sr_wb_dat$main_sheet),
    str.list = sr_wb_dat$sr_dat,
    main = sr_wb_dat$main_sheet
  )


  # ---------------------------------------------------------
  # Ensure that worksheet names and main tab line up properly
  n1 <- names(sr_wb_dat$sr_dat)
  n2 <- sr_wb_dat$main_sheet$Stressors
  expect_true(all(n1 == n2))

  stressors <- sr_wb_dat$stressor_names

  # Loop through stressors to confirm functionality
  for(p in 1:length(stressors)) {

    # Doese response data
    this_stressor <- stressors[p]
    m_index <- which(names(sr_wb_dat$sr_dat) == this_stressor)
    dr_data <- sr_wb_dat$sr_dat[[m_index]]

    # Functions
    f1 <- mean.resp.list[[m_index]][[1]] # Mean
    f2 <- mean.resp.list[[m_index]][[1]] # SD
    f3 <- mean.resp.list[[m_index]][[1]] # LL
    f4 <- mean.resp.list[[m_index]][[1]] # UL


    # LEFT LIMIT
    f1(dr_data$value[1])




  }


})
