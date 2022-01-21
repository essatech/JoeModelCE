test_that("System Capacity", {

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

  stressors <- sr_wb_dat$stressor_names


  # -----------------------------------------------------
  # Test each functions for each stressot
  for(i in 1:length(stressors)) {

    # Dose-response curves for a single variable
    my_rf <- mean.resp.list[[i]]
    metric_name <- sr_wb_dat$stressor_names[i]

    # Main sheet variable info
    f.main.df <- data.frame(
              Stressors = metric_name,
              Stressor_cat = metric_name,
              Interaction = NA, # Additive, Minimum
              Linked = NA,
              Stress_Scale = "linear",
              Function = "continuous", # step
              Life_stages = "adult",
              Parameters = ""
            )

    # Dose response curve data
    f.stressor.df <- sr_wb_dat$sr_dat[[metric_name]]

    # Dose for HUC
    f.dose.df <- data.frame(
      HUC_ID = 123,
      NAME = "",
      Stressor = metric_name,
      Stressor_cat = metric_name,
      Mean = 0,
      SD = 0,
      Distribution = "normal", # Normal or lognormal
      Low_Limit = 0,
      Up_Limit = 100,
      Comments = "")



    # -----------------------------------------------------
    # TEST LEFT OF CURVE
    # -----------------------------------------------------

    # With SD set to zero test the following
    f.dose.df$Mean <- f.stressor.df$value[2]
    f.dose.df$SD <- 0.2 * f.dose.df$Mean



    # call system capacity function for each stressor
    temp.list <- SystemCapacity(
      f.dose.df = f.dose.df,
      f.main.df = f.main.df,
      f.stressor.df = f.stressor.df,
      f.mean.resp.list = mean.resp.list[[i]],
      n.sims = 1
    )
    temp.list$sys.cap == f.stressor.df$mean_system_capacity[1]
















  }



})
