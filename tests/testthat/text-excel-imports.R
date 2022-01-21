test_that("Excel imports work", {

  library(JoeModelCE)

  # Test import of stressor magnitude workbook
  smw <- StressorMagnitudeWorkbook(filename = "./inst/stressor_magnitude_unc_ARTR.xlsx", scenario_worksheet = "natural_unc")

  # Check basics
  expect_true(ncol(smw) == 10)
  expect_true(nrow(smw) > 500)
  expect_true(setequal(colnames(smw), c("HUC_ID", "NAME", "Stressor", "Stressor_cat", "Mean", "SD", "Distribution", "Low_Limit", "Up_Limit", "Comments")))


  # ----------------------------------------
  # Test import of stressor response workbook
  srw <- StressorResponseWorkbook(filename = "./inst/stressor-response_fixed_ARTR.xlsx")

  # Check basics
  expect_true(class(srw) == "list")

  # That all names are equal
  s1 <- srw$main_sheet$Stressors
  s2 <- srw$stressor_names
  s3 <- names(srw$sr_dat)
  expect_true(setequal(s1, s2))
  expect_true(setequal(s1, s3))
  expect_true(setequal(s2, s3))


})
