test_that("Test that batch population model runs successfully...", {

    # rm(list = ls())

    # Load the stress-response workbook
    filename_sr <- system.file("extdata", "stressor_response_fixed_ARTR.xlsx", package = "JoeModelCE")
    sr_wb_dat <- StressorResponseWorkbook(filename = filename_sr)

    # Load the stress-magnitude workbook
    filename_rm <- system.file("extdata", "stressor_magnitude_unc_ARTR.xlsx", package = "JoeModelCE")
    dose <- StressorMagnitudeWorkbook(filename = filename_rm, scenario_worksheet = "natural_unc")

    # Load the life cycle parameters
    filename_lc <- system.file("extdata", "life_cycles.csv", package = "JoeModelCE")
    life_cycle_params <- read.csv(filename_lc)

    # Choose target ID
    HUC_ID <- dose$HUC_ID[1]

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



    data <- PopulationModel_Run(
        dose = dose,
        sr_wb_dat = sr_wb_dat,
        life_cycle_params = life_cycle_params,
        HUC_ID = HUC_ID,
        n_years = 50,
        MC_sims = 20,
        stressors = NA,
        output_type = "adults"
    )
    expect_true(!(is.null(data)))
    expect_true(nrow(data) == ((50 + 1) * 2) * 20)
    expect_true(length(unique(data$MC_sim)) == 20)

    m_ce <- median(data$N[which(data$group == "ce")])
    m_no_ce <- median(data$N[which(data$group == "baseline")])
    
    expect_true(!(is.na(m_ce)))
    expect_true(!(is.na(m_no_ce)))


    if(FALSE) {
            
        library(ggplot2)
            ggplot(data, aes(x = year, y = N, color = group)) +
                stat_smooth(method="loess", span=0.1, se=TRUE, aes(fill = group), alpha=0.3) +
                theme_bw() +
                theme(
                    legend.position = "bottom",
                    legend.title = element_blank(),
                    legend.text = element_text(size = 8),
                    legend.key = element_blank(),
                    axis.text.x = element_text(angle = 90, hjust = 1, size = 8),
                    axis.text.y = element_text(size = 8),
                    #axis.title.x = element_blank(),
                    #axis.title.y = element_blank(),
                    strip.text = element_text(size = 8))
    }



    # ===================================================
    # Try different environmental ranges
    dose$Mean <- rnorm(n = nrow(dose), 0, 0.1)
    data <- PopulationModel_Run(
        dose = dose,
        sr_wb_dat = sr_wb_dat,
        life_cycle_params = life_cycle_params,
        HUC_ID = HUC_ID,
        n_years = 50,
        MC_sims = 5,
        stressors = NA,
        output_type = "adults"
    )
    m_ce <- median(data$N[which(data$group == "ce")])
    m_no_ce <- median(data$N[which(data$group == "baseline")])
    expect_true(m_ce < m_no_ce)



    # ===================================================
    # Try different environmental ranges
    dose$Mean <- rnorm(n = nrow(dose), 10000, 100)
    data <- PopulationModel_Run(
        dose = dose,
        sr_wb_dat = sr_wb_dat,
        life_cycle_params = life_cycle_params,
        HUC_ID = HUC_ID,
        n_years = 70,
        MC_sims = 5,
        stressors = NA,
        output_type = "adults"
    )
    m_ce <- median(data$N[which(data$group == "ce")], na.rm = TRUE)
    m_no_ce <- median(data$N[which(data$group == "baseline")], na.rm = TRUE)
    expect_true(m_ce < m_no_ce)






    # ===================================================
    # Set all responses to NA - should be closer to baseline
    dose$Mean <- NA
    data <- PopulationModel_Run(
        dose = dose,
        sr_wb_dat = sr_wb_dat,
        life_cycle_params = life_cycle_params,
        HUC_ID = HUC_ID,
        n_years = 70,
        MC_sims = 5,
        stressors = NA,
        output_type = "adults"
    )
    
    m_ce <- median(data$N[which(data$group == "ce")], na.rm = TRUE)
    m_no_ce <- median(data$N[which(data$group == "baseline")], na.rm = TRUE)
    diff <- abs(m_ce / m_no_ce - 1)
    # within 20% of baseline
    expect_true(diff < 0.2)
    



    if(FALSE) {
            
        library(ggplot2)
            ggplot(data, aes(x = year, y = N, color = group)) +
                stat_smooth(method="loess", span=0.1, se=TRUE, aes(fill = group), alpha=0.3) +
                theme_bw() +
                theme(
                    legend.position = "bottom",
                    legend.title = element_blank(),
                    legend.text = element_text(size = 8),
                    legend.key = element_blank(),
                    axis.text.x = element_text(angle = 90, hjust = 1, size = 8),
                    axis.text.y = element_text(size = 8),
                    #axis.title.x = element_blank(),
                    #axis.title.y = element_blank(),
                    strip.text = element_text(size = 8))
    }







})
