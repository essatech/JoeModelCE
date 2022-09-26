#' Population Model Run
#'
#' @description Runs the population model
#'
#' @details Runs the integrated population model within
#'  the Joe Model stress-response framework. The population model is
#'  run for a single subwatershed unit (HUC_ID). Inputs for the
#'  PopulationModel_Run include the stressor-response and stressor magnitude
#'  workbooks as well as the life cycle parameters table.
#'
#' @param dose dataframe. Stressor magnitude dataset
#'  imported from StressorMagnitudeWorkbook().
#' @param sr_wb_dat list object. Stressor response workbook
#' imported from StressorResponseWorkbook().
#' @param life_cycle_params dataframe. Life cycle parameters.
#' @param HUC_ID character. HUC_ID for the subwatershed unit.
#' @param n_years numeric. Number of years to run the population.
#' @param MC_sims numeric. set number of Monte
#'  Carlo simulations for the Population Model.
#' @param stressors (optional) character vector of stressor
#'  names to include in the Population Model. Leave the default
#'  value as NA if you wish to include all stressors
#'  applicable to the population model.
#' @param output_type (optional) character. Set to "full" for all data of "adults" for only adult data.
#'
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' library(JoeModelCE)
#' }
#'
#' @export
PopulationModel_Run <- function(dose = NA,
                                sr_wb_dat = NA,
                                life_cycle_params = NA,
                                HUC_ID = NA,
                                n_years = 100,
                                MC_sims = 10,
                                stressors = NA,
                                output_type = "full") {

    # Define variables in function as null
    # .data <- HUC <- simulation <- NULL

    #------------------------------------------------------------------------
    # Run the population model time series projection for a target watershed
    #------------------------------------------------------------------------

    #------------------------------------------------------------------------
    # Subset the stressor magnitude dataframe to
    # include only the target HUC unit
    # (population model is run seperatly for each HUC)
    ce_df_sub <- dose[which(dose$HUC_ID == HUC_ID), ]

    # Omit non-target stressors from the stressor magnitude and sr dataframes
    if (!is.na(stressors)) {
        # Subset the stressor magnitude dataframe to include only the target stressors
        ce_df_sub <- ce_df_sub[which(ce_df_sub$Stressor %in% stressors), ]
        # Subset the stressor response dataframe to include only the target stressors
        sr_wb_dat$main_sheet <- sr_wb_dat$main_sheet[which(sr_wb_dat$main_sheet$Stressors %in% stressors), ]
        sr_wb_dat$stressor_names <- sr_wb_dat$stressor_names[sr_wb_dat$stressor_names %in% stressors] # nolint
        sr_wb_dat$sr_dat <- sr_wb_dat$sr_dat[which(names(sr_wb_dat$sr_dat) %in% stressors)]
    }

    # Merge stressor_response main sheet data
    ce_df_sub$Stressor_cat <- NULL
    ce_df_sub <-
        merge(
            ce_df_sub,
            sr_wb_dat$main_sheet,
            by.x = "Stressor",
            by.y = "Stressors",
            all.x = TRUE
        )

    # Stressor Magnitude...
    smw_sample <-
        data.frame(
            HUC_ID = ce_df_sub$HUC_ID,
            NAME = ce_df_sub$NAME,
            Stressor = ce_df_sub$Stressor,
            Stressor_cat = ce_df_sub$Stressor_cat,
            Mean = ce_df_sub$Mean,
            SD = ce_df_sub$SD,
            Distribution = ce_df_sub$Distribution,
            Low_Limit = ce_df_sub$Low_Limit,
            Up_Limit = ce_df_sub$Up_Limit
        )

    #------------------------------------------------------------------------
    # Calculate the dose and system capacity score for the selected HUC
    # for each stressor

    jm <- JoeModelCE::JoeModel_Run(
        dose = smw_sample,
        sr_wb_dat = sr_wb_dat,
        MC_sims = MC_sims,
        adult_sys_cap = FALSE
    )

    # Gather summary at stressor level
    dobj <- jm$sc.dose.df

    # add on missing attr columns
    merge_cols <-
        ce_df_sub[, c(
            "Stressor",
            "Life_stages",
            "Parameters",
            "Stressor_cat"
        )]

    merge_cols <-
        merge_cols[!(duplicated(merge_cols)), ]

    m_all <-
        merge(
            merge_cols,
            dobj,
            by.x = "Stressor",
            by.y = "Stressor",
            all.x = TRUE,
            all.y = TRUE
        )

    # Fix col names
    colnames(m_all)[colnames(m_all) == "Stressors"] <-
        "Stressor"
    colnames(m_all)[colnames(m_all) == "Life_stages"] <-
        "life_stage"
    colnames(m_all)[colnames(m_all) == "Parameters"] <-
        "parameter"
    colnames(m_all)[colnames(m_all) == "Stressor_cat"] <-
        "Stressor_cat"

    # Return cleaned object
    CE_df <- m_all


    #------------------------------------------------------------------------
    # Setup the population model to project the populaiton forward in time

    # Gather population model inputs
    # Setup objects for population model
    pop_mod_setup <-
        JoeModelCE::pop_model_setup(life_cycles = life_cycle_params)
    # Build matrix elements for population model
    pop_mod_mat <-
        JoeModelCE::pop_model_matrix_elements(pop_mod_setup = pop_mod_setup)


    # Set the K.adj (K adjustment prior to pop model run)
    life_histories <- pop_mod_mat$life_histories
    # Mathematical expression of the transition matrix
    life_stages_symbolic <-
        pop_mod_mat$life_stages_symbolic
    # Mathematical expression of the density matrix
    density_stage_symbolic <-
        pop_mod_mat$density_stage_symbolic

    all_outputs <- list()
    all_outputs_baseline <- list()
    counter_huc <- 1

    #------------------------------------------------------------------------
    # Run the population model for each batch replicate

    # Need to copy and rename some variables for code consistency
    this_huc <- HUC_ID
    huc_outputs <- list()
    huc_outputs_baseline <- list()
    counter_sim <- 1
    test_n_replicates <- MC_sims
    test_n_years <- n_years

    # Loop through simulations per HUC
    for (ii in 1:test_n_replicates) {

        # Environmental sample for this rep
        if (is.null(CE_df)) {
            CE_df_rep <- CE_df
        } else {
            CE_df_rep <-
                CE_df[which(CE_df$simulation == ii &
                    CE_df$HUC == this_huc), ]
            CE_df_rep <-
                CE_df_rep[!(duplicated(CE_df_rep[, c("Stressor", "life_stage", "HUC")])), ]
            # Do not include regular Joe parameters
            CE_df_rep <-
                CE_df_rep[which(!(is.na(CE_df_rep$parameter))), ]
        }

        # Run simple population projection - project forward through time
        # Run the simulation with CE stressors
        run_with_ce <-
            JoeModelCE::Projection_DD(
                M.mx = life_stages_symbolic,
                # projection matrix expression
                D.mx = density_stage_symbolic,
                # density-dependence matrix
                H.mx = NULL,
                dat = life_histories,
                # life history data
                K = life_histories$Ka,
                # initial pop size as stage-structure vector
                Nyears = test_n_years,
                # years to run simulation
                p.cat = 0,
                # Probability of catastrophe
                CE_df = CE_df_rep
            )

        # Run baseline with no CE
        run_with_baseline <-
            JoeModelCE::Projection_DD(
                M.mx = life_stages_symbolic,
                # projection matrix expression
                D.mx = density_stage_symbolic,
                # density-dependence matrix
                H.mx = NULL,
                dat = life_histories,
                # life history data
                K = life_histories$Ka,
                # initial pop size as stage-structure vector
                Nyears = test_n_years,
                # years to run simulation
                p.cat = 0,
                # Probability of catastrophe
                CE_df = NULL
            )

        # Gather info - for CE run
        run_with_ce$vars <- NULL
        run_with_ce$Cat. <- NULL

        run_with_ce$info <-
            data.frame(
                huc_id = this_huc,
                sim = ii,
                type = "CE"
            )

        huc_outputs[[counter_sim]] <- run_with_ce

        # Gather info - for CE run
        run_with_baseline$vars <- NULL
        run_with_baseline$Cat. <- NULL

        run_with_baseline$info <-
            data.frame(
                huc_id = this_huc,
                sim = ii,
                type = "baseline"
            )

        huc_outputs_baseline[[counter_sim]] <-
            run_with_baseline

        counter_sim <- counter_sim + 1
    }

    #------------------------------------------------------------------------
    # Gather the outputs from the population model

    full_output <- list()
    full_output[["ce"]] <- huc_outputs
    full_output[["baseline"]] <- huc_outputs_baseline
    full_output[["MC_sims"]] <- MC_sims


    #------------------------------------------------------------------------
    # Return the adult population vectors in a clean dataframe
    if (output_type == "adults") {
        # Gather the adult vectors
        getPopvec <- function(x) {
            x[["pop"]]
        }
        n_adults <- lapply(full_output[["ce"]], getPopvec)
        avec <- do.call("rbind", n_adults)
        avec$MC_sim <- rep(1:MC_sims, each = n_years + 1)
        avec$group <- "ce"

        n_adults <- lapply(full_output[["baseline"]], getPopvec)
        bvec <- do.call("rbind", n_adults)
        bvec$MC_sim <- rep(1:MC_sims, each = n_years + 1)
        bvec$group <- "baseline"
       
        # Gather the population vectors
        vec_all <- rbind(avec, bvec)
        return(vec_all)
    }


        #------------------------------------------------------------------------
    # Return all the data in a full list format
    if (output_type == "full") {
        return(full_output)
    }
}
