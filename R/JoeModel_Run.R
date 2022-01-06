#' Joe Model Run
#'
#' @description Runs the Joe Model.
#'
#' @details Runs the Joe Model for cumulative system capacity across stressors and watersheds.
#'
#' @param dose dataframe. Stressor magnitude file exported from StressorMagnitudeWorkbook().
#' @param sr_wb_dat list object. Stressor response workbook returned from StressorResponseWorkbook().
#' @param MC.sims numeric. set number of Monte Carlo simulations for the Joe model.
#'
#' @export
JoeModel_Run <- function(dose = NA,
                         sr_wb_dat = NA,
                         MC.sims = 100) {
  # Stressor Magnitude
  # dose = sm_wb_dat

  # Stressor Response
  stressors <- sr_wb_dat$stressor_names

  MC.sims = 100

  # Re-create the main sheet object
  main.sheet <- sr_wb_dat$main_sheet

  # Get the stressor names
  stressor.list <- sr_wb_dat$sr_dat


  # Run the Joe curves to generate the response functions
  mean.resp.list <-
    mean.resp.fun(
      n.stressors = nrow(sr_wb_dat$main_sheet),
      str.list = sr_wb_dat$sr_dat,
      main = sr_wb_dat$main_sheet
    )


  #We now have two important objects:

  #1) dose - This is a tibble of the doses for
  #each stressor (mean, SD, distribution type, low limit,
  #up limit).  At the moment there are only two distribution
  #types allowed for uncertainty in the stressor dose, truncated normal
  #and truncated lognormal.

  #2) mean.resp.list - This is a list of lists
  #The primary list is for each stressor ordered according to the tibble
  #main.sheet.  Each list within this list contains
  #4 approx functions that calculate the mean, SD, low limit and upper limit
  #for system capacity given a dose for that stressor.

  # Get unique HUCs and stressors.
  hucs <- unique(dose$HUC_ID)


  # Output is going to be stored in a 3D array to start.
  # Each row will be a HUC, each column is system capacity
  # for a stressor and sheets are different Monte Carlo
  # simulations.  Order of HUCs (rows) and system capacity per
  # stressor (columns) are given in the vectors hucs and stressors,
  # respectively. Make sys.capacity Global

  sys.capacity <- array(
    NA,
    dim = c(length(hucs),
            length(stressors),
            MC.sims),
    dimnames = list(hucs,
                    stressors,
                    1:MC.sims)
  )

  # the next array and list is more for debugging purposes

  dose.values <- array(
    NA,
    dim = c(length(hucs),
            length(stressors),
            MC.sims),
    dimnames = list(hucs,
                    stressors,
                    1:MC.sims)
  )

  dose.values.list <- array(list(),
                            dim = c(length(hucs),
                                    length(stressors)))



  # Loop through HUCs and stressors

  for (i in 1:length(hucs)) {
    for (j in 1:length(stressors)) {
      # find combination of HUC and stressor in the dose table
      pnt.dose <- intersect(grep(hucs[i], dose$HUC_ID),
                            grep(stressors[j], dose$Stressor))

      # If nothing set - assume system capacity is 100%
      if (length(pnt.dose) == 0) {
        sys.capacity[i, j, ] <- 1 # System capacity is 1 if mssing
        dose.values[i, j, ] <- NA
        dose.values.list[i, j][[1]] <- NA
        next
      }

      # find stressor in main.sheet for relationship
      pnt.curv <- grep(stressors[j], main.sheet$Stressors)

      # call system capacity function for each stressor
      temp.list <- sys.cap.func(
        f.dose.df = dose[pnt.dose, ],
        f.main.df = main.sheet[pnt.curv, ],
        f.stressor.df = stressor.list[[stressors[j]]],
        f.mean.resp.list = mean.resp.list[[pnt.curv]],
        n.sims = MC.sims
      )

      #assign system capacity for each stressor to array.
      sys.capacity[i, j, ] <- temp.list$sys.cap
      #store dose values as this is good output as well
      dose.values[i, j, ] <- temp.list$dose
      #The next dose array stores doses as a list and includes
      #the individual additive doses (i.e., mortality doses)
      dose.values.list[i, j][[1]] <- temp.list$dose.mat

    }
    # end j
  }
  # end k


  #Print error messages if NA appears in the system capacity
  #or stressor values arrays
  if (any(is.na(sys.capacity)))
    message("At least one NA in system capacity array")
  if (any(is.na(dose.values)))
    message("At least one NA in stressor values array")

  # change the 3D array to a data frame
  # adply is really slow
  # system.time(sc.df<-adply(sys.capacity,c(1,2,3),function(x) data.frame(sys.cap=x)))
  # melt is rocket fuel!!
  sc.df <- reshape2::melt(sys.capacity)

  #do the same for doses as this is useful output
  #dose.df<-adply(dose.values,c(1,2,3),function(x) data.frame(dose=x))

  dose.df <- reshape2::melt(dose.values)


  #rename columns
  sc.df <-
    dplyr::rename(
      sc.df,
      HUC = Var1,
      Stressor = Var2,
      simulation = Var3,
      sys.cap = value
    )

  #do same for dose values
  dose.df <-
    dplyr::rename(
      dose.df,
      HUC = Var1,
      Stressor = Var2,
      simulation = Var3,
      dose = value
    )

  #combine SC and dose dataframes (this df is used for output so make global)
  sc.dose.df <- merge(dose.df, sc.df)

  #add interaction type and link to sc.df data.frame
  sc.df$int.type <-
    main.sheet$Interaction[match(sc.df$Stressor, main.sheet$Stressors)]
  sc.df$link <-
    main.sheet$Linked[match(sc.df$Stressor, main.sheet$Stressors)]


  # calculate cumulative system capacity by multiplying values in each row (HUC)
  # the complicating factor is some stressors are linked
  # (e.g., take the minimum of the linked and not their product)
  # so we'll need to account for this.
  # use ddply and the function ce.func to create the cumulative effect
  # and make it global
  # uses ddply which is pretty slow (use dplyr??)
  # MJB updated with dplyr
  ce_df_raw <- sc.df %>% dplyr::group_by(HUC, simulation) %>%
    dplyr::group_modify( ~ ce.func(.x))

  ce.df <- ce_df_raw



  # Build return object
  return_list <- list()
  return_list$ce.df <- ce.df
  return_list$sc.dose.df <- sc.dose.df
  return(return_list)


}
