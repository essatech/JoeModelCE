#' System Capacity
#'
#' @description Calculates the system capacity for each stressor.
#'
#' @details function to calculate system capacity for each stressor.  Takes dataframes for doses and stressor.list plus a list for the approx functions (f. for local list or dataframe) note some stressors have multiple doses (Additive interaction)
#'
#' @param df A dataframe
SystemCapacity <- function(f.dose.df,
                         f.main.df,
                         f.stressor.df,
                         f.mean.resp.list,
                         n.sims = 100) {

  # Stressor Name
  sub.stressors <- f.dose.df$Stressor

  # go through each stressor and randomly assign a dose
  n.dose <- length(sub.stressors)

  # matrix to store random doses, rows are additive sub types, cols are n.sims
  rnd.dose.mat <- matrix(NA, nrow = n.dose, ncol = n.sims)

  for (i in 1:n.dose) {

    # Randomly generate doses based on mean, SD, limits and distribution type
    if (f.dose.df$Distribution[i] == "lognormal") {

      # If mean < 0 change mean to minimum limit (can't have zero or negative on log scale)
      mn.val <- ifelse(f.dose.df$Mean[i] <= 0,
                       f.dose.df$Low_Limit[i],
                       f.dose.df$Mean[i])

      # If SD is zero adjust to a very small number
      if (f.dose.df$SD[i] == 0) {
        rnd.dose.mat[i,] <- rep(mn.val, n.sims)
      } else {
        rnd.dose.mat[i,] <- rtlnorm(
          n.sims,
          log(mn.val),
          f.dose.df$SD[i],
          a = f.dose.df$Low_Limit[i],
          b = f.dose.df$Up_Limit[i]
        )

      }

    } else {

      # Normal distribution (lognormal is above)
      if (f.dose.df$SD[i] == 0) {
        rnd.dose.mat[i,] <- rep(f.dose.df$Mean[i], n.sims)
      } else {
        # Sample value at random
        rnd.dose.mat[i,] <-
          rtnorm_TruncatedDistributions(
            n.sims,
            f.dose.df$Mean[i],
            f.dose.df$SD[i],
            a = f.dose.df$Low_Limit[i],
            b = f.dose.df$Up_Limit[i]
          )
      }

    }
  }
  # end i loop for doses

  # Combine multiple stressors across rows into a single dose vector
  # multiple stressors must be proportions (ie, conditional mortalities)
  rnd.dose <- 1 - apply(rnd.dose.mat, 2, function(x)
    prod(1 - x))



  # Constrain dose to be >= smallest dose and <= largest dose
  # from the curve function (ie., first/last value used for approx. function)
  # IE. extrapolation takes the last given system capacity score

  rnd.dose <- ifelse(rnd.dose < min(f.stressor.df[, 1], na.rm = T),
                     min(f.stressor.df[, 1], na.rm = T),
                     rnd.dose)

  rnd.dose <- ifelse(rnd.dose > max(f.stressor.df[, 1], na.rm = T),
                     max(f.stressor.df[, 1], na.rm = T),
                     rnd.dose)


  # Change rnd.dose to log values if stress-response relation is logarithmic
  if (f.main.df$Stress_Scale == "log") {
    x.dose <- log(rnd.dose)
  } else {
    x.dose <- rnd.dose
  }


  # calculate system.capacity vector (x.dose is a vector)
  sys.capacity <- tbeta_rnd(
    mn_est = f.mean.resp.list[[1]](x.dose),
    sd_est = f.mean.resp.list[[2]](x.dose),
    low.limit = f.mean.resp.list[[3]](x.dose),
    up.limit = f.mean.resp.list[[4]](x.dose)
  )


  # Build return object
  ret_obj <- list(
    sys.cap = sys.capacity,
    dose = rnd.dose,
    dose.mat = rnd.dose.mat
  )

  return(ret_obj)

}
