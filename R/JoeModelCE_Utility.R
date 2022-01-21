#' tbeta Random
#'
#' @description Returns N random estimates for truncated beta distribution given vectors for mean and SD for the untruncated distribution of system capacity.
#'
#' @details Function returns returns one random value for each mean and sd value. Will not run with 0 or 1 values.
#'
#' @param mn_est Vector of mean values.
#' @param sd_est Vector of standard deviations.
#' @param low.limit Numeric. Defaults to 0.
#' @param up.limit Numeric. Defaults to 1 .
#'
#' @keywords internal
tbeta_rnd <- function(mn_est,
                      sd_est,
                      low.limit = 0,
                      up.limit = 1) {
  rnd.est <- numeric()

  # Issue loading package
  # https://bertcarnell.github.io/truncateddist/reference/tbeta.html
  truncateddist_rtbeta <- function(n,
                                   alpha,
                                   beta,
                                   a = 0,
                                   b = 1) {
    stopifnot(n > 0 & all(beta > 0) & all(alpha > 0))
    x <- runif(n)
    Fa <- pbeta(a, alpha, beta)
    Fb <- pbeta(b, alpha, beta)
    y <- (1 - x) * Fa + x * Fb
    return(qbeta(y, alpha, beta))
  }

  for (i in 1:length(mn_est)) {
    if (sd_est[i] == 0 | is.na(sd_est[i])) {
      #no variance in response
      rnd.est[i] <- mn_est[i] #simply return the mean estimates
    } else {
      f_beta <- beta_param(mn_est[i], sd_est[i])

      # Need to account for margins
      if (!(all(all(f_beta[2] > 0) & all(f_beta[1] > 0)))) {
        rnd.est[i] <- mn_est[i] #simply return the mean estimates
      } else {
        suppressWarnings({
          rnd.est[i] <- truncateddist_rtbeta(
            1,
            alpha = f_beta[1]$alpha,
            beta = f_beta[2]$beta,
            a = ifelse(is.na(low.limit[i]), 0, low.limit[i]),
            b = ifelse(is.na(up.limit[i]), 1, up.limit[i])
          )

        })

      }


    }
  }
  return(rnd.est)
}


#' Cumulative System Capacity Calculation
#'
#' @details Function to calculate cumulative system capacity mean and sd across simulations for each HUC from a series of Monte Carlo sims
#'
#' @param df A dataframe
#' @keywords internal
mn.sd.huc <- function(df) {
  mn <- mean(df$CE)
  sd <- sd(df$CE)
  return(data.frame(mean = mn, sd = sd))
}



#' Apply CSC Across dataframe
#'
#' @details ddply function to calculate cumulative system capacity, the df contains system capacity for each stressor for a given HUC and simulation
#'
#' @param df A dataframe
#' @keywords internal
ce.func <- function(df) {
  #separate stressors without a minimum interaction
  sys.cap.no.int <- df$sys.cap[df$int.type != "Minimum"]

  #for those with a minimum interaction take the minimum
  sys.cap.min <- tapply(df$sys.cap[df$int.type == "Minimum"],
                        df$link[df$int.type == "Minimum"],
                        min)

  # AT THIS POINT NO OTHER INTERACTIONS ARE CONSIDERED
  # NOTE - Total mortality is addressed prior to calculation of system capacity
  # Calculate the product across all cumulative effects
  # accounting for interactions
  ce.df <- data.frame(CE = prod(c(sys.cap.no.int, sys.cap.min)))

  return(ce.df)
}



#' rtnorm from Truncated Distributions
#'
#' @details rtnorm from Truncated Distributions
#'
#' @keywords internal
rtnorm_TruncatedDistributions <- function(n,
                                          mean = 0,
                                          sd = 1,
                                          a = -Inf,
                                          b = Inf) {

  # All zeros
  if(mean == 0 & sd == 0) {
    all_zeros <- rep(0, n)
    return(all_zeros)
  }

  # Send values back from sample
  x <- runif(n)
  Fa <- pnorm(a, mean, sd)
  Fb <- pnorm(b, mean, sd)
  y <- (1 - x) * Fa + x * Fb

  ret <- qnorm(y, mean, sd)

  return(ret)

}

