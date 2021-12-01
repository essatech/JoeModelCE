#' Mean Response Function
#'
#' @description Generate the mean response function for each stressor.
#'
#' @details Creates functions to interpolate the mean system capacity, SD, lower limit and upper limit.
#'
#' @param n.stressors Numeric. The number of stressors.
#' @param str.list List object. A list object of dataframes of stressor response relationships.
#' @param main Dataframe. A dataframe of the Main cover sheet for the stressor response relationships.
#'
#' @export
#'
mean.resp.fun <- function(n.stressors = NA, str.list = NA, main = NA) {

  mean.resp.list <- list(NULL)

  stress_nms <- main$Stressors

  for (i in 1:length(stress_nms)) {

    this_stressor <- stress_nms[i]
    this_data <- str.list[[this_stressor]]
    this_main <- main[main$Stressors == this_stressor, ]

    if (this_main$Stress_Scale == "log") {

      x <- log(str.list[[this_stressor]][,1])

    } else {

      x <- str.list[[this_stressor]][,1]
    }

    # func.type sets whether approximation function is continuous or stepped (constant)

    func.type <- ifelse(this_main$Function == "continuous", "linear", "constant")


    # The current response object
    this_resp <- this_data

    func.type <- as.character(func.type)

    # divide by 100 to convert percents to decimals

    # Mean System Capacity
    mdat <- this_resp$mean_system_capacity/100
    af1 <- approxfun(
                    x = x$value,
                    y = mdat,
                    method = func.type,
                    yleft = mdat[1],
                    yright = mdat[length(mdat)])

    # SD
    mdat <- this_resp$sd/100
    af2 <- approxfun(
      x = x$value,
      y = mdat,
      method = func.type,
      yleft = mdat[1],
      yright = mdat[length(mdat)])

    # LL
    mdat <- this_resp$lwr/100
    af3 <- approxfun(
      x = x$value,
      y = mdat,
      method = func.type,
      yleft = mdat[1],
      yright = mdat[length(mdat)])

  # UL
  mdat <- this_resp$upr/100
  af4 <- approxfun(
    x = x$value,
    y = mdat,
    method = func.type,
    yleft = mdat[1],
    yright = mdat[length(mdat)])


  temp.list <- list(af1, af2, af3, af4)

  mean.resp.list[[i]] <- temp.list

  }

  return(mean.resp.list)
}
