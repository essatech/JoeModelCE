#' Mean Response Function
#'
#' @description Generate the mean response function for each stressor.
#'
#' @details Creates functions to interpolate the mean system capacity, SD, lower limit and upper limit.
#'
#' @param n.stressors Numeric. The number of stressors.
#' @param str.list List object. A list object of dataframes of stressor response relationships.
#' @param main Dataframe. A dataframe of the Main cover sheet for the stressor response relationships .
#'
#' @export
#'
mean_Response <- function(n.stressors = NA, str.list = NA, main = NA) {
  mean.resp.list <- list(NULL)

  stress_nms <- main$Stressors


  for (i in 1:length(stress_nms)) {
    this_stressor <- stress_nms[i]
    this_data <- str.list[[this_stressor]]
    this_main <- main[main$Stressors == this_stressor, ]

    # Note that for approxfunc to work properly with extrapolation
    # we must order the dosd-response dataframe such that the (x) value
    # field goes from highest to lowest
    this_data <- this_data[order(this_data$value), ]

    # Fix any NA values
    # Drop any NAs for value or mean system capacity
    this_data <- this_data[!(is.na(this_data$value)), ]
    this_data <- this_data[!(is.na(this_data$mean_system_capacity)), ]

    # Set any NAs for SD to zero
    this_data$sd <- ifelse(is.na(this_data$sd), 0, this_data$sd)

    # Set any NAs for lwr the median
    m_lwr <- stats::median(this_data$lwr, na.rm = TRUE)
    this_data$lwr <- ifelse(is.na(this_data$lwr), m_lwr, this_data$lwr)

    # Set any NAs for upr to the median
    m_upr <- stats::median(this_data$upr, na.rm = TRUE)
    this_data$upr <- ifelse(is.na(this_data$upr), m_upr, this_data$upr)






    if (this_main$Stress_Scale == "log") {
      x <- this_data[, 1]
    } else {
      x <- this_data[, 1]
    }

    # func.type sets whether approximation function is continuous or stepped (constant)
    func.type <- ifelse(this_main$Function == "continuous", "linear", "constant")


    # The current response object
    this_resp <- this_data

    func.type <- as.character(func.type)

    # divide by 100 to convert percents to decimals

    # Mean System Capacity
    mdat <- this_resp$mean_system_capacity / 100

    # Function for the mean
    af1 <- stats::approxfun(
      x = x$value,
      y = mdat,
      method = func.type,
      yleft = mdat[1],
      yright = mdat[length(mdat)]
    )


    # Function for the SD
    mdat <- this_resp$sd / 100
    af2 <- stats::approxfun(
      x = x$value,
      y = mdat,
      method = func.type,
      yleft = mdat[1],
      yright = mdat[length(mdat)]
    )


    # Lower Limit LL
    mdat <- this_resp$lwr / 100
    af3 <- stats::approxfun(
      x = x$value,
      y = mdat,
      method = func.type,
      yleft = mdat[1],
      yright = mdat[length(mdat)]
    )


    # Upper Limit UL
    mdat <- this_resp$upr / 100
    af4 <- stats::approxfun(
      x = x$value,
      y = mdat,
      method = func.type,
      yleft = mdat[1],
      yright = mdat[length(mdat)]
    )

    # Function list
    temp.list <- list(af1, af2, af3, af4)

    mean.resp.list[[i]] <- temp.list
  }

  return(mean.resp.list)
}
