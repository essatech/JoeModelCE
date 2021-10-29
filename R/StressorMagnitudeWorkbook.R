#' Get Stressor Magnitude Data from Excel Workbook
#'
#' @description Extract the stressor magnitude data from each sheet in the Excel workbook.
#'
#' @details TODO add details.
#'
#' @param filename A string. Relative file name to the stressor magnitude Excel workbook.
#' @param scenario_worksheet A string. Worksheet name for the scenario.
#'
#' @returns ...
#'
#' @export
StressorMagnitudeWorkbook <- function(filename = NA, scenario_worksheet = NA) {

  # File to read and view stressor-response relations
  data <- readxl::read_excel(filename, sheet = scenario_worksheet)

  # Round values to two decimal places
  data$Mean <- round(data$Mean, 2)

  print("DROP OTHER MORT SOURCES....TEMP TO DO")
  data <- data[which(data$Sub_Type == "Nat_mort" | data$Sub_Type == "N/A"), ]

  return(data)


}
