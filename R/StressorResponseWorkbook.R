#' Get Stressor Response Data from Excel Workbook
#'
#' @description Extract the stressor response data from each sheet in the Excel workbook.
#'
#' @details TODO add details.
#'
#' @param filename A string. Relative file name to the stressor response Excel workbook.
#'
#' @returns A list object of the main_sheet, stressor_names and stressor response dataframes (sr_dat)
#'
#' @export
StressorResponseWorkbook <- function(filename = NA) {

  # File to read and view stressor-response relations
  main_sheet <- readxl::read_excel(filename, sheet="Main")


  # Get Stressor names
  stressor_names <- as.character(main_sheet$Stressors)
  stressor_names <- stressor_names[!(is.na(stressor_names))]


  # Gather stressor-response relationships for other variables
  ReadSheets <- function(sheet) {
    dat <- readxl::read_xlsx(filename,
                     sheet = sheet)
    dat <- dat[, c(1:5)]
    colnames(dat) <- c("value", "mean_system_capacity", "sd", "lwr", "upr")
    return(dat)
  }
  sr_dat <- lapply(stressor_names, ReadSheets)
  names(sr_dat) <- stressor_names


  # Make pretty names
  pretty_names <- c()
  for(i in 1:length(stressor_names)) {
    this_name <- stressor_names[i]
    this_name <- gsub("_", " ", this_name)
    pretty_names <- c(pretty_names, this_name)
  }


  # Build export object
  ret_obj <- list()
  ret_obj$main_sheet <- main_sheet
  ret_obj$stressor_names <- stressor_names
  ret_obj$pretty_names <- pretty_names
  ret_obj$sr_dat <- sr_dat

  return(ret_obj)


}
