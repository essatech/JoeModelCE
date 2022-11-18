#' Batch the JOEMODEL population model across multiple HUCs
#'
#' Runs the JOEMODEL PopulationModel_Run in batch mode across different HUCS and saved the population outputs in excel files and saves the lambdas in a list for both the baseline and the ce scanrios. 
#'
#' @param dir_in Character path of the directory where the JOEMODEL input files live. Make sure that  the directory holds: 1) stressor response Excel file; 2) stressor magnitude Excel file; 3) life cycle parameters Excel file
#' @param dir_out character path to the directory where new files will be saved
#' @param dose_file A character that specified the name of the Excel filename (with extension .xlsx) of the stressor Response file (e.g."stressor_response_fixed_ARTR.xlsx")
#' @param sr_wb_dat A character that specified the name of the Excel filename (with extension .xlsx) of the stressor magnitude file (e.g."stressor_magnitude_unc_ARTR.xlsx")
#' @param life_cycle_params A character that specified the name of the Excel filename (with extension .xlsx) that folds the life cycle parameters (e.g."life_cycles.csv")
#' @param HUC_ID a numeric vector that holds the IDs of the HUCs to be assessed
#' @param n_years A numeric number that specifies hoe long should the JOEMODEL be run.If not specified, it will default to 300
#' @param MC_sims A number that specified how many Monte Carlo (MC) simulations will be run for each HUC. Each MC run will sample from the stressor's distribution and return the corresponding mean system capacity. If not specified; it will default to 5
#' @param stressors A character vector that holds the stressors that you want to include when running the sensitivity analysis. These stressors are common to ALL HUCs
#' @param output_type A character that should be set either to adult or to "full". The former will only return the adult population. The latter will return the population for all stages
#' @returns The function will return:
#' \itemize{
#'  \item Excel files that hold the population numbers. A separate file is created for each HUC. Within each file, the populations of either the 4 stages (if you opt for output_type="full") or the population of the adult phase (if you opt for output_type="adult) is saved over time (for both the baseline and the ce scenarios) for all the Monte Carlo simulations. The columns are named to indicate the population stage, the scenario (baseline or ce), and the monte carlo simulation number associated with the output (e.g. column Stage_1_baseline_sim_1 = Population of Stage 1 for the basline scenario under simulation 1).
#'  \item The function also returns a list of all the lambdas calculated over time for the baseline and ce scenarios across the number of Monte Carlo simulations specified. Each list element is named by its corresponding HUC and holds the result for that particular HUC
#' @examples
#'\dontrun{
#' library(JoeModelCE)
#' POP <- PopulationModel_Batch_Run(dir_in="C:/JOEMODEL", dir_out="C:/JOEMODEL/Results", dose_file = "stressor_response_fixed_ARTR.xlsx", 
#'                                sr_wb_dat = "stressor_magnitude_unc_ARTR.xlsx", life_cycle_params = "life_cycles.csv", 
#'          HUC_ID = c(1701010201, 1701010202), n_years = 300, MC_sims = 5, stressors = NA, output_type="full") 
#'         }
#'         


PopulationModel_Batch_Run=function (dir_in, dir_out, dose_file = NA, sr_wb_dat = NA, life_cycle_params = NA, 
            HUC_ID = NA, n_years = 100, MC_sims = 10, stressors = NA,
            output_type = "full") 
  {
#########################      JUST FOR TESTING. Delete Afterwards    ##########################################################################
xx=PopulationModel_Batch_Run(dir_in=NA, dir_out=NA, dose = NA, sr_wb_dat = NA, life_cycle_params = NA, 
                                  HUC_ID = c(1701010201,1701010202,1701010203), n_years = 100, MC_sims = 10, stressors = NA, 
                                   output_type = "full") 

dose = NA; sr_wb_dat = NA; life_cycle_params = NA; 
HUC_ID =c(1701010201,1701010202,1701010203); n_years = 150; MC_sims = 5; stressors = NA;
output_type = "full"
################################################################################################################################################  
  # Check and install needed Packages
  packages <- c("ggplot2", "openxlsx", "dplyr", "tidyverse","lubridate")
  # Install packages not yet installed
  installed_packages <- packages %in% rownames(installed.packages())
  if (any(installed_packages == FALSE)) {
    install.packages(packages[!installed_packages])
  }
  
  # Packages loading
  invisible(lapply(packages, library, character.only = TRUE))
  
##############################################################  Some housekeeping to find file ##################################################
  #If the in and out directories are not specified, then just choose where the package files are stores as the directories
  dir_in=ifelse(is.na(dir_in),dirname(system.file("extdata", "stressor_response_fixed_ARTR.xlsx", package = "JoeModelCE")),dir_in)
  dir_out=ifelse(is.na(dir_out),dirname(system.file("extdata", "stressor_response_fixed_ARTR.xlsx", package = "JoeModelCE")),dir_out)
  
  #stress-magnitude workbook
  if(is.na(dose_file)){
    warning("You need to specify the stressor_magnitude Excel file. The sensitivity analysis will proceed using the default file that came with the package")
    # Load the stress-magnitude workbook
    filename_rm <- system.file("extdata", "stressor_magnitude_unc_ARTR.xlsx", package = "JoeModelCE")
    dose <- StressorMagnitudeWorkbook(filename = filename_rm, scenario_worksheet = "natural_unc")
  }else if (file.exists(paste(dir_in, dose_file, sep=""))){
    dose <- StressorMagnitudeWorkbook(filename = paste(dir_in, dose_file, sep=""), scenario_worksheet = "natural_unc")
  } else if(file.exists(paste(dir_in, dose_file, sep="/"))){
    dose <- StressorMagnitudeWorkbook(filename = paste(dir_in, dose_file, sep="/"), scenario_worksheet = "natural_unc")
  } else{
    stop("Your file is not located inside the proper directory or it is not formated properly to run in the JOEMODEL")
  }
  
  #stress-response workbook
  if(is.na(sr_wb_dat)){
    warning("You need to specify the stressor_response Excel file. The sensitivity analysis will proceed using the default file that came with the package")
    # Load the stress-response workbook
    filename_sr <- system.file("extdata", "stressor_response_fixed_ARTR.xlsx", package = "JoeModelCE")
    sr_wb_dat <- StressorResponseWorkbook(filename = filename_sr)
  }else if(file.exists(paste(dir_in, sr_wb_dat, sep=""))){
    sr_wb_dat <- StressorResponseWorkbook(filename = paste(dir_in, sr_wb_dat, sep=""))
  }else if(file.exists(paste(dir_in, sr_wb_dat, sep="/"))){
    sr_wb_dat <- StressorResponseWorkbook(filename = paste(dir_in, sr_wb_dat, sep="/"))
  }else{
    stop("Your file is not located inside the proper directory or it is not formated properly to run in the JOEMODEL")
  }
  
  #life cycle parameters
  if(is.na(life_cycle_params)){
    warning("You need to specify the life cycle parameters Excel file. The sensitivity analysis will proceed using the default file that came with the package")
    # Load the life cycle parameters
    filename_lc <- system.file("extdata", "life_cycles.csv", package = "JoeModelCE")
    life_cycle_params <- read.csv(filename_lc)
  }else if(file.exists(paste(dir_in, life_cycle_params, sep=""))){
    filename_lc=paste(dir_in, life_cycle_params, sep="")
    life_cycle_params <-read.csv( paste(dir_in, life_cycle_params, sep=""))
  } else if(file.exists(paste(dir_in, life_cycle_params, sep="/"))){
    filename_lc=paste(dir_in, life_cycle_params, sep="/")
    life_cycle_params <- read.csv(paste(dir_in, life_cycle_params, sep="/"))
  } else{
    stop("Your file is not located inside the proper directory or it is not formated properly to run in the JOEMODEL")
  }
  
  #Stressors
  stressors=ifelse(is.na(stressors),NA,stressors)
  #The HUC IDs
  if(length(HUC_ID)<1){
    stop("You need to specify at least 1 HUC code")
  }
  #Use apply to run the PopulationModel_Run across all HUCs
  ASSO=lapply(HUC_ID,JoeModelCE::PopulationModel_Run, dose = dose, sr_wb_dat = sr_wb_dat, life_cycle_params = life_cycle_params, 
              n_years = n_years, MC_sims = MC_sims, stressors = NA,                       output_type = "full")
  names(ASSO)=HUC_ID
  # Generate a list where each HUC is an element. In each element, you will have the following columns:
  # year, Stage1_P_base_sim1,Stage2_P_base_sim1,Stage3_P_base_sim1,Stage4_P_base_sim1,.....Stage1_P_ce_simn,.....Stage2_P_ce_simn,.....Stage3_P_ce_simn,.....Stage4_P_ce_simn
  # Generate a list of all the lambdas
excel_list=list()
lambda_list=list()
sim_period=length(ASSO[[1]]$ce[[1]]$pop$year)
#columns are year, sim, and then pop1....pop4 for baseline and pop1.....P4 for ce
for (i in 1: length(HUC_ID)){
HUC_Df=ASSO[[1]]$ce[[1]]$pop$year
base=ASSO[[i]]$baseline %>% map_dfc("N", .default = NA)
ce=ASSO[[i]]$ce %>% map_dfc("N", .default = NA)
HUC_Df=cbind(HUC_Df, base,ce)
HUC_Df=as.data.frame(HUC_Df)
colnames(HUC_Df)=c("year", paste(rep("Stage",MC_sims*4),rep(seq(1:4),MC_sims), rep(c("baseline", "ce"), each=MC_sims), rep("sim", MC_sims*4*2), rep(rep(1:MC_sims, each=4),2),sep="_"))
excel_list[[i]]=HUC_Df
lambda.base=ASSO[[i]]$baseline %>% map_dfc("lambdas", .default = NA)
lambda.ce=  ASSO[[i]]$ce %>% map_dfc("lambdas", .default = NA)
lambda.all=cbind(ASSO[[1]]$ce[[1]]$pop$year[-1],lambda.base,lambda.ce)
colnames(lambda.all)=c("year", paste(rep("lambda_baseline",MC_sims),"sim", 1:MC_sims, sep="_"), paste(rep("lambda_ce",MC_sims),"sim", 1:MC_sims, sep="_"))
lambda_list[[i]]=lambda.all
if(output_type=="full"){
openxlsx::write.xlsx(excel_list[[i]], file=paste(dir_out,"/HUC",HUC_ID[i],"_",output_type,"_pop_sim.xlsx", sep=""),overwrite=TRUE)}
if(output_type=="adult"){
  openxlsx::write.xlsx(excel_list[[i]][,c(1,seq(5,(4*MC_sims+1), by=4))], file=paste(dir_out,"/HUC",HUC_ID[i],"_",output_type,"_pop_sim.xlsx", sep=""), overwrite=TRUE)}
}
names(lambda_list)=HUC_ID
return(lamda_list)
}#End of function
