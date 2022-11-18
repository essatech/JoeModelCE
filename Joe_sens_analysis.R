#' JOEMOdel sensitivity analysis
#'
#' Runs an OAT type of sensitivity analysis on the parameters in the JOEMODEL. It can handle multiple HUCs at the same time. The sensitivity is reported for both the baselie and the CE scenarios
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
#' @param last_years A numeric that specifies the number of years prior to the last that you consider that the population reached stability in. If not specified; it will assume that the last 20% of your years were stable (e.g. if your n_years was set to 100; then the default stable years will be the last 20 years). The OAT sensitivity analysis will only consider the population in those last years 
#' @param sens_perc_change A numeric that specifies the percent change in the model parameters that the sensitivity analysis will be considered (e.g. specify 10 if you want to test how a 10 percent change in the parameters to be tested will impact the population in the defined stable years). YOU can specify -10 if you want to see how reducing the parameter by 10 percent will impact the stable populations. The function assumes the same percent change across all parameters.If not specified, it will default to 10 (percent) 
#' @param sens_param A character vector that specifies the JOEMODEL parameters that you would like to run the OAT sensitivity analysis on. Please report the Name and not the Parameter (e.g. report "Nstage: and not"Number of life stages"). If not specified; it will assume that you want to run it on all parameters EXCEPT Nstage and any parameter whose value in the life cycle parameters file was set to zero. 
#' @returns The function will return:
#' \itemize{
#'  \item SA: A list where each element holds the median sensitivities of the JOEMODEL both for the baseline and the CE runs for a given HUC. The list will have the same number of elements as your HUCs
#'  \item life_cycles_sens_analysis_HUCXXXXXXXXXX  a set of .csv files. Each HUC will have its own .csv file. They will be saved in your dir_out 
#' }
#' @examples
#'\dontrun{
#' library(JoeModelCE)
#' SA <- PopulationModel_Run_Sen(dir_in="C:/JOEMODEL", dir_out="C:/JOEMODEL/Results", dose_file = "stressor_response_fixed_ARTR.xlsx", 
#'                                sr_wb_dat = "stressor_magnitude_unc_ARTR.xlsx", life_cycle_params = "life_cycles.csv", 
#'          HUC_ID = c(1701010201, 1701010202), n_years = 300, MC_sims = 5, stressors = NA, last_years=20,
#'         sens_perc_change=10, sens_param=c("k","surv_1", "eps", "M.cv")) 
#'         }
#'         
PopulationModel_Run_Sen= function(dir_in=NA, dir_out=NA, dose_file = "stressor_response_fixed_ARTR.xlsx", 
                                                        sr_wb_dat = "stressor_magnitude_unc_ARTR.xlsx", life_cycle_params = "life_cycles.csv", 
                                  HUC_ID = NA, n_years = NA, MC_sims = NA, stressors = NA, last_years=NA,
                                 sens_perc_change=10, sens_param=NA){
#################################################
  # Currently only looking at perc change in adult population in the last_years 
#################################################
                                     
##################################Some housekeeping
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
  #The HUC IDs
  if(length(HUC_ID)<1){
    stop("You need to specify at least 1 HUC code")
  }
  #The last years when the population is supposed to have stabilized
  last_years=ifelse(is.na(last_years),n_years-0.8*n_years,last_years)
 
  # Generate a file that will be used for the sensitivity analysis
  file.copy(from = filename_lc, to = paste(dir_out,"/",substr(basename(filename_lc), 1, nchar(basename(filename_lc))-4),"_sens_analysis.csv", sep=""), overwrite =T)
  sens_file=read.csv( paste(dir_out,"//",substr(basename(filename_lc), 1, nchar(basename(filename_lc))-4),"_sens_analysis.csv", sep=""), header=T)
  #Which are the parameters that you want to analyze?
  #If there is an NA in the sens_param, do a full analysis
  if(sum(is.na(sens_param))>0){
  sens_param=c("k","events","eps","int","SE","S0","SR","surv_1","surv_2","surv_3","surv_4","year_1","year_2","year_3",
                           "year_4","cr_E","cr_0","cr_1","cr_2","cr_3","cr_4","mat_1","mat_2","mat_3","mat_4","eps_sd","egg_rho","M.cv","M.rho")
                           
    #"Adult capacity", "Spawn events per female", "Eggs per female spawn", "spawning interval", "egg survival", "yoy survival", "sex ratio",
    #                            "Hatchling Survival", "Juvenile Survival", "Sub-adult Survival", "Adult Survival", "Years as hatchling","years as juvenile","years as subadult","years as adult","egg survival compensation ratio","yoy survival compensation ratio","hatchling survival compensation ratio","juvenile survival compensation ratio",
    #                            "subadult survival compensation ratio","adult survival compensation ratio","maturity as hatchling","maturity as juvenile","maturity as subadult","maturity as adult","variance in eggs per female","correlation in egg fecundity through time","coefficient of variation in stage-specific mortality","correlation in mortality through time")
   } else if (length(setdiff(sens_param,c("k","events","eps","int","SE","S0","SR","surv_1","surv_2","surv_3","surv_4","year_1","year_2","year_3",
                                          "year_4","cr_E","cr_0","cr_1","cr_2","cr_3","cr_4","mat_1","mat_2","mat_3","mat_4","eps_sd","egg_rho","M.cv","M.rho")))==0) {
    sens_param= sens_param
   } else{
      stop("You specified one or more wrong parameters to assess their sensitivity. Allowed parameters include: k,events,eps,int,SE,S0,SR,surv_1,surv_2,surv_3,surv_4,year_1,year_2,year_3,year_4,cr_E,cr_0,cr_1,cr_2,cr_3,cr_4,mat_1,mat_2,mat_3,mat_4,eps_sd,egg_rho,M.cv,M.rho")
    }
  sens_file$Value.sa=  sens_file$Value*(1+sens_perc_change/100)
  #Remove the possibility to change the number of stages
  
  #Remove all parameters that were not specified
  param.specified=pmatch(sens_param,sens_file$Name)
  sens_file$Value.sa[-param.specified]=NA
  #Remove all parameters that are set to 0
  sens_file$Value.sa[which(sens_file$Value==0)]=NA
  #Remove number of stages
  sens_file$Value.sa[which(sens_file$Name=="Nstage")]=NA

########### loop over all HUCs  
for (h in 1:length(HUC_ID)){
huc_ID=HUC_ID[h]
if(sum(dose$HUC_ID==huc_ID)==0){
  stop("Your HUC is not present in the Stressor Magnitude file")
}else{
##############Setup the model  
####Choose the HUCs that we are interested in
  ce_df_sub <- dose[which(dose$HUC_ID == huc_ID), ]
  ####Choose the stressors and reformat the sheet. No change for sensitivity here
  if (!is.na(stressors)) {
    ce_df_sub <- ce_df_sub[which(ce_df_sub$Stressor %in% 
                                   stressors), ]
    sr_wb_dat$main_sheet <- sr_wb_dat$main_sheet[which(sr_wb_dat$main_sheet$Stressors %in% 
                                                         stressors), ]
    sr_wb_dat$stressor_names <- sr_wb_dat$stressor_names[sr_wb_dat$stressor_names %in% 
                                                           stressors]
    sr_wb_dat$sr_dat <- sr_wb_dat$sr_dat[which(names(sr_wb_dat$sr_dat) %in% 
                                                 stressors)]
  }else{
    ce_df_sub=ce_df_sub
  }
  ce_df_sub$Stressor_cat <- NULL
  ce_df_sub <- merge(ce_df_sub, sr_wb_dat$main_sheet, by.x = "Stressor", 
                     by.y = "Stressors", all.x = TRUE)
  smw_sample <- data.frame(HUC_ID = ce_df_sub$HUC_ID, NAME = ce_df_sub$NAME, 
                           Stressor = ce_df_sub$Stressor, Stressor_cat = ce_df_sub$Stressor_cat, 
                           Mean = ce_df_sub$Mean, SD = ce_df_sub$SD, Distribution = ce_df_sub$Distribution, 
                           Low_Limit = ce_df_sub$Low_Limit, Up_Limit = ce_df_sub$Up_Limit)
  #Generate random draws and calculate the capacity based on the curves
  jm <- JoeModelCE::JoeModel_Run(dose = smw_sample, sr_wb_dat = sr_wb_dat, 
                                 MC_sims = MC_sims, adult_sys_cap = FALSE)
  dobj <- jm$sc.dose.df
  merge_cols <- ce_df_sub[, c("Stressor", "Life_stages", "Parameters", 
                              "Stressor_cat")]
  merge_cols <- merge_cols[!(duplicated(merge_cols)), ]
  m_all <- merge(merge_cols, dobj, by.x = "Stressor", by.y = "Stressor", 
                 all.x = TRUE, all.y = TRUE)
  colnames(m_all)[colnames(m_all) == "Stressors"] <- "Stressor"
  colnames(m_all)[colnames(m_all) == "Life_stages"] <- "life_stage"
  colnames(m_all)[colnames(m_all) == "Parameters"] <- "parameter"
  colnames(m_all)[colnames(m_all) == "Stressor_cat"] <- "Stressor_cat"
  CE_df <- m_all
  
  
##############Here is where the sensitivity analysis will start###############
  
#Setup the "TRUE" population model
pop_mod_setup <- JoeModelCE::pop_model_setup(life_cycles = life_cycle_params)
#Generate the matrix for the "TRUE" model
pop_mod_mat <- JoeModelCE::pop_model_matrix_elements(pop_mod_setup = pop_mod_setup)
life_histories <- pop_mod_mat$life_histories
life_stages_symbolic <- pop_mod_mat$life_stages_symbolic
density_stage_symbolic <- pop_mod_mat$density_stage_symbolic

############################################################################  
#####Placeholders to hold the percent change
perc.change.baseline=matrix(NA, ncol=MC_sims+1, nrow=sum(!is.na(sens_file$Value.sa)))
perc.change.baseline=as.data.frame(perc.change.baseline)
colnames(perc.change.baseline)=c("Parameters", paste(rep("Sens_analysis_baseline"), seq(1:MC_sims), sep="_"))
perc.change.baseline[,1]=sens_file$Parameters[-which(is.na(sens_file$Value.sa))]

perc.change.ce=matrix(NA, ncol=MC_sims+1, nrow=sum(!is.na(sens_file$Value.sa)))
perc.change.ce=as.data.frame(perc.change.ce)
colnames(perc.change.ce)=c("Parameters", paste(rep("Sens_analysis_CE"), seq(1:MC_sims), sep="_"))
perc.change.ce[,1]=sens_file$Parameters[-which(is.na(sens_file$Value.sa))]
############################################################################

for (j in 1:sum(!is.na(sens_file$Value.sa))){
  #Setup the "SENS" population model
  sens_i=sens_file$Name[which(!is.na(sens_file$Value.sa))][j]
  life_cycle_params_sens=life_cycle_params
  life_cycle_params_sens$Value[which(life_cycle_params_sens$Name==sens_i)]=sens_file$Value.sa[which(sens_file$Name==sens_i)]
  #life_cycle_params_sens$Value[which(life_cycle_params_sens$Name==sens_i)]=1000
  pop_mod_setup.sens <- JoeModelCE::pop_model_setup(life_cycles = life_cycle_params_sens)
  #Generate the matrix for the "SENS" model
  pop_mod_mat.sens <- JoeModelCE::pop_model_matrix_elements(pop_mod_setup = pop_mod_setup.sens)
  life_histories.sens <- pop_mod_mat.sens$life_histories
  life_stages_symbolic.sens <- pop_mod_mat.sens$life_stages_symbolic
  density_stage_symbolic.sens <- pop_mod_mat.sens$density_stage_symbolic
   
  for (ii in 1:MC_sims) {
    if (is.null(CE_df)) {
      CE_df_rep <- CE_df
    }else {
      CE_df_rep <- CE_df[which(CE_df$simulation == ii & 
                                 CE_df$HUC == this_huc), ]
      CE_df_rep <- CE_df_rep[!(duplicated(CE_df_rep[, c("Stressor", 
                                                        "life_stage", "HUC")])), ]
      CE_df_rep <- CE_df_rep[which(!(is.na(CE_df_rep$parameter))), ]
      #print(CE_df_rep)
    }
   #}#remove
    ####Generate the projections for the baseline and for the CE
    run_with_ce <- JoeModelCE::Projection_DD(M.mx = life_stages_symbolic, 
                                             D.mx = density_stage_symbolic, H.mx = NULL, dat = life_histories, 
                                             K = life_histories$Ka, Nyears = test_n_years, p.cat = 0, 
                                             CE_df = CE_df_rep)
    pop_ce_stable=tail(run_with_ce$pop$N,last_years)
    
    run_with_baseline <- JoeModelCE::Projection_DD(M.mx = life_stages_symbolic, 
                                                   D.mx = density_stage_symbolic, H.mx = NULL, dat = life_histories, 
                                                   K = life_histories$Ka, Nyears = test_n_years, p.cat = 0, 
                                                   CE_df = NULL)
    run_with_baseline_stable=tail(run_with_baseline$pop$N,last_years)
   
    #points(run_with_baseline_stable~seq(1:last_years), col="blue", typ="l")
    #points(pop_ce_stable~seq(1:last_years), col="red", typ="l")
    
     ####Generate the projections for the baseline and for the CE with the sensitivity_analysis
    run_with_ce.sens <- JoeModelCE::Projection_DD(M.mx = life_stages_symbolic.sens, 
                                             D.mx = density_stage_symbolic.sens, H.mx = NULL, dat = life_histories.sens, 
                                             K = life_histories.sens$Ka, Nyears = test_n_years, p.cat = 0, 
                                             CE_df = CE_df_rep)
    pop_ce_stable.sens=tail(run_with_ce.sens$pop$N,last_years)
    
    run_with_baseline.sens <- JoeModelCE::Projection_DD(M.mx = life_stages_symbolic.sens, 
                                                   D.mx = density_stage_symbolic.sens, H.mx = NULL, dat = life_histories.sens, 
                                                   K = life_histories.sens$Ka, Nyears = test_n_years, p.cat = 0, 
                                                   CE_df = NULL)
    run_with_baseline_stable.sens=tail(run_with_baseline.sens$pop$N,last_years)
    

    perc.change.baseline[j,ii+1] = 100*(median(run_with_baseline_stable.sens)-median(run_with_baseline_stable))/median(run_with_baseline_stable)
    perc.change.ce[j,ii+1]       = 100*(median(pop_ce_stable.sens)-median(pop_ce_stable))/median(pop_ce_stable)
     } #close the loop for all MC simulations
   } #close the loop for all parameters
test1=left_join(sens_file,perc.change.baseline, by=c("Parameters"="Parameters"))
test2=left_join(test1,perc.change.ce, by=c("Parameters"="Parameters"))
#export teh sensitivity file for each HUC
write.csv(test2,paste(dir_out,"//",substr(basename(filename_lc), 1, nchar(basename(filename_lc))-4),"_sens_analysis_HUC", HUC_ID[h],".csv", sep=""), row.names = FALSE)
median_CE=apply(test2[,grep("CE",colnames(test2))],1,median)
median_base=apply(test2[,grep("baseline",colnames(test2))],1,median)
reconstruct.sens=data.frame(Paremeters=test2$Parameters,Name=test2$Name,Value=test2$Value,Value.sa=test2$Value.sa,Median_sensitivity_perc_change_baseline=median_base, Median_sensitivity_perc_change_CE=median_CE)
SA[[h]]=reconstruct.sens
names(SA)[h] = HUC_ID[h]
print(h)
}#the else loop for the HUCs ends here
}#close the loop for the HUCs
  return(SA)
} #close the loop for the function