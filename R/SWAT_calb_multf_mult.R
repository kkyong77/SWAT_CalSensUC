###############################################################################
#                             SWAT_calb_multf.R                                 #
################################################################################
# Purpose    : To write R scripts to prepare the folder/parameters for multi-running                     
################################################################################
# Output     : watershed outlet predictions                             #
################################################################################
# Author : Kyongho Son                                      #
# Started: 10/1/2020                                         #
# Updates: 10/4/2020 
# updated: 10/10/2020
################################################################################

# ii,number of sub-calibration
# ncal_m: number of calibration
# tcal: total calibration parameters
# para_cal: calibration parameter range/parameter locations
# TxtInout: folder containnig model parameters/inputs
# Calbf: folder saving the calibrated model outputs
# param_cal_ranges: model parameter values used for model calibration
# m: starting row number of model parameter values for model calibration
library(FME)
library(chron)
library(lubridate)
#library(hydroGOF)
library(gtools)
library(fs)
library(dplyr)






swat_calb_multf_mult<-function(ii,ncal_m,m,tcal){
  
  # define the calibration input/parameters folder name
  home_data<-"/mnt/4tbe/sonk739/SWAT-MR/M0"

  source("../R/Rfunctions.R")
  source("../R/checking_param_ranges.R")
  source("../R/ModifyInputFile_mult.R")
  source("../R/swat_hru_var_id.R")
  source("../R/swat_hru_var.R")
  source("../R/extract_wb.R")
 # source("../R/swat_hru_et.R")
 # source("../R/swat_hru_pet.R")
  #source("../R/extract_sub_v5.R")
  source("../R/extract.sub_LAI.R")
  source("../R/extract.sub_ET.R")


  TxtInoutf<-paste0(home_data,"/","TxtInOut",ii)
  print(TxtInoutf)
  #dir.exists(TxtInoutf)
  
  setwd(TxtInoutf)
  
  # subset calibrated parameter sets
  param_cal_ranges_mcal<-param_cal_ranges[m:(m+ncal_m-1),]

  param_cal_ranges_mcal    
  

  #print(para_cal)

  tncal_m=m+ncal_m-1
  # loop for the number of calibartion----
  for (j in m:(tncal_m)){
    ## modifying the parameters----
    #2) reset the parameter files-------------
    setwd(Backup)
    print(paste0("start backup files"))
    #start.time <- Sys.time()

   start.time<-Sys.time()

    for (k in 1:nrow(para_cal)) {
      
      Filename<-para_cal[k,3]
      files<-list.files(pattern=glob2rx(paste("*.",Filename,sep="")))
      #files<-list.files(pattern=glob2rx("*"))
      ## copy the orginal files to the TxtINOut folder----
      file_copy(files,TxtInoutf,overwrite = TRUE)
    }
      print(paste0("complete backup files","=",k))
   
    
    setwd(TxtInoutf)
    
    print(paste0("start modifying parameter values of simulation","=",j))
    #k, number of calibated parameters----
    
    file.remove("output.hru")
    
    
    jj=j-m+1

    print(jj)

    for (k in 1:nrow(para_cal)) {
      
      ## modifying the initial range of parameter values----
      Filename<-para_cal[k,3]
     
      files<-list.files(pattern=glob2rx(paste("*.",Filename,sep="")))
      id<-as.numeric(para_cal[para_cal$ParameterNmbr==k,1])
#      print(id)
#      print(files[1])
      newvalue<-as.numeric(param_cal_ranges_mcal[jj,k])
      row<-as.numeric(para_cal[para_cal$ParameterNmbr==k,4][1])
      col.ini<-as.numeric(para_cal[para_cal$ParameterNmbr==k,5])
      col.fin<-as.numeric(para_cal[para_cal$ParameterNmbr==k,6])
      dec<-as.numeric(para_cal[para_cal$ParameterNmbr==k,7])
      repl<-as.numeric(para_cal[para_cal$ParameterNmbr==k,8])
      mult<-as.numeric(para_cal[para_cal$ParameterNmbr==k,9])
      
            
      #############################################################
      #start.time <- Sys.time() 
      # start-modifying file---- 
      for (i in 1:length(files)){
        #### modifying the parameter values----
        
        ModifyInputFile_mult(id,newvalue,files[i],row,col.ini,col.fin,dec,repl,mult)
 #      print(files[i])
      }
      
      #end.time <- Sys.time()
      #time.taken <- end.time - start.time
      #########################################################
      #print(paste0("complete modifying parameter values","=",k))
    } # end -modifying files---
    ###################################################################
     end.time<-Sys.time()
     time.taken<-end.time-start.time
    print("time: modifying 22 parameters")
    print(time.taken)

    # running swat simulations with modified parameter sets----
    # updating swat executable file (using yilin codes)
    start.time<-Sys.time()

    system("./swat.exe") # or another version you desire
    print(paste0("numberof simulations","=",jj))
    #file.remove("output.hru")
    #   swat_rev670.exe
  
    end.time<-Sys.time()

    time.taken<-end.time-start.time
    print("time:simulation 20 years")
    print(time.taken)

    # the model outputs to calb folder-----------
    watout_files<-list.files(pattern=glob2rx("watout.dat"))
    #subbasin_files<-list.files(pattern=glob2rx("output.sub"))
    # saving model simulation results
    
    # define where (or folder name) the calibration results are saved
    home_data<-"/mnt/4tbe/sonk739/SWAT-MR/M0"
    Calbf<-paste0(home_data,"/","Calb_yr2000_2016_rad_rh_mult_1000sobol",ii)
    
#     Vald_yr2000_rad_rh_mult_1

    file.copy(watout_files,Calbf,overwrite = TRUE)
   # file.copy(subbasin_files,Calbf,overwrite=TRUE)
    # renaming the saved model outputs with the number of sub-calibration folder
    file.rename(paste0(Calbf,"/watout.dat"), paste(Calbf,paste0(watout_files,"_",j),sep='/'))
   # file.rename(paste0(Calbf,"/output.sub"),paste(Calbf,paste0(subbasin_files,"_",j),sep="/"))
   
    # ET
    
   # extract.sub_ET(TxtInoutf,87,"ET",jj,Calbf)
    # LAI
    # extract the hru level and sub-basin averaged LAI value
   # extract.sub_LAI(TxtInoutf,87,"LAI",jj,Calbf)

 
    # saving hru model output
    hru<-list.files(pattern=glob2rx(paste("0*.","hru",sep="")))
    # number of hrus
    num_hru<-length(hru)
    
    start.time<-Sys.time()

    # snotel site hru 
    swat_hru_var_id(TxtInoutf,"output.hru",270,"SNOmm","swe270_",j,Calbf)
    # basin-averaged SWE/ET/LAI/PET
    swat_hru_var(TxtInoutf,"output.hru",num_hru,"SNOmm","swe_",j,Calbf)
    swat_hru_var(TxtInoutf,"output.hru",num_hru,"ET","et_",j,Calbf)
    swat_hru_var(TxtInoutf,"output.hru",num_hru,"LAI","lai_",j,Calbf)
    swat_hru_var(TxtInoutf,"output.hru",num_hru,"PET","pet_",j,Calbf)
    
    end.time<-Sys.time()
    
    time.taken<-end.time-start.time
    print("time:extracting basin et/lai/swe")
    print(time.taken)


   
    
    # home directory-----
    home<-"/home/sonk739/SWAT-MR/M0/SWAT_CalbR"
    paramin<-paste(home,"param.in",sep="/")
    subbasin_area<-read.csv(paste(paramin,"m0_subbasin.csv",sep="/"),header=T)
    area<-subbasin_area$area_km2
  
    # compute the basin averaged water balance

    extract_wb(TxtInoutf,area,"output.sub","wb_",j,Calbf)

    file.remove("output.hru")
    ########################################################################
    # chaning directory to "TXtinOUT---------------------------- 
    
    
  } # end simulations
  
} # end of function

###############################################################################

#   # ii,number of sub-calibration
#   # ncal_m: number of calibration
#   # tcal: total calibration parameters
#   # para_cal: calibration parameter range/parameter locations
#   # TxtInout: folder containnig model parameters/inputs
#   # Calbf

#   # param_cal_ranges: model parameter values used for model calibration
#   # m: starting row number of model parameter values for model calibration

