################################################################################
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
library(hydroGOF)
library(gtools)
library(fs)

swat_calb_mult<-function(ii,ncal_m,m,tcal){
  
  # define the calibration input/parameters folder name
  TxtInoutf<-paste0(home,"/","TxtInOut",ii)
  print(TxtInoutf)
  #dir.exists(TxtInoutf)
  
  setwd(TxtInoutf)
  source("../R/Rfunctions.R")
  source("../R/checking_param_ranges.R")
  source("../R/ModifyInputFile.R")
  
  # subset calibrated parameter sets
  param_cal_ranges_mcal<-param_cal_ranges[m:(m+ncal_m-1),]
  
  # loop for the number of calibartion----
  for (j in 1:(ncal_m)){
    ## modifying the parameters----
    #2) reset the parameter files-------------
    setwd(Backup)
    print(paste0("start backup files"))
    #start.time <- Sys.time()
    
    for (k in 1:nrow(para_cal)) {
      
      Filename<-para_cal[k,3]
      files<-list.files(pattern=glob2rx(paste("*.",Filename,sep="")))
      ## copy the orginal files to the TxtINOut folder----
      file_copy(files,TxtInoutf,overwrite = TRUE)
      #print(paste0("complete backup files","=",k))
    }
    
    setwd(TxtInoutf)
    
    print(paste0("start modifying parameter values of simulation","=",j))
    #k, number of calibated parameters----
    
    
    for (k in 1:nrow(para_cal)) {
      
      ## modifying the initial range of parameter values----
      Filename<-para_cal[k,3]
      files<-list.files(pattern=glob2rx(paste("*.",Filename,sep="")))
      id<-as.numeric(para_cal[para_cal$ParameterNmbr==k,1])
      newvalue<-as.numeric(param_cal_ranges_mcal[j,k])
      row<-as.numeric(para_cal[para_cal$ParameterNmbr==k,4][1])
      col.ini<-as.numeric(para_cal[para_cal$ParameterNmbr==k,5])
      col.fin<-as.numeric(para_cal[para_cal$ParameterNmbr==k,6])
      dec<-as.numeric(para_cal[para_cal$ParameterNmbr==k,7])
      repl<-as.numeric(para_cal[para_cal$ParameterNmbr==k,8])
      #############################################################
      #start.time <- Sys.time() 
      # start-modifying file---- 
      for (i in 1:length(files)){
        #### modifying the parameter values----
        ModifyInputFile(id,newvalue,files[i],row,col.ini,col.fin,dec,repl)
        #print(files[i])
      }
      
      #end.time <- Sys.time()
      #time.taken <- end.time - start.time
      #########################################################
      #print(paste0("complete modifying parameter values","=",k))
    } # end -modifying files---
    ###################################################################
    
    # running swat simulations with modified parameter sets----
    system("swat.exe") # or another version you desire
    print(paste0("number of simulations","=",j))
    file.remove("output.hru")
    
    #copy the model outputs to calb folder-----------
    watout_files<-list.files(pattern=glob2rx("watout*"))
    # saving model simulation results
    
    # define where (or folder name) the calibration results are saved
    Calbf<-paste0(home,"/","Calb",ii)
    
    file.copy(watout_files,Calbf,overwrite = TRUE)
    
    # renaming the saved model outputs with the number of sub-calibration folder
    file.rename(paste0(Calbf,"/watout.dat"), paste(Calbf,paste0(watout_files,"_",j),sep='/'))
    
    
    ########################################################################
    # chaning directory to "TXtinOUT---------------------------- 
    
    
  } # end simulations
  
} # end of function

###############################################################################
# swat_calb_mult<-function(ii,ncal_m,m,tcal){
#   # ii,number of sub-calibration
#   # ncal_m: number of calibration
#   # tcal: total calibration parameters
#   # para_cal: calibration parameter range/parameter locations
#   # TxtInout: folder containnig model parameters/inputs
#   # Calbf: folder saving the calibrated model outputs
#   # param_cal_ranges: model parameter values used for model calibration
#   # m: starting row number of model parameter values for model calibration
# }  
