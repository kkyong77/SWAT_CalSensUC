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
################################################################################
#                             SWAT_calb_multf_prep.R                                 #
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

# preparing files-------------------------------
# sensitivity inputs files based on the SWAT setup
# reading files
## copy files
#https://fs.r-lib.org/reference/copy.html
###############################################
#C:\Project\Columbia_SWAT\codes\SWAT_CalbR\SWAT_CalbR\Backup
## copy orginal files to TxtInOut-----
## loading library----------------------------
library(FME)
library(chron)
library(lubridate)
library(hydroGOF)
library(gtools)
library(fs)
###############################################

# home directory-----
home<-"C:/Project/Columbia_SWAT/codes/SWAT_CalbR/SWAT_CalbR"

# backup directory-----
#Backup<-paste(home,"Backup",sep="/")
#setwd(Backup)


# list of backup folder files
#backup_files<-list.files(pattern=glob2rx("*"))

## copy the orginal files to the TxtINOut folder----
#TxtInOut<-paste(home,"TxtInOut",sep="/")
#file.copy(backup_files, TxtInOut,overwrite = TRUE)

# make directory-----------------------------
#number of directory and copy the directory 
# n=5
# for (i in 1:n){
#   path<-paste(home,paste0("TxtInOut",i),sep="/")
#   path_cal<-paste(home,paste0("Calb",i),sep="/")
#   
#   dir.create(path) 
#   dir.create(path_cal)
#   dir_copy(TxtInOut,path, overwrite = TRUE)
# }



##########################################

args<-commandArgs(trailingOnly = TRUE)
ii<-args[1]
ncal_m<-args[2]
m<-args[3]
tcal<-args[4]


### parameter calibration folder----
paramin<-paste(home,"param.in",sep="/")

# reading parameter ranges------
param_file<-"ParamRanges-Sens.txt"
#param_ranges<-read.csv(paste(paramin,param_file,sep="/"),header=T,sep="\t")
param_ranges<-read.csv(paste(paramin,param_file,sep="/"),header=T,sep="")


## reading parameter for calibration----- 
#para_sens<- data.table::fread(paste(paramin,"ParamFiles-Sens.txt",sep="/"),skip = 0,sep = "auto",
#                       header="auto", data.table = F, verbose = F)

para_cal<- data.table::fread(paste(paramin,"ParamFiles_Cals.txt",sep="/"),skip = 0,sep = "auto",
                             header="auto", data.table = F, verbose = F)

## determine which parameter will be calibrated----


# number of calibrated/sensitivity analysis parameters----
unique(para_cal$ParameterName)
length(unique(para_cal$ParameterName))

#C:\Project\Columbia_SWAT\codes\SWAT_CalbR\SWAT_CalbR\TxtInOut

# default calibration folder----
Calb<-paste(home,"Calb",sep="/")

#Calb_lh<-paste(home,"Calb_lh",sep="/")


## setwd folder "TxtInOut"----
TxtInOut<-paste(home,"TxtInOut",sep="/")
setwd(TxtInOut)

# editing the file.cio file for modifying the simulation periods/other setups
#shell.exec("file.cio")

# if replace the orignal value 1
# otherwise, 2
# number of running

# number of calibration-----
ncal=500
# generating the parameter ranges----
# number of parameters,tcal----
tcal=21
param_cal_ranges<-matrix(0,ncol=tcal+1,nrow=ncal)
param_cal_ranges_lh<-matrix(0,ncol=tcal+1,nrow=ncal)
###1) Latin hypercube------------ 
## 21 parameters
min_par<-c(-0.2,param_ranges[2,3],param_ranges[3,3],param_ranges[4,3],param_ranges[5,3],param_ranges[6,3],param_ranges[7,3],param_ranges[8,3],param_ranges[9,3],-0.2,-0.2,param_ranges[12,3],param_ranges[13,3],param_ranges[14,3],param_ranges[15,3],param_ranges[16,3],param_ranges[17,3],param_ranges[18,3],param_ranges[19,3],param_ranges[20,3],param_ranges[21,3])
max_par<-c(0.2,param_ranges[2,4],param_ranges[3,4],param_ranges[4,4],param_ranges[5,4],param_ranges[6,4],param_ranges[7,4],param_ranges[8,4],param_ranges[9,4],0.2,0.2,param_ranges[12,4],param_ranges[13,4],param_ranges[14,4],param_ranges[15,4],param_ranges[16,4],param_ranges[17,4],param_ranges[18,4],param_ranges[19,4],param_ranges[20,4],param_ranges[21,4])
parRange <- data.frame(min=min_par,max=max_par)
rownames(parRange) <-c(para_cal$ParameterName)  
#pairs(Latinhyper(parRange, ncal), main = "Latin hypercube")
# }
param_cal_ranges_lh<-Latinhyper(parRange, ncal)
## saving the generated parameter values-----
header<-c(para_cal$ParameterName,"ncal")


write.csv(param_cal_ranges_lh,file="../Calb_lh/param_calb_lh.csv",row.names = FALSE)
###############################################################################


##2) monte caro simulations/random uniform distribution-----
# generating the parameter values with random generator
tcal=21
param_cal_ranges<-matrix(0,ncol=tcal+1,nrow=ncal)

param_cal_ranges[,1]=runif(ncal,min=-0.2,max=0.2)
param_cal_ranges[,2]=runif(ncal,min=param_ranges[2,3],max=param_ranges[2,4])
param_cal_ranges[,3]=runif(ncal,min=param_ranges[3,3],max=param_ranges[3,4])
param_cal_ranges[,4]=runif(ncal,min=param_ranges[4,3],max=param_ranges[4,4])
param_cal_ranges[,5]=runif(ncal,min=param_ranges[5,3],max=param_ranges[5,4])
param_cal_ranges[,6]=runif(ncal,min=param_ranges[6,3],max=param_ranges[6,4])
param_cal_ranges[,7]=runif(ncal,min=param_ranges[7,3],max=param_ranges[7,4])
param_cal_ranges[,8]=runif(ncal,min=param_ranges[8,3],max=param_ranges[8,4])
param_cal_ranges[,9]=runif(ncal,min=param_ranges[9,3],max=param_ranges[9,4])

#SOL_K (relative)----
param_cal_ranges[,10]=runif(ncal,min=-0.2,max=0.2)
#SOL_AWC (relative)---
param_cal_ranges[,11]=runif(ncal,min=-0.2,max=0.2)

param_cal_ranges[,12]=runif(ncal,min=param_ranges[12,3],max=param_ranges[12,4])
param_cal_ranges[,13]=runif(ncal,min=param_ranges[13,3],max=param_ranges[13,4])
#OV_N (relative)-----
param_cal_ranges[,14]=runif(ncal,min=param_ranges[14,3],max=param_ranges[14,4])
param_cal_ranges[,15]=runif(ncal,min=param_ranges[15,3],max=param_ranges[15,4])
param_cal_ranges[,16]=runif(ncal,min=param_ranges[16,3],max=param_ranges[16,4])
param_cal_ranges[,17]=runif(ncal,min=param_ranges[17,3],max=param_ranges[17,4])
param_cal_ranges[,18]=runif(ncal,min=param_ranges[18,3],max=param_ranges[18,4])
param_cal_ranges[,19]=runif(ncal,min=param_ranges[19,3],max=param_ranges[19,4])
param_cal_ranges[,20]=runif(ncal,min=param_ranges[20,3],max=param_ranges[20,4])
param_cal_ranges[,21]=runif(ncal,min=param_ranges[21,3],max=param_ranges[21,4])
# adding number of parameter sets----
param_cal_ranges[,22]=1:ncal
###############################################################################

## saving the generated parameter values-----
header<-c(para_cal$ParameterName,"ncal")
colnames(param_cal_ranges)<-header
write.csv(param_cal_ranges,file="../Calb/param_calb.csv",row.names = FALSE)
###############################################################################


swat_calb_mult<-function(ii,ncal_m,m,tcal){
   
   # define the calibration input/parameters folder name
   TxtInoutf<-paste0(home,"/","TxtInout",ii)
   
   dir.exists(TxtInoutf)
   
    setwd(TxtInoutf)
    source("../R/Rfunctions.R")
    source("../R/checking_param_ranges.R")
    source("../R/ModifyInputFile.R")
  
  # loop for the number of calibartion----
  for (j in 1:(ncal_m-1)){
    ## modifying the parameters----
    #2) reset the parameter files-------------
    setwd(Backup)
    print(paste0("start backup files"))
    #start.time <- Sys.time()

    for (k in 1:nrow(para_cal)) {
      
      Filename<-para_cal[k,3]
      files<-list.files(pattern=glob2rx(paste("*.",Filename,sep="")))
      ## copy the orginal files to the TxtINOut folder----
      file_copy(files, TxtInOut,overwrite = TRUE)
      #print(paste0("complete backup files","=",k))
    }
    
    setwd(TxtInoutf)
    
    print(paste0("start modifying parameter values of simulation","=",j))
    #k, number of calibated parameters----
    # subset parameter ranges
    param_cal_ranges_mcal<-param_cal_ranges[m:(m+ncal_m-1),]
    
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
 


  