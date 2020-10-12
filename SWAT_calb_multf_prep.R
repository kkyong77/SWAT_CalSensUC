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
Backup<-paste(home,"Backup",sep="/")
setwd(Backup)

Backup_params<-paste(home,"Backup_params",sep="/")
setwd(Backup_params)

# list of backup folder files
backup_files<-list.files(pattern=glob2rx("*"))

## copy the orginal files to the TxtINOut folder----
TxtInOut<-paste(home,"TxtInOut",sep="/")
file.copy(backup_files, TxtInOut,overwrite = TRUE)

# make directory-----------------------------
#number of directory and copy the directory 
n=5
for (i in 1:n){
  path<-paste(home,paste0("TxtInOut",i),sep="/")
  path_cal<-paste(home,paste0("Calb",i),sep="/")
  
  dir.create(path) 
  dir.create(path_cal)
  dir_copy(TxtInOut,path, overwrite = TRUE)
}


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
