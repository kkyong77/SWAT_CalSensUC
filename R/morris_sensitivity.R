
#https://rdrr.io/cran/sensitivity/src/R/morris.R

################################################################################
#                             morris_sensitivity.R                                 #
################################################################################
# Purpose    : To write R scripts to design the morris sensitivity                      
################################################################################
# Output     : watershed outlet predictions                             #
################################################################################
# Author : Kyongho Son                                      #
# Started: 10/15/2020                                         #
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
library(sensitivity)
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

#1#################################################################
# create directories for morris sensitvity results and TxtInOut-----------------------------
#number of directory and copy the directory 

  path_sens_in<-paste(home,"TxtInOut_morris",sep="/")
  path_sens<-paste(home,"morris_sens",sep="/")
  
  dir.create(path_sens_in) 
  dir.create(path_sens)
  dir_copy(TxtInOut,path_sens_in, overwrite = TRUE)


##1)Create morris test design############################################################################# 
#factors is the number of varying parameters (21) in the model
#binf is a vector of lower bounds for input parameters
#bsuf is a vector of upper bounds for input parameters
#r, levels and grid.jump are morris test options


min_par<-c(-0.2,param_ranges[2,3],param_ranges[3,3],param_ranges[4,3],param_ranges[5,3],param_ranges[6,3],param_ranges[7,3],param_ranges[8,3],param_ranges[9,3],-0.2,-0.2,param_ranges[12,3],param_ranges[13,3],param_ranges[14,3],param_ranges[15,3],param_ranges[16,3],param_ranges[17,3],param_ranges[18,3],param_ranges[19,3],param_ranges[20,3],param_ranges[21,3])
max_par<-c(0.2,param_ranges[2,4],param_ranges[3,4],param_ranges[4,4],param_ranges[5,4],param_ranges[6,4],param_ranges[7,4],param_ranges[8,4],param_ranges[9,4],0.2,0.2,param_ranges[12,4],param_ranges[13,4],param_ranges[14,4],param_ranges[15,4],param_ranges[16,4],param_ranges[17,4],param_ranges[18,4],param_ranges[19,4],param_ranges[20,4],param_ranges[21,4])


# Morris <- morris(model = NULL, factors=21,c(10,50),
#                   design = list(type = "oat", levels =10, grid.jump = 5),
#                   binf = min_par,
#                   bsup = max_par,
#                   scale = TRUE)



design<-Morris$X


## saving the generated parameter values-----
header<-c(para_cal$ParameterName)
colnames(design)<-header
write.csv(design,file=paste0(path_sens,"param_morris.csv"),row.names = FALSE)
###############################################################################

# reading morris input files
morris420<-readRDS(paste0(path_sens,"param_morris."),)






