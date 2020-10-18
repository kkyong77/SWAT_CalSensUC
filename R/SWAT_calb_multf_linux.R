
# home directory-----
home<-"C:/Project/Columbia_SWAT/codes/SWAT_CalbR/SWAT_CalbR"
Backup<-paste(home,"Backup",sep="/")
## setwd folder "TxtInOut"----
TxtInOut<-paste(home,"TxtInOut",sep="/")
setwd(TxtInOut)

# default calibration folder----
Calb<-paste(home,"Calb",sep="/")

### parameter calibration  folder----
paramin<-paste(home,"param.in",sep="/")

# reading parameter ranges------
param_file<-"ParamRanges-Sens.txt"
param_ranges<-read.csv(paste(paramin,param_file,sep="/"),header=T,sep="")

para_cal<- data.table::fread(paste(paramin,"ParamFiles_Cals.txt",sep="/"),skip = 0,sep = "auto",
                             header="auto", data.table = F, verbose = F)

## determine which parameter will be calibrated----


# number of calibrated/sensitivity analysis parameters----
unique(para_cal$ParameterName)
length(unique(para_cal$ParameterName))

#C:\Project\Columbia_SWAT\codes\SWAT_CalbR\SWAT_CalbR\TxtInOut

# editing the file.cio file for modifying the simulation periods/other setups
#shell.exec("file.cio")

# if replace the orignal value 1
# otherwise, 2
# number of running

# number of calibration-----
#ncal=500
# generating the parameter ranges----
# number of parameters,tcal----
#tcal=21

# reading calibrated model parameter sets-------------
param_cal_ranges<-read.csv(file=paste(Calb,"param_calb.csv",sep="/"),header=T)

source("../R/SWAT_calb_multf.R")

# reading input files
args<-commandArgs(trailingOnly = TRUE)
args<-as.numeric(args)
swat_calb_mult(args[1],args[2],args[3],args[4])

  