require(sensitivity)
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


Morris1 <- morris(model = NULL, factors=21, r=20,
                  design = list(type = "oat", levels =10, grid.jump = 5),
                  binf = min_par,
                  bsup = max_par,
                  scale = TRUE)



design1<-Morris1$X


## saving the generated parameter values-----
header<-c(para_cal$ParameterName)
colnames(design1)<-header
write.csv(design1,file=paste0(path_sens,"param_morris.csv"),row.names = FALSE)
###############################################################################

ncal_sens<-nrow(design1)


# reading model outputs

setwd(path_sens_in)
out<-list.files(pattern=glob2rx("watout*"))
outflow<-do.call(cbind,lapply(out,function(x) data.table::fread(x, skip=5, header="auto",data.table =T, verbose = F,sep="auto",select="FLOWm^3/s")))   
outflow<-as.matrix(outflow)
outflow <- matrix(outflow, ncol = ncol(outflow), dimnames = NULL)
outflow<-as.data.frame(outflow)
outflow$date<-seq.dates(from="1/1/1980",length=nrow(outflow))


# reading observed flow
obsf<-paste(home,"obs",sep="/")
obs<-read.csv(paste(obsf,"SWAT_obs.txt",sep="/"),header=F,sep="")
colnames(obs)<-c("dates","cms")
obs$dates<-NULL
obs$date<-seq.dates(from='1/1/1980',length=nrow(obs))



flow_accuracy_morris_sens<-function(df,obs,start_wy,ncal_sens){
  
  # reading parameter sets for morris sensitivity analysis
  statsd= read.table(paste(path_sens,"param_morris.csv",sep="/"),header=T,sep=",")
  
  results<-df

  # streamflow
  ncal =ncol(results)-1
  lc = ncol(results)
  lr = nrow(results)
  tmp = results[,1:(lc)]  
  tmp2 = as.data.frame(tmp[1:(lr),])
  tmp2$date = results[1:(lr),"date"]
  resultd = tmp2
  
  lr = nrow(obs)
  tmp = obs$cms
  tmp2 = as.data.frame(tmp[1:(lr)])
  colnames(tmp2) = c("cms")
  obsd = tmp2
  obsd$date = obs$date[1:(lr)]
  tmp = merge(resultd, obsd, by=c("date"))
  resultd = tmp
  
  #########################################
  resultd<-wy(resultd)
  
  # subset simulation results for warming periods
  resultd<-subset(resultd,resultd$wy>=start_wy)
  
  tmp = apply(resultd[,2:(ncal+1)],2,nse, resultd$cms)
  statsd$nse = tmp
  
  tmp = apply(resultd[,2:(ncal+1)],2,lognse, resultd$cms)
  statsd$lognse = tmp
  
  tmp = apply(resultd[,2:(ncal+1)],2,mper.err,resultd$cms)
  statsd$perr = tmp
  
  tmp = apply(resultd[,2:(ncal+1)],2,cor,resultd$cms)
  statsd$r = tmp
  
  # select the behavioral parameter sets
  # using KGE efficieny to select the behavioral parameter sets 
  for (i in 1:ncal){
    tmp = KGE(resultd[,i+1],resultd$cms)
    statsd$kge[i] = tmp
    
  }
  
  return(statsd)
  
}


#design is matrix of design points with 24 rows and 5 columns
#obtain vector of output y from running the model.
# y is model output
# irrigqation area accuracy or mean irrigation area
# reformat the generated parameters for ABM-Riverware models
##
y<-matrix(statsd$nse)

tell(Morris1, y)

mu <- apply(Morris1$ee, 2, function(Morris) mean(abs(Morris)))
mu.star <- apply(Morris1$ee, 2, function(Morris) mean(abs(Morris)))
sigma <- apply(Morris$ee, 2, sd)



