
################################################################################
#                             SWAT_calb.R                                 #
################################################################################
# Purpose    : To write R scripts to calibrate the model parameters 
#             with a simple monte carlos simulation                                                     
################################################################################
# Output     : watershed outlet predictions                             #
################################################################################
# Author : Kyongho Son                                      #
# Started: 10/1/2020                                         #
# Updates: 10/4/2020                                                        #
################################################################################


# preparing files
# sensitivity inputs files based on the SWAT setup
# reading files
## copy files
#https://fs.r-lib.org/reference/copy.html


## default setting
#C:\Project\Columbia_SWAT\codes\SWAT_CalbR\SWAT_CalbR\Backup
## copy orginal files to TxtInOut-----

# home directory-----
home<-"C:/Project/Columbia_SWAT/codes/SWAT_CalbR/SWAT_CalbR"

# backup directory-----
Backup<-paste(home,"Backup",sep="/")
setwd(Backup)

# list of backup folder files
backup_files<-list.files(pattern=glob2rx("*"))

## copy the orginal files to the TxtINOut folder----
TxtInOut<-paste(home,"TxtInOut",sep="/")
file.copy(backup_files, TxtInOut,overwrite = TRUE)


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

# calibration folder----
Calb<-paste(home,"Calb",sep="/")

## setwd folder "TxtInOut"----
TxtInOut<-paste(home,"TxtInOut",sep="/")
setwd(TxtInOut)

# editing the file.cio file for modifying the simulation periods/other setups
shell.exec("file.cio")

# if replace the orignal value 1
# otherwise, 2
# number of running

  # number of calibration-----
  ncal=500
  # generating the parameter ranges----
  # number of parameters,tcal----
  tcal=21
  param_cal_ranges<-matrix(0,ncol=tcal+1,nrow=ncal)
  
  # generating the parameter values with random generator-----
  # CN2 (relative)----
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
 
  # loop for the number of calibartion----
  for (j in 20:ncal){
    ## modifying the parameters----
    print(paste0("start modifying parameter values of simulation","=",j))
    #k, number of calibated parameters----
    for (k in 1:nrow(para_cal)) {
      
      ## modifying the initial range of parameter values----
      Filename<-para_cal[k,3]
      files<-list.files(pattern=glob2rx(paste("*.",Filename,sep="")))
      id<-as.numeric(para_cal[para_cal$ParameterNmbr==k,1])
      newvalue<-as.numeric(param_cal_ranges[j,k])
      row<-as.numeric(para_cal[para_cal$ParameterNmbr==k,4][1])
      col.ini<-as.numeric(para_cal[para_cal$ParameterNmbr==k,5])
      col.fin<-as.numeric(para_cal[para_cal$ParameterNmbr==k,6])
      dec<-as.numeric(para_cal[para_cal$ParameterNmbr==k,7])
      repl<-as.numeric(para_cal[para_cal$ParameterNmbr==k,8])
      #############################################################
       # start-modifying file---- 
       for (i in 1:length(files)){
          #### modifying the parameter values----
          ModifyInputFile(id,newvalue,files[i],row,col.ini,col.fin,dec,repl)
          }
      #########################################################
      print(paste0("complete modifying parameter values","=",k))
      } # end -modifying files---
      ###################################################################
    
      # running swat simulations with modified parameter sets----
      system("swat.exe") # or another version you desire
      print(paste0("number of simulations","=",j))
      file.remove("output.hru")
      
      #copy the model outputs to calb folder-----------
      watout_files<-list.files(pattern=glob2rx("watout*"))
      file.copy(watout_files,Calb,overwrite = TRUE)
      file.rename("../Calb/watout.dat", paste("../Calb",paste0(watout_files,"_",j),sep='/'))
      ####################################################################################
        # reset the parameter files-------------
        setwd(Backup)
       print(paste0("start backup files"))
        for (k in 1:nrow(para_cal)) {
          Filename<-para_cal[k,3]
          files<-list.files(pattern=glob2rx(paste("*.",Filename,sep="")))
          ## copy the orginal files to the TxtINOut folder----
          file.copy(files, TxtInOut,overwrite = TRUE)
          print(paste0("complete backup files","=",k))
        }
       
       # copy directory
       # copyDirectory
       
        ########################################################################
      # chaning directory to "TXtinOUT----------------------------
      setwd(TxtInOut)
      
  }

  
 ###############################################################################
  
  
  
