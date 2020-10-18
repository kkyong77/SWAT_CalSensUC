# sobol sensivity analysis-------
library(pracma)
library(sensitivity)

#To perform GSA with the method of Sobol two random sets of samples with the same sample size for the parameters that should be analyzed are required.
#To perform the sensitivity analysis with using method of Sobol the following command has to be executed. In total 11000 model evaluations are necessary to analyze 21 parameters with 500 Sobol samples.

min_par<-c(-0.2,param_ranges[2,3],param_ranges[3,3],param_ranges[4,3],param_ranges[5,3],param_ranges[6,3],param_ranges[7,3],param_ranges[8,3],param_ranges[9,3],-0.2,-0.2,param_ranges[12,3],param_ranges[13,3],param_ranges[14,3],param_ranges[15,3],param_ranges[16,3],param_ranges[17,3],param_ranges[18,3],param_ranges[19,3],param_ranges[20,3],param_ranges[21,3])
max_par<-c(0.2,param_ranges[2,4],param_ranges[3,4],param_ranges[4,4],param_ranges[5,4],param_ranges[6,4],param_ranges[7,4],param_ranges[8,4],param_ranges[9,4],0.2,0.2,param_ranges[12,4],param_ranges[13,4],param_ranges[14,4],param_ranges[15,4],param_ranges[16,4],param_ranges[17,4],param_ranges[18,4],param_ranges[19,4],param_ranges[20,4],param_ranges[21,4])


tcal=21
ncal=500

X1<-matrix(0,ncol=tcal,nrow=ncal)
X2<-matrix(0,ncol=tcal,nrow=ncal)

####X1-------
X1[,1]=runif(ncal,min=-0.2,max=0.2)
X1[,2]=runif(ncal,min=param_ranges[2,3],max=param_ranges[2,4])
X1[,3]=runif(ncal,min=param_ranges[3,3],max=param_ranges[3,4])
X1[,4]=runif(ncal,min=param_ranges[4,3],max=param_ranges[4,4])
X1[,5]=runif(ncal,min=param_ranges[5,3],max=param_ranges[5,4])
X1[,6]=runif(ncal,min=param_ranges[6,3],max=param_ranges[6,4])
X1[,7]=runif(ncal,min=param_ranges[7,3],max=param_ranges[7,4])
X1[,8]=runif(ncal,min=param_ranges[8,3],max=param_ranges[8,4])
X1[,9]=runif(ncal,min=param_ranges[9,3],max=param_ranges[9,4])

#SOL_K (relative)----
X1[,10]=runif(ncal,min=-0.2,max=0.2)
#SOL_AWC (relative)---
X1[,11]=runif(ncal,min=-0.2,max=0.2)

X1[,12]=runif(ncal,min=param_ranges[12,3],max=param_ranges[12,4])
X1[,13]=runif(ncal,min=param_ranges[13,3],max=param_ranges[13,4])
#OV_N (relative)-----
X1[,14]=runif(ncal,min=param_ranges[14,3],max=param_ranges[14,4])
X1[,15]=runif(ncal,min=param_ranges[15,3],max=param_ranges[15,4])
X1[,16]=runif(ncal,min=param_ranges[16,3],max=param_ranges[16,4])
X1[,17]=runif(ncal,min=param_ranges[17,3],max=param_ranges[17,4])
X1[,18]=runif(ncal,min=param_ranges[18,3],max=param_ranges[18,4])
X1[,19]=runif(ncal,min=param_ranges[19,3],max=param_ranges[19,4])
X1[,20]=runif(ncal,min=param_ranges[20,3],max=param_ranges[20,4])
X1[,21]=runif(ncal,min=param_ranges[21,3],max=param_ranges[21,4])


### X2----
X2[,1]=runif(ncal,min=-0.2,max=0.2)
X2[,2]=runif(ncal,min=param_ranges[2,3],max=param_ranges[2,4])
X2[,3]=runif(ncal,min=param_ranges[3,3],max=param_ranges[3,4])
X2[,4]=runif(ncal,min=param_ranges[4,3],max=param_ranges[4,4])
X2[,5]=runif(ncal,min=param_ranges[5,3],max=param_ranges[5,4])
X2[,6]=runif(ncal,min=param_ranges[6,3],max=param_ranges[6,4])
X2[,7]=runif(ncal,min=param_ranges[7,3],max=param_ranges[7,4])
X2[,8]=runif(ncal,min=param_ranges[8,3],max=param_ranges[8,4])
X2[,9]=runif(ncal,min=param_ranges[9,3],max=param_ranges[9,4])

#SOL_K (relative)----
X2[,10]=runif(ncal,min=-0.2,max=0.2)
#SOL_AWC (relative)---
X2[,11]=runif(ncal,min=-0.2,max=0.2)

X2[,12]=runif(ncal,min=param_ranges[12,3],max=param_ranges[12,4])
X2[,13]=runif(ncal,min=param_ranges[13,3],max=param_ranges[13,4])
#OV_N (relative)-----
X2[,14]=runif(ncal,min=param_ranges[14,3],max=param_ranges[14,4])
X2[,15]=runif(ncal,min=param_ranges[15,3],max=param_ranges[15,4])
X2[,16]=runif(ncal,min=param_ranges[16,3],max=param_ranges[16,4])
X2[,17]=runif(ncal,min=param_ranges[17,3],max=param_ranges[17,4])
X2[,18]=runif(ncal,min=param_ranges[18,3],max=param_ranges[18,4])
X2[,19]=runif(ncal,min=param_ranges[19,3],max=param_ranges[19,4])
X2[,20]=runif(ncal,min=param_ranges[20,3],max=param_ranges[20,4])
X2[,21]=runif(ncal,min=param_ranges[21,3],max=param_ranges[21,4])

###############################################################################
sens_sobol <- sobol2002(model =NULL, X1 = X1, X2 = X2, nboot = 100)


sobol_param<-sens_sobol$X


## saving the generated parameter values-----
header<-c(para_cal$ParameterName)
colnames(sobol_param)<-header
write.csv(sobol_param,file=paste0(path_sobol_sens,"param_sobol.csv"),row.names = FALSE)

###############################################################################


