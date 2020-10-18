
## reading morris sensitivity simulation results

ncal_sens<-nrow(design)

# reading model outputs
setwd(morris_sens)

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

flow_accuracy_morris_sens(outflow,obs,1984,ncal_sens)


#design is matrix of design points with 24 rows and 5 columns
#obtain vector of output y from running the model.
# y is model output
# irrigqation area accuracy or mean irrigation area
# reformat the generated parameters for ABM-Riverware models
##
y<-matrix(statsd$nse)

tell(Morris, y)

mu <- apply(Morris$ee, 2, Morris)))
mu.star <- apply(Morris$ee, 2, function(Morris) mean(abs(Morris)))
sigma <- apply(Morris$ee, 2, sd)


