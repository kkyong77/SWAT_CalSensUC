library(chron)
library(lubridate)
library(hydroGOF)
library(gtools)


# reading model calibration results---------
# select behavioral parameter sets--------
# sensitivity analysis--------
# uncertainty analysis-----------

# ncalf
# number of calibration folder
ncalf=10
# select the flow variables
for (i in 1:ncalf){
  if(i==1){
  path_cal<-paste(home,paste0("Calb",i),sep="/")
  setwd(path_cal)
  out<-list.files(pattern=glob2rx("watout*"))
  outflow<-do.call(cbind,lapply(out,function(x) data.table::fread(x, skip=5, header="auto",data.table =T, verbose = F,sep="auto",select="FLOWm^3/s")))   
  outflow<-as.matrix(outflow)
  outflow <- matrix(outflow, ncol = ncol(outflow), dimnames = NULL)
  }
  else {
    path_cal<-paste(home,paste0("Calb",i),sep="/")
    setwd(path_cal)
    out<-list.files(pattern=glob2rx("watout*"))
    outflow2<-do.call(cbind,lapply(out,function(x) data.table::fread(x, skip=5, header="auto",data.table =T, verbose = F,sep="auto",select="FLOWm^3/s")))   
    outflow2<-as.matrix(outflow2)
    outflow2 <- matrix(outflow2, ncol = ncol(outflow2), dimnames = NULL)
    outflow<-cbind(outflow,outflow2)
  }
}


# reading observed flow
obsf<-paste(home,"obs",sep="/")
obs<-read.csv(paste(obsf,"SWAT_obs.txt",sep="/"),header=F,sep="")
colnames(obs)<-c("dates","cms")
obs$dates<-NULL
obs$date<-seq.dates(from='1/1/1980',length=nrow(obs))

# calibration------------------

# reading model parameters
statsd= read.table(paste(Calb,"param_calb_5000.csv",sep="/"),header=T,sep=",")

# ncal: number of calibration
ncal=ncol(outflow)
statsd=statsd[1:ncal,]

#### model calibration 
results<-as.data.frame(outflow)
results$date<-seq.dates(from="1/1/1980",length=nrow(results))


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
resultd<-subset(resultd,resultd$wy>=1983)


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

statsd$row = seq(from=1, to=length(statsd$nse))

statsd<-statsd[order(statsd$kge,decreasing=T),]

# sensitivity analysis 
# selecting the behavioral parameter sets
thres=0.5

#NSE
statsd_behavior<-subset(statsd,statsd$kgs>=thres)
max_acc=max(statsd_behavior$nse)
min_acc=min(statsd_behavior$nse)
statsd_behavior$w_acc=(statsd_behavior$nse-min_acc)/(max_acc-min_acc)
sum_acc=sum(statsd_behavior$nse)
statsd_behavior$wt_acc=statsd_behavior$nse/sum_acc

#KGE
statsd_behavior<-subset(statsd,statsd$kge>=thres)
max_acc=max(statsd_behavior$kge)
min_acc=min(statsd_behavior$kge)
statsd_behavior$w_acc=(statsd_behavior$kge-min_acc)/(max_acc-min_acc)
sum_acc=sum(statsd_behavior$kge)
statsd_behavior$wt_acc=statsd_behavior$kge/sum_acc

  
## non-behavioral parameter sets
#ind<-which(with(tmp,nse>=0.3&lognse>=0.3&abs(perr)<=15))
tmp<-statsd
ind<-which(with(tmp,nse>=thres))
statsd_non_behavior=statsd[-ind,]

tmp<-statsd
ind<-which(with(tmp,kge>=thres))
statsd_non_behavior=statsd[-ind,]


## exploring the parameter spaces----------------
par(mfrow=c(1,3))
par(cex.lab=1.1,cex=1.1,cex.axis=1.2)
par(mar=c(4,4,0.5,0.5)+0.5)

boxplot(statsd_behavior$kge,ylab="KGE",names=c("behavioral"),col=c("gray"))

boxplot(statsd_behavior$nse,ylab="NSE",names=c("behavioral"),col=c("gray"))

boxplot(statsd_behavior$lognse,ylab="logNSE",names=c("behavioral"),col=c("gray"))


par(mfrow=c(1,3),bty="l")
par(cex.lab=1.1,cex=1.1,cex.axis=1.2)
par(mar=c(4,4,0.5,0.5)+0.5)

boxplot(statsd$nse,statsd_behavior$nse,ylab="NSE",names=c("all","behavioral"),col=c("gray","red"))

plot(statsd$CN2,statsd$nse,xlab="CN2",ylab="NSE",pch=1,col="gray")
points(statsd_behavior$CN2,statsd_behavior$nse,col="red",pch=19)
legend("bottomright", legend=c("all parameter sets","behavioral"), col=c("gray","red"),pch=c(1,19),cex=0.8,bty="n")

plot(statsd$ALPHA_BF,statsd$nse,xlab="ALPHA_BF",ylab="NSE",pch=1,col="gray")
points(statsd_behavior$ALPHA_BF,statsd_behavior$nse,col="red",pch=19)

plot(statsd$SMTMP,statsd$nse,xlab="SMTMP",ylab="NSE",pch=1,col="gray")
points(statsd_behavior$SMTMP,statsd_behavior$nse,col="red",pch=19)



## parameter sensitivity------------------------- 
# using RSA analysis----------- 

par(mfrow=c(1,3),bty="l")
par(cex.lab=1.1,cex=1.1,cex.axis=1.2)
par(mar=c(4,4,0.5,0.5)+0.5)

min<-min(statsd$CN2)
max<-max(statsd$CN2)

plot(ecdf(statsd_behavior$CN2), xlab="CN2",ylab="Cumulative Proportion",main="(a)",col.p="blue",do.p=FALSE,lty=1,lwd=2,col.hor="red",xlim=c(min,max),bty="l")
plot(ecdf(statsd_non_behavior$CN2),col.p="blue",do.p=FALSE,lty=1,lwd=2,col.hor="blue",add=T,xlim=c(min,max))
legend("topleft", legend=c("behavioral","non-behavioral"), col=c("red","blue"), lty=c(1),cex=0.8,bty="n")

min<-min(statsd$ALPHA_BF)
max<-max(statsd$ALPHA_BF)

plot(ecdf(statsd_behavior$ALPHA_BF), xlab="ALPHA_BF",ylab="Cumulative Proportion",main="(b)",col.p="blue",do.p=FALSE,lty=1,lwd=2,col.hor="red",xlim=c(min,max),bty="l")
plot(ecdf(statsd_non_behavior$ALPHA_BF ),col.p="blue",do.p=FALSE,lty=1,lwd=2,col.hor="blue",add=T,xlim=c(min,max))

min<-min(statsd$SMTMP)
max<-max(statsd$SMTMP)

plot(ecdf(statsd_behavior$SMTMP), xlab="SMTMP",ylab="Cumulative Proportion",main="(c)",col.p="blue",do.p=FALSE,lty=1,lwd=2,col.hor="red",xlim=c(min,max),bty="l")
plot(ecdf(statsd_non_behavior$SMTMP),col.p="blue",do.p=FALSE,lty=1,lwd=2,col.hor="blue",add=T,xlim=c(min,max))



#################################################
labels<-colnames(statsd_behavior)

rsa_plot<-function(Xb,Xnb,i,labels){
minb=min(Xb[,i],Xnb[,i])
maxb=max(Xb[,i],Xnb[,i])

aCDF <- approximatecdfpair(Xb[,i], Xnb[,i])
CDFb <- aCDF$CDF1
CDFnb <- aCDF$CDF2
xx <- aCDF$x

plot(xx, CDFb, lwd = 2, type ="l", xlab = labels[i], ylab = "CDF",xlim=c(minb,maxb))
lines(xx, CDFnb, col = "gray", lwd = 2)	
str_legend=c("behavior","nonbehavior")
legend("bottomright", str_legend, col = c("black", "gray"), lwd = 2,bty="n")

}


rsa_plot_group<-function(Xb,Xnb,i,labels,ngroup){
  minb=min(Xb[,i],Xnb[,i])
  maxb=max(Xb[,i],Xnb[,i])
  
  ngroup=5
  tnrow<-nrow(Xb)
  tindx<-as.integer(tnrow/group)+1

  # Define indices for splitting inputs into ngroup:
  #Y_sort <- sort(Xb)
  ord <-1:nrow(Xb)
  
  # Define indices for splitting inputs into ngroup:
  split <- seq(0, N, by = floor(N / ngroup))
  
  idx <- numeric(N)
  
  for ( i in 1:ngroup){
    idx[ord[(split[i] + 1):split[i + 1]]] <- i
  }
  
   N=nrow(Xb)
  split <- seq(0, N, by = floor(N / ngroup))
  
  idx <- numeric(N)
  
  for ( i in 1:ngroup){
    idx[ord[(split[i] + 1):split[i + 1]]] <- i
  }
  
  
  Xb$idx<-idx
  
 
  xxi <- unique(sort(Xb[, i]))
  
  CDFj <- sapply(1:ngroup, function(h)  ecdf(Xb[idx == h, 1]))
  
  
  matplot(1:25, CDFj, type ="l", xlab = labels[i], ylab = "cdf", col = rev(rainbow(Ng + 1, end = 5 / 6)))
  
  legend('bottomright', legend = round(Yk, 2), lwd = 2, col = rev(rainbow(Ng + 1, end = 5 / 6)))
  
  
  aCDF <- approximatecdfpair(Xb[,i], Xnb[,i])
  CDFb <- aCDF$CDF1
  CDFnb <- aCDF$CDF2
  xx <- aCDF$x
  
  plot(xx, CDFb, lwd = 2, type ="l", xlab = labels[i], ylab = "CDF",xlim=c(minb,maxb))
  lines(xx, CDFnb, col = "gray", lwd = 2)	
  str_legend=c("behavior","nonbehavior")
  legend("bottomright", str_legend, col = c("black", "gray"), lwd = 2,bty="n")
  
}


tcal<-21
par(mfrow=c(2,11))
par(mar=c(2,2,2,2)+0.5)

for (i in 1:tcal){
    rsa_plot(statsd_behavior,statsd_non_behavior,i,labels)
  }

#######################################################
# calculate the "KolmogoroveSmirnov-statistic""-------------

rsa_ks<-function(x,y,col,header){
  
  tmp<-ks.test(x[,col],y[,col])
  tmp2=cbind(tmp$p.value,tmp$statistic)
  row.names(tmp2)<-header[col]
  colnames(tmp2)<-c("p.value","D")
  return(tmp2)
}

for (i in 1:tcal){
  if(i==1){
  tmp<-rsa_ks(statsd_behavior,statsd_non_behavior,i,header)
  }
  else{
    tmp2<-rsa_ks(statsd_behavior,statsd_non_behavior,i,header)
    tmp<-rbind(tmp,tmp2)
    }
  }



### model prediction uncertainty------------------

#streamflow
#reading behavioral parameter sets
#2:length(statsd.bb30m_snow_dy_rp_vald_behavior$m)-2
library(Hmisc)

#x:resultd
#y:statsd_behavior
#date:the first date of the simulation results
Uncertainty_plot<-function(x,y,fdate,conf1,conf2){
  
z=apply(x[,y$row+1],1,"wtd.quantile",weights=y$wt_acc,prob=c(conf1,conf2),normwt=T)
z=as.data.frame(t(z))
colnames(z)=c("low","high")

z2=as.data.frame(apply(x[,y$row+1],1,"wtd.mean",weights=y$wt_acc))
z$mean<-z2

z$date=seq.dates(from=fdate,length=nrow(z))

dates=z$date

par(mfrow=c(1,2))
par(mar=c(4,4,1,0.5)+0.5)

plot(z$date,z$mean,type="l",xlab="Time(days)",ylab="Streamflow(m3/s)",main="", ylim=c(0,max(glue_model$high)))
polygon(c(dates,rev(dates)),c(z$low,rev(z$high)),col="gray",border=NA)
lines(z$date,z$cms ,col="red",lwd=1)
lines(z$date,z$mean,lwd=2,lty=2)
legend("topright", legend=c("95% confidence limit","observation","weighted mean"), col=c("gray","red","black"), lty=c(1,1,2), lwd=c(6,2,2),cex=0.8,bty="n")


plot(z$date,z$mean,type="l",xlab="Time(days)",ylab="Streamflow(m3/s)",main="", ylim=c(min(log(glue_model$min)),max(log(glue_model$high)),log="y")
polygon(c(x1,rev(x1)),c(z$low,rev(z$high)),col="gray",border=NA)
lines(z$date,z$cms ,col="red",lwd=1)
lines(z$date,z$mean,lwd=2,lty=2)
#legend("topright", legend=c("95% confidence limit","observation","weighted mean"), col=c("gray","red","black"), lty=c(1,1,2), lwd=c(6,2,2),cex=0.8,bty="n")



}



glue_model=apply(resultd[,statsd_behavior$row+1],1,"wtd.quantile",weights=statsd_behavior$wt_acc,prob=c(0.025,0.975),normwt=T)
glue_model=as.data.frame(t(glue_model))
colnames(glue_model)=c("low","high")

mean_flow_model=as.data.frame(apply(resultd[,statsd_behavior$row+1],1,"wtd.mean",weights=statsd_behavior$wt_acc))
colnames(mean_flow_model)=c("mean")
mean_flow_model$date=seq.dates(from="10/1/1983",length=length(mean_flow_model$mean))

mean_flow_model<-wy(mean_flow_model)


glue_model$date=seq.dates(from="10/1/1983",length=length(glue_model$low))

glue_model$date<-NULL
glue_model$mean<-mean_flow_model$mean

x=mean_flow_model$date

par(mfrow=c(1,2))
par(mar=c(4,4,1,0.5)+0.5)

plot(mean_flow_model$date,mean_flow_model$mean,type="l",xlab="Time(days)",ylab="Streamflow(m3/s)",main="", ylim=c(0,max(glue_model$high)))
polygon(c(x,rev(x)),c(glue_model$low,rev(glue_model$high)),col="gray",border=NA)
lines(mean_flow_model$date,resultd$cms ,col="red",lwd=1)
lines(mean_flow_model$date,mean_flow_model$mean,lwd=2,lty=2)
legend("topright", legend=c("95% confidence limit","observation","weighted mean"), col=c("gray","red","black"), lty=c(1,1,2), lwd=c(6,2,2),cex=0.8,bty="n")


plot(mean_flow_model$date,mean_flow_model$mean,type="l",xlab="Time(days)",ylab="Streamflow(m3/s)",main="", log="y", ylim=c(0.5,30))
polygon(c(x,rev(x)),c(glue_model$low,rev(glue_model$high)),col="gray",border=NA)
lines(mean_flow_model$date,resultd$cms ,col="red",lwd=1)
lines(mean_flow_model$date,mean_flow_model$mean,lwd=2,lty=2)
#legend("topright", legend=c("95% confidence limit","observation","weighted mean"), col=c("gray","red","black"), lty=c(1,1,2), lwd=c(6,2,2),cex=0.8,bty="n")



