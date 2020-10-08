library(chron)
library(lubridate)
library(hydroGOF)
library(gtools)


# reading model calibration results---------
# select behavioral parameter sets--------
# sensitivity analysis--------
# uncertainty analysis-----------

setwd(Calb)

out<-list.files(pattern=glob2rx("watout*"))
outflow<-do.call(cbind,lapply(out,function(x) data.table::fread(x, skip=5, header="auto",data.table =T, verbose = F,sep="auto",select="FLOWm^3/s")))   
outflow<-as.matrix(outflow)
outflow <- matrix(outflow, ncol = ncol(outflow), dimnames = NULL)

# reading observed flow
obs<-read.csv("SWAT_obs.txt",header=F,sep="")
colnames(obs)<-c("dates","cms")
obs$dates<-NULL
obs$date<-seq.dates(from='1/1/1980',length=nrow(obs))

#obs<-wy(obs)

# calibration------------------

# reading model parameters
statsd= read.table("param_calb.csv",header=T,sep=",")

# ncal: number of calibration
statsd=statsd[1:ncal,]

#### model calibration 
results<-as.data.frame(outflow)

results$date<-seq.dates(from="1/1/1980",length=nrow(results))
#results<-wy(results)

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
resultd<-subset(resultd,resultd$wy>=1984)


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


# sensitivity analysis 
# selecting the behavioral parameter sets
statsd_behavior<-subset(statsd,statsd$nse>=0.1)
max_acc=max(statsd_behavior$nse)
min_acc=min(statsd_behavior$nse)
statsd_behavior$w_acc=(statsd_behavior$nse-min_acc)/(max_acc-min_acc)
sum_acc=sum(statsd_behavior$nse)
statsd_behavior$wt_acc=statsd_behavior$nse/sum_acc
  
## non-behavioral parameter sets
#ind<-which(with(tmp,nse>=0.3&lognse>=0.3&abs(perr)<=15))
tmp<-statsd
ind<-which(with(tmp,nse>=0.1))
statsd_non_behavior=statsd[-ind,]

## exploring the parameter spaces----------------

par(mfrow=c(2,2),bty="l")
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
# using GSA analysis 

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



### model prediction uncertainty------------------

#streamflow
#reading behavioral parameter sets
#2:length(statsd.bb30m_snow_dy_rp_vald_behavior$m)-2
library(Hmisc)

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

