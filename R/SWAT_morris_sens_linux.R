# reading input files
args<-commandArgs(trailingOnly = TRUE)
args<-as.numeric(args)

R<-paste(home,"R",sep="/")

source(paste(R,"swat_morris_sens",sep="/"))

swat_morris_sens(args[1],args[2])
