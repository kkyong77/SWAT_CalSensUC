# Part of the hydroPSO package, http://www.rforge.net/hydroPSO/
# Copyright 2010-2014 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################
#                             ModifyInputfile                                  #
################################################################################
# Purpose    : To write a numeric value into a specified position of a plain   #
#              text file                                                       #
################################################################################
# Output     : A mofified text file ('filename')                               #
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
# Started: 17-Dec-2010 at JRC Ispra                                            #
# Updates: 20-Jan-2011                                                         #
#          06-Sep-2013                                                         #
#          09-Abr-2014                                                         #
## updates 14-October-2020
# Kyongho Son
# include the relative options for modifying the initial parameter values
# also include the check the range of the modified parameter values 
# if the parameter values is larger/smaller than the physical boundary,it resets as the physical range
################################################################################

ModifyInputFile_mult <- function(
                            ParamID,    # character, with the ID of the parameter to be modified (only used for visualization purposes)
                            newvalue,   # numeric value to be written into the text file
                            filename,   # character, with the name of the text file that will be modified
                            row,        # numeric, with the row number in \code{filename} where \code{newvalue} will be written
                            col.ini,    # numeric, with the starting column number in \code{filename} where \code{newvalue} is going to be written.
                            col.fin,    # numeric, with the ending column number in \code{filename} where \code{newvalue} is going to be written.
                            decimals,   # numeric, with the number of decimal places used to write \code{newvalue} into \code{filename}
                            repl,      # replace or multipyling 
                            mult,      # if the layer is more than 1 (e.g. soil properties), 1 or 0
                           verbose=TRUE) {

  if (!file.exists(filename))
    stop( paste("Invalid argument: the file '", filename, "' doesn't exist!", sep="") )

  # reading file
  lines  <- readLines(filename)
  # reading row 
  myline <- lines[row]
  
  # checking multiple layers----------------
  if(mult==1){   # more than 1 layer
    
    # define number of layers 
      nlayer<-as.integer((nchar(myline)-col.ini+1)/(col.fin-col.ini+1))
      oldvalue=matrix(0,ncol=1,nrow=nlayer)
  
  # saving original values for modifying 
    for (i in 1:nlayer){
    # length of values 
      L.trg <- col.fin - col.ini + 1
      # saving origal values per layer
      oldvalue[i]<-as.numeric(substr(myline,col.ini+L.trg*(i-1),col.fin+L.trg*(i-1)))
    
  
  # replacing the values with new value
  if(repl==1){ 
    newvalue2<-newvalue
    ## checking the modified parameter ranges
    newvalue2<-check_param_ranges(ParamID,newvalue2)
    
    newvalue.stg <- as.character(round(newvalue2, decimals))
    
  }
  # multiplying the old values with new values
  else if (repl==2){
    newvalue2<-(oldvalue[i]+newvalue*oldvalue[i])
    ## checking the modified parameter ranges
    newvalue2<-check_param_ranges(ParamID,newvalue2)
    
    newvalue.stg <- as.character(round(newvalue2, decimals))
  } # eleIF end
  
  # addding the old values with new values
  else {
    
    newvalue2<-(newvalue+oldvalue[i])
    ## checking the modified parameter ranges
    newvalue2<-check_param_ranges(ParamID,newvalue2)
    
    newvalue.stg <- as.character(round(newvalue2, decimals))
  }
  
  L <- nchar(newvalue.stg)
  
  
  if (L < L.trg) newvalue.stg <- format(newvalue2, justify="right", width=L.trg, nsmall=decimals)  
  
  if (L > L.trg) {
    #newvalue.stg <- format(newvalue, justify="right", width=L.trg, scientific=TRUE)
    #e.pos <- which(strsplit(newvalue.stg, split=character(0))[[1]] == "e")
    #newvalue.stg <- format(newvalue, justify="right", width=L.trg, scientific=TRUE, digits=e.pos-1)
    nexp <- 2
    if (abs(newvalue2) >= 1E100) nexp <- 3
    dig          <- max(decimals-(L - L.trg)-3-nexp, 0) 
    suppressWarnings(
      newvalue.stg <- formatC(newvalue2, width=L.trg, format="E", digits=dig)
    )
  } # IF end 
  
  
  # replace 
  substr(myline, col.ini+L.trg*(i-1), col.fin+L.trg*(i-1)) <- newvalue.stg
  
  # repalce old value by a new value
  lines[row] <- myline
  
  writeLines(lines, filename)
  
    } # end for loops 
  } # end if layer
  

  ############# if layer is only 1################
  
  else {
    
    oldvalue<-as.numeric(substr(myline,col.ini,col.fin))


  
  # replacing the values with new value
  if(repl==1){ 
    newvalue2<-newvalue
    ## checking the modified parameter ranges
    newvalue2<-check_param_ranges(ParamID,newvalue2)
    
    newvalue.stg <- as.character(round(newvalue2, decimals))
    
    }
  # multiplying the old values with new values
  else if (repl==2){
    newvalue2<-(oldvalue+newvalue*oldvalue)
    ## checking the modified parameter ranges
    newvalue2<-check_param_ranges(ParamID,newvalue2)
    
    newvalue.stg <- as.character(round(newvalue2, decimals))
    } # eleIF end
   
  # addding the old values with new values
  else {
    
    newvalue2<-(newvalue+oldvalue)
    ## checking the modified parameter ranges
    newvalue2<-check_param_ranges(ParamID,newvalue2)
    
    newvalue.stg <- as.character(round(newvalue2, decimals))
  }
  
  L <- nchar(newvalue.stg)
  

  if (L < L.trg) newvalue.stg <- format(newvalue2, justify="right", width=L.trg, nsmall=decimals)  
  
    if (L > L.trg) {
     #newvalue.stg <- format(newvalue, justify="right", width=L.trg, scientific=TRUE)
     #e.pos <- which(strsplit(newvalue.stg, split=character(0))[[1]] == "e")
     #newvalue.stg <- format(newvalue, justify="right", width=L.trg, scientific=TRUE, digits=e.pos-1)
     nexp <- 2
     if (abs(newvalue2) >= 1E100) nexp <- 3
     dig          <- max(decimals-(L - L.trg)-3-nexp, 0) 
     suppressWarnings(
       newvalue.stg <- formatC(newvalue2, width=L.trg, format="E", digits=dig)
       )
    } # IF end 
  

  substr(myline, col.ini, col.fin) <- newvalue.stg
  
  lines[row] <- myline
  
  writeLines(lines, filename)

  } # end if for layer conditions  

} # 'ModifyInputFile' END

