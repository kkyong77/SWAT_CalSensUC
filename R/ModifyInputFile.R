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

ModifyInputFile <- function(
                            ParamID,    # character, with the ID of the parameter to be modified (only used for visualization purposes)
                            newvalue,   # numeric value to be written into the text file
                            filename,   # character, with the name of the text file that will be modified
                            row,        # numeric, with the row number in \code{filename} where \code{newvalue} will be written
                            col.ini,    # numeric, with the starting column number in \code{filename} where \code{newvalue} is going to be written.
                            col.fin,    # numeric, with the ending column number in \code{filename} where \code{newvalue} is going to be written.
                            decimals,   # numeric, with the number of decimal places used to write \code{newvalue} into \code{filename}
                            repl,      # replace or multipyling 
                            verbose=TRUE) {

  if (!file.exists(filename))
    stop( paste("Invalid argument: the file '", filename, "' doesn't exist!", sep="") )

  lines  <- readLines(filename)

  myline <- lines[row]
  
  # saving original values for modifying 
  oldvalue<-as.numeric(substr(myline, col.ini, col.fin))
  #oldvalues<-as.numeric(substr(myline, 4, 16))
  
  L.trg <- col.fin - col.ini + 1
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
  
  
  ## testing the range of the model parameters 
   
  
#  message("ParamID  : ", ParamID)
#  message("filename : ", basename(filename))
#  message("row      : ", row)
#  message("new value: ", newvalue.stg)
#  message("L.trg    : ", L.trg)

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

 # if (verbose){
#   message( paste("[", ParamID, ": '", round(newvalue2,5), "' was successfully put into '", basename(filename), "']", sep="") )
#  }
} # 'ModifyInputFile' END

