# checking the parameter ranges after modifying the initial parameter values 

check_param_ranges<-function(ParamID,    # character, with the ID of the parameter to be modified (only used for visualization purposes)
                              newvalue){
  
  
  #param_ranges
  #ParameterNmbr=1
  # minimum/maximum values for each parameter
  min<-param_ranges[ParamID,3]
  max<-param_ranges[ParamID,4]
  
 if(newvalue>max){
   newvalue=max
 }
  else if(newvalue<min){
    newvalue=min
  }
  else{
    newvalue=newvalue
  }

  return(newvalue)
}