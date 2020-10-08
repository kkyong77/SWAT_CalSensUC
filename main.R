
# this script is designed for calibration/sensitivity/uncertainty analysis in SWAT modeling

# open SWAT_Cal.R
# modifying the folder number and file.cio file 
# basic R functions
source("Rfunction.R")
# make sure that the modified parameter values should be witin the phyiscal ranges
source("checking_param_ranges")

# modifying parameter R scripts
source("ModifyInputFile.R")

# generating the calibrated model parameter sets
# and running SWAT simulations

source("SWAT_CalbR.R")

# model calibration/sensitivity analysis/uncertainty analysis
source("calb_sense_uncertainty.R")
