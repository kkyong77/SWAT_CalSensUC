
cd c:/Project/Columbia_SWAT/codes/SWAT_CalbR/SWAT_CalbR/R

 nohup Rscript SWAT_calb_multf_linux.R 1 100 1 21  > R100.out &

 nohup Rscript SWAT_calb_multf_linux.R 2 100 101 21  > R200.out &

 nohup Rscript SWAT_calb_multf_linux.R 3 100 201 21  > R300.out &

 nohup Rscript SWAT_calb_multfv.R 4 100 301 21  > R400.out &

 nohup Rscript SWAT_calb_multf_linux.R 4 100 401 21  > R500.out &
