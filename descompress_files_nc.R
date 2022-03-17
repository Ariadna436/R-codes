rm(list = ls(all = TRUE)) #clear all;
graphics.off() # close all;
gc() # Clear memmory (residuals of operations?, cache? Not sure)
################################################################
# Libraries:
require(ncdf4)
require(base)
require(R.utils)

dir_path_adt <- "F:/My_R_hp/My_R/variables_global/AVISO_altimetry/global_msla_dt_gridded_025_deg/2015/"

date_vec<-as.Date(seq(as.Date('09/11/2015', format='%d/%m/%Y'), as.Date('14/03/2016', format='%d/%m/%Y'), 'days'))

date_vec <- gsub("-","",date_vec)

date_vec<- ('20150924')
## List of .nc files
for (i in seq(1,length(date_vec))) {
ind_nc_file <- list.files(dir_path_adt,recursive=T, pattern=paste(date_vec[i]))

filename <-paste(dir_path_adt,
                 ind_nc_file,
                 sep="")


nc_file_c <- gzip(filename, remove = F, overwrite=F)
}
