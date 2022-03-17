rm(list = ls(all = TRUE)) #clear all;
graphics.off() # close all;
gc() # Clear memmory (residuals of operations?, cache? Not sure)
################################################################
# Libraries:
require(R.utils) # To uncompress the gz files
require(plyr) # round coordinates at a desired resolution
require(stringr) # for manipulating strings
require(ncdf4)
gpclibPermit()
require(SDMTools)
require(maptools)
require(utils)
#### Open Polygon, select files by location ####

polygon <- data.frame(getKMLcoordinates                   
                      ("C:/Users/Macroeco/Documents/My_R/Apt_pop_grow/dist_apt_92-93.kml",   #File example polygon               
                        ignoreAltitude=TRUE))  

####create a vector with date####
date_LFG <- read.table("C:/Users/Macroeco/Documents/My_R/conteos_LFG.txt",header = T)


date_vec <- seq(as.Date('01/01/1990', format='%d/%m/%Y'), as.Date('31/12/2018', format='%d/%m/%Y'), 'days')

#index date for date sep-dec previous year of count 
index_y_1<-which((format(strftime(date_vec,"%Y"))) %in% (as.numeric(date_LFG$year)-1))
new_d_v_1<-date_vec[index_y_1]
index_month_1<-which(format(strftime(new_d_v_1,"%m"))>="09")
new_d_prev_y<- new_d_v_1[index_month_1]

#index date for date feb-jul year of count 
index_y<-which((format(strftime(date_vec,"%Y"))) %in% as.numeric(date_LFG$year))
new_d_v<-date_vec[index_y]
index_month<-which(format(strftime(new_d_v,"%m"))<="06")
new_date_y<- new_d_v[index_month]

#add date of previous year and year of count
new_data_char_vec <- c(new_d_prev_y,new_date_y)
new_data_char_vec <- new_data_char_vec[order(new_data_char_vec)]  

#filtering by avalible date from adt
data_vec_new<-new_data_char_vec[which(format(strftime(new_data_char_vec,"%Y")) >= 1993)] #remove all simbols and space between 
year_data<- format(strftime(data_vec_new,"%Y"))
unique_years <- unique(year_data[c(14:16)])

#change format YYYY-mm-dd for YYYYmmdd, remove "-"
data_char_vec<-gsub("-","",data_vec_new)


##### Open netcdf for extract lon and lat length for loop####

dir_path_adt <- "C:/Users/Macroeco/Documents/My_R/variables/global_madt_dt_gridded_025_deg/"
## List of .nc files

ind_nc_file <- list.files(dir_path_adt,recursive=T, pattern=paste(data_char_vec[1]))

filename <-paste(dir_path_adt,
                 ind_nc_file,
                 sep="")
nc_file <- gunzip(filename, remove = F)
# Open NetCDF file:
nc_base <- nc_open(nc_file, write=FALSE) # read the file

# Get coordinates:
lon <- (ncvar_get(nc_base,'lon'))-180
lat <- ncvar_get(nc_base,'lat')
adt_scale_fact <- as.numeric(ncatt_get(nc_base,'adt','scale_factor'))

lon_mtx <- c(matrix(lon, nrow = length(lon), ncol = length(lat),byrow= F))
lat_mtx <- c(matrix(lat, nrow = length(lon), ncol = length(lat), byrow= T))
adt_index <-matrix(lon_mtx,lat_mtx,nrow = length(lon_mtx), ncol = 2 )

##Reduce the lon and lat pair in polyg
files_polyg  <- pnt.in.poly(cbind(lon_mtx,  
                                  lat_mtx),
                            cbind(polygon$X1,
                                  polygon$X2))
adt_index <- which(files_polyg$pip == 1)  
lon_mtx<- lon_mtx[adt_index]
lat_mtx<- lat_mtx[adt_index]
#close the netcdf and delete de unzip file
nc_close(nc_base)

# Create name of the file:
file_name_uncompressed <- str_sub(ind_nc_file,
                                  -nchar(ind_nc_file),
                                  -4)
unlink(x=paste(dir_path_adt,
               file_name_uncompressed,
               sep=""), force = TRUE)

###########################################################################################################################

for (yy in seq(1,length(unique_years))) {
  
  year_index <- which(year_data == unique_years[yy])
  # Redefine:
  
  date_char_vec_temp <- data_char_vec[year_index]
  
  # Groups of days:
  uniq_days_temp <- unique(date_char_vec_temp)
  
  #### adt ###
  ## Define adt's directory path##
  # dir_path_adt <- "C:/Users/Macroeco/Documents/My_R/variables/global_madt_dt_gridded_025_deg/"
  # ## List of .nc files
  # nc_files_adt <- list.files(dir_path_adt,recursive=T, pattern=paste("*.nc"))
  # Uncompress .nc file:
  
  # Prelocate:
  list_adt<- list()
  
  adt_obs <- data.frame(lon = lon_mtx,
                        lat = lat_mtx,
                        time = rep(NA,length(lon_mtx)),
                        adt = rep(NA, length(lon_mtx)))
  
  # Loop to pair adt to cells´ data
  for (i in 1:length(uniq_days_temp)) {
    # Search the .nc file with coincidence in date for each cell:
    index_date <- which(date_char_vec_temp == uniq_days_temp[i])
    
    ind_nc_file <- list.files(dir_path_adt,
                              recursive=T,
                              pattern=date_char_vec_temp[index_date[1]])
    
      # Uncompress .nc file:
      filename <- paste(dir_path_adt,
                       ind_nc_file,
                       sep="")
      nc_file <- gunzip(filename,
                        remove=FALSE)
      # Open NetCDF file:
      nc_base <- nc_open(nc_file, write=FALSE) # read the file
      
      # Read variable:
      adt <- ncvar_get(nc_base,'adt')  
      time <- ncvar_get(nc_base, 'time')                  
      
      # Add to data frame adt and time
      
      adt_obs$adt<-adt[adt_index]
      adt_obs$time<- rep(time,length(adt_index))
      
      # Close the nc dataset:
      nc_close(nc_base)
      
     # Create name of the file:
      file_name_uncompressed <- str_sub(ind_nc_file,
                                        -nchar(ind_nc_file),
                                        -4)
      unlink(x=paste(dir_path_adt,
                     file_name_uncompressed,
                     sep=""), force = TRUE)
      #save values in adt data frame 
    list_adt[[i]]<- adt_obs
    
    }
    
  
  final_df <- do.call(rbind,list_adt)
  write.csv(final_df,file=paste("data_frame_file_adt_",unique_years[yy],".csv",sep=""))
  gc()
}


## Concatenate all csv files:
# Prelocate list:
list_cells <- list()
for (kk in seq(1,length(unique_years[(1:14)]))) {
  list_cells[[kk]] <- read.csv(paste("~/My_R/data_frame_file_adt_",unique_years[kk],".csv",sep=""))
}

final_csv <- do.call(rbind,list_cells)
write.csv(final_df,file="~/My_R/data_count_LFG_adt.csv")
