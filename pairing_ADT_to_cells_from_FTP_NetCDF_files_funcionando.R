

#### CLEAN EVERYTHING ####
rm(list = ls(all = TRUE)) #clear all;
graphics.off() # close all;
gc() # Clear memmory (residuals of operations?)

# Libraries:
require(R.utils) # To uncompress the gz files
require(RNetCDF) # Read .nc files
require(plyr) # round coordinates at a desired resolution
require(stringr) # for manipulating strings


data_base_observ <- read.csv("F:/My_R_hp/My_R/cell_data_sa_0075deg_each4km.csv")

unique_years <- unique(data_base_observ$year.cell)
unique_years <- sort(unique_years[which(unique_years >= 1993)]) # adt availability

for (yy in seq(1,length(unique_years))) {
  # Open cells data:
  # data_base_observ <- read.csv("cell_data_sa_0075deg_each4km.csv")
  # Index year:
  year_index <- which(data_base_observ$year.cell == unique_years[yy])
  # Redefine:
  data_base_observ <- data_base_observ[year_index,]
  
  #### adt ###
  ## Define adt's directory path##
  dir_path_adt <- "F:/My_R_hp/My_R/variables_global/AVISO_altimetry/global_madt_dt_gridded_025_deg/"
  ## List of .nc files
  nc_files_adt <- list.files(dir_path_adt,recursive=T, pattern=paste("*.nc"))
  
  
  # Create a vector of characters with the dates of the cells data to pair with the .nc file
  # Little trick:
  # Which months are lower than 10:
  ind_months_low <- which(data_base_observ$month.cell < 10)
  
  # Add a zero to the right of the year at those months:
  new_yr_vec_cells <- data_base_observ$year.cell
  new_yr_vec_cells[ind_months_low] <- new_yr_vec_cells[ind_months_low]*10
  
  # Likewise, which days are less than 10?:
  ind_days_low <- which(data_base_observ$day.cell < 10)
  new_mo_vec_cells <- data_base_observ$month.cell
  new_mo_vec_cells[ind_days_low] <- new_mo_vec_cells[ind_days_low]*10
  
  # Create a character vector joining the three components of the date (copying AVISO's file names):
  date_char_vec <- paste(new_yr_vec_cells,
                         new_mo_vec_cells,
                         data_base_observ$day.cell,
                         sep="")

  
  # Prelocate:
  data_base_observ$adt_obs <- rep(NA,nrow(data_base_observ))
  
  # Loop to pair adt to cells´ data
  for (i in 1:length(date_char_vec)) {
    # Search the .nc file with coincidence in date for each cell:
    ind_nc_file <- list.files(dir_path_adt,recursive=T, pattern=paste(date_char_vec[i]))
    if (length(ind_nc_file) < 1) {
      data_base_observ$adt_obs[i]  <- NA
    } else {
      # Uncompress .nc file:
      filename <- paste(dir_path_adt,
                        ind_nc_file,
                        sep="")
      nc_file <- gunzip(filename,
                        remove=FALSE)
      # Open NetCDF file:
      nc_base <- open.nc(nc_file, write=FALSE) # read the file
     # print.nc(nc_base)
      # Get coordinates:
      lon_adt <- (var.get.nc(nc_base,'lon', start=NA, count=NA,
                             na.mode=0, collapse=TRUE, unpack=FALSE))-360
      lat_adt <- var.get.nc(nc_base,'lat', start=NA, count=NA,
                            na.mode=0, collapse=TRUE, unpack=FALSE)
      # Read variable:
      adt <- var.get.nc(nc_base,'adt')
      adt_scale_fact <- att.get.nc(nc_base,'adt','scale_factor')
      # Pair in space:
      jj <- which.min(abs(lon_adt - (data_base_observ$lon.cell[i])))
      kk <- which.min(abs(lat_adt - (data_base_observ$lat.cell[i])))
      if (length(jj) < 1 | length(kk) < 1) {
        data_base_observ$adt_obs[i] <- NA
      } else {
        data_base_observ$adt_obs[i]  <- mean(adt[jj,kk],na.rm=T)*adt_scale_fact*100 # See metadata for details
      }
      
      # Remove the uncompressed file:
      # Close the nc dataset:
      close.nc(nc_base)
      # Create name of the file:
      file_name_uncompressed <- str_sub(ind_nc_file,
                                        -nchar(ind_nc_file),
                                        -4)
      unlink(x=paste(dir_path_adt,
                     file_name_uncompressed,
                     sep=""), force = TRUE)
    }
  }
  write.csv(data_base_observ,file=paste("data_frame_file_adt_",unique_years[yy],".csv",sep=""))
  gc()
}


## Concatenate all csv files:
# Prelocate list:
list_cells <- list()
for (kk in seq(1,length(unique_years))) {
  list_cells[[kk]] <- read.csv(paste("~/My_R/data_frame_file_adt_",unique_years[kk],".csv",sep=""))
}

final_csv <- do.call(rbind,list_cells)



# NOTA: Falta poner algo así, para que salga la base arreglada por timepo:
# data_base_observ.03_ord <- data_base_observ.03[with(data_base_observ.03, order(time.cell)), ] # Sorts data.frame  by column "myColumn"




### SAVE CSV ####
write.csv(final_csv,file="data_frame_paired_adt.csv")





# ONLY WHEN EVERYTHING IS CHECKED:
#########################################################33
###########################################################
###########################################################
###########################################################

# Erase the partial files:
for (hh in seq(1,length(unique_years))) {
  yr <- which(unique_years == unique_years[hh])
  unlink(x=paste("~/My_R/data_frame_file_adt_",unique_years[yr],".csv",
                 sep=""),
         force = TRUE)
}
gc()

