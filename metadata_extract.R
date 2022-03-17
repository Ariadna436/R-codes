##Clean everithing##
rm(list=ls())
graphics.off()
gc()

library(exiftoolr)
library(dplyr)
library(leaflet)
library(raster)


dir_path_image<- "F:/Drone_LFG/09.VII.16 (Costilla - Campo Lima; Punta Proa - primer derrumbe Punta sur)/"  
files <- list.files(dir_path_image[1],pattern = "*.DNG")

write.csv(name,'files_2.csv')
count_lfg<- read.csv("list_photo_count_GFS_Flores.csv")
count_lfg<- count_lfg[which(count_lfg$day == 14),]
name<- gsub(".DNG", "", files)
files_2<- files[which(name %in% count_lfg$name)]

list_metadata<- list()
                        
for (i in seq(1,length(files))) {
  data_image<- exif_read(paste(dir_path_image,files_2[i], sep = ""))
  metadata_df<- data.frame(name=data_image$FileName,
                           date=data_image$DateTimeOriginal,
                           lon=data_image$GPSLongitude,
                           lat=data_image$GPSLatitude)
  list_metadata[[i]]<- metadata_df
  
  }

 final.df<-do.call(rbind,list_metadata)
 

 
final.df$Date <- as.POSIXct(as.character(final.df$date), format =  "%Y:%m:%d %H:%M")
final.df$date <- as.character(as.Date(final.df$Date))
final.df$time <- format(final.df$Date, "%T")
final.df$date<- as.Date(final.df$date)

csv_df<- data.frame(
                    year = as.numeric(format(final.df$date, format = "%Y")),
                    month = as.numeric(format(final.df$date, format = "%m")),
                    day = as.numeric(format(final.df$date, format = "%d")),
                    name = count_lfg$name,
                    lon = final.df$lon, 
                    lat = final.df$lat,
                    total_count= count_lfg$total_count,
                    male= count_lfg$male, 
                    subadult= count_lfg$subadult,
                    juvenile = count_lfg$juvenile,
                    female= count_lfg$female,
                    pup= count_lfg$pup,
                    unknow= count_lfg$unknow
                    )
                    

 write.csv(csv_df, paste('count_LFG_Flores_',unique(csv_df$day), '.csv', sep = ''))
        
 
 ####unir días en un solo archivo####
 unique_day<- c(6,7,8,9,14)
 list_cells <- list()
 for (kk in seq(1,length(unique_day))) {
   list_cells[[kk]] <- read.csv(paste('count_LFG_Flores_',unique_day[kk], '.csv', sep = ''))
 }
 
 final_csv <- do.call(rbind,list_cells)

# unique_name<- gsub("\\S+_", "DJI_", final_csv$name) #remueve todo lo anterior a un _
# unique_name<- gsub(".TIF", "", unique_name) #remueve las terminaciones .TIF
# unique_name<- gsub(".DNG", "", unique_name) #remueve terminación .DNG
#  
# final_csv$name<- unique_name

write.csv(final_csv,"count_LFG_Flores_2016.csv")
