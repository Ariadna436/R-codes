rm(list = ls(all = TRUE)) #clear all;
graphics.off() # close all;
gc() # Clear memmory 
################################################################

require(rgdal)
require(geosphere) # Measure of areas
require(SDMTools)
require(maptools)

path<- "C:/Users/Macroeco/Documents/My_R/Apt_pop_grow/Sustratos/estratos_isla_gpe"

files <- list.files(path = path,all.files = T)
files <- files [c(3:length(files))]
####Abrir todos los archivos .kml de la carpeta y extraer sus coordenadas####
list_count<- list()
for (i in seq(1,length(files))) {
  stratum_div= as.data.frame(getKMLcoordinates(paste(path,files[[i]],sep = "/")))
  lon = stratum_div[,1]
  lat = stratum_div[,2]
  name <- rep.int(gsub(".kml","",files[i]), times=length(lon))
  stratum_count<- data.frame(name,lon,lat)
  list_count[[i]] <- stratum_count
  
}

stratum_all_count<- do.call(rbind,list_count)
#####Abrir archivo con la clasificacion de estratos de Capitanachi-Garcia####
stratum_gpe<-read.csv("length_gpe_island_stratum.csv",sep = ';')
new_stratum <- stratum_gpe[order(-stratum_gpe$lat),]

unique_sites <- unique(stratum_all_count$name)
list_match<- list()
list_sites<- list()

####loop para empatar cada sitio de estudio con su sustrato correspondiente####
for (k in seq(1,length(unique_sites))){
  temp_site<- stratum_all_count[which(stratum_all_count$name == unique_sites[k]),]
  
  index_min<- which.min(abs(new_stratum$lat - min(temp_site$lat))) 
  index_max<- which.min(abs(new_stratum$lat - max(temp_site$lat)))  
  
    
  unique_id_temp<-unique(new_stratum$id[as.numeric(c(index_max:index_min))])
  index_id <- which(stratum_gpe$id %in% unique_id_temp) 
  
  sites_df<- (stratum_gpe[index_id,])
  sites_df<- sites_df[!is.na(sites_df$length),]
  sites_df$name<- rep(unique(temp_site$name), length(sites_df$id))
  list_sites[k]<- list(sites_df)
  
}


sites_final <- (do.call(rbind,list_sites))
write.csv(sites_final,file="sites_final.csv")

#sites_guadalupe<- read.csv("guadalupe_stratum_by_site.csv", sep = ";")

list_by_stratum<- list()
list_all_site<- list()

#####Loop para sumar la longitud de cada tipo de estrato presente por sitio de muestreo####
for (j in seq(1,length(unique_sites))){
  each_site<- sites_final[which(sites_final$name == unique_sites[j]),]
  # each_site<-each_site[order(each_site$stratum),]
  unique_stratum<- unique(each_site$stratum)

    for (m in seq(1,length(unique_stratum))){
    index_site <- which(each_site$stratum == unique_stratum [m] )
    
    by_stratum_df<- data.frame(name= unique(each_site$name[index_site]),
                               stratum= unique(each_site$stratum[index_site]),
                               length=sum(each_site$length[index_site]),
                               prop_stratum= sum(each_site$prop_stratum[index_site]),
                               prop_isla= sum(each_site$prop_island[index_site]))
    list_by_stratum [[m]] <- by_stratum_df
    }
  final_stratum_site<- do.call(rbind,list_by_stratum)
  
  list_all_site[[j]]<- final_stratum_site 
  }
final_all_site<- do.call(rbind,list_all_site)

write.csv(final_all_site, file= "guadalupe_site_count_all_revision.csv")
