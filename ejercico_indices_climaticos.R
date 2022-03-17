####Clean everithing
rm(list=ls())
graphics.off()
gc()

# mis_datos<- read.csv('ruta_de_tu_archivo.csv') #abre un archivo excel y guardalo como .csv puede ser abundancia de bichos o riqueza de especies, etc
mis_datos<- read.csv('C:/Users/Macroeco/Documents/ejemplo_indices.csv') #cuida que al pegar la ruta cambies "\" por "/"

str(mis_datos) #si no te acuerdas como se llaman ts encabezados con esto los lees 

mis_datos_df<- data.frame(year= mis_datos$aÒo,
                          abun_media= mis_datos$abundancia,
                          min_abun = mis_datos$minima,
                          max_abun = mis_datos$maxima) #este es solo un ejemplo, lo que va despues del $ es como se llama tu variable el archivo



####ir a la ventana de paquetes "packages" seleccionar "install" y esribir rsoi

require(rsoi)
enso<- data.frame(download_enso(climate_idx = c("all", "soi", "oni", "npgo"), #se crea un data.frame para poder gr·ficar el NiÒo
                                create_csv = F))

enso_study_period<- enso[which(enso$Year >= 1996 &     #este es un filtro para que selecciones el rango de a—os para graficar 
                        enso$Year <= 2007),]   #cambia a los aÒos que necesites

enso_study_period$d<- ifelse(c(enso_study_period$SOI)<0,0,1)  #crea una columna d para separar en aÒos c·lidos de frios

require(rpdo)
pdo<- data.frame(download_pdo())           #esta parte es lo mismo pero ahora con la decadal del pacifico
pdo$date<- as.Date(paste(pdo$Year,pdo$Month,1, sep = "-"))
pdo_study_period<- pdo[which(pdo$Year >= 1996 & 
                      pdo$Year <= 2007),]



grafico1_tus_datos<- 
  ggplot()+
  geom_errorbar(data = mis_datos_df,
                aes(x = year,
                    ymin = min_abun,
                    ymax = max_abun),
                color = "black", #elije el color que quieres
                size = 0.2,       #tamaÒo de la linea
                alpha = 0.75)+     #transparencia del color
  
  geom_point(data = mis_datos_df,
             aes(x = year,
                 y = abun_media),
             fill = "azure3",      #relleno del punto
             color = "black",      #color del contorno
             shape = 21,           #tipo de punto, si buscas en google "shape form for R" te da las opciones
             size = 2,            #tamaÒo del punto
             stroke = 0.5)+         #grosor del contorno
  
  
  
  
  theme_bw()+
  theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   strip.background = element_blank(),
                   panel.border = element_rect(colour = "black"))


grafico_2_el_niÒo<- 
  ggplot()+
  
    geom_ribbon(data = enso_study_period,   #
              aes(x= Date,  
                  ymax=d*SOI, ymin=0,  fill = "cold")) +   #puedes cambiar cold por la niÒa
  
  geom_ribbon(data = enso_study_period,
              aes(x= Date,  
                  ymax=0,  ymin=(1-d)*SOI, fill = "hot")) + 
  
  
  
  scale_fill_manual(name='SST', values=c("cold"= "red2","hot"= "royalblue1")) + #aquÌ puedes cambiar los colores 
  
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black"))


grafico3_decadal<- ggplot()+
  geom_line(data= pdo_study_period,
          aes(x= date, y=PDO))+
  
  geom_point(data= pdo_study_period,
             aes(x= date, y=PDO))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black"))

require(cowplot)
x11()
plot_grid(grafico1_tus_datos,grafico_2_el_niÒo,grafico3_decadal, 
          align= "v", #pon el nombre de tus g'raficos, align= si lo quieres alineado vertical u horizontal "h", o ambos "hv"
          nrow= 3, ncol= 1, #n˙mero de columnas y filas en que quieres que grafique
          labels = "AUTO") #los nombres de las graficas, puedes especificarlos o se pondr·n por default con la informaciÛn dada
                                               





