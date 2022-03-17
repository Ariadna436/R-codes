rm(list = ls(all = TRUE)) #clear all;
graphics.off() # close all;
gc() # Clear memmory (residuals of operations?, cache? Not sure)

abun_area_df<- read.csv("gfs_count_by_stratum_rel_abund_boat_corrected_CF_pond.csv")
abun_area_df$area<- as.factor(abun_area_df$area)
  abun_area_df$year<- as.factor(abun_area_df$year)


boxplot(abun_area_df$rel_abund ~ abun_area_df$year, ylab = "Proporción por área",fill = "gray")
fm = aov((abun_area_df$rel_abund ~ abun_area_df$year) )
summary(fm)

boxplot(abun_area_df$rel_abund ~ abun_area_df$area, ylab = "Proporción por área")
fm = aov(abun_area_df$rel_abund ~ abun_area_df$area) 
summary(fm)

interaction.plot(abun_area_df$area, abun_area_df$year,abun_area_df$total_corrected, trace.label = "Año")
fm = aov(abun_area_df$rel_abund ~ abun_area_df$area*abun_area_df$year) 
summary(fm)

qf(0.05, 16-1, 128-16, lower.tail = F)


intervals = TukeyHSD(fm)
plot(intervals)
tukey_result<- as.table(intervals[["area"]])
#Archivo de texto para guardar modelo
sink("model_anov_bay.txt")

####modelo jags####
cat("model{

#verosimilitud del d15N en cada sitio
 #Year 1967
 for(i in 1:nA){
 rel_abund.A[i] ~ dnorm(mu.A,tau) #jags usa tau para reducir los valores de varianza tau=1/varianza=1/d.e.^2
 }

  #Year 1977
 for(i in 1:nB){
    rel_abund.B[i] ~ dnorm(mu.B,tau) #jags usa tau para reducir los valores de varianza tau=1/varianza=1/d.e.^2
    }

  #Year 1988
 for(i in 1:nC){
 rel_abund.C[i] ~ dnorm(mu.C,tau) #jags usa tau para reducir los valores de varianza tau=1/varianza=1/d.e.^2
 }

 #Year 1991
 for(i in 1:nD){
 rel_abund.D[i] ~ dnorm(mu.D,tau) #jags usa tau para reducir los valores de varianza tau=1/varianza=1/d.e.^2
 }

 #Year 1992
 for(i in 1:nE){
 rel_abund.E[i] ~ dnorm(mu.E,tau) #jags usa tau para reducir los valores de varianza tau=1/varianza=1/d.e.^2
 }

 #Year 1993
 for(i in 1:nF){
 rel_abund.F[i] ~ dnorm(mu.F,tau) #jags usa tau para reducir los valores de varianza tau=1/varianza=1/d.e.^2
 }

#Year 2013
 for(i in 1:nG){
 rel_abund.G[i] ~ dnorm(mu.G,tau) #jags usa tau para reducir los valores de varianza tau=1/varianza=1/d.e.^2
 }

#Year 2016
 for(i in 1:nH){
 rel_abund.H[i] ~ dnorm(mu.H,tau) #jags usa tau para reducir los valores de varianza tau=1/varianza=1/d.e.^2
 }
 
#Previas para cada parametroa estimar 
  mu.A ~ dunif(10,20)
  mu.B ~ dunif(10,20)
  mu.C ~ dunif(10,20)
  sd ~ dunif(0,4)

#parametros derivados 
 tau<- 1/pow(sd,2) #derivamos tau de la desv estandar (sd)

 dif.AB<- mu.A-mu.B # diferencias d15N entre sitio A y B
 dif.AC<- mu.B-mu.C # diferencias d15N entre sitio B y C
 dif.AD<- mu.A-mu.C # diferencias d15N entre sitio A y C
 dif.AE<- mu.A-mu.B # diferencias d15N entre sitio A y B
 dif.AF<- mu.B-mu.C # diferencias d15N entre sitio B y C
 dif.AG<- mu.A-mu.C # diferencias d15N entre sitio A y C

}", 
    fill = T)
sink()

#Abrimos el archivo
cicese_data<-read.csv("C:/Users/Macroeco/Documents/My_R/cicese_2018.csv") 

#lista de parametros para jags
data.list<-list(
  nA=length(which(cicese_data$Site=="A")),
  nB=length(which(cicese_data$Site=="B")),
  nC=length(which(cicese_data$Site=="C")),
  
  d15N.A= cicese_data$d15N[cicese_data$Site=="A"],
  d15N.B= cicese_data$d15N[cicese_data$Site=="B"],
  d15N.C= cicese_data$d15N[cicese_data$Site=="C"]
)

#Valores iniciales

#parametros a monitorear
monitor<- c("mu.A","mu.B","mu.C","dif.AB","dif.BC","dif.AC","sd")

#Valores para las cadenas
n.chains<-3 
n.iter<-20000
n.burnin<-2000
n.thin<-10

require(R2jags) #este es el vinculo entre jags y R

#guardamos un objeto donde especificamos como se llama cada parametro para jags
out<-jags(data = data.list,
          #inits = inits.list,
          parameters.to.save = monitor,
          model.file = "model_anov_bay.txt",
          n.chains = n.chains,
          n.thin = n.thin,
          n.iter = n.iter,
          n.burnin = n.burnin,
          DIC=T)
#Resultados del modelo-> posteriores
model_summary<-print(out,dig=3) 

#extraer las posteriores como una lista de las MCMC del objeto jags
mcmc_list<- as.mcmc(out) 
require(mcmcplots)

#llamando librerias para el diagnóstico
require(ggmcmc)#para diagnostico de las cadenas
require(coda)#para analisis de mcmc
require(lattice)#para graficar diagnostico y analisis

####DIAGNOSTICO DE RESULTADOS####
#eL MEJOR DIAGNOSTICO ES EL OJO HUMANO
#densidades 
x11()
densityplot(mcmc_list)# Mientras mas ecimadas esten las cadenas hay mejor convergencia

#historial de las cadenas convergencia
x11()
traceplot(mcmc_list)# si NO se ven mezcladas las cadenas desde el inicio hay que aumentar el burnin

#grafico de autocorrelacion
x11()
autocorr.plot(mcmc_list) #las lineas muestran la autocorrelacion, el primer paso es alto pero debe bajar
# si la autocorrelacion no baja debe aumentarse el thinning y las iteracciones
acfplot(mcmc_list)# tambien es para autocorrelación, si no se ve que cae a valores cercanos a 0 aumentar el thinning y las iteracciones

#grafico Gelman-> convergencia
x11()
gelman.plot(mcmc_list) #nos muestra si fue suficiente la fase de burnin, hay que aumentarla si los valores se alejan de 1, una decima arriba es demaciado

#correlaciones posteriores, mientras mas cercano a 0 menos correlacionado los parametros, si se correlacionan hay que reevaluar los parametros utilzados
x11()
chain_res<-ggs(mcmc_list)
ggs_pairs(chain_res,
          lower=list(continuous= "density"))# permite evaluar la correlacion entre los parametros

#Dataframe de las posteriores
post<-as.data.frame(out$BUGSoutput$sims.matrix)#busca la matriz de simulaciones
                                               #(sims.matrix)se mezcla la posterior de las tres cadenas en una sola

#probabilidad de proporciones:
  ##Probabilidades AB
  index_positAB<-which(post$dif.AB>0) #posicion de valores positivos de la posterior (m2-m1)-> los valores positivos es donde la prop. de m2 es mayor a m1
  prop_positAB<-length(index_positAB)/nrow(post) #probabilidad=proporción de valores positivos en la posterior= numero de valores (index_pos) entre la cantidad total de valores (nrow)
  index_negatAB<-which(post$dif.AB<0) # valores donde los m2 es menor a m1
  prop_negtAB<-length(index_negatAB)/nrow(post)
  oddsAB<-round(length(index_positAB)/length(index_negatAB))


  ##Probabilidades AB
  index_positBC<-which(post$dif.BC>0) #posicion de valores positivos de la posterior 
  prop_positBC<-length(index_positBC)/nrow(post) #probabilidad=proporción de valores positivos en la posterior= numero de valores (index_pos) entre la cantidad total de valores (nrow)
  index_negatBC<-which(post$dif.BC<0) # valores donde los m2 es menor a m1
  prop_negtBC<-length(index_negatBC)/nrow(post)
  oddsBC<-round(length(index_positBC)/length(index_negatBC))

  ##Probabilidades AB
  index_positAC<-which(post$dif.AC>0) #posicion de valores positivos de la posterior 
  prop_positAC<-length(index_positAC)/nrow(post) #probabilidad=proporción de valores positivos en la posterior= numero de valores (index_pos) entre la cantidad total de valores (nrow)
  index_negatAC<-which(post$dif.AC<0) # valores donde los m2 es menor a m1
  prop_negtAC<-length(index_negatAC)/nrow(post)
  oddsAC<-round(length(index_positAC)/length(index_negatAC))
 
#Guardar resultados
  save(out, model_summary,
       mcmc_list, data.list,
       file="resul_anov_homo_var.RData")

library(fields)


