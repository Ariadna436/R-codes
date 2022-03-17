 # Bayesian ANOVA of simulated d15N data


#### CLEAN EVERYTHING ###
rm(list=ls())
graphics.off() # close all;
gc() # Clear memmory (residuals of operations?)

 # read data file and create data vectors
cicese_data <- read.csv("C:/Windows.old.000/Users/Ariadna/Documents/maestria/isotopos_grupos.csv")
d15N <- cicese_data$y
site <- cicese_data$group
site.names <- levels(site)

 # load library
require(R2jags)#mismo que library()


###################
## Create data list
###################
data.list <- list(            #creamos una lista con una serie de variables
  nA = length(d15N[site=="1"]), #tamaño de vector solo d15N donde los valores correspondan al sitio A
  nB = length(d15N[site=="2"]),
  nC = length(d15N[site=="3"]),
  d15N.A = d15N[site=="A"],#los valores que corresponden a los 10 sitios de la longitud anterior
  d15N.B = d15N[site=="B"],
  d15N.C = d15N[site=="C"])

#CREA UN ARCHIVO DE TEXT CON EL MODELO
sink("jags_anova.txt")


########################################################################################################
## "ANOVA" by site
###############
cat("model {   #a partir de aquí se escribe en lenguaje BUGS y no en R
 # verosimilitud for mean d15N at each site
  for(i in 1:nA) {d15N.A[i] ~ dnorm(mu.A, tau)} #de los datos desde 1 hasta la longitud nA tomara cada dato asumiendo que proviene de una distribución normal, con media A y tau=presición (1/varianza)
  for(i in 1:nB) {d15N.B[i] ~ dnorm(mu.B, tau)} #mu es la media de la dist. normal
  for(i in 1:nC) {d15N.C[i] ~ dnorm(mu.C, tau)} 
 
  # uniform priors creando la previa
  #si se tuviera una previa informativa se cambiaría por el valor de la distribución previa (media,tau) 
  #mu.A~dnorm(20.5,0.001) sería con una previa informativa con dist. normal con una probabilidad de media de 20.5 y tau de 1/var o 0.001
  mu.A ~ dunif(10,20) #asumiendo una distribución uniforme con limites de 10 a 20 o minimo y máximo de los datos mas un poco mas
  mu.B ~ dunif(10,20) #una dist. uniforme considerando que no se  cuenta con una función previa (prior)
  mu.C ~ dunif(10,20) 
  sd   ~ dunif(0,4) #se amplia los limites de la des. estandar

   # derived parameters
  tau <- 1/pow(sd,2) #tau es 1/varianza aqui tau 1/sd^2 pero se escribe pow(sd,2)
 
   delta.AB <- mu.A-mu.B  #cual es la diferencia entre medias de dos sitios         # difference in d15N between A and B
   delta.AC <- mu.A-mu.C 
   delta.BC <- mu.B-mu.C 
  }",fill=TRUE)
sink()
# termina lenguaje JAGS hasta aquí solo hemos creado el modelo que se aplicará despues


##############################
## set MCMC controls
##############################
#aqui generamos los parámetros de las cadenas de markov-monte carlo
ni <- 10000 #interacciones
nb<-2000 #burnin
nt<-10 #thin
nc<-3 #chain

##############################
## Especifica donde inicia los valores para la cadena de markov
## o en este caso elegimos valores al azar en una dist. uniforme (runif)
##############################

inits.list <- lapply(1:nc,function(i) list( #elige un punto donde iniciará cada cadena para cada parámetro y hace una lista, en este ejemplo solo habrá una cadena
      mu.A = runif(1,10,20), #elegir un punto al azar entre una dist. uniforme con limites de 10 a 20
      mu.B = runif(1,10,20),
      mu.C = runif(1,10,20),
      sd   = runif(1,0,4) 
))

#############################
### set parameters to monitor parametros a utilizar
#############################

monitor <- c(names(inits.list[[1]]),"delta.AB","delta.AC","delta.BC") #de la lista de valores iniciales monitorie los valores creados mu.A, mu.B, mu.C pero ademas delta.AB que es un valor resultante de la diferencia entre mu.A y mu.B

###########################
## Aquí empieza a correr el modelo con JAGS
###########################
out<-jags(data=data.list,
          inits = inits.list,
          parameters.to.save = monitor,
          model.file = "jags_anova.txt",
          n.chains = nc,
          n.thin = nt,
          n.iter = ni,
          n.burnin = nb,
          DIC = TRUE)
save(out,file="resultados_del_modelo_jags.RData")

#RESULTADOS DEL MODELO
model_summary_jags_anova<-print(out,dig=3)#para saber que los parámetros convergen Rhat debe ser cercano a 1

#extraer las posteriores
post_df<-as.data.frame(out$BUGSoutput$sims.list)

#probabilidad de que las diferencias entre A y B sean positivas
sum(post_df$delta.AB>0)/nrow(post_df)
sum(post_df$delta.AC>0)/nrow(post_df)
sum(post_df$delta.BC>0)/nrow(post_df)
 
#odds
p.AB<-sum(post_df$delta.AB>0)/nrow(post_df)
p.AB/(1-p.AB)
p.AC<-sum(post_df$delta.AC>0)/nrow(post_df)
p.AC/(1-p.AC)
p.BC<-sum(post_df$delta.BC>0)/nrow(post_df)
p.BC/(1-p.BC)

#Extraer las cadenas
mcmc_list<-as.mcmc(out)

require(ggmcmc)
require(coda)
require(lattice)

#densidades
densityplot(mcmc_list)

#historial de las cadenas
traceplot(mcmc_list)

#Autocorrelación
autocorr.plot(mcmc_list) #la autocorrelación debe bajar pronto a 0 

#MAtriz de correlación de las posteriores con paquete ggmcmc
chain_res<-ggs(mcmc_list)
ggs_pairs(chain_res, lower=list(continuos="density"))
#si las matrizz tiene una forma circular indica que cada parametro no esta correlacionado con el otro
#si la forna es como diagonal indica que los parámetros usados estan correlacionados entre si y uno de los dos parametros sale sobrando
