####Clean everithing
rm(list=ls())
graphics.off()
gc()

##abrir datos##
data_fish<-  read.csv("data_fish.csv")

#### MODEL IN JAGS LANGUAJE: ####
#### Text file to save the model: ####
sink("model_exerc_3_fish_count_hyper.txt")

 cat("model {
     
     #Likelihood of counts: 
      for(i in 1:n_counts) {
      count[i] ~  dpois(lambda_count[i])
      log(lambda_count[i]) <- log_lambda_count[i]

      log_lambda_count[i] <- alpha[pop[i]] + 
                             beta1[pop[i]] * year[i] + 
                              beta2[pop[i]] * pow(year[i],2) + 
                               beta3[pop[i]] * pow(year[i],3)
      
       err_vec_count[i]<- count[i] - lambda_count[i]

      #####Sum of square type discrepancy ----- for BAYESIAN-p-value ####
      
      #### Este analisis nos dirá si la distribucion usada (poisson) representa nuestros datos
      sqr_residuals_obs[i]<- pow(err_vec_count[i],2)
      
      ##Construimos un set de datos con ajuste perfecto a una dist. poisson
      new_obs_count[i] ~ dpois(lambda_count[i])
      new_sqr_residuals[i]<- pow(new_obs_count[i] - lambda_count[i],2)
      
      }
   
    
    
  
    #Hyper-distribution (random effects of population)
    for(j in 1:n_pop){
    alpha[j] ~ dnorm(hyp_mu_alpha, 1/pow(hyp_sd_alpha,2)) #efectos aleatorios en el intercepto
    beta1[j] ~ dnorm(hyp_mu_beta1, 1/pow(hyp_sd_beta1,2))
    beta2[j] ~ dnorm(hyp_mu_beta2, 1/pow(hyp_sd_beta2,2))
    beta3[j] ~ dnorm(hyp_mu_beta3, 1/pow(hyp_sd_beta3,2))
    }

   #Previas para hyper-parameters efectos aleatorios derivados de las diferentes poblaciones
    hyp_mu_alpha ~ dunif(min_alpha, max_alpha)            
    hyp_mu_beta1 ~ dunif(min_beta1, max_beta1)
    hyp_mu_beta2 ~ dunif(min_beta2, max_beta2)
    hyp_mu_beta3 ~ dunif(min_beta3, max_beta3)

  
    hyp_sd_alpha ~ dunif(0, max_sd_alpha)            
    hyp_sd_beta1 ~ dunif(0, max_sd_beta1)
    hyp_sd_beta2 ~ dunif(0, max_sd_beta2)
    hyp_sd_beta3 ~ dunif(0, max_sd_beta3)

   #Bayesian R-squared
     # r_squ_count<- pow(sd(lambda_count),2) /            #muestra el ajuste del modelo a los datos 
     # (pow(sd(lambda_count),2) + (pow(sd(err_vec_count),2)))
     # 
   # #Bayesian p-value estimation (bondad de ajuste): 
   # fit_obs_counts <- sum(sqr_residuals_obs[])
   # fit_new_counts <- sum(new_sqr_residuals[])
   # test_fit_counts <- step(fit_new_counts - fit_obs_counts) #step:si es positivo =1, si es negativo= 0
   # 
   # bayesian_p_value_count <- mean(test_fit_counts) #el valor esperado es de 0.5 +- 0.05 indicando un buen ajuste del modelo
   #                                                #si se obtiene un valor fuera de rango 0.45-0.55 toca probar otras dist. (binomial-negativa, gamma, twry, etc.)  
     
}",                       
    fill=TRUE)
sink() # end of JAGS model

    
 data_list<- list(
   n_counts = nrow(data_fish),
   count = data_fish$count,
   year = ((data_fish$year) - mean(data_fish$year))/ 100, # Reescalamos la variable
   n_pop = length(unique(data_fish$pop)),
   pop = data_fish$pop,
   
   min_alpha = 0, max_alpha = 7,   #alpha esta en unidades de conteos (y)
   min_beta1 = 7, max_beta1 = 15,
   min_beta2 = -20, max_beta2 = -5,
   min_beta3 = -300, max_beta3 = -50,
   max_sd_alpha = 10,            
   max_sd_beta1 = 10,
   max_sd_beta2 = 20,
   max_sd_beta3 = 100
      )
 
 ####Valores para cadenas MCMC####
 n.chains <- 3
 n.iter <- 100000
 n.burnin <- n.iter*0.1
 n.thin <- 10
 
 #### Parametros a monitorear ####
 monitor <- c("alpha",  #Solo monitoreas
              "hyp_mu_alpha",
              "hyp_sd_alpha",
              "beta1",
              "hyp_mu_beta1",
              "hyp_sd_beta1",
              "beta2",
              "hyp_mu_beta2",
              "hyp_sd_beta2",
              "beta3",
              "hyp_mu_beta3",
              "hyp_sd_beta3",
              "r_squ_count",
              "bayesian_p_value_count")
 
 ####Corremos el modelo#####
 require(R2jags)
 
 out <- jags(data = data_list,
             # inits = inits.list,
             parameters.to.save = monitor,
             model.file = "model_exerc_3_fish_count_hyper.txt",
             n.chains = n.chains,
             n.thin = n.thin,
             n.iter = n.iter,
             n.burnin = n.burnin,
             DIC = T)
 
 #### MODEL RESULTS ########
 # Summarize posteriors and statistics:
 model_summary <- print(out, dig=3) # number of decimals for the output.
 
 # Extract MCMC list: 
 mcmc_list <- as.mcmc(out) # For JAGS
 
 
 # Libraries for diagnostics:
 require(mcmcplots)
 require(ggmcmc)  # For MCMC diagnostics
 require(coda)    # For MCMC analysis
 require(lattice) # For quick posterior ploting and diagnostics
 
 # 
 # Densities:
 x11()
 densityplot(mcmc_list)
 
 ####Plot####
 
 count_predict <- 500
 
 ##HYPER-PREDICTION:
 hyper_predict_df<- data.frame(
   year_vec = seq(min(data_fish$year),
                  max(data_fish$year),
                  length.out = count_predict),
   
   year_vec_scaled = seq(min(data_list$year),
                         max(data_list$year),
                         length.out = count_predict))
 ##hyper-prediction
 hyper_predict_df$min_count = exp(model_summary$summary["hyp_mu_alpha", "2.5%"]+ 
                                    model_summary$summary["hyp_mu_beta1", "2.5%"]*hyper_predict_df$year_vec_scaled+
                                    model_summary$summary["hyp_mu_beta2", "2.5%"]*(hyper_predict_df$year_vec_scaled^2)+
                                    model_summary$summary["hyp_mu_beta3", "2.5%"]*hyper_predict_df$year_vec_scaled^3)
 
 hyper_predict_df$max_count = exp(model_summary$summary["hyp_mu_alpha", "97.5%"]+ 
                                    model_summary$summary["hyp_mu_beta1", "97.5%"]*hyper_predict_df$year_vec_scaled+
                                    model_summary$summary["hyp_mu_beta2", "97.5%"]*(hyper_predict_df$year_vec_scaled^2)+
                                    model_summary$summary["hyp_mu_beta3", "97.5%"]*hyper_predict_df$year_vec_scaled^3)
 
 hyper_predict_df$median_count = exp(model_summary$summary["hyp_mu_alpha", "50%"]+ 
                                       model_summary$summary["hyp_mu_beta1", "50%"]*hyper_predict_df$year_vec_scaled+
                                       model_summary$summary["hyp_mu_beta2", "50%"]*(hyper_predict_df$year_vec_scaled^2)+
                                       model_summary$summary["hyp_mu_beta3", "50%"]*hyper_predict_df$year_vec_scaled^3)
#prediction by opulation
 
 
 hyper_predict_df$median_pop1 = exp(model_summary$summary["alpha[1]", "50%"]+ 
                                      model_summary$summary["beta1[1]", "50%"]*hyper_predict_df$year_vec_scaled+
                                      model_summary$summary["beta2[1]", "50%"]*(hyper_predict_df$year_vec_scaled^2)+
                                      model_summary$summary["beta3[1]", "50%"]*hyper_predict_df$year_vec_scaled^3)
 hyper_predict_df$median_pop2 = exp(model_summary$summary["alpha[2]", "50%"]+ 
                                      model_summary$summary["beta1[2]", "50%"]*hyper_predict_df$year_vec_scaled+
                                      model_summary$summary["beta2[2]", "50%"]*(hyper_predict_df$year_vec_scaled^2)+
                                      model_summary$summary["beta3[2]", "50%"]*hyper_predict_df$year_vec_scaled^3)
 hyper_predict_df$median_pop3 = exp(model_summary$summary["alpha[3]", "50%"]+ 
                                      model_summary$summary["beta1[3]", "50%"]*hyper_predict_df$year_vec_scaled+
                                      model_summary$summary["beta2[3]", "50%"]*(hyper_predict_df$year_vec_scaled^2)+
                                      model_summary$summary["beta3[3]", "50%"]*hyper_predict_df$year_vec_scaled^3)
 hyper_predict_df$median_pop4 = exp(model_summary$summary["alpha[4]", "50%"]+ 
                                      model_summary$summary["beta1[4]", "50%"]*hyper_predict_df$year_vec_scaled+
                                      model_summary$summary["beta2[4]", "50%"]*(hyper_predict_df$year_vec_scaled^2)+
                                      model_summary$summary["beta3[4]", "50%"]*hyper_predict_df$year_vec_scaled^3)
 
 
 
 
require(ggplot2)  
   
hyper_predict_regresion_count<-
  ggplot()+
     #hyper prediction 
     geom_ribbon(data = hyper_predict_df,
                 aes(x = year_vec,
                     ymin = min_count,
                     ymax = max_count),
                 fill = "lightgray",
                 color = "black", 
                 linetype = "dashed",
                 size = 0.2,
                 alpha = 0.75)+
   
    geom_path(data = hyper_predict_df,
              aes(x = year_vec,
                  y = median_count),
              color = "black",
              size = 1) +
  
  geom_path(data = hyper_predict_df,
            aes(x = year_vec,
                y = median_pop1),
            color = "red",
            size = 1) +
  geom_path(data = hyper_predict_df,
            aes(x = year_vec,
                y = median_pop2),
            color = "blue",
            size = 1) +
  geom_path(data = hyper_predict_df,
            aes(x = year_vec,
                y = median_pop3),
            color = "green",
            size = 1) +
  geom_path(data = hyper_predict_df,
            aes(x = year_vec,
                y = median_pop4),
            color = "cyan",
            size = 1) +
    
      #Observation:
     
     geom_point(data = data_fish,
                aes(x = year,
                    y = count),
                fill = "white",
                color = "black",
                shape = 21,
                size = 1,
                stroke = 0.5)+
  
  theme_bw()+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,400))+
  coord_fixed(ratio = 0.08)+
  labs(y ="Count", x= "Year")+
  ggtitle("Random effects regression")
   
   
  
  
  #### Layout and file ####
   tiff(filename="hierarchical_model_by_pop_hypermedia.tiff",
        width=20,height=22.5,
        units="cm",
        bg="white",
        res=300,
        compression = c("lzw"))
   print(hyper_predict_regresion_count)
   dev.off()   