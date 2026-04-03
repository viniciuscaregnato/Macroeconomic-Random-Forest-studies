library(MacroRF)

source("functions/functions.R")
source("functions/fixed_window.R")
source("functions/expanding_window.R")


load("data/rawdata.RData")
df <- dados

model_list = list()

# call_mrf fixed window ####

model_name = "MRF_fxd_FAAARRF_4lags"


for (i in c(1:12)){
  cat("horizonte ", i,"\n")
  model = fixed_window(df=df, lin.model = "FAARRF_4lags", horizon = i,
                       n_y_lags = 12,
                       n_lags_of_each_var = 2 ,
                       n_lags_of_5_factors = 12,
                       n_lags_j = 12,
                       n_maf_components = 2,
                       oos.pos = 312)
  model_list[[i]] = model
}



# salvando os forecasts de fixed window

forecasts <-  Reduce(cbind,lapply(model_list, function(x)x$pred))






# call_mrf expanding window ####

model_name = "MRF_exp_FAARRF_st1"

for (i in 1:12){
  cat("horizonte ", i,"\n")
  model = expanding_window(df=df, lin.model = "FAARRF", horizon = i,
                           n_y_lags = 4,
                           n_lags_of_each_var = 4,
                           n_lags_of_5_factors = 4,
                           n_lags_j = 4,
                           n_maf_components = 4,
                           oos.pos = 312)
  model_list[[i]] = model
}


save(model_list, file = "model_list_exp.RData")


# salvando os forecasts de expanding window


forecast_horizon <- list()
for (i in 1:12){
    forecast_horizon[[i]] <- lapply(model_list[[i]], function(x)x$pred)
     }

forecast_series <- lapply(forecast_horizon, function(i){unlist(i)})

forecasts <- do.call(cbind, forecast_series)



# finalizando as previsoes ####


# o accumulate_model calcula as diagonais, sendo assim, os valores de previsao de 3 e 6 meses e adiciona as colunas referetnes
forecasts_4lags = accumulate_model(forecasts)

View(forecasts_4lags)

save(forecasts_4lags,file = paste("forecasts/",model_name,".rda",sep = ""))



#forecasts = accumulate_model(forecasts)

#View(forecasts)

#save(forecasts,file = paste("forecasts/",model_name,".rda",sep = ""))






plot(tail(df[,"CPI"],312),type = "l")
lines(forecasts[,1],col = 5)







"MRF_exp_FAARRF_st1: n_y_lags = 4,
                     n_lags_of_each_var = 4,
                     n_lags_of_5_factors = 4,
                     n_lags_j = 4,
                     n_maf_components = 4,
                     oos.pos = 312"


