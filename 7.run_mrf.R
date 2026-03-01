library(MacroRF)

source("functions/functions.R")
source("functions/fixed_window.R")
source("functions/expanding_window.R")


load("data/rawdata.RData")
df <- dados

model_list = list()

# call_mrf fixed window ####

model_name = "MRF_fxd_12lags"


for (i in c(1:12)){
  cat("horizonte ", i,"\n")
  model = fixed_window(df=df, lin.model = "FAARRF", horizon = i,
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

model_name = "MRF_exp_12lags"

for (i in 1:12){
  cat("horizonte ", i,"\n")
  model = expanding_window(df=df, lin.model = "FAARRF", horizon = i,
                           n_y_lags = 12,
                           n_lags_of_each_var = 2,
                           n_lags_of_5_factors = 12,
                           n_lags_j = 12,
                           n_maf_components = 2,
                           oos.pos = 312)
  model_list[[i]] = model
}


save(model_list, file = "model_list_exp.RData")


# salvando os forecasts de expanding window


forecast_horizon <- list()
for (i in 1:12) {
  temp_preds <- list() 
    for (j in 1:26) {
    temp_preds[[j]] <- model_list[[i]][[j]]$pred
  }
  forecast_horizon[[i]] <- unlist(temp_preds)
}

forecasts <- do.call(cbind, forecast_horizon)



# finalizando as previsoes ####


# o accumulate_model calcula as diagonais, sendo assim, os valores de previsao de 3 e 6 meses e adiciona as colunas referetnes
forecasts_4lags = accumulate_model(forecasts)

View(forecasts)

save(forecasts_4lags,file = paste("forecasts/",model_name,".rda",sep = ""))


plot(tail(df[,"CPI"],312),type = "l")
lines(forecasts[,1],col = 5)

