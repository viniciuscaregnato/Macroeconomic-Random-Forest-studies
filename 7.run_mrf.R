library(MacroRF)

source("functions/functions.R")
source("functions/fixed_window.R")
source("functions/expanding_window.R")


load("data/rawdata.RData")
df <- dados

# call_mrf fixed window ####

model_name = "MRF_exp_12lags"

model_list = list()

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


# salvando os forecasts de expanding window

forecast_list <- list()

for ( i in 1:2){
  
  forecast_list[[i]] <- list()
  
  for ( j in 1:oos.pos/12){
forecasts <-  Reduce(cbind,lapply(model_list, function(x)x[[i]][[j]]$pred))

forecast_list[[i]][[j]] <- forecasts
    }
}

model_list[[2]][[2]]$pred





# finalizando as previsoes ####


# o accumulate_model calcula as diagonais, sendo assim, os valores de previsao de 3 e 6 meses e adiciona as colunas referetnes
forecasts_4lags = accumulate_model(forecasts)

View(forecasts)

save(forecasts_4lags,file = paste("forecasts/",model_name,".rda",sep = ""))


plot(tail(df[,"CPI"],312),type = "l")
lines(forecasts[,1],col = 5)
