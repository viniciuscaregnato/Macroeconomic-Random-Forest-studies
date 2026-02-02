library(MacroRF)

source("functions/functions.R")
source("functions/fixed_window.R")

load("data/rawdata.RData")
df <- dados

# call_mrf ####

model_name = "MRF0202"

# FAARRF 12 lags ####
n_y_lags = 12
n_lags_of_each_var = 2 
n_lags_of_5_factors = 12
n_lags_j = 12
n_maf_components = 2 
lin.model = "FAARRF"


# FAARRF 8 lags ####
n_y_lags = 8
n_lags_of_each_var = 2 
n_lags_of_5_factors = 8
n_lags_j = 8
n_maf_components = 2 
lin.model = "FAARRF"


model_list = list()

for (i in c(1:12)){
  cat("horizonte ", i,"\n")
  model = fixed_window(df=df, lin.model = "FAARRF", horizon = i,n_y_lags = 12,
                       n_lags_of_each_var = 2 ,
                       n_lags_of_5_factors = 12,
                       n_lags_j = 12,
                       n_maf_components = 2)
  model_list[[i]] = model
}


forecasts <-  Reduce(cbind,lapply(model_list, function(x)x$pred))



# o accumulate_model calcula as diagonais, sendo assim, os valores de previsao de 3 e 6 meses e adiciona as colunas referetnes
forecasts_4lags = accumulate_model(forecasts)

View(forecasts)

save(forecasts_4lags,file = paste("forecasts/",model_name,".rda",sep = ""))


plot(tail(df[,"CPI"],312),type = "l")
lines(forecasts[,1],col = 5)
