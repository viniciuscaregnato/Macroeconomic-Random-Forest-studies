library(MacroRF)

source("functions/functions.R")

load("data/rawdata.RData")
df <- dados

# call_mrf ####

model_name = "MRF0102"

y.pos=1
x.pos=2:5
S.pos=6:ncol(df_adj)
oos.pos=(nrow(df_adj)-311):nrow(df_adj)
model_list = list()

lin.model = "FAARRF"
#horizon =2

fixed_window <- function(df, lin.model = "none", horizon){
  
  
  
  df_adj <- mrf_data(df = df, lin.model = lin.model, horizon = horizon)
  
  run <- MRF(data = df_adj, x.pos = x.pos, S.pos=6:ncol(df_adj), oos.pos=(nrow(df_adj)-311):nrow(df_adj),
             ridge.lambda=0.1, B=50)

    }

for (i in c(1:12)){
  cat("horizonte ", i,"\n")
model = fixed_window(df=df, lin.model = "FAARRF", horizon = i)
model_list[[i]] = model
}


forecasts <-  Reduce(cbind,lapply(model_list, function(x)x$pred))



# o accumulate_model calcula as diagonais, sendo assim, os valores de previsao de 3 e 6 meses e adiciona as colunas referetnes
forecasts_4lags = accumulate_model(forecasts)

View(forecasts)

save(forecasts_4lags,file = paste("forecasts/",model_name,".rda",sep = ""))


plot(tail(df[,"CPI"],312),type = "l")
lines(forecasts[,1],col = 5)
