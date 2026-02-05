expanding_window <- function(df, lin.model,
                         n_y_lags,
                         n_lags_of_each_var,
                         n_lags_of_5_factors,
                         n_lags_j, n_maf_components,
                         horizon,
                         oos.pos){
  
  
  df_adj <- mrf_data(df = df, lin.model = lin.model,
                     n_y_lags = n_y_lags,
                     n_lags_of_each_var = n_lags_of_each_var,
                     n_lags_of_5_factors = n_lags_of_5_factors,
                     n_lags_j=n_lags_j,
                     n_maf_components = n_maf_components,
                     horizon = horizon)
  
  if (lin.model == "FAARRF"){
    y.pos=1
    x.pos=2:5
    S.pos=6:ncol(df_adj)
    
    
  }
  
  n_windows <- (oos.pos/12)
  
  if(oos.pos %% 12 != 0){
    stop("o numero de observações out-of-sample devem ser múltiplo de 12")
  }
  
  results = list()
  k <- 1

  for (i in n_window:1){
    
    df_exp <- df_adj[1:(nrow(df_adj)-12*n_windows+12),]
  
  run <- MRF(data = df_exp, y.pos=y.pos, x.pos = x.pos, S.pos=S.pos,
             oos.pos=(nrow(df_exp)-12+1):nrow(df_exp),
             ridge.lambda=0.1, B=50)
  
  # se quiser salvar só a previsao
  #results[[k]] <- run$pred

  # se quiser salvar todas as infos da arvore  
  results[[k]] <- run
  k <- k +1
  
  
  }
  
  return(results)
}


