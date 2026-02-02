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
  
  results = list()
  k <- 1
  
  for (i in oos.pos:1){
    
    df_adj <- df_adj[1:(nrow(df_adj)-oos.pos+1),]
  
  run <- MRF(data = df_adj, y.pos=y.pos, x.pos = x.pos, S.pos=S.pos,
             oos.pos=nrow(df_adj),
             ridge.lambda=0.1, B=50)
  
  results[[k]] <- run
  k <- k +1
  }
  
}



