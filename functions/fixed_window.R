fixed_window <- function(df, lin.model,
                         n_y_lags,
                         n_lags_of_each_var,
                         n_lags_of_5_factors,
                         n_lags_j, n_maf_components,
                         horizon,
                         oos.pos){
  
  
  
  df_adj <- mrf_data(df, lin.model=lin.model,
                     n_y_lags=n_y_lags,
                     n_lags_of_each_var=n_lags_of_each_var,
                     n_lags_of_5_factors=n_lags_of_5_factors,
                     n_lags_j=n_lags_j,
                     n_maf_components=n_maf_components,
                     horizon=horizon)
  
  if (lin.model == "FAARRF"){
    y.pos=1
    x.pos=2:5
    S.pos=6:ncol(df_adj)
    oos.pos=(nrow(df_adj)-(oos.pos-1)):nrow(df_adj)
    
  }
  
  if (lin.model == "FAARRF_4lags"){
    y.pos=1
    x.pos=2:8
    S.pos=9:ncol(df_adj)
    oos.pos=(nrow(df_adj)-(oos.pos-1)):nrow(df_adj)
    
  }
  
  run <- MRF(data = df_adj, y.pos=y.pos, x.pos = x.pos, S.pos=S.pos,
             oos.pos=oos.pos,
             ridge.lambda=0.1, B=50)
  
  return(run)

    }



