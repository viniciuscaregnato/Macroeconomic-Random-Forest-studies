load("rawdata.RData")
df <- dados


# 1. ST DATA ####

st_data <- function(df, n_y_lags = 8, n_lags_of_each_var = 2,
                    n_lags_of_5_factors = 8,
                    n_lags_j=8, n_maf_components = 2){
  
  df <- as.data.frame(df)
  yt <- df[,1]
  xt <- df[,2:ncol(df)]
  
  dates <- row.names(df)
  
  # 1. lags of yt ####
  
  y_lags <- embed(yt,(n_y_lags+1))
  y_lags <- y_lags[,2:(n_y_lags+1)]
  lags_names <- paste0("y_lag", 1:n_y_lags)
  colnames(y_lags) <- lags_names
  dates_1 <- dates[-(1:n_y_lags)]
  row.names(y_lags) <- dates_1
  
  y_lags <- as.data.frame(y_lags)
  
  # 2. Two lags of each variable in FRED-MD ####
  
  
  lista_lags <- lapply(names(xt), function(nome){
    
    matriz <- embed(xt[[nome]], n_lags_of_each_var+1)
    matriz <- matriz[,2:(n_lags_of_each_var+1)]
    
    var_lags_names <- character()
    for (i in 1:n_lags_of_each_var){
      var_lags_names[i] <- paste0(nome, "_lag", i)
    }
    colnames(matriz) <- var_lags_names
    
    return(matriz)
  })
  
  xt_lags <- as.data.frame(do.call(cbind, lista_lags))
  
  dates_2 <- dates[-(1:n_lags_of_each_var)]
  row.names(xt_lags) <- dates_2
  
  
  
  # 3. Eight lags of 5 traditional factors F ####
  
  pca_result <- prcomp(xt, center = TRUE, scale. = TRUE)
  
  pca5_result <- pca_result$x[, 1:5]
  
  pca5_lags <- embed(pca5_result, n_lags_of_5_factors+1)
  pca5_lags <- pca5_lags[,6:ncol(pca5_lags)]
  
  pca5_lags <- as.data.frame(pca5_lags)
  
  row.names(pca5_lags) <- dates[-(1:n_lags_of_5_factors)]
  
  names_list <- list()
  for (i in 1:n_lags_of_5_factors){
    for (j in 1:5){
      names_list <- c(names_list, paste0("PCA",j,"_",i,"lag"))
    }
  }
  
  colnames(pca5_lags) <- names_list
  
  # 4. two MAFS ####
  
  maf_list <- list()
  for (var_name in colnames(df)) {
    
    serie_j <- df[[var_name]]
    
    embedded_j <- embed(serie_j, n_lags_j + 1)
    lags_j_matrix <- embedded_j[, -1] 
    
    dates_2 <- dates[-(1:n_lags_j)]
    row.names(lags_j_matrix) <- dates_2
    
    pca_j <- prcomp(lags_j_matrix, center = TRUE, scale. = TRUE)
    mafs_j <- pca_j$x[, 1:n_maf_components]
    
    maf_names <- character()
    for (i in 1:n_maf_components){
      maf_names[i] <- paste0(var_name, "_MAF", i)
    }
    
    colnames(mafs_j) <- maf_names
    
    maf_list[[var_name]] <- mafs_j
  }
  
  mafs_df <- do.call(cbind, maf_list)
  
  mafs_df <- as.data.frame(mafs_df)
  
  # 5. UNINDO TUDO e adding t
  
  dfs <- list(y_lags, xt_lags, pca5_lags, mafs_df)
  
  N <- min(sapply(dfs, nrow)) 
  dfs_alinhados <- lapply(dfs, tail, N)
  
  df_final <- do.call(cbind, dfs_alinhados)
  df_final$t <- c(1:nrow(df_final))
  
  return(df_final)
  
  # UNINDO TUDO e adicionando t 
  
  dfs <- list(y_lags, xt_lags, pca5_lags, mafs_df)
  
  N <- min(sapply(dfs, nrow)) 
  dfs_alinhados <- lapply(dfs, tail, N)
  
  df_final <- do.call(cbind, dfs_alinhados)
  df_final$t <- c(1:nrow(df_final))
  
  return(df_final)
  
}


# 2. FAARRF ####

FAARRF_data <- function(df){
  
  df <- as.data.frame(df)
  yt <- df[,1]
  xt <- df[,-1]
  
  dates <- row.names(df)
  
  #1. 2 lags de y ####
  
  y_2lags <- embed(yt,3)
  y_2lags <- y_2lags[,-1]
  
  
  dates_1 <- dates[-(1:2)]
  row.names(y_2lags) <- dates_1
  colnames(y_2lags) <- c("Yt-1", "Yt-2")
  
  #2. lag de PC1 e PC2 ####
  
  pca <- prcomp(df, center = TRUE, scale. = TRUE)
  pca2 <- pca$x[,1:2]
  
  pca2_lags <- embed(pca2, 2)
  pca2_lags <- pca2_lags[,-(1:2)]
  
  dates_2 <- dates[-1]
  row.names(pca2_lags) <- dates_2
  colnames(pca2_lags) <- c("PC1_lag1", "PC2_lag1")
  
  # UNINDO ####
  
  dfs <- list(y_2lags, pca2_lags)
  
  N <- min(sapply(dfs, nrow)) 
  dfs_alinhados <- lapply(dfs, tail, N)
  
  df_final <- do.call(cbind, dfs_alinhados)
  
  return(df_final)
  
}


# 3. CALL_MRF ####
call_mrf <- function(df, n_y_lags = 8, n_lags_of_each_var = 2,
                     n_lags_of_5_factors = 8,
                     n_lags_j=8, n_maf_components = 2,
                     lin.model = "none"){
  
  if (lin.model == "FAARRF"){
    FAARRF_data_called <- FAARRF_data(df = df)
  }
  
  
  
  st_data_called <- st_data(df = df,n_y_lags = n_y_lags, n_lags_of_each_var = n_lags_of_each_var,
                            n_lags_of_5_factors = n_lags_of_5_factors,
                            n_lags_j=n_lags_j, n_maf_components=n_maf_components)
  
  # UNINDO ####
  
  dfs <- list(FAARRF_data_called, st_data_called)
  
  N <- min(sapply(dfs, nrow)) 
  dfs_alinhados <- lapply(dfs, tail, N)
  
  df_final <- do.call(cbind, dfs_alinhados)
  
  return(df_final)
  
}



