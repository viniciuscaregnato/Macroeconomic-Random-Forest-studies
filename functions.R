data <- dados


st_data <- function(data, n_y_lags = 8, n_lags_of_factors = 8, n_lags_maf=8){
  
  data <- as.data.frame(data)
  yt <- data[,1]
  xt <- data[,2:ncol(data)]
  
  
  # 1. lags of yt ####
  
  y_lags <- embed(yt,(n_y_lags+1))
  y_lags <- y_lags[,2:(n_y_lags+1)]
  lags_names <- paste0("y_lag", 1:ncol(y_lags))
  colnames(y_lags) <- lags_names
  
  y_lags <- as.data.frame(y_lags)
  
  
  # 2. Two lags of each variable in FRED-MD ####
  
  
  lista_lags <- lapply(names(xt), function(nome) {
    
    matriz <- embed(xt[[nome]], 3)
    matriz <- matriz[,2:3]
    
    colnames(matriz) <- c(paste0(nome, "_lag1"), paste0(nome, "_lag2"))
    
    return(matriz)
  })
  
  xt_2_lags <- as.data.frame(do.call(cbind, lista_lags))
  
  # 3. Eight lags of 5 traditional factors F ####
  
  pca_result <- prcomp(xt, center = TRUE, scale. = TRUE)
  
  pca5_result <- pca_result$x[, 1:5]
  
  pca5_lags <- embed(pca5_result, n_lags_of_factors+1)
  pca5_lags <- pca5_lags[,6:ncol(pca5_lags)]
  
  pca5_lags <- as.data.frame(pca5_lags)
  
  names_list <- list()
  for (i in 1:n_lags_of_factors){
    for (j in 1:5){
      names_list <- c(names_list, paste0("PCA",j,"_",i,"lag"))
    }
  }
  
  colnames(pca5_lags) <- names_list
  
  # 4. Two MAFs for each variable J ####
  
  n_lags_maf <- n_lags_maf
  n_maf_components <- 2
  maf_list <- list()
  for (var_name in colnames(data)) {
    
    
    embedded_j <- embed(yt, n_lags_maf + 1)
    
    lags_j_matrix <- embedded_j[, -1] 
    
    pca_j <- prcomp(lags_j_matrix, center = TRUE, scale. = TRUE)
    
    mafs_j <- pca_j$x[, 1:n_maf_components]
    
    colnames(mafs_j) <- c(paste0(var_name, "_MAF1"), paste0(var_name, "_MAF2"))
    
    maf_list[[var_name]] <- mafs_j
  }
  
  mafs_df <- do.call(cbind, maf_list)
  
  mafs_df <- as.data.frame(mafs_df)
  
  
  
  
  # UNINDO TUDO ####
  
  #dataframe St
  
  
  dfs <- list(y_lags, xt_2_lags, pca5_lags, mafs_df)
  
  N <- min(sapply(dfs, nrow)) 
  dfs_alinhados <- lapply(dfs, tail, N)
  
  df_final <- do.call(cbind, dfs_alinhados)
  df_final$t <- c(1:nrow(df_final))
  
  return(df_final)
  
}
