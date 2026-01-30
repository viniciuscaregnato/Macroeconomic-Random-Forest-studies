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
    
    # Extrair a série temporal da variável j
    # Usamos [[var_name]] para extrair como um vetor
    serie_j <- data[["CPI"]]
    
    # Criar a matriz de 8 defasagens (P=8)
    # Assim como para y_t, usamos embed() com n_lags + 1
    # Isso cria uma matriz onde cada linha é (y_t, y_t-1, ..., y_t-8)
    embedded_j <- embed(serie_j, n_lags_maf + 1)
    
    # 5. Isolar APENAS as defasagens
    # Removemos a primeira coluna (que é o lag 0, ou o valor atual)
    lags_j_matrix <- embedded_j[, -1] 
    
    # 6. Rodar o PCA sobre a matriz de 8 defasagens da variável j
    # O seu fredqd_limpo já foi tratado para NAs, então prcomp deve rodar sem erros.
    pca_j <- prcomp(lags_j_matrix, center = TRUE, scale. = TRUE)
    
    # 7. Extrair os 2 primeiros componentes (MAFs)
    # pca_j$x contém os componentes principais
    mafs_j <- pca_j$x[, 1:n_maf_components]
    
    # 8. Renomear as colunas para evitar conflitos e para identificação
    colnames(mafs_j) <- c(paste0(var_name, "_MAF1"), paste0(var_name, "_MAF2"))
    
    # 9. Adicionar o data frame de 2 colunas à nossa lista
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

st_mddata <- st_data(data = data, n_lags_maf = 12)

