load("data/2025/rawdata.RData")
data<-dados


data_st <- function(data, )

# 1. Eight lags of 𝑦t ####
yt <- data[,1]
y_lags <- embed(yt,9)
y_lags <- y_lags[,2:9]
colnames(y_lags) <- c("y.lag1","y.lag2", "y.lag3", "y.lag4", "y.lag5", "y.lag6",
                    "y.lag7", "y.lag8")

y_lags <- as.data.frame(y_lags)



# 2. Two lags of each variable in FRED-MD ####
xt <- data[,2:ncol(data)]

xt <- as.data.frame(xt)

# 2.1. Criar uma lista onde cada elemento é a matriz de lags de uma variável
lista_lags <- lapply(names(xt), function(nome) {
  
  matriz <- embed(xt[[nome]], 3)
  matriz <- matriz[,2:3]
  
  
  # Criar nomes específicos para as colunas dessa variável
  colnames(matriz) <- c(paste0(nome, "_lag1"), paste0(nome, "_lag2"))
  
  return(matriz)
})

# 2.2. Combinar todas as matrizes da lista em um único dataframe
xt_2_lags <- as.data.frame(do.call(cbind, lista_lags))




# 3. Eight lags of 5 traditional factors F ####

# Executa o PCA
pca_result <- prcomp(xt, center = TRUE, scale. = TRUE)

# Extrai os 5 primeiros fatores (Componentes Principais)
fatores_5 <- pca_result$x[, 1:5]

pca5_8lags <- embed(fatores_5, 9)
pca5_8lags <- pca5_8lags[,6:ncol(pca5_8lags)]

pca5_8lags <- as.data.frame(pca5_8lags)

# Two MAFs for each variable J ####

n_lags_maf <- 8
n_maf_components <- 2
data <- as.data.frame(data)
maf_list <- list()
for (var_name in colnames(data)) {
  
  # Extrair a série temporal da variável j
  # Usamos [[var_name]] para extrair como um vetor
  serie_j <- data[[var_name]]
  
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

#dataframe Xt
yt <- yt[9:length(yt)]
xt <- xt[9:nrow(xt),]
cte <- rep(1, times = 663)

data_lin <- cbind(yt,xt)
data_lin_cte <- cbind(yt,cte,xt)

#dataframe St

y_lags
xt_2_lags <- xt_2_lags[7:nrow(xt_2_lags),]
pca5_8lags
mafs_df
t <- c(1:nrow(y_lags))


data_St <- cbind(y_lags, xt_2_lags, pca5_8lags, mafs_df, t)

final_data <- cbind(data_lin, data_St)

write.csv(data_lin, "data_lin.csv")
write.csv(data_St, "data_St.csv")
write.csv(final_data, "final_data_h0.csv")
write.csv(data_lin_cte, "data_lin_cte.csv")

# dataframe para h1
yt_h1 <- yt[2:663]
final_data_h1 <- final_data[1:nrow(final_data)-1,2:ncol(final_data)] 

data_h1_cte <- cbind(yt_h1,final_data_h1)

write.csv(data_h1, "data/MRF/data_h1.csv")
write.csv(data_h1_cte, "data/MRF/data_h1_cte.csv")

# CRIANDO DATA SET PARA FA-ARRF ####

load("data/2025/rawdata.RData")
data<-dados
colnames(data)[1]="CPIAUCSL"

yt <- data[,1]
yt_1_2 <- embed(yt,3)
colnames(yt_1_2) <- c("yt","yt-1", "yt-2")


xt <- data[,2:ncol(data)]
xt <- as.data.frame(xt)
pca_result <- prcomp(xt, center = TRUE, scale. = TRUE)
pca_1_2 <- pca_result$x[,1:2]
pca_1_2 <- pca_1_2[2:(nrow(pca_1_2)-1),]

FAARRF <- cbind(yt_1_2, pca_1_2)
FAARRF_st <- cbind(FAARRF[7:nrow(FAARRF),], data_St)

save(FAARRF_st, file = "data/MRF/FAARRF_st.RData")
write.csv(FAARRF, "data/MRF/FAARRF.csv")
write.csv(FAARRF_st, "data/MRF/FAARRF_st.csv")



