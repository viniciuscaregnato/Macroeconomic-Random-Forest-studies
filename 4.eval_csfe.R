# organizando dados ####

load("forecasts/yout.rda")
load("forecasts/rw.rda")

model_files = setdiff(list.files("forecasts/"),c("rw.rda","yout.rda"))

models_list = list()
for(i in 1:length(model_files)){
  
  load(paste("forecasts/",model_files[i],sep = ""))
  models_list[[i]] = forecasts_4lags
  
}
names(models_list) = model_files

# CSFE h=1 ####
CSFE_h1 = Reduce(cbind,lapply(models_list, function(x){
  cumsum(((rw[,1]-yout[,1])^2)-((x[,1]-yout[,1])^2))
}))
colnames(CSFE_h1) = model_files

png("C:/Users/celia/Desktop/ECONOMIA UFRGS/1. INICIAÇÃO CIENTIFICA/2/SICMRF/CSFE_h=1.png",
    width=1200, height=800, res=150)

matplot(1:nrow(CSFE_h1),  CSFE_h1, type="l", col=1:length(model_files), lty=1,
        xlab="Forecast", ylab="CSFE_h1")

legend("topleft", legend=colnames(CSFE_h1), col=1:ncol(CSFE_h1),
       lty=1, cex=0.50, bty="n")

dev.off()

# CSFE h=3 ####
CSFE_h3 = Reduce(cbind, lapply(models_list, function(x){
  cumsum(((rw[,3]-yout[,1])^2)-((x[,3]-yout[,1])^2))
}))
colnames(CSFE_h3) = model_files

png("C:/Users/celia/Desktop/ECONOMIA UFRGS/1. INICIAÇÃO CIENTIFICA/2/SICMRF/CSFE_h=1.png",
    width=1200, height=800, res=150)

matplot(1:nrow(CSFE_h3),  CSFE_h3, type="l", col=1:length(model_files), lty=1,
        xlab="Forecast", ylab="CSFE_h3")

legend("topleft", legend=colnames(CSFE_h1), col=1:ncol(CSFE_h1),
       lty=1, cex=0.50, bty="n")

dev.off()

# CSFE h=6 ####
CSFE_h6 = Reduce(cbind, lapply(models_list, function(x){
  cumsum(((rw[,6]-yout[,1])^2)-((x[,6]-yout[,1])^2))
}))
colnames(CSFE_h6) = model_files

png("C:/Users/celia/Desktop/ECONOMIA UFRGS/1. INICIAÇÃO CIENTIFICA/R/algoritmo0.5/CSFE_results/CSFE_h6.png",
    width=1200, height=800, res=150)

matplot(1:nrow(CSFE_h6),  CSFE_h6, type="l", col=1:length(model_files), lty=1,
        xlab="Forecast", ylab="CSFE_h6")

legend("topleft", legend=colnames(CSFE_h1), col=1:ncol(CSFE_h1),
       lty=1, cex=0.50, bty="n")

dev.off()

# CSFE h=12 ####
CSFE_h12 = Reduce(cbind, lapply(models_list, function(x){
  cumsum(((rw[,12]-yout[,1])^2)-((x[,12]-yout[,1])^2))
}))
colnames(CSFE_h12) = model_files

png("C:/Users/celia/Desktop/ECONOMIA UFRGS/1. INICIAÇÃO CIENTIFICA/R/algoritmo0.5/CSFE_results/CSFE_h12.png",
    width=1200, height=800, res=150)

matplot(1:nrow(CSFE_h12),  CSFE_h12, type="l", col=1:length(model_files), lty=1,
        xlab="Forecast", ylab="CSFE_h12")

legend("topleft", legend=colnames(CSFE_h1), col=1:ncol(CSFE_h1),
       lty=1, cex=0.50, bty="n")

dev.off()

#salvando ####

save(CSFE_h1,file = "csfe_results/h1.rda")
save(CSFE_h3,file = "csfe_results/h3.rda")
save(CSFE_h6,file = "csfe_results/h6.rda")
save(CSFE_h12,file = "csfe_results/h12.rda")


# Para visualizar as curvas ####

load("csfe_results/h12.rda")
matplot(1:nrow(CSFE_h12),  CSFE_h12, type="l", col=1:length(model_files), lty=1,
        xlab="Forecast", ylab="CSFE_h12")
legend("topleft", legend=colnames(CSFE_h1), col=1:ncol(CSFE_h1),
       lty=1, cex=0.50, bty="n")
