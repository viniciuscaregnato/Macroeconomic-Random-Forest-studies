library(roll)
library(dplyr)

load("data/rawdata.RData")
data=dados
colnames(data)[1]="CPIAUCSL"

#dates = data$date
#data = data%>%select(-date)%>%as.matrix()
#rownames(data) = as.character(dates)

nwindows = 312

y = data[,"CPIAUCSL"]
y = cbind(y,roll_prod(1+y,3)-1,roll_prod(1+y,6)-1,roll_prod(1+y,12)-1)    # cria matriz para cada taxa de variação (variação mensal, trimestral, semestral e anual)
yout = tail(y,nwindows)                                                   # salva as ultumas nwindows linhas da mariz y (taxas de variação)

View(yout)
head(yout)

rw = matrix(NA,nwindows,12)
for(i in 1:12){                                                          # armazena uma sequencia de 180 valores de CPIAUCSL, 
  aux=data[(nrow(data)-nwindows-i+1):(nrow(data)-i),"CPIAUCSL"]          # porém se diranciando 1 unidade da ultima observação, a cada iteração
  rw[,i]=aux;                                                            # cada janela de 180 valores de y é armazenada na matriz rw
}                                                                        # qut maior o indica da coluna, mais distante a janela de 180 obs está do final



rw3 = embed(y[,2],4)                                                     
rw3 = tail(embed(y[,2],4)[,4],nwindows)

str(rw3)
View(rw3)

rw6 = tail(embed(y[,3],7)[,7],nwindows)                                  
rw12 = tail(embed(y[,4],13)[,13],nwindows)
rw = cbind(rw,rw3,rw6,rw12)
colnames(rw) = c(paste("t+",1:12,sep = ""),"acc3","acc6","acc12")        

View(rw)

save(yout,file = "forecasts_4lags/yout.rda")
save(rw,file = "forecasts_4lags/rw.rda")

save(yout,file = "forecasts_12lags/yout.rda")
save(rw,file = "forecasts_12lags/rw.rda")
