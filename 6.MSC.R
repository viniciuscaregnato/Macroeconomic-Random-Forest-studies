install.packages("MCS")
library(MCS)

model_files = setdiff(list.files("forecasts/"),c("rw.rda","yout.rda"))
load("forecasts/yout.rda")

models_list = list()
for (i in 1:length(model_files)){
  load(paste0("forecasts/",model_files[i]))
  models_list[[i]] = forecasts_4lags
}

horizons = c(1,3,6,12)
errors_list = lapply(horizons, function(h){
  lapply(models_list, function(x)(x[,h]-yout[,1])^2)
})



errors <- lapply(1:length(horizons), function(i) {
  result <- Reduce(cbind, errors_list[[i]])
  colnames(result) <- model_files
  return(result)
})
errors_h1 = errors[[1]]
errors_h3 = errors[[2]]
errors_h6 = errors[[3]]
errors_h12 = errors[[4]]

View(errors)

View(errors_h1)


# aplicando MSC h1

MCSprocedure(errors_h1, alpha = 0.15, B = 5000, cl = NULL,
             ram.allocation = TRUE, statistic = "TR", k = NULL, min.k = 3,
             verbose = TRUE)


# aplicando MSC h3

MCSprocedure(errors_h3, alpha = 0.15, B = 5000, cl = NULL,
             ram.allocation = TRUE, statistic = "Tmax", k = NULL, min.k = 3,
             verbose = TRUE)


# aplicando MSC h6

MCSprocedure(errors_h6, alpha = 0.15, B = 5000, cl = NULL,
             ram.allocation = TRUE, statistic = "Tmax", k = NULL, min.k = 3,
             verbose = TRUE)


# aplicando MSC h12

MCSprocedure(errors_h12, alpha = 0.15, B = 5000, cl = NULL,
             ram.allocation = TRUE, statistic = "Tmax", k = NULL, min.k = 3,
             verbose = TRUE)





