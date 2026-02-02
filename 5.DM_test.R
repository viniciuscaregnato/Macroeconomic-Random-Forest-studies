library(forecast)

load("forecasts/yout.rda")
load("forecasts/rw.rda")

model_files = c(setdiff(list.files("forecasts/"), "yout.rda"))

models_list = list()
for(i in 1:(length(model_files)-1)){
  
  load(paste("forecasts/",model_files[i],sep = ""))
  models_list[[i]] = forecasts_4lags
  
}
models_list[[(length(models_list)+1)]] = rw

names(models_list) = model_files
View(models_list)


errors_list_h1 = Reduce(cbind,lapply(models_list, function(x){yout[,1] - x[,1]}))
colnames(errors_list_h1) = model_files

errors_list_h3 = Reduce(cbind,lapply(models_list, function(x){yout[,1] - x[,3]}))
colnames(errors_list_h3) = model_files

errors_list_h6 = Reduce(cbind,lapply(models_list, function(x){yout[,1] - x[,6]}))
colnames(errors_list_h6) = model_files

errors_list_h12 = Reduce(cbind,lapply(models_list, function(x){yout[,1] - x[,12]}))
colnames(errors_list_h12) = model_files

View(errors_list_h1)
View(errors_list_h3)
View(errors_list_h6)
View(errors_list_h12)




# RW x AR ####
#h=1
dm.test(errors_list_h1[,8], errors_list_h1[,1], h = 1, power = 2, alternative = "two.sided")

#h=3
dm.test(errors_list_h3[,8], errors_list_h3[,1], h = 3, power = 2, alternative = "two.sided")

#h=6
dm.test(errors_list_h6[,8], errors_list_h6[,1], h = 6, power = 2, alternative = "two.sided")

#h=12
dm.test(errors_list_h12[,8], errors_list_h12[,1], h = 12, power = 2, alternative = "two.sided")

# RW x CWB ####
#h=1
dm.test(errors_list_h1[,8], errors_list_h1[,2], h = 1, power = 2, alternative = "two.sided")

#h=3
dm.test(errors_list_h3[,8], errors_list_h3[,2], h = 3, power = 2, alternative = "two.sided")

#h=6
dm.test(errors_list_h6[,8], errors_list_h6[,2], h = 6, power = 2, alternative = "two.sided")

#h=12
dm.test(errors_list_h12[,8], errors_list_h12[,2], h = 12, power = 2, alternative = "two.sided")

# RW x CWB AR ####
#h=1
dm.test(errors_list_h1[,8], errors_list_h1[,3], h = 1, power = 2, alternative = "two.sided")

#h=3
dm.test(errors_list_h3[,8], errors_list_h3[,3], h = 3, power = 2, alternative = "two.sided")

#h=6
dm.test(errors_list_h6[,8], errors_list_h6[,3], h = 6, power = 2, alternative = "two.sided")

#h=12
dm.test(errors_list_h12[,8], errors_list_h12[,3], h = 12, power = 2, alternative = "two.sided")




# RW x ELASTIC NET ####
#h=1
dm.test(errors_list_h1[,8], errors_list_h1[,4], h = 1, power = 2, alternative = "two.sided")

#h=3
dm.test(errors_list_h3[,8], errors_list_h3[,4], h = 3, power = 2, alternative = "two.sided")

#h=6
dm.test(errors_list_h6[,8], errors_list_h6[,4], h = 6, power = 2, alternative = "two.sided")

#h=12
dm.test(errors_list_h12[,8], errors_list_h12[,4], h = 12, power = 2, alternative = "two.sided")

# RW x LASSO ####
#h=1
dm.test(errors_list_h1[,8], errors_list_h1[,5], h = 1, power = 2, alternative = "two.sided")

#h=3
dm.test(errors_list_h3[,8], errors_list_h3[,5], h = 3, power = 2, alternative = "two.sided")

#h=6
dm.test(errors_list_h6[,8], errors_list_h6[,5], h = 6, power = 2, alternative = "two.sided")

#h=12
dm.test(errors_list_h12[,8], errors_list_h12[,5], h = 12, power = 2, alternative = "two.sided")

# RW x RF ####
#h=1
dm.test(errors_list_h1[,8], errors_list_h1[,6], h = 1, power = 2, alternative = "two.sided")

#h=3
dm.test(errors_list_h3[,8], errors_list_h3[,6], h = 3, power = 2, alternative = "two.sided")

#h=6
dm.test(errors_list_h6[,8], errors_list_h6[,6], h = 6, power = 2, alternative = "two.sided")

#h=12
dm.test(errors_list_h12[,8], errors_list_h12[,6], h = 12, power = 2, alternative = "two.sided")

# RW x RIDGE ####

#h=1
dm.test(errors_list_h1[,8], errors_list_h1[,7], h = 1, power = 2, alternative = "two.sided")

#h=3
dm.test(errors_list_h3[,8], errors_list_h3[,7], h = 3, power = 2, alternative = "two.sided")

#h=6
dm.test(errors_list_h6[,8], errors_list_h6[,7], h = 6, power = 2, alternative = "two.sided")

#h=12
dm.test(errors_list_h12[,8], errors_list_h12[,7], h = 12, power = 2, alternative = "two.sided")


# CWB X AR ####

# h=1
dm.test(errors_list_h1[,2], errors_list_h1[,1], h=1,  power = 2, alternative = "two.sided")

# h=3
dm.test(errors_list_h3[,2], errors_list_h3[,1], h=3,  power = 2, alternative = "two.sided")


# h=6
dm.test(errors_list_h6[,2], errors_list_h6[,1], h=6,  power = 2, alternative = "two.sided")

# h=12
dm.test(errors_list_h12[,2], errors_list_h12[,1], h=12,  power = 2, alternative = "two.sided")

# CWB x CWB AR ####

# h=1
dm.test(errors_list_h1[,2], errors_list_h1[,3], h=1,  power = 2, alternative = "two.sided")


# h=3
dm.test(errors_list_h3[,2], errors_list_h3[,3], h=3,  power = 2, alternative = "two.sided")


# h=6
dm.test(errors_list_h6[,2], errors_list_h6[,3], h=6,  power = 2, alternative = "two.sided")

# h=12
dm.test(errors_list_h12[,2], errors_list_h12[,3], h=12,  power = 2, alternative = "two.sided")


# CWB x ELASTIC NET ####

# h=1
dm.test(errors_list_h1[,2], errors_list_h1[,4], h=1,  power = 2, alternative = "two.sided")


# h=3
dm.test(errors_list_h3[,2], errors_list_h3[,4], h=3,  power = 2, alternative = "two.sided")


# h=6
dm.test(errors_list_h6[,2], errors_list_h6[,4], h=6,  power = 2, alternative = "two.sided")

# h=12
dm.test(errors_list_h12[,2], errors_list_h12[,4], h=12,  power = 2, alternative = "two.sided")



# CWB x LASSO ####

# h=1
dm.test(errors_list_h1[,2], errors_list_h1[,5], h=1,  power = 2, alternative = "two.sided")


# h=3
dm.test(errors_list_h3[,2], errors_list_h3[,5], h=3,  power = 2, alternative = "two.sided")


# h=6
dm.test(errors_list_h6[,2], errors_list_h6[,5], h=6,  power = 2, alternative = "two.sided")

# h=12
dm.test(errors_list_h12[,2], errors_list_h12[,5], h=12,  power = 2, alternative = "two.sided")



# CWB x RANDOM FOREST ####

# h=1
dm.test(errors_list_h1[,2], errors_list_h1[,6], h=1,  power = 2, alternative = "two.sided")

# h=3
dm.test(errors_list_h3[,2], errors_list_h3[,6], h=3,  power = 2, alternative = "two.sided")

# h=6
dm.test(errors_list_h6[,2], errors_list_h6[,6], h=6,  power = 2, alternative = "two.sided")

# h=12
dm.test(errors_list_h12[,2], errors_list_h12[,6], h=12,  power = 2, alternative = "two.sided")


# CWB x RIDGE ####

# h=1
dm.test(errors_list_h1[,2], errors_list_h1[,7], h=1,  power = 2, alternative = "two.sided")

# h=3
dm.test(errors_list_h3[,2], errors_list_h3[,7], h=3,  power = 2, alternative = "two.sided")

# h=6
dm.test(errors_list_h6[,2], errors_list_h6[,7], h=6,  power = 2, alternative = "two.sided")

# h=12
dm.test(errors_list_h12[,2], errors_list_h12[,7], h=12,  power = 2, alternative = "two.sided")








# RANDOM FOREST x AR ####

# h=1
dm.test(errors_list_h1[,6], errors_list_h1[,1], h=1,  power = 2, alternative = "two.sided")


# h=3
dm.test(errors_list_h3[,6], errors_list_h3[,1], h=3,  power = 2, alternative = "two.sided")

# h=6
dm.test(errors_list_h6[,6], errors_list_h6[,1], h=6,  power = 2, alternative = "two.sided")

# h=12
dm.test(errors_list_h12[,6], errors_list_h12[,1], h=12,  power = 2, alternative = "two.sided")

# RANDOM FOREST x CWB AR ####

# h=1
dm.test(errors_list_h1[,6], errors_list_h1[,3], h=1,  power = 2, alternative = "two.sided")


# h=3
dm.test(errors_list_h3[,6], errors_list_h3[,3], h=3,  power = 2, alternative = "two.sided")

# h=6
dm.test(errors_list_h6[,6], errors_list_h6[,3], h=6,  power = 2, alternative = "two.sided")

# h=12
dm.test(errors_list_h12[,6], errors_list_h12[,], h=12,  power = 2, alternative = "two.sided")

# RANDOM FOREST x ELASTIC NET ####

# h=1
dm.test(errors_list_h1[,6], errors_list_h1[,4], h=1,  power = 2, alternative = "two.sided")

# h=3
dm.test(errors_list_h3[,6], errors_list_h3[,4], h=3,  power = 2, alternative = "two.sided")

# h=6
dm.test(errors_list_h6[,6], errors_list_h6[,4], h=6,  power = 2, alternative = "two.sided")

# h=12
dm.test(errors_list_h12[,6], errors_list_h12[,4], h=12,  power = 2, alternative = "two.sided")

# RANDOM FOREST x LASSO ####

# h=1
dm.test(errors_list_h1[,6], errors_list_h1[,5], h=1,  power = 2, alternative = "two.sided")

# h=3
dm.test(errors_list_h3[,6], errors_list_h3[,5], h=3,  power = 2, alternative = "two.sided")

# h=6
dm.test(errors_list_h6[,6], errors_list_h6[,5], h=6,  power = 2, alternative = "two.sided")

# h=12
dm.test(errors_list_h12[,6], errors_list_h12[,5], h=12,  power = 2, alternative = "two.sided")



# RANDOM FOREST x RIDGE ####

# h=1
dm.test(errors_list_h1[,6], errors_list_h1[,7], h=1,  power = 2, alternative = "two.sided")

# h=3
dm.test(errors_list_h3[,6], errors_list_h3[,7], h=3,  power = 2, alternative = "two.sided")


# h=6
dm.test(errors_list_h6[,6], errors_list_h6[,7], h=6,  power = 2, alternative = "two.sided")

# h=12
dm.test(errors_list_h12[,6], errors_list_h12[,7], h=12,  power = 2, alternative = "two.sided")

