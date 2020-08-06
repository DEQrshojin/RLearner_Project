rm(list = ls()); cat("\014")

data <- read.csv('C:/Users/rshojin/Desktop/RMS/003_deq/github/Demo/data/data.csv')

saveRDS(object = data, file = 'C:/Users/rshojin/Desktop/RMS/003_deq/github/DEMO/data/data.RData')
