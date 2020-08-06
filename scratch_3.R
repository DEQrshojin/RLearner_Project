data <- read.csv('C:/Users/rshojin/Desktop/RMS/003_deq/github/DEMO/data/data.csv')

model <- summary(lm(data = data, LON ~ LAT))

pnts <- function(x) model[["coefficients"]][2, 1] * x + model[["coefficients"]][2, 1]

line <- data.frame(LON = c(-1, 7),
                   LAT = c(pnts(-1), pnts(7)),
                   SITE = c(NA, NA))

model <- list(data  = data,
              model = model,
              pnts  = pnts,
              line  = line)

saveRDS(object = model,
        file = 'C:/Users/rshojin/Desktop/RMS/003_deq/github/DEMO/data/model.RData')
		
rm(list = ls()); cat("\014")
