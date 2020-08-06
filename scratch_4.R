library(ggplot2)

rm(list = ls()); cat("\014")

model <- readRDS('C:/Users/rshojin/Desktop/RMS/003_deq/github/DEMO/data/model.RData')

# DO STUFF ----
data <- model[['data']]

line <- model[['line']]

model <- model[['model']]

format_R2 <- function(m = 1, b = 0, r2 = 1) {

  if (b < 0) {

    b = -b

    eq <- substitute(italic(y) == m %.% italic(x) - b * "," ~~ italic(r)^2 ~ "=" ~ r2,
                     list(b = format(b, digits = 2), m = format(m, digits = 2),
                          r2 = format(r2, digits = 3)))

  } else {

    eq <- substitute(italic(y) == m %.% italic(x) + b * "," ~~ italic(r)^2 ~ "=" ~ r2,
                     list(b = format(b, digits = 2), m = format(m, digits = 2),
                          r2 = format(r2, digits = 3)))

  }

  return(as.character(as.expression(eq)))

}

windows(12, 12)

lblL <- format_R2(m = unlist(model$coefficients[[2, 1]]),
                  b = unlist(model$coefficients[[2, 1]]),
                  r2 = model[["r.squared"]])

# NOW GRAPH ----
pl <- ggplot(data, aes(x = LON, y = LAT, label = SITE)) + geom_text(size = 20) +
      geom_line(data = line, aes(x = LON, y = LAT, label = SITE), size = 3) +
      annotate('text', x = 2, y = 6, parse = T, label = lblL, hjust = 0,
               size = 10) + theme_classic()

ggsave(pl, 'C:/Users/rshojin/Desktop/RMS/003_deq/github/DEMO/data/plot.png',
       width = 11, height = 8.5, units = 'in', dpi = 300)


