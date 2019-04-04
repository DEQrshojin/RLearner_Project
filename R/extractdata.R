fildata <- "C:/Users/chartman/Documents/GitHub/RLearner_Project/R/HeatSourceCode.txt"
script <- readLines(fildata)
comments <- script[grep("^[[:blank:]]*#",script, value=FALSE)]
#x <- grep("^[[:blank:]]*#",script, value=FALSE)
#comments <- subset(script, grepl("plot",script[1]))
#testcom <- script[grepl("^#", script[1,])]


write.table(comments, file="HeatSource_cmnt.txt", append = FALSE)
