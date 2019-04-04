# Generate plot of Heat Source model calculated Effective Shade
# Middle Fork Coquille River
# Jim Bloom - Nov 2017
# VERSION FOR BASE GRAPHICS EXAMPLE - R USERS/LEARNERS GROUPS

#"The flexibility of base R comes at a price, but it means you can make all sorts of things
# you need to without struggling against the system. Which is a huge advantage for data analysts.
# There are some graphs that are pretty straightforward in base, but require quite a bit of work 
# in ggplot2."
# 
#"When making graphs that are production ready or fit for publication, you can do this with any 
# system. You can do it with ggplot2, with lattice, with base R graphics. But regardless of 
# which system you use it will require about an equal amount of code to make a graph ready for 
# publication."

#install.packages(c("chron", "hydroGOF", "zoo", "TTR", "car"),  dep=TRUE) 
# 
library(chron)
library(hydroGOF)
library(zoo)
library(TTR)
library(car) # JRB - I added this just for some() function
R.Version()

##################################################################################
#####
##### PROCESS HEAT SOURCE MODEL OUTPUT FOR A CURRENT CONDITION SCENARIO AND  #####
##### A SITE POTENTIAL SCENARIO AND GENERATE PLOTS AND TABLES OF DATA USEFUL #####
##### FOR A TMDL TO ADDRESS TEMPERATURE CONCERNS                             #####

##### APPLICATION TO MIDDLE FORK COQUILLE MODEL

##### Oct 10 2018 repeat to process Jul 15 to Aug 15 sims with zero cloudiness

inpathSim <- "//Deqhq1/tmdl/Library (weekly backup)/Models_Software/R/Script Library/BaseGraphicsExamples/HeatSource/Simulations"

yearSim <- 2003

##### Process Jul 15 to Aug 15 sims with zero cloudiness
simnoA <- "808F_L13CCC_7Q10M_ZeroCloud_Jul16toAug15"
simnoB <- "808F_L13SPV_7Q10M_ZeroCloud_Jul16toAug15"

#Paths to locations to place plots and tables
outpathA <- paste(inpathSim, "/", simnoA, "/", sep = ""); outpathA #Plot location for SimA single scenario plots
outpathB <- paste(inpathSim, "/", simnoB, "/", sep = ""); outpathB #Plot location for SimB plots, LA and comparison plots

#TITLES - REVISE TO MATCH SCENARIOS
TitleDate <- "Jul 16 to Aug 15 2003"
#TitleDate <- "Jul 16 to Sep 8 2003"
TitleCloud <- "Zero Cloudiness"
#TitleCloud <- "Observed Cloudiness"
TitleCCC <- "CCC Veg"
#TitleCCC <- "SPV Veg" #Note that even though TitleCCC, in this case treating an SPV veg scenario as simnoA 
TitleSPV <- "System Potential Vegetation"
#TitleSPV <- "CCC Veg and Reduced Trib and BC T"
#TitleSPV <- "Current Vegetation"
#TitleSPV <- "SPV Veg and Reduced Trib and BC T"
#TitleQ <- "7Q10 Low vs. 2003 Observed" #river flow
TitleQ <- "7Q10 Low" #river flow
#TitleQ <- "2003 Observed" #river flow

#READ IN AND PROCESS SHADE OUTPUT
simfile <- "Shade"

#CCC
paste(inpathSim, "/", simnoA, "/", simfile, ".txt", sep = "")
simShadeSIMA <- read.table(paste(inpathSim, "/", simnoA, "/", simfile, ".txt", sep = ""), 
                           header=TRUE, sep = "", strip.white = TRUE, skip=2)
simShadeSIMA
simShadeSIMA2 <- simShadeSIMA[-1] #Delete first column /(Datetime)
head(simShadeSIMA2, L=2)

#SPV or other simulation to be compared to SIM A
paste(inpathSim, "/", simnoB, "/", simfile, ".txt", sep = "")
simShadeSIMB <- read.table(paste(inpathSim, "/", simnoB, "/", simfile, ".txt", sep = ""), 
                           header=TRUE, sep = "", strip.white = TRUE, skip=2)
simShadeSIMB
simShadeSIMB2 <- simShadeSIMB[-1] #Delete first column /(Datetime)
head(simShadeSIMB2, L=2)

#Calculate MEAN EFFECTIVE SHADE 
ShadeMeanA <- sapply(simShadeSIMA2, mean)
ShadeMeanA
ShadeMeanAPct <- 100 * ShadeMeanA

ShadeMeanB <- sapply(simShadeSIMB2, mean)
ShadeMeanB
ShadeMeanBPct <- 100 * ShadeMeanB

#MAKE A VECTOR OF STREAM KM FROM THE HEADER ROW
sim.kmA <- read.table(paste(inpathSim, "/", simnoA, "/", simfile, ".txt", sep = ""), 
                      header=TRUE, sep = "", skip=2, check.names=FALSE)
head(sim.kmA, n=2L)
sim.kmA <- as.numeric(colnames(sim.kmA[-1]))
sim.kmA
sim.km <- sim.kmA

#MAKE A VECTOR OF STREAM KM FROM THE HEADER ROW FOR SPV
sim.kmB <- read.table(paste(inpathSim, "/", simnoB, "/", simfile, ".txt", sep = ""), 
                      header=TRUE, sep = "", skip=2, check.names=FALSE)
head(sim.kmB, n=2L)
sim.kmB <- as.numeric(colnames(sim.kmB[-1]))
sim.kmB

#SEGMENT LENGTH - 100 m for MF Coquille (vs. 200 m for SF Coquille)
segLengthKm <- sim.km[1] - sim.km[2];
segLengthm <- 1000 * segLengthKm; segLengthm 

#PLOT EFFECTIVE SHADE
windowsFonts(A=windowsFont("Calibri"))
#SIMA
#BaseGrafX
png(file=paste(outpathA,simnoA,"_","Shade.png",sep=""), width = 1000, height = 400)
plot(sim.kmA, ShadeMeanAPct, xlim=c(60,0), ylim=c(0,100), type="l", 
     lty=1, lwd=2, col="blue", xlab =" ", ylab=" ", las = 1, frame = FALSE, 
     cex.lab = 1.20, cex.axis = 1.20) #, xaxt = "n" )
plot(sim.kmA, ShadeMeanAPct, xlim=c(60,0), ylim=c(0,100), type="l", 
     lty=1, lwd=2, col="blue", xlab =" ", ylab=" ", las = 1, frame = TRUE, 
     cex.lab = 1.20, cex.axis = 1.20) #, xaxt = "n" )
?plot #BaseGrafX
?par  #BaseGrafX
grid(col = "gray", lty = "dotted", lwd = 1)
text(60, 0.0,labels = paste("Sim", simnoA),cex = 1.10, adj=c(0,0))
mtext("River Kilometer", side = 1, line=3, cex=1.20,outer=FALSE)
mtext("Effective Shade (%)", side = 2, line=3, cex=1.20,outer=FALSE)
mtext(paste("Average Model Calculated Effective Shade - ", TitleDate), side = 3, line=2, cex=1.5, outer=FALSE)
dev.off()

#SIMB
png(file=paste(outpathB,simnoB,"_","Shade.png",sep=""), width = 1000, height = 400)
plot(sim.kmB, ShadeMeanBPct, xlim=c(60,0), ylim=c(0,100), type="l", 
     lty=1, lwd=2, col="blue", xlab =" ", ylab=" ", las = 1, frame = FALSE, 
     cex.lab = 1.20, cex.axis = 1.20) #, xaxt = "n" )
grid(col = "gray", lty = "dotted", lwd = 1)
text(60, 0.0,labels = paste("Sim", simnoB),cex = 1.10, adj=c(0,0))
mtext("River Kilometer", side = 1, line=3, cex=1.20,outer=FALSE)
mtext("Effective Shade (%)", side = 2, line=3, cex=1.20,outer=FALSE)
mtext(paste("Average Model Calculated Effective Shade - ", TitleDate), side = 3, line=2, cex=1.5, outer=FALSE)
dev.off()

#BaseGrafX
#SIMA AND SIMB ON SAME PLOT
png(file=paste(outpathB,simnoB,"_","Shade_Comparison.png",sep=""), width = 1000, height = 400)
plot(sim.kmA, ShadeMeanAPct, xlim=c(60,0), ylim=c(-5.0,100.0), type="l", 
     lty=1, lwd=2, col="red", xlab =" ", ylab=" ", las = 1, frame = FALSE, 
     cex.lab = 1.20, cex.axis = 1.20) #, xaxt = "n" )
grid(col = "gray", lty = "dotted", lwd = 1)
lines(sim.kmB, ShadeMeanBPct, type="l", lty=1, lwd=2, col="blue" )
text(60, 0.0,labels = paste("Sim:", simnoB),cex = 1.10, adj=c(0,0))
text(60, -5.5,labels = paste("Sim:", simnoA),cex = 1.10, adj=c(0,0))
mtext("River Kilometer", side = 1, line=3, cex=1.20,outer=FALSE)
mtext("Effective Shade (%)", side = 2, line=3, cex=1.20,outer=FALSE)
mtext(paste("Average Model Calculated Effective Shade - ", TitleDate), side = 3, line=2, cex=1.5, outer=FALSE)
legend(x=12.0, y=8., bty="n", legend = "Site Potential Vegetation", lty=1, lwd=2, col="blue")
legend(x=12.0, y=3., bty="n", legend = "Current Shade", lty=1, lwd=2, col="red")
dev.off()


#READ IN MODEL CALCULATED TEMPERATURE AND DERIVE 7DADM TEMPERATURES
simfile <- "Temp_H2O" #Celsius - every hour every 0.1 km

simTSIMA <- read.table(paste(inpathSim,"/", simnoA, "/", simfile, ".txt", sep = ""), 
                       header=TRUE, sep = "", strip.white = TRUE, skip=2)
head(simTSIMA, n=2L)
simTSIMA2 <- simTSIMA[-1]; simTSIMA2 #Delete first column (Datetime)

#MAKE A VECTOR OF STREAM KM FROM THE HEADER ROW
simT.kmA <- read.table(paste(inpathSim, "/", simnoA, "/", simfile, ".txt", sep = ""), 
                       header=TRUE, sep = "", skip=2, check.names=FALSE)
simT.kmA <- as.numeric(colnames(simT.kmA[-1]))
simT.kmA

simTSIMB <- read.table(paste(inpathSim,"/", simnoB, "/", simfile, ".txt", sep = ""), 
                       header=TRUE, sep = "", strip.white = TRUE, skip=2)
head(simTSIMB, n=2L)
simTSIMB2 <- simTSIMB[-1]; simTSIMB2 #Delete 1st column 

#MAKE A VECTOR OF STREAM KM FROM THE HEADER ROW
simT.kmB <- read.table(paste(inpathSim, "/", simnoB, "/", simfile, ".txt", sep = ""), 
                       header=TRUE, sep = "", skip=2, check.names=FALSE)
simT.kmB <- as.numeric(colnames(simT.kmB[-1]))
simT.kmB

#Criteria
# u/s from RKm 18.45 T criteria is 16.0 C
# d/s from RKm 18.45 T criteria is 18.0 C
Tcrit.kmA <- simT.kmA 
#for(i in 1:length(simT.kmA)) { 
#  if (simT.kmA[i]>18.4){
#    Tcrit.kmA[i] = 16.0
#  }
#  else
#    Tcrit.kmA[i] = 18.0
#}  
#Avoid loop
Tcrit.kmA <- ifelse(simT.kmA>18.4,16.0,18.0)
Tcrit.kmA

#DERIVE 7DADM T FOR ALL SEGMENTS AND DAYS AND TEMPORAL MAXIMUM, MINIMUM, AND AVERAGE 7DADM T FOR EACH SEGMENT
#ALSO DERIVE DAILY AVERAGE T FOR ALL SEGS AND DAYS AND TEMPORAL MAX, MIN, AND AVG DAILY AVG T FOR EACH SEGMENT

#Simulation A
simTSIMA.day.group <- as.integer(simTSIMA$Datetime); simTSIMA.day.group
simTSIMA$Date <-      as.integer(simTSIMA$Datetime); simTSIMA$Date
simT.d.x <- chron(tapply(simTSIMA$Datetime,simTSIMA$Date,min), origin = c(month = 12, day = 30, year = 1899)); simT.d.x
write.table(simT.d.x)
simT.dmean2 <- simT.d.x 
simT.7DADM2 <- simT.d.x 
#simT.DailyMax2 <- simT.d.x
#simT.DailyMin2 <- simT.d.x
#simT.DailyMean2 <- simT.d.x
simT.sdadmMax2 <- 0
simT.sdadmMin2 <- 0
simT.sdadmMean2 <- 0
simT.dmeanMax2 <- 0
simT.dmeanMin2 <- 0
simT.dmeanMean2 <- 0

#for(i in 1:length(simT.kmA))
i=1 #TEST FOR 1ST OUTPUT LOCATION, THEN LOOP THRU THE OTHERS
simT.ikm <- i + 1 # 1st column is date and time, so add 1
simT.dm <- tapply(simTSIMA[[simT.ikm]], simTSIMA.day.group, max); data.frame(simT.dm) #Daily Max T for each day for the model segment
simT.dmean <- tapply(simTSIMA[[simT.ikm]], simTSIMA.day.group, mean); data.frame(simT.dmean) #Daily Mean T for each day for the model segment
simT.dmeanMax <- max(simT.dmean, na.rm = TRUE); simT.dmeanMax #Max of Daily Avg Ts for the model segment
simT.dmeanMin <- min(simT.dmean, na.rm = TRUE); simT.dmeanMin #Min of Daily Avg Ts
simT.dmeanMean <- mean(simT.dmean, na.rm = TRUE); simT.dmeanMean #Mean of Daily Avg Ts
simT.sdadm <- SMA(tapply(simTSIMA[[simT.ikm]], simTSIMA.day.group, max),n=7); data.frame(simT.dm, simT.dmean, simT.sdadm) #7-d Avg Daily Max for each day for the segment
simT.sdadmMax <- max(simT.sdadm, na.rm = TRUE); simT.sdadmMax #Max of 7DADMTs
simT.sdadmMin <- min(simT.sdadm, na.rm = TRUE); simT.sdadmMin #Min of 7DADMTs
simT.sdadmMean <- mean(simT.sdadm, na.rm = TRUE); simT.sdadmMean #Mean of 7DADMTs
simT.dmean2 <- cbind(simT.dmean2, simT.dmean); simT.dmean2
simT.7DADM2 <- cbind(simT.7DADM2,simT.sdadm); data.frame(simT.7DADM2) 
simT.sdadmMax2 <- cbind(simT.sdadmMax2,simT.sdadmMax); simT.sdadmMax2
simT.sdadmMin2 <- cbind(simT.sdadmMin2,simT.sdadmMin); simT.sdadmMin2
simT.sdadmMean2 <- cbind(simT.sdadmMean2,simT.sdadmMean); simT.sdadmMean2
simT.dmeanMax2 <- cbind(simT.dmeanMax2,simT.dmeanMax); simT.dmeanMax2
simT.dmeanMin2 <- cbind(simT.dmeanMin2,simT.dmeanMin); simT.dmeanMin2
simT.dmeanMean2 <- cbind(simT.dmeanMean2,simT.dmeanMean); simT.dmeanMean2

for(i in 2:length(simT.kmA)) { 
  simT.ikm <- i + 1 # 1st column is date and time, so add 1
  simT.dm <- tapply(simTSIMA[[simT.ikm]], simTSIMA.day.group, max) #Daily Max T for each day for the model segment
  simT.dmean <- tapply(simTSIMA[[simT.ikm]], simTSIMA.day.group, mean); data.frame(simT.dmean) #Daily Mean T for each day for the model segment
  simT.dmeanMax <- max(simT.dmean, na.rm = TRUE); simT.dmeanMax #Max of Daily Avg Ts for the model segment
  simT.dmeanMin <- min(simT.dmean, na.rm = TRUE); simT.dmeanMin #Min of Daily Avg Ts
  simT.dmeanMean <- mean(simT.dmean, na.rm = TRUE); simT.dmeanMean #Mean of Daily Avg Ts
  simT.sdadm <- SMA(tapply(simTSIMA[[simT.ikm]], simTSIMA.day.group, max),n=7) #7-d Avg Daily Max for each day for the segment
  simT.sdadmMax <- max(simT.sdadm, na.rm = TRUE) #Max of 7DADMTs
  simT.sdadmMin <- min(simT.sdadm, na.rm = TRUE) #Min
  simT.sdadmMean <- mean(simT.sdadm, na.rm = TRUE) #Mean
  simT.dmean2 <- cbind(simT.dmean2, simT.dmean); simT.dmean2
  simT.7DADM2 <- cbind(simT.7DADM2,simT.sdadm) 
  simT.sdadmMax2 <- cbind(simT.sdadmMax2,simT.sdadmMax)
  simT.sdadmMin2 <- cbind(simT.sdadmMin2,simT.sdadmMin)
  simT.sdadmMean2 <- cbind(simT.sdadmMean2,simT.sdadmMean)
  simT.dmeanMax2 <- cbind(simT.dmeanMax2,simT.dmeanMax); simT.dmeanMax2
  simT.dmeanMin2 <- cbind(simT.dmeanMin2,simT.dmeanMin); simT.dmeanMin2
  simT.dmeanMean2 <- cbind(simT.dmeanMean2,simT.dmeanMean); simT.dmeanMean2
}

simTSIMA.7DADM2 <- simT.7DADM2
simTSIMA.dmean2 <- simT.dmean2

xxx <- c("Date",simT.kmA); xxx
xxx2 <- rbind(xxx, simTSIMA.7DADM2); head(xxx2)
write.table(xxx2,paste(outpathA,simnoA,"_7DADMT.csv",sep=""), sep=",", row.names = FALSE, col.names = FALSE) #output 7DADMTs
xxx2 <- rbind(xxx, simTSIMA.dmean2); head(xxx2)
write.table(xxx2,paste(outpathA,simnoA,"_DailyAvgT.csv",sep=""), sep=",", row.names = FALSE, col.names = FALSE) #output Daily average Ts

simTSIMA.dmeanMax2x <- simT.dmeanMax2[-1]
simTSIMA.dmeanMin2x <- simT.dmeanMin2[-1]
simTSIMA.dmeanMean2x <- simT.dmeanMean2[-1]
simTSIMA.sdadmMax2x <- simT.sdadmMax2[-1]
simTSIMA.sdadmMin2x <- simT.sdadmMin2[-1]
simTSIMA.sdadmMean2x <- simT.sdadmMean2[-1]
data.frame(simT.kmA,simTSIMA.dmeanMax2x, simTSIMA.dmeanMin2x, simTSIMA.dmeanMean2x, simTSIMA.sdadmMax2x, simTSIMA.sdadmMin2x, simTSIMA.sdadmMean2x)

#Simulation B
simTSIMB.day.group <- as.integer(simTSIMB$Datetime); simTSIMB.day.group
simTSIMB$Date <-      as.integer(simTSIMB$Datetime); simTSIMB$Date
simT.d.x <- chron(tapply(simTSIMB$Datetime,simTSIMB$Date,min), origin = c(month = 12, day = 30, year = 1899)); simT.d.x
simT.dmean2 <- simT.d.x 
simT.7DADM2 <- simT.d.x 
simT.sdadmMax2 <- 0
simT.sdadmMin2 <- 0
simT.sdadmMean2 <- 0
simT.dmeanMax2 <- 0
simT.dmeanMin2 <- 0
simT.dmeanMean2 <- 0

for(i in 1:length(simT.kmB)) { 
  simT.ikm <- i + 1 # 1st column is date and time, so add 1
  simT.dm <- tapply(simTSIMB[[simT.ikm]], simTSIMB.day.group, max) #Daily Max T for each day for the model segment
  simT.dmean <- tapply(simTSIMB[[simT.ikm]], simTSIMB.day.group, mean); data.frame(simT.dmean) #Daily Mean T for each day for the model segment
  simT.dmeanMax <- max(simT.dmean, na.rm = TRUE); simT.dmeanMax #Max of Daily Avg Ts for the model segment
  simT.dmeanMin <- min(simT.dmean, na.rm = TRUE); simT.dmeanMin #Min of Daily Avg Ts
  simT.dmeanMean <- mean(simT.dmean, na.rm = TRUE); simT.dmeanMean #Mean of Daily Avg Ts
  simT.sdadm <- SMA(tapply(simTSIMB[[simT.ikm]], simTSIMB.day.group, max),n=7) #7-d Avg Daily Max for each day for the segment
  simT.sdadmMax <- max(simT.sdadm, na.rm = TRUE) #Max of 7DADMTs
  simT.sdadmMin <- min(simT.sdadm, na.rm = TRUE) #Min
  simT.sdadmMean <- mean(simT.sdadm, na.rm = TRUE) #Mean
  #simT.DailyMax2 <- cbind(simT.DailyMax2,simT.dm) #Attach newly calculated values to previously calculated values
  #simT.DailyMin2 <- cbind(simT.DailyMin2,simT.dm) #Attach newly calculated values to previously calculated values
  #simT.DailyMean2 <- cbind(simT.DailyMean2,simT.dm) #Attach newly calculated values to previously calculated values
  simT.dmean2 <- cbind(simT.dmean2, simT.dmean); simT.dmean2
  simT.7DADM2 <- cbind(simT.7DADM2,simT.sdadm) 
  simT.sdadmMax2 <- cbind(simT.sdadmMax2,simT.sdadmMax)
  simT.sdadmMin2 <- cbind(simT.sdadmMin2,simT.sdadmMin)
  simT.sdadmMean2 <- cbind(simT.sdadmMean2,simT.sdadmMean)
  simT.dmeanMax2 <- cbind(simT.dmeanMax2,simT.dmeanMax)
  simT.dmeanMin2 <- cbind(simT.dmeanMin2,simT.dmeanMin)
  simT.dmeanMean2 <- cbind(simT.dmeanMean2,simT.dmeanMean)
}

simTSIMB.7DADM2 <- simT.7DADM2
simTSIMB.dmean2 <- simT.dmean2

xxx <- c("Date",simT.kmB); xxx
xxx2 <- rbind(xxx, simTSIMB.7DADM2); xxx2
write.table(xxx2,paste(outpathB,simnoB,"_7DADMT.csv",sep=""), sep=",", row.names = FALSE, col.names = FALSE)
xxx2 <- rbind(xxx, simTSIMB.dmean2); head(xxx2)
write.table(xxx2,paste(outpathB,simnoB,"_DailyAvgT.csv",sep=""), sep=",", row.names = FALSE, col.names = FALSE)

simTSIMB.dmeanMax2x <- simT.dmeanMax2[-1]
simTSIMB.dmeanMin2x <- simT.dmeanMin2[-1]
simTSIMB.dmeanMean2x <- simT.dmeanMean2[-1]
simTSIMB.sdadmMax2x <- simT.sdadmMax2[-1]
simTSIMB.sdadmMin2x <- simT.sdadmMin2[-1]
simTSIMB.sdadmMean2x <- simT.sdadmMean2[-1]
data.frame(simT.kmB,simTSIMB.dmeanMax2x, simTSIMB.dmeanMin2x, simTSIMB.dmeanMean2x, simTSIMB.sdadmMax2x, simTSIMB.sdadmMin2x, simTSIMB.sdadmMean2x)


#PLOT 7DADM TEMPERATURE - CCC - MEAN MIN MAX - WITH SHADING
#biologically-based numeric criteria for MF Coquille: HW to RKm 18.45: 16.0C; RKm 18.4 to mouth: 18.0C
RKm.Tcrit <- c(62.7,18.45,18.45,0.0)
Tcrit <- c(16.0,16.0,18.0,18.0)
png(file=paste(outpathA,simnoA,"_7DADMTMeanMaxMin.png",sep=""), width = 1000, height = 400)
plot(simT.kmA, simTSIMA.sdadmMean2x, xlim=c(60,0), ylim=c(14,30), type="l", 
     lty=1, lwd=2, col="black", xlab =" ", ylab=" ", las = 1, frame = FALSE, 
     main = paste("Current Vegetation for ", TitleQ,"River Flow - ", TitleCloud, " - ",TitleDate), 
     cex.lab = 1.20, cex.axis = 1.20, yaxp = c(14, 30, 8))
lines(simT.kmA, simTSIMA.sdadmMin2x, type="l", lty=1, lwd=1, col="gray50" )
lines(simT.kmA, simTSIMA.sdadmMax2x, type="l", lty=1, lwd=1, col="gray50" )
#BaseGrafX - polygon
polygon(c(simT.kmA, rev(simT.kmA)), c(simTSIMA.sdadmMax2x, rev(simTSIMA.sdadmMin2x)),
        density = 10, col = "gray50", border = NA)
#lines(RKm.Tcrit,Tcrit, type="l", lty=2, lwd=2, col="black") 
lines(simT.kmA,Tcrit.kmA, type="l", lty=2, lwd=2, col="black") #Alternative
#BaseGrafX - grid v abline
#grid(col = "gray", lty = "dotted", lwd = 1)
abline(h = 14, lty = "dotted", lwd=1, col="gray" )
abline(h = 16, lty = "dotted", lwd=1, col="gray" )
abline(h = 18, lty = "dotted", lwd=1, col="gray" )
abline(h = 20, lty = "dotted", lwd=1, col="gray" )
abline(h = 22, lty = "dotted", lwd=1, col="gray" )
abline(h = 24, lty = "dotted", lwd=1, col="gray" )
abline(h = 26, lty = "dotted", lwd=1, col="gray" )
abline(h = 28, lty = "dotted", lwd=1, col="gray" )
abline(h = 30, lty = "dotted", lwd=1, col="gray" )
abline(v = 0, lty = "dotted", lwd=1, col="gray" )
abline(v = 10, lty = "dotted", lwd=1, col="gray" )
abline(v = 20, lty = "dotted", lwd=1, col="gray" )
abline(v = 30, lty = "dotted", lwd=1, col="gray" )
abline(v = 40, lty = "dotted", lwd=1, col="gray" )
abline(v = 50, lty = "dotted", lwd=1, col="gray" )
abline(v = 60, lty = "dotted", lwd=1, col="gray" )
text(20, 14,labels = paste("Sim", simnoA),cex = 1.0, adj=c(0,0))
mtext("River Kilometer", side = 1, line=3, cex=1.20,outer=FALSE)
mtext("7DADM Temperature (C)", side = 2, line=3, cex=1.20,outer=FALSE)
mtext("7-day Average Daily Maximum Temperature (mean, min, max)",side = 3, line=2.5, cex=1.5, outer=FALSE)
legend(x=60.0, y=30.0, bty="n", legend = "Current Shade", lty=1, lwd=2, col="black")
legend(x=60.0, y=29.0, bty="n", legend = "Temperature Criteria", lty=2, lwd=2, col="black")
dev.off()

#PLOT 7DADM TEMPERATURE - SPV - MEAN MIN MAX - WITH SHADING
png(file=paste(outpathB,simnoB,"_7DADMTMeanMaxMin.png",sep=""), width = 1000, height = 400)
plot(simT.kmB, simTSIMB.sdadmMean2x, xlim=c(60,0), ylim=c(14,30), type="l", 
     lty=1, lwd=2, col="black", xlab =" ", ylab=" ", las = 1, frame = FALSE, 
     main = paste(TitleSPV," for ", TitleQ,"River Flow - ", TitleCloud, " - ",TitleDate), 
     cex.lab = 1.20, cex.axis = 1.20, yaxp = c(14, 30, 8))
lines(simT.kmB, simTSIMB.sdadmMax2x, type="l", lty=1, lwd=1, col="gray50" )
lines(simT.kmB, simTSIMB.sdadmMin2x, type="l", lty=1, lwd=1, col="gray50" )
polygon(c(simT.kmB, rev(simT.kmB)), c(simTSIMB.sdadmMax2x, rev(simTSIMB.sdadmMin2x)),
        density = 10, col = "gray50", border = NA)
lines(RKm.Tcrit,Tcrit, type="l", lty=2, lwd=2, col="black")
#grid(col = "gray", lty = "dotted", lwd = 1)
abline(h = 14, lty = "dotted", lwd=1, col="gray" )
abline(h = 16, lty = "dotted", lwd=1, col="gray" )
abline(h = 18, lty = "dotted", lwd=1, col="gray" )
abline(h = 20, lty = "dotted", lwd=1, col="gray" )
abline(h = 22, lty = "dotted", lwd=1, col="gray" )
abline(h = 24, lty = "dotted", lwd=1, col="gray" )
abline(h = 26, lty = "dotted", lwd=1, col="gray" )
abline(h = 28, lty = "dotted", lwd=1, col="gray" )
abline(h = 30, lty = "dotted", lwd=1, col="gray" )
abline(v = 0, lty = "dotted", lwd=1, col="gray" )
abline(v = 10, lty = "dotted", lwd=1, col="gray" )
abline(v = 20, lty = "dotted", lwd=1, col="gray" )
abline(v = 30, lty = "dotted", lwd=1, col="gray" )
abline(v = 40, lty = "dotted", lwd=1, col="gray" )
abline(v = 50, lty = "dotted", lwd=1, col="gray" )
abline(v = 60, lty = "dotted", lwd=1, col="gray" )
text(20, 14,labels = paste("Sim", simnoB),cex = 1.0, adj=c(0,0))
mtext("River Kilometer", side = 1, line=3, cex=1.20,outer=FALSE)
mtext("7DADM Temperature (C)", side = 2, line=3, cex=1.20,outer=FALSE)
mtext("7-day Average Daily Maximum Temperature (mean, min, max)",side = 3, line=2.5, cex=1.5, outer=FALSE)
legend(x=60.0, y=29.0, bty="n", legend = TitleSPV, lty=1, lwd=2, col="black")
legend(x=60.0, y=28.0, bty="n", legend = "Temperature Criteria", lty=2, lwd=2, col="black")
dev.off()

#BaseGrafX
#PLOT 7DADM TEMPERATURE - CCC AND SPV - MEAN MIN MAX - WITH SHADING
#biologically-based numeric criteria for SF Coquille: HW to RKm 30.1: 16.0C; RKm 30.1 to mouth: 18.0C
png(file=paste(outpathB,simnoB,"_7DADMTMeanMaxMin__SPVvCCC.png",sep=""), width = 1000, height = 400)
plot(simT.kmA, simTSIMA.sdadmMean2x, xlim=c(60,0), ylim=c(14,30), type="l", 
     lty=1, lwd=2, col="red", xlab =" ", ylab=" ", las = 1, frame = FALSE, 
     main = paste(TitleSPV," vs.", TitleCCC, "for ", TitleQ,"River Flow - ", TitleCloud, " - ",TitleDate), 
     cex.lab = 1.20, cex.axis = 1.20, yaxp = c(14, 30, 8))
lines(simT.kmA, simTSIMA.sdadmMin2x, type="l", lty=1, lwd=1, col="red" )
lines(simT.kmA, simTSIMA.sdadmMax2x, type="l", lty=1, lwd=1, col="red" )
polygon(c(simT.kmA, rev(simT.kmA)), c(simTSIMA.sdadmMax2x, rev(simTSIMA.sdadmMin2x)),
        density = 10, col = "red", border = NA)
lines(simT.kmB, simTSIMB.sdadmMean2x, type="l", lty=1, lwd=2, col="blue" )
lines(simT.kmB, simTSIMB.sdadmMax2x, type="l", lty=1, lwd=1, col="blue" )
lines(simT.kmB, simTSIMB.sdadmMin2x, type="l", lty=1, lwd=1, col="blue" )
polygon(c(simT.kmB, rev(simT.kmB)), c(simTSIMB.sdadmMax2x, rev(simTSIMB.sdadmMin2x)),
        density = 10, col = "blue", border = NA)
lines(RKm.Tcrit,Tcrit, type="l", lty=2, lwd=2, col="black")
#grid(col = "gray", lty = "dotted", lwd = 1)
abline(h = 14, lty = "dotted", lwd=1, col="gray" )
abline(h = 16, lty = "dotted", lwd=1, col="gray" )
abline(h = 18, lty = "dotted", lwd=1, col="gray" )
abline(h = 20, lty = "dotted", lwd=1, col="gray" )
abline(h = 22, lty = "dotted", lwd=1, col="gray" )
abline(h = 24, lty = "dotted", lwd=1, col="gray" )
abline(h = 26, lty = "dotted", lwd=1, col="gray" )
abline(h = 28, lty = "dotted", lwd=1, col="gray" )
abline(h = 30, lty = "dotted", lwd=1, col="gray" )
abline(v = 0, lty = "dotted", lwd=1, col="gray" )
abline(v = 10, lty = "dotted", lwd=1, col="gray" )
abline(v = 20, lty = "dotted", lwd=1, col="gray" )
abline(v = 30, lty = "dotted", lwd=1, col="gray" )
abline(v = 40, lty = "dotted", lwd=1, col="gray" )
abline(v = 50, lty = "dotted", lwd=1, col="gray" )
abline(v = 60, lty = "dotted", lwd=1, col="gray" )
text(20, 15,labels = paste("Sim", simnoA),cex = 1.0, adj=c(0,0))
text(20, 14,labels = paste("Sim", simnoB),cex = 1.0, adj=c(0,0))
mtext("River Kilometer", side = 1, line=3, cex=1.20,outer=FALSE)
mtext("7DADM Temperature (C)", side = 2, line=3, cex=1.20,outer=FALSE)
mtext("7-day Average Daily Maximum Temperature (mean, min, max)",side = 3, line=2.5, cex=1.5, outer=FALSE)
legend(x=60.0, y=30.0, bty="n", legend = TitleCCC, lty=1, lwd=2, col="red")
legend(x=60.0, y=29.0, bty="n", legend = TitleSPV, lty=1, lwd=2, col="blue")
legend(x=60.0, y=28.0, bty="n", legend = "Temperature Criteria", lty=2, lwd=2, col="black")
dev.off()
