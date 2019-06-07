ipnw_climate_county_comparison <- function(var_year, var_commodity, var_damage, unit) {

library(plotrix)



setwd("/dmine/data/USDA/agmesh-scenarios/Idaho/summaries")
temp = list.files(pattern = "_summary")
myfiles = lapply(temp, read.csv, header = TRUE)
ziggy.df <- do.call(rbind , myfiles)

ID1 <- aggregate(.~countyfips + year, ziggy.df, sum)
ID1 <- aggregate(.~countyfips, ID1, mean)

setwd("/dmine/data/USDA/agmesh-scenarios/Washington/summaries")
temp = list.files(pattern = "_summary")
myfiles = lapply(temp, read.csv, header = TRUE)
ziggy.df <- do.call(rbind , myfiles)

WA1 <- aggregate(.~countyfips + year, ziggy.df, sum)
WA1 <- aggregate(.~countyfips, WA1, mean)

setwd("/dmine/data/USDA/agmesh-scenarios/Oregon/summaries")
temp = list.files(pattern = "_summary")
myfiles = lapply(temp, read.csv, header = TRUE)
ziggy.df <- do.call(rbind , myfiles)

OR1 <- aggregate(.~countyfips + year, ziggy.df, sum)
OR1 <- aggregate(.~countyfips, OR1, mean)

countiesfips <- read.csv("/dmine/data/counties/counties_fips.csv", header = TRUE)
colnames(countiesfips) <- c("countyfips", "county", "state")

OR2 <- merge(countiesfips, OR1, by=("countyfips"))
ID2 <- merge(countiesfips, ID1, by=("countyfips"))
WA2 <- merge(countiesfips, WA1, by=("countyfips"))


#------

setwd("/dmine/data/USDA/agmesh-scenarios/Allstates/")
#setwd(monthdir2)

temp = list.files(pattern = paste("Idaho", "_summary", sep=""))
myfiles = lapply(temp, read.csv, header = TRUE)
ziggy.df <- do.call(rbind , myfiles)
xrange <- as.data.frame(ziggy.df)
xrange$county <- as.character(xrange$county)
xrange$damagecause <- as.character(xrange$damagecause)

xrange_1989 <- subset(xrange, year <= 2000)
xrange_2015 <- subset(xrange, year >= 2001)
xrange_1989$loss <- xrange_1989$acres
xrange_1989$acres <- NA

xrange <- rbind(xrange_2015)

xrange$commodity <- trimws(xrange$commodity)
xrange$county <- trimws(xrange$county)
xrange$damagecause <- trimws(xrange$damagecause)


#--acres for all damage causes aggregated
ID_loss_commodity <- subset(xrange, commodity == var_commodity)
ID_loss_all <- aggregate(ID_loss_commodity$acres, by=list(ID_loss_commodity$county, ID_loss_commodity$state), FUN = "sum")
colnames(ID_loss_all) <- c("county", "state", "all_damagecause_acres")


#-loss and lossperacre for just one damage cause

ID_loss1 <- subset(xrange, commodity == var_commodity & damagecause == var_damage)
ID_loss2 <- aggregate(ID_loss1$loss, by=list(ID_loss1$county, ID_loss1$state), FUN = "sum")
ID_loss2_acres <- aggregate(ID_loss1$acres, by=list(ID_loss1$county, ID_loss1$state), FUN = "sum")
colnames(ID_loss2) <- c("county", "state", "loss")
colnames(ID_loss2_acres) <- c("county", "state", "acres")

ID_loss2$acres <- ID_loss2_acres$acres
ID_loss2$lossperacre <- ID_loss2$loss / ID_loss2$acres

#--

ID_loss_climate <- merge(ID_loss2, ID2, by=c("county", "state"))
ID_loss_climate_2 <- merge(ID_loss_all, ID_loss_climate, by=c("county", "state"))

#---WA

setwd("/dmine/data/USDA/agmesh-scenarios/Allstates/")
#setwd(monthdir2)

temp = list.files(pattern = paste("Washington", "_summary", sep=""))
myfiles = lapply(temp, read.csv, header = TRUE)
ziggy.df <- do.call(rbind , myfiles)
xrange <- as.data.frame(ziggy.df)
xrange$county <- as.character(xrange$county)
xrange$damagecause <- as.character(xrange$damagecause)

xrange_1989 <- subset(xrange, year <= 2000)
xrange_2015 <- subset(xrange, year >= 2001)
xrange_1989$loss <- xrange_1989$acres
xrange_1989$acres <- NA

xrange <- rbind(xrange_2015)

xrange$commodity <- trimws(xrange$commodity)
xrange$county <- trimws(xrange$county)
xrange$damagecause <- trimws(xrange$damagecause)


#--acres for all damage causes aggregated
WA_loss_commodity <- subset(xrange, commodity == var_commodity)
WA_loss_all <- aggregate(WA_loss_commodity$acres, by=list(WA_loss_commodity$county, WA_loss_commodity$state), FUN = "sum")
colnames(WA_loss_all) <- c("county", "state", "all_damagecause_acres")


WA_loss1 <- subset(xrange, commodity == var_commodity & damagecause == var_damage)
WA_loss2 <- aggregate(WA_loss1$loss, by=list(WA_loss1$county, WA_loss1$state), FUN = "sum")
WA_loss2_acres <- aggregate(WA_loss1$acres, by=list(WA_loss1$county, WA_loss1$state), FUN = "sum")
colnames(WA_loss2) <- c("county", "state", "loss")
colnames(WA_loss2_acres) <- c("county", "state", "acres")

WA_loss2$acres <- WA_loss2_acres$acres
WA_loss2$lossperacre <- WA_loss2$loss / WA_loss2$acres


WA_loss_climate <- merge(WA_loss2, WA2, by=c("county", "state"))
WA_loss_climate_2 <- merge(WA_loss_all, WA_loss_climate, by=c("county", "state"))


#--OR

setwd("/dmine/data/USDA/agmesh-scenarios/Allstates/")
#setwd(monthdir2)

temp = list.files(pattern = paste("Oregon", "_summary", sep=""))
myfiles = lapply(temp, read.csv, header = TRUE)
ziggy.df <- do.call(rbind , myfiles)
xrange <- as.data.frame(ziggy.df)
xrange$county <- as.character(xrange$county)
xrange$damagecause <- as.character(xrange$damagecause)

xrange_1989 <- subset(xrange, year <= 2000)
xrange_2015 <- subset(xrange, year >= 2001)
xrange_1989$loss <- xrange_1989$acres
xrange_1989$acres <- NA

xrange <- rbind(xrange_2015)

xrange$commodity <- trimws(xrange$commodity)
xrange$county <- trimws(xrange$county)
xrange$damagecause <- trimws(xrange$damagecause)


#--acres for all damage causes aggregated
OR_loss_commodity <- subset(xrange, commodity == var_commodity)
OR_loss_all <- aggregate(OR_loss_commodity$acres, by=list(OR_loss_commodity$county, OR_loss_commodity$state), FUN = "sum")
colnames(OR_loss_all) <- c("county", "state", "all_damagecause_acres")

OR_loss1 <- subset(xrange, commodity == var_commodity & damagecause == var_damage)
OR_loss2 <- aggregate(OR_loss1$loss, by=list(OR_loss1$county, OR_loss1$state), FUN = "sum")
OR_loss2_acres <- aggregate(OR_loss1$acres, by=list(OR_loss1$county, OR_loss1$state), FUN = "sum")
colnames(OR_loss2) <- c("county", "state", "loss")
colnames(OR_loss2_acres) <- c("county", "state", "acres")

OR_loss2$acres <- OR_loss2_acres$acres
OR_loss2$lossperacre <- OR_loss2$loss / OR_loss2$acres


OR_loss_climate <- merge(OR_loss2, OR2, by=c("county", "state"))
OR_loss_climate_2 <- merge(OR_loss_all, OR_loss_climate, by=c("county", "state"))





#-merge all three states

iPNW_loss_climate <- rbind(OR_loss_climate_2, ID_loss_climate_2, WA_loss_climate_2)

#--subset to only iPNW 24 counties

Franklin <- subset(iPNW_loss_climate, county == "Franklin" & state == "WA")

iPNW_loss_climate <- subset(iPNW_loss_climate, county == "Benewah" 
                             | county == "Latah" | county == "Nez Perce" | county == "Lewis" 
                             | county == "Idaho" | county == "Wasco" | county == "Sherman" 
                             | county == "Gilliam" | county == "Morrow" | county == "Umatilla" 
                             | county == "Union" | county == "Wallowa" | county == "Douglas" 
                             | county == "Walla Walla" | county == "Benton" | county == "Columbia" 
                             | county == "Asotin" | county == "Garfield" 
                             | county == "Grant" | county =="Whitman" | county == "Spokane" 
                             | county == "Lincoln" | county == "Adams" )

iPNW_loss_climate <- rbind(iPNW_loss_climate, Franklin)


iPNW_loss_climate$pct_acreage <- iPNW_loss_climate$acres / iPNW_loss_climate$all_damagecause_acres



#iPNW_loss_climate <- subset(iPNW_loss_climate, year == var_year)

#tmmx

iPNW_loss_climate_tmmx <- iPNW_loss_climate[order(iPNW_loss_climate$tmmx),] 

iPNW_loss_climate_tmmx$tmmx <- iPNW_loss_climate_tmmx$tmmx - 273


#yrangemin <- min(eval(parse(text=paste("iPNW_loss_climate_tmmx$", unit, sep="")))) - (.05 * min(eval(parse(text=paste("iPNW_loss_climate_tmmx$", unit, sep="")))))
#yrangemax <- max(eval(parse(text=paste("iPNW_loss_climate_tmmx$", unit, sep="")))) + (.05 * max(eval(parse(text=paste("iPNW_loss_climate_tmmx$", unit, sep="")))))

y2rangemin <- min(iPNW_loss_climate_tmmx$tmmx) - (.05 * min(iPNW_loss_climate_tmmx$tmmx))
y2rangemax <- max(iPNW_loss_climate_tmmx$tmmx) + (.05 * max(iPNW_loss_climate_tmmx$tmmx))

twoord.plot(c(1:nrow(iPNW_loss_climate_tmmx)), iPNW_loss_climate_tmmx$pct_acreage, c(1:nrow(iPNW_loss_climate_tmmx)), rylim=c(y2rangemin, y2rangemax), lylim=c(0, 1), iPNW_loss_climate_tmmx$tmmx, mar=c(8,4,4,4), ylab = "% wheat insurance loss acreage due to drought", xticklab=c(" "), rylab = "Mean Max Temperature (C)", type=c("bar", "b"), lcol = "red", rcol = "blue", main = paste(" 2001 - 2015 iPNW % Wheat/drought insurance loss acreage\n compared to mean TMMX", sep=""))
text(1:nrow(iPNW_loss_climate_tmmx), 0 - .05, srt = 90, adj = 1,
     labels = iPNW_loss_climate_tmmx$county, xpd = TRUE)
mtext(1, text = "iPNW counties", line = 4, cex = 2)
mtext(2, text = "% wheat/drought insurance loss acreage", line = 4, cex = 2)
mtext(4, text = "Annual Mean Precipitation (mm)", line = 1, cex = 2)

#pr

par(mfrow=c(1,3),mar=c(11.8, 6.1, 4, 2.1), family = 'serif', mgp=c(4, 1, 0), las=0)

iPNW_loss_climate_pr <- iPNW_loss_climate[order(-iPNW_loss_climate$pr),] 


#yrangemin <- min(iPNW_loss_climate_pr$lossperacre) - 10
#yrangemax <- max(iPNW_loss_climate_pr$lossperacre) + 10

y2rangemin <- min(iPNW_loss_climate_pr$pr) - (.05 * min(iPNW_loss_climate_pr$pr))
y2rangemax <- max(iPNW_loss_climate_pr$pr) + (.05 * max(iPNW_loss_climate_pr$pr))

twoord.plot(c(1:nrow(iPNW_loss_climate_pr)), iPNW_loss_climate_pr$pct_acreage, c(1:nrow(iPNW_loss_climate_pr)), rylim=c(y2rangemin, y2rangemax), lylim=c(0, 1), iPNW_loss_climate_pr$pr, mar=c(8,4,4,4), xticklab=c(" "), type=c("bar", "b"), lcol = "red", rcol = "blue")
#text(1:nrow(iPNW_loss_climate_pr), 0 - .05, srt = 90, adj = 1, cex = 1.5,
#     labels = iPNW_loss_climate_pr$county, xpd = TRUE)
mtext(1, text = "iPNW counties", line = 4, cex = 1.5)
mtext(2, text = "% wheat/drought insurance loss acreage", line = 4, cex = 1.5)
mtext(4, text = "Annual Mean PR (mm)", line = 1, cex = 1.5)
#mtext(3, text = "2001 - 2015 iPNW % Wheat/drought insurance loss acreage\n compared to annual mean precipitation/county", line = 1, cex = 2)

#pr

#par(mar=c(11.8, 6.1, 4, 2.1), family = 'serif', mgp=c(4, 1, 0), las=0)

#iPNW_loss_climate_pr <- iPNW_loss_climate[order(-iPNW_loss_climate$pr),] 

#y2rangemin <- min(iPNW_loss_climate_pr$pr) - (.05 * min(iPNW_loss_climate_pr$pr))
#y2rangemax <- max(iPNW_loss_climate_pr$pr) + (.05 * max(iPNW_loss_climate_pr$pr))

#twoord.plot(c(1:nrow(iPNW_loss_climate_pr)), iPNW_loss_climate_pr$pct_acreage, c(1:nrow(iPNW_loss_climate_pr)), rylim=c(y2rangemin, y2rangemax), lylim=c(0, 1), iPNW_loss_climate_pr$pr, mar=c(8,4,4,4), ylab = "% wheat/drought insurance loss acreage", xticklab=c(" "), rylab = "Annual Mean Precipitation (mm)", type=c("bar", "b"), lcol = "red", rcol = "blue", main = paste("2001 - 2015 iPNW % Wheat/drought insurance loss acreage\n compared to annual mean PR/county", sep=""))
#text(1:nrow(iPNW_loss_climate_pr), 0 - .05, srt = 90, adj = 1, 
#     labels = iPNW_loss_climate_pr$county, xpd = TRUE)
#mtext(1, text = "iPNW counties", line = 9, cex = 1)

#pet


iPNW_loss_climate_pet <- iPNW_loss_climate[order(iPNW_loss_climate$pet),] 


#yrangemin <- min(iPNW_loss_climate_pet$lossperacre) - 10
#yrangemax <- max(iPNW_loss_climate_pet$lossperacre) + 10

y2rangemin <- min(iPNW_loss_climate_pet$pet) - (.05 * min(iPNW_loss_climate_pet$pet))
y2rangemax <- max(iPNW_loss_climate_pet$pet) + (.05 * max(iPNW_loss_climate_pet$pet))

twoord.plot(c(1:nrow(iPNW_loss_climate_pet)), iPNW_loss_climate_pet$pct_acreage, c(1:nrow(iPNW_loss_climate_pet)), rylim=c(y2rangemin, y2rangemax), lylim=c(0, 1), iPNW_loss_climate_pet$pet, mar=c(8,4,4,4),  xticklab=c(" "), type=c("bar", "b"), lcol = "red", rcol = "blue")
#text(1:nrow(iPNW_loss_climate_pet), 0 - .05, srt = 90, adj = 1,
#     labels = iPNW_loss_climate_pet$county, xpd = TRUE)
mtext(1, text = "iPNW counties", line = 4, cex = 1.5)
#mtext(2, text = "% wheat/drought insurance loss acreage", line = 4, cex = 1.5)
mtext(4, text = "Annual Mean PET (mm)", line = 1, cex = 1.5)

#pr/pet

iPNW_loss_climate$prpet <- iPNW_loss_climate$pr / iPNW_loss_climate$pet
iPNW_loss_climate_prpet <- iPNW_loss_climate[order(-iPNW_loss_climate$prpet),] 


#yrangemin <- min(iPNW_loss_climate_pet$lossperacre) - 10
#yrangemax <- max(iPNW_loss_climate_pet$lossperacre) + 10

y2rangemin <- min(iPNW_loss_climate_prpet$prpet) - (.05 * min(iPNW_loss_climate_prpet$prpet))
y2rangemax <- max(iPNW_loss_climate_prpet$prpet) + (.05 * max(iPNW_loss_climate_prpet$prpet))

twoord.plot(c(1:nrow(iPNW_loss_climate_prpet)), iPNW_loss_climate_prpet$pct_acreage, c(1:nrow(iPNW_loss_climate_prpet)), rylim=c(y2rangemin, y2rangemax), lylim=c(0, 1), iPNW_loss_climate_prpet$prpet, mar=c(8,4,4,4),  xticklab=c(" "), type=c("bar", "b"), lcol = "red", rcol = "blue")
#text(1:nrow(iPNW_loss_climate_prpet), 0 - .05, srt = 90, adj = 1,
#     labels = iPNW_loss_climate_prpet$county, xpd = TRUE)
mtext(1, text = "iPNW counties", line = 4, cex = 1.5)
#mtext(2, text = "% wheat/drought insurance loss acreage", line = 4, cex = 1.5)
mtext(4, text = "Annual Mean Aridity Index (PR/PET (mm))", line = 1, cex = 1.5)


#

iPNW_loss_climate_pdsi <- iPNW_loss_climate[order(iPNW_loss_climate$pdsi),] 


#yrangemin <- min(iPNW_loss_climate_pet$lossperacre) - 10
#yrangemax <- max(iPNW_loss_climate_pet$lossperacre) + 10

y2rangemin <- min(iPNW_loss_climate_pdsi$pdsi) - (-.25 * min(iPNW_loss_climate_pdsi$pdsi))
y2rangemax <- max(iPNW_loss_climate_pdsi$pdsi) + (-.5 * max(iPNW_loss_climate_pdsi$pdsi))

twoord.plot(c(1:nrow(iPNW_loss_climate_pdsi)), iPNW_loss_climate_pdsi$pct_acreage, c(1:nrow(iPNW_loss_climate_pdsi)), rylim=c(y2rangemin, y2rangemax), lylim=c(0,1), iPNW_loss_climate_pdsi$pdsi, mar=c(8,4,4,4), ylab = "% wheat insurance loss acreage due to drought", xticklab=c(" "), rylab = "Mean PDSI", type=c("bar", "b"), lcol = "red", rcol = "blue", main = paste(" 2001 - 2015 iPNW % Wheat/drought insurance loss acreage\n compared to PDSI", sep=""))
text(1:nrow(iPNW_loss_climate_pdsi), 0 - .05, srt = 90, adj = 1,
     labels = iPNW_loss_climate_pdsi$county, xpd = TRUE)
mtext(1, text = "iPNW counties", line = 5)



}

