
library(lubridate)
library(doBy)

start.year = 2000
end.year = 2017
num.vars = 8

function(start.year,
         end.year){
  windows(height = 8.5, width = 11)
  par(mar=c(1, 1, 1, 1), xpd=T)
  plot(0, 0, type = "n",
       xlim = c(start.year-5, end.year),
       ylim = c(-11, 1),
       xaxt = "n", yaxt = "n",
       xlab = "", ylab = "",
       bty="n")
  
  #EMPTY BOXES
  points(rep(start.year:end.year, 8),
         rep(seq(-2,-9), each = length(start.year:end.year)),
         pch=22, lwd=3, bg="white", cex=5)
  
  #DEMOGRAPHICS
  
  #SOCIAL CAPITAL
  
  #PHYSICAL
  
  
  #TEXT LABELS
  #labels for years along the top
  text(start.year:end.year, rep(-1, length(start.year:end.year)), 
       labels=start.year:end.year, 
       srt = 90, cex=1.5)
  #labels for variable names down the left side
  text(rep(1997, num.vars), c(-2, -3, -4, -5, -6, -7, -8, -9), cex=1.25,
       labels = c("Demographic", 
                  "Land Use",
                  "Elevation",
                  "Climate",
                  "Sea Level",
                  "Bonding Capital",
                  "Bridging Capital",
                  "Linking Capital"))
  #title for the top
  text(2010, 0.5, "Data Availability", cex =2)
}


#### THIS IS THE CODE THAT I REPURPOSED TO MAKE THIS FIGURE: ####
# #CDEC Data Availability
# points(rep(2003:2016, 5),
#        rep(seq(-2,-6), each = length(2003:2016)),
#        pch=22, lwd=3, bg="white", cex=5.5)
# points(regavg$year[which(!is.na(regavg$CDEC_Temperature)==TRUE)],
#        rep(-2, length(regavg$date[which(!is.na(regavg$CDEC_Temperature)==TRUE)])),
#        pch=15, col="black", cex=5.5)
# points(regavg$year[which(!is.na(regavg$CDEC_Turbidity)==TRUE)],
#        rep(-3, length(regavg$date[which(!is.na(regavg$CDEC_Turbidity)==TRUE)])),
#        pch=15, col="black", cex=5.5)
# points(regavg$year[which(!is.na(regavg$CDEC_DO)==TRUE)],
#        rep(-4, length(regavg$date[which(!is.na(regavg$CDEC_DO)==TRUE)])),
#        pch=15, col="black", cex=5.5)
# 
# #DBW Data Availability
# points(wh$year[which(wh$wqregion == "FranksTractPlus")], 
#        rep(-5, length(wh$year[which(wh$wqregion == "FranksTractPlus")])), 
#        pch=15, col="black", cex=5.5)
# points(unique(ed.ft$YEAR), 
#        rep(-6, length(unique(ed.ft$YEAR))),
#        pch=15, col="black", cex=5.5)
# 
# text(2003:2016, rep(-1, length(2003:2016)), labels=2003:2016, srt = 90, cex=1.5)
# text(c(2000, 2000, 2000), c(-2, -3, -4, -5, -6), cex=1.25,
#      labels = c("Temperature", "Turbidity", "Dissolved Oxygen", "Water Hyacinth", "Egeria"))
# text(2010, 0.5, "Franks Tract Data", cex =2)
# 
# 
# #WH-only Figure
# windows(height = 5, width = 8.5)
# par(mar=c(1, 1, 1, 1), pdx=T)
# plot(0, 0, type = "n",
#      xlim = c(1997, 2017),
#      ylim = c(-7, 1),
#      xaxt = "n", yaxt = "n",
#      xlab = "", ylab = "",
#      bty="n")
# #CDEC Data Availability
# points(rep(2003:2016, 4),
#        rep(seq(-2,-5), each = length(2003:2016)),
#        pch=22, lwd=3, bg="white", cex=5.5)
# points(regavg$year[which(!is.na(regavg$CDEC_Temperature)==TRUE)],
#        rep(-2, length(regavg$date[which(!is.na(regavg$CDEC_Temperature)==TRUE)])),
#        pch=22, bg ="grey", lwd = 3, cex=5.5)
# points(regavg$year[which(!is.na(regavg$CDEC_Turbidity)==TRUE)],
#        rep(-3, length(regavg$date[which(!is.na(regavg$CDEC_Turbidity)==TRUE)])),
#        pch=22, bg ="grey", lwd = 3, cex=5.5)
# points(regavg$year[which(!is.na(regavg$CDEC_DO)==TRUE)],
#        rep(-4, length(regavg$date[which(!is.na(regavg$CDEC_DO)==TRUE)])),
#        pch=22, bg ="grey", lwd = 3, cex=5.5)
# 
# #DBW Data Availability
# points(wh$year[which(wh$wqregion == "FranksTractPlus")], 
#        rep(-5, length(wh$year[which(wh$wqregion == "FranksTractPlus")])), 
#        pch=22, bg ="grey", lwd = 3, cex=5.5)
# #legend for WH-only MS:
# text(2003:2016, rep(-1, length(2003:2016)), labels=2003:2016, srt = 90, cex=1.5)
# text(c(2000, 2000, 2000), c(-2, -3, -4, -5), cex=1.25,
#      labels = c("Temperature", "Turbidity", "Dissolved Oxygen", "Discrete Samples"))
# mtext(side = 2, line = -2, "Continuous", cex = 1.6)
# text(2010, 0.5, "Franks Tract Data", cex =2)
# 
# 
# ed.years <- summaryBy(SAMPLEID ~ YEAR, data = ed.ft, FUN = function(x) {length(unique(x))})
# for(i in 1:9)
# {
#   segments(x0 = ed.years[i, 1]-0.1, y0 = 0,
#            x1 = ed.years[i, 1]-0.1, y1 = ed.years[i, 2]/10,
#            lwd=7, col = "springgreen4")
# }
# 
# 
# wh.years <- summaryBy(SAMPLEID ~ year, data = wh[wh$wqregion == "FranksTractPlus",], 
#                       FUN = function(x) {length(unique(x))})
# for(i in 1:9)
# {
#   segments(x0 = wh.years[i, 1]+0.1, y0 = 0,
#            x1 = wh.years[i, 1]+0.1, y1 = wh.years[i, 2]/10,
#            lwd=7, col = "darkorchid")
# }

