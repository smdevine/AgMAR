library(extrafont)
library(extrafontdb)
# font_import() #only needs to be done one time after updating and re-installing R and moving and updating packages
loadfonts(device = 'win')
actualNO3 <- read.csv(file.path('C:/Users/smdevine/Desktop/post doc/Dahlke/RZWQM/nick_murphy_experiments/cumulativeNO3_N.csv'))
actualNO3$time_daily <- actualNO3$time_min/(24*60)
actualNO3_daily <- actualNO3$gNO3_cm2[match(days_minutes, actualNO3$time_min)]
FiguresDir <- 'C:/Users/smdevine/Desktop/post doc/Dahlke/RZWQM/nick_murphy_experiments/Figures'
workDir <- 'C:/Users/smdevine/Desktop/post doc/Dahlke/RZWQM/projects/ColumnNO3LeachingExperiment'
projectName <- 'FineSandyLoam_v8'
list.files(file.path(workDir, projectName))

dailyPT <- read.table(file.path(workDir, projectName, 'DAILY.PLT'), col.names = c('DAY', 'ACCUMULATED INFILTRATION (CM)', 'H2O BRK THROUGH CURVE (CM/DAY)', 'TEMP BREAK THROUGH CURVE (C)', 'WATER FLUX INTO GW (CM/DAY)', 'TOTAL RUNOFF (CM)', 'TOTAL NO3-N IN PROFILE (KG/HA)', 'NO3 FLUX INTO GW (UG/CM^2/DAY)', 'NO3 BREAK THROUGH (MG/L)', 'MINERALIZATION (KG/HA)', 'NITRIFICATION (KG/HA)', 'N2O PRODUCTION (KG N/HA)', 'NxO PRODUCTIOIN (KG N/HA)'), header = FALSE, skip=100)
dailyPT$DAY
dailyPT$TOTAL.NO3.N.IN.PROFILE..KG.HA.

leachingDF <- read.table(file.path(workDir, projectName, 'CLEACH.OUT'), col.names = c('DAY', 'H', 'CA', 'NA', 'MG', 'CL', 'HCO3', 'SO4', 'AL', 'NO3-N', 'NH4-N', 'CO3', 'UREA-N', 'PEST #1', 'PEST #2', 'PEST #3'), header = FALSE, skip=6)
# leachingDF$NO3.N
# writeClipboard(as.character(leachingDF$NO3.N))
# writeClipboard(as.character(leachingDF$DAY))
tiff(file = file.path(FiguresDir, 'cumulativeNO3_column_RZWQM_vs_actual.tif'), family = 'Times New Roman', width = 4.5, height = 3.25, pointsize = 12, units = 'in', res=800, compression = 'lzw')
par(mar=c(3,3.5,0.25,0.25))
plot(actualNO3$time_daily, actualNO3$gNO3_cm2*10^6, ylim = c(0,175), ylab='', xlab='')
mtext(expression('NO'[3]*'-N leached ('*mu*'g cm'^-2*')'), side = 2, line=2.25)
mtext('days', side=1, line=2)
points(x=leachingDF$DAY, y=leachingDF$NO3.N, col='red')
legend(x='topleft', legend=c('measured', 'RZWQM simulated'), pch=1, col=c('black', 'red'), bty='n')
dev.off()

tiff(file = file.path(FiguresDir, 'cumulativeNO3_column_RZWQM_vs_actual_v2.tif'), family = 'Times New Roman', width = 4.5, height = 3.25, pointsize = 12, units = 'in', res=800, compression = 'lzw')
par(mar=c(3,3.5,0.25,0.25))
plot(actualNO3$time_daily, actualNO3$gNO3_cm2*10^6, ylim = c(0,175), ylab='', xlab='')
mtext(expression('NO'[3]*'-N leached ('*mu*'g cm'^-2*')'), side = 2, line=2.25)
mtext('days', side=1, line=2)
points(x=leachingDF$DAY, y=leachingDF$NO3.N, col='red')
legend(x='topleft', legend=c('measured', 'RZWQM simulated'), pch=1, col=c('black', 'red'), bty='n')
dev.off()

tiff(file = file.path(FiguresDir, 'cumulativeNO3_column_RZWQM_vs_actual_v3.tif'), family = 'Times New Roman', width = 4.5, height = 3.25, pointsize = 12, units = 'in', res=800, compression = 'lzw')
par(mar=c(3,3.5,0.25,0.25))
plot(actualNO3$time_daily, actualNO3$gNO3_cm2*10^6, ylim = c(0,175), ylab='', xlab='')
mtext(expression('NO'[3]*'-N leached ('*mu*'g cm'^-2*')'), side = 2, line=2.25)
mtext('days', side=1, line=2)
points(x=leachingDF$DAY, y=leachingDF$NO3.N, col='red')
legend(x='topleft', legend=c('measured', 'RZWQM simulated'), pch=1, col=c('black', 'red'), bty='n')
dev.off()
