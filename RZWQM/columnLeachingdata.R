library(extrafont)
library(extrafontdb)
# font_import() #only needs to be done one time after updating and re-installing R and moving and updating packages
loadfonts(device = 'win')
actualNO3 <- read.csv(file.path('C:/Users/smdevine/Desktop/post doc/Dahlke/RZWQM/nick_murphy_experiments/cumulativeNO3_N.csv'))
actualNO3$time_daily <- actualNO3$time_min/(24*60)
actualNO3_daily <- actualNO3$gNO3_cm2[match(days_minutes, actualNO3$time_min)]
FiguresDir <- 'C:/Users/smdevine/Desktop/post doc/Dahlke/RZWQM/nick_murphy_experiments/Figures'
workDir <- 'C:/Users/smdevine/Desktop/post doc/Dahlke/RZWQM/projects/ColumnNO3LeachingExperiment'
projectName <- 'FineSandyLoam_v11' #was last using v8
list.files(file.path(workDir, projectName))

dailyPT <- read.table(file.path(workDir, projectName, 'DAILY.PLT'), col.names = c('DAY', 'ACCUMULATED INFILTRATION (CM)', 'H2O BRK THROUGH CURVE (CM/DAY)', 'TEMP BREAK THROUGH CURVE (C)', 'WATER FLUX INTO GW (CM/DAY)', 'TOTAL RUNOFF (CM)', 'TOTAL NO3-N IN PROFILE (KG/HA)', 'NO3 FLUX INTO GW (UG/CM^2/DAY)', 'NO3 BREAK THROUGH (MG/L)', 'MINERALIZATION (KG/HA)', 'NITRIFICATION (KG/HA)', 'N2O PRODUCTION (KG N/HA)', 'NxO PRODUCTIOIN (KG N/HA)'), header = FALSE, skip=100)
dailyPT$DAY
dailyPT$TOTAL.NO3.N.IN.PROFILE..KG.HA.
sum(dailyPT$NO3.FLUX.INTO.GW..UG.CM.2.DAY.)/10

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

#function to graph
#for v8: c('DAY', 'ACCUMULATED INFILTRATION (CM)', 'H2O BRK THROUGH CURVE (CM/DAY)', 'TEMP BREAK THROUGH CURVE (C)', 'WATER FLUX INTO GW (CM/DAY)', 'TOTAL RUNOFF (CM)', 'TOTAL NO3-N IN PROFILE (KG/HA)', 'NO3 FLUX INTO GW (UG/CM^2/DAY)', 'NO3 BREAK THROUGH (MG/L)', 'MINERALIZATION (KG/HA)', 'NITRIFICATION (KG/HA)', 'N2O PRODUCTION (KG N/HA)', 'NxO PRODUCTIOIN (KG N/HA)')

plotActVsSim <- function(subDir, plot_fname, ylim_upp, columnNames, skiplines) {
  dailyPT <- read.table(file.path(workDir, subDir, 'DAILY.PLT'), col.names = columnNames, header = FALSE, skip=skiplines)
  print(paste(dailyPT$TOTAL.NO3.N.IN.PROFILE..KG.HA.[nrow(dailyPT)], 'kg ha-1 residual nitrate in profile'))
  print(paste(sum(dailyPT$NO3.FLUX.INTO.GW..UG.CM.2.DAY.[1:27]/10), 'kg ha-1 total nitrate leached'))
  sum(dailyPT$NO3.FLUX.INTO.GW..UG.CM.2.DAY.)/10
  leachingDF <- read.table(file.path(workDir, subDir, 'CLEACH.OUT'), col.names = c('DAY', 'H', 'CA', 'NA', 'MG', 'CL', 'HCO3', 'SO4', 'AL', 'NO3-N', 'NH4-N', 'CO3', 'UREA-N', 'PEST #1', 'PEST #2', 'PEST #3'), header = FALSE, skip=6)
  tiff(file = file.path(FiguresDir, plot_fname), family = 'Times New Roman', width = 4.5, height = 3.25, pointsize = 12, units = 'in', res=800, compression = 'lzw')
  par(mar=c(3,3.5,0.25,0.25))
  plot(actualNO3$time_daily, actualNO3$gNO3_cm2*10^5, ylim = c(0,ylim_upp), ylab='', xlab='', xlim = c(0,34))
  mtext(expression('NO'[3]*'-N leached (kg ha'^-1*')'), side = 2, line=2.25)
  mtext('days', side=1, line=2)
  points(x=leachingDF$DAY, y=leachingDF$NO3.N/10, col='red')
  legend(x='topleft', legend=c('measured', 'RZWQM simulated'), pch=1, col=c('black', 'red'), bty='n')
  dev.off()
}

plotActVsSim('FineSandyLoam_v9', 'cumulative_NO3_actual_vs_RZWQM_v9.tif', 20, c('DAY', 'ACCUMULATED INFILTRATION (CM)', 'WATER FLUX INTO GW (CM/DAY)', 'TOTAL NO3-N IN PROFILE (KG/HA)', 'NO3 FLUX INTO GW (UG/CM^2/DAY)', 'NO3 BREAK THROUGH (MG/L)', 'MINERALIZATION (KG/HA)', 'NITRIFICATION (KG/HA)'), 65)
plotActVsSim('FineSandyLoam_v10', 'cumulative_NO3_actual_vs_RZWQM_v10.tif', 17.5, c('DAY', 'ACCUMULATED INFILTRATION (CM)', 'H2O BRK THROUGH CURVE (CM/DAY)', 'TEMP BREAK THROUGH CURVE (C)', 'WATER FLUX INTO GW (CM/DAY)', 'TOTAL RUNOFF (CM)', 'TOTAL NO3-N IN PROFILE (KG/HA)', 'NO3 FLUX INTO GW (UG/CM^2/DAY)', 'NO3 BREAK THROUGH (MG/L)', 'MINERALIZATION (KG/HA)', 'NITRIFICATION (KG/HA)', 'N2O PRODUCTION (KG N/HA)', 'NxO PRODUCTIOIN (KG N/HA)'), 100)
plotActVsSim('FineSandyLoam_v11', 'cumulative_NO3_actual_vs_RZWQM_v11.tif', 20,  c('DAY', 'ACCUMULATED INFILTRATION (CM)', 'WATER FLUX INTO GW (CM/DAY)', 'TOTAL NO3-N IN PROFILE (KG/HA)', 'NO3 FLUX INTO GW (UG/CM^2/DAY)', 'NO3 BREAK THROUGH (MG/L)', 'MINERALIZATION (KG/HA)', 'NITRIFICATION (KG/HA)'), 65)
plotActVsSim('FineSandyLoam_v12', 'cumulative_NO3_actual_vs_RZWQM_v12.tif', 20, c('DAY', 'ACCUMULATED INFILTRATION (CM)', 'WATER FLUX INTO GW (CM/DAY)', 'TOTAL NO3-N IN PROFILE (KG/HA)', 'NO3 FLUX INTO GW (UG/CM^2/DAY)', 'NO3 BREAK THROUGH (MG/L)', 'MINERALIZATION (KG/HA)', 'NITRIFICATION (KG/HA)'), 65)
plotActVsSim('FineSandyLoam_v13', 'cumulative_NO3_actual_vs_RZWQM_v13.tif', 20, c('DAY', 'ACCUMULATED INFILTRATION (CM)', 'WATER FLUX INTO GW (CM/DAY)', 'TOTAL NO3-N IN PROFILE (KG/HA)', 'NO3 FLUX INTO GW (UG/CM^2/DAY)', 'NO3 BREAK THROUGH (MG/L)', 'MINERALIZATION (KG/HA)', 'NITRIFICATION (KG/HA)'), 65)
plotActVsSim('FineSandyLoam_v14', 'cumulative_NO3_actual_vs_RZWQM_v14.tif', 20, c('DAY', 'ACCUMULATED INFILTRATION (CM)', 'WATER FLUX INTO GW (CM/DAY)', 'TOTAL NO3-N IN PROFILE (KG/HA)', 'NO3 FLUX INTO GW (UG/CM^2/DAY)', 'NO3 BREAK THROUGH (MG/L)', 'MINERALIZATION (KG/HA)', 'NITRIFICATION (KG/HA)'), 65)
plotActVsSim('FineSandyLoam_v15', 'cumulative_NO3_actual_vs_RZWQM_v15.tif', 20, c('DAY', 'ACCUMULATED INFILTRATION (CM)', 'WATER FLUX INTO GW (CM/DAY)', 'TOTAL NO3-N IN PROFILE (KG/HA)', 'NO3 FLUX INTO GW (UG/CM^2/DAY)', 'NO3 BREAK THROUGH (MG/L)', 'MINERALIZATION (KG/HA)', 'NITRIFICATION (KG/HA)'), 65)
plotActVsSim('FineSandyLoam_v16', 'cumulative_NO3_actual_vs_RZWQM_v16.tif', 20, c('DAY', 'ACCUMULATED INFILTRATION (CM)', 'WATER FLUX INTO GW (CM/DAY)', 'TOTAL NO3-N IN PROFILE (KG/HA)', 'NO3 FLUX INTO GW (UG/CM^2/DAY)', 'NO3 BREAK THROUGH (MG/L)', 'MINERALIZATION (KG/HA)', 'NITRIFICATION (KG/HA)'), 65)
plotActVsSim('FineSandyLoam_v17', 'cumulative_NO3_actual_vs_RZWQM_v17.tif', 20, c('DAY', 'ACCUMULATED INFILTRATION (CM)', 'WATER FLUX INTO GW (CM/DAY)', 'TOTAL NO3-N IN PROFILE (KG/HA)', 'NO3 FLUX INTO GW (UG/CM^2/DAY)', 'NO3 BREAK THROUGH (MG/L)', 'MINERALIZATION (KG/HA)', 'NITRIFICATION (KG/HA)'), 65)
plotActVsSim('FineSandyLoam_v18', 'cumulative_NO3_actual_vs_RZWQM_v18.tif', 20, c('DAY', 'ACCUMULATED INFILTRATION (CM)', 'WATER FLUX INTO GW (CM/DAY)', 'TOTAL NO3-N IN PROFILE (KG/HA)', 'NO3 FLUX INTO GW (UG/CM^2/DAY)', 'NO3 BREAK THROUGH (MG/L)', 'MINERALIZATION (KG/HA)', 'NITRIFICATION (KG/HA)'), 65)
plotActVsSim('FineSandyLoam_v19', 'cumulative_NO3_actual_vs_RZWQM_v19.tif', 20, c('DAY', 'ACCUMULATED INFILTRATION (CM)', 'WATER FLUX INTO GW (CM/DAY)', 'TOTAL NO3-N IN PROFILE (KG/HA)', 'NO3 FLUX INTO GW (UG/CM^2/DAY)', 'NO3 BREAK THROUGH (MG/L)', 'MINERALIZATION (KG/HA)', 'NITRIFICATION (KG/HA)'), 65)
plotActVsSim('FineSandyLoam_v20', 'cumulative_NO3_actual_vs_RZWQM_v20.tif', 20, c('DAY', 'ACCUMULATED INFILTRATION (CM)', 'WATER FLUX INTO GW (CM/DAY)', 'TOTAL NO3-N IN PROFILE (KG/HA)', 'NO3 FLUX INTO GW (UG/CM^2/DAY)', 'NO3 BREAK THROUGH (MG/L)', 'MINERALIZATION (KG/HA)', 'NITRIFICATION (KG/HA)'), 65)
plotActVsSim('FineSandyLoam_v21', 'cumulative_NO3_actual_vs_RZWQM_v21.tif', 20, c('DAY', 'ACCUMULATED INFILTRATION (CM)', 'WATER FLUX INTO GW (CM/DAY)', 'TOTAL NO3-N IN PROFILE (KG/HA)', 'NO3 FLUX INTO GW (UG/CM^2/DAY)', 'NO3 BREAK THROUGH (MG/L)', 'MINERALIZATION (KG/HA)', 'NITRIFICATION (KG/HA)'), 65)
plotActVsSim('FineSandyLoam_v22', 'cumulative_NO3_actual_vs_RZWQM_v22.tif', 20, c('DAY', 'ACCUMULATED INFILTRATION (CM)', 'WATER FLUX INTO GW (CM/DAY)', 'TOTAL NO3-N IN PROFILE (KG/HA)', 'NO3 FLUX INTO GW (UG/CM^2/DAY)', 'NO3 BREAK THROUGH (MG/L)', 'MINERALIZATION (KG/HA)', 'NITRIFICATION (KG/HA)'), 65)
plotActVsSim('FineSandyLoam_v23', 'cumulative_NO3_actual_vs_RZWQM_v23.tif', 20, c('DAY', 'ACCUMULATED INFILTRATION (CM)', 'WATER FLUX INTO GW (CM/DAY)', 'TOTAL NO3-N IN PROFILE (KG/HA)', 'NO3 FLUX INTO GW (UG/CM^2/DAY)', 'NO3 BREAK THROUGH (MG/L)', 'MINERALIZATION (KG/HA)', 'NITRIFICATION (KG/HA)'), 65)
plotActVsSim('FineSandyLoam_v24', 'cumulative_NO3_actual_vs_RZWQM_v24.tif', 20, c('DAY', 'ACCUMULATED INFILTRATION (CM)', 'WATER FLUX INTO GW (CM/DAY)', 'TOTAL NO3-N IN PROFILE (KG/HA)', 'NO3 FLUX INTO GW (UG/CM^2/DAY)', 'NO3 BREAK THROUGH (MG/L)', 'MINERALIZATION (KG/HA)', 'NITRIFICATION (KG/HA)'), 65)
plotActVsSim('FineSandyLoam_v25', 'cumulative_NO3_actual_vs_RZWQM_v25.tif', 20, c('DAY', 'ACCUMULATED INFILTRATION (CM)', 'WATER FLUX INTO GW (CM/DAY)', 'TOTAL NO3-N IN PROFILE (KG/HA)', 'NO3 FLUX INTO GW (UG/CM^2/DAY)', 'NO3 BREAK THROUGH (MG/L)', 'MINERALIZATION (KG/HA)', 'NITRIFICATION (KG/HA)'), 65)
plotActVsSim('FineSandyLoam_v26', 'cumulative_NO3_actual_vs_RZWQM_v26.tif', 20, c('DAY', 'ACCUMULATED INFILTRATION (CM)', 'WATER FLUX INTO GW (CM/DAY)', 'TOTAL NO3-N IN PROFILE (KG/HA)', 'NO3 FLUX INTO GW (UG/CM^2/DAY)', 'NO3 BREAK THROUGH (MG/L)', 'MINERALIZATION (KG/HA)', 'NITRIFICATION (KG/HA)'), 65)
plotActVsSim('FineSandyLoam_v27', 'cumulative_NO3_actual_vs_RZWQM_v27.tif', 20, c('DAY', 'ACCUMULATED INFILTRATION (CM)', 'WATER FLUX INTO GW (CM/DAY)', 'TOTAL NO3-N IN PROFILE (KG/HA)', 'NO3 FLUX INTO GW (UG/CM^2/DAY)', 'NO3 BREAK THROUGH (MG/L)', 'MINERALIZATION (KG/HA)', 'NITRIFICATION (KG/HA)'), 65)
plotActVsSim('FineSandyLoam_v28', 'cumulative_NO3_actual_vs_RZWQM_v28.tif', 20, c('DAY', 'ACCUMULATED INFILTRATION (CM)', 'WATER FLUX INTO GW (CM/DAY)', 'TOTAL NO3-N IN PROFILE (KG/HA)', 'NO3 FLUX INTO GW (UG/CM^2/DAY)', 'NO3 BREAK THROUGH (MG/L)', 'MINERALIZATION (KG/HA)', 'NITRIFICATION (KG/HA)'), 65)
plotActVsSim('FineSandyLoam_v29', 'cumulative_NO3_actual_vs_RZWQM_v29.tif', 20, c('DAY', 'ACCUMULATED INFILTRATION (CM)', 'WATER FLUX INTO GW (CM/DAY)', 'TOTAL NO3-N IN PROFILE (KG/HA)', 'NO3 FLUX INTO GW (UG/CM^2/DAY)', 'NO3 BREAK THROUGH (MG/L)', 'MINERALIZATION (KG/HA)', 'NITRIFICATION (KG/HA)'), 65)
plotActVsSim('FineSandyLoam_v30', 'cumulative_NO3_actual_vs_RZWQM_v30.tif', 20, c('DAY', 'ACCUMULATED INFILTRATION (CM)', 'WATER FLUX INTO GW (CM/DAY)', 'TOTAL NO3-N IN PROFILE (KG/HA)', 'NO3 FLUX INTO GW (UG/CM^2/DAY)', 'NO3 BREAK THROUGH (MG/L)', 'MINERALIZATION (KG/HA)', 'NITRIFICATION (KG/HA)'), 65)