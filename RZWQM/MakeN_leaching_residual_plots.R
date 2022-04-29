library(extrafont)
library(extrafontdb)
# font_import() #only needs to be done one time after updating and re-installing R and moving and updating packages
loadfonts(device = 'win')

workDir <- 'C:/Users/smdevine/Desktop/post doc/Dahlke/RZWQM/projects/PulseSoilClimate/ClimateRuns_Silage/'
FiguresDir <- file.path(workDir, 'Figures')
resultsDir <- file.path(workDir, 'Results')

#define functions for plotting
dailyReport<- function(station, projectName, soil) {
  leachingDF <- read.table(file.path(workDir, station, projectName, soil, 'DAILY.PLT'), col.names = c('DAY', 'ACCUMULATED PRECIPITATION (CM)',  'ACCUMULATED INFILTRATION (CM)', 'TEMP BREAK THROUGH CURVE (C)', 'WATER FLUX INTO GW (CM/DAY)', 'ACTUAL EVAPORATION (CM)', 'ACTUAL TRANSPIRATION (CM)', 'SURFACE MULCH MASS (KG/HA)', 'TOTAL NO3-N IN PROFILE (KG/HA)', 'NO3 FLUX INTO GW (UG/CM^2/DAY)', 'NO3 BREAK THROUGH (MG/L)', 'MINERALIZATION (KG/HA)', 'LEAF AREA INDEX', 'PLANT HEIGHT (CM)', 'PLANT AREA COVER (%)', 'DEPTH OF ROOTS (CM)', 'WATER STRESS', 'TEMPERATURE STRESS', 'NUTRIENT STRESS', 'NUMBER OF LIVE PLANTS', 'TOT ABOVE GRD BIOMASS (KG/HA)'), header = FALSE, skip=156)
  leachingDF$date <- seq(as.Date("1983/10/1"), as.Date("2020/10/1"), "days")
  leachingDF$year <- format.Date(leachingDF$date, '%Y')
  leachingDF$NO3_cumulative <- cumsum(leachingDF$NO3.FLUX.INTO.GW..UG.CM.2.DAY. / 10)
  leachingDF
}
plot_residual_nitrate <- function(soil, clims=c('Shafter', 'FivePoints', 'Parlier', 'Davis', 'Durham'), scn='AgMAR_21d', y_max, clim_labels=c('Shafter', 'Five Points', 'Parlier', 'Davis', 'Durham')) {
  for(i in 1:length(clims)) {
    control <- dailyReport(station = clims[i], projectName = 'SteadyStateRuns', soil)
    experimental <- dailyReport(station = clims[i], projectName = scn, soil)
    # y_max <- max(control$TOTAL.NO3.N.IN.PROFILE..KG.HA., experimental$TOTAL.NO3.N.IN.PROFILE..KG.HA.)  
    tiff(file = file.path(FiguresDir, 'By Soil', paste0(soil,'_res_nitrate_', clims[i], '.tif')), family = 'Times New Roman', width = 4.5, height = 3, pointsize = 12, units = 'in', res=800, compression='lzw')
    par(mar=c(2, 4.25, 0.5, 0.25), xpd=TRUE)
    plot(control$date, control$TOTAL.NO3.N.IN.PROFILE..KG.HA., type='l', ylim= c(0, y_max), col='orange', ylab='', xlab='')
    mtext(expression('Soil profile nitrate (kg ha'^-1*')'), side = 2, line = 2.5)
    # mtext('Year', side = 1, line=2.25)
    legend('topleft', clim_labels[i], bty='n')
    lines(experimental$date, experimental$TOTAL.NO3.N.IN.PROFILE..KG.HA., col='turquoise1')
    dev.off()
  }
}
plot_residual_nitrate('Kimberlina', y_max = 310)
plot_residual_nitrate('Cerini', y_max = 490)
plot_residual_nitrate('Merced', y_max = 235)
plot_residual_nitrate('Colpien', y_max = 1500)

plot_cumulative_nitrate <- function(soil, clims=c('Shafter', 'FivePoints', 'Parlier', 'Davis', 'Durham'), scn='AgMAR_21d', y_max, clim_labels=c('Shafter', 'Five Points', 'Parlier', 'Davis', 'Durham')) {
  for(i in 1:length(clims)) {
    control <- dailyReport(station = clims[i], projectName = 'SteadyStateRuns', soil)
    experimental <- dailyReport(station = clims[i], projectName = scn, soil)
    # y_max <- max(control$TOTAL.NO3.N.IN.PROFILE..KG.HA., experimental$TOTAL.NO3.N.IN.PROFILE..KG.HA.)  
    tiff(file = file.path(FiguresDir, 'By Soil', paste0(soil,'_cum_nitrate_', clims[i], '.tif')), family = 'Times New Roman', width = 4.5, height = 3, pointsize = 12, units = 'in', res=800, compression='lzw')
    par(mar=c(2, 4.25, 0.5, 0.25), xpd=TRUE)
    plot(control$date, control$NO3_cumulative, type='l', ylim= c(0, y_max), col='orange', ylab='', xlab='')
    mtext(expression('Cumulative nitrate leached (kg ha'^-1*')'), side = 2, line = 2.5)
    # mtext('Year', side = 1, line=2.25)
    legend('topleft', clim_labels[i], bty='n')
    lines(experimental$date, experimental$NO3_cumulative, col='turquoise1')
    if(clims[i]=='Davis') {
      legend('topleft', legend = c('Control', 'Flood-MAR(21d)'), lty=1, lwd=1.5, col=c('orange', 'turquoise1'), bty='n', inset=c(0,0.1))
    }
    dev.off()
  }
}
plot_cumulative_nitrate('Kimberlina', y_max = 575)
plot_cumulative_nitrate('Cerini', y_max = 1150)
plot_cumulative_nitrate('Merced', y_max = 600)
plot_cumulative_nitrate('Colpien', y_max = 2500)
