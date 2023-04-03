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
plot_residual_nitrate <- function(soil, clims=c('Shafter', 'FivePoints', 'Parlier', 'Davis'), scn='AgMAR_21d', y_max, clim_labels=c('Shafter', 'Five Points', 'Parlier', 'Davis'), lwd_arg=0.9, cex_axis_arg=0.8, cex_legend=0.9) {
  for(i in 1:length(clims)) {
    control <- dailyReport(station = clims[i], projectName = 'SteadyStateRuns', soil)
    experimental <- dailyReport(station = clims[i], projectName = scn, soil)
    # y_max <- max(control$TOTAL.NO3.N.IN.PROFILE..KG.HA., experimental$TOTAL.NO3.N.IN.PROFILE..KG.HA.)  
    tiff(file = file.path(FiguresDir, 'By Soil', 'Residual nitrate', paste0(soil,'_res_nitrate_', clims[i], '.tif')), family = 'Times New Roman', width = 2.75, height = 2.5, pointsize = 12, units = 'in', res=800, compression='lzw')
    par(mar=c(1.5, 2.75, 0.5, 0.5), xpd=FALSE, mgp=c(3,0.5,0), tcl=-0.4)
    plot(control$date, control$TOTAL.NO3.N.IN.PROFILE..KG.HA., type='l', ylim= c(0, y_max), col='orange1', ylab='', xlab='', cex.axis=cex_axis_arg, lwd=lwd_arg, xlim=c(as.Date("1984-07-01"), as.Date("2020-01-01")))
    mtext(expression('Soil profile NO'[3]~'(kg N ha'^-1*')'), side = 2, line = 1.5)
    # mtext('Year', side = 1, line=2.25)
    legend('topleft', clim_labels[i], bty='n', inset=c(-0.1,-0.05), xjust=0, cex = cex_legend)
    lines(experimental$date, experimental$TOTAL.NO3.N.IN.PROFILE..KG.HA., col='turquoise1', lwd=lwd_arg)
    dev.off()
  }
}
plot_residual_nitrate('Hesperia', y_max = 800)
plot_residual_nitrate('Milham', y_max = 800)
plot_residual_nitrate('Lokern', y_max = 800)
# plot_residual_nitrate('Cropley', y_max = 500)
# plot_residual_nitrate('Colpien', y_max = 1500)

plot_cumulative_nitrate <- function(soil, clims=c('Shafter', 'FivePoints', 'Parlier', 'Davis'), scn='AgMAR_21d', y_max, clim_labels=c('Shafter', 'Five Points', 'Parlier', 'Davis'), lwd_arg=0.9, cex_axis_arg=0.85, cex_legend=0.9) {
  for(i in 1:length(clims)) {
    control <- dailyReport(station = clims[i], projectName = 'SteadyStateRuns', soil)
    experimental <- dailyReport(station = clims[i], projectName = scn, soil)
    # y_max <- max(control$TOTAL.NO3.N.IN.PROFILE..KG.HA., experimental$TOTAL.NO3.N.IN.PROFILE..KG.HA.)  
    tiff(file = file.path(FiguresDir, 'By Soil', 'Cumulative nitrate', paste0(soil,'_cum_nitrate_', clims[i], '.tif')), family = 'Times New Roman', width = 2.75, height = 2.5, pointsize = 12, units = 'in', res=800, compression='lzw')
    par(mar=c(1.5, 2.75, 0.5, 0.5), xpd=FALSE, mgp=c(3,0.5,0), tcl=-0.4)
    plot(control$date, control$NO3_cumulative, type='l', ylim= c(0, y_max), col='orange1', ylab='', xlab='', cex.axis=cex_axis_arg, lwd=lwd_arg, xlim=c(as.Date("1984-07-01"), as.Date("2020-01-01")))
    mtext(expression('Total NO'[3]~'leached (kg N ha'^-1*')'), side = 2, line = 1.5)
    # mtext('Year', side = 1, line=2.25)
    legend('topleft', clim_labels[i], bty='n', inset=c(-0.1,-0.05), xjust=0, cex=cex_legend)
    lines(experimental$date, experimental$NO3_cumulative, col='turquoise1', lwd=lwd_arg)
    if(clims[i]=='Davis') {
      legend('topleft', legend = c('Flood-MAR(21d)', 'no Flood-MAR'), lty=1, lwd=1.5, col=c('turquoise1', 'orange1'), bty='n', inset=c(-0.03,0.04), xjust=0, x.intersp = 0.4, cex=cex_legend, seg.len = 0.75)
    }
    dev.off()
  }
}
plot_cumulative_nitrate('Hesperia', y_max = 2350)
plot_cumulative_nitrate('Milham', y_max = 2350)
plot_cumulative_nitrate('Lokern', y_max = 2350)
# plot_cumulative_nitrate('Cropley', y_max = 1025)
# plot_cumulative_nitrate('Colpien', y_max = 2500)