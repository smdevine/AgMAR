library(extrafont)
library(extrafontdb)
# font_import() #only needs to be done one time after updating and re-installing R and moving and updating packages
loadfonts(device = 'win')
workDir <- 'C:/Users/smdevine/Desktop/post doc/Dahlke/RZWQM/projects/PulseSoilClimate/ClimateRuns_Silage/'
FiguresDir <- file.path(workDir, 'Figures')
resultsDir <- file.path(workDir, 'Results')
ssurgoDir2 <- 'C:/Users/smdevine/Desktop/post doc/Dahlke/RZWQM/ssurgo_profile_data/RZWQM input'
comp_ksat <- read.csv(file.path(ssurgoDir2, 'comp_data_RZWQMruns_Feb22.csv'), stringsAsFactors = FALSE)
plot_clay_vs_NO3leached_overall <- function(stns) {
  overall_results <- read.csv(file.path(resultsDir, 'Summaries', paste0('overall_results_', stns[1], '.csv')), stringsAsFactors = FALSE)
  overall_results$clay <- comp_ksat$clay[match(overall_results$soil, comp_ksat$compnames)]
  tiff(file = file.path(FiguresDir, 'Overall', 'nitrate_leached_all_climates.tif'), family = 'Times New Roman', width = 6.5, height = 4.5, pointsize = 12, units = 'in', res=800, compression='lzw')
  par(mar=c(3.5, 4.5, 0.5, 0.5), xpd=TRUE)
  plot(overall_results$clay[overall_results$scenario=='Mar7d'], overall_results$NO3_leached_delta_kgN_ha[overall_results$scenario=='Mar7d']/37, xlab='', ylab='', ylim=c(0,62), col='darkblue')
  mtext(expression('Additional nitrate leached (kg N ha'^-1~'yr'^-1*' Flood-MAR)'), side=2, line=2.25)
  mtext('Clay (%)', side=1, line=2.25)
  overall_results <- read.csv(file.path(resultsDir, 'Summaries', paste0('overall_results_', stns[2], '.csv')), stringsAsFactors = FALSE)
  overall_results$clay <- comp_ksat$clay[match(overall_results$soil, comp_ksat$compnames)]
  points(x=overall_results$clay[overall_results$scenario=='Mar7d'], y=(overall_results$NO3_leached_delta_kgN_ha[overall_results$scenario=='Mar7d']/37), col='lightblue')
  overall_results <- read.csv(file.path(resultsDir, 'Summaries', paste0('overall_results_', stns[3], '.csv')), stringsAsFactors = FALSE)
  overall_results$clay <- comp_ksat$clay[match(overall_results$soil, comp_ksat$compnames)]
  points(x=overall_results$clay[overall_results$scenario=='Mar7d'], y=(overall_results$NO3_leached_delta_kgN_ha[overall_results$scenario=='Mar7d']/37), col='yellow2')
  overall_results <- read.csv(file.path(resultsDir, 'Summaries', paste0('overall_results_', stns[4], '.csv')), stringsAsFactors = FALSE)
  overall_results$clay <- comp_ksat$clay[match(overall_results$soil, comp_ksat$compnames)]
  points(x=overall_results$clay[overall_results$scenario=='Mar7d'], y=(overall_results$NO3_leached_delta_kgN_ha[overall_results$scenario=='Mar7d']/37), col='orange')
  overall_results <- read.csv(file.path(resultsDir, 'Summaries', paste0('overall_results_', stns[5], '.csv')), stringsAsFactors = FALSE)
  overall_results$clay <- comp_ksat$clay[match(overall_results$soil, comp_ksat$compnames)]
  points(x=overall_results$clay[overall_results$scenario=='Mar7d'], y=(overall_results$NO3_leached_delta_kgN_ha[overall_results$scenario=='Mar7d']/37), col='red')
  legend('topright', legend = c('Durham', 'Davis', 'Parlier', 'Five Points', 'Shafter'), col=c('darkblue', 'lightblue', 'yellow2', 'orange', 'red'), pch=1, bty = 'n')
  dev.off()
}
plot_clay_vs_NO3leached_overall(c('Durham', 'Davis', 'Parlier', 'FivePoints', 'Shafter'))

plot_clay_vs_NO3leached <- function(stn) {
  overall_results <- read.csv(file.path(resultsDir, 'Summaries', paste0('overall_results_', stn, '.csv')), stringsAsFactors = FALSE)
  overall_results$clay <- comp_ksat$clay[match(overall_results$soil, comp_ksat$compnames)]
  plot(overall_results$clay[overall_results$scenario=='Mar7d'], overall_results$NO3_leached_delta_kgN_ha[overall_results$scenario=='Mar7d']/37, xlab='Clay (%)', ylab='', ylim=c(0,240))
  mtext(expression('Mean increase nitrate leached (kg N ha'^-1~'yr'^-1*'Flood-MAR)'), side=2, line=2.25)
}
plot_clay_vs_NO3leached('Durham')
plot_clay_vs_NO3leached('Davis')
plot_clay_vs_NO3leached('Parlier')
plot_clay_vs_NO3leached('FivePoints')
plot_clay_vs_NO3leached('Shafter')
