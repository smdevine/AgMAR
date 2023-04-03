library(extrafont)
library(extrafontdb)
# font_import() #only needs to be done one time after updating and re-installing R and moving and updating packages
loadfonts(device = 'win')
workDir <- 'C:/Users/smdevine/Desktop/post doc/Dahlke/RZWQM/projects/PulseSoilClimate/ClimateRuns_Silage/'
FiguresDir <- file.path(workDir, 'Figures')
resultsDir <- file.path(workDir, 'Results')
ssurgoDir2 <- 'C:/Users/smdevine/Desktop/post doc/Dahlke/RZWQM/ssurgo_profile_data/RZWQM input'
list.files(ssurgoDir2)
# comp_ksat <- read.csv(file.path(ssurgoDir2, 'comp_data_RZWQMruns_Feb22.csv'), stringsAsFactors = FALSE) #doesn't include latest soils added to project
comp_ksat <- read.csv(file.path(ssurgoDir2, 'comp_data_RZWQMruns_Jun22.csv'), stringsAsFactors = FALSE)
# comp_props <- read.csv(file.path(ssurgoDir2, 'comp_30cm_summary.csv'), stringsAsFactors = FALSE) #this is the 0-30 cm summary, produced in ???

#exploratory plotting functions
plot_30cm_prop_vs_add_NO3leached <- function(stn, scn, prop, ylim_max) {
  overall_results <- read.csv(file.path(resultsDir, 'Summaries', paste0('overall_results_', stn, '.csv')), stringsAsFactors = FALSE)
  overall_results[[prop]] <- comp_props[[prop]][match(overall_results$soil, comp_props$compname)]
  plot(overall_results[[prop]][overall_results$scenario==scn], overall_results$NO3_leached_delta_kgN_ha[overall_results$scenario==scn]/37, xlab=prop, ylab='', ylim=c(0,ylim_max))
  mtext(expression('Mean increase nitrate leached (kg N ha'^-1~'yr'^-1*')'), side=2, line=2.25)
  legend('topleft', legend = stn, bty='n')
}
colnames(comp_props)
#OM
plot_30cm_prop_vs_add_NO3leached(stn = 'Durham', scn = '21d', prop = 'SOM_30cm', ylim_max=6)
plot_30cm_prop_vs_add_NO3leached(stn = 'Davis', scn = '21d', prop = 'SOM_30cm', ylim_max=10)
plot_30cm_prop_vs_add_NO3leached(stn='Parlier', scn = '21d', prop = 'SOM_30cm', ylim_max=35)
plot_30cm_prop_vs_add_NO3leached(stn='FivePoints', scn = '21d', prop = 'SOM_30cm', ylim_max=57)
plot_30cm_prop_vs_add_NO3leached(stn='Shafter', scn = '21d', prop = 'SOM_30cm', ylim_max=62)

#clay
plot_30cm_prop_vs_add_NO3leached(stn = 'Durham', scn = '21d', prop = 'clay_30cm', ylim_max=6)
plot_30cm_prop_vs_add_NO3leached(stn = 'Davis', scn = '21d', prop = 'clay_30cm', ylim_max=10)
plot_30cm_prop_vs_add_NO3leached(stn='Parlier', scn = '21d', prop = 'clay_30cm', ylim_max=35)
plot_30cm_prop_vs_add_NO3leached(stn='FivePoints', scn = '21d', prop = 'clay_30cm', ylim_max=60)
plot_30cm_prop_vs_add_NO3leached(stn='Shafter', scn = '21d', prop = 'clay_30cm', ylim_max=65)

#silt
plot_30cm_prop_vs_add_NO3leached(stn = 'Durham', scn = '21d', prop = 'silt_30cm', ylim_max=6)
plot_30cm_prop_vs_add_NO3leached(stn = 'Davis', scn = '21d', prop = 'silt_30cm', ylim_max=10)
plot_30cm_prop_vs_add_NO3leached(stn = 'Parlier', scn = '21d', prop = 'silt_30cm', ylim_max=35)
plot_30cm_prop_vs_add_NO3leached(stn = 'FivePoints', scn = '21d', prop = 'silt_30cm', ylim_max=57)
plot_30cm_prop_vs_add_NO3leached(stn = 'Shafter', scn = '21d', prop = 'silt_30cm', ylim_max=62)

#sand
plot_30cm_prop_vs_add_NO3leached(stn = 'Durham', scn = '21d', prop = 'sand_30cm', ylim_max=6)
plot_30cm_prop_vs_add_NO3leached(stn = 'Davis', scn = '21d', prop = 'sand_30cm', ylim_max=10)
plot_30cm_prop_vs_add_NO3leached(stn = 'Parlier', scn = '21d', prop = 'sand_30cm', ylim_max=35)
plot_30cm_prop_vs_add_NO3leached(stn = 'FivePoints', scn = '21d', prop = 'sand_30cm', ylim_max=57)
plot_30cm_prop_vs_add_NO3leached(stn = 'Shafter', scn = '21d', prop = 'sand_30cm', ylim_max=62)

plot_profile_prop_vs_add_NO3leached <- function(stn, scn, prop, ylim_max) {
  overall_results <- read.csv(file.path(resultsDir, 'Summaries', paste0('overall_results_', stn, '.csv')), stringsAsFactors = FALSE)
  overall_results[[prop]] <- comp_ksat[[prop]][match(overall_results$soil, comp_ksat$compname)]
  plot(overall_results[[prop]][overall_results$scenario==scn], overall_results$NO3_leached_delta_kgN_ha[overall_results$scenario==scn]/37, xlab=prop, ylab='', ylim=c(0,ylim_max))
  mtext(expression('Mean increase nitrate leached (kg N ha'^-1~'yr'^-1*')'), side=2, line=2.25)
  legend('topleft', legend = stn, bty='n')
}

#clay
plot_profile_prop_vs_add_NO3leached(stn = 'Durham', scn = '21d', prop = 'clay', ylim_max=6)
plot_profile_prop_vs_add_NO3leached(stn = 'Davis', scn = '21d', prop = 'clay', ylim_max=10)
plot_profile_prop_vs_add_NO3leached(stn='Parlier', scn = '21d', prop = 'clay', ylim_max=35)
plot_profile_prop_vs_add_NO3leached(stn='FivePoints', scn = '21d', prop = 'clay', ylim_max=60)
plot_profile_prop_vs_add_NO3leached(stn='Shafter', scn = '21d', prop = 'clay', ylim_max=65)

#silt
plot_profile_prop_vs_add_NO3leached(stn = 'Durham', scn = '21d', prop = 'silt', ylim_max=6)
plot_profile_prop_vs_add_NO3leached(stn = 'Davis', scn = '21d', prop = 'silt', ylim_max=10)
plot_profile_prop_vs_add_NO3leached(stn = 'Parlier', scn = '21d', prop = 'silt', ylim_max=35)
plot_profile_prop_vs_add_NO3leached(stn = 'FivePoints', scn = '21d', prop = 'silt', ylim_max=57)
plot_profile_prop_vs_add_NO3leached(stn = 'Shafter', scn = '21d', prop = 'silt', ylim_max=62)

#sand
plot_profile_prop_vs_add_NO3leached(stn = 'Durham', scn = '21d', prop = 'sand', ylim_max=6)
plot_profile_prop_vs_add_NO3leached(stn = 'Davis', scn = '21d', prop = 'sand', ylim_max=10)
plot_profile_prop_vs_add_NO3leached(stn = 'Parlier', scn = '21d', prop = 'sand', ylim_max=35)
plot_profile_prop_vs_add_NO3leached(stn = 'FivePoints', scn = '21d', prop = 'sand', ylim_max=57)
plot_profile_prop_vs_add_NO3leached(stn = 'Shafter', scn = '21d', prop = 'sand', ylim_max=62)

#plotting to file functions
# View(comp_props)
#changed to total cumulative NO3 leached as opposed to annualized values on 8/15/22
plot_prop_vs_add_NO3leached_overall <- function(stns, prop, xlab_arg, scalar=1,y_max) {
  overall_results <- read.csv(file.path(resultsDir, 'Summaries', paste0('overall_results_', stns[1], '.csv')), stringsAsFactors = FALSE)
  print(all(overall_results$soil %in% comp_ksat$compnames))
  overall_results[[prop]] <- comp_ksat[[prop]][match(overall_results$soil, comp_ksat$compnames)]
  tiff(file = file.path(FiguresDir, 'Overall', 'Additional nitrate leached vs soil prop', paste0('nitrate_leached_vs_', prop, '_all_climates.tif')), family = 'Times New Roman', width = 6.5, height = 4.5, pointsize = 12, units = 'in', res=800, compression='lzw')
  par(mar=c(3, 3, 0.5, 0.5), xpd=TRUE, mgp=c(3,0.5,0), tcl=-0.4)
  plot(overall_results[[prop]][overall_results$scenario=='21d'], overall_results$NO3_leached_delta_kgN_ha[overall_results$scenario=='21d']/scalar, xlab='', ylab='', ylim=c(0,y_max), col='darkblue')
  mtext(expression('Additional nitrate leached (kg N ha'^-1*')'), side=2, line=1.75)
  mtext(xlab_arg, side=1, line=1.75)
  overall_results <- read.csv(file.path(resultsDir, 'Summaries', paste0('overall_results_', stns[2], '.csv')), stringsAsFactors = FALSE)
  overall_results[[prop]] <- comp_ksat[[prop]][match(overall_results$soil, comp_ksat$compnames)]
  points(x=overall_results[[prop]][overall_results$scenario=='21d'], y=(overall_results$NO3_leached_delta_kgN_ha[overall_results$scenario=='21d']/scalar), col='lightblue')
  overall_results <- read.csv(file.path(resultsDir, 'Summaries', paste0('overall_results_', stns[3], '.csv')), stringsAsFactors = FALSE)
  overall_results[[prop]] <- comp_ksat[[prop]][match(overall_results$soil, comp_ksat$compnames)]
  points(x=overall_results[[prop]][overall_results$scenario=='21d'], y=(overall_results$NO3_leached_delta_kgN_ha[overall_results$scenario=='21d']/scalar), col='yellow2')
  overall_results <- read.csv(file.path(resultsDir, 'Summaries', paste0('overall_results_', stns[4], '.csv')), stringsAsFactors = FALSE)
  overall_results[[prop]] <- comp_ksat[[prop]][match(overall_results$soil, comp_ksat$compnames)]
  points(x=overall_results[[prop]][overall_results$scenario=='21d'], y=(overall_results$NO3_leached_delta_kgN_ha[overall_results$scenario=='21d']/scalar), col='orange')
  overall_results <- read.csv(file.path(resultsDir, 'Summaries', paste0('overall_results_', stns[5], '.csv')), stringsAsFactors = FALSE)
  overall_results[[prop]] <- comp_ksat[[prop]][match(overall_results$soil, comp_ksat$compnames)]
  # print(max(overall_results$NO3_leached_delta_kgN_ha[overall_results$scenario=='21d']/scalar))
  points(x=overall_results[[prop]][overall_results$scenario=='21d'], y=(overall_results$NO3_leached_delta_kgN_ha[overall_results$scenario=='21d']/scalar), col='red')
  legend('topright', legend = rev(c('Durham', 'Davis', 'Parlier', 'Five Points', 'Shafter')), col=rev(c('darkblue', 'lightblue', 'yellow2', 'orange', 'red')), pch=1, bty = 'n')
  dev.off()
}
colnames(comp_ksat)
plot_prop_vs_add_NO3leached_overall(stns=c('Durham', 'Davis', 'Parlier', 'FivePoints', 'Shafter'), prop = 'clay', xlab_arg = 'Clay (%)', y_max=5000)
plot_prop_vs_add_NO3leached_overall(stns=c('Durham', 'Davis', 'Parlier', 'FivePoints', 'Shafter'), prop = 'silt', xlab_arg = 'Silt (%)', y_max=5000)
plot_prop_vs_add_NO3leached_overall(stns=c('Durham', 'Davis', 'Parlier', 'FivePoints', 'Shafter'), prop = 'sand', xlab_arg = 'Sand (%)', y_max=5000)
plot_prop_vs_add_NO3leached_overall(stns=c('Durham', 'Davis', 'Parlier', 'FivePoints', 'Shafter'), prop = 'microporosity', xlab_arg = 'Microporosity', y_max=5000)
plot_prop_vs_add_NO3leached_overall(stns=c('Durham', 'Davis', 'Parlier', 'FivePoints', 'Shafter'), prop = 'theta_15b', xlab_arg = 'Wilting point (% vol.)', y_max=5000)
plot_prop_vs_add_NO3leached_overall(stns=c('Durham', 'Davis', 'Parlier', 'FivePoints', 'Shafter'), prop = 'theta_0.3b', xlab_arg = 'Field capacity (% vol.)', y_max=5000)
plot_prop_vs_add_NO3leached_overall(stns=c('Durham', 'Davis', 'Parlier', 'FivePoints', 'Shafter'), prop = 'ksat_cm_day', xlab_arg = 'Saturated hydraulic conductivity', y_max = 5000)

#not yet updated 8/15/22
plot_30cm_prop_vs_add_NO3leached_overall <- function(stns, prop, xlab_arg) {
  overall_results <- read.csv(file.path(resultsDir, 'Summaries', paste0('overall_results_', stns[1], '.csv')), stringsAsFactors = FALSE)
  overall_results[[prop]] <- comp_props[[prop]][match(overall_results$soil, comp_props$compname)]
  tiff(file = file.path(FiguresDir, 'Overall', 'Additional nitrate leached vs 30 cm soil prop', paste0('nitrate_leached_vs_', prop, '_all_climates.tif')), family = 'Times New Roman', width = 6.5, height = 4.5, pointsize = 12, units = 'in', res=800, compression='lzw')
  par(mar=c(3.5, 4.5, 0.5, 0.5), xpd=TRUE)
  plot(overall_results[[prop]][overall_results$scenario=='21d'], overall_results$NO3_leached_delta_kgN_ha[overall_results$scenario=='21d']/37, xlab='', ylab='', ylim=c(0,62), col='darkblue')
  mtext(expression('Additional nitrate leached (kg N ha'^-1~'yr'^-1*')'), side=2, line=2.25)
  mtext(xlab_arg, side=1, line=2.25)
  overall_results <- read.csv(file.path(resultsDir, 'Summaries', paste0('overall_results_', stns[2], '.csv')), stringsAsFactors = FALSE)
  overall_results[[prop]] <- comp_props[[prop]][match(overall_results$soil, comp_props$compname)]
  points(x=overall_results[[prop]][overall_results$scenario=='21d'], y=(overall_results$NO3_leached_delta_kgN_ha[overall_results$scenario=='21d']/37), col='lightblue')
  overall_results <- read.csv(file.path(resultsDir, 'Summaries', paste0('overall_results_', stns[3], '.csv')), stringsAsFactors = FALSE)
  overall_results[[prop]] <- comp_props[[prop]][match(overall_results$soil, comp_props$compname)]
  points(x=overall_results[[prop]][overall_results$scenario=='21d'], y=(overall_results$NO3_leached_delta_kgN_ha[overall_results$scenario=='21d']/37), col='yellow2')
  overall_results <- read.csv(file.path(resultsDir, 'Summaries', paste0('overall_results_', stns[4], '.csv')), stringsAsFactors = FALSE)
  overall_results[[prop]] <- comp_props[[prop]][match(overall_results$soil, comp_props$compname)]
  points(x=overall_results[[prop]][overall_results$scenario=='21d'], y=(overall_results$NO3_leached_delta_kgN_ha[overall_results$scenario=='21d']/37), col='orange')
  overall_results <- read.csv(file.path(resultsDir, 'Summaries', paste0('overall_results_', stns[5], '.csv')), stringsAsFactors = FALSE)
  overall_results[[prop]] <- comp_props[[prop]][match(overall_results$soil, comp_props$compname)]
  points(x=overall_results[[prop]][overall_results$scenario=='21d'], y=(overall_results$NO3_leached_delta_kgN_ha[overall_results$scenario=='21d']/37), col='red')
  legend('topleft', legend = c('Durham', 'Davis', 'Parlier', 'Five Points', 'Shafter'), col=c('darkblue', 'lightblue', 'yellow2', 'orange', 'red'), pch=1, bty = 'n')
  dev.off()
}
colnames(comp_props)
plot_30cm_prop_vs_add_NO3leached_overall(stns=c('Durham', 'Davis', 'Parlier', 'FivePoints', 'Shafter'), prop = 'SOM_30cm', xlab_arg = 'Initial soil organic matter (%), 0-30 cm')

#plot mean increase nitrate leached vs. other n flux components
#updated 8/15/22, changing from annualized to total
plot_Ncomp_vs_add_NO3leached_overall <- function(stns, Ncomp, xlab_arg, fname_arg, scalar, leg_pos='topright', y_max) {
  xmin_vals <- as.numeric(rep(NA, 5))
  xmax_vals <- as.numeric(rep(NA, 5))
  for(i in seq_along(stns)) {
    overall_results <- read.csv(file.path(resultsDir, 'Summaries', paste0('overall_results_', stns[i], '.csv')), stringsAsFactors = FALSE)
    xmin_vals[i] <- min(overall_results[[Ncomp]][overall_results$scenario=='21d'])
    xmax_vals[i] <- max(overall_results[[Ncomp]][overall_results$scenario=='21d'])
  }
  xmin_vals <- xmin_vals / scalar
  xmax_vals <- xmax_vals / scalar
  overall_results <- read.csv(file.path(resultsDir, 'Summaries', paste0('overall_results_', stns[1], '.csv')), stringsAsFactors = FALSE)
  print(colnames(overall_results))
  tiff(file = file.path(FiguresDir, 'Overall', 'Additional nitrate leached vs. N comp', paste0('nitrate_leached_vs_', fname_arg, '_all_climates.tif')), family = 'Times New Roman', width = 6.5, height = 4.5, pointsize = 12, units = 'in', res=800, compression='lzw')
  par(mar=c(3.5, 4.5, 0.5, 0.5), xpd=TRUE)
  plot(overall_results[[Ncomp]][overall_results$scenario=='21d']/scalar, overall_results$NO3_leached_delta_kgN_ha[overall_results$scenario=='21d']/scalar, xlab='', ylab='', xlim=c(min(xmin_vals), max(xmax_vals)), ylim=c(0,y_max), col='darkblue')
  mtext(expression('Additional nitrate leached (kg N ha'^-1*')'), side=2, line=2.25)
  mtext(xlab_arg, side=1, line=2.25)
  overall_results <- read.csv(file.path(resultsDir, 'Summaries', paste0('overall_results_', stns[2], '.csv')), stringsAsFactors = FALSE)
  points(x=overall_results[[Ncomp]][overall_results$scenario=='21d']/scalar, y=(overall_results$NO3_leached_delta_kgN_ha[overall_results$scenario=='21d']/scalar), col='lightblue')
  overall_results <- read.csv(file.path(resultsDir, 'Summaries', paste0('overall_results_', stns[3], '.csv')), stringsAsFactors = FALSE)
  points(x=overall_results[[Ncomp]][overall_results$scenario=='21d']/scalar, y=(overall_results$NO3_leached_delta_kgN_ha[overall_results$scenario=='21d']/scalar), col='yellow2')
  overall_results <- read.csv(file.path(resultsDir, 'Summaries', paste0('overall_results_', stns[4], '.csv')), stringsAsFactors = FALSE)
  points(x=overall_results[[Ncomp]][overall_results$scenario=='21d']/scalar, y=(overall_results$NO3_leached_delta_kgN_ha[overall_results$scenario=='21d']/scalar), col='orange')
  overall_results <- read.csv(file.path(resultsDir, 'Summaries', paste0('overall_results_', stns[5], '.csv')), stringsAsFactors = FALSE)
  points(x=overall_results[[Ncomp]][overall_results$scenario=='21d']/scalar, y=(overall_results$NO3_leached_delta_kgN_ha[overall_results$scenario=='21d']/scalar), col='red')
  legend(leg_pos, legend = rev(c('Durham', 'Davis', 'Parlier', 'Five Points', 'Shafter')), col=rev(c('darkblue', 'lightblue', 'yellow2', 'orange', 'red')), pch=1, bty = 'n')
  dev.off()
}
colnames(overall_results)
plot_Ncomp_vs_add_NO3leached_overall(stns=c('Durham', 'Davis', 'Parlier', 'FivePoints', 'Shafter'), Ncomp='delta_orgN', xlab_arg = expression('Change in soil organic N (kg N ha'^-1*')'), fname_arg = 'delta_orgN', scalar = 1, y_max=5000) #stns, Ncomp, xlab_arg, fname_arg
plot_Ncomp_vs_add_NO3leached_overall(stns=c('Durham', 'Davis', 'Parlier', 'FivePoints', 'Shafter'), Ncomp='mean_abovegrnd_biomass_kg_ha', xlab_arg = expression('Aboveground biomass yield (kg ha'^-1*')'), fname_arg = 'biomass', scalar = 1, y_max=5000)
plot_Ncomp_vs_add_NO3leached_overall(stns=c('Durham', 'Davis', 'Parlier', 'FivePoints', 'Shafter'), Ncomp='NO3_leached_kgN_ha', xlab_arg = expression('Nitrate leached (kg N ha'^-1*')'), fname_arg = 'total_NO3_leached', scalar = 1, leg_pos = 'topleft', y_max = 5000)
plot_Ncomp_vs_add_NO3leached_overall(stns=c('Durham', 'Davis', 'Parlier', 'FivePoints', 'Shafter'), Ncomp='denitrification_kgN_ha', xlab_arg = expression('Denitrification (kg N ha'^-1*')'), fname_arg = 'denitrification', scalar = 1, y_max = 5000)
plot_Ncomp_vs_add_NO3leached_overall(stns=c('Durham', 'Davis', 'Parlier', 'FivePoints', 'Shafter'), Ncomp='net_min_kgN_ha', xlab_arg = expression('Net mineralization (kg N ha'^-1*')'), fname_arg = 'net_mineralization', scalar = 1, leg_pos = 'topleft', y_max = 5000)
plot_Ncomp_vs_add_NO3leached_overall(stns=c('Durham', 'Davis', 'Parlier', 'FivePoints', 'Shafter'), Ncomp='final_totalsoilMgC_ha', xlab_arg = expression('Total SOC (Mg C ha'^-1*')'), fname_arg = 'total_SOC_final', scalar = 1, y_max = 5000)
plot_Ncomp_vs_add_NO3leached_overall(stns=c('Durham', 'Davis', 'Parlier', 'FivePoints', 'Shafter'), Ncomp='delta_SOC', xlab_arg = expression('Change in SOC (Mg C ha'^-1*')'), fname_arg = 'total_SOC_delta', scalar = 1, y_max = 5000)
plot_Ncomp_vs_add_NO3leached_overall(stns=c('Durham', 'Davis', 'Parlier', 'FivePoints', 'Shafter'), Ncomp='volatilized_kgN_ha', xlab_arg = expression('Volatilized N (kg N ha'^-1*')'), fname_arg = 'volatilized', scalar = 1, y_max = 5000)
plot_Ncomp_vs_add_NO3leached_overall(stns=c('Durham', 'Davis', 'Parlier', 'FivePoints', 'Shafter'), Ncomp='runoff_cm', xlab_arg = expression('Total runoff (cm)'), fname_arg = 'runoff', scalar = 1, y_max = 5000)
plot_Ncomp_vs_add_NO3leached_overall(stns=c('Durham', 'Davis', 'Parlier', 'FivePoints', 'Shafter'), Ncomp='runoff_cm', xlab_arg = expression('Total runoff (cm)'), fname_arg = 'runoff', scalar = 1, y_max = 5000)

#not yet updated 8/15/22
plot_Ncomp_vs_total_NO3leached_overall <- function(stns, Ncomp, xlab_arg, fname_arg, scalar, leg_pos='topright') {
  xmin_vals <- as.numeric(rep(NA, 5))
  xmax_vals <- as.numeric(rep(NA, 5))
  for(i in seq_along(stns)) {
    overall_results <- read.csv(file.path(resultsDir, 'Summaries', paste0('overall_results_', stns[i], '.csv')), stringsAsFactors = FALSE)
    xmin_vals[i] <- min(overall_results[[Ncomp]][overall_results$scenario=='21d'])
    xmax_vals[i] <- max(overall_results[[Ncomp]][overall_results$scenario=='21d'])
  }
  xmin_vals <- xmin_vals / scalar
  xmax_vals <- xmax_vals / scalar
  overall_results <- read.csv(file.path(resultsDir, 'Summaries', paste0('overall_results_', stns[1], '.csv')), stringsAsFactors = FALSE)
  # print(colnames(overall_results))
  tiff(file = file.path(FiguresDir, 'Overall', 'Total nitrate leached vs N comp', paste0('total_nitrate_leached_vs_', fname_arg, '_all_climates.tif')), family = 'Times New Roman', width = 6.5, height = 4.5, pointsize = 12, units = 'in', res=800, compression='lzw')
  par(mar=c(3.5, 4.5, 0.5, 0.5), xpd=TRUE)
  plot(overall_results[[Ncomp]][overall_results$scenario=='21d']/scalar, overall_results$NO3_leached_kgN_ha[overall_results$scenario=='21d']/37, xlab='', ylab='', xlim=c(min(xmin_vals), max(xmax_vals)), ylim=c(0,70), col='darkblue')
  mtext(expression('Total nitrate leached (kg N ha'^-1~'yr'^-1*')'), side=2, line=2.25)
  mtext(xlab_arg, side=1, line=2.25)
  overall_results <- read.csv(file.path(resultsDir, 'Summaries', paste0('overall_results_', stns[2], '.csv')), stringsAsFactors = FALSE)
  points(x=overall_results[[Ncomp]][overall_results$scenario=='21d']/scalar, y=(overall_results$NO3_leached_kgN_ha[overall_results$scenario=='21d']/37), col='lightblue')
  overall_results <- read.csv(file.path(resultsDir, 'Summaries', paste0('overall_results_', stns[3], '.csv')), stringsAsFactors = FALSE)
  points(x=overall_results[[Ncomp]][overall_results$scenario=='21d']/scalar, y=(overall_results$NO3_leached_kgN_ha[overall_results$scenario=='21d']/37), col='yellow2')
  overall_results <- read.csv(file.path(resultsDir, 'Summaries', paste0('overall_results_', stns[4], '.csv')), stringsAsFactors = FALSE)
  points(x=overall_results[[Ncomp]][overall_results$scenario=='21d']/scalar, y=(overall_results$NO3_leached_kgN_ha[overall_results$scenario=='21d']/37), col='orange')
  overall_results <- read.csv(file.path(resultsDir, 'Summaries', paste0('overall_results_', stns[5], '.csv')), stringsAsFactors = FALSE)
  points(x=overall_results[[Ncomp]][overall_results$scenario=='21d']/scalar, y=(overall_results$NO3_leached_kgN_ha[overall_results$scenario=='21d']/37), col='red')
  legend(leg_pos, legend = c('Durham', 'Davis', 'Parlier', 'Five Points', 'Shafter'), col=c('darkblue', 'lightblue', 'yellow2', 'orange', 'red'), pch=1, bty = 'n')
  dev.off()
}
plot_Ncomp_vs_total_NO3leached_overall(stns=c('Durham', 'Davis', 'Parlier', 'FivePoints', 'Shafter'), Ncomp='delta_orgN', xlab_arg = expression('Change in soil organic N (kg N ha'^-1~'yr'^-1*')'), fname_arg = 'delta_orgN', scalar = 37)
plot_Ncomp_vs_total_NO3leached_overall(stns=c('Durham', 'Davis', 'Parlier', 'FivePoints', 'Shafter'), Ncomp='mean_abovegrnd_biomass_kg_ha', xlab_arg = expression('Aboveground biomass yield (kg ha'^-1~'yr'^-1*')'), fname_arg = 'biomass', scalar = 1)
# plot_Ncomp_vs_total_NO3leached_overall(stns=c('Durham', 'Davis', 'Parlier', 'FivePoints', 'Shafter'), Ncomp='NO3_leached_kgN_ha', xlab_arg = expression('Nitrate leached (kg N ha'^-1~'yr'^-1*')'), fname_arg = 'total_NO3_leached', scalar = 37, leg_pos = 'topleft')
plot_Ncomp_vs_total_NO3leached_overall(stns=c('Durham', 'Davis', 'Parlier', 'FivePoints', 'Shafter'), Ncomp='denitrification_kgN_ha', xlab_arg = expression('Denitrification (kg N ha'^-1~'yr'^-1*')'), fname_arg = 'denitrification', scalar = 37)
plot_Ncomp_vs_total_NO3leached_overall(stns=c('Durham', 'Davis', 'Parlier', 'FivePoints', 'Shafter'), Ncomp='net_min_kgN_ha', xlab_arg = expression('Net mineralization (kg N ha'^-1~'yr'^-1*')'), fname_arg = 'net_mineralization', scalar = 37, leg_pos = 'topleft')
plot_Ncomp_vs_total_NO3leached_overall(stns=c('Durham', 'Davis', 'Parlier', 'FivePoints', 'Shafter'), Ncomp='final_totalsoilMgC_ha', xlab_arg = expression('Total SOC (Mg C ha'^-1~')'), fname_arg = 'total_SOC_final', scalar = 1)
plot_Ncomp_vs_total_NO3leached_overall(stns=c('Durham', 'Davis', 'Parlier', 'FivePoints', 'Shafter'), Ncomp='delta_SOC', xlab_arg = expression('Change in SOC (Mg C ha'^-1~'yr'^-1~')'), fname_arg = 'totalC_delta', scalar = 37)
plot_Ncomp_vs_total_NO3leached_overall(stns=c('Durham', 'Davis', 'Parlier', 'FivePoints', 'Shafter'), Ncomp='volatilized_kgN_ha', xlab_arg = expression('Volatilized N (kg N ha'^-1~'yr'^-1*')'), fname_arg = 'volatilized', scalar = 37)
plot_Ncomp_vs_total_NO3leached_overall(stns=c('Durham', 'Davis', 'Parlier', 'FivePoints', 'Shafter'), Ncomp='runoff_cm', xlab_arg = expression('Runoff (cm yr'^-1~')'), fname_arg = 'runoff', scalar = 37)

#plot soil profile property vs. total nitrate leached
#not yet updated 8/15/22
plot_prop_vs_total_NO3leached_overall <- function(stns, prop, xlab_arg) {
  overall_results <- read.csv(file.path(resultsDir, 'Summaries', paste0('overall_results_', stns[1], '.csv')), stringsAsFactors = FALSE)
  overall_results[[prop]] <- comp_ksat[[prop]][match(overall_results$soil, comp_ksat$compnames)]
  tiff(file = file.path(FiguresDir, 'Overall', 'Total nitrate leached vs soil prop', paste0('total_nitrate_leached_vs_', prop, '_all_climates.tif')), family = 'Times New Roman', width = 6.5, height = 4.5, pointsize = 12, units = 'in', res=800, compression='lzw')
  par(mar=c(3.5, 4.5, 0.5, 0.5), xpd=TRUE)
  plot(overall_results[[prop]][overall_results$scenario=='21d'], overall_results$NO3_leached_kgN_ha[overall_results$scenario=='21d']/37, xlab='', ylab='', ylim=c(0,70), col='darkblue')
  mtext(expression('Total nitrate leached (kg N ha'^-1~'yr'^-1*')'), side=2, line=2.25)
  mtext(xlab_arg, side=1, line=2.25)
  overall_results <- read.csv(file.path(resultsDir, 'Summaries', paste0('overall_results_', stns[2], '.csv')), stringsAsFactors = FALSE)
  overall_results[[prop]] <- comp_ksat[[prop]][match(overall_results$soil, comp_ksat$compnames)]
  points(x=overall_results[[prop]][overall_results$scenario=='21d'], y=(overall_results$NO3_leached_kgN_ha[overall_results$scenario=='21d']/37), col='lightblue')
  overall_results <- read.csv(file.path(resultsDir, 'Summaries', paste0('overall_results_', stns[3], '.csv')), stringsAsFactors = FALSE)
  overall_results[[prop]] <- comp_ksat[[prop]][match(overall_results$soil, comp_ksat$compnames)]
  points(x=overall_results[[prop]][overall_results$scenario=='21d'], y=(overall_results$NO3_leached_kgN_ha[overall_results$scenario=='21d']/37), col='yellow2')
  overall_results <- read.csv(file.path(resultsDir, 'Summaries', paste0('overall_results_', stns[4], '.csv')), stringsAsFactors = FALSE)
  overall_results[[prop]] <- comp_ksat[[prop]][match(overall_results$soil, comp_ksat$compnames)]
  points(x=overall_results[[prop]][overall_results$scenario=='21d'], y=(overall_results$NO3_leached_kgN_ha[overall_results$scenario=='21d']/37), col='orange')
  overall_results <- read.csv(file.path(resultsDir, 'Summaries', paste0('overall_results_', stns[5], '.csv')), stringsAsFactors = FALSE)
  overall_results[[prop]] <- comp_ksat[[prop]][match(overall_results$soil, comp_ksat$compnames)]
  points(x=overall_results[[prop]][overall_results$scenario=='21d'], y=(overall_results$NO3_leached_kgN_ha[overall_results$scenario=='21d']/37), col='red')
  legend('topright', legend = c('Durham', 'Davis', 'Parlier', 'Five Points', 'Shafter'), col=c('darkblue', 'lightblue', 'yellow2', 'orange', 'red'), pch=1, bty = 'n')
  dev.off()
}
plot_prop_vs_total_NO3leached_overall(stns=c('Durham', 'Davis', 'Parlier', 'FivePoints', 'Shafter'), prop = 'clay', xlab_arg = 'Clay (%)')
plot_prop_vs_total_NO3leached_overall(stns=c('Durham', 'Davis', 'Parlier', 'FivePoints', 'Shafter'), prop = 'silt', xlab_arg = 'Silt (%)')
plot_prop_vs_total_NO3leached_overall(stns=c('Durham', 'Davis', 'Parlier', 'FivePoints', 'Shafter'), prop = 'sand', xlab_arg = 'Sand (%)')
plot_prop_vs_total_NO3leached_overall(stns=c('Durham', 'Davis', 'Parlier', 'FivePoints', 'Shafter'), prop = 'microporosity', xlab_arg = 'Microporosity')
plot_prop_vs_total_NO3leached_overall(stns=c('Durham', 'Davis', 'Parlier', 'FivePoints', 'Shafter'), prop = 'theta_15b', xlab_arg = 'Wilting point (% vol.)')
plot_prop_vs_total_NO3leached_overall(stns=c('Durham', 'Davis', 'Parlier', 'FivePoints', 'Shafter'), prop = 'theta_0.3b', xlab_arg = 'Field capacity (% vol.)')
plot_prop_vs_total_NO3leached_overall(stns=c('Durham', 'Davis', 'Parlier', 'FivePoints', 'Shafter'), prop = 'ksat_cm_day', xlab_arg = 'Saturated hydraulic conductivity')

#plot N comp relationship across all climates
plot_Ncomp_vs_Ncomp2_overall <- function(stns, Ncomp, Ncomp2, xlab_arg, ylab_arg, fname_arg, fname_arg2, scalar, scalar2, leg_pos='topright') {
  xmin_vals <- as.numeric(rep(NA, 5))
  xmax_vals <- as.numeric(rep(NA, 5))
  ymin_vals <- as.numeric(rep(NA, 5))
  ymax_vals <- as.numeric(rep(NA, 5))
  for(i in seq_along(stns)) {
    overall_results <- read.csv(file.path(resultsDir, 'Summaries', paste0('overall_results_', stns[i], '.csv')), stringsAsFactors = FALSE)
    xmin_vals[i] <- min(overall_results[[Ncomp]][overall_results$scenario=='21d'])
    xmax_vals[i] <- max(overall_results[[Ncomp]][overall_results$scenario=='21d'])
    ymin_vals[i] <- min(overall_results[[Ncomp2]][overall_results$scenario=='21d'])
    ymax_vals[i] <- max(overall_results[[Ncomp2]][overall_results$scenario=='21d'])
    
  }
  xmin_vals <- xmin_vals / scalar
  xmax_vals <- xmax_vals / scalar
  ymin_vals <- ymin_vals / scalar2
  ymax_vals <- ymax_vals / scalar2
  overall_results <- read.csv(file.path(resultsDir, 'Summaries', paste0('overall_results_', stns[1], '.csv')), stringsAsFactors = FALSE)
  print(colnames(overall_results))
  tiff(file = file.path(FiguresDir, 'Overall', 'N components only', paste0(fname_arg2, '_vs_', fname_arg, '_all_climates.tif')), family = 'Times New Roman', width = 6.5, height = 4.5, pointsize = 12, units = 'in', res=800, compression='lzw')
  par(mar=c(3.5, 4.5, 0.5, 0.5), xpd=TRUE)
  plot(overall_results[[Ncomp]][overall_results$scenario=='21d']/scalar, overall_results[[Ncomp2]][overall_results$scenario=='21d']/scalar2, xlab='', ylab='', xlim=c(min(xmin_vals), max(xmax_vals)), ylim=c(min(ymin_vals), max(ymax_vals)), col='darkblue')
  mtext(ylab_arg, side=2, line=2.25)
  mtext(xlab_arg, side=1, line=2.25)
  overall_results <- read.csv(file.path(resultsDir, 'Summaries', paste0('overall_results_', stns[2], '.csv')), stringsAsFactors = FALSE)
  points(x=overall_results[[Ncomp]][overall_results$scenario=='21d']/scalar, y=overall_results[[Ncomp2]][overall_results$scenario=='21d']/scalar2, col='lightblue')
  overall_results <- read.csv(file.path(resultsDir, 'Summaries', paste0('overall_results_', stns[3], '.csv')), stringsAsFactors = FALSE)
  points(x=overall_results[[Ncomp]][overall_results$scenario=='21d']/scalar, y=overall_results[[Ncomp2]][overall_results$scenario=='21d']/scalar2, col='yellow2')
  overall_results <- read.csv(file.path(resultsDir, 'Summaries', paste0('overall_results_', stns[4], '.csv')), stringsAsFactors = FALSE)
  points(x=overall_results[[Ncomp]][overall_results$scenario=='21d']/scalar, y=overall_results[[Ncomp2]][overall_results$scenario=='21d']/scalar2, col='orange')
  overall_results <- read.csv(file.path(resultsDir, 'Summaries', paste0('overall_results_', stns[5], '.csv')), stringsAsFactors = FALSE)
  points(x=overall_results[[Ncomp]][overall_results$scenario=='21d']/scalar, y=overall_results[[Ncomp2]][overall_results$scenario=='21d']/scalar2, col='red')
  legend(leg_pos, legend = c('Durham', 'Davis', 'Parlier', 'Five Points', 'Shafter'), col=c('darkblue', 'lightblue', 'yellow2', 'orange', 'red'), pch=1, bty = 'n')
  dev.off()
}
plot_Ncomp_vs_Ncomp2_overall(stns = c('Durham', 'Davis', 'Parlier', 'FivePoints', 'Shafter'), Ncomp = 'net_min_kgN_ha', Ncomp2 = 'delta_orgN', xlab_arg = expression('Net mineralization (kg N ha'^-1~'yr'^-1*')'), ylab_arg = expression('Change in soil organic N (kg N ha'^-1~'yr'^-1*')'), fname_arg = 'net_mineralization', fname_arg2 = 'delta_SON', scalar = 37, scalar2 = 37)
plot_Ncomp_vs_Ncomp2_overall(stns = c('Durham', 'Davis', 'Parlier', 'FivePoints', 'Shafter'), Ncomp = 'net_min_kgN_ha', Ncomp2 = 'initial_kgNO3_ha', xlab_arg = expression('Net mineralization (kg N ha'^-1~'yr'^-1*')'), ylab_arg = expression('Initial residual nitrate (kg N ha'^-1~')'), fname_arg = 'net_mineralization', fname_arg2 = 'ini_res_NO3', scalar = 37, scalar2 = 1, leg_pos = 'topleft')

#plot soil profile property vs N components for all climates
plot_prop_vs_N_comp_overall <- function(stns, prop, xlab_arg, ylab_arg, Ncomp, scalar, fname_arg, leg_pos='topright') {
  ymin_vals <- as.numeric(rep(NA, 5))
  ymax_vals <- as.numeric(rep(NA, 5))
  for(i in seq_along(stns)) {
    overall_results <- read.csv(file.path(resultsDir, 'Summaries', paste0('overall_results_', stns[i], '.csv')), stringsAsFactors = FALSE)
    ymin_vals[i] <- min(overall_results[[Ncomp]][overall_results$scenario=='21d'])
    ymax_vals[i] <- max(overall_results[[Ncomp]][overall_results$scenario=='21d'])
    
  }
  ymin_vals <- ymin_vals / scalar
  ymax_vals <- ymax_vals / scalar
  overall_results <- read.csv(file.path(resultsDir, 'Summaries', paste0('overall_results_', stns[1], '.csv')), stringsAsFactors = FALSE)
  overall_results[[prop]] <- comp_ksat[[prop]][match(overall_results$soil, comp_ksat$compnames)]
  tiff(file = file.path(FiguresDir, 'Overall', 'N comp vs soil prop', paste0(fname_arg, '_vs_', prop, '_all_climates.tif')), family = 'Times New Roman', width = 6.5, height = 4.5, pointsize = 12, units = 'in', res=800, compression='lzw')
  par(mar=c(3.5, 4.5, 0.5, 0.5), xpd=TRUE)
  plot(overall_results[[prop]][overall_results$scenario=='21d'], overall_results[[Ncomp]][overall_results$scenario=='21d']/scalar, xlab='', ylab='', ylim=c(min(ymin_vals), max(ymax_vals)), col='darkblue')
  mtext(ylab_arg, side=2, line=2.25)
  mtext(xlab_arg, side=1, line=2.25)
  overall_results <- read.csv(file.path(resultsDir, 'Summaries', paste0('overall_results_', stns[2], '.csv')), stringsAsFactors = FALSE)
  overall_results[[prop]] <- comp_ksat[[prop]][match(overall_results$soil, comp_ksat$compnames)]
  points(x=overall_results[[prop]][overall_results$scenario=='21d'], y=overall_results[[Ncomp]][overall_results$scenario=='21d']/scalar, col='lightblue')
  overall_results <- read.csv(file.path(resultsDir, 'Summaries', paste0('overall_results_', stns[3], '.csv')), stringsAsFactors = FALSE)
  overall_results[[prop]] <- comp_ksat[[prop]][match(overall_results$soil, comp_ksat$compnames)]
  points(x=overall_results[[prop]][overall_results$scenario=='21d'], y=overall_results[[Ncomp]][overall_results$scenario=='21d']/scalar, col='yellow2')
  overall_results <- read.csv(file.path(resultsDir, 'Summaries', paste0('overall_results_', stns[4], '.csv')), stringsAsFactors = FALSE)
  overall_results[[prop]] <- comp_ksat[[prop]][match(overall_results$soil, comp_ksat$compnames)]
  points(x=overall_results[[prop]][overall_results$scenario=='21d'], y=overall_results[[Ncomp]][overall_results$scenario=='21d']/scalar, col='orange')
  overall_results <- read.csv(file.path(resultsDir, 'Summaries', paste0('overall_results_', stns[5], '.csv')), stringsAsFactors = FALSE)
  overall_results[[prop]] <- comp_ksat[[prop]][match(overall_results$soil, comp_ksat$compnames)]
  points(x=overall_results[[prop]][overall_results$scenario=='21d'], y=overall_results[[Ncomp]][overall_results$scenario=='21d']/scalar, col='red')
  legend(leg_pos, legend = c('Durham', 'Davis', 'Parlier', 'Five Points', 'Shafter'), col=c('darkblue', 'lightblue', 'yellow2', 'orange', 'red'), pch=1, bty = 'n')
  dev.off()
}
plot_prop_vs_N_comp_overall(stns = c('Durham', 'Davis', 'Parlier', 'FivePoints', 'Shafter'), prop = 'clay', xlab_arg = 'Clay (%)', ylab_arg = expression('Net mineralization (kg N ha'^-1~'yr'^-1*')'), Ncomp = 'net_min_kgN_ha', scalar = 37, fname_arg = 'net_mineralization', leg_pos = 'bottomright')
plot_prop_vs_N_comp_overall(stns = c('Durham', 'Davis', 'Parlier', 'FivePoints', 'Shafter'), prop = 'clay', xlab_arg = 'Clay (%)', ylab_arg = expression('Change in soil organic N (kg N ha'^-1~'yr'^-1*')'), Ncomp = 'delta_orgN', scalar = 37, fname_arg = 'delta_orgN', leg_pos = 'topright')

#plot control (business-as-usual) variables vs. additional nitrate leached
plot_controlVar_vs_add_NO3leached_overall <- function(stns, controlVar, xlab_arg, fname_arg, scalar, leg_pos='topright', y_max) {
  xmin_vals <- as.numeric(rep(NA, 5))
  xmax_vals <- as.numeric(rep(NA, 5))
  for(i in seq_along(stns)) {
    overall_results <- read.csv(file.path(resultsDir, 'Summaries', paste0('overall_results_', stns[i], '.csv')), stringsAsFactors = FALSE)
    xmin_vals[i] <- min(overall_results[[controlVar]][overall_results$scenario=='Control'])
    xmax_vals[i] <- max(overall_results[[controlVar]][overall_results$scenario=='Control'])
  }
  xmin_vals <- xmin_vals / scalar
  xmax_vals <- xmax_vals / scalar
  overall_results <- read.csv(file.path(resultsDir, 'Summaries', paste0('overall_results_', stns[1], '.csv')), stringsAsFactors = FALSE)
  print(colnames(overall_results))
  tiff(file = file.path(FiguresDir, 'Overall', 'Additional nitrate leached vs. Control Variables', paste0('additional_nitrate_leached_vs_', fname_arg, '_all_climates.tif')), family = 'Times New Roman', width = 6.5, height = 4.5, pointsize = 12, units = 'in', res=800, compression='lzw')
  par(mar=c(3, 3, 0.5, 0.5), xpd=TRUE, mgp=c(3,0.5,0), tcl=-0.4)
  plot(overall_results[[controlVar]][overall_results$scenario=='Control']/scalar, overall_results$NO3_leached_delta_kgN_ha[overall_results$scenario=='21d']/scalar, xlab='', ylab='', xlim=c(min(xmin_vals), max(xmax_vals)), ylim=c(0,y_max), col='darkblue')
  mtext(expression('Additional nitrate leached (kg N ha'^-1*')'), side=2, line=1.75)
  mtext(xlab_arg, side=1, line=1.75)
  overall_results <- read.csv(file.path(resultsDir, 'Summaries', paste0('overall_results_', stns[2], '.csv')), stringsAsFactors = FALSE)
  points(x=overall_results[[controlVar]][overall_results$scenario=='Control']/scalar, y=(overall_results$NO3_leached_delta_kgN_ha[overall_results$scenario=='21d']/scalar), col='lightblue')
  overall_results <- read.csv(file.path(resultsDir, 'Summaries', paste0('overall_results_', stns[3], '.csv')), stringsAsFactors = FALSE)
  points(x=overall_results[[controlVar]][overall_results$scenario=='Control']/scalar, y=(overall_results$NO3_leached_delta_kgN_ha[overall_results$scenario=='21d']/scalar), col='yellow2')
  overall_results <- read.csv(file.path(resultsDir, 'Summaries', paste0('overall_results_', stns[4], '.csv')), stringsAsFactors = FALSE)
  points(x=overall_results[[controlVar]][overall_results$scenario=='Control']/scalar, y=(overall_results$NO3_leached_delta_kgN_ha[overall_results$scenario=='21d']/scalar), col='orange')
  overall_results <- read.csv(file.path(resultsDir, 'Summaries', paste0('overall_results_', stns[5], '.csv')), stringsAsFactors = FALSE)
  points(x=overall_results[[controlVar]][overall_results$scenario=='Control']/scalar, y=(overall_results$NO3_leached_delta_kgN_ha[overall_results$scenario=='21d']/scalar), col='red')
  legend(leg_pos, legend = rev(c('Durham', 'Davis', 'Parlier', 'Five Points', 'Shafter')), col=rev(c('darkblue', 'lightblue', 'yellow2', 'orange', 'red')), pch=1, bty = 'n')
  dev.off()
}
plot_controlVar_vs_add_NO3leached_overall(stns = c('Durham', 'Davis', 'Parlier', 'FivePoints', 'Shafter'), controlVar = 'DP_cm', xlab_arg = 'Business-as-usual deep percolation (cm)', fname_arg = 'deep_perc', scalar = 1, y_max=5000)
