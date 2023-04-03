#to-do
#make boxplot of coarse, loamy, and fine soils net nitrate leaching increase that includes data from all climates
#and boxplots of individual climates--but is n=10 enough for this? [DONE]
#make figure comparing yields by treatment x soil type [DONE]
#make figure comparing difference in NO3 leaching by treatment, using Jan7d as the control [DONE]
library(extrafont)
library(extrafontdb)
# font_import() #only needs to be done one time after updating and re-installing R and moving and updating packages
loadfonts(device = 'win')

N_harvest_efficiency <- 0.977
met_stns <- c('Durham', 'Davis', 'Parlier', 'FivePoints', 'Shafter')
met_stn <- met_stns[5]
workDir <- 'C:/Users/smdevine/Desktop/post doc/Dahlke/RZWQM/projects/PulseSoilClimate/ClimateRuns_Silage/'
FiguresDir <- file.path(workDir, 'Figures')
if(!dir.exists(file.path(FiguresDir, met_stn))) {
  dir.create(file.path(FiguresDir, met_stn))
}
resultsDir <- file.path(workDir, 'Results')

#read-in results file
overall_results <- read.csv(file.path(resultsDir, 'Summaries', paste0('overall_results_', met_stn, '.csv')), stringsAsFactors = FALSE)
overall_results$N_misc_loss_kgN_ha <- overall_results$N2O_loss_nitri + overall_results$NxO_loss_nitri + overall_results$NxO_abs_nitri
overall_results$delta_NO3 <- overall_results$final_kgNO3_ha - overall_results$initial_kgNO3_ha
summary(overall_results$delta_NO3)
overall_results$N_export_kgN_ha <-  overall_results$total_abovegrnd_biomass_N_kg_ha*N_harvest_efficiency + overall_results$NO3_leached_kgN_ha + overall_results$denitrification_kgN_ha + overall_results$N_misc_loss_kgN_ha
summary(overall_results$N_export_kgN_ha/37)
overall_results$N_stock_change_kgN_ha <- overall_results$delta_NO3 + overall_results$delta_orgN 
summary(overall_results$N_stock_change_kgN_ha/37)
# overall_results$SHR[overall_results$SHR=='1. Coarse with no restrictions'] <- 'Region 1'
# overall_results$SHR[overall_results$SHR=='2. Loamy with no restrictions'] <- 'Region 2'
# overall_results$SHR[overall_results$SHR=='7. Shrink-swell'] <- 'Region 7'
# overall_results$taxpartclass_simplified <- ifelse(overall_results$taxpartclass %in% c('Sandy', 'Coarse-loamy'), 1, ifelse(overall_results$taxpartclass %in% c('Fine', 'Very-fine'), 3, 2))
table(overall_results$taxpartclass[overall_results$scenario=='Control'])
table(overall_results$taxpartclass_simplified[overall_results$scenario=='Control'])

#N balance check
N_balance <- data.frame(soil=overall_results$soil, scenario=overall_results$scenario, N_input=overall_results$initial_kgNO3_ha+overall_results$initial_orgN+overall_results$fert_app_kgN_ha, N_output=overall_results$final_kgNO3_ha+overall_results$final_orgN+overall_results$NO3_leached_kgN_ha+overall_results$volatilized_kgN_ha+overall_results$denitrification_kgN_ha+overall_results$total_abovegrnd_biomass_N_kg_ha*N_harvest_efficiency+overall_results$N_misc_loss_kgN_ha)
N_balance$check <- N_balance$N_input - N_balance$N_output
summary(N_balance$check)
N_balance[N_balance$soil=='Kimberlina' & N_balance$scenario=='Control',]
#make barplot comparing controls vs. January 7-d interval Flood-MAR
if(!dir.exists(file.path(FiguresDir, met_stn, 'Boxplots by SHR'))) {
  dir.create(file.path(FiguresDir, met_stn, 'Boxplots by SHR'))
}
if(!dir.exists(file.path(FiguresDir, met_stn, 'Boxplots by SHR', 'Control vs. 21d'))) {
  dir.create(file.path(FiguresDir, met_stn, 'Boxplots by SHR', 'Control vs. 21d'))
}
if(!dir.exists(file.path(FiguresDir, met_stn, 'Boxplots by SHR', 'Control vs. All'))) {
  dir.create(file.path(FiguresDir, met_stn, 'Boxplots by SHR', 'Control vs. All'))
}
if(!dir.exists(file.path(FiguresDir, met_stn, 'Boxplots by SHR', 'Flood-MAR effect'))) {
  dir.create(file.path(FiguresDir, met_stn, 'Boxplots by SHR', 'Flood-MAR effect'))
}
if(!dir.exists(file.path(FiguresDir, met_stn, 'Boxplots by SHR', 'Residual NO3'))) {
  dir.create(file.path(FiguresDir, met_stn, 'Boxplots by SHR', 'Residual NO3'))
}
if(!dir.exists(file.path(FiguresDir, met_stn, 'Boxplots by PSC'))) {
  dir.create(file.path(FiguresDir, met_stn, 'Boxplots by PSC'))
}
if(!dir.exists(file.path(FiguresDir, met_stn, 'Boxplots by PSC', 'Control vs. 21d'))) {
  dir.create(file.path(FiguresDir, met_stn, 'Boxplots by PSC', 'Control vs. 21d'))
}
if(!dir.exists(file.path(FiguresDir, met_stn, 'Boxplots by PSC', 'Control vs. All'))) {
  dir.create(file.path(FiguresDir, met_stn, 'Boxplots by PSC', 'Control vs. All'))
}
if(!dir.exists(file.path(FiguresDir, met_stn, 'Boxplots by PSC', 'Flood-MAR effect'))) {
  dir.create(file.path(FiguresDir, met_stn, 'Boxplots by PSC', 'Flood-MAR effect'))
}
if(!dir.exists(file.path(FiguresDir, met_stn, 'Boxplots by PSC', 'Residual NO3'))) {
  dir.create(file.path(FiguresDir, met_stn, 'Boxplots by PSC', 'Residual NO3'))
}
# test <- test <- boxplot(NO3_leached_kgN_ha / 37 ~ SHR, data= overall_results, plot = FALSE, subset = scenario == 'Control')
# test2 <- boxplot(NO3_leached_kgN_ha / 37 ~ SHR, data= overall_results, plot = FALSE, subset = scenario == 'Jan3d')
# ymax <- max(test$stats, test2$stats, test$out, test2$out)

make_a_boxplot <- function(fname, varname, yaxis_name, lwd_arg=0.8, leg_pos='topleft', scalar=37, groupname, subDir, make_legend=FALSE, boxwex_arg=0.15, ymin_arg, fig_lab, ylab_pos=NULL, place_ylab=FALSE, outcex_arg=0.8, medlwd_arg=0.6) {
  test <- boxplot(get(varname) / scalar ~ get(groupname), data= overall_results, plot = FALSE, subset = scenario == 'Control')
  test2 <- boxplot(get(varname) / scalar ~ get(groupname), data= overall_results, plot = FALSE, subset = scenario == '21d')
  ymax <- max(test$stats, test2$stats, test$out, test2$out)
  if(!place_ylab) {ylab_pos <- ymax/2}
  tiff(file = file.path(FiguresDir, met_stn, subDir, 'Control vs. 21d', paste0(fname, met_stn, '.tif')), family = 'Times New Roman', width = 2.25, height = 3, pointsize = 12, units = 'in', res=800, compression='lzw')
  par(mar=c(1.5, 3.75, 0.5, 0.5), xpd=TRUE)
  boxplot(get(varname) / scalar ~ get(groupname), data= overall_results, xlab='', ylab = '', names=c('', '', ''), subset = scenario == 'Control', boxwex=boxwex_arg, staplewex=0.4, at=c(0.4,1,1.6) - 0.1, xlim = c(0.2,1.8), ylim=c(ymin_arg,ymax), col='orange', xaxt='n', lwd=lwd_arg, cex.axis=0.85, outcex=outcex_arg, medlwd=medlwd_arg)
  boxplot(get(varname) / scalar ~ get(groupname), data= overall_results, add=TRUE, xlab='', ylab = '', names=c('', '', ''), subset = scenario == '21d', boxwex=boxwex_arg, staplewex=0.4, at=c(0.4,1,1.6) + 0.1, xlim = c(0.2,1.8), col='turquoise1', xaxt='n', lwd=lwd_arg, cex.axis=0.85, outcex=outcex_arg, medlwd=medlwd_arg)
  mtext(yaxis_name, side=2, line=2.25, at=ylab_pos, cex = 0.9)
  mtext(c('Coarse', 'Loamy', 'Fine'), side = 1, at=c(0.4,1.05,1.6), line = 0.1, cex = 0.9)
  if(make_legend) {
    legend(leg_pos, legend=c('no Flood-MAR', 'Flood-MAR(21d)'), bty='n', fill = c('orange', 'turquoise1'), border='black', cex = 0.9, inset = c(-0.05,0), x.intersp = 0.4)
  }
  legend('topright', fig_lab, inset=-0.02, bty = 'n')
  dev.off()
}

#boxplots by SHR were not updated with late June 2022 model runs
# make_a_boxplot(fname = 'aboveground_biomass_', varname = 'mean_abovegrnd_biomass_kg_ha', yaxis_name = expression('Aboveground biomass yield (kg ha'^-1~'yr'^-1*')'), groupname = 'SHR', subDir = 'Boxplots by SHR', scalar = 1, ymin_arg = 14500, fig_lab = 'e', ylab_pos = 15200, place_ylab = TRUE)
# make_a_boxplot(fname = 'orgN_delta_', varname = 'delta_orgN', yaxis_name = expression('Change in soil organic N (kg N ha'^-1~'yr'^-1*')'), scalar=37, groupname = 'SHR', subDir = 'Boxplots by SHR', ymin_arg = -10, ylab_pos = 15, fig_lab = '', place_ylab = TRUE)
# make_a_boxplot(fname = 'nitrate_leached_', varname = 'NO3_leached_kgN_ha', yaxis_name = expression('Nitrate leached (kg N ha'^-1~'yr'^-1*')'), groupname = 'SHR', subDir = 'Boxplots by SHR', make_legend = TRUE, ymin_arg = 0, fig_lab = '', leg_pos = 'bottomleft')
# make_a_boxplot(fname = 'denitrification_', varname = 'denitrification_kgN_ha', yaxis_name = expression('Denitrification (kg N ha'^-1~'yr'^-1*')'), groupname = 'SHR', subDir = 'Boxplots by SHR', ymin_arg = 0, fig_lab = '')
# make_a_boxplot(fname = 'net_mineralization_', varname = 'net_min_kgN_ha', yaxis_name = expression('Net mineralization (kg N ha'^-1~'yr'^-1*')'), leg_pos = 'bottomright', groupname = 'SHR', subDir = 'Boxplots by SHR', ymin_arg = 0, fig_lab = '')
# make_a_boxplot(fname = 'water_quality_', varname = 'NO3_ppm', yaxis_name = 'Nitrate in deep percolation (ppm)', scalar=1, groupname = 'SHR', subDir = 'Boxplots by SHR', ymin_arg = 0, fig_lab = '')
# make_a_boxplot(fname = 'gross_mineralization_', varname = 'N_min_kgN_ha', yaxis_name = expression('Gross mineralization (kg N ha'^-1~'yr'^-1*')'), scalar=37, leg_pos = 'bottomright', groupname = 'SHR', subDir = 'Boxplots by SHR', ymin_arg = 0, fig_lab = '')
# make_a_boxplot(fname = 'deep_percolation_', varname = 'DP_cm', yaxis_name = 'Deep percolation (cm)', scalar=1, groupname = 'SHR', subDir = 'Boxplots by SHR', ymin_arg = 0, fig_lab = '')
# make_a_boxplot(fname = 'totalC_final_', varname = 'final_totalsoilMgC_ha', yaxis_name = expression('Total SOC (Mg C ha'^-1~')'), scalar=1, groupname = 'SHR', subDir = 'Boxplots by SHR', ymin_arg = 0, fig_lab = '')
# make_a_boxplot(fname = 'totalC_delta_', varname = 'delta_SOC', yaxis_name = expression('Change in SOC (Mg C ha'^-1~'yr'^-1~')'), scalar=37, groupname = 'SHR', subDir = 'Boxplots by SHR', ymin_arg = -0.15, fig_lab = '', ylab_pos = 0.15, place_ylab = TRUE)
# make_a_boxplot(fname = 'runoff_', varname = 'runoff_cm', yaxis_name = expression('Runoff (cm yr'^-1~')'), scalar=37, groupname = 'SHR', subDir = 'Boxplots by SHR', ymin_arg = 0, fig_lab = '')
# make_a_boxplot(fname = 'volatilized_', varname = 'volatilized_kgN_ha', yaxis_name = expression('Volatilized N (kg N ha'^-1~'yr'^-1*')'), scalar=37, groupname = 'SHR', subDir = 'Boxplots by SHR', ymin_arg = 0, fig_lab = '')

#boxplots by PSC
# make_a_boxplot(fname = 'aboveground_biomass_', varname = 'mean_abovegrnd_biomass_kg_ha', yaxis_name = expression('Aboveground biomass yield (kg ha'^-1~'yr'^-1*')'), groupname = 'taxpartclass_simplified', subDir = 'Boxplots by PSC', scalar = 1, ymin_arg = 14000, fig_lab = '', place_ylab = T, ylab_pos = 14600)
make_a_boxplot(fname = 'biomass_N_export_', varname = 'total_abovegrnd_biomass_N_kg_ha', yaxis_name = expression('Biomass N export (kg N ha'^-1~'yr'^-1*')'), scalar=37*N_harvest_efficiency, groupname = 'taxpartclass_simplified', subDir = 'Boxplots by PSC', ymin_arg = 145, fig_lab = 'c', ylab_pos = 170, place_ylab = TRUE)
make_a_boxplot(fname = 'deep_percolation_', varname = 'DP_cm', yaxis_name = 'Total deep percolation (cm)', scalar=1, groupname = 'taxpartclass_simplified', subDir = 'Boxplots by PSC', ymin_arg = 0, fig_lab = 'g')
# make_a_boxplot(fname = 'denitrification_', varname = 'denitrification_kgN_ha', yaxis_name = expression('Denitrification (kg N ha'^-1~'yr'^-1*')'), groupname = 'taxpartclass_simplified', subDir = 'Boxplots by PSC', ymin_arg = 0, fig_lab = '')
# make_a_boxplot(fname = 'gross_mineralization_', varname = 'N_min_kgN_ha', yaxis_name = expression('Gross mineralization (kg N ha'^-1~'yr'^-1*')'), scalar=37, leg_pos = 'bottomright', groupname = 'taxpartclass_simplified', subDir = 'Boxplots by PSC', ymin_arg = 0, fig_lab = '')
make_a_boxplot(fname = 'net_mineralization_', varname = 'net_min_kgN_ha', yaxis_name = expression('Net mineralization (kg N ha'^-1~'yr'^-1*')'), leg_pos = 'bottomright', groupname = 'taxpartclass_simplified', subDir = 'Boxplots by PSC', ymin_arg = 93, fig_lab = 'b', ylab_pos = 120, place_ylab = TRUE)
make_a_boxplot(fname = 'nitrate_leached_', varname = 'NO3_leached_kgN_ha', yaxis_name = expression('Nitrate leached (kg N ha'^-1~'yr'^-1*')'), groupname = 'taxpartclass_simplified', subDir = 'Boxplots by PSC', make_legend = TRUE, ymin_arg = 0, fig_lab = 'a', leg_pos = 'bottomleft')
make_a_boxplot(fname = 'orgN_delta_', varname = 'delta_orgN', yaxis_name = expression('Change in soil organic N (kg N ha'^-1~'yr'^-1*')'), scalar=37, groupname = 'taxpartclass_simplified', subDir = 'Boxplots by PSC', ymin_arg = -10, fig_lab = 'd', ylab_pos = 12, place_ylab = TRUE)
make_a_boxplot(fname = 'runoff_', varname = 'runoff_cm', yaxis_name = expression('Total runoff (cm)'), scalar=1, groupname = 'taxpartclass_simplified', subDir = 'Boxplots by PSC', ymin_arg = 0, fig_lab = 'f')
# make_a_boxplot(fname = 'totalC_delta_', varname = 'delta_SOC', yaxis_name = expression('Change in soil organic C (Mg C ha'^-1~'yr'^-1*')'), scalar=37, groupname = 'taxpartclass_simplified', subDir = 'Boxplots by PSC', ymin_arg = -0.1, fig_lab = '', ylab_pos = 0.17, place_ylab = TRUE)
# make_a_boxplot(fname = 'totalC_final_', varname = 'final_totalsoilMgC_ha', yaxis_name = expression('Total soil organic C (Mg C ha'^-1*')'), scalar=1, groupname = 'taxpartclass_simplified', subDir = 'Boxplots by PSC', ymin_arg = 0, fig_lab = '')
make_a_boxplot(fname = 'volatilized_', varname = 'volatilized_kgN_ha', yaxis_name = expression('Volatilized N (kg N ha'^-1~'yr'^-1*')'), scalar=37, groupname = 'taxpartclass_simplified', subDir = 'Boxplots by PSC', ymin_arg = 0, fig_lab = 'e')
make_a_boxplot(fname = 'water_quality_', varname = 'NO3_ppm', yaxis_name = 'Nitrate in deep percolation (ppm)', scalar=1, groupname = 'taxpartclass_simplified', subDir = 'Boxplots by PSC', ymin_arg = 0, fig_lab = 'h')

#assemble change in NO3 leaching across Ag-MAR treatments
#boxplot version for Ag-MAR treatment differences
make_a_boxplot_AgMAR_comp <- function(fname, varname, yaxis_name, lwd_arg=0.8, leg_pos='topleft', scalar=37, groupname, subDir, plot_colors=c('deepskyblue', 'deepskyblue4', 'turquoise1', 'springgreen', 'springgreen4'), boxwex_arg=0.13, group_placement=c(0.9,2,3.1), fig_lab, make_legend=FALSE, ymax_arg, outcex_arg=0.8, medlwd_arg=0.6) {
  test <- boxplot(get(varname) / scalar ~ get(groupname), data= overall_results, plot = FALSE, subset = scenario == 'Jan3d')
  test2 <- boxplot(get(varname) / scalar ~ get(groupname), data= overall_results, plot = FALSE, subset = scenario == 'Jan7d')
  test3 <- boxplot(get(varname) / scalar ~ get(groupname), data= overall_results, plot = FALSE, subset = scenario == 'Mar3d')
  test4 <- boxplot(get(varname) / scalar ~ get(groupname), data= overall_results, plot = FALSE, subset = scenario == 'Mar7d')
  test5 <- boxplot(get(varname) / scalar ~ get(groupname), data= overall_results, plot = FALSE, subset = scenario == '21d')
  ymax <- max(test$stats, test2$stats, test3$stats, test4$stats, test5$stats, test$out, test2$out, test3$out, test4$out, test5$out)
  ymax <- ymax_arg #determined manually to combine climate figures with same scale
  tiff(file = file.path(FiguresDir, met_stn, subDir, 'Flood-MAR effect', paste0(fname, met_stn, '_AgMARcomparion.tif')), family = 'Times New Roman', width = 2.5, height = 3.5, pointsize = 12, units = 'in', res=800, compression='lzw')
  par(mar=c(1.2, 3.1, 0.5, 0.5), xpd=TRUE, mgp=c(3,0.5,0), tcl=-0.4)
  boxplot(get(varname) / scalar ~ get(groupname), data= overall_results, xlab='', ylab = '', names=c('', '', ''), subset = scenario == 'Jan3d', boxwex=boxwex_arg, staplewex=0.4, at=group_placement - 0.4, xlim = c(0.5, 3.5), ylim=c(0,ymax), col=plot_colors[1], xaxt='n', lwd=lwd_arg, outcex=outcex_arg, medlwd=medlwd_arg)
  boxplot(get(varname) / scalar ~ get(groupname), data= overall_results, add=TRUE, xlab='', ylab = '', names=c('', '', ''), subset = scenario == 'Jan7d', boxwex=boxwex_arg, staplewex=0.4, at=group_placement - 0.2, xlim = c(0.5, 3.5), col=plot_colors[2], xaxt='n', lwd=lwd_arg, outcex=outcex_arg, medlwd=medlwd_arg)
  boxplot(get(varname) / scalar ~ get(groupname), data= overall_results, add=TRUE, xlab='', ylab = '', names=c('', '', ''), subset = scenario == '21d', boxwex=boxwex_arg, staplewex=0.4, at=group_placement, xlim = c(0.5, 3.5), col=plot_colors[3], xaxt='n', lwd=lwd_arg, outcex=outcex_arg, medlwd=medlwd_arg)
  boxplot(get(varname) / scalar ~ get(groupname), data= overall_results, add=TRUE, xlab='', ylab = '', names=c('', '', ''), subset = scenario == 'Mar3d', boxwex=boxwex_arg, staplewex=0.4, at=group_placement+0.2, xlim = c(0.5, 3.5), col=plot_colors[4], xaxt='n', lwd=lwd_arg, outcex=outcex_arg, medlwd=medlwd_arg)
  boxplot(get(varname) / scalar ~ get(groupname), data= overall_results, add=TRUE, xlab='', ylab = '', names=c('', '', ''), subset = scenario == 'Mar3d', boxwex=boxwex_arg, staplewex=0.4, at=group_placement+0.4, xlim = c(0.5, 3.5), col=plot_colors[5], xaxt='n', lwd=lwd_arg, outcex=outcex_arg, medlwd=medlwd_arg)
  mtext(yaxis_name, side=2, line=1.75, cex = 1)
  mtext(c('Coarse', 'Loamy', 'Fine'), side = 1, at=group_placement, line = 0.1, cex=1)
  if(make_legend) {
    legend(leg_pos, legend=c('Jan(3d)', 'Jan(7d)', 'Jan-Mar(21d)', 'Mar(3d)', 'Mar(7d)'), bty='n', fill = plot_colors, border='black', cex = 0.9, x.intersp = 0.4, inset = c(-0.02, 0.04))
  }
  legend('topleft', fig_lab, bty='n', inset = c(-0.14,-0.03), xjust=0)
  dev.off()
}
make_a_boxplot_AgMAR_comp(fname = 'total_delta_nitrate_leached_', varname = 'NO3_leached_delta_kgN_ha', yaxis_name = expression('Additional NO'[3]~'leached (kg N ha'^-1*')'), groupname = 'taxpartclass_simplified', subDir = 'Boxplots by PSC', scalar = 1, fig_lab = met_stn, ymax_arg = 5000, make_legend = F) #860 kg N for Durham and Davis; 5000 kg for Parlier, Five Points, and Shafter; for annualized y-axis: expression('Additional NO'[3]~'leached (kg N ha'^-1~'yr'^-1*')')
# make_a_boxplot_AgMAR_comp(fname = 'delta_nitrate_leached_', varname = 'NO3_leached_delta_kgN_ha', yaxis_name = expression('Additional NO'[3]~'leached (kg N ha'^-1~'yr'^-1*')'), groupname = 'SHR', subDir = 'Boxplots by SHR', scalar = 37, fig_lab = 'a')

#assemble change in NO3 leaching across Ag-MAR treatments
#boxplot version for Ag-MAR treatment differences
make_a_boxplot_all_comp <- function(fname, varname, yaxis_name, lwd_arg=0.8, leg_pos='topleft', scalar=37, groupname, subDir, plot_colors=c('orange1', 'deepskyblue', 'deepskyblue4', 'turquoise1', 'springgreen', 'springgreen4'), boxwex_arg=0.13, ylab_placement=NULL, group_placement=c(0.7,2,3.3), ymin_arg=0, make_legend=FALSE, place_ylab=FALSE, fig_lab, ymax_manual=FALSE, ymax_arg, outcex_arg=0.8, medlwd_arg=0.6) {
  test <- boxplot(get(varname) / scalar ~ get(groupname), data= overall_results, plot = FALSE, subset = scenario == 'Jan3d')
  test2 <- boxplot(get(varname) / scalar ~ get(groupname), data= overall_results, plot = FALSE, subset = scenario == 'Jan7d')
  test3 <- boxplot(get(varname) / scalar ~ get(groupname), data= overall_results, plot = FALSE, subset = scenario == 'Mar3d')
  test4 <- boxplot(get(varname) / scalar ~ get(groupname), data= overall_results, plot = FALSE, subset = scenario == 'Mar7d')
  test5 <- boxplot(get(varname) / scalar ~ get(groupname), data= overall_results, plot = FALSE, subset = scenario == '21d')
  test6 <- boxplot(get(varname) / scalar ~ get(groupname), data= overall_results, plot = FALSE, subset = scenario == 'Control')
  ymax <- max(test$stats, test2$stats, test3$stats, test4$stats, test5$stats, test6$stats, test$out, test2$out, test3$out, test4$out, test5$out, test6$out)
  if(ymax_manual) {
    ymax <- ymax_arg
  }
  if(!place_ylab) {ylab_placement <- ymax/2}
  tiff(file = file.path(FiguresDir, met_stn, subDir, 'Control vs. All', paste0(fname, met_stn, '_AgMARcomparion.tif')), family = 'Times New Roman', width = 2.5, height = 3.5, pointsize = 12, units = 'in', res=800, compression='lzw')
  par(mar=c(1.2, 3.1, 0.5, 0.5), xpd=TRUE, mgp=c(3,0.5,0), tcl=-0.4)
  boxplot(get(varname) / scalar ~ get(groupname), data= overall_results, xlab='', ylab = '', names=c('', '', ''), subset = scenario == 'Control', boxwex=boxwex_arg, staplewex=0.4, at=group_placement - 0.6, xlim = c(0, 3.8), ylim=c(ymin_arg,ymax), col=plot_colors[1], xaxt='n', lwd=lwd_arg, outcex=outcex_arg, medlwd=medlwd_arg)
  boxplot(get(varname) / scalar ~ get(groupname), data= overall_results, add=TRUE, xlab='', ylab = '', names=c('', '', ''), subset = scenario == 'Jan3d', boxwex=boxwex_arg, staplewex=0.4, at=group_placement - 0.4, xlim = c(0, 3.8), ylim=c(0,ymax), col=plot_colors[2], xaxt='n', lwd=lwd_arg, outcex=outcex_arg, medlwd=medlwd_arg)
  boxplot(get(varname) / scalar ~ get(groupname), data= overall_results, add=TRUE, xlab='', ylab = '', names=c('', '', ''), subset = scenario == 'Jan7d', boxwex=boxwex_arg, staplewex=0.4, at=group_placement - 0.2, xlim = c(0, 3.8), col=plot_colors[3], xaxt='n', lwd=lwd_arg, outcex=outcex_arg, medlwd=medlwd_arg)
  boxplot(get(varname) / scalar ~ get(groupname), data= overall_results, add=TRUE, xlab='', ylab = '', names=c('', '', ''), subset = scenario == '21d', boxwex=boxwex_arg, staplewex=0.4, at=group_placement, xlim = c(0, 3.8), col=plot_colors[4], xaxt='n', lwd=lwd_arg, outcex=outcex_arg, medlwd=medlwd_arg)
  boxplot(get(varname) / scalar ~ get(groupname), data= overall_results, add=TRUE, xlab='', ylab = '', names=c('', '', ''), subset = scenario == 'Mar3d', boxwex=boxwex_arg, staplewex=0.4, at=group_placement+0.2, xlim = c(0, 3.8), col=plot_colors[5], xaxt='n', lwd=lwd_arg, outcex=outcex_arg, medlwd=medlwd_arg)
  boxplot(get(varname) / scalar ~ get(groupname), data= overall_results, add=TRUE, xlab='', ylab = '', names=c('', '', ''), subset = scenario == 'Mar3d', boxwex=boxwex_arg, staplewex=0.4, at=group_placement+0.4, xlim = c(0, 3.8), col=plot_colors[6], xaxt='n', lwd=lwd_arg, outcex=outcex_arg, medlwd=medlwd_arg)
  mtext(yaxis_name, side=2, line=1.75, at=ylab_placement, cex=1)
  mtext(c('Coarse', 'Loamy', 'Fine'), side = 1, at=group_placement-0.1, line = 0.1, cex=1)
  if(make_legend){legend(leg_pos, legend=c('no Flood-MAR', 'Jan(3d)', 'Jan(7d)', 'Jan-Mar(21d)', 'Mar(3d)', 'Mar(7d)'), bty='n', fill = plot_colors, border='black', cex = 0.9, x.intersp = 0.4, inset = c(-0.02, 0.04))}
  legend('topleft', fig_lab, bty = 'n', inset = c(-0.14,-0.03), xjust=0)
  dev.off()
}
#by PSC, total
make_a_boxplot_all_comp(fname = 'total_denitrification_', varname = 'denitrification_kgN_ha', yaxis_name = expression('Denitrification (kg N ha'^-1*')'), groupname = 'taxpartclass_simplified', subDir = 'Boxplots by PSC', scalar = 1, fig_lab = met_stn, ymax_manual = TRUE, ymax_arg = 2990, make_legend = F) #, ylab_placement = 15

#by PSC
make_a_boxplot_all_comp(fname = 'aboveground_biomass_', varname = 'mean_abovegrnd_biomass_kg_ha', yaxis_name = expression('Aboveground biomass yield (kg ha'^-1~'yr'^-1*')'), groupname = 'taxpartclass_simplified', subDir = 'Boxplots by PSC', scalar = 1, ylab_placement = 14600, ymin_arg = 14000, place_ylab = TRUE, fig_lab = '')
make_a_boxplot_all_comp(fname = 'denitrification_', varname = 'denitrification_kgN_ha', yaxis_name = expression('Denitrification (kg N ha'^-1~'yr'^-1*')'), groupname = 'taxpartclass_simplified', subDir = 'Boxplots by PSC', scalar = 37, fig_lab = met_stn, ymax_manual = TRUE, ymax_arg = 83, make_legend = T) #, ylab_placement = 15
make_a_boxplot_all_comp(fname = 'gross_mineralization_', varname = 'N_min_kgN_ha', yaxis_name = expression('Gross mineralization (kg N ha'^-1~'yr'^-1*')'), scalar=37, leg_pos = 'bottomright', groupname = 'taxpartclass_simplified', subDir = 'Boxplots by PSC', fig_lab = '') #, ylab_placement=75
make_a_boxplot_all_comp(fname = 'net_mineralization_', varname = 'net_min_kgN_ha', yaxis_name = expression('Net mineralization (kg N ha'^-1~'yr'^-1*')'), leg_pos = 'bottomright', groupname = 'taxpartclass_simplified', subDir = 'Boxplots by PSC', scalar = 37, fig_lab = 'c', ylab_placement = 115, ymin_arg = 85, place_ylab = TRUE) #, ylab_placement=50
make_a_boxplot_all_comp(fname = 'nitrate_leached_', varname = 'NO3_leached_kgN_ha', yaxis_name = expression('Nitrate leached (kg N ha'^-1~'yr'^-1*')'), groupname = 'taxpartclass_simplified', subDir = 'Boxplots by PSC', make_legend = TRUE, fig_lab = '', leg_pos = 'bottomleft') #, ylab_placement = 35
make_a_boxplot_all_comp(fname = 'orgN_delta_', varname = 'delta_orgN', yaxis_name = expression('Change in soil organic N (kg N ha'^-1~'yr'^-1*')'), scalar=37, groupname = 'taxpartclass_simplified', subDir = 'Boxplots by PSC', ylab_placement = 12, ymin_arg = -10, place_ylab = TRUE, fig_lab = '') #, ylab_placement = 50
make_a_boxplot_all_comp(fname = 'runoff_', varname = 'runoff_cm', yaxis_name = expression('Runoff (cm yr'^-1~')'), scalar=37, groupname = 'taxpartclass_simplified', subDir = 'Boxplots by PSC', ymin_arg = 0, fig_lab = '')
make_a_boxplot_all_comp(fname = 'totalC_delta_', varname = 'delta_SOC', yaxis_name = expression('Change in SOC (Mg C ha'^-1~'yr'^-1~')'), scalar=37, groupname = 'taxpartclass_simplified', subDir = 'Boxplots by PSC', ymin_arg = -0.1, place_ylab = TRUE, ylab_placement = 0.15, fig_lab = '')
make_a_boxplot_all_comp(fname = 'totalC_final_', varname = 'final_totalsoilMgC_ha', yaxis_name = expression('Total SOC (Mg C ha'^-1~')'), scalar=1, groupname = 'taxpartclass_simplified', subDir = 'Boxplots by PSC', fig_lab = '')
make_a_boxplot_all_comp(fname = 'volatilized_', varname = 'volatilized_kgN_ha', yaxis_name = expression('Volatilized N (kg N ha'^-1~'yr'^-1*')'), scalar=37, groupname = 'taxpartclass_simplified', subDir = 'Boxplots by PSC', ymin_arg = 0, fig_lab = '')
make_a_boxplot_all_comp(fname = 'water_quality_', varname = 'NO3_ppm', yaxis_name = 'Nitrate in deep percolation (ppm)', scalar=1, groupname = 'taxpartclass_simplified', subDir = 'Boxplots by PSC', fig_lab = '') #, ylab_placement = 250

#by SHR
# make_a_boxplot_all_comp(fname = 'aboveground_biomass_', varname = 'mean_abovegrnd_biomass_kg_ha', yaxis_name = expression('Aboveground biomass yield (kg ha'^-1~'yr'^-1*')'), groupname = 'SHR', subDir = 'Boxplots by SHR', scalar = 1, ylab_placement = 14500, ymin_arg = 14000, place_ylab = TRUE, fig_lab = '')
# make_a_boxplot_all_comp(fname = 'orgN_delta_', varname = 'delta_orgN', yaxis_name = expression('Change in soil organic N (kg N ha'^-1~'yr'^-1*')'), scalar=37, groupname = 'SHR', subDir = 'Boxplots by SHR', ylab_placement = 20, ymin_arg = -10, place_ylab = TRUE, fig_lab = '') #, ylab_placement = 50
# make_a_boxplot_all_comp(fname = 'nitrate_leached_', varname = 'NO3_leached_kgN_ha', yaxis_name = expression('Nitrate leached (kg N ha'^-1~'yr'^-1*')'), groupname = 'SHR', subDir = 'Boxplots by SHR', make_legend = TRUE, fig_lab = '', leg_pos = 'bottomleft') #, ylab_placement = 35
# make_a_boxplot_all_comp(fname = 'denitrification_', varname = 'denitrification_kgN_ha', yaxis_name = expression('Denitrification (kg N ha'^-1~'yr'^-1*')'), groupname = 'SHR', subDir = 'Boxplots by SHR', scalar = 37, fig_lab = '') #, ylab_placement = 15
# make_a_boxplot_all_comp(fname = 'net_mineralization_', varname = 'net_min_kgN_ha', yaxis_name = expression('Net mineralization (kg N ha'^-1~'yr'^-1*')'), leg_pos = 'bottomright', groupname = 'SHR', subDir = 'Boxplots by SHR', scalar = 37, fig_lab = '') #, ylab_placement=50
# make_a_boxplot_all_comp(fname = 'water_quality_', varname = 'NO3_ppm', yaxis_name = 'Nitrate in deep percolation (ppm)', scalar=1, groupname = 'SHR', subDir = 'Boxplots by SHR', fig_lab = '') #, ylab_placement = 250
# make_a_boxplot_all_comp(fname = 'gross_mineralization_', varname = 'N_min_kgN_ha', yaxis_name = expression('Gross mineralization (kg N ha'^-1~'yr'^-1*')'), scalar=37, leg_pos = 'bottomright', groupname = 'SHR', subDir = 'Boxplots by SHR', fig_lab = '') #, ylab_placement=75
# make_a_boxplot_all_comp(fname = 'deep_percolation_', varname = 'DP_cm', yaxis_name = 'Deep percolation (cm)', scalar=1, groupname = 'SHR', subDir = 'Boxplots by SHR', fig_lab = '') #, ylab_placement = 15
# make_a_boxplot_all_comp(fname = 'totalC_final_', varname = 'final_totalsoilMgC_ha', yaxis_name = expression('Total SOC (Mg C ha'^-1~')'), scalar=1, groupname = 'SHR', subDir = 'Boxplots by SHR', fig_lab = '')
# make_a_boxplot_all_comp(fname = 'totalC_delta_', varname = 'delta_SOC', yaxis_name = expression('Change in SOC (Mg C ha'^-1~'yr'^-1~')'), scalar=37, groupname = 'SHR', subDir = 'Boxplots by SHR', ymin_arg = -0.1, ylab_placement = 0.15, place_ylab = TRUE, fig_lab = '')
# make_a_boxplot_all_comp(fname = 'runoff_', varname = 'runoff_cm', yaxis_name = expression('Runoff (cm yr'^-1~')'), scalar=37, groupname = 'SHR', subDir = 'Boxplots by SHR', ymin_arg = 0, fig_lab = '')
# make_a_boxplot_all_comp(fname = 'volatilized_', varname = 'volatilized_kgN_ha', yaxis_name = expression('Volatilized N (kg N ha'^-1~'yr'^-1*')'), scalar=37, groupname = 'SHR', subDir = 'Boxplots by SHR', ymin_arg = 0, fig_lab = '')

#boxplot of Control's residual nitrate, initial and final
make_a_boxplot_res_NO3 <- function(fname, varname='initial_kgNO3_ha', varname2='final_kgNO3_ha', yaxis_name, lwd_arg=0.8, leg_pos='topleft', scalar=37, groupname, subDir, make_legend=FALSE, boxwex_arg=0.15, ymin_arg, fix_ymax=FALSE, ymax_arg=NULL, fig_label=met_stn) {
  test <- boxplot(get(varname) / scalar ~ get(groupname), data=overall_results, plot = FALSE, subset = scenario == 'Control')
  test2 <- boxplot(get(varname2) / scalar ~ get(groupname), data= overall_results, plot = FALSE, subset = scenario == 'Control')
  if(!fix_ymax) {ymax <- max(test$stats, test2$stats, test$out, test2$out)} else{ymax<-ymax_arg}
  tiff(file = file.path(FiguresDir, met_stn, subDir, 'Residual NO3', paste0(fname, met_stn, '.tif')), family = 'Times New Roman', width = 2.5, height = 3.25, pointsize = 12, units = 'in', res=800, compression='lzw')
  par(mar=c(1.25, 4, 0.5, 0.5), xpd=TRUE)
  boxplot(get(varname) / scalar ~ get(groupname), data= overall_results, xlab='', ylab = '', names=c('', '', ''), subset = scenario == 'Control', boxwex=boxwex_arg, staplewex=0.4, at=c(0.4,1,1.6) - 0.1, xlim = c(0.2,1.8), ylim=c(ymin_arg,ymax), col='orange', xaxt='n', lwd=lwd_arg, cex.axis=0.85)
  boxplot(get(varname2) / scalar ~ get(groupname), data= overall_results, add=TRUE, xlab='', ylab = '', names=c('', '', ''), subset = scenario == 'Control', boxwex=boxwex_arg, staplewex=0.4, at=c(0.4,1,1.6) + 0.1, xlim = c(0.2,1.8), col='orange3', xaxt='n', lwd=lwd_arg, cex.axis=0.85)
  mtext(yaxis_name, side=2, line=2.25)
  mtext(c('Coarse', 'Loamy', 'Fine'), side = 1, at=c(0.4,1.05,1.6), line = 0.1)
  if(make_legend) {
    legend(leg_pos, legend=c('Preliminary', 'Final'), bty='n', fill = c('orange1', 'orange3'), border='black', cex = 0.85, x.intersp = 0.4)
  }
  legend('topright', fig_label, bty='n')
  dev.off()
}
# make_a_boxplot_res_NO3(fname = 'residual_NO3_', yaxis_name = expression('Residual nitrate (kg N ha'^-1~')'), groupname = 'SHR', subDir = 'Boxplots by SHR', make_legend = F, ymin_arg = 0, scalar = 1, fix_ymax = TRUE, ymax_arg = 250)
make_a_boxplot_res_NO3(fname = 'residual_NO3_', yaxis_name = expression('Residual nitrate (kg N ha'^-1~')'), groupname = 'taxpartclass_simplified', subDir = 'Boxplots by PSC', make_legend = T, ymin_arg = 0, scalar = 1, fix_ymax = TRUE, ymax_arg = 210, fig_label = met_stn)

# clims <- c('Parlier', 'FivePoints', 'Shafter')
# clims <- met_stn
# for(i in 1:length(clims)) {
#   overall_results <- read.csv(file.path(resultsDir, 'Summaries', paste0('overall_results_', clims[i], '.csv')), stringsAsFactors = FALSE)
#   met_stn <- clims[i]
#   make_a_boxplot_res_NO3(fname = 'residual_NO3_', yaxis_name = expression('Residual nitrate (kg N ha'^-1~')'), groupname = 'SHR', subDir = 'Boxplots by SHR', make_legend = F, ymin_arg = 0, scalar = 1, fix_ymax = TRUE, ymax_arg = 1500)
#   make_a_boxplot_res_NO3(fname = 'residual_NO3_', yaxis_name = expression('Residual nitrate (kg N ha'^-1~')'), groupname = 'taxpartclass_simplified', subDir = 'Boxplots by PSC', make_legend = F, ymin_arg = 0, scalar = 1, fix_ymax = TRUE, ymax_arg = 1500)
# }
# # clims <- c('Durham', 'Davis')
# for(i in 1:length(clims)) {
#   overall_results <- read.csv(file.path(resultsDir, 'Summaries', paste0('overall_results_', clims[i], '.csv')), stringsAsFactors = FALSE)
#   met_stn <- clims[i]
#   make_a_boxplot_res_NO3(fname = 'residual_NO3_', yaxis_name = expression('Residual nitrate (kg N ha'^-1~')'), groupname = 'SHR', subDir = 'Boxplots by SHR', make_legend = F, ymin_arg = 0, scalar = 1, fix_ymax = TRUE, ymax_arg = 52)
#   make_a_boxplot_res_NO3(fname = 'residual_NO3_', yaxis_name = expression('Residual nitrate (kg N ha'^-1~')'), groupname = 'taxpartclass_simplified', subDir = 'Boxplots by PSC', make_legend = F, ymin_arg = 0, scalar = 1, fix_ymax = TRUE, ymax_arg = 52)
# }
# 
