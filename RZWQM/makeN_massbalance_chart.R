#to-do: make barplot by soil name (typical coarse, loamy, and fine examples)
#mass balance approach needs revising to show unleached residual nitrate amounts in controls above the 9242 kg N dashed line
library(extrafont)
library(extrafontdb)
# font_import() #only needs to be done one time after updating and re-installing R and moving and updating packages
loadfonts(device = 'win')

# N_harvest_efficiency <- 0.977
# clims <- c('Durham', 'Davis', 'Parlier', 'FivePoints', 'Shafter')
workDir <- 'C:/Users/smdevine/Desktop/post doc/Dahlke/RZWQM/projects/PulseSoilClimate/ClimateRuns_Silage/'
FiguresDir <- file.path(workDir, 'Figures')
resultsDir <- file.path(workDir, 'Results')

N_balance_by_soil <- function(df, soilname, scenario_name) {
  subset(df, scenario == scenario_name & soil == soilname, select = c(soil, delta_orgN, biomass_export_kgN_ha, N_misc_loss_kgN_ha, volatilized_kgN_ha, denitrification_kgN_ha, NO3_leached_kgN_ha, delta_NO3, final_kgNO3_ha))
}

#read-in results file
met_stn <- 'Shafter'
if(!dir.exists(file.path(FiguresDir, met_stn, 'N mass balance'))) {
  dir.create(file.path(FiguresDir, met_stn, 'N mass balance'))
}
overall_results <- read.csv(file.path(resultsDir, 'Summaries', paste0('overall_results_', met_stn, '.csv')), stringsAsFactors = FALSE)
overall_results$N_misc_loss_kgN_ha <- overall_results$N2O_loss_nitri + overall_results$NxO_loss_nitri + overall_results$NxO_abs_nitri
overall_results$delta_NO3 <- overall_results$final_kgNO3_ha - overall_results$initial_kgNO3_ha
summary(overall_results$delta_NO3)

# overall_results$N_export_kgN_ha <-  overall_results$total_abovegrnd_biomass_N_kg_ha*N_harvest_efficiency + overall_results$NO3_leached_kgN_ha + overall_results$denitrification_kgN_ha + overall_results$N_misc_loss_kgN_ha
# summary(overall_results$N_export_kgN_ha/37)
# overall_results$N_stock_change_kgN_ha <- overall_results$delta_NO3 + overall_results$delta_orgN
# summary(overall_results$N_stock_change_kgN_ha/37)

overall_results$biomass_export_kgN_ha <- overall_results$initial_orgN + overall_results$initial_kgNO3_ha + 9243 - overall_results$final_orgN - overall_results$final_kgNO3_ha - overall_results$NO3_leached_kgN_ha - overall_results$volatilized_kgN_ha - overall_results$denitrification_kgN_ha - overall_results$N_misc_loss_kgN_ha
summary(overall_results$biomass_export_kgN_ha)

# get_median_by_PSC <- function(varname) {
#   result <- aggregate(get(varname) ~ taxpartclass_simplified, data = overall_results, FUN=median, subset = scenario == '21d')
#   colnames(result)[2] <- varname
#   result
# }
# FloodMAR_21d <- cbind(delta_SON=get_median_by_PSC('delta_orgN')[,2], biomass_export_kgN_ha= get_median_by_PSC('biomass_export_kgN_ha')[,2], misc_losses_kgN_ha=get_median_by_PSC('N_misc_loss_kgN_ha')[,2], denitrification_kgN_ha=get_median_by_PSC('denitrification_kgN_ha')[,2], NO3_leached_kgN_ha=get_median_by_PSC('NO3_leached_kgN_ha')[,2])
# barplot(t(FloodMAR_21d), beside = FALSE)
# abline(h=9243, lty=2)

coarse <- N_balance_by_soil(df=overall_results, soilname = 'Kimberlina', scenario_name = 'Control')
loamy <- N_balance_by_soil(df=overall_results, soilname = 'Cerini', scenario_name = 'Control')
fine <- N_balance_by_soil(df=overall_results, soilname = 'Merced', scenario_name = 'Control')
all_soils <- rbind(coarse, loamy, fine, make.row.names=FALSE, stringsAsFactors=FALSE)
all_soils
apply(all_soils[,2:7], 1, sum)
apply(all_soils[,2:8], 1, sum)
apply(all_soils[,2:9], 1, sum)

coarse <- N_balance_by_soil(df=overall_results, soilname = 'Kimberlina', scenario_name = '21d')
loamy <- N_balance_by_soil(df=overall_results, soilname = 'Cerini', scenario_name = '21d')
fine <- N_balance_by_soil(df=overall_results, soilname = 'Merced', scenario_name = '21d')
all_soils <- rbind(coarse, loamy, fine, make.row.names=FALSE, stringsAsFactors=FALSE)
all_soils
apply(all_soils[,2:7], 1, sum)
apply(all_soils[,2:8], 1, sum)
apply(all_soils[,2:9], 1, sum)

#duplicate delta NO3 column, make opposite, include as invisible vector in barchart, and then plot final residual NO3 above this as red hashmarked line
scn <- '21d'
makeN_massblnc_chart <- function(scn, make_legend=FALSE) {
  coarse <- N_balance_by_soil(df=overall_results, soilname = 'Kimberlina', scenario_name = scn)
  coarse <- cbind(coarse[,1:8], -coarse['delta_NO3'], coarse['final_kgNO3_ha'])
  colnames(coarse)[9] <- 'neg_delta_NO3'
  loamy <- N_balance_by_soil(df=overall_results, soilname = 'Cerini', scenario_name = scn)
  loamy <- cbind(loamy[,1:8], -loamy['delta_NO3'], loamy['final_kgNO3_ha'])
  colnames(loamy)[9] <- 'neg_delta_NO3'
  fine <- N_balance_by_soil(df=overall_results, soilname = 'Merced', scenario_name = scn)
  fine <- cbind(fine[,1:8], -fine['delta_NO3'], fine['final_kgNO3_ha'])
  colnames(fine)[9] <- 'neg_delta_NO3'
  all_soils <- rbind(coarse, loamy, fine, make.row.names=FALSE, stringsAsFactors=FALSE)
  print(apply(all_soils[,2:ncol(all_soils)], 1, sum))
#21d figure
  tiff(file = file.path(FiguresDir, met_stn, 'N mass balance', paste0('FloodMAR_', scn, '_', met_stn, '.tif')), family = 'Times New Roman', width = 3.5, height = 4.25, pointsize = 12, units = 'in', res=800, compression='lzw')
  par(mar=c(1.5, 3.75, 0.25, 0.25))
  bardims <- barplot(t(all_soils[,2:ncol(all_soils)]), names.arg = c('', '', ''), beside = FALSE, ylab='', col=c('tan4', 'springgreen2', 'yellow', 'orange2', 'violetred', 'grey', 'black', NULL, 'red'), density=c(rep(-0.1, 6), rep(20, 3)), angle = 45, ylim=c(0,11500), border = NA, lwd=0.8)
  mtext(expression('N mass balance (kg ha'^-1*')'), side=2, line=2.25)
  mtext(c('Coarse', 'Loamy', 'Fine'), at=bardims, side = 1, line = 0.5)
  if(make_legend) {
    legend('topleft', legend = c('SON accrual', 'biomass export', 'misc. losses', 'volatilization', 'denitrification', expression('NO'[3]~'leached')), ncol=2, fill=c('tan4', 'springgreen2', 'yellow', 'orange2', 'violetred', 'grey', 'grey4'), bty = 'n', inset = c(0,-0.02), border = NA, cex=0.9)
  }
  abline(h=9243, lty=2)
  dev.off()
}
makeN_massblnc_chart('21d')
makeN_massblnc_chart('Control')

makeN_massblnc_chart_vs_control <- function(scn, make_legend, xlim_max=2.9, ylim_max=11000, stn, stn_label, precip) {
  if(!dir.exists(file.path(FiguresDir, stn, 'N mass balance'))) {
    dir.create(file.path(FiguresDir, stn, 'N mass balance'))
  }
  overall_results <- read.csv(file.path(resultsDir, 'Summaries', paste0('overall_results_', stn, '.csv')), stringsAsFactors = FALSE)
  assign("overall_results", overall_results, envir = .GlobalEnv)
  overall_results$N_misc_loss_kgN_ha <- overall_results$N2O_loss_nitri + overall_results$NxO_loss_nitri + overall_results$NxO_abs_nitri
  overall_results$delta_NO3 <- overall_results$final_kgNO3_ha - overall_results$initial_kgNO3_ha
  overall_results$biomass_export_kgN_ha <- overall_results$initial_orgN + overall_results$initial_kgNO3_ha + 9243 - overall_results$final_orgN - overall_results$final_kgNO3_ha - overall_results$NO3_leached_kgN_ha - overall_results$volatilized_kgN_ha - overall_results$denitrification_kgN_ha - overall_results$N_misc_loss_kgN_ha
  coarse <- N_balance_by_soil(df=overall_results, soilname = 'Kimberlina', scenario_name = scn)
  loamy <- N_balance_by_soil(df=overall_results, soilname = 'Cerini', scenario_name = scn)
  fine <- N_balance_by_soil(df=overall_results, soilname = 'Merced', scenario_name = scn)
  all_soils <- rbind(coarse, loamy, fine, make.row.names=FALSE, stringsAsFactors=FALSE)
  print(apply(all_soils[,2:ncol(all_soils)], 1, sum))
  coarse_control <- N_balance_by_soil(df=overall_results, soilname = 'Kimberlina', scenario_name = 'Control')
  loamy_control <- N_balance_by_soil(df=overall_results, soilname = 'Cerini', scenario_name = 'Control')
  fine_control <- N_balance_by_soil(df=overall_results, soilname = 'Merced', scenario_name = 'Control')
  all_soils_control <- rbind(coarse_control, loamy_control, fine_control, make.row.names=FALSE, stringsAsFactors=FALSE)
  print(apply(all_soils_control[,2:ncol(all_soils_control)], 1, sum))
  #FloodMar vs. Control figure
  tiff(file = file.path(FiguresDir, stn, 'N mass balance', paste0('FloodMAR_vs_Control', scn, '_', stn, '_w_legend.tif')), family = 'Times New Roman', width = 4.5, height = 4.5, pointsize = 12, units = 'in', res=800, compression='lzw')
  par(mar=c(2.25, 3.75, 0.25, 0.25))
  bardims2 <- barplot(t(all_soils[,2:ncol(all_soils)]), names.arg = c('', '', ''), beside = FALSE, ylab='', col=c('tan4', 'springgreen2', 'yellow', 'orange2', 'violetred', 'grey', 'black', 'red'), density=c(rep(-0.1, 6), rep(20, 2)), angle = 45, ylim=c(0,ylim_max), border = NA, lwd=0.8, width = 0.4, xlim = c(0,xlim_max), space = c(1.2,1.5,1.5)) #was 11500 with make_legend=TRUE 
  bardims <- barplot(t(all_soils_control[,2:ncol(all_soils_control)]), names.arg = c('', '', ''), beside = FALSE, ylab='', col=c('tan4', 'springgreen2', 'yellow', 'orange2', 'violetred', 'grey', 'black', 'red'), density=c(rep(-0.1, 6), rep(20, 2)), angle = 45, ylim=c(0,ylim_max), border = NA, lwd=0.8, width = 0.4, xlim = c(0,xlim_max), space = c(0.1,1.5,1.5), add=TRUE)
  mtext(expression('N mass balance (kg ha'^-1*')'), side=2, line=2.25)
  mtext(c('Coarse', 'Loamy', 'Fine'), at=c(0.5,1.5,2.5), side = 1, line = 0.75)
  mtext(c('Control', 'Flood-MAR'), at=c(bardims[1], bardims2[1], bardims[2], bardims2[2], bardims[3], bardims2[3]), side = 1, line = -0.05, cex=0.6)
  if(make_legend) {
    legend('topleft', legend = c('SON accrual', 'biomass export', 'misc. losses', 'volatilization', 'denitrification', expression('NO'[3]~'leached')), ncol=3, fill=c('tan4', 'springgreen2', 'yellow', 'orange2', 'violetred', 'grey', 'grey4'), bty = 'n', inset = c(-0.02,-0.02), border = NA, cex=0.85)
  } else{legend('topleft', paste(stn_label, ', median annual P =', precip), bty='n')}
  abline(h=9243, lty=2)
  dev.off()
  print(bardims)
  print(bardims2)
  rm(overall_results, envir = .GlobalEnv)
}
makeN_massblnc_chart_vs_control(scn='21d', make_legend = FALSE, stn = 'Shafter', stn_label='Shafter', precip='143 mm') #, make_legend = TRUE
makeN_massblnc_chart_vs_control(scn='21d', make_legend = FALSE, stn = 'FivePoints', stn_label='Five Points', precip='186 mm')
makeN_massblnc_chart_vs_control(scn='21d', make_legend = FALSE, stn = 'Parlier', stn_label='Parlier', precip='278 mm')
makeN_massblnc_chart_vs_control(scn='21d', make_legend = FALSE, stn = 'Davis', stn_label='Davis', precip='398 mm')
makeN_massblnc_chart_vs_control(scn='21d', make_legend = FALSE, stn = 'Durham', stn_label='Durham', precip='537 mm')
makeN_massblnc_chart_vs_control(scn='21d', make_legend = TRUE, stn = 'Shafter', stn_label='Shafter', precip='143 mm')

N_balance_by_group <- function(classname, scenario_name) {
  result <- subset(overall_results, scenario == scenario_name & taxpartclass_simplified == classname, select = c(soil, delta_orgN, biomass_export_kgN_ha, N_misc_loss_kgN_ha, denitrification_kgN_ha, NO3_leached_kgN_ha, NO3_leached_delta_kgN_ha))
  result <- result[order(result$NO3_leached_delta_kgN_ha),]
  result
}
N_balance_by_group(classname = 1, scenario_name = '21d')
N_balance_by_group(classname = 2, scenario_name = '21d')
N_balance_by_group(classname = 3, scenario_name = '21d')

#Shafter
coarse <- N_balance_by_soil(soilname = 'Kimberlina', scenario_name = '21d')
loamy <- N_balance_by_soil(soilname = 'Cerini', scenario_name = '21d')
fine <- N_balance_by_soil(soilname = 'Merced', scenario_name = '21d')
all_soils <- rbind(coarse, loamy, fine, make.row.names=FALSE, stringsAsFactors=FALSE)
all_soils
apply(all_soils[,2:ncol(all_soils)], 1, sum)

#21d figure scratch
tiff(file = file.path(FiguresDir, met_stn, 'N mass balance', paste0('FloodMAR_21d_',met_stn, '.tif')), family = 'Times New Roman', width = 3.5, height = 4.25, pointsize = 12, units = 'in', res=800, compression='lzw')
par(mar=c(1.5, 3.75, 0.25, 0.25))
bardims <- barplot(t(all_soils[,2:ncol(all_soils)]), names.arg = c('', '', ''), beside = FALSE, ylab='', col=c('tan4', 'springgreen2', 'yellow', 'orange2', 'violetred', 'grey', 'black'), density=c(rep(-0.1, 6), 20), angle = 45, ylim=c(0,11500), border = NA, lwd=0.8, width = 0.4, xlim = c(0,4), space = c(0.3,1.5,1.5))
mtext(expression('N mass balance (kg ha'^-1*')'), side=2, line=2.25)
mtext(c('Coarse', 'Loamy', 'Fine'), at=bardims, side = 1, line = 0.5)
legend('topleft', legend = c('SON accrual', 'biomass export', 'misc. losses', 'volatilization', 'denitrification', expression('NO'[3]~'leached')), ncol=2, fill=c('tan4', 'springgreen2', 'yellow', 'orange2', 'violetred', 'grey', 'grey4'), bty = 'n', inset = c(0,-0.02), border = NA, cex=0.9)
# abline(h=9243, lty=2)
dev.off()

#control figure
