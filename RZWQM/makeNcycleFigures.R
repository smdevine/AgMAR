#to-do
#make boxplot of coarse, loamy, and fine soils net nitrate leaching increase that includes data from all climates
#and boxplots of individual climates--but is n=10 enough for this?
#make figure comparing yields by treatment x soil type
#make figure comparing difference in NO3 leaching by treatment, using Jan7d as the control

library(extrafont)
library(extrafontdb)
# font_import() #only needs to be done one time after updating and re-installing R and moving and updating packages
loadfonts(device = 'win')

met_stn <- 'Parlier'
workDir <- 'C:/Users/smdevine/Desktop/post doc/Dahlke/RZWQM/projects/PulseSoilClimate/ClimateRuns_Silage/'
FiguresDir <- file.path(workDir, 'Figures')
if(!dir.exists(file.path(FiguresDir, met_stn))) {
  dir.create(file.path(FiguresDir, met_stn))
}
resultsDir <- file.path(workDir, 'Results')

#read-in results file
overall_results <- read.csv(file.path(resultsDir, 'Summaries', paste0('overall_results_', met_stn, '.csv')), stringsAsFactors = FALSE)
overall_results$SHR[overall_results$SHR=='1. Coarse with no restrictions'] <- 'Region 1'
overall_results$SHR[overall_results$SHR=='2. Loamy with no restrictions'] <- 'Region 2'
overall_results$SHR[overall_results$SHR=='7. Shrink-swell'] <- 'Region 7'
overall_results$taxpartclass_simplified <- ifelse(overall_results$taxpartclass %in% c('Sandy', 'Coarse-loamy'), 1, ifelse(overall_results$taxpartclass %in% c('Fine', 'Very-fine'), 3, 2))
table(overall_results$taxpartclass[overall_results$scenario=='Control'])
table(overall_results$taxpartclass_simplified[overall_results$scenario=='Control'])

#summary stats by SHR
tapply(overall_results$NO3_leached_kgN_ha[overall_results$scenario=='Control'], overall_results$SHR[overall_results$scenario=='Control'], summary)
tapply(overall_results$NO3_leached_kgN_ha[overall_results$scenario=='Control'], overall_results$SHR[overall_results$scenario=='Control'], mean)
tapply(overall_results$NO3_leached_kgN_ha[overall_results$scenario=='Control'], overall_results$SHR[overall_results$scenario=='Control'], sd)
tapply(overall_results$NO3_leached_kgN_ha[overall_results$scenario=='Control']/37, overall_results$SHR[overall_results$scenario=='Control'], mean)
tapply(overall_results$NO3_leached_kgN_ha[overall_results$scenario=='Control']/37, overall_results$SHR[overall_results$scenario=='Control'], sd)
tapply(overall_results$NO3_leached_kgN_ha[overall_results$scenario=='Jan7d']/37, overall_results$SHR[overall_results$scenario=='Jan7d'], mean)
tapply(overall_results$NO3_leached_kgN_ha[overall_results$scenario=='Jan7d']/37, overall_results$SHR[overall_results$scenario=='Jan7d'], sd)
tapply(overall_results$NO3_leached_delta_kgN_ha[overall_results$scenario=='Jan7d']/10, overall_results$SHR[overall_results$scenario=='Jan7d'], mean)
tapply(overall_results$NO3_leached_delta_kgN_ha[overall_results$scenario=='Jan7d']/10, overall_results$SHR[overall_results$scenario=='Jan7d'], sd)

#summary stats by taxpartclass
tapply(overall_results$NO3_leached_kgN_ha[overall_results$scenario=='Control'], overall_results$taxpartclass[overall_results$scenario=='Control'], summary)
tapply(overall_results$NO3_leached_kgN_ha[overall_results$scenario=='Control'], overall_results$taxpartclass[overall_results$scenario=='Control'], mean)
tapply(overall_results$NO3_leached_kgN_ha[overall_results$scenario=='Control'], overall_results$taxpartclass[overall_results$scenario=='Control'], sd)
tapply(overall_results$NO3_leached_kgN_ha[overall_results$scenario=='Control']/37, overall_results$taxpartclass[overall_results$scenario=='Control'], mean)
tapply(overall_results$NO3_leached_kgN_ha[overall_results$scenario=='Control']/37, overall_results$taxpartclass[overall_results$scenario=='Control'], sd)
tapply(overall_results$NO3_leached_kgN_ha[overall_results$scenario=='Jan7d']/37, overall_results$taxpartclass[overall_results$scenario=='Jan7d'], mean)
tapply(overall_results$NO3_leached_kgN_ha[overall_results$scenario=='Jan7d']/37, overall_results$taxpartclass[overall_results$scenario=='Jan7d'], sd)
tapply(overall_results$NO3_leached_delta_kgN_ha[overall_results$scenario=='Jan7d']/10, overall_results$taxpartclass[overall_results$scenario=='Jan7d'], mean)
tapply(overall_results$NO3_leached_delta_kgN_ha[overall_results$scenario=='Jan7d']/10, overall_results$taxpartclass[overall_results$scenario=='Jan7d'], sd)

#summary stats by taxpartclass_simplified
tapply(overall_results$NO3_leached_kgN_ha[overall_results$scenario=='Control'], overall_results$taxpartclass_simplified[overall_results$scenario=='Control'], summary)
tapply(overall_results$NO3_leached_kgN_ha[overall_results$scenario=='Control'], overall_results$taxpartclass_simplified[overall_results$scenario=='Control'], mean)
tapply(overall_results$NO3_leached_kgN_ha[overall_results$scenario=='Control'], overall_results$taxpartclass_simplified[overall_results$scenario=='Control'], sd)
tapply(overall_results$NO3_leached_kgN_ha[overall_results$scenario=='Control']/37, overall_results$taxpartclass_simplified[overall_results$scenario=='Control'], mean)
tapply(overall_results$NO3_leached_kgN_ha[overall_results$scenario=='Control']/37, overall_results$taxpartclass_simplified[overall_results$scenario=='Control'], sd)
tapply(overall_results$NO3_leached_kgN_ha[overall_results$scenario=='Jan7d']/37, overall_results$taxpartclass_simplified[overall_results$scenario=='Jan7d'], summary)
tapply(overall_results$NO3_leached_kgN_ha[overall_results$scenario=='Jan7d']/37, overall_results$taxpartclass_simplified[overall_results$scenario=='Jan7d'], mean)
tapply(overall_results$NO3_leached_kgN_ha[overall_results$scenario=='Jan7d']/37, overall_results$taxpartclass_simplified[overall_results$scenario=='Jan7d'], sd)
tapply(overall_results$NO3_leached_delta_kgN_ha[overall_results$scenario=='Jan7d']/10, overall_results$taxpartclass_simplified[overall_results$scenario=='Jan7d'], summary)
tapply(overall_results$NO3_leached_delta_kgN_ha[overall_results$scenario=='Jan7d']/10, overall_results$taxpartclass_simplified[overall_results$scenario=='Jan7d'], mean)
tapply(overall_results$NO3_leached_delta_kgN_ha[overall_results$scenario=='Jan7d']/10, overall_results$taxpartclass_simplified[overall_results$scenario=='Jan7d'], sd)

#add compksat

#make barplot comparing controls vs. January 7-d interval Flood-MAR
control_NO3_leached <- as.numeric(tapply(overall_results$NO3_leached_kgN_ha[overall_results$scenario=='Control']/37, overall_results$SHR[overall_results$scenario=='Control'], mean))
Jan7d_NO3_leached <- as.numeric(tapply(overall_results$NO3_leached_kgN_ha[overall_results$scenario=='Jan7d']/37, overall_results$SHR[overall_results$scenario=='Jan7d'], mean))
control_NO3_leached_sd <- as.numeric(tapply(overall_results$NO3_leached_kgN_ha[overall_results$scenario=='Control']/37, overall_results$SHR[overall_results$scenario=='Control'], sd))
Jan7d_NO3_leached_sd <- as.numeric(tapply(overall_results$NO3_leached_kgN_ha[overall_results$scenario=='Jan7d']/37, overall_results$SHR[overall_results$scenario=='Jan7d'], sd))
nitrate_leached_matrix <- matrix(data = c(control_NO3_leached, Jan7d_NO3_leached), nrow=2, ncol=3, byrow = TRUE, dimnames = list(c('control', 'Jan7d'), c('Coarse', 'Loamy', 'Fine')))
nitrate_leached_sd_matrix <- matrix(data = c(control_NO3_leached_sd, Jan7d_NO3_leached_sd), nrow=2, ncol=3, byrow = TRUE, dimnames = list(c('control', 'Jan7d'), c('Coarse', 'Loamy', 'Fine')))
max(nitrate_leached_matrix + nitrate_leached_sd_matrix)
min(nitrate_leached_matrix - nitrate_leached_sd_matrix)
ymax <- 48
ymin <- -1
if(!dir.exists(file.path(FiguresDir, met_stn, 'By SHR'))) {
  dir.create(file.path(FiguresDir, met_stn, 'By SHR'))
}
tiff(file = file.path(FiguresDir, met_stn, 'By SHR', paste0('nitrate_leached_', met_stn, '.tif')), family = 'Times New Roman', width = 4.5, height = 4, pointsize = 12, units = 'in', res=800, compression='lzw')
par(mar=c(2, 4.5, 1, 0.5), xpd=TRUE)
bardims <- barplot(nitrate_leached_matrix, beside=TRUE, names.arg = rep('', 3), col='white', cex.names=1, ylim = c(ymin,ymax), ylab='')
barplot(nitrate_leached_matrix, beside=TRUE, col='black', names.arg = rep('', 3), density = rep(c(0,12), 3), angle = 45, ylim = c(ymin,ymax), add = TRUE)
legend('topleft', legend=c('Control', 'Flood-MAR'), bty='n', density = c(NA, 20), angle=45, fill = c('white', 'black'), border='black', col = 'black', inset=.05)
mtext(expression('Nitrate leached (kg N ha'^-1~'yr'^-1*')'), side=2, line=2.25)
mtext('Coarse', side = 1, line=0, at=mean(bardims[1:2,1]))
mtext('Loamy', side = 1, line=0, at=mean(bardims[1:2,2]))
mtext('Fine', side = 1, line=0, at=mean(bardims[1:2,3]))
segments(x0=bardims[1,1], y0=nitrate_leached_matrix[1,1] - nitrate_leached_sd_matrix[1,1], x1=bardims[1,1], y1=nitrate_leached_matrix[1,1] + nitrate_leached_sd_matrix[1,1], lwd = 1)
segments(x0=bardims[2,1], y0=nitrate_leached_matrix[2,1] - nitrate_leached_sd_matrix[2,1], x1=bardims[2,1], y1=nitrate_leached_matrix[2,1] + nitrate_leached_sd_matrix[2,1], lwd = 1)
segments(x0=bardims[1,2], y0=nitrate_leached_matrix[1,2] - nitrate_leached_sd_matrix[1,2], x1=bardims[1,2], y1=nitrate_leached_matrix[1,2] + nitrate_leached_sd_matrix[1,2], lwd = 1)
segments(x0=bardims[2,2], y0=nitrate_leached_matrix[2,2] - nitrate_leached_sd_matrix[2,2], x1=bardims[2,2], y1=nitrate_leached_matrix[2,2] + nitrate_leached_sd_matrix[2,2], lwd = 1)
segments(x0=bardims[2,3], y0=nitrate_leached_matrix[2,3] - nitrate_leached_sd_matrix[2,3], x1=bardims[2,3], y1=nitrate_leached_matrix[2,3] + nitrate_leached_sd_matrix[2,3], lwd = 1)
segments(x0=bardims[1,3], y0=nitrate_leached_matrix[1,3] - nitrate_leached_sd_matrix[1,3], x1=bardims[1,3], y1=nitrate_leached_matrix[1,3] + nitrate_leached_sd_matrix[1,3], lwd = 1)
# text(x=2, y=ymax-10, labels=paste0(met_stn, ', CA'), adj=0)
dev.off()

#assemble change in NO3 leaching across Ag-MAR treatments
Jan3d_NO3_leached <- as.numeric(tapply(overall_results$NO3_leached_delta_kgN_ha[overall_results$scenario=='Jan3d']/10, overall_results$SHR[overall_results$scenario=='Jan3d'], mean))
Jan7d_NO3_leached <- as.numeric(tapply(overall_results$NO3_leached_delta_kgN_ha[overall_results$scenario=='Jan7d']/10, overall_results$SHR[overall_results$scenario=='Jan7d'], mean))
Jan3d_NO3_leached_sd <- as.numeric(tapply(overall_results$NO3_leached_delta_kgN_ha[overall_results$scenario=='Jan3d']/10, overall_results$SHR[overall_results$scenario=='Jan3d'], sd))
Jan7d_NO3_leached_sd <- as.numeric(tapply(overall_results$NO3_leached_delta_kgN_ha[overall_results$scenario=='Jan7d']/10, overall_results$SHR[overall_results$scenario=='Jan7d'], sd))

Mar3d_NO3_leached <- as.numeric(tapply(overall_results$NO3_leached_delta_kgN_ha[overall_results$scenario=='Mar3d']/10, overall_results$SHR[overall_results$scenario=='Mar3d'], mean))
Mar7d_NO3_leached <- as.numeric(tapply(overall_results$NO3_leached_delta_kgN_ha[overall_results$scenario=='Mar7d']/10, overall_results$SHR[overall_results$scenario=='Mar7d'], mean))
Mar3d_NO3_leached_sd <- as.numeric(tapply(overall_results$NO3_leached_delta_kgN_ha[overall_results$scenario=='Mar3d']/10, overall_results$SHR[overall_results$scenario=='Mar3d'], sd))
Mar7d_NO3_leached_sd <- as.numeric(tapply(overall_results$NO3_leached_delta_kgN_ha[overall_results$scenario=='Mar7d']/10, overall_results$SHR[overall_results$scenario=='Mar7d'], sd))

F21d_NO3_leached <- as.numeric(tapply(overall_results$NO3_leached_delta_kgN_ha[overall_results$scenario=='21d']/10, overall_results$SHR[overall_results$scenario=='21d'], mean))
F21d_NO3_leached_sd <- as.numeric(tapply(overall_results$NO3_leached_delta_kgN_ha[overall_results$scenario=='21d']/10, overall_results$SHR[overall_results$scenario=='21d'], sd))

nitrate_leached_matrix <- matrix(data = c(Jan3d_NO3_leached, Jan7d_NO3_leached, Mar3d_NO3_leached, Mar7d_NO3_leached, F21d_NO3_leached), nrow=5, ncol=3, byrow = TRUE, dimnames = list(c('Jan3d', 'Jan7d', 'Mar3d', 'Mar7d', '21d'), c('Coarse', 'Loamy', 'Fine')))
nitrate_leached_sd_matrix <- matrix(data = c(Jan3d_NO3_leached_sd, Jan7d_NO3_leached_sd, Mar3d_NO3_leached_sd, Mar7d_NO3_leached_sd, F21d_NO3_leached_sd), nrow=5, ncol=3, byrow = TRUE, dimnames = list(c('Jan3d', 'Jan7d', 'Mar3d', 'Mar7d', '21d'), c('Coarse', 'Loamy', 'Fine')))

max(nitrate_leached_matrix + nitrate_leached_sd_matrix)
min(nitrate_leached_matrix - nitrate_leached_sd_matrix)
ymax <- 80
ymin <- 0
tiff(file = file.path(FiguresDir, met_stn, 'By SHR', paste0('delta_nitrate_leached_', met_stn, '.tif')), family = 'Times New Roman', width = 4.5, height = 4, pointsize = 12, units = 'in', res=800, compression='lzw')
par(mar=c(2, 4.5, 1, 0.5), xpd=TRUE)
bardims <- barplot(nitrate_leached_matrix, beside=TRUE, names.arg = rep('', 3), col=c('white', 'lightgrey', 'white', 'lightgrey', 'darkgrey'), cex.names=1, ylim = c(ymin,ymax), ylab='')
barplot(nitrate_leached_matrix, beside=TRUE, col='black', names.arg = rep('', 3), density = c(0,0,12,12,0), angle = 45, ylim = c(ymin,ymax), add = TRUE)
legend('topleft', legend=c('Jan 3d', 'Jan 7d', 'Mar 3d', 'Mar 7d', 'Jan-Mar 21d'), bty='n', fill=c('white', 'lightgrey', 'white', 'lightgrey', 'darkgrey'), inset=.05)
legend('topleft', legend=c('Jan 3d', 'Jan 7d', 'Mar 3d', 'Mar 7d', 'Jan-Mar 21d'), bty='n', density = c(0,0,20,20,0), angle=45, col = 'black', inset=.05)
mtext(expression('Mean increase nitrate leached (kg N ha'^-1~'yr'^-1*'Flood-MAR)'), side=2, line=2.25)
mtext('Coarse', side = 1, line=0, at=mean(bardims[3,1]))
mtext('Loamy', side = 1, line=0, at=mean(bardims[3,2]))
mtext('Fine', side = 1, line=0, at=mean(bardims[3,3]))
segments(x0=bardims[1,1], y0=nitrate_leached_matrix[1,1] - nitrate_leached_sd_matrix[1,1], x1=bardims[1,1], y1=nitrate_leached_matrix[1,1] + nitrate_leached_sd_matrix[1,1], lwd = 1)
segments(x0=bardims[2,1], y0=nitrate_leached_matrix[2,1] - nitrate_leached_sd_matrix[2,1], x1=bardims[2,1], y1=nitrate_leached_matrix[2,1] + nitrate_leached_sd_matrix[2,1], lwd = 1)
segments(x0=bardims[3,1], y0=nitrate_leached_matrix[3,1] - nitrate_leached_sd_matrix[3,1], x1=bardims[3,1], y1=nitrate_leached_matrix[3,1] + nitrate_leached_sd_matrix[3,1], lwd = 1)
segments(x0=bardims[4,1], y0=nitrate_leached_matrix[4,1] - nitrate_leached_sd_matrix[4,1], x1=bardims[4,1], y1=nitrate_leached_matrix[4,1] + nitrate_leached_sd_matrix[4,1], lwd = 1)
segments(x0=bardims[5,1], y0=nitrate_leached_matrix[5,1] - nitrate_leached_sd_matrix[5,1], x1=bardims[5,1], y1=nitrate_leached_matrix[5,1] + nitrate_leached_sd_matrix[5,1], lwd = 1)
segments(x0=bardims[1,2], y0=nitrate_leached_matrix[1,2] - nitrate_leached_sd_matrix[1,2], x1=bardims[1,2], y1=nitrate_leached_matrix[1,2] + nitrate_leached_sd_matrix[1,2], lwd = 1)
segments(x0=bardims[2,2], y0=nitrate_leached_matrix[2,2] - nitrate_leached_sd_matrix[2,2], x1=bardims[2,2], y1=nitrate_leached_matrix[2,2] + nitrate_leached_sd_matrix[2,2], lwd = 1)
segments(x0=bardims[3,2], y0=nitrate_leached_matrix[3,2] - nitrate_leached_sd_matrix[3,2], x1=bardims[3,2], y1=nitrate_leached_matrix[3,2] + nitrate_leached_sd_matrix[3,2], lwd = 1)
segments(x0=bardims[4,2], y0=nitrate_leached_matrix[4,2] - nitrate_leached_sd_matrix[4,2], x1=bardims[4,2], y1=nitrate_leached_matrix[4,2] + nitrate_leached_sd_matrix[4,2], lwd = 1)
segments(x0=bardims[1,3], y0=nitrate_leached_matrix[1,3] - nitrate_leached_sd_matrix[1,3], x1=bardims[1,3], y1=nitrate_leached_matrix[1,3] + nitrate_leached_sd_matrix[1,3], lwd = 1)
segments(x0=bardims[2,3], y0=nitrate_leached_matrix[2,3] - nitrate_leached_sd_matrix[2,3], x1=bardims[2,3], y1=nitrate_leached_matrix[2,3] + nitrate_leached_sd_matrix[2,3], lwd = 1)
segments(x0=bardims[3,3], y0=nitrate_leached_matrix[3,3] - nitrate_leached_sd_matrix[3,3], x1=bardims[3,3], y1=nitrate_leached_matrix[3,3] + nitrate_leached_sd_matrix[3,3], lwd = 1)
segments(x0=bardims[4,3], y0=nitrate_leached_matrix[4,3] - nitrate_leached_sd_matrix[4,3], x1=bardims[4,3], y1=nitrate_leached_matrix[4,3] + nitrate_leached_sd_matrix[4,3], lwd = 1)
segments(x0=bardims[5,3], y0=nitrate_leached_matrix[5,3] - nitrate_leached_sd_matrix[5,3], x1=bardims[5,3], y1=nitrate_leached_matrix[5,3] + nitrate_leached_sd_matrix[5,3], lwd = 1)
# text(x=2, y=ymax-10, labels=paste0(met_stn, ', CA'), adj=0)
dev.off()

#denitrificaiton
control_denitrification <- as.numeric(tapply(overall_results$denitrification_kgN_ha[overall_results$scenario=='Control']/37, overall_results$SHR[overall_results$scenario=='Control'], mean))
Jan7d_denitrification <- as.numeric(tapply(overall_results$denitrification_kgN_ha[overall_results$scenario=='Jan7d']/37, overall_results$SHR[overall_results$scenario=='Jan7d'], mean))
control_denitrification_sd <- as.numeric(tapply(overall_results$denitrification_kgN_ha[overall_results$scenario=='Control']/37, overall_results$SHR[overall_results$scenario=='Control'], sd))
Jan7d_denitrification_sd <- as.numeric(tapply(overall_results$denitrification_kgN_ha[overall_results$scenario=='Jan7d']/37, overall_results$SHR[overall_results$scenario=='Jan7d'], sd))
denitrification_matrix <- matrix(data = c(control_denitrification, Jan7d_denitrification), nrow=2, ncol=3, byrow = TRUE, dimnames = list(c('control', 'Jan7d'), c('Coarse', 'Loamy', 'Fine')))
denitrification_sd_matrix <- matrix(data = c(control_denitrification_sd, Jan7d_denitrification_sd), nrow=2, ncol=3, byrow = TRUE, dimnames = list(c('control', 'Jan7d'), c('Coarse', 'Loamy', 'Fine')))

max(denitrification_matrix + denitrification_sd_matrix)
min(denitrification_matrix - denitrification_sd_matrix)
ymax <- 27
ymin <- -0.5
tiff(file = file.path(FiguresDir, met_stn, 'By SHR', paste0('denitrification_', met_stn, '.tif')), family = 'Times New Roman', width = 4.5, height = 4, pointsize = 12, units = 'in', res=800, compression='lzw')
par(mar=c(2, 4.5, 1, 0.5), xpd=TRUE)
bardims <- barplot(denitrification_matrix, beside=TRUE, names.arg = rep('', 3), col='white', cex.names=1, ylim = c(ymin,ymax), ylab='')
barplot(denitrification_matrix, beside=TRUE, col='black', names.arg = rep('', 3), density = rep(c(0,12), 3), angle = 45, ylim = c(ymin,ymax), add = TRUE)
legend('topleft', legend=c('Control', 'Flood-MAR'), bty='n', density = c(NA, 20), angle=45, fill = c('white', 'black'), border='black', col = 'black')
mtext(expression('Denitrification (kg N ha'^-1~'yr'^-1*')'), side=2, line=2.25)
mtext('Coarse', side = 1, line=0, at=mean(bardims[1:2,1]))
mtext('Loamy', side = 1, line=0, at=mean(bardims[1:2,2]))
mtext('Fine', side = 1, line=0, at=mean(bardims[1:2,3]))
segments(x0=bardims[1,1], y0=denitrification_matrix[1,1] - denitrification_sd_matrix[1,1], x1=bardims[1,1], y1=denitrification_matrix[1,1] + denitrification_sd_matrix[1,1], lwd = 1)
segments(x0=bardims[2,1], y0=denitrification_matrix[2,1] - denitrification_sd_matrix[2,1], x1=bardims[2,1], y1=denitrification_matrix[2,1] + denitrification_sd_matrix[2,1], lwd = 1)
segments(x0=bardims[1,2], y0=denitrification_matrix[1,2] - denitrification_sd_matrix[1,2], x1=bardims[1,2], y1=denitrification_matrix[1,2] + denitrification_sd_matrix[1,2], lwd = 1)
segments(x0=bardims[2,2], y0=denitrification_matrix[2,2] - denitrification_sd_matrix[2,2], x1=bardims[2,2], y1=denitrification_matrix[2,2] + denitrification_sd_matrix[2,2], lwd = 1)
segments(x0=bardims[2,3], y0=denitrification_matrix[2,3] - denitrification_sd_matrix[2,3], x1=bardims[2,3], y1=denitrification_matrix[2,3] + denitrification_sd_matrix[2,3], lwd = 1)
segments(x0=bardims[1,3], y0=denitrification_matrix[1,3] - denitrification_sd_matrix[1,3], x1=bardims[1,3], y1=denitrification_matrix[1,3] + denitrification_sd_matrix[1,3], lwd = 1)
# text(x=8, y=22, labels=paste0(met_stn, ', CA'))
dev.off()

#net mineralization
control_mineralization <- as.numeric(tapply(overall_results$net_min_kgN_ha[overall_results$scenario=='Control']/37, overall_results$SHR[overall_results$scenario=='Control'], mean))
Jan7d_mineralization <- as.numeric(tapply(overall_results$net_min_kgN_ha[overall_results$scenario=='Jan7d']/37, overall_results$SHR[overall_results$scenario=='Jan7d'], mean))
control_mineralization_sd <- as.numeric(tapply(overall_results$net_min_kgN_ha[overall_results$scenario=='Control']/37, overall_results$SHR[overall_results$scenario=='Control'], sd))
Jan7d_mineralization_sd <- as.numeric(tapply(overall_results$net_min_kgN_ha[overall_results$scenario=='Jan7d']/37, overall_results$SHR[overall_results$scenario=='Jan7d'], sd))
mineralization_matrix <- matrix(data = c(control_mineralization, Jan7d_mineralization), nrow=2, ncol=3, byrow = TRUE, dimnames = list(c('control', 'Jan7d'), c('Coarse', 'Loamy', 'Fine')))
mineralization_sd_matrix <- matrix(data = c(control_mineralization_sd, Jan7d_mineralization_sd), nrow=2, ncol=3, byrow = TRUE, dimnames = list(c('control', 'Jan7d'), c('Coarse', 'Loamy', 'Fine')))

max(mineralization_matrix + mineralization_sd_matrix)
min(mineralization_matrix - mineralization_sd_matrix)
ymax <- 90
ymin <- 0
tiff(file = file.path(FiguresDir, met_stn, 'By SHR', paste0('net_mineralization_', met_stn, '.tif')), family = 'Times New Roman', width = 4.5, height = 4, pointsize = 12, units = 'in', res=800, compression='lzw')
par(mar=c(2, 4.5, 1, 0.5), xpd=TRUE)
bardims <- barplot(mineralization_matrix, beside=TRUE, names.arg = rep('', 3), col='white', cex.names=1, ylim = c(ymin,ymax), ylab='')
barplot(mineralization_matrix, beside=TRUE, col='black', names.arg = rep('', 3), density = rep(c(0,12), 3), angle = 45, ylim = c(ymin,ymax), add = TRUE)
legend('topleft', legend=c('Control', 'Flood-MAR'), bty='n', density = c(NA, 20), angle=45, fill = c('white', 'black'), border='black', col = 'black')
mtext(expression('Net mineralization (kg N ha'^-1~'yr'^-1*')'), side=2, line=2.25)
mtext('Coarse', side = 1, line=0, at=mean(bardims[1:2,1]))
mtext('Loamy', side = 1, line=0, at=mean(bardims[1:2,2]))
mtext('Fine', side = 1, line=0, at=mean(bardims[1:2,3]))
segments(x0=bardims[1,1], y0=mineralization_matrix[1,1] - mineralization_sd_matrix[1,1], x1=bardims[1,1], y1=mineralization_matrix[1,1] + mineralization_sd_matrix[1,1], lwd = 1)
segments(x0=bardims[2,1], y0=mineralization_matrix[2,1] - mineralization_sd_matrix[2,1], x1=bardims[2,1], y1=mineralization_matrix[2,1] + mineralization_sd_matrix[2,1], lwd = 1)
segments(x0=bardims[1,2], y0=mineralization_matrix[1,2] - mineralization_sd_matrix[1,2], x1=bardims[1,2], y1=mineralization_matrix[1,2] + mineralization_sd_matrix[1,2], lwd = 1)
segments(x0=bardims[2,2], y0=mineralization_matrix[2,2] - mineralization_sd_matrix[2,2], x1=bardims[2,2], y1=mineralization_matrix[2,2] + mineralization_sd_matrix[2,2], lwd = 1)
segments(x0=bardims[2,3], y0=mineralization_matrix[2,3] - mineralization_sd_matrix[2,3], x1=bardims[2,3], y1=mineralization_matrix[2,3] + mineralization_sd_matrix[2,3], lwd = 1)
segments(x0=bardims[1,3], y0=mineralization_matrix[1,3] - mineralization_sd_matrix[1,3], x1=bardims[1,3], y1=mineralization_matrix[1,3] + mineralization_sd_matrix[1,3], lwd = 1)
# text(x=8, y=105, paste0(met_stn, ', CA'))
dev.off()

#water quality
control_NO3_ppm <- as.numeric(tapply(overall_results$NO3_ppm[overall_results$scenario=='Control'], overall_results$SHR[overall_results$scenario=='Control'], mean))
Jan7d_NO3_ppm <- as.numeric(tapply(overall_results$NO3_ppm[overall_results$scenario=='Jan7d'], overall_results$SHR[overall_results$scenario=='Jan7d'], mean))
control_NO3_ppm_sd <- as.numeric(tapply(overall_results$NO3_ppm[overall_results$scenario=='Control'], overall_results$SHR[overall_results$scenario=='Control'], sd))
Jan7d_NO3_ppm_sd <- as.numeric(tapply(overall_results$NO3_ppm[overall_results$scenario=='Jan7d'], overall_results$SHR[overall_results$scenario=='Jan7d'], sd))
NO3_ppm_matrix <- matrix(data = c(control_NO3_ppm, Jan7d_NO3_ppm), nrow=2, ncol=3, byrow = TRUE, dimnames = list(c('control', 'Jan7d'), c('Coarse', 'Loamy', 'Fine')))
NO3_ppm_sd_matrix <- matrix(data = c(control_NO3_ppm_sd, Jan7d_NO3_ppm_sd), nrow=2, ncol=3, byrow = TRUE, dimnames = list(c('control', 'Jan7d'), c('Coarse', 'Loamy', 'Fine')))

max(NO3_ppm_matrix + NO3_ppm_sd_matrix)
min(NO3_ppm_matrix - NO3_ppm_sd_matrix)
ymax <- 120
ymin <- 0
tiff(file = file.path(FiguresDir, met_stn, 'By SHR', paste0('NO3_ppm_', met_stn, '.tif')), family = 'Times New Roman', width = 4.5, height = 4, pointsize = 12, units = 'in', res=800, compression='lzw')
par(mar=c(2, 4.5, 1, 0.5), xpd=TRUE)
bardims <- barplot(NO3_ppm_matrix, beside=TRUE, names.arg = rep('', 3), col='white', cex.names=1, ylim = c(ymin,ymax), ylab='')
barplot(NO3_ppm_matrix, beside=TRUE, col='black', names.arg = rep('', 3), density = rep(c(0,12), 3), angle = 45, ylim = c(ymin,ymax), add = TRUE)
legend('topright', legend=c('Control', 'Flood-MAR'), bty='n', density = c(NA, 20), angle=45, fill = c('white', 'black'), border='black', col = 'black')
mtext('Nitrate in deep percolation (ppm)', side=2, line=2.25)
mtext('Coarse', side = 1, line=0, at=mean(bardims[1:2,1]))
mtext('Loamy', side = 1, line=0, at=mean(bardims[1:2,2]))
mtext('Fine', side = 1, line=0, at=mean(bardims[1:2,3]))
segments(x0=bardims[1,1], y0=NO3_ppm_matrix[1,1] - NO3_ppm_sd_matrix[1,1], x1=bardims[1,1], y1=NO3_ppm_matrix[1,1] + NO3_ppm_sd_matrix[1,1], lwd = 1)
segments(x0=bardims[2,1], y0=NO3_ppm_matrix[2,1] - NO3_ppm_sd_matrix[2,1], x1=bardims[2,1], y1=NO3_ppm_matrix[2,1] + NO3_ppm_sd_matrix[2,1], lwd = 1)
segments(x0=bardims[1,2], y0=NO3_ppm_matrix[1,2] - NO3_ppm_sd_matrix[1,2], x1=bardims[1,2], y1=NO3_ppm_matrix[1,2] + NO3_ppm_sd_matrix[1,2], lwd = 1)
segments(x0=bardims[2,2], y0=NO3_ppm_matrix[2,2] - NO3_ppm_sd_matrix[2,2], x1=bardims[2,2], y1=NO3_ppm_matrix[2,2] + NO3_ppm_sd_matrix[2,2], lwd = 1)
segments(x0=bardims[2,3], y0=NO3_ppm_matrix[2,3] - NO3_ppm_sd_matrix[2,3], x1=bardims[2,3], y1=NO3_ppm_matrix[2,3] + NO3_ppm_sd_matrix[2,3], lwd = 1)
segments(x0=bardims[1,3], y0=NO3_ppm_matrix[1,3] - NO3_ppm_sd_matrix[1,3], x1=bardims[1,3], y1=NO3_ppm_matrix[1,3] + NO3_ppm_sd_matrix[1,3], lwd = 1)
# text(x=8, y=120, paste0(met_stn, ', CA'))
dev.off()

#plot deep percolation differences by soil type and AgMAR vs. control
#net mineralization
control_deep_percolation <- as.numeric(tapply(overall_results$DP_cm[overall_results$scenario=='Control'], overall_results$SHR[overall_results$scenario=='Control'], mean))
Jan7d_deep_percolation <- as.numeric(tapply(overall_results$DP_cm[overall_results$scenario=='Jan7d'], overall_results$SHR[overall_results$scenario=='Jan7d'], mean))
control_deep_percolation_sd <- as.numeric(tapply(overall_results$DP_cm[overall_results$scenario=='Control'], overall_results$SHR[overall_results$scenario=='Control'], sd))
Jan7d_deep_percolation_sd <- as.numeric(tapply(overall_results$DP_cm[overall_results$scenario=='Jan7d'], overall_results$SHR[overall_results$scenario=='Jan7d'], sd))
deep_percolation_matrix <- matrix(data = c(control_deep_percolation, Jan7d_deep_percolation), nrow=2, ncol=3, byrow = TRUE, dimnames = list(c('control', 'Jan7d'), c('Coarse', 'Loamy', 'Fine')))
deep_percolation_sd_matrix <- matrix(data = c(control_deep_percolation_sd, Jan7d_deep_percolation_sd), nrow=2, ncol=3, byrow = TRUE, dimnames = list(c('control', 'Jan7d'), c('Coarse', 'Loamy', 'Fine')))

max(deep_percolation_matrix + deep_percolation_sd_matrix)
min(deep_percolation_matrix - deep_percolation_sd_matrix)
ymax <- 950
ymin <- -10
tiff(file = file.path(FiguresDir, met_stn, 'By SHR', paste0('deep_percolation_', met_stn, '.tif')), family = 'Times New Roman', width = 4.5, height = 4, pointsize = 12, units = 'in', res=800, compression='lzw')
par(mar=c(2, 4.5, 1, 0.5), xpd=TRUE)
bardims <- barplot(deep_percolation_matrix, beside=TRUE, names.arg = rep('', 3), col='white', cex.names=1, ylim = c(ymin,ymax), ylab='')
barplot(deep_percolation_matrix, beside=TRUE, col='black', names.arg = rep('', 3), density = rep(c(0,12), 3), angle = 45, ylim = c(ymin,ymax), add = TRUE)
legend('topright', legend=c('Control', 'Flood-MAR'), bty='n', density = c(NA, 20), angle=45, fill = c('white', 'black'), border='black', col = 'black')
mtext('Deep percolation (cm)', side=2, line=2.25)
mtext('Coarse', side = 1, line=0, at=mean(bardims[1:2,1]))
mtext('Loamy', side = 1, line=0, at=mean(bardims[1:2,2]))
mtext('Fine', side = 1, line=0, at=mean(bardims[1:2,3]))
segments(x0=bardims[1,1], y0=deep_percolation_matrix[1,1] - deep_percolation_sd_matrix[1,1], x1=bardims[1,1], y1=deep_percolation_matrix[1,1] + deep_percolation_sd_matrix[1,1], lwd = 1)
segments(x0=bardims[2,1], y0=deep_percolation_matrix[2,1] - deep_percolation_sd_matrix[2,1], x1=bardims[2,1], y1=deep_percolation_matrix[2,1] + deep_percolation_sd_matrix[2,1], lwd = 1)
segments(x0=bardims[1,2], y0=deep_percolation_matrix[1,2] - deep_percolation_sd_matrix[1,2], x1=bardims[1,2], y1=deep_percolation_matrix[1,2] + deep_percolation_sd_matrix[1,2], lwd = 1)
segments(x0=bardims[2,2], y0=deep_percolation_matrix[2,2] - deep_percolation_sd_matrix[2,2], x1=bardims[2,2], y1=deep_percolation_matrix[2,2] + deep_percolation_sd_matrix[2,2], lwd = 1)
segments(x0=bardims[2,3], y0=deep_percolation_matrix[2,3] - deep_percolation_sd_matrix[2,3], x1=bardims[2,3], y1=deep_percolation_matrix[2,3] + deep_percolation_sd_matrix[2,3], lwd = 1)
segments(x0=bardims[1,3], y0=deep_percolation_matrix[1,3] - deep_percolation_sd_matrix[1,3], x1=bardims[1,3], y1=deep_percolation_matrix[1,3] + deep_percolation_sd_matrix[1,3], lwd = 1)
# text(x=8, y=105, paste0(met_stn, ', CA'))
dev.off()

SHRnames <- unique(overall_results$SHR)
SHRnames <- SHRnames[order(SHRnames)]

#plot net mineralization vs. NO3 leaching
overall_results$SHRcolor <- ifelse(overall_results$SHR=='1. Coarse with no restrictions', 'lightgoldenrod', ifelse(overall_results$SHR=='2. Loamy with no restrictions', 'tan4', ifelse(overall_results$SHR=='7. Shrink-swell', 'violetred', NA)))
plot(overall_results$net_min_kgN_ha[overall_results$scenario=='Control'], overall_results$NO3_leached_kgN_ha[overall_results$scenario=='Control'], col=overall_results$SHRcolor[overall_results$scenario=='Control'], xlab='', ylab='', cex=1.1)
points(overall_results$net_min_kgN_ha[overall_results$scenario=='21d'], overall_results$NO3_leached_kgN_ha[overall_results$scenario=='21d'], col=overall_results$SHRcolor[overall_results$scenario=='21d'], xlab='', ylab='', cex=1.1, pch=2)
legend()

plot(overall_results$increase_totalsoilkgN_ha[overall_results$scenario=='Control'], overall_results$NO3_leached_kgN_ha[overall_results$scenario=='Control'], col=overall_results$SHRcolor[overall_results$scenario=='Mar7d'])
plot(overall_results$increase_totalsoilkgN_ha[overall_results$scenario=='21d'], overall_results$NO3_leached_kgN_ha[overall_results$scenario=='21d'], col=overall_results$SHRcolor[overall_results$scenario=='21d'])
plot(overall_results$final_totalsoilMgC_ha[overall_results$scenario=='21d'] - overall_results$initial_totalsoilMgC_ha[overall_results$scenario=='21d'], overall_results$NO3_leached_kgN_ha[overall_results$scenario=='21d'], col=overall_results$SHRcolor[overall_results$scenario=='21d'])

plot(overall_results$initial_totalsoilMgC_ha[overall_results$scenario=='Jan7d'], overall_results$NO3_leached_delta_kgN_ha[overall_results$scenario=='Jan7d'], col=overall_results$SHRcolor[overall_results$scenario=='Mar7d'])
plot(overall_results$increase_totalsoilkgN_ha[overall_results$scenario=='Jan7d'], overall_results$NO3_leached_delta_kgN_ha[overall_results$scenario=='Jan7d'], col=overall_results$SHRcolor[overall_results$scenario=='Mar7d'])
plot(overall_results$net_min_kgN_ha[overall_results$scenario=='Jan7d'], overall_results$NO3_leached_delta_kgN_ha[overall_results$scenario=='Jan7d'], col=overall_results$SHRcolor[overall_results$scenario=='Mar7d'])
plot(overall_results$net_min_kgN_ha[overall_results$scenario=='Mar7d'], overall_results$NO3_leached_delta_kgN_ha[overall_results$scenario=='Mar7d'], col=overall_results$SHRcolor[overall_results$scenario=='Mar7d'])
plot(overall_results$final_totalsoilMgC_ha[overall_results$scenario=='21d'] - overall_results$initial_totalsoilMgC_ha[overall_results$scenario=='21d'], overall_results$NO3_leached_delta_kgN_ha[overall_results$scenario=='21d'], col=overall_results$SHRcolor[overall_results$scenario=='21d'])
plot(overall_results$NO3_leached_kgN_ha[overall_results$scenario=='Control'], overall_results$NO3_leached_delta_kgN_ha[overall_results$scenario=='Mar7d'], col=overall_results$SHRcolor[overall_results$scenario=='Mar7d'])

plot(overall_results$ksat_cm_day[overall_results$scenario=='Mar7d'], overall_results$NO3_leached_delta_kgN_ha[overall_results$scenario=='Mar7d'], col=overall_results$SHRcolor[overall_results$scenario=='Mar7d'])
plot(overall_results$microporosity[overall_results$scenario=='Mar7d'], overall_results$NO3_leached_delta_kgN_ha[overall_results$scenario=='Mar7d'], col=overall_results$SHRcolor[overall_results$scenario=='Mar7d'])
plot(overall_results$clay[overall_results$scenario=='Mar7d'], overall_results$NO3_leached_delta_kgN_ha[overall_results$scenario=='Mar7d'], col=overall_results$SHRcolor[overall_results$scenario=='Mar7d'])
plot(overall_results$sand[overall_results$scenario=='Mar7d'], overall_results$NO3_leached_delta_kgN_ha[overall_results$scenario=='Mar7d'], col=overall_results$SHRcolor[overall_results$scenario=='Mar7d'])
plot(overall_results$theta_0.3b[overall_results$scenario=='Mar7d'], overall_results$NO3_leached_delta_kgN_ha[overall_results$scenario=='Mar7d'], col=overall_results$SHRcolor[overall_results$scenario=='Mar7d'])

plot_clay_vs_NO3leached <- function(stn) {
  overall_results <- read.csv(file.path(resultsDir, 'Summaries', paste0('overall_results_', stn, '.csv')), stringsAsFactors = FALSE)
  overall_results$clay <- comp_ksat$clay[match(overall_results$soil, comp_ksat$compnames)]
  plot(overall_results$clay[overall_results$scenario=='Mar7d'], overall_results$NO3_leached_delta_kgN_ha[overall_results$scenario=='Mar7d']/10, xlab='Clay (%)', ylab='', ylim=c(0,240))
  mtext(expression('Mean increase nitrate leached (kg N ha'^-1~'yr'^-1*'Flood-MAR)'), side=2, line=2.25)
}
plot_clay_vs_NO3leached('Durham')
plot_clay_vs_NO3leached('Davis')
plot_clay_vs_NO3leached('Parlier')
plot_clay_vs_NO3leached('FivePoints')
plot_clay_vs_NO3leached('Shafter')

plot_clay_vs_NO3leached <- function(stns) {
  overall_results <- read.csv(file.path(resultsDir, 'Summaries', paste0('overall_results_', stns[1], '.csv')), stringsAsFactors = FALSE)
  overall_results$clay <- comp_ksat$clay[match(overall_results$soil, comp_ksat$compnames)]
  tiff(file = file.path(FiguresDir, 'Overall', 'nitrate_leached_all_climates.tif'), family = 'Times New Roman', width = 6.5, height = 4.5, pointsize = 12, units = 'in', res=800, compression='lzw')
  par(mar=c(3.5, 4.5, 0.5, 0.5), xpd=TRUE)
  plot(overall_results$clay[overall_results$scenario=='Mar7d'], overall_results$NO3_leached_delta_kgN_ha[overall_results$scenario=='Mar7d']/10, xlab='', ylab='', ylim=c(0,240), col='darkblue')
  mtext(expression('Additional nitrate leached (kg N ha'^-1~'yr'^-1*' Flood-MAR)'), side=2, line=2.25)
  mtext('Clay (%)', side=1, line=2.25)
  overall_results <- read.csv(file.path(resultsDir, 'Summaries', paste0('overall_results_', stns[2], '.csv')), stringsAsFactors = FALSE)
  overall_results$clay <- comp_ksat$clay[match(overall_results$soil, comp_ksat$compnames)]
  points(x=overall_results$clay[overall_results$scenario=='Mar7d'], y=(overall_results$NO3_leached_delta_kgN_ha[overall_results$scenario=='Mar7d']/10), col='lightblue')
  overall_results <- read.csv(file.path(resultsDir, 'Summaries', paste0('overall_results_', stns[3], '.csv')), stringsAsFactors = FALSE)
  overall_results$clay <- comp_ksat$clay[match(overall_results$soil, comp_ksat$compnames)]
  points(x=overall_results$clay[overall_results$scenario=='Mar7d'], y=(overall_results$NO3_leached_delta_kgN_ha[overall_results$scenario=='Mar7d']/10), col='yellow2')
  overall_results <- read.csv(file.path(resultsDir, 'Summaries', paste0('overall_results_', stns[4], '.csv')), stringsAsFactors = FALSE)
  overall_results$clay <- comp_ksat$clay[match(overall_results$soil, comp_ksat$compnames)]
  points(x=overall_results$clay[overall_results$scenario=='Mar7d'], y=(overall_results$NO3_leached_delta_kgN_ha[overall_results$scenario=='Mar7d']/10), col='orange')
  overall_results <- read.csv(file.path(resultsDir, 'Summaries', paste0('overall_results_', stns[5], '.csv')), stringsAsFactors = FALSE)
  overall_results$clay <- comp_ksat$clay[match(overall_results$soil, comp_ksat$compnames)]
  points(x=overall_results$clay[overall_results$scenario=='Mar7d'], y=(overall_results$NO3_leached_delta_kgN_ha[overall_results$scenario=='Mar7d']/10), col='red')
  legend('topright', legend = c('Durham', 'Davis', 'Parlier', 'Five Points', 'Shafter'), col=c('darkblue', 'lightblue', 'yellow2', 'orange', 'red'), pch=1, bty = 'n')
  dev.off()
}
plot_clay_vs_NO3leached(c('Durham', 'Davis', 'Parlier', 'FivePoints', 'Shafter'))

plot_clay_vs_NO3leached_stn <- function(stns, col, add) {
  overall_results <- read.csv(file.path(resultsDir, 'Summaries', paste0('overall_results_', stns[1], '.csv')), stringsAsFactors = FALSE)
  overall_results$clay <- comp_ksat$clay[match(overall_results$soil, comp_ksat$compnames)]
  if(add) {
    points(overall_results$clay[overall_results$scenario=='Mar7d'], overall_results$NO3_leached_delta_kgN_ha[overall_results$scenario=='Mar7d']/10, col=col)
  } else {
      plot(overall_results$clay[overall_results$scenario=='Mar7d'], overall_results$NO3_leached_delta_kgN_ha[overall_results$scenario=='Mar7d']/10, xlab='Clay (%)', ylab='', ylim=c(0,240), col=col)
    mtext(expression('Mean increase nitrate leached (kg N ha'^-1~'yr'^-1*'Flood-MAR)'), side=2, line=2.25)
  }
}

plot_clay_vs_NO3leached_stn('FivePoints', col = 'orange', add=FALSE)
plot_clay_vs_NO3leached_stn('Shafter', col = 'red', add=TRUE)
plot_clay_vs_NO3leached_stn('Durham', col = 'blue', add=TRUE)
plot_clay_vs_NO3leached_stn('Parlier', col = 'yellow2', add=TRUE)

plot(overall_results$clay[overall_results$scenario=='Jan7d'], overall_results$NO3_leached_delta_kgN_ha[overall_results$scenario=='Jan7d'], col='darkblue')
points(overall_results$clay[overall_results$scenario=='Jan7d'], overall_results$NO3_leached_delta_kgN_ha[overall_results$scenario=='Jan7d'])

#test boxplots of nitrate leached by SHR
boxplot(overall_results$NO3_leached_delta_kgN_ha[overall_results$scenario=='Jan7d'] / 10 ~ overall_results$SHR[overall_results$scenario=='Jan7d'], xlab='', ylab = '')
boxplot(overall_results$NO3_leached_kgN_ha[overall_results$scenario=='Jan7d'] / 37 ~ overall_results$SHR[overall_results$scenario=='Jan7d'], xlab='', ylab = '')
boxplot(overall_results$NO3_leached_kgN_ha[overall_results$scenario=='Control'] / 37 ~ overall_results$SHR[overall_results$scenario=='Control'], xlab='', ylab = '')

#make residual nitrate and cumulative nitrate leached plots by soil across all climates
dailyReport<- function(station, projectName, soil) {
  leachingDF <- read.table(file.path(workDir, station, projectName, soil, 'DAILY.PLT'), col.names = c('DAY', 'ACCUMULATED PRECIPITATION (CM)',  'ACCUMULATED INFILTRATION (CM)', 'TEMP BREAK THROUGH CURVE (C)', 'WATER FLUX INTO GW (CM/DAY)', 'ACTUAL EVAPORATION (CM)', 'ACTUAL TRANSPIRATION (CM)', 'SURFACE MULCH MASS (KG/HA)', 'TOTAL NO3-N IN PROFILE (KG/HA)', 'NO3 FLUX INTO GW (UG/CM^2/DAY)', 'NO3 BREAK THROUGH (MG/L)', 'MINERALIZATION (KG/HA)', 'LEAF AREA INDEX', 'PLANT HEIGHT (CM)', 'PLANT AREA COVER (%)', 'DEPTH OF ROOTS (CM)', 'WATER STRESS', 'TEMPERATURE STRESS', 'NUTRIENT STRESS', 'NUMBER OF LIVE PLANTS', 'TOT ABOVE GRD BIOMASS (KG/HA)'), header = FALSE, skip=156)
  leachingDF$date <- seq(as.Date("1983/10/1"), as.Date("2020/10/1"), "days")
  leachingDF$year <- format.Date(leachingDF$date, '%Y')
  leachingDF$NO3_cumulative <- cumsum(leachingDF$NO3.FLUX.INTO.GW..UG.CM.2.DAY. / 10)
  leachingDF
}
plot_residual_nitrate <- function(soil, clims=climates, scn='AgMAR_21d', y_max) {
  for(i in 1:length(clims)) {
    control <- dailyReport(station = clims[i], projectName = 'SteadyStateRuns', soil)
    experimental <- dailyReport(station = clims[i], projectName = scn, soil)
    # y_max <- max(control$TOTAL.NO3.N.IN.PROFILE..KG.HA., experimental$TOTAL.NO3.N.IN.PROFILE..KG.HA.)  
    tiff(file = file.path(FiguresDir, 'By Soil', paste0(soil,'_res_nitrate_', clims[i], '.tif')), family = 'Times New Roman', width = 4, height = 3.5, pointsize = 12, units = 'in', res=800, compression='lzw')
    par(mar=c(2, 4.25, 0.5, 0.25), xpd=TRUE)
    plot(control$date, control$TOTAL.NO3.N.IN.PROFILE..KG.HA., type='l', ylim= c(0, y_max), col='red', ylab='', xlab='')
    mtext(expression('Soil profile nitrate (kg ha'^-1*')'), side = 2, line = 2.5)
    # mtext('Year', side = 1, line=2.25)
    legend('topleft', clims[i], bty='n')
    lines(experimental$date, experimental$TOTAL.NO3.N.IN.PROFILE..KG.HA., col='blue')
    dev.off()
  }
}
plot_residual_nitrate('Kimberlina', y_max = 310, clims = met_stn)
plot_residual_nitrate('Cerini', y_max = 490, clims = met_stn)
plot_residual_nitrate('Merced', y_max = 235, clims = met_stn)

plot_cumulative_nitrate <- function(soil, clims=climates, scn='AgMAR_21d', y_max) {
  for(i in 1:length(clims)) {
    control <- dailyReport(station = clims[i], projectName = 'SteadyStateRuns', soil)
    experimental <- dailyReport(station = clims[i], projectName = scn, soil)
    # y_max <- max(control$TOTAL.NO3.N.IN.PROFILE..KG.HA., experimental$TOTAL.NO3.N.IN.PROFILE..KG.HA.)  
    tiff(file = file.path(FiguresDir, 'By Soil', paste0(soil,'_cum_nitrate_', clims[i], '.tif')), family = 'Times New Roman', width = 4, height = 3.5, pointsize = 12, units = 'in', res=800, compression='lzw')
    par(mar=c(2, 4.25, 0.5, 0.25), xpd=TRUE)
    plot(control$date, control$NO3_cumulative, type='l', ylim= c(0, y_max), col='red', ylab='', xlab='')
    mtext(expression('Cumulative nitrate leached (kg ha'^-1*')'), side = 2, line = 2.5)
    # mtext('Year', side = 1, line=2.25)
    legend('topleft', clims[i], bty='n')
    lines(experimental$date, experimental$NO3_cumulative, col='blue')
    dev.off()
  }
}
plot_cumulative_nitrate('Kimberlina', y_max = 575, clims = met_stn)
plot_cumulative_nitrate('Cerini', y_max = 1150, clims = met_stn)
plot_cumulative_nitrate('Merced', y_max = 600, clims = met_stn)
