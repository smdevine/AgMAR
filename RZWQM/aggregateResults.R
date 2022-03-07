library(extrafont)
library(extrafontdb)
# font_import() #only needs to be done one time after updating and re-installing R and moving and updating packages
loadfonts(device = 'win')
library(soilDB)
library(aqp)
# workDir <- 'C:/Users/smdevine/Desktop/post doc/Dahlke/RZWQM/projects/PulseSoilClimate/ClimateRuns'
workDir #read-in from other scripts
resultsDir <- file.path(workDir, 'Results')
ssurgoDir <- 'C:/Users/smdevine/Desktop/post doc/Dahlke/RZWQM/ssurgo_profile_data/RZWQM input'
FiguresDir <- file.path(workDir, 'Figures')
met_stn
resultsDir
list.files(resultsDir, recursive = FALSE)

textural.class.calc <- function(sand, silt, clay) {
  ifelse(is.na(sand) | is.na(silt) | is.na(clay), NA,
         ifelse(sand + silt + clay > 101 |
                  sand + silt + clay < 99, 'proportions do not sum to 100+-1',
                ifelse(silt + 1.5 * clay < 15, 'sand',
                       ifelse(silt + 1.5 * clay >= 15 & silt + 2 * clay < 30, 'loamy sand',
                              ifelse((clay >= 7 & clay < 20 & sand > 52 & silt + 2 * clay >= 30) | 
                                       (clay < 7 & silt < 50 & silt + 2 * clay >= 30), 'sandy loam',
                                     ifelse(clay >= 7 & clay < 27 & silt >=28 & silt < 50 & sand <= 52, 'loam',
                                            ifelse((silt >= 50 & clay >= 12 & clay < 27) | 
                                                     (silt >=50 & silt < 80 & clay < 12), 'silt loam',
                                                   ifelse(silt >= 80 & clay < 12, 'silt',
                                                          ifelse(clay >= 20 & clay < 35 & silt < 28 & sand > 45, 'sandy clay loam',
                                                                 ifelse(clay >= 27 & clay < 40 & sand > 20 & sand <= 45, 'clay loam',
                                                                        ifelse(clay >= 27 & clay < 40 & sand <= 20, 'silty clay loam',
                                                                               ifelse(clay >= 35 & sand > 45, 'sandy clay',
                                                                                      ifelse(clay >= 40 & silt >= 40, 'silty clay',
                                                                                             ifelse(clay >= 40 & sand <= 45 & silt < 40, 'clay',
                                                                                                    'undefined textural class'))))))))))))))
}
# test <- list.files(file.path(resultsDir, 'Overall'), full.names = FALSE)
# test2 <- unlist(strsplit(test[1], '_'))

overall_results <- do.call(rbind, mapply(function(x, y) {
  if(grepl('AgMAR', y)) {
    soilname <- unlist(strsplit(y, '_'))[1]
    scenario <- unlist(strsplit(y, '_'))[3]
  } else {
      soilname <- unlist(strsplit(y, '_'))[1]
      scenario <- 'Control'
  }
  z <- read.csv(x, stringsAsFactors = FALSE)
  z$soil <- soilname
  z$scenario <- scenario
  z
}, x=list.files(file.path(resultsDir, 'Overall', met_stn), full.names = TRUE), y=list.files(file.path(resultsDir, 'Overall', met_stn), full.names = FALSE), SIMPLIFY = FALSE))
head(overall_results)
overall_results$X <- NULL
dim(overall_results)
unique(overall_results$soil)

#read-in SSURGO data
ssurgo_horizons <- read.csv(file.path(ssurgoDir, 'SSURGO_soils_to_initialize.csv'), stringsAsFactors = FALSE)
colnames(ssurgo_horizons)
ssurgo_horizons2 <- read.csv(file.path(ssurgoDir, 'SSURGO_soils_to_initialize_part2.csv'), stringsAsFactors = FALSE)
colnames(ssurgo_horizons2)

ssurgo_horizons <- rbind(ssurgo_horizons, ssurgo_horizons2)
ssurgo_horizons$texture <- textural.class.calc(sand = ssurgo_horizons$sandtotal_r, silt = ssurgo_horizons$silttotal_r, clay = ssurgo_horizons$claytotal_r)

#organize by compname
comp_data <- data.frame(compnames=unique(ssurgo_horizons$compname), stringsAsFactors = FALSE)
comp_data$SHR <- ssurgo_horizons$SHRname[match(comp_data$compnames, ssurgo_horizons$compname)]
comp_data[order(comp_data$SHR),]
table(comp_data$SHR)

#add SHR info
overall_results$SHR <- comp_data$SHR[match(overall_results$soil, comp_data$compnames)]
rownames(overall_results) <- 1:nrow(overall_results)
table(overall_results$SHR[overall_results$scenario=='Control'])

#add net min column
overall_results$net_min_kgN_ha <- overall_results$N_min_kgN_ha - overall_results$N_imm_kgN_ha

#add net soil N increase
overall_results$increase_totalsoilkgN_ha <- overall_results$final_totalsoilkgN_ha - overall_results$initial_totalsoilkgN_ha

#add some more columns
overall_results$NO3_leached_delta_kgN_ha <- sapply(1:nrow(overall_results), function(x) {
  control_leaching <- overall_results[overall_results$scenario=='Control',]
  overall_results$NO3_leached_kgN_ha[x] - control_leaching$NO3_leached_kgN_ha[match(overall_results$soil[x], control_leaching$soil)]
})
overall_results$NO3_leached_delta_percent <- sapply(1:nrow(overall_results), function(x) {
  control_leaching <- overall_results[overall_results$scenario=='Control',]
  round(100*(overall_results$NO3_leached_kgN_ha[x] - control_leaching$NO3_leached_kgN_ha[match(overall_results$soil[x], control_leaching$soil)]) / control_leaching$NO3_leached_kgN_ha[match(overall_results$soil[x], control_leaching$soil)], 2)
})
overall_results$ksat_cm_day <- comp_data$ksat_cm_day[match(overall_results$soil, comp_data$compnames)]
#write to file
write.csv(overall_results, file.path(resultsDir, 'Summaries', paste0('overall_results_', met_stn, '.csv')), row.names = FALSE)

#summary stats
tapply(overall_results$NO3_leached_kgN_ha[overall_results$scenario=='Control'], overall_results$SHR[overall_results$scenario=='Control'], summary)
tapply(overall_results$NO3_leached_kgN_ha[overall_results$scenario=='Control'], overall_results$SHR[overall_results$scenario=='Control'], mean)
tapply(overall_results$NO3_leached_kgN_ha[overall_results$scenario=='Control'], overall_results$SHR[overall_results$scenario=='Control'], sd)
tapply(overall_results$NO3_leached_kgN_ha[overall_results$scenario=='Control']/37, overall_results$SHR[overall_results$scenario=='Control'], mean)
tapply(overall_results$NO3_leached_kgN_ha[overall_results$scenario=='Control']/37, overall_results$SHR[overall_results$scenario=='Control'], sd)
tapply(overall_results$NO3_leached_kgN_ha[overall_results$scenario=='Jan7d']/37, overall_results$SHR[overall_results$scenario=='Jan7d'], mean)
tapply(overall_results$NO3_leached_kgN_ha[overall_results$scenario=='Jan7d']/37, overall_results$SHR[overall_results$scenario=='Jan7d'], sd)

#make barplot comparing controls vs. January 7-d interval Flood-MAR
control_NO3_leached <- as.numeric(tapply(overall_results$NO3_leached_kgN_ha[overall_results$scenario=='Control']/37, overall_results$SHR[overall_results$scenario=='Control'], mean))
Jan7d_NO3_leached <- as.numeric(tapply(overall_results$NO3_leached_kgN_ha[overall_results$scenario=='Jan7d']/37, overall_results$SHR[overall_results$scenario=='Jan7d'], mean))
control_NO3_leached_sd <- as.numeric(tapply(overall_results$NO3_leached_kgN_ha[overall_results$scenario=='Control']/37, overall_results$SHR[overall_results$scenario=='Control'], sd))
Jan7d_NO3_leached_sd <- as.numeric(tapply(overall_results$NO3_leached_kgN_ha[overall_results$scenario=='Jan7d']/37, overall_results$SHR[overall_results$scenario=='Jan7d'], sd))
nitrate_leached_matrix <- matrix(data = c(control_NO3_leached, Jan7d_NO3_leached), nrow=2, ncol=3, byrow = TRUE, dimnames = list(c('control', 'Jan7d'), c('Coarse', 'Loamy', 'Fine')))
nitrate_leached_sd_matrix <- matrix(data = c(control_NO3_leached_sd, Jan7d_NO3_leached_sd), nrow=2, ncol=3, byrow = TRUE, dimnames = list(c('control', 'Jan7d'), c('Coarse', 'Loamy', 'Fine')))

tiff(file = file.path(FiguresDir, stn, 'nitrate_leached.tif'), family = 'Times New Roman', width = 4.5, height = 4, pointsize = 12, units = 'in', res=800, compression='lzw')
par(mar=c(2, 4.5, 1, 0.5), xpd=TRUE)
bardims <- barplot(nitrate_leached_matrix, beside=TRUE, names.arg = rep('', 3), col='white', cex.names=1, ylim = c(-5,50), ylab='')
barplot(nitrate_leached_matrix, beside=TRUE, col='black', names.arg = rep('', 3), density = rep(c(0,12), 3), angle = 45, ylim = c(-5,50), add = TRUE)
legend('topleft', legend=c('Control', 'Flood-MAR'), bty='n', density = c(NA, 20), angle=45, fill = c('white', 'black'), border='black', col = 'black')
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
dev.off()

#denitrificaiton
control_denitrification <- as.numeric(tapply(overall_results$denitrification_kgN_ha[overall_results$scenario=='Control']/37, overall_results$SHR[overall_results$scenario=='Control'], mean))
Jan7d_denitrification <- as.numeric(tapply(overall_results$denitrification_kgN_ha[overall_results$scenario=='Jan7d']/37, overall_results$SHR[overall_results$scenario=='Jan7d'], mean))
control_denitrification_sd <- as.numeric(tapply(overall_results$denitrification_kgN_ha[overall_results$scenario=='Control']/37, overall_results$SHR[overall_results$scenario=='Control'], sd))
Jan7d_denitrification_sd <- as.numeric(tapply(overall_results$denitrification_kgN_ha[overall_results$scenario=='Jan7d']/37, overall_results$SHR[overall_results$scenario=='Jan7d'], sd))
denitrification_matrix <- matrix(data = c(control_denitrification, Jan7d_denitrification), nrow=2, ncol=3, byrow = TRUE, dimnames = list(c('control', 'Jan7d'), c('Coarse', 'Loamy', 'Fine')))
denitrification_sd_matrix <- matrix(data = c(control_denitrification_sd, Jan7d_denitrification_sd), nrow=2, ncol=3, byrow = TRUE, dimnames = list(c('control', 'Jan7d'), c('Coarse', 'Loamy', 'Fine')))

tiff(file = file.path(FiguresDir, stn, 'denitrification.tif'), family = 'Times New Roman', width = 4.5, height = 4, pointsize = 12, units = 'in', res=800, compression='lzw')
par(mar=c(2, 4.5, 1, 0.5), xpd=TRUE)
bardims <- barplot(denitrification_matrix, beside=TRUE, names.arg = rep('', 3), col='white', cex.names=1, ylim = c(0,30), ylab='')
barplot(denitrification_matrix, beside=TRUE, col='black', names.arg = rep('', 3), density = rep(c(0,12), 3), angle = 45, ylim = c(0,30), add = TRUE)
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
dev.off()

#net mineralization
control_mineralization <- as.numeric(tapply(overall_results$net_min_kgN_ha[overall_results$scenario=='Control']/37, overall_results$SHR[overall_results$scenario=='Control'], mean))
Jan7d_mineralization <- as.numeric(tapply(overall_results$net_min_kgN_ha[overall_results$scenario=='Jan7d']/37, overall_results$SHR[overall_results$scenario=='Jan7d'], mean))
control_mineralization_sd <- as.numeric(tapply(overall_results$net_min_kgN_ha[overall_results$scenario=='Control']/37, overall_results$SHR[overall_results$scenario=='Control'], sd))
Jan7d_mineralization_sd <- as.numeric(tapply(overall_results$net_min_kgN_ha[overall_results$scenario=='Jan7d']/37, overall_results$SHR[overall_results$scenario=='Jan7d'], sd))
mineralization_matrix <- matrix(data = c(control_mineralization, Jan7d_mineralization), nrow=2, ncol=3, byrow = TRUE, dimnames = list(c('control', 'Jan7d'), c('Coarse', 'Loamy', 'Fine')))
mineralization_sd_matrix <- matrix(data = c(control_mineralization_sd, Jan7d_mineralization_sd), nrow=2, ncol=3, byrow = TRUE, dimnames = list(c('control', 'Jan7d'), c('Coarse', 'Loamy', 'Fine')))

tiff(file = file.path(FiguresDir, stn, 'net_mineralization.tif'), family = 'Times New Roman', width = 4.5, height = 4, pointsize = 12, units = 'in', res=800, compression='lzw')
par(mar=c(2, 4.5, 1, 0.5), xpd=TRUE)
bardims <- barplot(mineralization_matrix, beside=TRUE, names.arg = rep('', 3), col='white', cex.names=1, ylim = c(0,100), ylab='')
barplot(mineralization_matrix, beside=TRUE, col='black', names.arg = rep('', 3), density = rep(c(0,12), 3), angle = 45, ylim = c(0,100), add = TRUE)
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
dev.off()

#water quality
control_NO3_ppm <- as.numeric(tapply(overall_results$NO3_ppm[overall_results$scenario=='Control'], overall_results$SHR[overall_results$scenario=='Control'], mean))
Jan7d_NO3_ppm <- as.numeric(tapply(overall_results$NO3_ppm[overall_results$scenario=='Jan7d'], overall_results$SHR[overall_results$scenario=='Jan7d'], mean))
control_NO3_ppm_sd <- as.numeric(tapply(overall_results$NO3_ppm[overall_results$scenario=='Control'], overall_results$SHR[overall_results$scenario=='Control'], sd))
Jan7d_NO3_ppm_sd <- as.numeric(tapply(overall_results$NO3_ppm[overall_results$scenario=='Jan7d'], overall_results$SHR[overall_results$scenario=='Jan7d'], sd))
NO3_ppm_matrix <- matrix(data = c(control_NO3_ppm, Jan7d_NO3_ppm), nrow=2, ncol=3, byrow = TRUE, dimnames = list(c('control', 'Jan7d'), c('Coarse', 'Loamy', 'Fine')))
NO3_ppm_sd_matrix <- matrix(data = c(control_NO3_ppm_sd, Jan7d_NO3_ppm_sd), nrow=2, ncol=3, byrow = TRUE, dimnames = list(c('control', 'Jan7d'), c('Coarse', 'Loamy', 'Fine')))

tiff(file = file.path(FiguresDir, stn, 'NO3_ppm.tif'), family = 'Times New Roman', width = 4.5, height = 4, pointsize = 12, units = 'in', res=800, compression='lzw')
par(mar=c(2, 4.5, 1, 0.5), xpd=TRUE)
bardims <- barplot(NO3_ppm_matrix, beside=TRUE, names.arg = rep('', 3), col='white', cex.names=1, ylim = c(0,125), ylab='')
barplot(NO3_ppm_matrix, beside=TRUE, col='black', names.arg = rep('', 3), density = rep(c(0,12), 3), angle = 45, ylim = c(0,125), add = TRUE)
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
dev.off()

SHRnames <- unique(overall_results$SHR)
SHRnames <- SHRnames[order(SHRnames)]
overall_results[which(overall_results$SHR==SHRnames[1]),]

head(overall_results)
length(1984:2020) #37
tapply(overall_results$NO3_leached_delta_percent[overall_results$scenario!='Control'], overall_results$SHR[overall_results$scenario!='Control'], summary)
tapply(overall_results$NO3_leached_delta_kgN_ha[overall_results$scenario!='Control'], overall_results$SHR[overall_results$scenario!='Control'], summary)
sum(abs(overall_results$NO3_leached_delta_percent[overall_results$scenario!='Control']) < 10)
summary(overall_results$NO3_leached_kgN_ha)
summary(overall_results$NO3_leached_kgN_ha/37)
summary(overall_results$NO3_leached_delta_kgN_ha)
summary(overall_results$NO3_leached_delta_kgN_ha/37)
sum(overall_results$NO3_leached_delta_percent[which(overall_results$scenario!='Control' & overall_results$SHR==SHRnames[1])] < 0)

#plot net mineralization vs. NO3 leaching
overall_results$SHRcolor <- ifelse(overall_results$SHR=='1. Coarse with no restrictions', 'lightgoldenrod', ifelse(overall_results$SHR=='2. Loamy with no restrictions', 'tan4', ifelse(overall_results$SHR=='7. Shrink-swell', 'violetred', NA)))
plot(overall_results$net_min_kgN_ha[overall_results$scenario=='Control'], overall_results$NO3_leached_kgN_ha[overall_results$scenario=='Control'], col=overall_results$SHRcolor[overall_results$scenario=='Control'], xlab='', ylab='', cex=1.1, ylim = c(0,3600))
points(overall_results$net_min_kgN_ha[overall_results$scenario=='21d'], overall_results$NO3_leached_kgN_ha[overall_results$scenario=='21d'], col=overall_results$SHRcolor[overall_results$scenario=='21d'], xlab='', ylab='', cex=1.1, pch=2)
legend()



plot(overall_results$increase_totalsoilkgN_ha[overall_results$scenario=='Control'], overall_results$NO3_leached_kgN_ha[overall_results$scenario=='Control'])
plot(overall_results$increase_totalsoilkgN_ha[overall_results$scenario=='21d'], overall_results$NO3_leached_kgN_ha[overall_results$scenario=='21d'], col=overall_results$SHRcolor[overall_results$scenario=='21d'])

#add cokey info
length(unique(ssurgo_horizons$cokey)) #31

#ordered looks
colnames(overall_results)
overall_results[order(overall_results$SHR),]
overall_results[order(overall_results$scenario),]
test <- overall_results[overall_results$soil %in% compnames2,]
test <- overall_results[overall_results$scenario=='Control',]
test[order(test$SHR),]


