#TO-DO: (1) add annual PET summary (2) verify PET sums to sums provided by crop growth period output file
compnames <- c('Hanford', 'Delhi', 'Wasco', 'Hesperia', 'Milham', 'Tujunga', 'Panoche', 'Cerini', 'Yolo', 'Colpien', 'Tehama', 'Capay', 'Clear Lake', 'Tulare', 'Kimberlina', 'Columbia', 'Excelsior', 'Atwater', 'Rincon', 'Sycamore', 'Conejo', 'Westhaven', 'Pleito', 'Lokern', 'Merced', 'Cropley', 'Tachi', 'Myers', 'Lofgren', 'Wekoda', 'Guijarral', 'Granoso', 'Chanac')
compnames <- compnames[order(compnames)]
sol_conversion <- 10^3/(60*60) #MJ per m2 per day to kWh per day per m2 (multiply by this)
wind_conversion <- 1000/(24*60*60) #km per day -to- m per s
workDir <- 'C:/Users/smdevine/Desktop/post doc/Dahlke/RZWQM/cimis'
tablesDir <- 'C:/Users/smdevine/Desktop/post doc/Dahlke/RZWQM/publication/tables'
met_stns <- c('Durham', 'Davis', 'Parlier', 'FivePoints', 'Shafter')
met_stns <- met_stns[order(met_stns)]
resultsDir <- 'C:/Users/smdevine/Desktop/post doc/Dahlke/RZWQM/projects/PulseSoilClimate/ClimateRuns_Silage/'
EToDirs <- lapply(met_stns, function(x) {
  file.path(resultsDir, x, 'SteadyStateRuns', 'Atwater', 'WEATHERD.OUT')
})
Evap_growing <- lapply(met_stns, function(y) {
  final_result <- do.call(cbind, lapply(compnames, function(x) {
    result <- read.csv(file.path(resultsDir, 'Results', 'ET', y, paste0(x,'_SteadyStateRuns_', y, '.csv')))
    result$Evap_crop_cm
  }))
  row.names(final_result) <- 1984:2020
  colnames(final_result) <- compnames
  as.data.frame(final_result)
})
names(Evap_growing) <- met_stns
Evap_growing$Davis

Evap_dormant <- lapply(met_stns, function(y) {
  final_result <- do.call(cbind, lapply(compnames, function(x) {
    result <- read.csv(file.path(resultsDir, 'Results', 'ET', y, paste0(x,'_SteadyStateRuns_', y, '.csv')))
    result$Evap_dormant_cm
  }))
  row.names(final_result) <- 1984:2020
  colnames(final_result) <- compnames
  as.data.frame(final_result)
})
names(Evap_dormant) <- met_stns
Evap_dormant$Davis

Trans_crop <- lapply(met_stns, function(y) {
  final_result <- do.call(cbind, lapply(compnames, function(x) {
    result <- read.csv(file.path(resultsDir, 'Results', 'ET', y, paste0(x,'_SteadyStateRuns_', y, '.csv')))
    result$Trans_cm
  }))
  row.names(final_result) <- 1984:2020
  colnames(final_result) <- compnames
  as.data.frame(final_result)
})
names(Trans_crop) <- met_stns
Trans_crop$Davis


EToFiles <- lapply(EToDirs, function(x) {
  read.table(x, header = FALSE, col.names = c('YEAR', 'MON', 'DAY', 'DOY', 'TMIN', 'TMAX', 'WIND', 'RAD', 'REL H', 'CO2', 'PAR', 'RefET_T', 'RefET_S'), skip=4)
})
names(EToFiles) <- met_stns
lapply(EToFiles, head)
lapply(EToFiles$Durham, class)
lapply(EToFiles, tail)
lapply(EToFiles, function(x) sum(x$RefET_T))
lapply(EToFiles, function(x) sum(x$RefET_S))
EToFiles <- lapply(EToFiles, function (x) {
  x <- x[-nrow(x),]
  x$WY <- ifelse(x$MON %in% 10:12, x$YEAR+1, x$YEAR)
  x
})
lapply(EToFiles, tail)
climateDirs <- list.dirs(workDir, full.names = TRUE, recursive = FALSE)
climateFiles <-  lapply(climateDirs, function(x) read.csv(list.files(x, pattern = glob2rx('*cimisQC.csv'), full.names = TRUE)))
names(climateFiles) <- list.dirs(workDir, full.names = FALSE, recursive = FALSE)
colnames(climateFiles$Davis_Stn6)
unique(climateFiles$Davis_Stn6$month)
class(climateFiles$Davis_Stn6$month)
class(climateFiles$Davis_Stn6$year)
climateFiles$Davis_Stn6[climateFiles$Davis_Stn6$date=='01/10/2020',]
head(rowMeans(climateFiles$Davis_Stn6[c('minTemp_C', 'maxTemp_C')]))
climateFiles <- lapply(climateFiles, function(x) {
  x$meanTemp_C <- rowMeans(x[c('minTemp_C', 'maxTemp_C')])
  x$GrowingSeason <- ifelse(x$month %in% c(1:3,10:12), 'dormant', 'growing')
  x$WY <- ifelse(x$month %in% 10:12, x$year+1, x$year)
  x <- x[-(which(x$date=='01/10/2020'):nrow(x)),]
  x
})
lapply(climateFiles, head)
lapply(climateFiles, tail)
lapply(climateFiles, summary)

names(climateFiles)
names(EToFiles)
names(Evap_dormant)
names(Evap_growing)
names(Trans_crop)
median(sapply(Trans_crop[[2]], median))
summarizeClimateAnnually <- function(x, y) {
  result <- data.frame(station=names(x), wind_m_s=NA, relHumidity=NA, solrad_kWh_day=NA, mean_Jan_C=NA, mean_Mar_C=NA, MAT_C=NA, mean_Jul_C=NA, evap_dormant=NA, evap_growing=NA, transpiration=NA, min_WY_precip=NA, med_WY_precip=NA, mean_WY_precip=NA, WY_precip_10th_wettest=NA, max_WY_precip=NA, dormant_precip_perc=NA)
  for(i in seq_along(names(x))) {
    # print(sum(x[[i]]$GrowingSeason=='dormant'))
    # print(sum(x[[i]]$GrowingSeason=='growing'))
    print(names(x)[i])
    result[i, 'wind_m_s'] <- round(mean(x[[i]]$windRun_km_d)*wind_conversion, 2)
    result[i, 'relHumidity'] <- round(mean(x[[i]]$relHumidity), 1)
    result[i, 'mean_Jan_C'] <- round(mean(x[[i]]$meanTemp_C[x[[i]]$month==1]),1)
    result[i, 'mean_Mar_C'] <- round(mean(x[[i]]$meanTemp_C[x[[i]]$month==3]),1)
    result[i, 'MAT_C'] <- round(mean(x[[i]]$meanTemp_C), 2)
    result[i, 'mean_Jul_C'] <- round(mean(x[[i]]$meanTemp_C[x[[i]]$month==7]), 1)
    x[[i]]$solRad_kWh_m2_d <- x[[i]]$solRad_MJ_m2_d * sol_conversion
    solrad <- tapply(x[[i]]$solRad_kWh_m2_d, x[[i]]$WY, sum)
    # print(summary(solrad))
    result[i, 'solrad_kWh_day'] <- round(mean(x[[i]]$solRad_kWh_m2_d), 2)
    result[i, 'evap_dormant'] <- round(median(sapply(Evap_dormant[[i]], median))*10, 0)
    result[i, 'evap_growing'] <- round(median(sapply(Evap_growing[[i]], median))*10, 0)
    result[i, 'transpiration'] <- round(median(sapply(Trans_crop[[i]], median))*10, 0)
    # EToWY <- tapply(y[[i]]$RefET_T, y[[i]]$WY, sum)
    # print(EToWY)
    # result[i, 'min_WY_ETo'] <- min(EToWY) * 10
    # result[i, 'med_WY_ETo'] <- median(EToWY) * 10
    # result[i, 'mean_WY_ETo'] <- mean(EToWY) * 10
    # result[i, 'max_WY_ETo'] <- max(EToWY) * 10
    precipWY <- tapply(x[[i]]$precip_mm, x[[i]]$WY, sum)
    result[i, 'min_WY_precip'] <- round(min(precipWY), 0)
    result[i, 'med_WY_precip'] <- round(median(precipWY), 0)
    result[i, 'mean_WY_precip'] <- round(mean(precipWY), 0)
    result[i, 'WY_precip_10th_wettest'] <- round(precipWY[order(precipWY, decreasing = TRUE)][10], 0)
    result[i, 'max_WY_precip'] <- round(max(precipWY), 0)
    result[i, 'dormant_precip_perc'] <- round(100*(sum(x[[i]]$precip_mm[x[[i]]$GrowingSeason=='dormant'])/sum(x[[i]]$precip_mm)), 1)
  }
  result <- result[order(result$med_WY_precip, decreasing = TRUE),]
  result
}
annual_climate_summary <- summarizeClimateAnnually(climateFiles, EToFiles)
annual_climate_summary
write.csv(annual_climate_summary, file.path(tablesDir, 'station_climate_summary_v2.csv'), row.names = FALSE)
