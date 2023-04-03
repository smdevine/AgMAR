compnames <- c('Hanford', 'Delhi', 'Wasco', 'Hesperia', 'Milham', 'Tujunga', 'Panoche', 'Cerini', 'Yolo', 'Colpien', 'Tehama', 'Capay', 'Clear Lake', 'Tulare', 'Kimberlina', 'Columbia', 'Excelsior', 'Atwater', 'Rincon', 'Sycamore', 'Conejo', 'Westhaven', 'Pleito', 'Lokern', 'Merced', 'Cropley', 'Tachi', 'Myers', 'Lofgren', 'Wekoda', 'Guijarral', 'Granoso', 'Chanac')
compnames <- compnames[order(compnames)]
resultsDir <- 'C:/Users/smdevine/Desktop/post doc/Dahlke/RZWQM/projects/PulseSoilClimate/ClimateRuns_Silage'
ET_results_fn <- function(wd, station, projectName, soil) {
  x <- read.table(file.path(wd, station, projectName, soil, 'DAILY.PLT'), col.names = c('DAY', 'ACCUMULATED PRECIPITATION (CM)',  'ACCUMULATED INFILTRATION (CM)', 'TEMP BREAK THROUGH CURVE (C)', 'WATER FLUX INTO GW (CM/DAY)', 'ACTUAL EVAPORATION (CM)', 'ACTUAL TRANSPIRATION (CM)', 'SURFACE MULCH MASS (KG/HA)', 'TOTAL NO3-N IN PROFILE (KG/HA)', 'NO3 FLUX INTO GW (UG/CM^2/DAY)', 'NO3 BREAK THROUGH (MG/L)', 'MINERALIZATION (KG/HA)', 'LEAF AREA INDEX', 'PLANT HEIGHT (CM)', 'PLANT AREA COVER (%)', 'DEPTH OF ROOTS (CM)', 'WATER STRESS', 'TEMPERATURE STRESS', 'NUTRIENT STRESS', 'NUMBER OF LIVE PLANTS', 'TOT ABOVE GRD BIOMASS (KG/HA)'), header = FALSE, skip=156, nrow=13515)
  x$date <- seq(as.Date("1983/10/1"), as.Date("2020/9/30"), "days")
  x$year <- as.integer(format.Date(x$date, '%Y'))
  x$month <- as.integer(format.Date(x$date, '%m'))
  x$day <- as.integer(format.Date(x$date, '%d'))
  x$WY <- ifelse(x$month %in% 10:12, x$year+1, x$year)
  x$GS <- ifelse(x$month %in% c(1:3, 10:12), 'dormant', 'crop')
  # print(colnames(x))
  evap_dormant <- tapply(x$ACTUAL.EVAPORATION..CM.[x$GS=='dormant'], x$WY[x$GS=='dormant'], sum)
  evap_crop <- tapply(x$ACTUAL.EVAPORATION..CM.[x$GS=='crop'], x$WY[x$GS=='crop'], sum)
  trans <- tapply(x$ACTUAL.TRANSPIRATION..CM., x$WY, sum)
  result <- data.frame(WY=unique(x$WY), Evap_crop_cm=evap_crop, Evap_dormant_cm=evap_dormant, Trans_cm=trans, stringsAsFactors = FALSE)
  print(sum(result[,2:4]))
  print(sum(c(x$ACTUAL.TRANSPIRATION..CM., x$ACTUAL.EVAPORATION..CM.)))
  # print(head(x))
  # print(tail(x))
  # print(soil)
  # print(result)
  if(!dir.exists(file.path(wd, 'Results', 'ET', station))) {dir.create(file.path(wd, 'Results', 'ET', station))}
  write.csv(result, file.path(wd, 'Results', 'ET', station, paste0(soil,'_', projectName, '_', station, '.csv')), row.names = FALSE)
}
# ET_results_fn(resultsDir, 'Parlier', 'SteadyStateRuns', 'Capay')
writeET_Results <- function(compnames, scenario, weather_stn) {
  for(i in 1:length(compnames)) {
    ET_results_fn(wd=resultsDir, station = weather_stn, projectName = scenario, soil = compnames[i])
  }
}
writeET_Results(compnames = compnames, scenario = 'SteadyStateRuns', weather_stn = 'Davis')
writeET_Results(compnames = compnames, scenario = 'SteadyStateRuns', weather_stn = 'Durham')
writeET_Results(compnames = compnames, scenario = 'SteadyStateRuns', weather_stn = 'FivePoints')
writeET_Results(compnames = compnames, scenario = 'SteadyStateRuns', weather_stn = 'Parlier')
writeET_Results(compnames = compnames, scenario = 'SteadyStateRuns', weather_stn = 'Shafter')

#RZWQM.OUT has variable number of lines before data.table
input <- read.table(file.path(wd, station, projectName, soil, 'RZWQM.OUT'), col.names = c('day', 'precip_cumulative_cm', 'infiltration_cumulative_cm', 'temp_breakthrough_C', 'deep_percolation_cm', 'evaporation_cm', 'transpiration_cm', 'mulch_mass_kg_ha', 'profile_NO3_kg_ha'), header = FALSE, skip=284, nrows=13515)