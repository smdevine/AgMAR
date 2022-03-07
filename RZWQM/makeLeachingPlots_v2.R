#add loss to volatization
library(extrafont)
library(extrafontdb)
# font_import() #only needs to be done one time after updating and re-installing R and moving and updating packages
loadfonts(device = 'win')
# workDir <- 'C:/Users/smdevine/Desktop/post doc/Dahlke/RZWQM/projects/PulseSoilClimate/InitialTest_v2'
# workDir <- 'C:/Users/smdevine/Desktop/post doc/Dahlke/RZWQM/projects/PulseSoilClimate/ClimateRuns'
workDir <- 'C:/Users/smdevine/Desktop/post doc/Dahlke/RZWQM/projects/PulseSoilClimate/ClimateRuns_Silage/'
FiguresDir <- 'C:/Users/smdevine/Desktop/post doc/Dahlke/RZWQM/summaries/Figures'
MBresultsDir <- 'C:/Users/smdevine/Desktop/post doc/Dahlke/RZWQM/summaries'
# compnames <- c('Hanford', 'Delhi', 'Wasco', 'Hesperia', 'Milham', 'Tujunga', 'Panoche', 'Cerini', 'Yolo', 'Colpien', 'Tehama', 'Capay', 'Clear Lake', 'Tulare') #'Willows'
# compnames2 <- c('Kimberlina', 'Columbia', 'Excelsior', 'Atwater', 'Rincon', 'Sycamore', 'Conejo', 'Westhaven', 'Pleito', 'Lokern', 'Merced', 'Cropley', 'Tachi', 'Myers')
compnames <- c('Hanford', 'Delhi', 'Wasco', 'Hesperia', 'Milham', 'Tujunga', 'Panoche', 'Cerini', 'Yolo', 'Colpien', 'Tehama', 'Capay', 'Clear Lake', 'Tulare', 'Kimberlina', 'Columbia', 'Excelsior', 'Atwater', 'Rincon', 'Sycamore', 'Conejo', 'Westhaven', 'Pleito', 'Lokern', 'Merced', 'Cropley', 'Tachi', 'Myers', 'Lofgren', 'Wekoda')
compnames <- compnames[order(compnames)]
compnames
stress_soils <- c('Capay', 'Willows', 'Wekoda')

cumulativeFluxes <- function(station, projectName, soil) {
  scenarioDirs <- list.dirs(file.path(workDir, station, projectName), recursive = FALSE)
  result <- lapply(scenarioDirs, function(x) {
  leachingDF <- read.table(file.path(x, soil, 'CLEACH.OUT'), col.names = c('DAY', 'H', 'CA', 'NA', 'MG', 'CL', 'HCO3', 'SO4', 'AL', 'NO3-N', 'NH4-N', 'CO3', 'UREA-N', 'PEST #1', 'PEST #2', 'PEST #3'), header = FALSE, skip=6)
  leachingDF$date <- seq(as.Date("2016/10/1"), as.Date("2017/4/30"), "days")
  leachingDF
  })
  names(result) <- basename(scenarioDirs)
  result
}

# dailyFluxes <- function(projectName, soil) {
#   scenarioDirs <- list.dirs(file.path(workDir, projectName), recursive = FALSE)
#   result <- lapply(scenarioDirs, function(x) {
#     leachingDF <- read.table(file.path(x, soil, 'DAILY.PLT'), col.names = c('DAY', 'ACCUMULATED PRECIPITATION (CM)',  'ACCUMULATED INFILTRATION (CM)', 'TEMP BREAK THROUGH CURVE (C)', 'WATER FLUX INTO GW (CM/DAY)', 'ACTUAL EVAPORATION (CM)', 'ACTUAL TRANSPIRATION (CM)', 'SURFACE MULCH MASS (KG/HA)', 'TOTAL NO3-N IN PROFILE (KG/HA)', 'NO3 FLUX INTO GW (UG/CM^2/DAY)', 'MINERALIZATION (KG/HA)', 'LEAF AREA INDEX', 'PLANT HEIGHT (CM)', 'PLANT AREA COVER (%)', 'DEPTH OF ROOTS (CM)', 'WATER STRESS', 'TEMPERATURE STRESS', 'NUTRIENT STRESS', 'NUMBER OF LIVE PLANTS', 'TOT ABOVE GRD BIOMASS (KG/HA)'), header = FALSE, skip=156)
#     leachingDF$date <- seq(as.Date("1983/10/1"), as.Date("2021/4/30"), "days")
#     leachingDF
#   })
#   names(result) <- basename(scenarioDirs)
#   result
# }


dailyReport<- function(station, projectName, soil) {
  leachingDF <- read.table(file.path(workDir, station, projectName, soil, 'DAILY.PLT'), col.names = c('DAY', 'ACCUMULATED PRECIPITATION (CM)',  'ACCUMULATED INFILTRATION (CM)', 'TEMP BREAK THROUGH CURVE (C)', 'WATER FLUX INTO GW (CM/DAY)', 'ACTUAL EVAPORATION (CM)', 'ACTUAL TRANSPIRATION (CM)', 'SURFACE MULCH MASS (KG/HA)', 'TOTAL NO3-N IN PROFILE (KG/HA)', 'NO3 FLUX INTO GW (UG/CM^2/DAY)', 'NO3 BREAK THROUGH (MG/L)', 'MINERALIZATION (KG/HA)', 'LEAF AREA INDEX', 'PLANT HEIGHT (CM)', 'PLANT AREA COVER (%)', 'DEPTH OF ROOTS (CM)', 'WATER STRESS', 'TEMPERATURE STRESS', 'NUTRIENT STRESS', 'NUMBER OF LIVE PLANTS', 'TOT ABOVE GRD BIOMASS (KG/HA)'), header = FALSE, skip=156)
  leachingDF$date <- seq(as.Date("1983/10/1"), as.Date("2020/10/1"), "days")
  leachingDF$year <- format.Date(leachingDF$date, '%Y')
  leachingDF$NO3_cumulative <- cumsum(leachingDF$NO3.FLUX.INTO.GW..UG.CM.2.DAY. / 10)
  leachingDF
}

#assemble datasets for plotting cumulative nitrate leached
westhaven_control <- dailyReport(station = 'Parlier', projectName = 'SteadyStateRuns', 'Westhaven')
westhaven_AgMAR_21d <- dailyReport(station = 'Parlier', projectName = 'AgMAR_21d', 'Westhaven')
plot(westhaven_control$date, westhaven_control$TOTAL.NO3.N.IN.PROFILE..KG.HA., type='l', ylim=c(0,1300))
lines(westhaven_AgMAR_21d$date, westhaven_AgMAR_21d$TOTAL.NO3.N.IN.PROFILE..KG.HA., col='blue')

kimberlina_control <- dailyReport(station = 'Parlier', projectName = 'SteadyStateRuns', 'Kimberlina')
kimberlina_AgMAR_21d <- dailyReport(station = 'Parlier', projectName = 'AgMAR_21d', 'Kimberlina')
plot(kimberlina_control$date, kimberlina_control$TOTAL.NO3.N.IN.PROFILE..KG.HA., type='l', ylim=c(0,450))
lines(kimberlina_AgMAR_21d$date, kimberlina_AgMAR_21d$TOTAL.NO3.N.IN.PROFILE..KG.HA., col='blue')
plot(kimberlina_control$date, cumsum(kimberlina_control$NO3.FLUX.INTO.GW..UG.CM.2.DAY./10, type='l')
lines(kimberlina_AgMAR_21d$date, kimberlina_AgMAR_21d$TOTAL.NO3.N.IN.PROFILE..KG.HA., col='blue')

soils_to_report <- c(compnames, compnames2)
#soil profile nitrate
for(i in seq_along(soils_to_report)) {
  control <- dailyReport(station = 'Parlier', projectName = 'SteadyStateRuns', soils_to_report[i])
  AgMAR_21d <- dailyReport(station = 'Parlier', projectName = 'AgMAR_21d', soils_to_report[i])
  y_max <- max(control$TOTAL.NO3.N.IN.PROFILE..KG.HA., AgMAR_21d$TOTAL.NO3.N.IN.PROFILE..KG.HA.)  
  plot(control$date, control$TOTAL.NO3.N.IN.PROFILE..KG.HA., type='l', ylim= c(0, y_max), col='red', ylab=expression('Soil profile nitrate (kg ha'^-1*')'), xlab='Year', main=soils_to_report[i])
  lines(AgMAR_21d$date, AgMAR_21d$TOTAL.NO3.N.IN.PROFILE..KG.HA., col='blue')
}

#cumulative nitrate leached
for(i in seq_along(soils_to_report)) {
  control <- dailyReport(station = 'Parlier', projectName = 'SteadyStateRuns', soils_to_report[i])
  AgMAR_21d <- dailyReport(station = 'Parlier', projectName = 'AgMAR_21d', soils_to_report[i])
  y_max <- max(control$NO3_cumulative, AgMAR_21d$NO3_cumulative)  
  plot(control$date, control$NO3_cumulative, type='l', ylim= c(0, y_max), col='red', ylab=expression('Cumulative nitrate leached (kg ha'^-1*')'), xlab='Year', main=soils_to_report[i])
  lines(AgMAR_21d$date, AgMAR_21d$NO3_cumulative, col='blue')
}

dailyReport_df <- dailyReport(station = 'Davis', projectName = 'SteadyStateRuns', 'Pleito')
plot(dailyReport_df$date, dailyReport_df$NO3_cumulative, type='l')
colnames(dailyReport_df)
plot_window <- 1:4000
plot(dailyReport_df$date[plot_window], dailyReport_df$NUTRIENT.STRESS[plot_window], type = 'l')
plot(dailyReport_df$date[plot_window], dailyReport_df$WATER.STRESS[plot_window], type = 'l')
plot(dailyReport_df$date[plot_window], dailyReport_df$LEAF.AREA.INDEX[plot_window], type = 'l')
plot(dailyReport_df$date[plot_window], dailyReport_df$TOTAL.NO3.N.IN.PROFILE..KG.HA.[plot_window], type = 'l')
plot(dailyReport_df$date[plot_window], dailyReport_df$WATER.FLUX.INTO.GW..CM.DAY.[plot_window], type = 'l')

head(dailyReport_df)
colnames(dailyReport_df)
# summary(dailyReport_df$TEMP.BREAK.THROUGH.CURVE..C.)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 3.062  10.603  16.109  15.470  19.573  28.460
sum(dailyReport_df$NO3.FLUX.INTO.GW..UG.CM.2.DAY.)/10 #1618.488 kg NO3-N ha-1 total
#1346.6 kg with 20 cm tillage and UAN apps for fert
#1346.6 kg with 15 cm tillage and UAN apps for fert
#1468.6 kg with 10 cm tillage and UAN apps for fert

#after initializing profile C, N, and residues with BaseRun end-of-run system state
#250 kg N ha-1 yr-1 corn
#15 cm tillage, steady state:
#run 1:1783.762 #32.5% change
#run 2:1894.799 #6.2% change
#run 3:1977.258 #4.4% change
#run 4:2056.721 #4.0% change
#run 5:2162.401 #5.1% change
#run 6:2250.628 #4.1% change
#run 7:2321.718 #3.2% change
#run 8:2381.733 #
#run 9:2444.716
#run 10:2505.042 

#after changing to end date of 10/1/20
#base run: 1339.119 kg N
#steady state runs:
#see notebook for results

sum(dailyReport_df$WATER.FLUX.INTO.GW..CM.DAY.) #329 cm H20
sum(dailyReport_df$NO3.FLUX.INTO.GW..UG.CM.2.DAY.)/sum(dailyReport_df$WATER.FLUX.INTO.GW..CM.DAY.) #49.17823 ppm NO3 in GW recharge
mean(dailyReport_df$NO3.BREAK.THROUGH..MG.L.) #57.07079
sum(dailyReport_df$MINERALIZATION..KG.HA.) #6413.334

plot(dailyReport_df$date, dailyReport_df$MINERALIZATION..KG.HA., type='l')
plot(dailyReport_df$date, dailyReport_df$TOT.ABOVE.GRD.BIOMASS..KG.HA., type='l')
plot(dailyReport_df$date, dailyReport_df$NUMBER.OF.LIVE.PLANTS, type='l')
plot(dailyReport_df$date, dailyReport_df$WATER.FLUX.INTO.GW..CM.DAY., type='l')

plot(dailyReport_df$date, dailyReport_df$SURFACE.MULCH.MASS..KG.HA., type = 'l')

plot(dailyReport_df$date, dailyReport_df$ACTUAL.EVAPORATION..CM., type = 'l')
plot(dailyReport_df$date, dailyReport_df$ACTUAL.TRANSPIRATION..CM., type = 'l')
plot(dailyReport_df$date, dailyReport_df$TEMP.BREAK.THROUGH.CURVE..C., type = 'l')
plot(dailyReport_df$date, dailyReport_df$NO3.BREAK.THROUGH..MG.L., type = 'l')

plot_window <- 1:2000
plot(dailyReport_df$date[plot_window], dailyReport_df$DEPTH.OF.ROOTS..CM.[plot_window], type = 'l')

tapply(dailyReport_df$MINERALIZATION..KG.HA., dailyReport_df$year, sum)
tapply(dailyReport_df$WATER.FLUX.INTO.GW..CM.DAY., dailyReport_df$year, sum)
tapply(dailyReport_df$NO3.FLUX.INTO.GW..UG.CM.2.DAY., dailyReport_df$year, function(x) sum(x)/10)
as.data.frame(tapply(dailyReport_df$NUTRIENT.STRESS, dailyReport_df$year, function(x) sum(x < 0.9 & x > 0)))


as.Date('1984-07-22', format='%Y-%m-%d')
dailyReport_df[dailyReport_df$date==as.Date('1984-07-22', format='%Y-%m-%d'),]


overall_results_fn <- function(station, projectName, soil) {
  input <- readLines(file.path(workDir, station, projectName, soil, 'RZWQM.OUT'), n=2000)
  initial_soilC <- input[grepl('INITAL TOTAL SOIL C KG C/HA:', input)]
  initial_soilC <- as.numeric(unlist(strsplit(initial_soilC, ':'))[2]) #in kg C ha-1
  final_soilC <- input[grepl('FINAL  TOTAL SOIL C KG C/HA:', input)]
  final_soilC <- as.numeric(unlist(strsplit(final_soilC, ':'))[2]) #in kg C ha-1
  initial_soilN <- input[grepl('INITAL TOTAL SOIL N KG N/HA:', input)]
  initial_soilN <- as.numeric(unlist(strsplit(initial_soilN, ':'))[2]) #in kg N ha-1
  final_soilN <- input[grepl('FINAL  TOTAL SOIL N KG N/HA:', input)]
  final_soilN <- as.numeric(unlist(strsplit(final_soilN, ':'))[2]) #in kg N ha-1
  denitrification <- input[grepl('LOSS TO DENITRIFICATION KG N/HA:', input)]
  denitrification <- as.numeric(unlist(strsplit(denitrification, ':'))[2])
  NO3_leaching <- input[grepl('LOSS TO SEEPAGE KG N/HA:', input)]
  NO3_leaching <- as.numeric(unlist(strsplit(NO3_leaching, ':'))[2])
  N_min <- input[grepl('TOTAL MINERALIZATION', input)][3]
  N_min <- as.numeric(unlist(strsplit(N_min, 'MINERALIZATION'))[2])
  N_imm <- input[grepl('TOTAL IMMOBILIZATION', input)]
  N_imm <- as.numeric(unlist(strsplit(N_imm, 'IMMOBILIZATION'))[2])
  fert_app <- input[grepl('TOTAL FERTILIZER APP', input)]
  fert_app <- as.numeric(unlist(strsplit(fert_app, 'APP'))[2])
  precip <- input[grepl('PRECIPITATION +[(]+CM+[)]', input)]
  precip <- as.numeric(unlist(strsplit(precip, '[)]'))[2])
  irr <- input[grepl('IRRIGATION +[(]+CM+[)]', input)]
  irr <- as.numeric(unlist(strsplit(irr, '[)]'))[2])
  runoff <- input[grepl('TOTAL SURFACE RUNOFF +[(]+CM+[)]', input)]
  runoff <- as.numeric(unlist(strsplit(runoff, '[)]'))[2]) 
  evap <- input[grepl('EVAPORATION FROM SOIL SURFACE +[(]+CM+[)]', input)]
  evap <- as.numeric(unlist(strsplit(evap, '[)]'))[2])
  trans <- input[grepl('PLANT UPTAKE FOR TRANSPIRATION +[(]+CM+[)]', input)]
  trans <- as.numeric(unlist(strsplit(trans, '[)]'))[2])
  DP <- input[grepl('DEEP SEEPAGE OUT OF PROFILE +[(]+CM+[)]', input)]
  DP <- as.numeric(unlist(strsplit(DP, '[)]'))[2])
  vol <- input[grepl('TOTAL VOLATILIZATION', input)]
  vol <- as.numeric(unlist(strsplit(vol, 'VOLATILIZATION'))[2])
  NO3_ppm <- 10* (NO3_leaching / DP)
  result <- data.frame(initial_totalsoilMgC_ha=initial_soilC/1000, final_totalsoilMgC_ha=final_soilC/1000, initial_totalsoilkgN_ha=initial_soilN, final_totalsoilkgN_ha=final_soilN, denitrification_kgN_ha=denitrification, NO3_leached_kgN_ha=NO3_leaching, NO3_ppm=NO3_ppm, volatilized_kgN_ha=vol, fert_app_kgN_ha=fert_app, N_min_kgN_ha=N_min, N_imm_kgN_ha=N_imm, precip_cm=precip, irrigation_cm=irr, runoff_cm=runoff, evap_cm=evap, trans_cm=trans, DP_cm=DP)
  # print(soil)
  # print(result)
  write.csv(result, file.path(workDir,'results', 'Overall', station, paste0(soil,'_', projectName, '_', station, '.csv')))
}
# overall_results_fn('Parlier', 'SteadyStateRuns', 'Capay')
writeOverallResults <- function(compnames, scenario, weather_stn) {
  for(i in 1:length(compnames))
    overall_results_fn(station = weather_stn, projectName = scenario, soil = compnames[i])
}
compnames <- c('Hanford', 'Delhi', 'Wasco', 'Hesperia', 'Milham', 'Tujunga', 'Panoche', 'Cerini', 'Yolo', 'Colpien', 'Tehama', 'Capay', 'Clear Lake', 'Willows', 'Tulare', 'CoarseSHR', 'LoamySHR', 'FineSHR')
writeOverallResults(compnames = compnames, scenario = 'SteadyStateRuns', weather_stn = 'Parlier')
writeOverallResults(compnames = compnames, scenario = 'AgMAR_Jan3d', weather_stn = 'Parlier')
writeOverallResults(compnames = compnames, scenario = 'AgMAR_Jan7d', weather_stn = 'Parlier')
writeOverallResults(compnames = compnames, scenario = 'AgMAR_Mar3d', weather_stn = 'Parlier')
writeOverallResults(compnames = compnames, scenario = 'AgMAR_Mar7d', weather_stn = 'Parlier')
writeOverallResults(compnames = compnames, scenario = 'AgMAR_21d', weather_stn = 'Parlier')


#calculate daily net mineralization
# projectName <- 'Parlier_1983_2021test'
# soil <- 'CoarseSHR_automation'
N_MBL_season <- function(projectName, stn, soil) {
  df <- readLines(file.path(workDir, stn, projectName, soil, 'MBLNIT.OUT'))
  dailyMin <- df[grepl('DAILY MINERALIZATION', df)]
  dailyImm <- df[grepl('DAILY IMMOBILIZATION', df)]
  dailyDenit <- df[(which(grepl('N MASS BALANCE AT END OF DAY', df))-4)]
  dailyLeaching <- df[(which(grepl('DAILY MINERALIZATION', df))-8)]
  dailyVol <- df[(which(grepl('DAILY MINERALIZATION', df))-12)]
  dailyMin <- as.numeric(gsub('DAILY MINERALIZATION', '', dailyMin))
  dailyImm <- as.numeric(gsub('DAILY IMMOBILIZATION', '', dailyImm))
  dailyDenit <- as.numeric(gsub('DENITRIFICATION', '', dailyDenit))
  dailyLeaching <- as.numeric(gsub('SEEPAGE', '', dailyLeaching))
  dailyVol <- as.numeric(gsub('VOLATILIZATION', '', dailyVol))
  result <- data.frame(date=seq(as.Date("1983/10/1"), as.Date("2020/10/1"), "days"), mineralization_kg_ha=dailyMin, immobilization_kg_ha=dailyImm, denitrification_kg_ha=dailyDenit, leaching_kg_ha=dailyLeaching, volatilization_kg_ha=dailyVol, stringsAsFactors = FALSE)
  result$net_min_kg_ha <- result$mineralization_kg_ha - result$immobilization_kg_ha
  result$year <- format.Date(result$date, '%Y')
  result$month <- format.Date(result$date, '%m')
  result$day <- format.Date(result$date, '%d')
  result$season <- ifelse(result$month %in% c('10', '11', '12', '01', '02', '03'), 'dormant', 'growing')
  denitrification_season <- tapply(result$denitrification_kg_ha, result$season, sum)
  mineralization_season <- tapply(result$mineralization_kg_ha, result$season, sum)
  immobilization_season <- tapply(result$immobilization_kg_ha, result$season, sum)
  leaching_season <- tapply(result$leaching_kg_ha, result$season, sum)
  vol_season <- tapply(result$volatilization_kg_ha, result$season, sum)
  net_min_season <- tapply(result$net_min_kg_ha, result$season, sum)
  result <- data.frame(denitrification_kg_ha=denitrification_season, mineralization_kg_ha= mineralization_season, immobilization_kg_ha = immobilization_season, net_min_kg_ha=net_min_season, leaching_kg_ha=leaching_season, volatilization_kg_ha=vol_season)
  result <- rbind(result, apply(result, 2, sum))
  rownames(result)[3] <- 'total'
  write.csv(result, file.path(workDir,'results', 'SeasonalFluxes', paste0(soil,'_', projectName, '_', stn, '.csv')))
}
writeSeasonalResults <- function(compnames, scenario, weather_stn) {
  for(i in 1:length(compnames))
    N_MBL_season(projectName = scenario, stn = weather_stn, soil = compnames[i])
}
writeSeasonalResults(compnames = compnames, scenario = 'SteadyStateRuns', weather_stn = 'Parlier')
writeSeasonalResults(compnames = compnames, scenario = 'AgMAR_Jan7d', weather_stn = 'Parlier')
writeSeasonalResults(compnames = compnames, scenario = 'AgMAR_Jan3d', weather_stn = 'Parlier')
writeSeasonalResults(compnames = compnames, scenario = 'AgMAR_Mar7d', weather_stn = 'Parlier')
writeSeasonalResults(compnames = compnames, scenario = 'AgMAR_Mar3d', weather_stn = 'Parlier')
writeSeasonalResults(compnames = compnames, scenario = 'AgMAR_21d', weather_stn = 'Parlier')

#monthly N balance results
N_MBL_monthly <- function(projectName, stn, soil) {
  df <- readLines(file.path(workDir, stn, projectName, soil, 'MBLNIT.OUT'))
  dailyMin <- df[grepl('DAILY MINERALIZATION', df)]
  dailyImm <- df[grepl('DAILY IMMOBILIZATION', df)]
  dailyDenit <- df[(which(grepl('N MASS BALANCE AT END OF DAY', df))-4)]
  dailyLeaching <- df[(which(grepl('DAILY MINERALIZATION', df))-8)]
  dailyVol <- df[(which(grepl('DAILY MINERALIZATION', df))-12)]
  dailyMin <- as.numeric(gsub('DAILY MINERALIZATION', '', dailyMin))
  dailyImm <- as.numeric(gsub('DAILY IMMOBILIZATION', '', dailyImm))
  dailyDenit <- as.numeric(gsub('DENITRIFICATION', '', dailyDenit))
  dailyLeaching <- as.numeric(gsub('SEEPAGE', '', dailyLeaching))
  dailyVol <- as.numeric(gsub('VOLATILIZATION', '', dailyVol))
  result <- data.frame(date=seq(as.Date("1983/10/1"), as.Date("2020/10/1"), "days"), mineralization_kg_ha=dailyMin, immobilization_kg_ha=dailyImm, denitrification_kg_ha=dailyDenit, leaching_kg_ha=dailyLeaching, volatilization_kg_ha=dailyVol, stringsAsFactors = FALSE)
  result$net_min_kg_ha <- result$mineralization_kg_ha - result$immobilization_kg_ha
  result$year <- format.Date(result$date, '%Y')
  result$month <- format.Date(result$date, '%m')
  result$day <- format.Date(result$date, '%d')
  denitrification_monthly <- tapply(result$denitrification_kg_ha, result$month, sum)
  mineralization_monthly <- tapply(result$mineralization_kg_ha, result$month, sum)
  immobilization_monthly <- tapply(result$immobilization_kg_ha, result$month, sum)
  leaching_monthly <- tapply(result$leaching_kg_ha, result$month, sum)
  vol_monthly <- tapply(result$volatilization_kg_ha, result$month, sum)
  net_min_monthly <- tapply(result$net_min_kg_ha, result$month, sum)
  result <- data.frame(denitrification_kg_ha=denitrification_monthly, mineralization_kg_ha= mineralization_monthly, immobilization_kg_ha = immobilization_monthly, net_min_kg_ha=net_min_monthly, leaching_kg_ha=leaching_monthly, volatilization_kg_ha=vol_monthly)
  result <- rbind(result, apply(result, 2, sum))
  rownames(result)[13] <- 'total'
  write.csv(result, file.path(workDir,'results', 'MonthlyFluxes', paste0(soil,'_', projectName, '_', stn, '.csv')))
}
writeMonthlyResults <- function(compnames, scenario, weather_stn) {
  for(i in 1:length(compnames))
    N_MBL_monthly(projectName = scenario, stn = weather_stn, soil = compnames[i])
}
writeMonthlyResults(compnames = compnames, scenario = 'SteadyStateRuns', weather_stn = 'Parlier')
writeMonthlyResults(compnames = compnames, scenario = 'AgMAR_Jan7d', weather_stn = 'Parlier')
writeMonthlyResults(compnames = compnames, scenario = 'AgMAR_Jan3d', weather_stn = 'Parlier')
writeMonthlyResults(compnames = compnames, scenario = 'AgMAR_Mar7d', weather_stn = 'Parlier')
writeMonthlyResults(compnames = compnames, scenario = 'AgMAR_Mar3d', weather_stn = 'Parlier')
writeMonthlyResults(compnames = compnames, scenario = 'AgMAR_21d', weather_stn = 'Parlier')

#harvest data
harvest_results <- function(station, projectName, soil) {
  input <- readLines(file.path(workDir, station, projectName, soil, 'MANAGE.OUT'))
  harvest_dates <- input[which(grepl('DSSAT Crop Harvest', input))+1]
  harvest_dates <- strsplit(harvest_dates, '-----')
  harvest_dates <- sapply(harvest_dates, function(x) x[1])
  harvest_dates <- gsub('    ON  ', '', harvest_dates)
  harvest_dates <- strsplit(harvest_dates, '[/]')
  harvest_dates <- do.call(rbind, lapply(harvest_dates, function(x) {data.frame(month=x[2], day=x[1], year=x[3], stringsAsFactors = FALSE)}))
  crop_type <- input[grepl('DSSAT Crop Harvest', input)]
  crop_type <- strsplit(crop_type, '----')
  crop_type <- sapply(crop_type, function(x) x[3])
  biomass <- input[grepl('YIELD FROM ABOVE GROUND BIOMASS', input)]
  biomass <- strsplit(biomass, ':')
  biomass <- sapply(biomass, function(x){as.numeric(x[2])})
  above_biomass_N <- input[grepl('TOTAL ABOVE GROUND NITROGEN', input)]
  above_biomass_N <- strsplit(above_biomass_N, ':')
  above_biomass_N <- sapply(above_biomass_N, function(x){as.numeric(x[2])})
  below_biomass_N <- input[grepl('TOTAL BELOW GROUND NITROGEN', input)]
  below_biomass_N <- strsplit(below_biomass_N, ':')
  below_biomass_N <- sapply(below_biomass_N, function(x){as.numeric(x[2])})
  harvest_info <- cbind(harvest_dates, crop_type, biomass_kg_ha=biomass, above_biomass_N_kg_ha=above_biomass_N, below_biomass_N_kg_ha=below_biomass_N, total_biomass_N_kg_ha=(above_biomass_N+below_biomass_N))
  # harvest_info$date <- 
  harvest_info$day <- as.integer(harvest_info$day)
  harvest_info$month <- as.integer(harvest_info$month)
  harvest_info$year <- as.integer(harvest_info$year)
  harvest_info$date_harvest <- as.Date(paste0(harvest_info$year, '-', harvest_info$month, '-', harvest_info$day))
  harvest_info[,1:3] <- NULL
  harvest_info <- harvest_info[,c(ncol(harvest_info), 1:(ncol(harvest_info)-1))]
  # print(harvest_info)
  harvest_info
}

#ssurgo results
test <- harvest_results('Parlier', 'SteadyStateRuns', 'Capay v2')
test
tapply(test$biomass_kg_ha, test$crop_type, summary)

soils_to_report <- compnames[order(compnames)]


for(i in seq_along(soils_to_report)) {
  test <- harvest_results('Parlier', 'SteadyStateRuns', soils_to_report[i])
  print(soils_to_report[i])
  print(tapply(test$biomass_kg_ha, test$crop_type, summary))
}
for(i in seq_along(soils_to_report)) {
  test <- harvest_results('Davis', 'SteadyStateRuns', soils_to_report[i])
  print(soils_to_report[i])
  print(tapply(test$biomass_kg_ha, test$crop_type, summary))
}
for(i in seq_along(soils_to_report)) {
  print(soils_to_report[i])
  overall_results('Parlier', 'AgMAR_Jan7d', soils_to_report[i])
}
