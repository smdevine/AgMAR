library(extrafont)
library(extrafontdb)
# font_import() #only needs to be done one time after updating and re-installing R and moving and updating packages
loadfonts(device = 'win')
# workDir <- 'C:/Users/smdevine/Desktop/post doc/Dahlke/RZWQM/projects/PulseSoilClimate/InitialTest_v2'
workDir <- 'C:/Users/smdevine/Desktop/post doc/Dahlke/RZWQM/projects/PulseSoilClimate/ClimateRuns/Parlier'
FiguresDir <- 'C:/Users/smdevine/Desktop/post doc/Dahlke/RZWQM/summaries/Figures'
MBresultsDir <- 'C:/Users/smdevine/Desktop/post doc/Dahlke/RZWQM/summaries'

#for scenario names:
#v1 is soil properties from textural class look-up tables in RZWQM
#v2 is soil properties from KSSL and Rosetta-estimated Ks and theta at 0.33 bar and 15 bars
#v3 is soil properties, including Ks and moisture retention, from SSURGO



cumulativeFluxes <- function(projectName, soil) {
  scenarioDirs <- list.dirs(file.path(workDir, projectName), recursive = FALSE)
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


dailyReport<- function(projectName, soil) {
  leachingDF <- read.table(file.path(workDir, projectName, soil, 'DAILY.PLT'), col.names = c('DAY', 'ACCUMULATED PRECIPITATION (CM)',  'ACCUMULATED INFILTRATION (CM)', 'TEMP BREAK THROUGH CURVE (C)', 'WATER FLUX INTO GW (CM/DAY)', 'ACTUAL EVAPORATION (CM)', 'ACTUAL TRANSPIRATION (CM)', 'SURFACE MULCH MASS (KG/HA)', 'TOTAL NO3-N IN PROFILE (KG/HA)', 'NO3 FLUX INTO GW (UG/CM^2/DAY)', 'NO3 BREAK THROUGH (MG/L)', 'MINERALIZATION (KG/HA)', 'LEAF AREA INDEX', 'PLANT HEIGHT (CM)', 'PLANT AREA COVER (%)', 'DEPTH OF ROOTS (CM)', 'WATER STRESS', 'TEMPERATURE STRESS', 'NUTRIENT STRESS', 'NUMBER OF LIVE PLANTS', 'TOT ABOVE GRD BIOMASS (KG/HA)'), header = FALSE, skip=156)
  leachingDF$date <- seq(as.Date("1983/10/1"), as.Date("2020/10/1"), "days")
  leachingDF$year <- format.Date(leachingDF$date, '%Y')
  leachingDF
}

dailyReport_df <- dailyReport(projectName = 'SteadyStateRuns', 'CoarseSHR')
# head(dailyReport_df)
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
#

sum(dailyReport_df$WATER.FLUX.INTO.GW..CM.DAY.) #329 cm H20
sum(dailyReport_df$NO3.FLUX.INTO.GW..UG.CM.2.DAY.)/sum(dailyReport_df$WATER.FLUX.INTO.GW..CM.DAY.) #49.17823 ppm NO3 in GW recharge
mean(dailyReport_df$NO3.BREAK.THROUGH..MG.L.) #57.07079
sum(dailyReport_df$MINERALIZATION..KG.HA.) #6413.334

plot(dailyReport_df$date, dailyReport_df$MINERALIZATION..KG.HA., type='l')
plot(dailyReport_df$date, dailyReport_df$TOT.ABOVE.GRD.BIOMASS..KG.HA., type='l')
plot(dailyReport_df$date, dailyReport_df$NUMBER.OF.LIVE.PLANTS, type='l')
plot(dailyReport_df$date, dailyReport_df$WATER.FLUX.INTO.GW..CM.DAY., type='l')
plot(dailyReport_df$date, dailyReport_df$NUTRIENT.STRESS, type = 'l')
plot(dailyReport_df$date, dailyReport_df$WATER.STRESS, type = 'l')
plot(dailyReport_df$date, dailyReport_df$SURFACE.MULCH.MASS..KG.HA., type = 'l')
plot(dailyReport_df$date, dailyReport_df$TOTAL.NO3.N.IN.PROFILE..KG.HA., type = 'l')
plot(dailyReport_df$date, dailyReport_df$ACTUAL.EVAPORATION..CM., type = 'l')
plot(dailyReport_df$date, dailyReport_df$ACTUAL.TRANSPIRATION..CM., type = 'l')
plot(dailyReport_df$date, dailyReport_df$TEMP.BREAK.THROUGH.CURVE..C., type = 'l')
plot(dailyReport_df$date, dailyReport_df$NO3.BREAK.THROUGH..MG.L., type = 'l')


tapply(dailyReport_df$MINERALIZATION..KG.HA., dailyReport_df$year, sum)
tapply(dailyReport_df$NO3.FLUX.INTO.GW..UG.CM.2.DAY., dailyReport_df$year, function(x) sum(x)/10)
as.data.frame(tapply(dailyReport_df$NUTRIENT.STRESS, dailyReport_df$year, function(x) sum(x < 0.9 & x > 0)))


as.Date('1984-07-22', format='%Y-%m-%d')
dailyReport_df[dailyReport_df$date==as.Date('1984-07-22', format='%Y-%m-%d'),]

#calculate daily net mineralization
# projectName <- 'Parlier_1983_2021test'
# soil <- 'CoarseSHR_automation'
N_MBL <- function(projectName, soil) {
  df <- readLines(file.path(workDir, projectName, soil, 'MBLNIT.OUT'))
  dailyMin <- df[grepl('DAILY MINERALIZATION', df)]
  dailyImm <- df[grepl('DAILY IMMOBILIZATION', df)]
  dailyMin <- as.numeric(gsub('DAILY MINERALIZATION', '', dailyMin))
  dailyImm <- as.numeric(gsub('DAILY IMMOBILIZATION', '', dailyImm))
  result <- data.frame(date=seq(as.Date("1983/10/1"), as.Date("2021/4/30"), "days"), mineralization_kg_ha=dailyMin, immobilization_kg_ha=dailyImm,stringsAsFactors = FALSE)
  result$net_min_kg_ha <- result$mineralization_kg_ha - result$immobilization_kg_ha
  result$year <- format.Date(result$date, '%Y')
  result$month <- format.Date(result$date, '%B')
  result
}
n_mbl <- N_MBL(projectName = 'Parlier_1983_2021test', soil = 'CoarseSHR_automation')
head(n_mbl)
tapply(n_mbl$mineralization_kg_ha, n_mbl$year, sum)
tapply(n_mbl$immobilization_kg_ha, n_mbl$year, sum)
tapply(n_mbl$net_min_kg_ha, n_mbl$year, sum)
plot(n_mbl$date, n_mbl$mineralization_kg_ha, type='l', col='red')
lines(n_mbl$date, n_mbl$immobilization_kg_ha, col='blue')
plot(n_mbl$date, n_mbl$net_min_kg_ha, type='l', col='black')
tapply(n_mbl$net_min_kg_ha, n_mbl$year, function(x) sum(x < 0))
tapply(n_mbl$net_min_kg_ha, n_mbl$month, sum)
table(n_mbl$month[n_mbl$net_min_kg_ha < 0])

management