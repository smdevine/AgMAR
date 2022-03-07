#first, source functions in prepare_run_maize_only.R
stress_soils <- c('Capay') #'Willows'
compnames <- c('Hanford', 'Delhi', 'Wasco', 'Hesperia', 'Milham', 'Tujunga', 'Panoche', 'Cerini', 'Yolo', 'Colpien', 'Tehama', 'Capay', 'Clear Lake', 'Tulare') #'Willows'
compnames2 <- c('Kimberlina', 'Columbia', 'Excelsior', 'Atwater', 'Rincon', 'Sycamore', 'Conejo', 'Westhaven', 'Pleito', 'Lokern', 'Merced', 'Cropley', 'Tachi', 'Myers') #'Esquon' 'Porterville'

#prepare steady state templates
steady_state_helper <- function(soilnames, stn, stress_soils=c('Capay', 'Willows')) {
  for(i in 1:length(soilnames)) {
    ipnames_fix(station = stn, scenario = 'SteadyStateRuns', soil = soilnames[i], met_fname = paste0(stn, '_1983_2021.MET'), brk_fname = paste0(stn, '_1983_2021.BRK'), ana_fname = paste0(soilnames[i], '_SteadyStateRun.ana'))
    copyRZINIT(stn, 'SteadyStateRuns', soilnames[i])
    gen_input <- templateRZWQM(stn, 'SteadyStateRuns', soilnames[i], 'RZWQM_init.DAT')
    suelo <- soilInfo(stn, 'SteadyStateRuns', soilnames[i], 'RZWQM.DAT')
    planting <- writePlantingPlan()
    fert <- writeFertPlan(end_year = 2020)
    tillage <- writeTillagePlan(depth = '15.0')
    Irr <- if(soilnames[i] %in% stress_soils) {writeIrrPlan(cornADtrigger1 = '0.8', cornADtrigger2 = '0.7')} else{writeIrrPlan()}
    writeRZWQM(station = stn, scenario = 'SteadyStateRuns', soil = soilnames[i], input=gen_input, soil_input = suelo, planting_data = planting, fert_plan = fert, tillage_plan = tillage, Irr_plan = Irr)
    steady_state_setup(station = stn, scenario = 'SteadyStateRuns', soil = soilnames[i], type = 'run 1', suelo)
  }
}

#steady state function helper 2
steady_state_helper2 <- function(soilnames, stn) { 
  for(i in 1:length(soilnames)) {
    suelo <- soilInfo(stn, 'SteadyStateRuns', soilnames[i], 'RZWQM.DAT')
    steady_state_setup(station = stn, scenario = 'SteadyStateRuns', soil = soilnames[i], type = 'run 2', soil_input = suelo)
  }
}

# compnames2 <- compnames2[c(2:3,12)]
# compnames2 <- 'Merced'
compnames2 <- c('Lofgren', 'Wekoda')
#prepare steady state
steady_state_helper(soilnames = compnames2, stn = 'Parlier')

#then run
steady_state_helper2(soilnames = compnames2, stn = 'Parlier')

#prepare AgMAR scenarios
#Jan 7d interval
AgMAR_runs(stn = 'Parlier', scn = 'AgMAR_Jan7d', suelos = compnames2, AgMAR_month = '1', AgMAR_days = c('5', '12', '19', '26'), fake_planting = '4    1   1992', fake_harvest = '27 1 1992')
#Jan 3d interval
AgMAR_runs(stn = 'Parlier', scn = 'AgMAR_Jan3d', suelos = compnames2, AgMAR_month = '1', AgMAR_days = c('11', '14', '17', '20'), fake_planting = '4    1   1992', fake_harvest = '27 1 1992')
#Mar 3d interval
AgMAR_runs(stn = 'Parlier', scn = 'AgMAR_Mar3d', suelos = compnames2, AgMAR_month = '3', AgMAR_days = c('11', '14', '17', '20'), fake_planting = '4    3   1992', fake_harvest = '27 3 1992')
#Mar 7d interval
AgMAR_runs(stn = 'Parlier', scn = 'AgMAR_Mar7d', suelos = compnames2, AgMAR_month = '3', AgMAR_days = c('5', '12', '19', '26'), fake_planting = '4    3   1992', fake_harvest = '27 3 1992')

#21-day interval starting Jan 7
AgMAR_runs_LF(stn = 'Parlier', scn = 'AgMAR_21d', suelos = compnames2, AgMAR_month = c('1', '1', '2', '3'), AgMAR_days = c('8', '29', '19', '12'), fake_planting = c('7    1', '28   1', '18   2', '11   3'), fake_harvest = c('9 1', '30 1', '20 2', '13 3'))

#aggregate results
#overall
writeOverallResults(compnames = compnames2, scenario = 'SteadyStateRuns', weather_stn = 'Parlier')
writeOverallResults(compnames = compnames2, scenario = 'AgMAR_21d', weather_stn = 'Parlier')
writeOverallResults(compnames = compnames2, scenario = 'AgMAR_Jan3d', weather_stn = 'Parlier')
writeOverallResults(compnames = compnames2, scenario = 'AgMAR_Jan7d', weather_stn = 'Parlier')
writeOverallResults(compnames = compnames2, scenario = 'AgMAR_Mar3d', weather_stn = 'Parlier')
writeOverallResults(compnames = compnames2, scenario = 'AgMAR_Mar7d', weather_stn = 'Parlier')

#seasonal
writeSeasonalResults(compnames = compnames2, scenario = 'SteadyStateRuns', weather_stn = 'Parlier')
writeSeasonalResults(compnames = compnames2, scenario = 'AgMAR_Jan7d', weather_stn = 'Parlier')
writeSeasonalResults(compnames = compnames2, scenario = 'AgMAR_Jan3d', weather_stn = 'Parlier')
writeSeasonalResults(compnames = compnames2, scenario = 'AgMAR_Mar7d', weather_stn = 'Parlier')
writeSeasonalResults(compnames = compnames2, scenario = 'AgMAR_Mar3d', weather_stn = 'Parlier')
writeSeasonalResults(compnames = compnames2, scenario = 'AgMAR_21d', weather_stn = 'Parlier')

#monthly
writeMonthlyResults(compnames = compnames2, scenario = 'SteadyStateRuns', weather_stn = 'Parlier')
writeMonthlyResults(compnames = compnames2, scenario = 'AgMAR_Jan7d', weather_stn = 'Parlier')
writeMonthlyResults(compnames = compnames2, scenario = 'AgMAR_Jan3d', weather_stn = 'Parlier')
writeMonthlyResults(compnames = compnames2, scenario = 'AgMAR_Mar7d', weather_stn = 'Parlier')
writeMonthlyResults(compnames = compnames2, scenario = 'AgMAR_Mar3d', weather_stn = 'Parlier')
writeMonthlyResults(compnames = compnames2, scenario = 'AgMAR_21d', weather_stn = 'Parlier')
