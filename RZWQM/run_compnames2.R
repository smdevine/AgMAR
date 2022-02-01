
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

compnames <- c('Hanford', 'Delhi', 'Wasco', 'Hesperia', 'Milham', 'Tujunga', 'Panoche', 'Cerini', 'Yolo', 'Colpien', 'Tehama', 'Capay', 'Clear Lake', 'Willows', 'Tulare')

compnames2 <- c('Kimberlina', 'Columbia', 'Excelsior', 'Atwater', 'Rincon', 'Sycamore', 'Conejo', 'Westhaven', 'Pleito', 'Porterville', 'Lokern', 'Esquon', 'Cropley', 'Tachi', 'Myers')

#prepare steady state
steady_state_helper(soilnames = compnames, stn = 'Parlier')
#then run
steady_state_helper2(soilnames = compnames, stn = 'Parlier')

#prepare AgMAR scenarios
#Jan 7d interval
AgMAR_runs(stn = 'Parlier', scn = 'AgMAR_Jan7d', suelos = compnames, AgMAR_month = '1', AgMAR_days = c('5', '12', '19', '26'), fake_planting = '4    1   1992', fake_harvest = '27 1 1992')
#Jan 3d interval
AgMAR_runs(stn = 'Parlier', scn = 'AgMAR_Jan3d', suelos = compnames, AgMAR_month = '1', AgMAR_days = c('11', '14', '17', '20'), fake_planting = '4    1   1992', fake_harvest = '27 1 1992')
#Mar 3d interval
AgMAR_runs(stn = 'Parlier', scn = 'AgMAR_Mar3d', suelos = compnames, AgMAR_month = '3', AgMAR_days = c('11', '14', '17', '20'), fake_planting = '4    3   1992', fake_harvest = '27 3 1992')
#Mar 7d interval
AgMAR_runs(stn = 'Parlier', scn = 'AgMAR_Mar7d', suelos = compnames, AgMAR_month = '3', AgMAR_days = c('5', '12', '19', '26'), fake_planting = '4    3   1992', fake_harvest = '27 3 1992')

#21-day interval starting Jan 7
AgMAR_runs_LF(stn = 'Parlier', scn = 'AgMAR_21d', suelos = compnames, AgMAR_month = c('1', '1', '2', '3'), AgMAR_days = c('8', '29', '19', '12'), fake_planting = c('7    1', '28   1', '18   2', '11   3'), fake_harvest = c('9 1', '30 1', '20 2', '13 3'))