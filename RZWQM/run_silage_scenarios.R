#important: read-in prepare_run_maize_only.R and fine_soil_functions.R first
#then read this in to line 231 before running line by line manually
met_stn <- 'Shafter'
stn_no <- '5'
compnames <- c('Hanford', 'Delhi', 'Wasco', 'Hesperia', 'Milham', 'Tujunga', 'Panoche', 'Cerini', 'Yolo', 'Colpien', 'Tehama', 'Capay', 'Clear Lake', 'Tulare', 'Kimberlina', 'Columbia', 'Excelsior', 'Atwater', 'Rincon', 'Sycamore', 'Conejo', 'Westhaven', 'Pleito', 'Lokern', 'Merced', 'Cropley', 'Tachi', 'Myers', 'Lofgren', 'Wekoda', 'Guijarral', 'Granoso', 'Chanac')
compnames <- compnames[order(compnames)]
compnames
fine_soils
stress_soils <- c('Capay', 'Clear Lake', 'Tulare', 'Wekoda', 'Tujunga', 'Pleito') #'Willows'
workDir <- 'C:/Users/smdevine/Desktop/post doc/Dahlke/RZWQM/projects/PulseSoilClimate/ClimateRuns_Silage/'
cimisDir <- 'C:/Users/smdevine/Desktop/post doc/Dahlke/RZWQM/cimis'
wetyrs <- read.csv(file.path(cimisDir, paste0(met_stn, '_stn', stn_no), 'wettest_ten_years_WY.csv')) #using WY basis as determinant as of 4/6/22
wetyrs <- as.character(wetyrs$year)
wetyrs
#read in relevant functions in prepare_run_maize_only.R
ipnames_fix_v2 <- function(station, project, scenario, soil, met_fname, brk_fname, ana_fname) {
  ipnames <- readLines(file.path(workDir, station, scenario, soil, 'ipnames.DAT'))
  path1 <- unlist(strsplit(ipnames[1], '\\\\'))
  path1[10] <- project
  path1[11] <- station
  path1[12] <- scenario
  path1[13] <- soil
  # ipnames[1] <- cat(path1, sep='\\')
  
  path2 <- unlist(strsplit(ipnames[2], '\\\\'))
  path2[10] <- project
  path2[11] <- station
  path2[12] <- scenario
  path2[13] <- soil
  # ipnames[2] <- cat(paste(path2, collapse='\\'))
  
  path3 <- unlist(strsplit(ipnames[3], '\\\\'))
  path3[10] <- project
  path3[11] <- station
  path3[13] <- met_fname
  # ipnames[3] <- cat(paste(path3, collapse='\\'))
  
  path4 <- unlist(strsplit(ipnames[4], '\\\\'))
  path4[10] <- project
  path4[11] <- station
  path4[13] <- brk_fname
  # ipnames[4] <- cat(paste(path4, collapse='\\'))
  
  path5 <- unlist(strsplit(ipnames[5], '\\\\'))
  path5[10] <- project
  path5[11] <- station
  path5[12] <- scenario
  path5[13] <- soil
  # ipnames[5] <- cat(paste(path5, collapse='\\'))
  
  path6 <- unlist(strsplit(ipnames[6], '\\\\'))
  path6[10] <- project
  path6[11] <- station
  path6[12] <- scenario
  path6[13] <- soil
  # ipnames[6] <- cat(paste(path6, collapse='\\'))
  
  path7 <- unlist(strsplit(ipnames[7], '\\\\'))
  path7[10] <- project
  path7[11] <- station
  # path7 <- unlist(strsplit('C:\\Users\\smdevine\\Desktop\\post doc\\Dahlke\\RZWQM\\projects\\PulseSoilClimate\\ClimateRuns_Silage\\Parlier\\Meteorology\\RZWQM2MetData.SNO', '\\\\'))
  
  path8 <- unlist(strsplit(ipnames[8], '\\\\'))
  path8[10] <- project
  path8[11] <- station
  path8[13] <- ana_fname
  # ipnames[8] <- cat(paste(path8, collapse='\\'))
  
  fileconn <- file(file.path(workDir, station, scenario, soil, 'ipnames.DAT'), 'w')
  cat(path1, file = fileconn, sep='\\')
  cat('\n', file = fileconn)
  cat(path2, file = fileconn, sep = '\\')
  cat('\n', file = fileconn)
  cat(path3, file = fileconn, sep = '\\')
  cat('\n', file = fileconn)
  cat(path4, file = fileconn, sep = '\\')
  cat('\n', file = fileconn)
  cat(path5, file = fileconn, sep = '\\')
  cat('\n', file = fileconn)
  cat(path6, file = fileconn, sep = '\\')
  cat('\n', file = fileconn)
  cat(path7, file = fileconn, sep = '\\')
  cat('\n', file = fileconn)
  cat(path8, file = fileconn, sep = '\\')
  cat('\n', file = fileconn)
  writeLines(ipnames[9:length(ipnames)], con = fileconn)
  close(fileconn)
  projDir <- path1[1:(length(path1)-1)]
  mzdssat <- readLines(file.path(workDir, station, scenario, soil, 'mzdssat.rzx'))
  fileconn2 <- file(file.path(workDir, station, scenario, soil, 'mzdssat.rzx'), 'w')
  writeLines(mzdssat[1:66], con = fileconn2)
  cat(projDir, file= fileconn2, sep = '\\')
  writeLines('\\', con = fileconn2)
  # cat('\n', file = fileconn2)
  writeLines(mzdssat[68:length(mzdssat)], con = fileconn2)
  close(fileconn2)
}
# mzdssat <- readLines('C:/Users/smdevine/Desktop/post doc/Dahlke/RZWQM/projects/PulseSoilClimate/ClimateRuns_Silage/Shafter/AgMAR_Jan3d/Cerini/mzdssat.rzx')
# mzdssat[66]

#need additional function for AgMAR scenarios that require winter crops to enable irrigation (see writeMgmtFiles_Flood_NoCC.R for example)
writePlantingPlan_Silage <- function(corn_line1 = '1  15   4  1984  20.0   3 40000.0   0 -99.0   10 -99.0   -99',  corn_line2 = '               1 0.95   0 0.0  0 0 0', corn_line3 = '               10.0  0.95   4 0 0', start_year = 1984, end_year = 2020) { #harvest at 95% maturity with 95% harvest recovery of all aboveground biomass (10 cm cutting height)
  years <- start_year:end_year
  plantings <- length(years)
  planting_plan <- do.call(c, lapply(years, function(x) {
    corn_line1_final <- corn_line1
    substring(corn_line1_final, 12, 15) <- as.character(x)
    c(corn_line1_final, corn_line2, corn_line3)
  }))
  planting_data <- c(as.character(plantings), planting_plan)
  planting_data
}

#planting plan for enabling AgMAR during wet years
writePlantingPlan_AgMAR_Silage <- function(start_year=1984, end_year=2020, wet_years, cover_crop = FALSE, rye_date_planting = '14   1   1992', rye_date_harvest = '28 1 1993', rye_emergence = '10', corn_line1 = '1  15   4  1984  20.0   3 40000.0   0 -99.0   10 -99.0   -99', corn_line2 = '               1 0.95   0 0.0  0 0 0', corn_line3 = '               10.0  0.95   4 0 0', rye_line1 = paste('3', rye_date_planting, '10.0   2 0.0   0 -99.0', rye_emergence, '-99.0   -99', sep = '  '), rye_line2 = paste('               3 0.0   0 0.0', rye_date_harvest, sep = ' '), rye_line3 = '               10.0  0.9   4 0 0') {
  years <- start_year:end_year
  plantings <- length(years) + length(wet_years)
  planting_plan <- do.call(c, lapply(years, function(x) {
    corn_line1_final <- corn_line1
    substring(corn_line1_final, 12, 15) <- as.character(x)
    if((x)%in%wet_years) {
      rye_line1_final <- rye_line1
      substring(rye_line1_final, 13, 16) <- as.character(x)
      rye_line2_final <- rye_line2
      substring(rye_line2_final, 35, 38) <- if(cover_crop){as.character((x+1))}else{as.character(x)}
      c(rye_line1_final, rye_line2_final, rye_line3, corn_line1_final, corn_line2, corn_line3)
    } else {c(corn_line1_final, corn_line2, corn_line3)}
  }))
  planting_data <- c(as.character(plantings), planting_plan)
  planting_data
}

#AgMar planting for low frequency applications
writePlantingPlan_AgMAR_LF_Silage <- function (start_year=1984, end_year=2020, wet_years, cover_crop = FALSE, rye_date_planting, rye_date_harvest, rye_emergence = '10', corn_line1 = '1  15   4  1984  20.0   3 40000.0   0 -99.0   10 -99.0   -99', corn_line2 = '               1 0.95   0 0.0  0 0 0', corn_line3 = '               10.0  0.95   4 0 0') {
  rye_planting <- function(y, z, k) { #harvest at 95% maturity with 95% harvest recovery of all aboveground biomass (10 cm cutting height)
    a <- paste(y, as.character(k), sep='  ')
    b <- paste(z, as.character(k), sep=' ')
    rye_line1 <- paste('3', a, '10.0   2 0.0   0 -99.0', rye_emergence, '-99.0   -99', sep = '  ')
    rye_line2 <- paste('               3 0.0   0 0.0', b, sep = ' ')
    rye_line3 <- '               10.0  0.9   4 0 0'
    c(rye_line1, rye_line2, rye_line3)
  }
  years <- start_year:end_year
  plantings <- length(years) + length(wet_years)*4
  planting_plan <- do.call(c, lapply(years, function(x) {
    corn_line1_final <- corn_line1
    substring(corn_line1_final, 12, 15) <- as.character(x)
    if((x)%in%wet_years) {
      rye_lines <- as.character(mapply(FUN=rye_planting, y = rye_date_planting, z = rye_date_harvest, k = x, SIMPLIFY = TRUE))
      c(rye_lines, corn_line1_final, corn_line2, corn_line3)
    } else {c(corn_line1_final, corn_line2, corn_line3)}
  }))
  planting_data <- c(as.character(plantings), planting_plan)
  planting_data
}

#AgMAR scenario functions
AgMAR_runs_Silage <- function(stn, scn, suelos, AgMAR_month, AgMAR_days, fake_planting, fake_harvest, wetyears) {
  met <- paste0(stn, '_1983_2021.MET')
  brk <- paste0(stn, '_1983_2021.BRK')
  for(i in 1:length(suelos)) {
    print(i)
    ana <- paste0(suelos[i], '_', scn, '.ana')
    ipnames_fix_v2(station = stn, project = 'ClimateRuns_Silage', scenario = scn, soil = suelos[i], met_fname = met, brk_fname = brk, ana_fname = ana)
    copyRZINIT(stn, scn, suelos[i])
    gen_input <- templateRZWQM(station=stn, scenario=scn, soil=suelos[i], fname='RZWQM_init.DAT', ChangeOMdecay = TRUE, fastOMdecay = '2.500E-07', intOMdecay = '5.000E-08', slowOMdecay='4.500E-10', ChangeRvals = TRUE, R34=0.1, R45 = 0.1)
    suelo_input <- soilInfo(stn, scn, suelos[i], 'RZWQM.DAT')
    planting <- writePlantingPlan_AgMAR_Silage(wet_years=as.integer(wetyears), rye_date_planting = fake_planting, rye_date_harvest = fake_harvest)
    fert <- writeFertPlan(end_year = 2020)
    tillage <- writeTillagePlan(depth = '15.0')
    Irr <- if(suelos[i] %in% stress_soils) {writeIrrPlan_AgMAR(cornADtrigger1 = '0.8', cornADtrigger2 = '0.7', flood_month=AgMAR_month, flood_days=AgMAR_days, wet_yrs=as.integer(wetyears))} else{writeIrrPlan_AgMAR(flood_month=AgMAR_month, flood_days=AgMAR_days, wet_yrs=as.integer(wetyears))}
    writeRZWQM(station = stn, scenario = scn, soil = suelos[i], input=gen_input, soil_input = suelo_input, planting_data = planting, fert_plan = fert, tillage_plan = tillage, Irr_plan = Irr)
    steady_state_setup(station = stn, scenario = scn, soil = suelos[i], type = 'run 2', soil_input = suelo_input)
  }
}
AgMAR_runs_LF_Silage <- function(stn, scn, suelos, AgMAR_month, AgMAR_days, fake_planting, fake_harvest, wetyears) {
  met <- paste0(stn, '_1983_2021.MET')
  brk <- paste0(stn, '_1983_2021.BRK')
  for(i in 1:length(suelos)) {
    print(i)
    ana <- paste0(suelos[i], '_', scn, '.ana')
    ipnames_fix_v2(station = stn, project = 'ClimateRuns_Silage', scenario = scn, soil = suelos[i], met_fname = met, brk_fname = brk, ana_fname = ana)
    # copyRZINIT(stn, scn, suelos[i])
    gen_input <- templateRZWQM(station=stn, scenario=scn, soil=suelos[i], fname='RZWQM_init.DAT', ChangeOMdecay = TRUE, fastOMdecay = '2.500E-07', intOMdecay = '5.000E-08', slowOMdecay='4.500E-10', ChangeRvals = TRUE, R34=0.1, R45 = 0.1)
    suelo_input <- soilInfo(stn, scn, suelos[i], 'RZWQM.DAT')
    planting <- writePlantingPlan_AgMAR_LF_Silage(wet_years=as.integer(wetyears), rye_date_planting = fake_planting, rye_date_harvest = fake_harvest)
    fert <- writeFertPlan(end_year = 2020)
    tillage <- writeTillagePlan(depth = '15.0')
    Irr <- if(suelos[i] %in% stress_soils) {writeIrrPlan_AgMAR_LF(cornADtrigger1 = '0.8', cornADtrigger2 = '0.7', flood_month=AgMAR_month, flood_days=AgMAR_days, wet_yrs=as.integer(wetyears))} else{writeIrrPlan_AgMAR_LF(flood_month=AgMAR_month, flood_days=AgMAR_days, wet_yrs=as.integer(wetyears))}
    writeRZWQM(station = stn, scenario = scn, soil = suelos[i], input=gen_input, soil_input = suelo_input, planting_data = planting, fert_plan = fert, tillage_plan = tillage, Irr_plan = Irr)
  }
}
#fine soil function
AgMAR_runs_fine_silage <- function(stn, scn, suelos, AgMAR_month, AgMAR_days, fake_planting, fake_harvest, comp_df, wetyears) {
  met <- paste0(stn, '_1983_2021.MET')
  brk <- paste0(stn, '_1983_2021.BRK')
  for(i in 1:length(suelos)) {
    print(i)
    AgMAR_m <- AgMAR_month[[which(names(AgMAR_month)==comp_df$AgMARplan[comp_df$compnames==suelos[i]])]]
    AgMAR_d <- AgMAR_days[[which(names(AgMAR_days)==comp_df$AgMARplan[comp_df$compnames==suelos[i]])]]
    soilksat <- comp_df$ksat_cm_day[comp_df$compnames==suelos[i]]
    ana <- paste0(suelos[i], '_', scn, '.ana')
    ipnames_fix_v2(station = stn, project = 'ClimateRuns_Silage', scenario = scn, soil = suelos[i], met_fname = met, brk_fname = brk, ana_fname = ana)
    # copyRZINIT(stn, scn, suelos[i])
    gen_input <- templateRZWQM(station=stn, scenario=scn, soil=suelos[i], fname='RZWQM_init.DAT', ChangeOMdecay = TRUE, fastOMdecay = '2.500E-07', intOMdecay = '5.000E-08', slowOMdecay='4.500E-10', ChangeRvals = TRUE, R34=0.1, R45 = 0.1)
    suelo_input <- soilInfo(stn, scn, suelos[i], 'RZWQM.DAT')
    planting <- writePlantingPlan_AgMAR_Silage(wet_years=as.integer(wetyears), rye_date_planting = fake_planting, rye_date_harvest = fake_harvest)
    fert <- writeFertPlan(end_year = 2020)
    tillage <- writeTillagePlan(depth = '15.0')
    Irr <- if(suelos[i] %in% stress_soils) {writeIrrPlan_AgMAR_fine(cornADtrigger1 = '0.8', cornADtrigger2 = '0.7', flood_month=AgMAR_m, flood_days=AgMAR_d, wet_yrs=as.integer(wetyears), soil_Ksat = soilksat)} else{writeIrrPlan_AgMAR_fine(flood_month=AgMAR_m, flood_days=AgMAR_d, wet_yrs=as.integer(wetyears), soil_Ksat = soilksat)}
    writeRZWQM(station = stn, scenario = scn, soil = suelos[i], input=gen_input, soil_input = suelo_input, planting_data = planting, fert_plan = fert, tillage_plan = tillage, Irr_plan = Irr)
    steady_state_setup(station = stn, scenario = scn, soil = suelos[i], type = 'run 2', soil_input = suelo_input)
  }
}
#generate a batch run set of files
#write file paths and run.bat file
automateRun <- function(soilnames, station, scenario, workDir_text, wait=FALSE, wait_time) {
  paths_shortcut <- paste0('path', 1:length(soilnames))
  if(!dir.exists(file.path(workDir, station, scenario, 'batch'))) {
    dir.create(file.path(workDir, station, scenario, 'batch'))
  }
  # file.copy(from='D:/PostDoc/Trafficability/climate_runs/2005-03-15/cell_174576_2005-03-15/paths/H1D_CALC.EXE', to=file.path(modelDir, sub_Dir, 'paths', 'H1D_CALC.EXE'))
  runbat <- file(file.path(workDir, station, scenario, 'batch', 'run.bat'), 'w')
  if(wait){writeLines(paste('timeout /t', wait_time), con = runbat)}
  for (i in seq_along(paths_shortcut)) {
    writeLines(paste('cd "', file.path(workDir_text, station, scenario, soilnames[i]), '"', sep = ''), con = runbat)
    writeLines('START RZWQMrelease.exe', con = runbat)
    writeLines('timeout /t 90', con = runbat)
  }
  close(runbat)
}
# compnames <- c('Guijarral', 'Granoso', 'Chanac')
compnames <- 'Granoso'
#prepare steady state templates
for(i in 1:length(compnames)) {
  ipnames_fix_v2(station = met_stn, project = 'ClimateRuns_Silage', scenario = 'SteadyStateRuns', soil = compnames[i], met_fname = paste0(met_stn, '_1983_2021.MET'), brk_fname = paste0(met_stn, '_1983_2021.BRK'), ana_fname = paste0(compnames[i], '_SteadyStateRun.ana'))
  copyRZINIT(met_stn, 'SteadyStateRuns', compnames[i])
  gen_input <- templateRZWQM(met_stn, 'SteadyStateRuns', compnames[i], 'RZWQM_init.DAT', ChangeOMdecay = TRUE, fastOMdecay = '2.500E-07', intOMdecay = '5.000E-08', slowOMdecay='4.500E-10', ChangeRvals = TRUE, R34=0.1, R45 = 0.1) #default is fastOMdecay=2.500E-07, intOMdecay=5.000E-08, and slowOMdecay=4.500E-10 and ChangeOMdecay=FALSE; and ChangeRvals=FALSE, R45=0.4
  suelo <- soilInfo(met_stn, 'SteadyStateRuns', compnames[i], 'RZWQM.DAT')
  planting <- writePlantingPlan_Silage()
  fert <- writeFertPlan(end_year = 2020)
  tillage <- writeTillagePlan(depth = '15.0')
  Irr <- if(compnames[i] %in% stress_soils) {writeIrrPlan(cornADtrigger1 = '0.8', cornADtrigger2 = '0.7')} else{writeIrrPlan()}
  writeRZWQM(station = met_stn, scenario = 'SteadyStateRuns', soil = compnames[i], input=gen_input, soil_input = suelo, planting_data = planting, fert_plan = fert, tillage_plan = tillage, Irr_plan = Irr)
  steady_state_setup(station = met_stn, scenario = 'SteadyStateRuns', soil = compnames[i], type = 'run 1', suelo)
  rm(gen_input, planting, fert, tillage, Irr, suelo)
}
rm(i)
automateRun(soilnames = compnames, station = met_stn, scenario = 'SteadyStateRuns', workDir_text = 'C:/Users/smdevine/Desktop/post doc/Dahlke/RZWQM/projects/PulseSoilClimate/ClimateRuns_Silage')

#run then change set-up and run again
for(i in 1:length(compnames)) {
  suelo <- soilInfo(met_stn, 'SteadyStateRuns', compnames[i], 'RZWQM.DAT')
  steady_state_setup(station = met_stn, scenario = 'SteadyStateRuns', soil = compnames[i], type = 'run 2', soil_input = suelo)
  rm(suelo)
}
rm(i)
automateRun(soilnames = compnames, station = met_stn, scenario = 'SteadyStateRuns', workDir_text = 'C:/Users/smdevine/Desktop/post doc/Dahlke/RZWQM/projects/PulseSoilClimate/ClimateRuns_Silage')

#AgMAR scenarios
#steady state folders must be manually copied to these directories first!!!
#Jan 3d interval
for (j in 1:length(compnames)) {
  if(compnames[j] %in% fine_soils) {
    AgMAR_runs_fine_silage(stn = met_stn, scn = 'AgMAR_Jan3d', suelos = compnames[j], AgMAR_month = AgMAR_3d_irrmonths_early, AgMAR_days = AgMAR_3d_irrdays_early,  fake_planting = '1    1   1992', fake_harvest = '25 2 1992', comp_df = comp_ksat, wetyears = wetyrs)
  } else {AgMAR_runs_Silage(stn = met_stn, scn = 'AgMAR_Jan3d', suelos = compnames[j], AgMAR_month = '1', AgMAR_days = c('11', '14', '17', '20'), fake_planting = '4    1   1992', fake_harvest = '27 1 1992', wetyears = wetyrs)}
}
automateRun(soilnames = compnames, station = met_stn, scenario = 'AgMAR_Jan3d', workDir_text = 'C:/Users/smdevine/Desktop/post doc/Dahlke/RZWQM/projects/PulseSoilClimate/ClimateRuns_Silage') #wait = TRUE, wait_time = '2700'

#Jan 7d interval
for (j in 1:length(compnames)) {
  if(compnames[j] %in% fine_soils) {
    AgMAR_runs_fine_silage(stn = met_stn, scn = 'AgMAR_Jan7d', suelos = compnames[j], AgMAR_month = AgMAR_7d_irrmonths_early, AgMAR_days = AgMAR_7d_irrdays_early,  fake_planting = '1    1   1992', fake_harvest = '25 2 1992', comp_df = comp_ksat, wetyears = wetyrs)
  } else {AgMAR_runs_Silage(stn = met_stn, scn = 'AgMAR_Jan7d', suelos = compnames[j], AgMAR_month = '1', AgMAR_days = c('5', '12', '19', '26'), fake_planting = '4    1   1992', fake_harvest = '27 1 1992', wetyears = wetyrs)}
}
automateRun(soilnames = compnames, station = met_stn, scenario = 'AgMAR_Jan7d', workDir_text = 'C:/Users/smdevine/Desktop/post doc/Dahlke/RZWQM/projects/PulseSoilClimate/ClimateRuns_Silage', wait = F, wait_time = '300')#'3000' for all soils

#Mar 3d interval
for (j in 1:length(compnames)) {
  if(compnames[j] %in% fine_soils) {
    AgMAR_runs_fine_silage(stn = met_stn, scn = 'AgMAR_Mar3d', suelos = compnames[j], AgMAR_month = AgMAR_3d_irrmonths_late, AgMAR_days = AgMAR_3d_irrdays_late,  fake_planting = '9    2   1992', fake_harvest = '13 4 1992', comp_df = comp_ksat, wetyears = wetyrs)
  } else {AgMAR_runs_Silage(stn = met_stn, scn = 'AgMAR_Mar3d', suelos = compnames[j], AgMAR_month = '3', AgMAR_days = c('11', '14', '17', '20'), fake_planting = '4    3   1992', fake_harvest = '27 3 1992', wetyears = wetyrs)}
}
automateRun(soilnames = compnames, station = met_stn, scenario = 'AgMAR_Mar3d', workDir_text = 'C:/Users/smdevine/Desktop/post doc/Dahlke/RZWQM/projects/PulseSoilClimate/ClimateRuns_Silage', wait = F, wait_time = '600')

#Mar 7d interval
for (j in 1:length(compnames)) {
  if(compnames[j] %in% fine_soils) {
    AgMAR_runs_fine_silage(stn = met_stn, scn = 'AgMAR_Mar7d', suelos = compnames[j], AgMAR_month = AgMAR_7d_irrmonths_late, AgMAR_days = AgMAR_7d_irrdays_late,  fake_planting = '9    2   1992', fake_harvest = '13 4 1992', comp_df = comp_ksat, wetyears = wetyrs)
  } else {AgMAR_runs_Silage(stn = met_stn, scn = 'AgMAR_Mar7d', suelos = compnames[j], AgMAR_month = '3', AgMAR_days = c('5', '12', '19', '26'), fake_planting = '4    3   1992', fake_harvest = '27 3 1992', wetyears = wetyrs)}
}
automateRun(soilnames = compnames, station = met_stn, scenario = 'AgMAR_Mar7d', workDir_text = 'C:/Users/smdevine/Desktop/post doc/Dahlke/RZWQM/projects/PulseSoilClimate/ClimateRuns_Silage', wait = F, wait_time = '900')

#21-day interval starting Jan 7
for (j in 1:length(compnames)) {
  if(compnames[j] %in% fine_soils) {
    AgMAR_runs_fine_silage(stn = met_stn, scn = 'AgMAR_21d', suelos = compnames[j], AgMAR_month = AgMAR_21d_irrmonths, AgMAR_days = AgMAR_21d_irrdays,  fake_planting = '1    1   1992', fake_harvest = '13 4 1992', comp_df = comp_ksat, wetyears = wetyrs)
  } else {AgMAR_runs_LF_Silage(stn = met_stn, scn = 'AgMAR_21d', suelos = compnames[j], AgMAR_month = c('1', '1', '2', '3'), AgMAR_days = c('8', '29', '19', '12'), fake_planting = c('7    1', '28   1', '18   2', '11   3'), fake_harvest = c('9 1', '30 1', '20 2', '13 3'), wetyears = wetyrs)}
}
automateRun(soilnames = compnames, station = met_stn, scenario = 'AgMAR_21d', workDir_text = 'C:/Users/smdevine/Desktop/post doc/Dahlke/RZWQM/projects/PulseSoilClimate/ClimateRuns_Silage', wait = F, wait_time = '1200')
