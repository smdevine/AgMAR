workDir <- 'C:/Users/smdevine/Desktop/post doc/Dahlke/RZWQM/projects/PulseSoilClimate/ClimateRuns_Silage/'
met_stn <- 'Shafter'
compnames <- c('Hanford', 'Delhi', 'Wasco', 'Hesperia', 'Milham', 'Tujunga', 'Panoche', 'Cerini', 'Yolo', 'Colpien', 'Tehama', 'Capay', 'Clear Lake', 'Tulare', 'Kimberlina', 'Columbia', 'Excelsior', 'Atwater', 'Rincon', 'Sycamore', 'Conejo', 'Westhaven', 'Pleito', 'Lokern', 'Merced', 'Cropley', 'Tachi', 'Myers', 'Lofgren', 'Wekoda')
#Wekoda, Clear Lake, and Tulare also defined as stress soils on 2/28/22 and Capay v2 on 3/1/22
writeOverallResults(compnames = compnames, scenario = 'SteadyStateRuns', weather_stn = met_stn)
writeOverallResults(compnames = compnames, scenario = 'AgMAR_Jan3d', weather_stn = met_stn)
writeOverallResults(compnames = compnames, scenario = 'AgMAR_Jan7d', weather_stn = met_stn)
writeOverallResults(compnames = compnames, scenario = 'AgMAR_Mar3d', weather_stn = met_stn)
writeOverallResults(compnames = compnames, scenario = 'AgMAR_Mar7d', weather_stn = met_stn)
writeOverallResults(compnames = compnames, scenario = 'AgMAR_21d', weather_stn = met_stn)

writeSeasonalResults(compnames = compnames, scenario = 'SteadyStateRuns', weather_stn = met_stn)
writeSeasonalResults(compnames = compnames, scenario = 'AgMAR_Jan7d', weather_stn = met_stn)
writeSeasonalResults(compnames = compnames, scenario = 'AgMAR_Jan3d', weather_stn = met_stn)
writeSeasonalResults(compnames = compnames, scenario = 'AgMAR_Mar7d', weather_stn = met_stn)
writeSeasonalResults(compnames = compnames, scenario = 'AgMAR_Mar3d', weather_stn = met_stn)
writeSeasonalResults(compnames = compnames, scenario = 'AgMAR_21d', weather_stn = met_stn)

writeMonthlyResults(compnames = compnames, scenario = 'SteadyStateRuns', weather_stn = met_stn)
writeMonthlyResults(compnames = compnames, scenario = 'AgMAR_Jan7d', weather_stn = met_stn)
writeMonthlyResults(compnames = compnames, scenario = 'AgMAR_Jan3d', weather_stn = met_stn)
writeMonthlyResults(compnames = compnames, scenario = 'AgMAR_Mar7d', weather_stn = met_stn)
writeMonthlyResults(compnames = compnames, scenario = 'AgMAR_Mar3d', weather_stn = met_stn)
writeMonthlyResults(compnames = compnames, scenario = 'AgMAR_21d', weather_stn = met_stn)

writeHarvestResults(compnames = compnames, scenario = 'SteadyStateRuns', weather_stn = met_stn)
writeHarvestResults(compnames = compnames, scenario = 'AgMAR_Jan3d', weather_stn = met_stn)
writeHarvestResults(compnames = compnames, scenario = 'AgMAR_Jan7d', weather_stn = met_stn)
writeHarvestResults(compnames = compnames, scenario = 'AgMAR_Mar3d', weather_stn = met_stn)
writeHarvestResults(compnames = compnames, scenario = 'AgMAR_Mar7d', weather_stn = met_stn)
writeHarvestResults(compnames = compnames, scenario = 'AgMAR_21d', weather_stn = met_stn)