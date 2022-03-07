#prepare template folder for running RZWQM
#make sure template RZWQM.DAT is manually renamed to RZWQM_init.DAT to have record of changes

#fineSHR soil
ipnames_fix(station = 'Parlier', scenario = 'BaseRuns', soil = 'FineSHR', met_fname = 'Parlier_1983_2021.MET', brk_fname = 'Parlier_1983_2021.BRK', ana_fname = 'FineSHR_baserun.ana')
copyRZINIT('Parlier', 'BaseRuns', 'FineSHR')
input <- templateRZWQM('Parlier', 'BaseRuns', 'FineSHR', 'RZWQM_init.DAT')
soil_input <- soilInfo('Parlier', 'BaseRuns', 'FineSHR', 'RZWQM.DAT')
planting_data <- writePlantingPlan()
fert_plan <- writeFertPlan(end_year = 2020, tomato_N_kg_ha_yr = 200, tomato_splits = 6)
tillage_plan <- writeTillagePlan(depth = '15.0')
Irr_plan <- writeIrrPlan(tomADtrigger1='0.95', tomADtrigger2 = '0.9') #modified for fine soil due to water stress
writeRZWQM('Parlier', 'BaseRuns', 'FineSHR')

#coarseSHR
ipnames_fix(station = 'Parlier', scenario = 'BaseRuns', soil = 'CoarseSHR', met_fname = 'Parlier_1983_2021.MET', brk_fname = 'Parlier_1983_2021.BRK', ana_fname = 'CoarseSHR_baserun.ana')
copyRZINIT('Parlier', 'BaseRuns', 'CoarseSHR')
input <- templateRZWQM('Parlier', 'BaseRuns', 'CoarseSHR', 'RZWQM_init.DAT')
soil_input <- soilInfo('Parlier', 'BaseRuns', 'CoarseSHR', 'RZWQM.DAT')
planting_data <- writePlantingPlan()
fert_plan <- writeFertPlan(end_year = 2020, tomato_N_kg_ha_yr = 200, tomato_splits = 6)
tillage_plan <- writeTillagePlan(depth = '15.0')
Irr_plan <- writeIrrPlan()
writeRZWQM('Parlier', 'BaseRuns', 'CoarseSHR')

#loamy SHR
ipnames_fix(station = 'Parlier', scenario = 'BaseRuns', soil = 'LoamySHR', met_fname = 'Parlier_1983_2021.MET', brk_fname = 'Parlier_1983_2021.BRK', ana_fname = 'LoamySHR_baserun.ana')
copyRZINIT('Parlier', 'BaseRuns', 'LoamySHR')
input <- templateRZWQM('Parlier', 'BaseRuns', 'LoamySHR', 'RZWQM_init.DAT')
soil_input <- soilInfo('Parlier', 'BaseRuns', 'LoamySHR', 'RZWQM.DAT')
planting_data <- writePlantingPlan()
fert_plan <- writeFertPlan(end_year = 2020, tomato_N_kg_ha_yr = 200, tomato_splits = 6)
tillage_plan <- writeTillagePlan(depth = '15.0')
Irr_plan <- writeIrrPlan()
writeRZWQM('Parlier', 'BaseRuns', 'LoamySHR')



#steady state runs also require 'system state resets flags' in cntrl.dat to:
#run 1: 0 1 0 0 1 1 0 0 0 0 1 0
#all subsequent runs: 1 0 0 0 1 1 0 0 0 0 1 0

#run 1's
#coarse
ipnames_fix(station = 'Parlier', scenario = 'SteadyStateRuns', soil = 'CoarseSHR', met_fname = 'Parlier_1983_2021.MET', brk_fname = 'Parlier_1983_2021.BRK', ana_fname = 'CoarseSHR_SteadyState.ana')
steady_state_setup(station = 'Parlier', scenario = 'SteadyStateRuns', soil = 'CoarseSHR', type = 'run 1')

#loamy
ipnames_fix(station = 'Parlier', scenario = 'SteadyStateRuns', soil = 'LoamySHR', met_fname = 'Parlier_1983_2021.MET', brk_fname = 'Parlier_1983_2021.BRK', ana_fname = 'LoamySHR_SteadyState.ana')
steady_state_setup(station = 'Parlier', scenario = 'SteadyStateRuns', soil = 'LoamySHR', type = 'run 1')

#fine
ipnames_fix(station = 'Parlier', scenario = 'SteadyStateRuns', soil = 'FineSHR', met_fname = 'Parlier_1983_2021.MET', brk_fname = 'Parlier_1983_2021.BRK', ana_fname = 'FineSHR_SteadyState.ana')
steady_state_setup(station = 'Parlier', scenario = 'SteadyStateRuns', soil = 'FineSHR', type = 'run 1')

#then prepare run 2 to complete steady state run
steady_state_setup(station = 'Parlier', scenario = 'SteadyStateRuns', soil = 'CoarseSHR', type = 'run 2')
steady_state_setup(station = 'Parlier', scenario = 'SteadyStateRuns', soil = 'LoamySHR', type = 'run 2')
steady_state_setup(station = 'Parlier', scenario = 'SteadyStateRuns', soil = 'FineSHR', type = 'run 2')

#now use completed steady state run to complete all further AgMAR scenarios
#Jan AgMar, 7-day interval
#coarse SHR
ipnames_fix(station = 'Parlier', scenario = 'AgMAR_Jan7d', soil = 'CoarseSHR', met_fname = 'Parlier_1983_2021.MET', brk_fname = 'Parlier_1983_2021.BRK', ana_fname = 'CoarseSHR_AgMARJan7d.ana')
copyRZINIT('Parlier', 'AgMAR_Jan7d', 'CoarseSHR')
input <- templateRZWQM('Parlier', 'AgMAR_Jan7d', 'templates', 'RZWQM_init.DAT')
soil_input <- soilInfo('Parlier', 'AgMAR_Jan7d', 'CoarseSHR', 'RZWQM.DAT')
planting_data <- writePlantingPlan_AgMAR(wet_years=as.integer(c("1995", "2010", "1992", "1993", "1987", "1998", "2006", "1991", "1986", "1994")), rye_date_planting = '4    1   1992', rye_date_harvest = '27 1 1992')
fert_plan <- writeFertPlan(end_year = 2020, tomato_N_kg_ha_yr = 200, tomato_splits = 6)
tillage_plan <- writeTillagePlan(depth = '15.0')
Irr_plan <- writeIrrPlan_AgMAR(flood_month='1', flood_days=c('5', '12', '19', '26'), wet_yrs=as.integer(c("1995", "2010", "1992", "1993", "1987", "1998", "2006", "1991", "1986", "1994")))
writeRZWQM('Parlier', 'AgMAR_Jan7d', 'CoarseSHR')

#loamy SHR
ipnames_fix(station = 'Parlier', scenario = 'AgMAR_Jan7d', soil = 'LoamySHR', met_fname = 'Parlier_1983_2021.MET', brk_fname = 'Parlier_1983_2021.BRK', ana_fname = 'LoamySHR_AgMARJan7d.ana')
copyRZINIT('Parlier', 'AgMAR_Jan7d', 'LoamySHR')
input <- templateRZWQM('Parlier', 'AgMAR_Jan7d', 'templates', 'RZWQM_init.DAT')
soil_input <- soilInfo('Parlier', 'AgMAR_Jan7d', 'LoamySHR', 'RZWQM.DAT')
planting_data <- writePlantingPlan_AgMAR(wet_years=as.integer(c("1995", "2010", "1992", "1993", "1987", "1998", "2006", "1991", "1986", "1994")), rye_date_planting = '4    1   1992', rye_date_harvest = '27 1 1992')
fert_plan <- writeFertPlan(end_year = 2020, tomato_N_kg_ha_yr = 200, tomato_splits = 6)
tillage_plan <- writeTillagePlan(depth = '15.0')
Irr_plan <- writeIrrPlan_AgMAR(flood_month='1', flood_days=c('5', '12', '19', '26'), wet_yrs=as.integer(c("1995", "2010", "1992", "1993", "1987", "1998", "2006", "1991", "1986", "1994")))
writeRZWQM('Parlier', 'AgMAR_Jan7d', 'LoamySHR')

#fine SHR
ipnames_fix(station = 'Parlier', scenario = 'AgMAR_Jan7d', soil = 'FineSHR', met_fname = 'Parlier_1983_2021.MET', brk_fname = 'Parlier_1983_2021.BRK', ana_fname = 'FineSHR_AgMARJan7d.ana')
copyRZINIT('Parlier', 'AgMAR_Jan7d', 'FineSHR')
input <- templateRZWQM('Parlier', 'AgMAR_Jan7d', 'templates', 'RZWQM_init.DAT')
soil_input <- soilInfo('Parlier', 'AgMAR_Jan7d', 'FineSHR', 'RZWQM.DAT')
planting_data <- writePlantingPlan_AgMAR(wet_years=as.integer(c("1995", "2010", "1992", "1993", "1987", "1998", "2006", "1991", "1986", "1994")), rye_date_planting = '4    1   1992', rye_date_harvest = '27 1 1992')
fert_plan <- writeFertPlan(end_year = 2020, tomato_N_kg_ha_yr = 200, tomato_splits = 6)
tillage_plan <- writeTillagePlan(depth = '15.0')
Irr_plan <- writeIrrPlan_AgMAR(tomADtrigger1='0.95', tomADtrigger2 = '0.9', flood_month='1', flood_days=c('5', '12', '19', '26'), wet_yrs=as.integer(c("1995", "2010", "1992", "1993", "1987", "1998", "2006", "1991", "1986", "1994")))
writeRZWQM('Parlier', 'AgMAR_Jan7d', 'FineSHR')

#Jan AgMar, 3-day interval
#coarse SHR
ipnames_fix(station = 'Parlier', scenario = 'AgMAR_Jan3d', soil = 'CoarseSHR', met_fname = 'Parlier_1983_2021.MET', brk_fname = 'Parlier_1983_2021.BRK', ana_fname = 'CoarseSHR_AgMARJan3d.ana')
copyRZINIT('Parlier', 'AgMAR_Jan3d', 'CoarseSHR')
input <- templateRZWQM('Parlier', 'AgMAR_Jan3d', 'templates', 'RZWQM_init.DAT')
soil_input <- soilInfo('Parlier', 'AgMAR_Jan3d', 'CoarseSHR', 'RZWQM.DAT')
planting_data <- writePlantingPlan_AgMAR(wet_years=as.integer(c("1995", "2010", "1992", "1993", "1987", "1998", "2006", "1991", "1986", "1994")), rye_date_planting = '4    1   1992', rye_date_harvest = '27 1 1992')
fert_plan <- writeFertPlan(end_year = 2020, tomato_N_kg_ha_yr = 200, tomato_splits = 6)
tillage_plan <- writeTillagePlan(depth = '15.0')
Irr_plan <- writeIrrPlan_AgMAR(flood_month='1', flood_days=c('11', '14', '17', '20'), wet_yrs=as.integer(c("1995", "2010", "1992", "1993", "1987", "1998", "2006", "1991", "1986", "1994")))
writeRZWQM('Parlier', 'AgMAR_Jan3d', 'CoarseSHR')

#loamy SHR
ipnames_fix(station = 'Parlier', scenario = 'AgMAR_Jan3d', soil = 'LoamySHR', met_fname = 'Parlier_1983_2021.MET', brk_fname = 'Parlier_1983_2021.BRK', ana_fname = 'LoamySHR_AgMARJan3d.ana')
copyRZINIT('Parlier', 'AgMAR_Jan3d', 'LoamySHR')
input <- templateRZWQM('Parlier', 'AgMAR_Jan3d', 'templates', 'RZWQM_init.DAT')
soil_input <- soilInfo('Parlier', 'AgMAR_Jan3d', 'LoamySHR', 'RZWQM.DAT')
planting_data <- writePlantingPlan_AgMAR(wet_years=as.integer(c("1995", "2010", "1992", "1993", "1987", "1998", "2006", "1991", "1986", "1994")), rye_date_planting = '4    1   1992', rye_date_harvest = '27 1 1992')
fert_plan <- writeFertPlan(end_year = 2020, tomato_N_kg_ha_yr = 200, tomato_splits = 6)
tillage_plan <- writeTillagePlan(depth = '15.0')
Irr_plan <- writeIrrPlan_AgMAR(flood_month='1', flood_days=c('11', '14', '17', '20'), wet_yrs=as.integer(c("1995", "2010", "1992", "1993", "1987", "1998", "2006", "1991", "1986", "1994")))
writeRZWQM('Parlier', 'AgMAR_Jan3d', 'LoamySHR')

#fine SHR
ipnames_fix(station = 'Parlier', scenario = 'AgMAR_Jan3d', soil = 'FineSHR', met_fname = 'Parlier_1983_2021.MET', brk_fname = 'Parlier_1983_2021.BRK', ana_fname = 'FineSHR_AgMARJan3d.ana')
copyRZINIT('Parlier', 'AgMAR_Jan3d', 'FineSHR')
input <- templateRZWQM('Parlier', 'AgMAR_Jan3d', 'templates', 'RZWQM_init.DAT')
soil_input <- soilInfo('Parlier', 'AgMAR_Jan3d', 'FineSHR', 'RZWQM.DAT')
planting_data <- writePlantingPlan_AgMAR(wet_years=as.integer(c("1995", "2010", "1992", "1993", "1987", "1998", "2006", "1991", "1986", "1994")), rye_date_planting = '4    1   1992', rye_date_harvest = '27 1 1992')
fert_plan <- writeFertPlan(end_year = 2020, tomato_N_kg_ha_yr = 200, tomato_splits = 6)
tillage_plan <- writeTillagePlan(depth = '15.0')
Irr_plan <- writeIrrPlan_AgMAR(tomADtrigger1='0.95', tomADtrigger2 = '0.9', flood_month='1', flood_days=c('11', '14', '17', '20'), wet_yrs=as.integer(c("1995", "2010", "1992", "1993", "1987", "1998", "2006", "1991", "1986", "1994")))
writeRZWQM('Parlier', 'AgMAR_Jan3d', 'FineSHR')

#Mar AgMar, 7-day interval
#coarse SHR
ipnames_fix(station = 'Parlier', scenario = 'AgMAR_Mar7d', soil = 'CoarseSHR', met_fname = 'Parlier_1983_2021.MET', brk_fname = 'Parlier_1983_2021.BRK', ana_fname = 'CoarseSHR_AgMARMar7d.ana')
copyRZINIT('Parlier', 'AgMAR_Mar7d', 'CoarseSHR')
input <- templateRZWQM('Parlier', 'AgMAR_Mar7d', 'templates', 'RZWQM_init.DAT')
soil_input <- soilInfo('Parlier', 'AgMAR_Mar7d', 'CoarseSHR', 'RZWQM.DAT')
planting_data <- writePlantingPlan_AgMAR(wet_years=as.integer(c("1995", "2010", "1992", "1993", "1987", "1998", "2006", "1991", "1986", "1994")), rye_date_planting = '4    3   1992', rye_date_harvest = '27 3 1992')
fert_plan <- writeFertPlan(end_year = 2020, tomato_N_kg_ha_yr = 200, tomato_splits = 6)
tillage_plan <- writeTillagePlan(depth = '15.0')
Irr_plan <- writeIrrPlan_AgMAR(flood_month='3', flood_days=c('5', '12', '19', '26'), wet_yrs=as.integer(c("1995", "2010", "1992", "1993", "1987", "1998", "2006", "1991", "1986", "1994")))
writeRZWQM('Parlier', 'AgMAR_Mar7d', 'CoarseSHR')

#loamy SHR
ipnames_fix(station = 'Parlier', scenario = 'AgMAR_Mar7d', soil = 'LoamySHR', met_fname = 'Parlier_1983_2021.MET', brk_fname = 'Parlier_1983_2021.BRK', ana_fname = 'LoamySHR_AgMARMar7d.ana')
copyRZINIT('Parlier', 'AgMAR_Mar7d', 'LoamySHR')
input <- templateRZWQM('Parlier', 'AgMAR_Mar7d', 'templates', 'RZWQM_init.DAT')
soil_input <- soilInfo('Parlier', 'AgMAR_Mar7d', 'LoamySHR', 'RZWQM.DAT')
planting_data <- writePlantingPlan_AgMAR(wet_years=as.integer(c("1995", "2010", "1992", "1993", "1987", "1998", "2006", "1991", "1986", "1994")), rye_date_planting = '4    3   1992', rye_date_harvest = '27 3 1992')
fert_plan <- writeFertPlan(end_year = 2020, tomato_N_kg_ha_yr = 200, tomato_splits = 6)
tillage_plan <- writeTillagePlan(depth = '15.0')
Irr_plan <- writeIrrPlan_AgMAR(flood_month='3', flood_days=c('5', '12', '19', '26'), wet_yrs=as.integer(c("1995", "2010", "1992", "1993", "1987", "1998", "2006", "1991", "1986", "1994")))
writeRZWQM('Parlier', 'AgMAR_Mar7d', 'LoamySHR')

#fine SHR
ipnames_fix(station = 'Parlier', scenario = 'AgMAR_Mar7d', soil = 'FineSHR', met_fname = 'Parlier_1983_2021.MET', brk_fname = 'Parlier_1983_2021.BRK', ana_fname = 'FineSHR_AgMARMar7d.ana')
copyRZINIT('Parlier', 'AgMAR_Mar7d', 'FineSHR')
input <- templateRZWQM('Parlier', 'AgMAR_Mar7d', 'templates', 'RZWQM_init.DAT')
soil_input <- soilInfo('Parlier', 'AgMAR_Mar7d', 'FineSHR', 'RZWQM.DAT')
planting_data <- writePlantingPlan_AgMAR(wet_years=as.integer(c("1995", "2010", "1992", "1993", "1987", "1998", "2006", "1991", "1986", "1994")), rye_date_planting = '4    3   1992', rye_date_harvest = '27 3 1992')
fert_plan <- writeFertPlan(end_year = 2020, tomato_N_kg_ha_yr = 200, tomato_splits = 6)
tillage_plan <- writeTillagePlan(depth = '15.0')
Irr_plan <- writeIrrPlan_AgMAR(tomADtrigger1='0.95', tomADtrigger2 = '0.9', flood_month='3', flood_days=c('5', '12', '19', '26'), wet_yrs=as.integer(c("1995", "2010", "1992", "1993", "1987", "1998", "2006", "1991", "1986", "1994")))
writeRZWQM('Parlier', 'AgMAR_Mar7d', 'FineSHR')

#Mar AgMar, 3-day interval
#coarse SHR
ipnames_fix(station = 'Parlier', scenario = 'AgMAR_Mar3d', soil = 'CoarseSHR', met_fname = 'Parlier_1983_2021.MET', brk_fname = 'Parlier_1983_2021.BRK', ana_fname = 'CoarseSHR_AgMARMar3d.ana')
copyRZINIT('Parlier', 'AgMAR_Mar3d', 'CoarseSHR')
input <- templateRZWQM('Parlier', 'AgMAR_Mar3d', 'templates', 'RZWQM_init.DAT')
soil_input <- soilInfo('Parlier', 'AgMAR_Mar3d', 'CoarseSHR', 'RZWQM.DAT')
planting_data <- writePlantingPlan_AgMAR(wet_years=as.integer(c("1995", "2010", "1992", "1993", "1987", "1998", "2006", "1991", "1986", "1994")), rye_date_planting = '4    3   1992', rye_date_harvest = '27 3 1992')
fert_plan <- writeFertPlan(end_year = 2020, tomato_N_kg_ha_yr = 200, tomato_splits = 6)
tillage_plan <- writeTillagePlan(depth = '15.0')
Irr_plan <- writeIrrPlan_AgMAR(flood_month='3', flood_days=c('11', '14', '17', '20'), wet_yrs=as.integer(c("1995", "2010", "1992", "1993", "1987", "1998", "2006", "1991", "1986", "1994")))
writeRZWQM('Parlier', 'AgMAR_Mar3d', 'CoarseSHR')

#loamy SHR
ipnames_fix(station = 'Parlier', scenario = 'AgMAR_Mar3d', soil = 'LoamySHR', met_fname = 'Parlier_1983_2021.MET', brk_fname = 'Parlier_1983_2021.BRK', ana_fname = 'LoamySHR_AgMARMar3d.ana')
copyRZINIT('Parlier', 'AgMAR_Mar3d', 'LoamySHR')
input <- templateRZWQM('Parlier', 'AgMAR_Mar3d', 'templates', 'RZWQM_init.DAT')
soil_input <- soilInfo('Parlier', 'AgMAR_Mar3d', 'LoamySHR', 'RZWQM.DAT')
planting_data <- writePlantingPlan_AgMAR(wet_years=as.integer(c("1995", "2010", "1992", "1993", "1987", "1998", "2006", "1991", "1986", "1994")), rye_date_planting = '4    3   1992', rye_date_harvest = '27 3 1992')
fert_plan <- writeFertPlan(end_year = 2020, tomato_N_kg_ha_yr = 200, tomato_splits = 6)
tillage_plan <- writeTillagePlan(depth = '15.0')
Irr_plan <- writeIrrPlan_AgMAR(flood_month='3', flood_days=c('11', '14', '17', '20'), wet_yrs=as.integer(c("1995", "2010", "1992", "1993", "1987", "1998", "2006", "1991", "1986", "1994")))
writeRZWQM('Parlier', 'AgMAR_Mar3d', 'LoamySHR')

#fine SHR
ipnames_fix(station = 'Parlier', scenario = 'AgMAR_Mar3d', soil = 'FineSHR', met_fname = 'Parlier_1983_2021.MET', brk_fname = 'Parlier_1983_2021.BRK', ana_fname = 'FineSHR_AgMARMar3d.ana')
copyRZINIT('Parlier', 'AgMAR_Mar3d', 'FineSHR')
input <- templateRZWQM('Parlier', 'AgMAR_Mar3d', 'templates', 'RZWQM_init.DAT')
soil_input <- soilInfo('Parlier', 'AgMAR_Mar3d', 'FineSHR', 'RZWQM.DAT')
planting_data <- writePlantingPlan_AgMAR(wet_years=as.integer(c("1995", "2010", "1992", "1993", "1987", "1998", "2006", "1991", "1986", "1994")), rye_date_planting = '4    3   1992', rye_date_harvest = '27 3 1992')
fert_plan <- writeFertPlan(end_year = 2020, tomato_N_kg_ha_yr = 200, tomato_splits = 6)
tillage_plan <- writeTillagePlan(depth = '15.0')
Irr_plan <- writeIrrPlan_AgMAR(tomADtrigger1='0.95', tomADtrigger2 = '0.9', flood_month='3', flood_days=c('11', '14', '17', '20'), wet_yrs=as.integer(c("1995", "2010", "1992", "1993", "1987", "1998", "2006", "1991", "1986", "1994")))
writeRZWQM('Parlier', 'AgMAR_Mar3d', 'FineSHR')

#AgMAR 21-day frequency
#coarse SHR
ipnames_fix(station = 'Parlier', scenario = 'AgMAR_21d', soil = 'CoarseSHR', met_fname = 'Parlier_1983_2021.MET', brk_fname = 'Parlier_1983_2021.BRK', ana_fname = 'CoarseSHR_AgMAR21d.ana')
copyRZINIT('Parlier', 'AgMAR_21d', 'CoarseSHR')
input <- templateRZWQM('Parlier', 'AgMAR_21d', 'templates', 'RZWQM_init.DAT')
soil_input <- soilInfo('Parlier', 'AgMAR_21d', 'CoarseSHR', 'RZWQM.DAT')
planting_data <- writePlantingPlan_AgMAR_LF(wet_years=as.integer(c("1995", "2010", "1992", "1993", "1987", "1998", "2006", "1991", "1986", "1994")), rye_date_planting = c('7    1', '28   1', '18   2', '11   3'), rye_date_harvest = c('9 1', '30 1', '20 2', '13 3'))
fert_plan <- writeFertPlan(end_year = 2020, tomato_N_kg_ha_yr = 200, tomato_splits = 6)
tillage_plan <- writeTillagePlan(depth = '15.0')
Irr_plan <- writeIrrPlan_AgMAR_LF(flood_month=c('1', '1', '2', '3'), flood_days=c('8', '29', '19', '12'), wet_yrs=as.integer(c("1995", "2010", "1992", "1993", "1987", "1998", "2006", "1991", "1986", "1994")))
writeRZWQM('Parlier', 'AgMAR_21d', 'CoarseSHR')

#loamy SHR
ipnames_fix(station = 'Parlier', scenario = 'AgMAR_21d', soil = 'LoamySHR', met_fname = 'Parlier_1983_2021.MET', brk_fname = 'Parlier_1983_2021.BRK', ana_fname = 'LoamySHR_AgMAR21d.ana')
copyRZINIT('Parlier', 'AgMAR_21d', 'LoamySHR')
input <- templateRZWQM('Parlier', 'AgMAR_21d', 'templates', 'RZWQM_init.DAT')
soil_input <- soilInfo('Parlier', 'AgMAR_21d', 'LoamySHR', 'RZWQM.DAT')
planting_data <- writePlantingPlan_AgMAR_LF(wet_years=as.integer(c("1995", "2010", "1992", "1993", "1987", "1998", "2006", "1991", "1986", "1994")), rye_date_planting = c('7    1', '28   1', '18   2', '11   3'), rye_date_harvest = c('9 1', '30 1', '20 2', '13 3'))
fert_plan <- writeFertPlan(end_year = 2020, tomato_N_kg_ha_yr = 200, tomato_splits = 6)
tillage_plan <- writeTillagePlan(depth = '15.0')
Irr_plan <- writeIrrPlan_AgMAR_LF(flood_month=c('1', '1', '2', '3'), flood_days=c('8', '29', '19', '12'), wet_yrs=as.integer(c("1995", "2010", "1992", "1993", "1987", "1998", "2006", "1991", "1986", "1994")))
writeRZWQM('Parlier', 'AgMAR_21d', 'LoamySHR')

#fine SHR
ipnames_fix(station = 'Parlier', scenario = 'AgMAR_21d', soil = 'FineSHR', met_fname = 'Parlier_1983_2021.MET', brk_fname = 'Parlier_1983_2021.BRK', ana_fname = 'FineSHR_AgMAR21d.ana')
copyRZINIT('Parlier', 'AgMAR_21d', 'FineSHR')
input <- templateRZWQM('Parlier', 'AgMAR_21d', 'templates', 'RZWQM_init.DAT')
soil_input <- soilInfo('Parlier', 'AgMAR_21d', 'FineSHR', 'RZWQM.DAT')
planting_data <- writePlantingPlan_AgMAR_LF(wet_years=as.integer(c("1995", "2010", "1992", "1993", "1987", "1998", "2006", "1991", "1986", "1994")), rye_date_planting = c('7   1', '28   1', '18   2', '11   3'), rye_date_harvest = c('9 1', '30 1', '20 2', '13 3'))
fert_plan <- writeFertPlan(end_year = 2020, tomato_N_kg_ha_yr = 200, tomato_splits = 6)
tillage_plan <- writeTillagePlan(depth = '15.0')
Irr_plan <- writeIrrPlan_AgMAR_LF(tomADtrigger1='0.95', tomADtrigger2 = '0.9', flood_month=c('1', '1', '2', '3'), flood_days=c('8', '29', '19', '12'), wet_yrs=as.integer(c("1995", "2010", "1992", "1993", "1987", "1998", "2006", "1991", "1986", "1994")))
# Irr_plan <- writeIrrPlan(tomADtrigger1='0.95', tomADtrigger2 = '0.9')
writeRZWQM('Parlier', 'AgMAR_21d', 'FineSHR')


#taken from prepare_run_maize_only.R
#run SSURGO soils through base runs with maize only scenario
# compnames <- c('Hanford', 'Delhi', 'Wasco', 'Hesperia', 'Milham', 'Tujunga', 'Panoche', 'Cerini', 'Yolo', 'Colpien', 'Tehama', 'Capay', 'Clear Lake', 'Willows', 'Tulare', 'CoarseSHR', 'LoamySHR', 'FineSHR') #'Channac' not included
# stress_soils <- c('Capay', 'Willows', 'Wekoda')#, 'Clear Lake', 'Tulare')
# stn <- 'Parlier'

# for(i in 1:length(compnames)) {
#   print(i)
#   ipnames_fix(station = stn, scenario = 'BaseRuns', soil = compnames[i], met_fname = 'Parlier_1983_2021.MET', brk_fname = 'Parlier_1983_2021.BRK', ana_fname = paste0(compnames[i], '_baserun.ana'))
#   copyRZINIT(stn, 'BaseRuns', compnames[i])
#   input <- templateRZWQM(stn, 'BaseRuns', compnames[i], 'RZWQM_init.DAT')
#   soil_input <- soilInfo(stn, 'BaseRuns', compnames[i], 'RZWQM.DAT')
#   planting_data <- writePlantingPlan()
#   fert_plan <- writeFertPlan(end_year = 2020) #tomato_N_kg_ha_yr = 200, tomato_splits = 6
#   tillage_plan <- writeTillagePlan(depth = '15.0')
#   Irr_plan <- if(compnames[i] %in% stress_soils) {writeIrrPlan(cornADtrigger1 = '0.8', cornADtrigger2 = '0.7')} else{writeIrrPlan()}
#   writeRZWQM(station = stn, scenario = 'BaseRuns', soil = compnames[i], input=input, soil_input = soil_input, planting_data = planting_data, fert_plan = fert_plan, tillage_plan = tillage_plan, Irr_plan = Irr_plan)
# }

#prepare steady state templates
for(i in 1:length(compnames)) {
  ipnames_fix(station = stn, scenario = 'SteadyStateRuns', soil = compnames[i], met_fname = 'Parlier_1983_2021.MET', brk_fname = 'Parlier_1983_2021.BRK', ana_fname = paste0(compnames[i], '_SteadyStateRun.ana'))
  copyRZINIT(stn, 'SteadyStateRuns', compnames[i])
  gen_input <- templateRZWQM(stn, 'SteadyStateRuns', compnames[i], 'RZWQM_init.DAT')
  suelo <- soilInfo(stn, 'SteadyStateRuns', compnames[i], 'RZWQM.DAT')
  planting <- writePlantingPlan()
  fert <- writeFertPlan(end_year = 2020)
  tillage <- writeTillagePlan(depth = '15.0')
  Irr <- if(compnames[i] %in% stress_soils) {writeIrrPlan(cornADtrigger1 = '0.8', cornADtrigger2 = '0.7')} else{writeIrrPlan()}
  writeRZWQM(station = stn, scenario = 'SteadyStateRuns', soil = compnames[i], input=gen_input, soil_input = suelo, planting_data = planting, fert_plan = fert, tillage_plan = tillage, Irr_plan = Irr)
  steady_state_setup(station = stn, scenario = 'SteadyStateRuns', soil = compnames[i], type = 'run 1', suelo)
  rm(gen_input, planting, fert, tillage, Irr, suelo)
}

#run then change set-up and run again
for(i in 1:length(compnames)) {
  suelo <- soilInfo(stn, 'SteadyStateRuns', compnames[i], 'RZWQM.DAT')
  steady_state_setup(station = stn, scenario = 'SteadyStateRuns', soil = compnames[i], type = 'run 2', soil_input = suelo)
  rm(suelo)
}

#AgMAR scenarios
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

#test
'Milham_test'

stn <- 'Parlier'
scn <- 'SteadyStateRuns'
ipnames_fix(station = stn, scenario = 'SteadyStateRuns', soil = 'Milham_test', met_fname = 'Parlier_1983_2021.MET', brk_fname = 'Parlier_1983_2021.BRK', ana_fname = paste0('Milham_test', '_SteadyState.ana'))
steady_state_setup(station = stn, scenario = 'SteadyStateRuns', soil = 'Milham_test', type = 'run 1')
gen_input <- templateRZWQM(stn, scn, 'Milham_test', 'RZWQM_init.DAT')
suelo_input <- soilInfo(stn, scn, 'Milham_test', 'RZWQM.DAT')
planting <- writePlantingPlan()
fert <- writeFertPlan(end_year = 2020)
tillage <- writeTillagePlan(depth = '15.0')
Irr <- if(compnames[i] %in% stress_soils) {writeIrrPlan(cornADtrigger1 = '0.8', cornADtrigger2 = '0.7')} else{writeIrrPlan()}
writeRZWQM(station = stn, scenario = scn, soil = 'Milham_test', input=gen_input, soil_input = suelo_input, planting_data = planting, fert_plan = fert, tillage_plan = tillage, Irr_plan = Irr)

#temp fix of breakthrough node number
scenarios <- c('AgMAR_Jan7d', 'AgMAR_Jan3d', 'AgMAR_Mar7d', 'AgMAR_Mar3d', 'AgMAR_21d')
suelo_name <- 'Colpien'
for(i in 1:length(scenarios)) {
  suelo <- soilInfo(stn, 'SteadyStateRuns', suelo_name, 'RZWQM.DAT')
  steady_state_setup(station = stn, scenario = scenarios[i], soil = suelo_name, type = 'run 2', soil_input = suelo)
}

#set-up steady state script by soil name
#update node numbers in cntrl.dat file: note, this is taken care of by steady_state_setup function
soil <- 'Tehama'
ipnames_fix(station = stn, scenario = 'SteadyStateRuns', soil = soil, met_fname = 'Parlier_1983_2021.MET', brk_fname = 'Parlier_1983_2021.BRK', ana_fname = paste0(soil, '_SteadyStateRun.ana'))
copyRZINIT(stn, 'SteadyStateRuns', soil)
gen_input <- templateRZWQM(stn, 'SteadyStateRuns', soil, 'RZWQM_init.DAT')
suelo <- soilInfo(stn, 'SteadyStateRuns', soil, 'RZWQM.DAT')
planting <- writePlantingPlan()
fert <- writeFertPlan(end_year = 2020)
tillage <- writeTillagePlan(depth = '15.0')
Irr <- if(soil %in% stress_soils) {writeIrrPlan(cornADtrigger1 = '0.8', cornADtrigger2 = '0.7')} else{writeIrrPlan()}
writeRZWQM(station = stn, scenario = 'SteadyStateRuns', soil = soil, input=gen_input, soil_input = suelo, planting_data = planting, fert_plan = fert, tillage_plan = tillage, Irr_plan = Irr)
steady_state_setup(station = stn, scenario = 'SteadyStateRuns', soil = soil, type = 'run 1', suelo)
rm(gen_input, planting, fert, tillage, Irr)
steady_state_setup(station = stn, scenario = 'SteadyStateRuns', soil = soil, type = 'run 2', suelo)
rm(suelo)

#then copy updated soil name's steady state folder to AgMAR scenario directories and run the following to correct the management input files
#Jan 7d interval
AgMAR_runs(stn = 'Parlier', scn = 'AgMAR_Jan7d', suelos = soil, AgMAR_month = '1', AgMAR_days = c('5', '12', '19', '26'), fake_planting = '4    1   1992', fake_harvest = '27 1 1992')
#Jan 3d interval
AgMAR_runs(stn = 'Parlier', scn = 'AgMAR_Jan3d', suelos = soil, AgMAR_month = '1', AgMAR_days = c('11', '14', '17', '20'), fake_planting = '4    1   1992', fake_harvest = '27 1 1992')
#Mar 3d interval
AgMAR_runs(stn = 'Parlier', scn = 'AgMAR_Mar3d', suelos = soil, AgMAR_month = '3', AgMAR_days = c('11', '14', '17', '20'), fake_planting = '4    3   1992', fake_harvest = '27 3 1992')
#Mar 7d interval
AgMAR_runs(stn = 'Parlier', scn = 'AgMAR_Mar7d', suelos = soil, AgMAR_month = '3', AgMAR_days = c('5', '12', '19', '26'), fake_planting = '4    3   1992', fake_harvest = '27 3 1992')
#21 d interval
AgMAR_runs_LF(stn = 'Parlier', scn = 'AgMAR_21d', suelos = soil, AgMAR_month = c('1', '1', '2', '3'), AgMAR_days = c('8', '29', '19', '12'), fake_planting = c('7    1', '28   1', '18   2', '11   3'), fake_harvest = c('9 1', '30 1', '20 2', '13 3'))

#taken from running fine_soil_functions.R
# fine_soils <- c('Lofgren', 'Wekoda')
#run functions for list of fine soils with irrdays > 1 and < 10
#Jan 3-day interval
AgMAR_runs_fine(stn = 'Parlier', scn = 'AgMAR_Jan3d', suelos = fine_soils, AgMAR_month = AgMAR_3d_irrmonths_early, AgMAR_days = AgMAR_3d_irrdays_early,  fake_planting = '1    1   1992', fake_harvest = '25 2 1992', comp_df = comp_ksat)
writeOverallResults(compnames = fine_soils, scenario = 'AgMAR_Jan3d', weather_stn = 'Parlier')

#Jan 7-day interval
AgMAR_runs_fine(stn = 'Parlier', scn = 'AgMAR_Jan7d', suelos = fine_soils, AgMAR_month = AgMAR_7d_irrmonths_early, AgMAR_days = AgMAR_7d_irrdays_early,  fake_planting = '1    1   1992', fake_harvest = '25 2 1992', comp_df = comp_ksat)
writeOverallResults(compnames = fine_soils, scenario = 'AgMAR_Jan7d', weather_stn = 'Parlier')

#Mar 3-day interval
AgMAR_runs_fine(stn = 'Parlier', scn = 'AgMAR_Mar3d', suelos = fine_soils, AgMAR_month = AgMAR_3d_irrmonths_late, AgMAR_days = AgMAR_3d_irrdays_late,  fake_planting = '9    2   1992', fake_harvest = '13 4 1992', comp_df = comp_ksat)
writeOverallResults(compnames = fine_soils, scenario = 'AgMAR_Mar3d', weather_stn = 'Parlier')

#Mar 7-day interval
AgMAR_runs_fine(stn = 'Parlier', scn = 'AgMAR_Mar7d', suelos = fine_soils, AgMAR_month = AgMAR_7d_irrmonths_late, AgMAR_days = AgMAR_7d_irrdays_late,  fake_planting = '9    2   1992', fake_harvest = '13 4 1992', comp_df = comp_ksat)
writeOverallResults(compnames = fine_soils, scenario = 'AgMAR_Mar7d', weather_stn = 'Parlier')

#21-day interval
AgMAR_runs_fine(stn = 'Parlier', scn = 'AgMAR_21d', suelos = fine_soils, AgMAR_month = AgMAR_21d_irrmonths, AgMAR_days = AgMAR_21d_irrdays,  fake_planting = '1    1   1992', fake_harvest = '13 4 1992', comp_df = comp_ksat)
writeOverallResults(compnames = fine_soils, scenario = 'AgMAR_21d', weather_stn = 'Parlier')
