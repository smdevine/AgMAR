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