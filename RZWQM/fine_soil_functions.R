#irrigation plan with AgMAR on fine soils requiring more than 1 day to recharge 15 cm
#read in depth-weighted profile ksat
ssurgoDir2 <- 'C:/Users/smdevine/Desktop/post doc/Dahlke/RZWQM/ssurgo_profile_data/RZWQM input'
list.files(ssurgoDir2)
comp_ksat <- read.csv(file.path(ssurgoDir2, 'comp_data_RZWQMruns_Jun22.csv'), stringsAsFactors = FALSE) #updated with three additional soils on 6/21/22
comp_ksat$AgMARplan <- paste('days', comp_ksat$irrdays, sep = '')
# stress_soils <- c('Capay', 'Willows', 'Wekoda')
fine_soils <- comp_ksat$compnames[which(comp_ksat$irrdays > 1 & comp_ksat$irrdays < 10)]
fine_soils

#3-day interval plans
AgMAR_3d_irrdays_early <- list(days1=as.character(c(11,14,17,20)), days2=as.character(c(8:9,13:14,18:19,23:24)), days3=as.character(c(6:8,12:14,18:20,24:26)), days4=as.character(c(4:7,11:14,18:21,25:28)), days5=as.character(c(2:6,10:14,18:22,26:30)), days6=as.character(c(2:7,11:16,20:25,29:31,1:3)), days7=as.character(c(2:8,12:18,22:28,1:7)), days9=as.character(c(2:10,14:22,26:31,1:3,7:15)))
AgMAR_3d_irrdays_late <- list(days1=as.character(c(11,14,17,20)), days2=as.character(c(8:9,13:14,18:19,23:24)), days3=as.character(c(6:8,12:14,18:20,24:26)), days4=as.character(c(4:7,11:14,18:21,25:28)), days5=as.character(c(2:6,10:14,18:22,26:30)), days6=as.character(c(2:7,11:16,20:25,29:31,1:3)), days7=as.character(c(2:8,12:18,22:28,1:7)), days9=as.character(c(27:28,1:7,11:19,23:31,4:12))) #only days9 differs from above
AgMAR_3d_irrmonths_early <- list(days1='1', days2='1', days3='1', days4='1', days5='1', days6=c(rep('1', 21), rep('2', 3)), days7=c(rep('1', 21), rep('2', 7)), days9=c(rep('1', 24), rep('2', 12)))
AgMAR_3d_irrmonths_late <- list(days1='3', days2='3', days3='3', days4='3', days5='3', days6=c(rep('3', 21), rep('4', 3)), days7=c(rep('3', 21), rep('4', 7)), days9=c(rep('2', 2), rep('3', 25), rep('4', 9)))
lapply(AgMAR_3d_irrdays_early, length)
lapply(AgMAR_3d_irrmonths_early, length)
lapply(AgMAR_3d_irrdays_late, length)
lapply(AgMAR_3d_irrmonths_late, length)
#6-day interval plans (can't go earlier than Jan 1 each year)
AgMAR_7d_irrdays_early <- list(days1=as.character(c(5,12,19,26)), days2=as.character(c(4:5,12:13,20:21,28:29)), days3=as.character(c(3:5,12:14,21:23,30:31,1)), days4=as.character(c(2:5,12:15,22:25,1:4)), days5=as.character(c(2:6,13:17,24:28,4:8)), days6=as.character(c(2:7,14:19,26:31,7:12)), days7=as.character(c(2:8,15:21,28:31,1:3,10:16)), days9=as.character(c(2:10,17:25,1:9,16:24)))
AgMAR_7d_irrdays_late <- list(days1=as.character(c(5,12,19,26)), days2=as.character(c(4:5,12:13,20:21,28:29)), days3=as.character(c(3:5,12:14,21:23,30:31,1)), days4=as.character(c(2:5,12:15,22:25,1:4)), days5=as.character(c(1:5,12:16,23:27,3:7)), days6=as.character(c(1:6,13:18,25:30,7:12)), days7=as.character(c(26:28,1:4,11:17,24:30,6:12)), days9=as.character(c(18:26,5:13,20:28,4:12)))
lapply(AgMAR_7d_irrdays_early, length)
lapply(AgMAR_7d_irrdays_late, length)
AgMAR_7d_irrmonths_early <- list(days1='1', days2='1', days3=c(rep('1', 11), '2'), days4=c(rep('1', 12), rep('2', 4)), days5=c(rep('1', 15), rep('2', 5)), days6=c(rep('1', 18), rep('2', 6)), days7=c(rep('1', 18), rep('2', 10)), days9=c(rep('1', 18), rep('2', 18)))
AgMAR_7d_irrmonths_late <- list(days1='3', days2='3', days3=c(rep('3', 11), '4'), days4=c(rep('3', 12), rep('4', 4)), days5=c(rep('3', 15), rep('4', 5)), days6=c(rep('3', 18), rep('4', 6)), days7=c(rep('2', 3), rep('3', 18), rep('4', 7)), days9=c(rep('2', 9), rep('3', 18), rep('4', 9)))
lapply(AgMAR_7d_irrmonths_early, length)
lapply(AgMAR_7d_irrmonths_late, length)

##21-day interval plans
AgMAR_21d_irrdays <- list(days1=as.character(c(8,29,19,12)), days2=as.character(c(7:8,29:30,20:21,14:15)), days3=as.character(c(6:8,29:31,21:23,16:18)), days4=as.character(c(5:8,29:31,1,22:25,18:21)), days5=as.character(c(4:8,29:31,1:2,23:27,20:24)), days6=as.character(c(3:8,29:31,1:3,24:28,1,22:27)), days7=as.character(c(2:8,29:31,1:4,25:28,1:3,24:30)), days9=as.character(c(2:10,31,1:8,1:9,30:31,1:7)))
AgMAR_21d_irrmonths <- list(days1=c('1', '1', '2', '3'), days2=c(rep('1', 4), rep('2', 2), rep('3', 2)), days3=c(rep('1', 6), rep('2', 3), rep('3', 3)), days4=c(rep('1', 7), rep('2', 5), rep('3', 4)), days5=c(rep('1', 8), rep('2', 7), rep('3', 5)), days6=c(rep('1', 9), rep('2', 8), rep('3', 7)), days7=c(rep('1', 10), rep('2', 8), rep('3', 10)), days9=c(rep('1', 10), rep('2', 8), rep('3', 11), rep('4', 7)))
lapply(AgMAR_21d_irrdays, length)
lapply(AgMAR_21d_irrmonths, length)
# comp_ksat$AgMAR_3d <- ifelse(comp_ksat$irrdays==1, 'days1', ifelse(comp_ksat$irrdays==2, 'days2', ifelse(comp_ksat$irrdays==3, 'days3', ifelse(comp_ksat$irrdays==4, 'days4', ifelse(comp_ksat==5, 'days5', ifelse(comp_ksat==6, 'days6', ifelse(comp_ksat==7, 'days7', NA))))))) 


#IMPORTANT needs to be modified still to be flexible
# writeIrrPlan_AgMAR_LF_fine <- function(start_year=1984, end_year=2020, irrigationAssumptions='0  0  0.0  100.0  15.0  0.0  0', cornADtrigger1='0.6', cornDaystrigger1='3', cornDaystrigger2='60', cornADtrigger2='0.5', IrrAppPlanting=1.25, flood_month, flood_days, flood_app=15.0, wet_yrs) {
#   years <- start_year:end_year
#   plantings <- length(years)
#   IrrPlan <- do.call(c, lapply(years, function(x) {
#     corn_line1 <- paste('1  2  3  3 20 4', x, '1 9', x, '0 0.0  0.0', sep=' ') #date is 4/14; that #2 near the beginning should be a 3 for 'furrow'
#     corn_line2 <- '2' #this is irrigation rule #2
#     corn_line3 <- paste(' ', cornDaystrigger1, cornADtrigger1, '  ', cornDaystrigger2, cornADtrigger2, sep = ' ') 
#     corn_line4 <- '2' #this is irrigation rule #2
#     corn_line5 <- paste('', cornDaystrigger1, ' 1.0', ' ', cornDaystrigger2, ' 1.0', sep = ' ')
#     c(corn_line1, corn_line2, corn_line3, corn_line4, corn_line5)
#   }))
#   IrrPlan2 <- do.call(c, lapply(years, function(x) {
#     corn_line1 <- paste('1  1  2  1 16 4', x, '16 4', x, '0 0.0  2.0', sep=' ') #date is 4/16
#     corn_line2 <- '1' #not sure what this refers to
#     corn_line3 <- paste('16  4', x, sep = '  ')
#     if(x%in%wet_yrs) { #left off here to fix this on 12/21/21
#       c(paste('3  2  2  2', flood_days[1], flood_month[1], x, flood_days[1], flood_month[1], x, '0 0.0  2.0', sep=' '), '1', paste(flood_days[1], flood_month[1], x, sep = '  '), '1', as.character(flood_app), paste('3  2  2  2', flood_days[2], flood_month[2], x, flood_days[2], flood_month[2], x, '0 0.0  2.0', sep=' '), '1', paste(flood_days[2], flood_month[2], x, sep = '  '), '1', as.character(flood_app), paste('3  2  2  2', flood_days[3], flood_month[3], x, flood_days[3], flood_month[3], x, '0 0.0  2.0', sep=' '), '1', paste(flood_days[3], flood_month[3], x, sep = '  '), '1', as.character(flood_app), paste('3  2  2  2', flood_days[4], flood_month[4], x, flood_days[4], flood_month[4], x, '0 0.0  2.0', sep=' '), '1', paste(flood_days[4], flood_month[4], x, sep = '  '), '1', as.character(flood_app), corn_line1, corn_line2, corn_line3, as.character(IrrAppPlanting)) # the '4' denotes 4 irrigation dates
#     } else {c(corn_line1, corn_line2, corn_line3, as.character(IrrAppPlanting))} 
#   }))
#   c(paste((length(years)*2+length(wet_yrs)*4), irrigationAssumptions, sep = '  '), IrrPlan, IrrPlan2)
# }

#more flexible function for fine soil Ag-MAR
writeIrrPlan_AgMAR_fine <- function(start_year=1984, end_year=2020, irrigationAssumptions='0  0  0.0  100.0  15.0  0.0  0', cornADtrigger1='0.6', cornDaystrigger1='3', cornDaystrigger2='60', cornADtrigger2='0.5', IrrAppPlanting=1.25, flood_month=NA, flood_days=NA, flood_app=15, wet_yrs, soil_Ksat) {
  flood_length <- length(flood_days) / 4
  flood_app1 <- soil_Ksat
  flood_app2 <- flood_app - soil_Ksat * (flood_length - 1)
  years <- start_year:end_year
  plantings <- length(years)
  IrrPlan <- do.call(c, lapply(years, function(x) {
    corn_line1 <- paste('1  2  3  3 20 4', x, '1 9', x, '0 0.0  0.0', sep=' ') #date is 4/14; that #2 near the beginning should be a 3 for 'furrow'
    corn_line2 <- '2' #this is irrigation rule #2
    corn_line3 <- paste(' ', cornDaystrigger1, cornADtrigger1, '  ', cornDaystrigger2, cornADtrigger2, sep = ' ') 
    corn_line4 <- '2' #this is irrigation rule #2
    corn_line5 <- paste('', cornDaystrigger1, ' 1.0', ' ', cornDaystrigger2, ' 1.0', sep = ' ')
    c(corn_line1, corn_line2, corn_line3, corn_line4, corn_line5)
  }))
  IrrPlan2 <- do.call(c, lapply(years, function(x) {
    corn_line1 <- paste('1  1  2  1 16 4', x, '16 4', x, '0 0.0  2.0', sep=' ') #date is 4/16
    corn_line2 <- '1' #not sure what this refers to
    corn_line3 <- paste('16  4', x, sep = '  ')
    if(x%in%wet_yrs) { #left off here to fix this on 12/21/21
      c(paste('3  2  2  2', flood_days[1], flood_month[1], x, flood_days[length(flood_days)], flood_month[length(flood_month)], x, '0 0.0  2.0', sep=' '), paste(as.character(length(flood_days))), paste(flood_days, flood_month, x, sep = '  '), paste(as.character(length(flood_days))), paste(rep(c(rep(as.character(flood_app1), (flood_length-1)), as.character(flood_app2)), 4), collapse = ' '), corn_line1, corn_line2, corn_line3, as.character(IrrAppPlanting)) # the '4' denotes 4 irrigation dates
    } else {c(corn_line1, corn_line2, corn_line3, as.character(IrrAppPlanting))} 
  }))
  c(paste((length(years)*2+length(wet_yrs)), irrigationAssumptions, sep = '  '), IrrPlan, IrrPlan2)
}

# suelos = fine_soils
# AgMAR_month = AgMAR_3d_irrmonths_early
# AgMAR_days = AgMAR_3d_irrdays
# comp_df = comp_ksat
AgMAR_runs_fine <- function(stn, scn, suelos, AgMAR_month, AgMAR_days, fake_planting, fake_harvest, comp_df, wetyears) {
  met <- paste0(stn, '_1983_2021.MET')
  brk <- paste0(stn, '_1983_2021.BRK')
  for(i in 1:length(suelos)) {
    print(i)
    AgMAR_m <- AgMAR_month[[which(names(AgMAR_month)==comp_df$AgMARplan[comp_df$compnames==suelos[i]])]]
    AgMAR_d <- AgMAR_days[[which(names(AgMAR_days)==comp_df$AgMARplan[comp_df$compnames==suelos[i]])]]
    soilksat <- comp_df$ksat_cm_day[comp_df$compnames==suelos[i]]
    ana <- paste0(suelos[i], '_', scn, '.ana')
    ipnames_fix(station = stn, scenario = scn, soil = suelos[i], met_fname = met, brk_fname = brk, ana_fname = ana)
    copyRZINIT(stn, scn, suelos[i])
    gen_input <- templateRZWQM(stn, scn, suelos[i], 'RZWQM_init.DAT')
    suelo_input <- soilInfo(stn, scn, suelos[i], 'RZWQM.DAT')
    planting <- writePlantingPlan_AgMAR(wet_years=as.integer(wetyears), rye_date_planting = fake_planting, rye_date_harvest = fake_harvest)
    fert <- writeFertPlan(end_year = 2020)
    tillage <- writeTillagePlan(depth = '15.0')
    Irr <- if(suelos[i] %in% stress_soils) {writeIrrPlan_AgMAR_fine(cornADtrigger1 = '0.8', cornADtrigger2 = '0.7', flood_month=AgMAR_m, flood_days=AgMAR_d, wet_yrs=as.integer(wetyears), soil_Ksat = soilksat)} else{writeIrrPlan_AgMAR_fine(flood_month=AgMAR_m, flood_days=AgMAR_d, wet_yrs=as.integer(wetyears), soil_Ksat = soilksat)}
    writeRZWQM(station = stn, scenario = scn, soil = suelos[i], input=gen_input, soil_input = suelo_input, planting_data = planting, fert_plan = fert, tillage_plan = tillage, Irr_plan = Irr)
    steady_state_setup(station = stn, scenario = scn, soil = suelos[i], type = 'run 2', soil_input = suelo_input)
  }
}