#to-do
#1: make crop planting not dependent on soil moisture [done]
#2: make fake winter cover crop planting and harvest functional regardless of whether the day in the date is single or double digits
#functions go to line 341

options(max.print = 15000)
options(width=130)
workDir <- 'C:/Users/smdevine/Desktop/post doc/Dahlke/RZWQM/projects/PulseSoilClimate/ClimateRuns/'
copyRZINIT <- function(station, scenario, soil) {
  x <- readLines(file.path(workDir, station, scenario, soil, 'soil info', 'RZINIT.DAT')) #this file has to be manually copied into this directory after creation with the RZWQM GUI by entering soil information
  x_file <- file(file.path(workDir, station, scenario, soil, 'RZINIT.DAT'), 'w') #overwrite existing RZINIT.DAT file that was imported with template scenario
  writeLines(x, con = x_file)
  close(x_file)
}


templateRZWQM <- function(station, scenario, soil, fname) {
  input <- readLines(file.path(workDir, station, scenario, soil, fname))
  input
}
soilInfo <- function(station, scenario, soil, fname) {
  input <- readLines(file.path(workDir, station, scenario, soil, 'soil info', fname))
  input
}

#fix links in ipnames.DAT
# station <- 'Parlier'
# scenario <- 'BaseRuns'
# soil <- 'FineSHR'
ipnames_fix <- function(station, scenario, soil, met_fname, brk_fname, ana_fname) {
  ipnames <- readLines(file.path(workDir, station, scenario, soil, 'ipnames.DAT'))
  path1 <- unlist(strsplit(ipnames[1], '\\\\'))
  path1[11] <- station
  path1[12] <- scenario
  path1[13] <- soil
  # ipnames[1] <- cat(path1, sep='\\')
  
  path2 <- unlist(strsplit(ipnames[2], '\\\\'))
  path2[11] <- station
  path2[12] <- scenario
  path2[13] <- soil
  # ipnames[2] <- cat(paste(path2, collapse='\\'))
  
  path3 <- unlist(strsplit(ipnames[3], '\\\\'))
  path3[11] <- station
  path3[13] <- met_fname
  # ipnames[3] <- cat(paste(path3, collapse='\\'))
  
  path4 <- unlist(strsplit(ipnames[4], '\\\\'))
  path4[11] <- station
  path4[13] <- brk_fname
  # ipnames[4] <- cat(paste(path4, collapse='\\'))
  
  path5 <- unlist(strsplit(ipnames[5], '\\\\'))
  path5[11] <- station
  path5[12] <- scenario
  path5[13] <- soil
  # ipnames[5] <- cat(paste(path5, collapse='\\'))
  
  path6 <- unlist(strsplit(ipnames[6], '\\\\'))
  path6[11] <- station
  path6[12] <- scenario
  path6[13] <- soil
  # ipnames[6] <- cat(paste(path6, collapse='\\'))
  
  path8 <- unlist(strsplit(ipnames[8], '\\\\'))
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
  cat(ipnames[7], file = fileconn)
  cat('\n', file = fileconn)
  cat(path8, file = fileconn, sep = '\\')
  cat('\n', file = fileconn)
  writeLines(ipnames[9:length(ipnames)], con = fileconn)
  close(fileconn)
}

#need additional function for AgMAR scenarios that require winter crops to enable irrigation (see writeMgmtFiles_Flood_NoCC.R for example)
writePlantingPlan <- function(corn_line1 = '1  15   4  1984  20.0   3 40000.0   0 -99.0   10 -99.0   -99',  corn_line2 = '               1 1.0   0 0.0  0 0 0', corn_line3 = '               10.0  0.95   3 0 0', start_year = 1984, end_year = 2020) {
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
writePlantingPlan_AgMAR <- function(start_year=1984, end_year=2020, wet_years, cover_crop = FALSE, rye_date_planting = '14   1   1992', rye_date_harvest = '28 1 1993', rye_emergence = '10', corn_line1 = '1  15   4  1984  20.0   3 40000.0   0 -99.0   10 -99.0   -99', corn_line2 = '               1 1.0   0 0.0  0 0 0', corn_line3 = '               10.0  0.95   3 0 0', rye_line1 = paste('3', rye_date_planting, '10.0   2 0.0   0 -99.0', rye_emergence, '-99.0   -99', sep = '  '), rye_line2 = paste('               3 0.0   0 0.0', rye_date_harvest, sep = ' '), rye_line3 = '               10.0  0.9   4 0 0') {
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
writePlantingPlan_AgMAR_LF <- function (start_year=1984, end_year=2020, wet_years, cover_crop = FALSE, rye_date_planting, rye_date_harvest, rye_emergence = '10', corn_line1 = '1  15   4  1984  20.0   3 40000.0   0 -99.0   10 -99.0   -99', corn_line2 = '               1 1.0   0 0.0  0 0 0', corn_line3 = '               10.0  0.95   3 0 0') {
  rye_planting <- function(y, z, k) {
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

#write fert plan
#this is not yet a dynamic function in terms of modifying numbers of lines and dates for alternative split application plans (i.e. right now assumes preplant + 2 splits for corn and preplant + 8 splits for tomato with regularized annual schedule)
writeFertPlan <- function(corn_N_kg_ha_yr = 250, corn_preplant = 50, corn_splits = 3, start_year=1984, end_year=2020) {
  years <- start_year:end_year
  plantings <- length(start_year:end_year)
  # corn_app <- as.character(format(round((corn_N_kg_ha_yr - as.numeric(corn_preplant))/corn_splits, digits=1), nsmall=1))
  # tomato_app <- as.character(format(round((tomato_N_kg_ha_yr - as.numeric(tomato_preplant))/tomato_splits, digits=1), nsmall=1))
  corn_app <- (corn_N_kg_ha_yr - corn_preplant)/corn_splits
  NO3_NH4_rate <- 7.75/32 #was 7.75/32 based on UAN, now ammonium nitrate
  urea_rate <- 16.5/32 #was 16.5/32 based on UAN, now ammonium
  fertPlan <- do.call(c, lapply(years, function(x) {
    cornPreplant <- paste('1  5  15  4', x, '2', round(corn_preplant*NO3_NH4_rate, 1), round(corn_preplant*NO3_NH4_rate, 1), round(corn_preplant*urea_rate, 1), '1  1  0  0.0  0.0  0.0', sep='  ') #date is 4/15
    corn_split1 <- paste('1  5  1  6', x, '2', round(corn_app*NO3_NH4_rate, 1), round(corn_app*NO3_NH4_rate, 1), round(corn_app*urea_rate, 1), '1  1  0  0.0  0.0  0.0', sep='  ')
    corn_split2 <- paste('1  5  1  7', x, '2', round(corn_app*NO3_NH4_rate, 1), round(corn_app*NO3_NH4_rate, 1), round(corn_app*urea_rate, 1), '1  1  0  0.0  0.0  0.0', sep='  ')
    corn_split3 <- paste('1  5  21  7', x, '2', round(corn_app*NO3_NH4_rate, 1), round(corn_app*NO3_NH4_rate, 1), round(corn_app*urea_rate, 1), '1  1  0  0.0  0.0  0.0', sep='  ')
    c(cornPreplant, corn_split1, corn_split2, corn_split3)
  }))
  c(length(fertPlan), fertPlan)
}

#write tillage plan
writeTillagePlan <- function(start_year=1984, end_year=2020, depth=15) {
  years <- start_year:end_year
  plantings <- length(years)
  tillagePlan <- do.call(c, lapply(years, function(x) {
    corn_line1 <- paste('1 5 14  4', x, '5', depth, '0.5  1', sep='  ') #date is 4/14
    corn_line2 <- paste('1 5 15  4', x, '17  3.0  0.08  2', sep='  ')
    corn_line3 <- paste('1 5 15  9', x, '5', depth, '0.5  1', sep='  ') #depth is 15.0 cm
    corn_line4 <- paste('1 5 16  9', x, '5', depth, '0.5  1', sep='  ')
    corn_line5 <- paste('1 5 17  9', x, '12', depth, '0.75  1', sep='  ')
    c(corn_line1, corn_line2, corn_line3, corn_line4, corn_line5)
  }))
  c(length(tillagePlan), tillagePlan)
}

#create irrigation plan
#additional function still needed for AgMAR scenarios
#irrigationAssumptions argument consists of: 
#[2] depth of the rooting zone for depletion calculation, (DEFAULT = 0 use active rooting zone) [0...3000 cm]
#[3] flag to use maximum monthly irrigation amounts [0=NO,1=YES]
#[4] minimum daily irrigation amount (cm)
#[5] maximum daily irrigation amount (cm)
#[6] subirrigation depth (default=15 cm) but not N/A to this scenario
#[7] irrigation interval limit amount(default=0)
#[8] irrigation interval time in years(default=0)
# the number of irrigation operations is calculated by the function
writeIrrPlan <- function(start_year=1984, end_year=2020, irrigationAssumptions='0  0  0.0  100.0  15.0  0.0  0', cornADtrigger1='0.6', cornDaystrigger1='3', cornDaystrigger2='60', cornADtrigger2='0.5', IrrAppPlanting=1.25) {
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
    c(corn_line1, corn_line2, corn_line3, as.character(IrrAppPlanting)) 
  }))
  c(paste(length(years)*2, irrigationAssumptions, sep = '  '), IrrPlan, IrrPlan2)
}

#irrigation plan with AgMAR
writeIrrPlan_AgMAR <- function(start_year=1984, end_year=2020, irrigationAssumptions='0  0  0.0  100.0  15.0  0.0  0', cornADtrigger1='0.6', cornDaystrigger1='3', cornDaystrigger2='60', cornADtrigger2='0.5', IrrAppPlanting=1.25, flood_month='1', flood_days=c('15', '19', '23', '27'), flood_app=15.0, wet_yrs) {
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
      c(paste('3  2  2  2', flood_days[1], flood_month, x, flood_days[4], flood_month, x, '0 0.0  2.0', sep=' '), '4', paste(flood_days[1], flood_month, x, sep = '  '), paste(flood_days[2], flood_month, x, sep = '  '), paste(flood_days[3], flood_month, x, sep = '  '), paste(flood_days[4], flood_month, x, sep = '  '), '4', paste(as.character(flood_app), as.character(flood_app), as.character(flood_app), as.character(flood_app)), corn_line1, corn_line2, corn_line3, as.character(IrrAppPlanting)) # the '4' denotes 4 irrigation dates
    } else {c(corn_line1, corn_line2, corn_line3, as.character(IrrAppPlanting))} 
  }))
  c(paste(length(years)*2, irrigationAssumptions, sep = '  '), IrrPlan, IrrPlan2)
}

writeIrrPlan_AgMAR_LF <- function(start_year=1984, end_year=2020, irrigationAssumptions='0  0  0.0  100.0  15.0  0.0  0', cornADtrigger1='0.6', cornDaystrigger1='3', cornDaystrigger2='60', cornADtrigger2='0.5', IrrAppPlanting=1.25, flood_month, flood_days, flood_app=15.0, wet_yrs) {
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
      c(paste('3  2  2  2', flood_days[1], flood_month[1], x, flood_days[1], flood_month[1], x, '0 0.0  2.0', sep=' '), '1', paste(flood_days[1], flood_month[1], x, sep = '  '), '1', as.character(flood_app), paste('3  2  2  2', flood_days[2], flood_month[2], x, flood_days[2], flood_month[2], x, '0 0.0  2.0', sep=' '), '1', paste(flood_days[2], flood_month[2], x, sep = '  '), '1', as.character(flood_app), paste('3  2  2  2', flood_days[3], flood_month[3], x, flood_days[3], flood_month[3], x, '0 0.0  2.0', sep=' '), '1', paste(flood_days[3], flood_month[3], x, sep = '  '), '1', as.character(flood_app), paste('3  2  2  2', flood_days[4], flood_month[4], x, flood_days[4], flood_month[4], x, '0 0.0  2.0', sep=' '), '1', paste(flood_days[4], flood_month[4], x, sep = '  '), '1', as.character(flood_app), corn_line1, corn_line2, corn_line3, as.character(IrrAppPlanting)) # the '4' denotes 4 irrigation dates
    } else {c(corn_line1, corn_line2, corn_line3, as.character(IrrAppPlanting))} 
  }))
  c(paste((length(years)*2+length(wet_yrs)*4), irrigationAssumptions, sep = '  '), IrrPlan, IrrPlan2)
}

#write file
writeRZWQM <- function(station, scenario, soil, input, soil_input, planting_data, fert_plan, tillage_plan, Irr_plan) {
  horizon_depths_start <- which(input=='= Rec 2: soil horizon lower depths                                  [cm]') + 2
  intro_section <- input[1:horizon_depths_start]
  horizon_props <- soil_input[(which(soil_input=='= Rec 2: soil horizon lower depths                                  [cm]') + 3):(which(soil_input=='==            P O T E N T I A L   E V A P O R A T I O N               ==') - 3)]
  horizon_props_end <- which(input=='==            P O T E N T I A L   E V A P O R A T I O N               ==') - 2
  planting_data_start <- which(input=='=    5       planting window in days after plant date (# of days) [0-45]') + 1
  section1 <- input[horizon_props_end:planting_data_start]
  planting_data_end <- which(input=='==               M A N U R E   M A N A G E M E N T                    ==') - 2
  fert_data_start <- which(input=='=     ...   repeat record 2 for each application.') + 1 #this is line before start of fertilization details
  section2 <- input[planting_data_end:fert_data_start]
  fert_data_end <- which(input=='==                   B M P   M A N A G E M E N T                      ==') - 2
  tillage_data_start <- which(input=='=     3...   repeat record 2 for each tillage operation') + 1
  section3 <- input[fert_data_end:tillage_data_start]
  tillage_data_end <- which(input=='==            T I L E H E A D G A T E      M A N A G E M E N T        ==') - 2
  irrigation_data_start <- which(input=='=   . . .  repeat Rec 2-4 for each operation') + 1
  section4 <- input[tillage_data_end:irrigation_data_start]
  mgmt_output <- file(file.path(workDir, station, scenario, soil, 'RZWQM.DAT'), 'w')
  writeLines(intro_section, con = mgmt_output)
  writeLines(horizon_props, con = mgmt_output)
  writeLines(section1, con = mgmt_output)
  writeLines(planting_data, con = mgmt_output)
  writeLines(section2, con = mgmt_output) #just before start of fert data
  writeLines(fert_plan, con = mgmt_output)
  writeLines(section3, con = mgmt_output)
  writeLines(tillage_plan, con = mgmt_output)
  writeLines(section4, con = mgmt_output)
  writeLines(Irr_plan, con=mgmt_output)
  close(mgmt_output)
}

#run 1 setup for steady state
steady_state_setup <- function(station, scenario, soil, type, soil_input) {
  node_number <- soil_input[(which(grepl('depth increasing with node no.', soil_input))+2)] 
  cntrl <- readLines(file.path(workDir, station, scenario, soil, 'CNTRL.DAT'))
  cntrl[(which(grepl('for Break Through curves here', cntrl))+2)] <- node_number
  if(type=='run 1') {
    cntrl[length(cntrl)] <- '0   1   0   0   1   1   1   1   0   0   1   0' #writing binary file at end of run as initial conditions for subsequent runs to read-in
    writeFile <- file(file.path(workDir, station, scenario, soil, 'CNTRL.DAT'), 'w')
    writeLines(cntrl, con = writeFile)
    close(writeFile)
  } else {
      cntrl[length(cntrl)] <- '1   0   0   0   1   1   1   1   0   0   1   0' #reading in but not writing binary file
      writeFile <- file(file.path(workDir, station, scenario, soil, 'CNTRL.DAT'), 'w')
      writeLines(cntrl, con = writeFile)
      close(writeFile)
  }
}

#AgMAR scenario functions
AgMAR_runs <- function(stn, scn, suelos, AgMAR_month, AgMAR_days, fake_planting, fake_harvest) {
  met <- paste0(stn, '_1983_2021.MET')
  brk <- paste0(stn, '_1983_2021.BRK')
  for(i in 1:length(suelos)) {
    print(i)
    ana <- paste0(suelos[i], '_', scn, '.ana')
    ipnames_fix(station = stn, scenario = scn, soil = suelos[i], met_fname = met, brk_fname = brk, ana_fname = ana)
    copyRZINIT(stn, scn, suelos[i])
    gen_input <- templateRZWQM(stn, scn, suelos[i], 'RZWQM_init.DAT')
    suelo_input <- soilInfo(stn, scn, suelos[i], 'RZWQM.DAT')
    planting <- writePlantingPlan_AgMAR(wet_years=as.integer(c("1995", "2010", "1992", "1993", "1987", "1998", "2006", "1991", "1986", "1994")), rye_date_planting = fake_planting, rye_date_harvest = fake_harvest)
    fert <- writeFertPlan(end_year = 2020)
    tillage <- writeTillagePlan(depth = '15.0')
    Irr <- if(suelos[i] %in% stress_soils) {writeIrrPlan_AgMAR(cornADtrigger1 = '0.8', cornADtrigger2 = '0.7', flood_month=AgMAR_month, flood_days=AgMAR_days, wet_yrs=as.integer(c("1995", "2010", "1992", "1993", "1987", "1998", "2006", "1991", "1986", "1994")))} else{writeIrrPlan_AgMAR(flood_month=AgMAR_month, flood_days=AgMAR_days, wet_yrs=as.integer(c("1995", "2010", "1992", "1993", "1987", "1998", "2006", "1991", "1986", "1994")))}
    writeRZWQM(station = stn, scenario = scn, soil = suelos[i], input=gen_input, soil_input = suelo_input, planting_data = planting, fert_plan = fert, tillage_plan = tillage, Irr_plan = Irr)
    steady_state_setup(station = stn, scenario = scn, soil = suelos[i], type = 'run 2', soil_input = suelo_input)
  }
}
AgMAR_runs_LF <- function(stn, scn, suelos, AgMAR_month, AgMAR_days, fake_planting, fake_harvest) {
  met <- paste0(stn, '_1983_2021.MET')
  brk <- paste0(stn, '_1983_2021.BRK')
  for(i in 1:length(suelos)) {
    print(i)
    ana <- paste0(suelos[i], '_', scn, '.ana')
    ipnames_fix(station = stn, scenario = scn, soil = suelos[i], met_fname = met, brk_fname = brk, ana_fname = ana)
    copyRZINIT(stn, scn, suelos[i])
    gen_input <- templateRZWQM(stn, scn, suelos[i], 'RZWQM_init.DAT')
    suelo_input <- soilInfo(stn, scn, suelos[i], 'RZWQM.DAT')
    planting <- writePlantingPlan_AgMAR_LF(wet_years=as.integer(c("1995", "2010", "1992", "1993", "1987", "1998", "2006", "1991", "1986", "1994")), rye_date_planting = fake_planting, rye_date_harvest = fake_harvest)
    fert <- writeFertPlan(end_year = 2020)
    tillage <- writeTillagePlan(depth = '15.0')
    Irr <- if(suelos[i] %in% stress_soils) {writeIrrPlan_AgMAR_LF(cornADtrigger1 = '0.8', cornADtrigger2 = '0.7', flood_month=AgMAR_month, flood_days=AgMAR_days, wet_yrs=as.integer(c("1995", "2010", "1992", "1993", "1987", "1998", "2006", "1991", "1986", "1994")))} else{writeIrrPlan_AgMAR_LF(flood_month=AgMAR_month, flood_days=AgMAR_days, wet_yrs=as.integer(c("1995", "2010", "1992", "1993", "1987", "1998", "2006", "1991", "1986", "1994")))}
    writeRZWQM(station = stn, scenario = scn, soil = suelos[i], input=gen_input, soil_input = suelo_input, planting_data = planting, fert_plan = fert, tillage_plan = tillage, Irr_plan = Irr)
  }
}
  
#run SSURGO soils through base runs with maize only scenario
compnames <- c('Hanford', 'Delhi', 'Wasco', 'Hesperia', 'Milham', 'Tujunga', 'Panoche', 'Cerini', 'Yolo', 'Colpien', 'Tehama', 'Capay', 'Clear Lake', 'Willows', 'Tulare', 'CoarseSHR', 'LoamySHR', 'FineSHR') #'Channac' not included
stress_soils <- c('Capay', 'Willows')#, 'Clear Lake', 'Tulare')
stn <- 'Parlier'

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
