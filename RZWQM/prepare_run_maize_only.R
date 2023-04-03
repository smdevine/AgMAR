#to-do
#1: make crop planting not dependent on soil moisture [done]
#2: make fake winter cover crop planting and harvest functional regardless of whether the day in the date is single or double digits
#3: Calc initial and final SON (Total Soil N less NO3-N) [done]
#functions go to line 341

#get all initial N pools from here down in mblnit.out : "---  1/10/1983 --- 274 --- TOTAL NITROGEN MASS BALANCE (KG/HA)"
#get all final N pools from here down in mblnit.out : "---  1/10/2020 --- 275 --- TOTAL NITROGEN MASS BALANCE (KG/HA)"
options(max.print = 15000)
options(width=130)
# workDir <- 'C:/Users/smdevine/Desktop/post doc/Dahlke/RZWQM/projects/PulseSoilClimate/ClimateRuns_Silage/'
copyRZINIT <- function(station, scenario, soil) {
  x <- readLines(file.path(workDir, station, scenario, soil, 'soil info', 'RZINIT.DAT')) #this file has to be manually copied into this directory after creation with the RZWQM GUI by entering soil information
  x_file <- file(file.path(workDir, station, scenario, soil, 'RZINIT.DAT'), 'w') #overwrite existing RZINIT.DAT file that was imported with template scenario
  writeLines(x, con = x_file)
  close(x_file)
}
# station <- 'Shafter'
# scenario <- 'SteadyStateRuns'
# soil <- 'Kimberlina'
# fname <- 'RZWQM_init.DAT'
# slowOMdecay <- '3.600E-09'
templateRZWQM <- function(station, scenario, soil, fname, ChangeOMdecay=FALSE, fastOMdecay='2.500E-07', intOMdecay='5.000E-08', slowOMdecay='4.500E-10', ChangeRvals=FALSE, R34=0.6, R45=0.4) {
  input <- readLines(file.path(workDir, station, scenario, soil, fname))
  if(ChangeOMdecay) {
    om_decay <- unlist(strsplit(input[which(grepl('INDIVIDUAL OM DECAY "A" VALUES', input))+6], '  '))
    om_decay <- paste(c(om_decay[1:2], fastOMdecay, intOMdecay, slowOMdecay), collapse =  '  ')
    input[which(grepl('INDIVIDUAL OM DECAY "A" VALUES', input))+6] <- om_decay
  }
  if(ChangeRvals) {
    Rvals <- unlist(strsplit(input[which(grepl('OM TRANSFORMATION DISTRIBUTION CONSTANTS', input))+13], '  '))
    Rvals <- paste(c(Rvals[1:2], R34, Rvals[4], R45, Rvals[6]), collapse = '  ')
    input[which(grepl('OM TRANSFORMATION DISTRIBUTION CONSTANTS', input))+13] <- Rvals
  }
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
    corn_line2 <- '1' #refers to one date specific irrigation
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
  c(paste((length(years)*2+length(wet_yrs)), irrigationAssumptions, sep = '  '), IrrPlan, IrrPlan2)
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
  c(paste((length(years)*2+4*length(wet_yrs)), irrigationAssumptions, sep = '  '), IrrPlan, IrrPlan2) #because this relies on separate irrigation statements for each AgMAR application
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
      cntrl[length(cntrl)] <- '1   0   0   0   1   1   1   1   0   0   1   0' #reading in but not writing binary file for copying to Flood-MAR subdirectories
      writeFile <- file(file.path(workDir, station, scenario, soil, 'CNTRL.DAT'), 'w')
      writeLines(cntrl, con = writeFile)
      close(writeFile)
  }
}

#AgMAR scenario functions
AgMAR_runs <- function(stn, scn, suelos, AgMAR_month, AgMAR_days, fake_planting, fake_harvest, wetyears) {
  met <- paste0(stn, '_1983_2021.MET')
  brk <- paste0(stn, '_1983_2021.BRK')
  for(i in 1:length(suelos)) {
    print(i)
    ana <- paste0(suelos[i], '_', scn, '.ana')
    ipnames_fix(station = stn, scenario = scn, soil = suelos[i], met_fname = met, brk_fname = brk, ana_fname = ana)
    copyRZINIT(stn, scn, suelos[i])
    gen_input <- templateRZWQM(stn, scn, suelos[i], 'RZWQM_init.DAT')
    suelo_input <- soilInfo(stn, scn, suelos[i], 'RZWQM.DAT')
    planting <- writePlantingPlan_AgMAR(wet_years=as.integer(wetyears), rye_date_planting = fake_planting, rye_date_harvest = fake_harvest)
    fert <- writeFertPlan(end_year = 2020)
    tillage <- writeTillagePlan(depth = '15.0')
    Irr <- if(suelos[i] %in% stress_soils) {writeIrrPlan_AgMAR(cornADtrigger1 = '0.8', cornADtrigger2 = '0.7', flood_month=AgMAR_month, flood_days=AgMAR_days, wet_yrs=as.integer(wetyears))} else{writeIrrPlan_AgMAR(flood_month=AgMAR_month, flood_days=AgMAR_days, wet_yrs=as.integer(wetyears))}
    writeRZWQM(station = stn, scenario = scn, soil = suelos[i], input=gen_input, soil_input = suelo_input, planting_data = planting, fert_plan = fert, tillage_plan = tillage, Irr_plan = Irr)
    steady_state_setup(station = stn, scenario = scn, soil = suelos[i], type = 'run 2', soil_input = suelo_input)
  }
}
AgMAR_runs_LF <- function(stn, scn, suelos, AgMAR_month, AgMAR_days, fake_planting, fake_harvest, wetyears) {
  met <- paste0(stn, '_1983_2021.MET')
  brk <- paste0(stn, '_1983_2021.BRK')
  for(i in 1:length(suelos)) {
    print(i)
    ana <- paste0(suelos[i], '_', scn, '.ana')
    ipnames_fix(station = stn, scenario = scn, soil = suelos[i], met_fname = met, brk_fname = brk, ana_fname = ana)
    copyRZINIT(stn, scn, suelos[i])
    gen_input <- templateRZWQM(stn, scn, suelos[i], 'RZWQM_init.DAT')
    suelo_input <- soilInfo(stn, scn, suelos[i], 'RZWQM.DAT')
    planting <- writePlantingPlan_AgMAR_LF(wet_years=as.integer(wetyears), rye_date_planting = fake_planting, rye_date_harvest = fake_harvest)
    fert <- writeFertPlan(end_year = 2020)
    tillage <- writeTillagePlan(depth = '15.0')
    Irr <- if(suelos[i] %in% stress_soils) {writeIrrPlan_AgMAR_LF(cornADtrigger1 = '0.8', cornADtrigger2 = '0.7', flood_month=AgMAR_month, flood_days=AgMAR_days, wet_yrs=as.integer(wetyears))} else{writeIrrPlan_AgMAR_LF(flood_month=AgMAR_month, flood_days=AgMAR_days, wet_yrs=as.integer(wetyears))}
    writeRZWQM(station = stn, scenario = scn, soil = suelos[i], input=gen_input, soil_input = suelo_input, planting_data = planting, fert_plan = fert, tillage_plan = tillage, Irr_plan = Irr)
  }
}

# projectName <- 'SteadyStateRuns'
# station <- 'Shafter'
# soil <- 'Kimberlina'
# initialday=274
# finalday=13789

overall_results_fn <- function(station, projectName, soil) {
  input <- readLines(file.path(workDir, station, projectName, soil, 'RZWQM.OUT'), n=14000)
  initial_soilNO3 <- input[which(grepl('LOSS TO LATERAL FLOW KG N/HA:', input))+7]
  initial_soilNO3 <- as.numeric(unlist(strsplit(initial_soilNO3, ' +'))[10])
  final_soilNO3 <- input[which(grepl('LOSS TO LATERAL FLOW KG N/HA:', input))+13522]
  final_soilNO3 <- as.numeric(unlist(strsplit(final_soilNO3, ' +'))[10])
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
  N2O_loss_nitri <- input[grepl('N2O LOSS FROM NITRIFICATION KG N/HA:', input)]
  N2O_loss_nitri <- as.numeric(unlist(strsplit(N2O_loss_nitri, ':'))[2])
  NxO_loss_nitri <- input[grepl('NxO LOSS FROM NITRIFICATION KG N/HA:', input)]
  NxO_loss_nitri <- as.numeric(unlist(strsplit(NxO_loss_nitri, ':'))[2])
  NxO_abs_nitri <- input[grepl('NxO ABSORBED FROM NITRIFICATION KG N/HA:', input)]
  NxO_abs_nitri <- as.numeric(unlist(strsplit(NxO_abs_nitri, ':'))[2])
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
  result <- data.frame(initial_totalsoilMgC_ha=initial_soilC/1000, final_totalsoilMgC_ha=final_soilC/1000, initial_kgNO3_ha=initial_soilNO3, final_kgNO3_ha=final_soilNO3, initial_totalsoilkgN_ha=initial_soilN, final_totalsoilkgN_ha=final_soilN, denitrification_kgN_ha=denitrification, NO3_leached_kgN_ha=NO3_leaching, NO3_ppm=NO3_ppm, volatilized_kgN_ha=vol, fert_app_kgN_ha=fert_app, N_min_kgN_ha=N_min, N_imm_kgN_ha=N_imm, N2O_loss_nitri=N2O_loss_nitri, NxO_loss_nitri=NxO_loss_nitri, NxO_abs_nitri=NxO_abs_nitri, precip_cm=precip, irrigation_cm=irr, runoff_cm=runoff, evap_cm=evap, trans_cm=trans, DP_cm=DP)
  # print(soil)
  # print(result)
  if(!dir.exists(file.path(workDir, 'Results', 'Overall', station))) {dir.create(file.path(workDir, 'Results', 'Overall', station))}
  write.csv(result, file.path(workDir, 'Results', 'Overall', station, paste0(soil,'_', projectName, '_', station, '.csv')))
}
# overall_results_fn('Parlier', 'SteadyStateRuns', 'Capay')
writeOverallResults <- function(compnames, scenario, weather_stn) {
  for(i in 1:length(compnames))
    overall_results_fn(station = weather_stn, projectName = scenario, soil = compnames[i])
}

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
  if(!dir.exists(file.path(workDir, 'Results', 'SeasonalFluxes', stn))) {dir.create(file.path(workDir, 'Results', 'SeasonalFluxes', stn))}
  write.csv(result, file.path(workDir, 'Results', 'SeasonalFluxes', stn, paste0(soil,'_', projectName, '_', stn, '.csv')))
}
writeSeasonalResults <- function(compnames, scenario, weather_stn) {
  for(i in 1:length(compnames))
    N_MBL_season(projectName = scenario, stn = weather_stn, soil = compnames[i])
}

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
  if(!dir.exists(file.path(workDir, 'Results', 'MonthlyFluxes', stn))) {dir.create(file.path(workDir, 'Results', 'MonthlyFluxes', stn))}
  write.csv(result, file.path(workDir, 'Results', 'MonthlyFluxes', stn, paste0(soil,'_', projectName, '_', stn, '.csv')))
}
writeMonthlyResults <- function(compnames, scenario, weather_stn) {
  for(i in 1:length(compnames))
    N_MBL_monthly(projectName = scenario, stn = weather_stn, soil = compnames[i])
}
#temp args
station <- 'Shafter'
projectName <- 'AgMAR_Jan3d'
soil <- 'Kimberlina'
#harvest data
harvest_results_fn <- function(station, projectName, soil) {
  input <- readLines(file.path(workDir, station, projectName, soil, 'MANAGE.OUT'), skipNul = TRUE)
  harvest_dates <- input[which(grepl('----DSSAT Crop Harvest----MAIZE 990001 LONG SEASON', input))+1]
  harvest_dates <- strsplit(harvest_dates, '-----')
  harvest_dates <- sapply(harvest_dates, function(x) x[1])
  harvest_dates <- gsub('    ON  ', '', harvest_dates)
  harvest_dates <- strsplit(harvest_dates, '[/]')
  harvest_dates <- do.call(rbind, lapply(harvest_dates, function(x) {data.frame(month=x[2], day=x[1], year=x[3], stringsAsFactors = FALSE)}))
  crop_type <- input[grepl('----DSSAT Crop Harvest----MAIZE 990001 LONG SEASON', input)]
  crop_type <- strsplit(crop_type, '----')
  crop_type <- sapply(crop_type, function(x) x[3])
  biomass <- input[which(grepl('----DSSAT Crop Harvest----MAIZE 990001 LONG SEASON', input))+7]
  biomass <- strsplit(biomass, ':')
  biomass <- sapply(biomass, function(x){as.numeric(x[2])})
  above_biomass_N <- input[which(grepl('----DSSAT Crop Harvest----MAIZE 990001 LONG SEASON', input))+12]
  above_biomass_N <- strsplit(above_biomass_N, ':')
  above_biomass_N <- sapply(above_biomass_N, function(x){as.numeric(x[2])})
  below_biomass_N <- input[which(grepl('----DSSAT Crop Harvest----MAIZE 990001 LONG SEASON', input))+13]
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
  if(!dir.exists(file.path(workDir, 'Results', 'Yields', station))) {dir.create(file.path(workDir, 'Results', 'Yields', station))}
  write.csv(harvest_info, file.path(workDir, 'Results', 'Yields', station, paste0(soil,'_', projectName, '_', station, '.csv')))
}
writeHarvestResults <- function(compnames, scenario, weather_stn) {
  for(i in 1:length(compnames))
    harvest_results_fn(station = weather_stn, projectName = scenario, soil = compnames[i])
}