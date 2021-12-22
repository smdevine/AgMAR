options(max.print = 15000)
options(width=130)
# workDir <- 'C:/Users/smdevine/Desktop/post doc/Dahlke/RZWQM/projects/PulseSoilClimate/InitialTest_v2/Parlier_1983_2021test/CoarseSHR_automation'
workDir <- 'C:/Users/smdevine/Desktop/post doc/Dahlke/RZWQM/projects/PulseSoilClimate/ClimateRuns/Parlier/BaseRuns/CoarseSHR'
workDir <- 'C:/Users/smdevine/Desktop/post doc/Dahlke/RZWQM/projects/PulseSoilClimate/ClimateRuns/Parlier/FloodRuns_v1/LoamySHR'
#ClimateRuns\Parlier\BaseRuns\CoarseSHR
input <- readLines(file.path(workDir, 'RZWQM_input.DAT'))
class(input)
length(input)


planting_data_start <- which(input=='=    5       planting window in days after plant date (# of days) [0-45]') + 2
section1 <- input[1:(planting_data_start-1)]
planting_data_end <- which(input=='==               M A N U R E   M A N A G E M E N T                    ==') - 2
fert_data_start <- which(input=='=     ...   repeat record 2 for each application.') + 1 #this is line before start of fertilization details
section2 <- input[planting_data_end:fert_data_start] 
fert_data_end <- which(input=='==                   B M P   M A N A G E M E N T                      ==') - 2
tillage_data_start <- which(input=='=     3...   repeat record 2 for each tillage operation') + 1
section3 <- input[fert_data_end:tillage_data_start]
tillage_data_end <- which(input=='==            T I L E H E A D G A T E      M A N A G E M E N T        ==') - 2
irrigation_data_start <- which(input=='=   . . .  repeat Rec 2-4 for each operation') + 1
section4 <- input[tillage_data_end:irrigation_data_start]

start_year <- 1984
end_year <- 2020
years <- start_year:end_year
plantings <- length(1984:2020)

#TO-DO: add cereal rye placeholder, varying pop. density to get desired effect and allowing for winter irrigation events

#turn planting plan into function
corn_line1 <- '1  15   4  1984  20.0   3 40000.0   0 -99.0   10 -99.0   -99'
corn_line2 <- '               1 1.0   0 0.0  0 0 0'
corn_line3 <- '               10.0  0.95   3 10 15'
unlist(strsplit(corn_line1, ' '))
substring(corn_line1, 12, 15) #the year
tomato_line1 <- '2  15   4  1985  150.0   2 40000.0   0 -99.0   10 -99.0   -99' #could make this transplant in future
tomato_line2 <- '               1 1.0   0 0.0  0 0 0'
tomato_line3 <- '               10.0  0.55   4 10 15'

planting_plan <- do.call(c, lapply(years, function(x) {
  if(x%%2==0) {
    corn_line1_final <- corn_line1
    substring(corn_line1_final, 12, 15) <- as.character(x)
    c(corn_line1_final, corn_line2, corn_line3)
  } else {
      tomato_line1_final <- tomato_line1
      substring(tomato_line1_final, 12, 15) <- as.character(x)
      c(tomato_line1_final, tomato_line2, tomato_line3)
    }
  }))
planting_data <- c(as.character(plantings), planting_plan)

#write fert plan
#this is not yet a dynamic function in terms of modifying numbers of lines and dates for alternative split application plans (i.e. right now assumes preplant + 2 splits for corn and preplant + 8 splits for tomato with regularized annual schedule)
writeFertPlan <- function(corn_N_kg_ha_yr = 250, corn_preplant = 50, corn_splits = 2, tomato_N_kg_ha_yr = 250, tomato_preplant=50, tomato_splits=8, start_year=1984, end_year=2020) {
  years <- start_year:end_year
  plantings <- length(start_year:end_year)
  # corn_app <- as.character(format(round((corn_N_kg_ha_yr - as.numeric(corn_preplant))/corn_splits, digits=1), nsmall=1))
  # tomato_app <- as.character(format(round((tomato_N_kg_ha_yr - as.numeric(tomato_preplant))/tomato_splits, digits=1), nsmall=1))
  corn_app <- (corn_N_kg_ha_yr - corn_preplant)/corn_splits
  tomato_app <- (tomato_N_kg_ha_yr - tomato_preplant)/tomato_splits
  NO3_NH4_rate <- 7.75/32
  urea_rate <- 16.5/32
  fertPlan <- do.call(c, lapply(years, function(x) {
    if(x%%2==0) {
      cornPreplant <- paste('1  5  15  4', x, '2', round(corn_preplant*NO3_NH4_rate, 1), round(corn_preplant*NO3_NH4_rate, 1), round(corn_preplant*urea_rate, 1), '1  1  0  0.0  0.0  0.0', sep='  ') #date is 4/15
      corn_split1 <- paste('1  5  1  6', x, '1', round(corn_app*NO3_NH4_rate, 1), round(corn_app*NO3_NH4_rate, 1), round(corn_app*urea_rate, 1), '1  1  0  0.0  0.0  0.0', sep='  ')
      corn_split2 <- paste('1  5  1  7', x, '1', round(corn_app*NO3_NH4_rate, 1), round(corn_app*NO3_NH4_rate, 1), round(corn_app*urea_rate, 1), '1  1  0  0.0  0.0  0.0', sep='  ')
      c(cornPreplant, corn_split1, corn_split2)
    } else {
      tomatoPreplant <- paste('2  5  15  4', x, '2', round(tomato_preplant*NO3_NH4_rate, 1), round(tomato_preplant*NO3_NH4_rate, 1), round(tomato_preplant*urea_rate, 1), '1  1  0  0.0  0.0  0.0', sep='  ') #date is 4/15
      tomato_split1 <- paste('2  5  10  5', x, '1', round(tomato_app*NO3_NH4_rate, 1), round(tomato_app*NO3_NH4_rate, 1), round(tomato_app*urea_rate, 1), '1  1  0  0.0  0.0  0.0', sep='  ') #date is 5/10
      tomato_split2 <- paste('2  5  22  5', x, '1', round(tomato_app*NO3_NH4_rate, 1), round(tomato_app*NO3_NH4_rate, 1), round(tomato_app*urea_rate, 1), '1  1  0  0.0  0.0  0.0', sep='  ') #date is 5/22
      tomato_split3 <- paste('2  5  4  6', x, '1', round(tomato_app*NO3_NH4_rate, 1), round(tomato_app*NO3_NH4_rate, 1), round(tomato_app*urea_rate, 1), '1  1  0  0.0  0.0  0.0', sep='  ') #date is 6/4
      tomato_split4 <- paste('2  5  14  6', x, '1', round(tomato_app*NO3_NH4_rate, 1), round(tomato_app*NO3_NH4_rate, 1), round(tomato_app*urea_rate, 1), '1  1  0  0.0  0.0  0.0', sep='  ') #date is 6/14
      tomato_split5 <- paste('2  5  24  6', x, '1', round(tomato_app*NO3_NH4_rate, 1), round(tomato_app*NO3_NH4_rate, 1), round(tomato_app*urea_rate, 1), '1  1  0  0.0  0.0  0.0', sep='  ') #date is 6/24
      tomato_split6 <- paste('2  5  3  7', x, '1', round(tomato_app*NO3_NH4_rate, 1), round(tomato_app*NO3_NH4_rate, 1), round(tomato_app*urea_rate, 1), '1  1  0  0.0  0.0  0.0', sep='  ') #date is 7/3
      # tomato_split7 <- paste('2  5  13  7', x, '1', tomato_app, '0.0  0.0  1  1  0  0.0  0.0  0.0', sep='  ') #date is 7/13
      # tomato_split8 <- paste('2  5  23  7', x, '1', tomato_app, '0.0  0.0  1  1  0  0.0  0.0  0.0', sep='  ') #date is 7/23
      c(tomatoPreplant, tomato_split1, tomato_split2, tomato_split3, tomato_split4, tomato_split5, tomato_split6)#, tomato_split7, tomato_split8)
      }
    })
  )
  c(length(fertPlan), fertPlan)
}
fert_plan <- writeFertPlan(end_year = 2020, tomato_N_kg_ha_yr = 200, tomato_splits = 6)

#write tillage plan
writeTillagePlan <- function(start_year=1984, end_year=2020, depth=15) {
  years <- start_year:end_year
  plantings <- length(years)
  tillagePlan <- do.call(c, lapply(years, function(x) {
    if(x%%2==0) {
      corn_line1 <- paste('1 5 14  4', x, '5', depth, '0.5  1', sep='  ') #date is 4/14
      corn_line2 <- paste('1 5 15  4', x, '17  3.0  0.08  2', sep='  ')
      corn_line3 <- paste('1 5 15  9', x, '5', depth, '0.5  1', sep='  ') #depth is 15.0 cm
      corn_line4 <- paste('1 5 16  9', x, '5', depth, '0.5  1', sep='  ')
      corn_line5 <- paste('1 5 17  9', x, '12', depth, '0.75  1', sep='  ')
      c(corn_line1, corn_line2, corn_line3, corn_line4, corn_line5)
    } else {
      tomato_line1 <- paste('2 5 15  4', x, '20  3.0  0.4  2', sep='  ') #date is 4/15
      tomato_line2 <- paste('2 5 15  10', x, '5', depth, '0.5  1', sep='  ') #date is 10/15
      tomato_line3 <- paste('2 5 16  10', x, '5', depth, '0.5  1', sep='  ') #date is 10/15
      c(tomato_line1, tomato_line2, tomato_line3)
    }
  })
  )
  c(length(tillagePlan), tillagePlan)
}
tillage_plan <- writeTillagePlan(depth = '15.0')
tillage_plan

#create irrigation plan
#irrigationAssumptions argument consists of: 
#[2] depth of the rooting zone for depletion calculation, (DEFAULT = 0 use active rooting zone) [0...3000 cm]
#[3] flag to use maximum monthly irrigation amounts [0=NO,1=YES]
#[4] minimum daily irrigation amount (cm)
#[5] maximum daily irrigation amount (cm)
#[6] subirrigation depth (default=15 cm) but not N/A to this scenario
#[7] irrigation interval limit amount(default=0)
#[8] irrigation interval time in years(default=0)
# the number of irrigation operations is calculated by the function
writeIrrPlan <- function(start_year=1984, end_year=2020, irrigationAssumptions='0  0  0.0  100.0  15.0  0.0  0', cornADtrigger1='0.6', cornDaystrigger1='3', cornDaystrigger2='60', cornADtrigger2='0.5', tomADtrigger1='0.8', tomDaystrigger1='3', tomADtrigger2='0.6', tomDaystrigger2='84', tomADtrigger3='0.4', tomDaystrigger3='135', tomFilltrigger3='0.75', IrrAppPlanting=1.25, flood_month='1', flood_days=c('15', '19', '23', '27'), flood_app=15.0, wet_yrs) {
  years <- start_year:end_year
  plantings <- length(years)
  IrrPlan <- do.call(c, lapply(years, function(x) {
    if(x%%2==0) {
      corn_line1 <- paste('1  2  3  3 20 4', x, '1 9', x, '3 0.0  0.0', sep=' ') #date is 4/14; that #2 near the beginning should be a 3 for 'furrow'
      corn_line2 <- '2' #this is irrigation rule #2
      corn_line3 <- paste(' ', cornDaystrigger1, cornADtrigger1, '  ', cornDaystrigger2, cornADtrigger2, sep = ' ') 
      corn_line4 <- '2' #this is irrigation rule #2
      corn_line5 <- paste('', cornDaystrigger1, ' 1.0', ' ', cornDaystrigger2, ' 1.0', sep = ' ')
      c(corn_line1, corn_line2, corn_line3, corn_line4, corn_line5)
    } else {
      tom_line1 <- paste('2  2  3  3 20 4', x, '15 9', x, '3 0.0  0.0', sep=' ') #date is 4/14
      tom_line2 <- '3' #this is irrigation rule #3
      tom_line3 <- paste(' ', tomDaystrigger1, tomADtrigger1, '  ', tomDaystrigger2, tomADtrigger2, '  ', tomDaystrigger3, tomADtrigger3, sep = ' ') 
      tom_line4 <- '3' #this is irrigation rule #3
      tom_line5 <- paste('', tomDaystrigger1, ' 1.0', ' ', tomDaystrigger2, ' 1.0', ' ', tomDaystrigger3, tomFilltrigger3, sep = ' ')
      c(tom_line1, tom_line2, tom_line3, tom_line4, tom_line5)
    }
  })
  )
  IrrPlan2 <- do.call(c, lapply(years, function(x) {
    if(x%%2==0) {
      corn_line1 <- paste('1  1  2  1 16 4', x, '16 4', x, '0 0.0  2.0', sep=' ') #date is 4/16
      corn_line2 <- '1' #not sure what this refers to
      corn_line3 <- paste('16  4', x, sep = '  ')
        if(x%in%wet_yrs) { #left off here to fix this on 12/21/21
        c(paste('1  2  2  2', flood_days[1], flood_month, x, '16  4', x, '0 0.0  2.0', sep=' '), '5', paste(flood_days[1], flood_month, x, sep = '  '), paste(flood_days[2], flood_month, x, sep = '  '), paste(flood_days[3], flood_month, x, sep = '  '), paste(flood_days[4], flood_month, x, sep = '  '), corn_line3, '5', paste(as.character(flood_app), as.character(flood_app), as.character(flood_app), as.character(flood_app), as.character(IrrAppPlanting))) # the '5' denotes 5 irrigation dates
      } else {c(corn_line1, corn_line2, corn_line3, as.character(IrrAppPlanting))} 
    } else {
      tom_line1 <- paste('2  1  2  1 16 4', x, '16 4', x, '0 0.0  2.0', sep=' ') #date is 4/16
      tom_line2 <- '1' #not sure what this refers to
      tom_line3 <- paste('16  4', x, sep = ' ')
        if(x%in%wet_yrs) {
          c(paste('2  2  2  2', flood_days[1], flood_month, x, '16  4', x, '0 0.0  2.0', sep=' '), '5', paste(flood_days[1], flood_month, x, sep = '  '), paste(flood_days[2], flood_month, x, sep = '  '), paste(flood_days[3], flood_month, x, sep = '  '), paste(flood_days[4], flood_month, x, sep = '  '), tom_line3, '5', paste(as.character(flood_app), as.character(flood_app), as.character(flood_app), as.character(flood_app), as.character(IrrAppPlanting)))
      } else {c(tom_line1, tom_line2, tom_line3, as.character(IrrAppPlanting))}
    }
  })
  )
  c(paste(length(years)*2, irrigationAssumptions, sep = '  '), IrrPlan, IrrPlan2)
}
wet_years <- c("1995", "2010", "1992", "1993", "1987", "1998", "2006", "1991", "1986", "1994") #as determined in get_CIMIS.R for Parlier dataset
Irr_plan <- writeIrrPlan(flood_month = '4', wet_yrs = wet_years)
Irr_plan

#write mgmt details to file
workDir <- 'C:/Users/smdevine/Desktop/post doc/Dahlke/RZWQM/projects/PulseSoilClimate/ClimateRuns/Parlier/FloodRuns_v1/LoamySHR'
mgmt_output <- file(file.path(workDir, 'RZWQM.DAT'), 'w')
writeLines(section1, con = mgmt_output)
writeLines(planting_data, con = mgmt_output)
writeLines(section2, con = mgmt_output) #just before start of fert data
writeLines(fert_plan, con = mgmt_output)
writeLines(section3, con = mgmt_output)
writeLines(tillage_plan, con = mgmt_output)
writeLines(section4, con = mgmt_output)
writeLines(Irr_plan, con=mgmt_output)
close(mgmt_output)
