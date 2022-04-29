library(cimir)
set_key(key = '4a7d3dc2-7431-46d1-9734-f28365113e68') #obtained through CIMIS web site account details page
is_key_set()
workDir <- 'C:/Users/smdevine/Desktop/post doc/Dahlke/RZWQM/cimis'
subDir <- 'Shafter_Stn5'
stn_no <- 5
met_stn <- 'Shafter'

#get metadata about station
stn_metadata <- cimis_station(station=stn_no)
stn_metadata <- as.data.frame(stn_metadata)
stn_metadata$HmsLatitude
stn_metadata$HmsLongitude
stn_metadata$Elevation
stn_metadata$ConnectDate[1]
dir.create(file.path(workDir, subDir))
write.csv(stn_metadata, file.path(workDir, subDir, 'stn_metadata.csv'), row.names=FALSE)

#get all parlier data
cimis_all <- do.call(rbind, mapply(cimis_please, start_date=c('1983-10-01', '1988-01-01', '1992-01-01', '1996-01-01', '2000-01-01', '2004-01-01', '2008-01-01', '2012-01-01', '2016-01-01', '2020-01-01'), end_date=c('1987-12-30', '1991-12-31', '1995-12-31', '1999-12-31', '2003-12-31', '2007-12-31', '2011-12-31', '2015-12-31', '2019-12-31', '2021-04-30'), MoreArgs = list(station=stn_no), SIMPLIFY = FALSE))
dim(cimis_all)
colnames(cimis_all)
sum(is.na(cimis_all$Value))
sum(cimis_all$Value==' ', na.rm = TRUE)
write.csv(cimis_all, file.path(workDir, subDir, paste0(met_stn, '_1983_2021cimisRAW.csv')), row.names = FALSE)

#read-in raw data
cimis_all <- read.csv(file.path(workDir, subDir, paste0(met_stn, '_1983_2021cimisRAW.csv')), stringsAsFactors = FALSE)

cimis_all_reformatted <- reformat_cimis(cimis_all)
dim(cimis_all_reformatted) 
head(cimis_all_reformatted)
tail(cimis_all_reformatted)
lapply(cimis_all_reformatted[,2:7], summary) #101 precip missing
sum(is.na(cimis_all_reformatted$precip_mm)) / nrow(cimis_all_reformatted)
cimis_all_reformatted[is.na(cimis_all_reformatted$precip_mm),]
sum(cimis_all_reformatted$precip_mm, na.rm = TRUE) / 10

#fill in missing dates
model_dates <- seq.Date(from = as.Date('1983/10/1'), to = as.Date('2021/4/30'), by = 'day')
length(model_dates)
head(model_dates)
cimis_dates <- cimis_all_reformatted$date
cimis_dates <- as.Date(cimis_dates, format = '%d/%m/%Y')
head(cimis_dates)
missing_dates <- model_dates[!(model_dates %in% cimis_dates)]
fix_indices <- match((missing_dates - 1), cimis_dates)
fix_indices[is.na(fix_indices)] <- fix_indices[which(is.na(fix_indices))-1] + 1
fix_indices
cimis_dates[fix_indices]
missing_dates

#just use first 2 and substitute nearby data for gap year (2012-2013)
missing_dates <- missing_dates[1:2]
fix_indices <- fix_indices[1:2]

dummy_row <- cimis_all_reformatted[1,]
dummy_row <- as.data.frame(sapply(dummy_row, function(x) {x <- NA}, simplify=FALSE))

#fix still needed for consecutive missing dates other than last date that falls into "else if" statement
sapply(1:length(missing_dates), function(i) { #
  print(i)
  existingDF <- 
    if(length(missing_dates)==1) {
      newrow <- dummy_row
      newrow$date <- as.character(format.Date(missing_dates[i], '%d/%m/%Y'))
      newrow$day <- as.character(format.Date(missing_dates[i], '%d'))
      newrow$month <- as.character(format.Date(missing_dates[i], '%m'))
      newrow$year <- as.character(format.Date(missing_dates[i], '%Y'))
      existingDF <- rbind(cimis_all_reformatted[1:fix_indices[i], ], newrow, cimis_all_reformatted[(fix_indices[i]+1):nrow(cimis_all_reformatted), ])
      assign("existingDF", existingDF, envir = .GlobalEnv)
    } else if(i==1){
      print('In if')
      newrow <- dummy_row
      newrow$date <- as.character(format.Date(missing_dates[i], '%d/%m/%Y'))
      newrow$day <- as.character(format.Date(missing_dates[i], '%d'))
      newrow$month <- as.character(format.Date(missing_dates[i], '%m'))
      newrow$year <- as.character(format.Date(missing_dates[i], '%Y'))
      existingDF <- rbind(cimis_all_reformatted[1:fix_indices[i], ], newrow)
      assign("existingDF", existingDF, envir = .GlobalEnv)
  } else if(i==length(missing_dates)) {
      print('In else if')
      newrow <- dummy_row
      newrow$date <- as.character(format.Date(missing_dates[i], '%d/%m/%Y'))
      newrow$day <- as.character(format.Date(missing_dates[i], '%d'))
      newrow$month <- as.character(format.Date(missing_dates[i], '%m'))
      newrow$year <- as.character(format.Date(missing_dates[i], '%Y'))
      existingDF <- if((fix_indices[i] - fix_indices[i-1])==1) { rbind(existingDF, newrow, cimis_all_reformatted[fix_indices[i]:nrow(cimis_all_reformatted), ])} else{rbind(existingDF, cimis_all_reformatted[(fix_indices[i-1]+1):fix_indices[i], ], newrow, cimis_all_reformatted[(fix_indices[i]+1):nrow(cimis_all_reformatted), ])}
      assign("existingDF", existingDF, envir = .GlobalEnv)
    } else {
      print('In else')  
      newrow <- dummy_row
      newrow$date <- as.character(format.Date(missing_dates[i], '%d/%m/%Y'))
      newrow$day <- as.character(format.Date(missing_dates[i], '%d'))
      newrow$month <- as.character(format.Date(missing_dates[i], '%m'))
      newrow$year <- as.character(format.Date(missing_dates[i], '%Y'))
      existingDF <- rbind(existingDF, cimis_all_reformatted[(fix_indices[i-1]+1):fix_indices[i], ], newrow)
      assign("existingDF", existingDF, envir = .GlobalEnv)
    }
  # existingDF
}, simplify = FALSE)
dim(existingDF)
tail(existingDF)
row.names(existingDF) <- 1:nrow(existingDF)
fix_indices
existingDF[1550:1555,]
existingDF[5934:5936,]
cimis_all_reformatted <- existingDF
rm(existingDF, fix_indices)

length(cimis_all_reformatted$date)
length(model_dates)
cimis_all_reformatted[10400:10500,]

#now add missing Belridge data 13/06/2012 - 11/09/2013 [temperature data was problematic]
# belridge_raw <- cimis_please(station=146, start_date = '2012-06-13', end_date = '2013-09-11')
# write.csv(belridge_raw, file.path(workDir, subDir, 'Belridge_2012_2013cimisRAW.csv'), row.names = FALSE)
# belridge <- reformat_cimis(belridge_raw)
# head(belridge) #has a couple of zeros in min temp
# tail(belridge)

#use Delano station 182 instead
# delano_raw <- cimis_please(station=182, start_date = '2012-06-13', end_date = '2013-09-11')
# write.csv(delano_raw, file.path(workDir, subDir, 'Delano_2012_2013cimisRAW.csv'), row.names = FALSE)
delano_raw <- read.csv(file.path(workDir, subDir, 'Delano_2012_2013cimisRAW.csv'), stringsAsFactors = FALSE)
delano <- reformat_cimis(delano_raw)
lapply(delano, summary) #no missing precip
sum(delano$maxTemp_C - delano$minTemp_C <= 0)
head(delano)
tail(delano)
plot(as.Date(delano$date, format = '%d/%m/%Y'), delano$minTemp_C, type = 'l')
lines(as.Date(delano$date, format = '%d/%m/%Y'), delano$maxTemp_C, col='red')

cimis_all_reformatted <- rbind(cimis_all_reformatted[1:match('12/06/2012', cimis_all_reformatted$date),], delano, cimis_all_reformatted[match('12/09/2013', cimis_all_reformatted$date):nrow(cimis_all_reformatted),])
dim(cimis_all_reformatted)
length(model_dates)

lapply(cimis_all_reformatted, class)
lapply(cimis_all_reformatted, summary) #87 missing precip

#read-in prism precip
list.files(file.path(workDir, subDir))
prism_precip <- read.csv(file.path(workDir, subDir, 'PRISM_ppt_stable_4km_19831001_20210430_35.5326_-119.2818.csv'), stringsAsFactors = FALSE)
head(prism_precip)
sum(prism_precip$ppt..mm.) #6861
sum(cimis_all_reformatted$precip_mm, na.rm = TRUE)


#QC fix
#https://www.wrh.noaa.gov/hnx/fat/normals/fat01nrm.htm climatic normals and records for Fresno, CA to establish reasonable bounds for CIMIS station
#https://wrcc.dri.edu/cgi-bin/clilcd.pl?ca23232 for Sacramento area: 18-115 F

cimis_all_reformatted_final <- cimis_QC_fix(input_df=cimis_all_reformatted, critMinTemp=-11)
head(cimis_all_reformatted_final)
lapply(cimis_all_reformatted_final[,2:7], summary)
sum(cimis_all_reformatted_final$maxTemp_C - cimis_all_reformatted_final$minTemp_C <= 0)
cimis_all_reformatted_final[cimis_all_reformatted_final$maxTemp_C - cimis_all_reformatted_final$minTemp_C <= 0,]
plot(cimis_all_reformatted_final$solRad_MJ_m2_d, cimis_all_reformatted_final$maxTemp_C)
plot(cimis_all_reformatted_final$solRad_MJ_m2_d, cimis_all_reformatted_final$minTemp_C)
lapply(cimis_all_reformatted_final[,2:7], function(x) plot(as.Date(cimis_all_reformatted_final$date, '%d/%m/%Y'), x, type = 'l'))

#replace precip data from 3/2/2004 - 10/3/2005 with nearby station data as a long series of zeroes was reported during this time period which includes the winter of 2004-05
#use Delano station 182 instead
delano_raw <- cimis_please(station=182, start_date = '2004-03-02', end_date = '2005-10-3')
write.csv(delano_raw, file.path(workDir, subDir, 'Delano_2004_2005cimisRAW.csv'), row.names = FALSE)
delano_raw <- read.csv(file.path(workDir, subDir, 'Delano_2004_2005cimisRAW.csv'), stringsAsFactors = FALSE)
delano <- reformat_cimis(delano_raw)
lapply(delano, summary)
sum(delano$precip_mm)

sum(cimis_all_reformatted_final$precip_mm[which(cimis_all_reformatted_final$date=='02/03/2004'):which(cimis_all_reformatted_final$date=='03/10/2005')]) #last day was suspect and delano confirmed no precip on that day
cimis_all_reformatted_final$precip_mm[which(cimis_all_reformatted_final$date=='02/03/2004'):which(cimis_all_reformatted_final$date=='03/10/2005')] <- delano$precip_mm


#write data to file for use in RZWQM
#project for creating meteorology file is in: C:\Users\smdevine\Desktop\post doc\Dahlke\RZWQM\projects\PulseSoilClimate\InitialTest_v2\Parlier_1983_2021test
write.csv(cimis_all_reformatted_final, file.path(workDir, subDir, paste0(met_stn, '_1983_2021cimisQC.csv')), row.names = FALSE)
tail(cimis_all_reformatted_final)

writeClipboard(as.character(cimis_all_reformatted_final$date))
writeClipboard(as.character(cimis_all_reformatted_final$minTemp_C))
writeClipboard(as.character(cimis_all_reformatted_final$maxTemp_C))
writeClipboard(as.character(cimis_all_reformatted_final$windRun_km_d))
writeClipboard(as.character(cimis_all_reformatted_final$solRad_MJ_m2_d))
writeClipboard(as.character(cimis_all_reformatted_final$relHumidity))
writeClipboard(as.character(cimis_all_reformatted_final$precip_mm))
# writeClipboard(c('15/04/1984', '15/04/1985'))
#breakpoint file creation assumptions
#duration: 480 minutes; intensity: 0.05 (min) - 5.0 (max)


#read-in data to identify wet years
cimis_all_reformatted_final <- read.csv(file.path(workDir, subDir, paste0(met_stn, '_1983_2021cimisQC.csv')), stringsAsFactors = FALSE)
sum(cimis_all_reformatted_final$precip_mm)
precip_by_year <- data.frame(precip_mm=tapply(cimis_all_reformatted_final$precip_mm, cimis_all_reformatted_final$year, sum))
write.csv(precip_by_year, file.path(workDir, subDir, 'precip_by_year.csv'), row.names = TRUE)
precip_by_year[order(precip_by_year$precip_mm, decreasing = TRUE), ]
wet_years <- row.names(precip_by_year)[order(precip_by_year[,1], decreasing = TRUE)][1:10]
wet_years <- as.integer(wet_years)
wet_years
write.csv(data.frame(year=wet_years), file.path(workDir, subDir, 'wettest_ten_years.csv'), row.names = FALSE)
rm(wet_years)

#look at precip by year and month
precip_by_month <- tapply(cimis_all_reformatted_final$precip_mm, list(cimis_all_reformatted_final$year, cimis_all_reformatted_final$month), sum)
write.csv(precip_by_month, file.path(workDir, subDir, 'precip_by_month.csv'), row.names = TRUE)

#calc wettest water years
cimis_all_reformatted_final$WY <- ifelse(cimis_all_reformatted_final$month %in% c('10', '11', '12'), as.character(as.integer(cimis_all_reformatted_final$year)+1), cimis_all_reformatted_final$year)
precip_by_WY <- data.frame(precip_mm=tapply(cimis_all_reformatted_final$precip_mm, cimis_all_reformatted_final$WY, sum))
precip_by_WY[order(precip_by_WY$precip_mm, decreasing = TRUE), ]
write.csv(precip_by_WY, file.path(workDir, subDir, 'precip_by_WY.csv'), row.names = TRUE)
wet_years <- row.names(precip_by_WY)[order(precip_by_WY[,1], decreasing = TRUE)][1:10]
wet_years <- as.integer(wet_years)
wet_years
write.csv(data.frame(year=wet_years), file.path(workDir, subDir, 'wettest_ten_years_WY.csv'), row.names = FALSE)
rm(wet_years)
