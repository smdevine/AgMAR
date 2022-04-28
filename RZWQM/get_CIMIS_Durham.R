library(cimir)
set_key(key = '4a7d3dc2-7431-46d1-9734-f28365113e68') #obtained through CIMIS web site account details page
is_key_set()
workDir <- 'C:/Users/smdevine/Desktop/post doc/Dahlke/RZWQM/cimis'
subDir <- 'Durham_Stn12'
stn_no <- 12
met_stn <- 'Durham'

#get metadata about station
stn_metadata <- cimis_station(station=stn_no)
stn_metadata <- as.data.frame(stn_metadata)
stn_metadata$HmsLatitude
stn_metadata$HmsLongitude
stn_metadata$Elevation
stn_metadata$ConnectDate[1]
dir.create(file.path(workDir, subDir))
write.csv(stn_metadata, file.path(workDir, subDir, 'stn_metadata.csv'), row.names=FALSE)
stn_metadata <- read.csv(file.path(workDir, subDir, 'stn_metadata.csv'))
stn_metadata$HmsLatitude
stn_metadata$HmsLongitude



#get all durham data
cimis_all <- do.call(rbind, mapply(cimis_please, start_date=c('1983-10-01', '1988-01-01', '1992-01-01', '1996-01-01', '2000-01-01', '2004-01-01', '2008-01-01', '2012-01-01', '2016-01-01', '2020-01-01'), end_date=c('1987-12-30', '1991-12-31', '1995-12-31', '1999-12-31', '2003-12-31', '2007-12-31', '2011-12-31', '2015-12-31', '2019-12-31', '2021-04-30'), MoreArgs = list(station=stn_no), SIMPLIFY = FALSE))
dim(cimis_all)
colnames(cimis_all)
sum(is.na(cimis_all$Value))
sum(cimis_all$Value==' ', na.rm = TRUE)
write.csv(cimis_all, file.path(workDir, subDir, paste0(met_stn, '_1983_2021cimisRAW.csv')), row.names = FALSE)

#read-in raw data
cimis_all <- read.csv(file.path(workDir, subDir, paste0(met_stn, '_1983_2021cimisRAW.csv')), stringsAsFactors = FALSE)

cimis_all_reformatted <- reformat_cimis(cimis_all)
dim(cimis_all_reformatted) #missing one day
head(cimis_all_reformatted)
tail(cimis_all_reformatted)
lapply(cimis_all_reformatted[,2:7], summary) #missing 132 precip days, which can be filled with PRISM

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

dummy_row <- cimis_all_reformatted[1,]
dummy_row <- as.data.frame(sapply(dummy_row, function(x) {x <- NA}, simplify=FALSE))

#fix still needed for consecutive missing dates other than last date that falls into "else if" statement
sapply(1:length(missing_dates), function(i) { #
  print(i)
  existingDF <- 
    if(length(missing_dates==1)) {
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
existingDF[1550:1555,]
cimis_all_reformatted <- existingDF
rm(existingDF)

#read-in prism precip to fill missing P days
list.files(file.path(workDir, subDir))
prism_precip <- read.csv(file.path(workDir, subDir, 'PRISM_ppt_stable_4km_19831001_20210430_39.6086_-121.8244.csv'), stringsAsFactors = FALSE)
head(prism_precip)
sum(prism_precip$ppt..mm.) #25942.73
sum(cimis_all_reformatted$precip_mm, na.rm = TRUE) #21133.6

#QC fix
#https://www.wrh.noaa.gov/hnx/fat/normals/fat01nrm.htm climatic normals and records for Fresno, CA to establish reasonable bounds for CIMIS station
#https://wrcc.dri.edu/cgi-bin/clilcd.pl?ca23232 for Sacramento area: 18-115 F

#mas P of 100
cimis_all_reformatted_final <- cimis_QC_fix(input_df=cimis_all_reformatted, critMinTemp=-11, maxP = 100)
head(cimis_all_reformatted_final)
lapply(cimis_all_reformatted_final[,2:7], summary)
sum(cimis_all_reformatted_final$maxTemp_C - cimis_all_reformatted_final$minTemp_C <= 0)
cimis_all_reformatted_final[cimis_all_reformatted_final$maxTemp_C - cimis_all_reformatted_final$minTemp_C <= 0,]
#fix this date by running function again on MinTemp
cimis_all_reformatted_final$minTemp_C[cimis_all_reformatted_final$maxTemp_C - cimis_all_reformatted_final$minTemp_C <= 0] <- NA
cimis_all_reformatted_final$minTemp_C[is.na(cimis_all_reformatted_final$minTemp_C)] <- calc_daily_mean(cimis_all_reformatted_final, cimis_all_reformatted_final$minTemp_C)[[1]]

plot(cimis_all_reformatted_final$solRad_MJ_m2_d, cimis_all_reformatted_final$maxTemp_C)
lapply(cimis_all_reformatted_final[,2:7], function(x) plot(as.Date(cimis_all_reformatted_final$date, '%d/%m/%Y'), x, type = 'l'))


#write data to file for use in RZWQM
#project for creating meteorology file is in: C:\Users\smdevine\Desktop\post doc\Dahlke\RZWQM\projects\PulseSoilClimate\InitialTest_v2\Parlier_1983_2021test
write.csv(cimis_all_reformatted_final, file.path(workDir, subDir, paste0(met_stn, '_1983_2021cimisQC.csv')), row.names = FALSE)
cimis_all_reformatted_final <- read.csv(file.path(workDir, subDir, paste0(met_stn, '_1983_2021cimisQC.csv')), stringsAsFactors = FALSE)

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
cimis_all_reformatted_final$precip_mm
precip_by_year <- data.frame(precip_mm=tapply(cimis_all_reformatted_final$precip_mm, cimis_all_reformatted_final$year, sum))
precip_by_year[order(precip_by_year$precip_mm, decreasing = TRUE), ]
write.csv(precip_by_year, file.path(workDir, subDir, 'precip_by_year.csv'), row.names = TRUE)
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
