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

#look at QC flag meaning
flag_meaning <- function(flag) {
  cimis_flags()[cimis_flags()$Flag==flag,]
}

#get all parlier data
cimis_please <- function(station, start_date, end_date) {
  stn_data <- cimis_data(targets = station, start.date = start_date, end.date = end_date, items = c('day-air-tmp-min', 'day-air-tmp-max', 'day-wind-spd-avg', 'day-sol-rad-avg', 'day-rel-hum-avg', 'day-precip'), measure.unit='M')
  stn_data <- as.data.frame(stn_data)
  stn_data <- stn_data[,c(4:5,10:ncol(stn_data))]
  stn_data
}

cimis_all <- do.call(rbind, mapply(cimis_please, start_date=c('1983-10-01', '1988-01-01', '1992-01-01', '1996-01-01', '2000-01-01', '2004-01-01', '2008-01-01', '2012-01-01', '2016-01-01', '2020-01-01'), end_date=c('1987-12-30', '1991-12-31', '1995-12-31', '1999-12-31', '2003-12-31', '2007-12-31', '2011-12-31', '2015-12-31', '2019-12-31', '2021-04-30'), MoreArgs = list(station=stn_no), SIMPLIFY = FALSE))
dim(cimis_all)
colnames(cimis_all)
sum(is.na(cimis_all$Value))
sum(cimis_all$Value==' ', na.rm = TRUE)
write.csv(cimis_all, file.path(workDir, subDir, paste0(met_stn, '_1983_2021cimisRAW.csv')), row.names = FALSE)



#create subsets of data 
reformat_cimis <- function(df) {
  df_min_temp <- df[df$Item=='DayAirTmpMin',]
  df_max_temp <- df[df$Item=='DayAirTmpMax',]
  df_wind <- df[df$Item=='DayWindSpdAvg',]
  df_wind$wind_run <- df_wind$Value * (60 * 60 * 24 / 1000)
  df_sol <- df[df$Item=='DaySolRadAvg',]
  df_sol$MJ_m2_day <- df_sol$Value *(60 * 60 * 24 / 10^6)
  df_precip <- df[df$Item=='DayPrecip',]
  df_rel_hum <- df[df$Item=='DayRelHumAvg',]
  df <- data.frame(date=format.Date(df_min_temp$Date, '%d/%m/%Y'), minTemp_C=df_min_temp$Value, maxTemp_C=df_max_temp$Value, windRun_km_d=df_wind$wind_run, solRad_MJ_m2_d=df_sol$MJ_m2_day, precip_mm=df_precip$Value, relHumidity=df_rel_hum$Value)
  df$day <- sapply(df$date, function(x) {unlist(strsplit(x, '/'))[1]})
  df$month <- sapply(df$date, function(x) {unlist(strsplit(x, '/'))[2]})
  df$year <- sapply(df$date, function(x) {unlist(strsplit(x, '/'))[3]})
  df
}
#read-in raw data
cimis_all <- read.csv(file.path(workDir, subDir, paste0(met_stn, '_1983_2021cimisRAW.csv')), stringsAsFactors = FALSE)

cimis_all_reformatted <- reformat_cimis(cimis_all)
dim(cimis_all_reformatted) 
head(cimis_all_reformatted)
tail(cimis_all_reformatted)
lapply(cimis_all_reformatted[,2:7], summary)

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


#QC fix
#https://www.wrh.noaa.gov/hnx/fat/normals/fat01nrm.htm climatic normals and records for Fresno, CA to establish reasonable bounds for CIMIS station
#https://wrcc.dri.edu/cgi-bin/clilcd.pl?ca23232 for Sacramento area: 18-115 F
which(cimis_all_reformatted$maxTemp_C > 48)
cimis_all_reformatted$maxTemp_C[2410:2420]
cimis_all_reformatted$maxTemp_C[2630:2650]

sum(cimis_all_reformatted$minTemp_C < -7, na.rm = TRUE)
which(cimis_all_reformatted$minTemp_C < -5)
cimis_all_reformatted$minTemp_C[140:160]
cimis_all_reformatted[745:815,c('date', 'minTemp_C')]

summary(cimis_all_reformatted$windRun_km_d)
sum(cimis_all_reformatted$windRun_km_d > 450, na.rm=TRUE)
which(cimis_all_reformatted$windRun_km_d > 450)
cimis_all_reformatted$windRun_km_d[200:220]
#700 km day-1 equates to 18 mph wind for 24 hours 

summary(cimis_all_reformatted$solRad_MJ_m2_d)
sum(cimis_all_reformatted$solRad_MJ_m2_d < 0, na.rm = TRUE) #11
sum(cimis_all_reformatted$solRad_MJ_m2_d==0, na.rm = TRUE) #3
sum(cimis_all_reformatted$solRad_MJ_m2_d < 0.1, na.rm = TRUE)
quantile(cimis_all_reformatted$solRad_MJ_m2_d, 0.001, na.rm = TRUE)
quantile(cimis_all_reformatted$solRad_MJ_m2_d, 0.999, na.rm = TRUE)
sum(cimis_all_reformatted$solRad_MJ_m2_d>37.8, na.rm = TRUE)

which(cimis_all_reformatted$solRad_MJ_m2_d < 0)
cimis_all_reformatted$solRad_MJ_m2_d[35:45]
cimis_all_reformatted$solRad_MJ_m2_d[780:800]
cimis_all_reformatted$solRad_MJ_m2_d[2700:2720]


test <- cimis_all_reformatted$minTemp_C
summary(test)
test[which(test < -7)] <- NA

# df <- cimis_all_reformatted
# df_vector <- cimis_all_reformatted$minTemp_C
calc_daily_mean <- function(df, df_vector) {
  NA_indices <- which(is.na(df_vector))
  result <- sapply(NA_indices, function(x) {
    mean(df_vector[df$day==df$day[x] & df$month==df$month[x]], na.rm = TRUE)})
  list(result, NA_indices)
}
# test <- calc_daily_mean(cimis_all_reformatted, cimis_all_reformatted$minTemp_C)  

cimis_QC_fix <- function(df, critMaxTemp=50, critMinTemp=-7, critWind=700, critSolLow=0.1, critSolHigh=35, critRHLow=0, critRHHigh=100, minP=0, maxP=100) {
  df$maxTemp_C[which(df$maxTemp_C > critMaxTemp)] <- NA
  df$maxTemp_C[which(df$maxTemp_C < critMinTemp)] <- NA
  df$minTemp_C[which(df$minTemp_C > critMaxTemp)] <- NA
  df$minTemp_C[which(df$minTemp_C < critMinTemp)] <- NA
  df$windRun_km_d[which(df$windRun_km_d > critWind)] <- NA 
  df$solRad_MJ_m2_d[which(df$solRad_MJ_m2_d < critSolLow)] <- NA
  df$solRad_MJ_m2_d[which(df$solRad_MJ_m2_d > critSolHigh)] <- NA
  df$relHumidity[which(df$relHumidity < critRHLow)] <- NA
  df$relHumidity[which(df$relHumidity > critRHHigh)] <- NA
  maxTemp_fixes <- calc_daily_mean(df, df$maxTemp_C)
  df$maxTemp_C[maxTemp_fixes[[2]]] <- round(maxTemp_fixes[[1]], 1)
  minTemp_fixes <- calc_daily_mean(df, df$minTemp_C)
  df$minTemp_C[minTemp_fixes[[2]]] <- round(minTemp_fixes[[1]], 1)
  windRun_fixes <- calc_daily_mean(df, df$windRun_km_d)
  df$windRun_km_d[windRun_fixes[[2]]] <- round(windRun_fixes[[1]], 2)
  solRad_fixes <- calc_daily_mean(df, df$solRad_MJ_m2_d)
  df$solRad_MJ_m2_d[solRad_fixes[[2]]] <- round(solRad_fixes[[1]], 4)
  RH_fixes <- calc_daily_mean(df, df$relHumidity)
  df$relHumidity[RH_fixes[[2]]] <- round(RH_fixes[[1]], 0)
  df$precip_mm[which(df$precip_mm < minP)] <- NA
  df$precip_mm[which(df$precip_mm > maxP)] <- NA
  df$precip_mm[is.na(df$precip_mm)] <- 0 #convert NA precip to 0
  temp_fix_indices <- which(df$maxTemp_C - df$minTemp_C < 0)
  df$maxTemp_C[temp_fix_indices] <- NA
  df$minTemp_C[temp_fix_indices] <- NA
  maxTemp_fixes <- calc_daily_mean(df, df$maxTemp_C)
  df$maxTemp_C[maxTemp_fixes[[2]]] <- round(maxTemp_fixes[[1]], 1)
  minTemp_fixes <- calc_daily_mean(df, df$minTemp_C)
  df$minTemp_C[minTemp_fixes[[2]]] <- round(minTemp_fixes[[1]], 1)
  df
}


cimis_all_reformatted_final <- cimis_QC_fix(df=cimis_all_reformatted, critMinTemp=-11)
head(cimis_all_reformatted_final)
lapply(cimis_all_reformatted_final[,2:7], summary)
sum(cimis_all_reformatted_final$maxTemp_C - cimis_all_reformatted_final$minTemp_C <= 0)
cimis_all_reformatted_final[cimis_all_reformatted_final$maxTemp_C - cimis_all_reformatted_final$minTemp_C <= 0,]
plot(cimis_all_reformatted_final$solRad_MJ_m2_d, cimis_all_reformatted_final$maxTemp_C)
lapply(cimis_all_reformatted_final[,2:7], function(x) plot(as.Date(cimis_all_reformatted_final$date, '%d/%m/%Y'), x, type = 'l'))


#fix major data problems
fixit_finale <- function(df, colname, years, critMin, critMax, startDate, endDate, digits) {
  df_good <- df[df$year %in% years, ]
  daily_means <- tapply(df_good[[colname]], list(df_good$day, df_good$month), mean, na.rm=TRUE)
  daily_sds <- tapply(df_good[[colname]], list(df_good$day, df_good$month), sd, na.rm=TRUE)
  startFix <- which(df$date==startDate)
  stopFix <- which(df$date==endDate)
  result <- df[[colname]]
  result[startFix:stopFix] <- sapply(startFix:stopFix, function(x) {round(rnorm(n=1, mean=daily_means[df$day[x], df$month[x]], sd=daily_sds[df$day[x], df$month[x]]), digits = digits)})
  result[which(result < critMin)] <- NA
  result[which(result > critMax)] <- NA
  fixes <- calc_daily_mean(df, result)
  result[fixes[[2]]] <- round(fixes[[1]], digits = digits)
  result
}



table(cimis_all_reformatted_final$year)[table(cimis_all_reformatted_final$year) < 365]


#insert missing data (now taken care of above)
# index <- 1548:1557
# missing_data <- data.frame(date='31/12/1987', minTemp_C=mean(cimis_all_reformatted_final$minTemp_C[index]), maxTemp_C=mean(cimis_all_reformatted_final$maxTemp_C[index]), windRun_km_d=mean(cimis_all_reformatted_final$windRun_km_d[index]), solRad_MJ_m2_d=mean(cimis_all_reformatted_final$solRad_MJ_m2_d[index]), precip_mm=mean(cimis_all_reformatted_final$precip_mm[index]), relHumidity=mean(cimis_all_reformatted_final$relHumidity[index]), day='31', month='12', year='1987')
# cimis_all_reformatted_final <- rbind(cimis_all_reformatted_final[1:1552,], missing_data, cimis_all_reformatted_final[1553:nrow(cimis_all_reformatted_final),])
# rm(index, missing_data)

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
# cimis_all_reformatted_final <- read.csv(file.path(workDir, subDir, 'Davis_1983_2021cimisQC.csv'), stringsAsFactors = FALSE)
cimis_all_reformatted_final$precip_mm
precip_by_year <- data.frame(precip_mm=tapply(cimis_all_reformatted_final$precip_mm, cimis_all_reformatted_final$year, sum))
precip_by_year[order(precip_by_year$precip_mm, decreasing = TRUE), ]
wet_years <- row.names(precip_by_year)[order(precip_by_year[,1], decreasing = TRUE)][1:10]
wet_years <- as.integer(wet_years)
wet_years
write.csv(data.frame(year=wet_years), file.path(workDir, subDir, 'wettest_ten_years.csv'), row.names = FALSE)
rm(wet_years)
