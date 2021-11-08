library(cimir)
set_key(key = '4a7d3dc2-7431-46d1-9734-f28365113e68') #obtained through CIMIS web site account details page
is_key_set()


workDir <- 'C:/Users/smdevine/Desktop/post doc/Dahlke/RZWQM/cimis'

#get metadata about station
stn39_metadata <- cimis_station(station=39)
stn39_metadata <- as.data.frame(stn39_metadata)
stn39_metadata$HmsLatitude
stn39_metadata$HmsLongitude
stn39_metadata$Elevation
334/3.2808
stn39_metadata$ConnectDate[1]
# write.csv(stn39_metadata, file.path(workDir, 'Parlier_Stn39', 'stn_metadata.csv'), row.names=FALSE)

#get 2016-17 data for first test
parlier_2016_17 <- cimis_data(targets = 39, start.date = '2016-10-01', end.date = '2017-04-30', items = c('day-air-tmp-min', 'day-air-tmp-max', 'day-wind-spd-avg', 'day-sol-rad-avg', 'day-rel-hum-avg', 'day-precip'), measure.unit='M')
class(parlier_2016_17)
dim(parlier_2016_17)
parlier_2016_17 <- as.data.frame(parlier_2016_17)
colnames(parlier_2016_17)
parlier_2016_17 <- parlier_2016_17[,c(4:5,10:ncol(parlier_2016_17))]

dir.create(file.path(workDir, 'Parlier_Stn39'))
write.csv(parlier_2016_17, file.path(workDir, 'Parlier_Stn39', 'parlier_2016_17WY_cimis.csv'), row.names = FALSE)

#look at QC flag meaning
flag_meaning <- function(flag) {
  cimis_flags()[cimis_flags()$Flag==flag,]
}
lapply(unique(parlier_all$Qc), flag_meaning)

#get all parlier data
cimis_please <- function(station, start_date, end_date) {
  stn_data <- cimis_data(targets = station, start.date = start_date, end.date = end_date, items = c('day-air-tmp-min', 'day-air-tmp-max', 'day-wind-spd-avg', 'day-sol-rad-avg', 'day-rel-hum-avg', 'day-precip'), measure.unit='M')
  stn_data <- as.data.frame(stn_data)
  stn_data <- stn_data[,c(4:5,10:ncol(stn_data))]
  stn_data
}
parlier_all <- do.call(rbind, mapply(cimis_please, start_date=c('1983-10-01', '1988-01-01', '1992-01-01', '1996-01-01', '2000-01-01', '2004-01-01', '2008-01-01', '2012-01-01', '2016-01-01', '2020-01-01'), end_date=c('1987-12-30', '1991-12-31', '1995-12-31', '1999-12-31', '2003-12-31', '2007-12-31', '2011-12-31', '2015-12-31', '2019-12-31', '2021-04-30'), MoreArgs = list(station=39), SIMPLIFY = FALSE))
dim(parlier_all)
colnames(parlier_all)
sum(is.na(parlier_all$Value))
sum(parlier_all$Value==' ', na.rm = TRUE)
write.csv(parlier_all, file.path(workDir, 'Parlier_Stn39', 'parlier_1983_2021cimisRAW.csv'), row.names = FALSE)

unique(parlier_2016_17$Item)
parlier_2016_17[parlier_2016_17$Qc=='M',]
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
parlier_all_reformatted <- reformat_cimis(parlier_all)
head(parlier_all_reformatted)
tail(parlier_all_reformatted)
lapply(parlier_all_reformatted[,2:7], summary)

#QC fix
#https://www.wrh.noaa.gov/hnx/fat/normals/fat01nrm.htm climatic normals and records for Fresno, CA to establish reasonable bounds for CIMIS station
which(parlier_all_reformatted$maxTemp_C > 48)
parlier_all_reformatted$maxTemp_C[245:255]
parlier_all_reformatted$maxTemp_C[795:815]
parlier_all_reformatted$maxTemp_C[1905:1925]


sum(parlier_all_reformatted$minTemp_C < -7, na.rm = TRUE)
which(parlier_all_reformatted$minTemp_C < -5)
parlier_all_reformatted$minTemp_C[140:160]
parlier_all_reformatted[745:815,c('date', 'minTemp_C')]

summary(parlier_all_reformatted$windRun_km_d)
sum(parlier_all_reformatted$windRun_km_d > 450, na.rm=TRUE)
which(parlier_all_reformatted$windRun_km_d > 450)
parlier_all_reformatted$windRun_km_d[200:220]
#700 km day-1 equates to 18 mph wind for 24 hours 

summary(parlier_all_reformatted$solRad_MJ_m2_d)
sum(parlier_all_reformatted$solRad_MJ_m2_d < 0, na.rm = TRUE) #11
sum(parlier_all_reformatted$solRad_MJ_m2_d==0, na.rm = TRUE) #3
sum(parlier_all_reformatted$solRad_MJ_m2_d < 0.1, na.rm = TRUE)
quantile(parlier_all_reformatted$solRad_MJ_m2_d, 0.001, na.rm = TRUE)
quantile(parlier_all_reformatted$solRad_MJ_m2_d, 0.999, na.rm = TRUE)
sum(parlier_all_reformatted$solRad_MJ_m2_d>37.8, na.rm = TRUE)

which(parlier_all_reformatted$solRad_MJ_m2_d < 0)
parlier_all_reformatted$solRad_MJ_m2_d[35:45]
parlier_all_reformatted$solRad_MJ_m2_d[780:800]
parlier_all_reformatted$solRad_MJ_m2_d[2700:2720]


test <- parlier_all_reformatted$minTemp_C
summary(test)
test[which(test < -7)] <- NA

# df <- parlier_all_reformatted
# df_vector <- parlier_all_reformatted$minTemp_C
calc_daily_mean <- function(df, df_vector) {
  NA_indices <- which(is.na(df_vector))
  result <- sapply(NA_indices, function(x) {
    mean(df_vector[df$day==df$day[x] & df$month==df$month[x]], na.rm = TRUE)})
  list(result, NA_indices)
}
test <- calc_daily_mean(parlier_all_reformatted, parlier_all_reformatted$minTemp_C)  

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

parlier_all_reformatted_final <- cimis_QC_fix(df=parlier_all_reformatted)
head(parlier_all_reformatted_final)
lapply(parlier_all_reformatted_final[,2:7], summary)
sum(parlier_all_reformatted_final$maxTemp_C - parlier_all_reformatted_final$minTemp_C < 0)
parlier_all_reformatted_final[parlier_all_reformatted_final$maxTemp_C - parlier_all_reformatted_final$minTemp_C < 0,]
plot(parlier_all_reformatted_final$solRad_MJ_m2_d, parlier_all_reformatted_final$maxTemp_C)
plot(parlier_all_reformatted_final$solRad_MJ_m2_d, parlier_all_reformatted_final$maxTemp_C)
lapply(parlier_all_reformatted_final[,2:7], function(x) plot(as.Date(parlier_all_reformatted_final$date, '%d/%m/%Y'), x, type = 'l')) #rel humidity wonky in 90s
plot(as.Date(parlier_all_reformatted_final$date, '%d/%m/%Y')[1:5000], parlier_all_reformatted_final$relHumidity[1:5000], type='l')

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

parlier_all_reformatted_final$relHumidity <- fixit_finale(parlier_all_reformatted_final, 'relHumidity', c(1983:1989, 1995:2021), critMin = 0, critMax = 100, startDate = '01/01/1990', endDate = '31/12/1994', digits = 0)
plot(as.Date(parlier_all_reformatted_final$date, '%d/%m/%Y'), parlier_all_reformatted_final$relHumidity, type='l')

#temp fix to add missing Dec 31, 1987
parlier_all_reformatted_final[1552:1555,]
dec31_1987 <- parlier_all_reformatted_final[1552,]
dec31_1987$date <- '31/12/1987'
dec31_1987$day <- '31'
dec31_1987[,2:7] <- lapply(parlier_all_reformatted_final[,2:7], function(x) {mean(x[parlier_all_reformatted_final$day=='31' & parlier_all_reformatted_final$month=='12'])})
dec31_1987
parlier_all_reformatted_final <- rbind(parlier_all_reformatted_final[1:1552,], dec31_1987, parlier_all_reformatted_final[1553:nrow(parlier_all_reformatted_final),])


#write data to file for use in RZWQM
write.csv(parlier_all_reformatted_final, file.path(workDir, 'Parlier_Stn39', 'parlier_1983_2021cimisQC.csv'), row.names = FALSE)
writeClipboard(as.character(parlier_all_reformatted_final$date))
writeClipboard(as.character(parlier_all_reformatted_final$minTemp_C))
writeClipboard(as.character(parlier_all_reformatted_final$maxTemp_C))
writeClipboard(as.character(parlier_all_reformatted_final$windRun_km_d))
writeClipboard(as.character(parlier_all_reformatted_final$solRad_MJ_m2_d))
writeClipboard(as.character(parlier_all_reformatted_final$relHumidity))
writeClipboard(as.character(parlier_all_reformatted_final$precip_mm))

writeClipboard(c('15/04/1984', '15/04/1985'))
#now do this for other CIMIS stations

#orginal climate test dataset production
#combine 2016-17 read-in
#min temps
parlier_min_temp <- parlier_2016_17[parlier_2016_17$Item=='DayAirTmpMin',]
head(parlier_min_temp)
summary(parlier_min_temp$Value)
parlier_min_temp[parlier_min_temp$Qc!=' ',]
plot(parlier_min_temp$Date, parlier_min_temp$Value)

#max temp
parlier_max_temp <- parlier_2016_17[parlier_2016_17$Item=='DayAirTmpMax',]
head(parlier_max_temp)
summary(parlier_max_temp$Value)
parlier_max_temp[parlier_max_temp$Qc!=' ',]
summary(parlier_max_temp$Value - parlier_min_temp$Value)

plot(parlier_max_temp$Date, parlier_max_temp$Value, type = 'l', col='red', ylim = c(-1,33))
lines(parlier_min_temp$Date, parlier_min_temp$Value, col='blue')


#wind
parlier_wind <-  parlier_2016_17[parlier_2016_17$Item=='DayWindSpdAvg',]
head(parlier_wind)
summary(parlier_wind$Value)
parlier_wind[parlier_wind$Qc!=' ',]
plot(parlier_wind$Date, parlier_wind$Value, type = 'l', col='grey')
parlier_wind$wind_run <- parlier_wind$Value * (60 * 60 * 24 / 1000) #convert to km / day

#radiation
294*(60 * 60 * 24 / 10^6) #should be 25.4 MJ/m2
parlier_sol <- parlier_2016_17[parlier_2016_17$Item=='DaySolRadAvg',]
parlier_sol[parlier_sol$Qc!=' ',]
head(parlier_sol)
parlier_sol$MJ_m2_day <- parlier_sol$Value *(60 * 60 * 24 / 10^6)
plot(parlier_sol$Date, parlier_sol$MJ_m2_day, type = 'l', col='orange')
summary(parlier_sol$MJ_m2_day)

#precip
parlier_precip <- parlier_2016_17[parlier_2016_17$Item=='DayPrecip',]
parlier_precip[parlier_precip$Qc!=' ',]
summary(parlier_precip$Value)
plot(parlier_precip$Date, parlier_precip$Value, type = 's')

#rel. humidiy
parlier_relhum <- parlier_2016_17[parlier_2016_17$Item=='DayRelHumAvg',]
summary(parlier_relhum$Value)
plot(parlier_relhum$Date, parlier_relhum$Value, type='l', col='lightblue2')
parlier_relhum[parlier_relhum$Qc!=' ',] #5th index
#temporary gap fix; make function in future
parlier_relhum$Value[is.na(parlier_relhum$Value)] <- round(mean(parlier_relhum$Value[1:(which(is.na(parlier_relhum$Value))+7)], na.rm=TRUE), 0)


format.Date(parlier_min_temp$Date, '%d/%m/%Y')
parlier_2016_17_reformatted <- data.frame(date=format.Date(parlier_min_temp$Date, '%d/%m/%Y'), minTemp=parlier_min_temp$Value, maxTemp=parlier_max_temp$Value, windRun=parlier_wind$wind_run, solRad=parlier_sol$MJ_m2_day, precip=parlier_precip$Value, relHumidiay=parlier_relhum$Value)
class(parlier_2016_17_reformatted$date)
write.csv(parlier_2016_17_reformatted, file.path(workDir, 'Parlier_Stn39', 'parlier_2016_17_reformatted.csv'), row.names = FALSE)