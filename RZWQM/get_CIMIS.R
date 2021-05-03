library(cimir)
set_key(key = '4a7d3dc2-7431-46d1-9734-f28365113e68') #obtained through CIMIS web site account details page
is_key_set()


workDir <- 'C:/Users/smdevine/Desktop/post doc/Dahlke/RZWQM/cimis'

parlier_2016_17 <- cimis_data(targets = 39, start.date = '2016-10-01', end.date = '2017-04-30', items = c('day-air-tmp-min', 'day-air-tmp-max', 'day-wind-spd-avg', 'day-sol-rad-avg', 'day-rel-hum-avg', 'day-precip'), measure.unit='M')
class(parlier_2016_17)
dim(parlier_2016_17)
parlier_2016_17 <- as.data.frame(parlier_2016_17)
colnames(parlier_2016_17)
parlier_2016_17 <- parlier_2016_17[,c(4:5,10:ncol(parlier_2016_17))]

dir.create(file.path(workDir, 'Parlier_Stn39'))
write.csv(parlier_2016_17, file.path(workDir, 'Parlier_Stn39', 'parlier_2016_17WY_cimis.csv'), row.names = FALSE)

#get metadata about station
stn39_metadata <- cimis_station(station=39)
stn39_metadata <- as.data.frame(stn39_metadata)
stn39_metadata$HmsLatitude
stn39_metadata$HmsLongitude
stn39_metadata$Elevation
334/3.2808
write.csv(stn39_metadata, file.path(workDir, 'Parlier_Stn39', 'stn_metadata.csv'), row.names=FALSE)

unique(parlier_2016_17$Item)
parlier_2016_17[parlier_2016_17$Qc=='M',]
#create subsets of data 
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

#look at QC flag meaning
flag_meaning <- function(flag) {
  cimis_flags()[cimis_flags()$Flag==flag,]
}
lapply(unique(parlier_2016_17$Qc), flag_meaning)
