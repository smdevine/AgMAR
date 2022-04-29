library(cimir)
set_key(key = '4a7d3dc2-7431-46d1-9734-f28365113e68') #obtained through CIMIS web site account details page
is_key_set()
subDir <- 'Parlier_Stn39'
stn_no <- 39
met_stn <- 'Parlier'
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




#get all parlier data
parlier_all <- do.call(rbind, mapply(cimis_please, start_date=c('1983-10-01', '1988-01-01', '1992-01-01', '1996-01-01', '2000-01-01', '2004-01-01', '2008-01-01', '2012-01-01', '2016-01-01', '2020-01-01'), end_date=c('1987-12-30', '1991-12-31', '1995-12-31', '1999-12-31', '2003-12-31', '2007-12-31', '2011-12-31', '2015-12-31', '2019-12-31', '2021-04-30'), MoreArgs = list(station=39), SIMPLIFY = FALSE))
dim(parlier_all)
colnames(parlier_all)
sum(is.na(parlier_all$Value))
sum(parlier_all$Value==' ', na.rm = TRUE)
write.csv(parlier_all, file.path(workDir, 'Parlier_Stn39', 'parlier_1983_2021cimisRAW.csv'), row.names = FALSE)

parlier_all <- read.csv(file.path(workDir, 'Parlier_Stn39', 'parlier_1983_2021cimisRAW.csv'), stringsAsFactors = FALSE)

parlier_all_reformatted <- reformat_cimis(parlier_all)
dim(parlier_all_reformatted)
head(parlier_all_reformatted)
tail(parlier_all_reformatted)
lapply(parlier_all_reformatted[,2:7], summary) #54 missing precip days

#temp fix to add missing Dec 31, 1987
parlier_all_reformatted[1552:1555,]
dec31_1987 <- parlier_all_reformatted[1552,]
dec31_1987$date <- '31/12/1987'
dec31_1987$day <- '31'
dec31_1987[,2:7] <- lapply(parlier_all_reformatted[,2:7], function(x) {mean(x[parlier_all_reformatted$day=='31' & parlier_all_reformatted$month=='12'])})
dec31_1987
parlier_all_reformatted <- rbind(parlier_all_reformatted[1:1552,], dec31_1987, parlier_all_reformatted[1553:nrow(parlier_all_reformatted),])
dim(parlier_all_reformatted)

#read-in prism precip
#read-in prism precip
list.files(file.path(workDir, subDir))
prism_precip <- read.csv(file.path(workDir, subDir, 'PRISM_ppt_stable_4km_19831001_20210430_36.5974_-119.5040.csv'), stringsAsFactors = FALSE)
head(prism_precip)
sum(prism_precip$ppt..mm.) #10002.13
sum(parlier_all_reformatted$precip_mm, na.rm = TRUE) #10297.8

#QC fix
#https://www.wrh.noaa.gov/hnx/fat/normals/fat01nrm.htm climatic normals and records for Fresno, CA to establish reasonable bounds for CIMIS station
parlier_all_reformatted_final <- cimis_QC_fix(input_df=parlier_all_reformatted, critMinTemp = -11)
head(parlier_all_reformatted_final)
lapply(parlier_all_reformatted_final[,2:7], summary)
sum(parlier_all_reformatted_final$maxTemp_C - parlier_all_reformatted_final$minTemp_C < 0)
parlier_all_reformatted_final[parlier_all_reformatted_final$maxTemp_C - parlier_all_reformatted_final$minTemp_C < 0,]
plot(parlier_all_reformatted_final$solRad_MJ_m2_d, parlier_all_reformatted_final$maxTemp_C)
plot(parlier_all_reformatted_final$solRad_MJ_m2_d, parlier_all_reformatted_final$maxTemp_C)
lapply(parlier_all_reformatted_final[,2:7], function(x) plot(as.Date(parlier_all_reformatted_final$date, '%d/%m/%Y'), x, type = 'l')) #rel humidity wonky in 90s
plot(as.Date(parlier_all_reformatted_final$date, '%d/%m/%Y')[1:5000], parlier_all_reformatted_final$relHumidity[1:5000], type='l')


parlier_all_reformatted_final$relHumidity <- fixit_finale(parlier_all_reformatted_final, 'relHumidity', c(1983:1989, 1995:2021), critMin = 0, critMax = 100, startDate = '01/01/1990', endDate = '31/12/1994', digits = 0)
plot(as.Date(parlier_all_reformatted_final$date, '%d/%m/%Y'), parlier_all_reformatted_final$relHumidity, type='l')
sum(parlier_all_reformatted_final$precip_mm) #10319.13

#lookat precip
plot(as.Date(parlier_all_reformatted_final$date, '%d/%m/%Y')[1:5000], parlier_all_reformatted_final$precip_mm[1:5000], type='l')
plot(as.Date(parlier_all_reformatted_final$date, '%d/%m/%Y')[5001:10000], parlier_all_reformatted_final$precip_mm[5001:10000], type='l')
plot(as.Date(parlier_all_reformatted_final$date, '%d/%m/%Y')[10001:nrow(parlier_all_reformatted_final)], parlier_all_reformatted_final$precip_mm[10001:nrow(parlier_all_reformatted_final)], type='l')

#fix 2010 growing season precip problems identified manually
parlier_all_reformatted_final$precip_mm[which(parlier_all_reformatted_final$date=='11/12/1995'):which(parlier_all_reformatted_final$date=='31/03/1996')] <-
  prism_precip$ppt..mm.[which(prism_precip$Date=='12/11/1995'):which(prism_precip$Date=='3/31/1996')]

#write data to file for use in RZWQM
write.csv(parlier_all_reformatted_final, file.path(workDir, 'Parlier_Stn39', 'parlier_1983_2021cimisQC.csv'), row.names = FALSE)

#read-in data to identify wet years
parlier_all_reformatted_final <- read.csv(file.path(workDir, 'Parlier_Stn39', 'parlier_1983_2021cimisQC.csv'), stringsAsFactors = FALSE)
writeClipboard(as.character(parlier_all_reformatted_final$date))
writeClipboard(as.character(parlier_all_reformatted_final$minTemp_C))
writeClipboard(as.character(parlier_all_reformatted_final$maxTemp_C))
writeClipboard(as.character(parlier_all_reformatted_final$windRun_km_d))
writeClipboard(as.character(parlier_all_reformatted_final$solRad_MJ_m2_d))
writeClipboard(as.character(parlier_all_reformatted_final$relHumidity))
writeClipboard(as.character(parlier_all_reformatted_final$precip_mm))
writeClipboard(c('15/04/1984', '15/04/1985'))


parlier_all_reformatted_final$precip_mm
precip_by_year <- data.frame(precip_mm=tapply(parlier_all_reformatted_final$precip_mm, parlier_all_reformatted_final$year, sum))
write.csv(precip_by_year, file.path(workDir, 'Parlier_Stn39', 'precip_by_year.csv'), row.names = TRUE)
precip_by_year[order(precip_by_year$precip_mm, decreasing = TRUE), ]
wet_years <- row.names(precip_by_year)[order(precip_by_year[,1], decreasing = TRUE)][1:10]
wet_years <- as.integer(wet_years)
write.csv(data.frame(year=wet_years), file.path(workDir, 'Parlier_Stn39', 'wettest_ten_years.csv'), row.names = FALSE)

#look at precip by year and month
precip_by_month <- tapply(parlier_all_reformatted_final$precip_mm, list(parlier_all_reformatted_final$year, parlier_all_reformatted_final$month), sum)
write.csv(precip_by_month, file.path(workDir, subDir, 'precip_by_month.csv'), row.names = TRUE)

#calc wettest water years
parlier_all_reformatted_final$WY <- ifelse(parlier_all_reformatted_final$month %in% c('10', '11', '12'), as.character(as.integer(parlier_all_reformatted_final$year)+1), parlier_all_reformatted_final$year)
precip_by_WY <- data.frame(precip_mm=tapply(parlier_all_reformatted_final$precip_mm, parlier_all_reformatted_final$WY, sum))
precip_by_WY[order(precip_by_WY$precip_mm, decreasing = TRUE), ]
write.csv(precip_by_WY, file.path(workDir, subDir, 'precip_by_WY.csv'), row.names = TRUE)
wet_years <- row.names(precip_by_WY)[order(precip_by_WY[,1], decreasing = TRUE)][1:10]
wet_years <- as.integer(wet_years)
wet_years
write.csv(data.frame(year=wet_years), file.path(workDir, subDir, 'wettest_ten_years_WY.csv'), row.names = FALSE)
rm(wet_years)



#old attempt to apply Flood-MAR through precip
add_floodMAR <- function(df, years, month, days, application) {
  df$precip_mm[df$year %in% wet_years & df$month==month & df$day %in% days] <- df$precip_mm[df$year %in% wet_years & df$month==month & df$day %in% days] + application
  df$precip_mm
}
sum(parlier_all_reformatted_final$precip_mm) #10298.7
precip_JanAgMar <- add_floodMAR(parlier_all_reformatted_final, years = wet_years, month = 1, days = c(15,19,23,27), application = 150)
sum(precip_JanAgMar) #16298.7
precip_FebAgMar <- add_floodMAR(parlier_all_reformatted_final, years = wet_years, month = 2, days = c(15,19,23,27), application = 150)
sum(precip_FebAgMar)
precip_MarAgMar <- add_floodMAR(parlier_all_reformatted_final, years = wet_years, month = 3, days = c(15,19,23,27), application = 150)
sum(precip_MarAgMar)

#1/15, 2/5, 2/26, 3/19
add_floodMAR_LF <- function(df, years, dates, application) {
  dates_final <- do.call(c, lapply(c(dates), function(x) paste(x, wet_years, sep = '/')))
  df$precip_mm[df$date %in% dates_final] <- df$precip_mm[df$date %in% dates_final] + application
  df$precip_mm
}
precip_AgMar_LF_early <- add_floodMAR_LF(parlier_all_reformatted_final, dates = c('15/01', '05/02', '26/02', '19/03'), application = 150)
sum(precip_AgMar_LF_early)

precip_AgMar_LF_mid <- add_floodMAR_LF(parlier_all_reformatted_final, dates = c('25/01', '15/02', '08/03', '29/03'), application = 150)
sum(precip_AgMar_LF_mid)
precip_AgMar_LF_late <- add_floodMAR_LF(parlier_all_reformatted_final, dates = c('04/02', '25/02', '18/03', '08/04'), application = 150)
sum(precip_AgMar_LF_late)


AgMar_precip <- data.frame(day=parlier_all_reformatted_final$day, month=parlier_all_reformatted_final$month, year=parlier_all_reformatted_final$year, JanAgMAR_mm=precip_JanAgMar, FebAgMAR_mm=precip_FebAgMar, MarAgMAR_mm=precip_MarAgMar, LF_early_AgMAR=precip_AgMar_LF_early, LF_mid_AgMAR=precip_AgMar_LF_mid, LF_late_AgMAR=precip_AgMar_LF_late)
write.csv(AgMar_precip, file.path(workDir, 'Parlier_Stn39', 'Palier_synthetic_precip_AgMAR.csv'), row.names = FALSE)
AgMar_precip <- read.csv(file.path(workDir, 'Parlier_Stn39', 'Palier_synthetic_precip_AgMAR.csv'))
writeClipboard(as.character(AgMar_precip$JanAgMAR_mm))


