library(cimir)
set_key(key = '4a7d3dc2-7431-46d1-9734-f28365113e68') #obtained through CIMIS web site account details page
is_key_set()
workDir <- 'C:/Users/smdevine/Desktop/post doc/Dahlke/RZWQM/cimis'
subDir <- 'Davis_Stn6'
met_stn <- 'Davis'
stn_no <- 6

#get metadata about station
stn6_metadata <- cimis_station(station=6)
stn6_metadata <- as.data.frame(stn6_metadata)
stn6_metadata$HmsLatitude
stn6_metadata$HmsLongitude
stn6_metadata$Elevation
stn6_metadata$ConnectDate[1]
write.csv(stn6_metadata, file.path(workDir, subDir, 'stn_metadata.csv'), row.names=FALSE)

#get cimis data
cimis_all <- do.call(rbind, mapply(cimis_please, start_date=c('1983-10-01', '1988-01-01', '1992-01-01', '1996-01-01', '2000-01-01', '2004-01-01', '2008-01-01', '2012-01-01', '2016-01-01', '2020-01-01'), end_date=c('1987-12-30', '1991-12-31', '1995-12-31', '1999-12-31', '2003-12-31', '2007-12-31', '2011-12-31', '2015-12-31', '2019-12-31', '2021-04-30'), MoreArgs = list(station=6), SIMPLIFY = FALSE))
dim(cimis_all)
colnames(cimis_all)
sum(is.na(cimis_all$Value))
sum(cimis_all$Value==' ', na.rm = TRUE)
write.csv(cimis_all, file.path(workDir, subDir, 'Davis_1983_2021cimisRAW.csv'), row.names = FALSE)

cimis_all <- read.csv(file.path(workDir, subDir, 'Davis_1983_2021cimisRAW.csv'), stringsAsFactors = FALSE)

cimis_all_reformatted <- reformat_cimis(cimis_all)
head(cimis_all_reformatted)
tail(cimis_all_reformatted)
lapply(cimis_all_reformatted[,2:7], summary) #missing 63 precip days

#fill precip gaps with PRISM
list.files(file.path(workDir, subDir))
prism_precip <- read.csv(file.path(workDir, subDir, 'PRISM_ppt_stable_4km_19831001_20210430_38.5357_-121.7764.csv'), stringsAsFactors = FALSE)
head(prism_precip)
dim(prism_precip)
dim(cimis_all_reformatted)
sum(prism_precip$ppt..mm.) #6861
sum(cimis_all_reformatted$precip_mm, na.rm = TRUE)

#insert missing data on 12/31/
index <- 1548:1557
cimis_all_reformatted[index,]
missing_data <- data.frame(date='31/12/1987', minTemp_C=mean(cimis_all_reformatted$minTemp_C[index]), maxTemp_C=mean(cimis_all_reformatted$maxTemp_C[index]), windRun_km_d=mean(cimis_all_reformatted$windRun_km_d[index]), solRad_MJ_m2_d=mean(cimis_all_reformatted$solRad_MJ_m2_d[index]), precip_mm=NA, relHumidity=mean(cimis_all_reformatted$relHumidity[index]), day='31', month='12', year='1987')
cimis_all_reformatted <- rbind(cimis_all_reformatted[1:1552,], missing_data, cimis_all_reformatted[1553:nrow(cimis_all_reformatted),])
rm(index, missing_data)
#QC fix
#https://www.wrh.noaa.gov/hnx/fat/normals/fat01nrm.htm climatic normals and records for Fresno, CA to establish reasonable bounds for CIMIS station
#https://wrcc.dri.edu/cgi-bin/clilcd.pl?ca23232 for Sacramento area: 18-115 F
cimis_all_reformatted_final <- cimis_QC_fix(input_df=cimis_all_reformatted, critMinTemp=-11)
head(cimis_all_reformatted_final)
lapply(cimis_all_reformatted_final[,2:7], summary)
sum(cimis_all_reformatted_final$maxTemp_C - cimis_all_reformatted_final$minTemp_C <= 0)
cimis_all_reformatted_final[cimis_all_reformatted_final$maxTemp_C - cimis_all_reformatted_final$minTemp_C < 0,]
plot(cimis_all_reformatted_final$solRad_MJ_m2_d, cimis_all_reformatted_final$maxTemp_C)
lapply(cimis_all_reformatted_final[,2:7], function(x) plot(as.Date(cimis_all_reformatted_final$date, '%d/%m/%Y'), x, type = 'l'))

#write data to file for use in RZWQM
#project for creating meteorology file is in: C:\Users\smdevine\Desktop\post doc\Dahlke\RZWQM\projects\PulseSoilClimate\InitialTest_v2\Parlier_1983_2021test
write.csv(cimis_all_reformatted_final, file.path(workDir, subDir, 'Davis_1983_2021cimisQC.csv'), row.names = FALSE)
cimis_all_reformatted_final <- read.csv(file.path(workDir, subDir, 'Davis_1983_2021cimisQC.csv'), stringsAsFactors = FALSE)
lapply(cimis_all_reformatted_final, summary)
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
cimis_all_reformatted_final <- read.csv(file.path(workDir, subDir, 'Davis_1983_2021cimisQC.csv'), stringsAsFactors = FALSE)
cimis_all_reformatted_final$precip_mm
precip_by_year <- data.frame(precip_mm=tapply(cimis_all_reformatted_final$precip_mm, cimis_all_reformatted_final$year, sum))
precip_by_year[order(precip_by_year$precip_mm, decreasing = TRUE), ]
write.csv(precip_by_year, file.path(workDir, subDir, 'precip_by_year.csv'), row.names = TRUE)
wet_years <- row.names(precip_by_year)[order(precip_by_year[,1], decreasing = TRUE)][1:10]
wet_years <- as.integer(wet_years)
wet_years
write.csv(data.frame(year=wet_years), file.path(workDir, subDir, 'wettest_ten_years.csv'), row.names = FALSE)
rm(wet_years)
wetyrs[order(wetyrs)]

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
