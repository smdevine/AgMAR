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

# df <- cimis_all_reformatted
# df_vector <- cimis_all_reformatted$minTemp_C
calc_daily_mean <- function(df, df_vector) {
  NA_indices <- which(is.na(df_vector))
  result <- sapply(NA_indices, function(x) {
    mean(df_vector[df$day==df$day[x] & df$month==df$month[x]], na.rm = TRUE)})
  list(result, NA_indices)
}
# test <- calc_daily_mean(cimis_all_reformatted, cimis_all_reformatted$minTemp_C)

cimis_QC_fix <- function(input_df, critMaxTemp=50, critMinTemp=-7, critWind=700, critSolLow=0.1, critSolHigh=35, critRHLow=0, critRHHigh=100, minP=0, maxP=100, critSD=3.5) {
  input_df$maxTemp_C[which(input_df$maxTemp_C > critMaxTemp)] <- NA
  input_df$maxTemp_C[which(input_df$maxTemp_C < critMinTemp)] <- NA
  input_df$minTemp_C[which(input_df$minTemp_C > critMaxTemp)] <- NA
  input_df$minTemp_C[which(input_df$minTemp_C < critMinTemp)] <- NA
  input_df$windRun_km_d[which(input_df$windRun_km_d > critWind)] <- NA 
  input_df$solRad_MJ_m2_d[which(input_df$solRad_MJ_m2_d < critSolLow)] <- NA
  input_df$solRad_MJ_m2_d[which(input_df$solRad_MJ_m2_d > critSolHigh)] <- NA
  input_df$relHumidity[which(input_df$relHumidity < critRHLow)] <- NA
  input_df$relHumidity[which(input_df$relHumidity > critRHHigh)] <- NA
  maxTemp_fixes <- calc_daily_mean(input_df, input_df$maxTemp_C)
  input_df$maxTemp_C[maxTemp_fixes[[2]]] <- round(maxTemp_fixes[[1]], 1)
  minTemp_fixes <- calc_daily_mean(input_df, input_df$minTemp_C)
  input_df$minTemp_C[minTemp_fixes[[2]]] <- round(minTemp_fixes[[1]], 1)
  windRun_fixes <- calc_daily_mean(input_df, input_df$windRun_km_d)
  input_df$windRun_km_d[windRun_fixes[[2]]] <- round(windRun_fixes[[1]], 2)
  solRad_fixes <- calc_daily_mean(input_df, input_df$solRad_MJ_m2_d)
  input_df$solRad_MJ_m2_d[solRad_fixes[[2]]] <- round(solRad_fixes[[1]], 4)
  RH_fixes <- calc_daily_mean(input_df, input_df$relHumidity)
  input_df$relHumidity[RH_fixes[[2]]] <- round(RH_fixes[[1]], 0)
  input_df$precip_mm[which(input_df$precip_mm < minP)] <- NA
  input_df$precip_mm[which(input_df$precip_mm > maxP)] <- NA
  if(length(input_df$precip_mm)==length(prism_precip$ppt..mm.)) {
    input_df$precip_mm[is.na(input_df$precip_mm)] <- prism_precip$ppt..mm.[is.na(input_df$precip_mm)] #gap fill using prism precip data for the station location
  } else{print('Length of PRISM dataset does not match length of CIMIS dataset!')}
  temp_fix_indices <- which(input_df$maxTemp_C - input_df$minTemp_C <= 0) #changed to <= on 3/15/22
  if(length(temp_fix_indices) != 0) {
    input_df$maxTemp_C[temp_fix_indices] <- NA
    input_df$minTemp_C[temp_fix_indices] <- NA
    maxTemp_fixes <- calc_daily_mean(input_df, input_df$maxTemp_C)
    input_df$maxTemp_C[maxTemp_fixes[[2]]] <- round(maxTemp_fixes[[1]], 1)
    minTemp_fixes <- calc_daily_mean(input_df, input_df$minTemp_C)
    input_df$minTemp_C[minTemp_fixes[[2]]] <- round(minTemp_fixes[[1]], 1)
  }
  maxtemp_mean <- tapply(input_df$maxTemp_C, list(input_df$month, input_df$day), mean)
  maxtemp_sd <- tapply(input_df$maxTemp_C, list(input_df$month, input_df$day), sd)
  maxtemp_devs <- sapply(1:length(input_df$maxTemp_C), function(i) {
    abs(input_df$maxTemp_C[i] - maxtemp_mean[input_df$month[i], input_df$day[i]])/maxtemp_sd[input_df$month[i], input_df$day[i]]
  })
  mintemp_mean <- tapply(input_df$minTemp_C, list(input_df$month, input_df$day), mean)
  mintemp_sd <- tapply(input_df$minTemp_C, list(input_df$month, input_df$day), sd)
  mintemp_devs <- sapply(1:length(input_df$minTemp_C), function(i) {
    abs(input_df$minTemp_C[i] - mintemp_mean[input_df$month[i], input_df$day[i]])/mintemp_sd[input_df$month[i], input_df$day[i]]
  })
  print(paste(sum(maxtemp_devs > critSD), 'max temp. observations exceeded', critSD, 'standard deviations'))
  print(paste(sum(mintemp_devs > critSD), 'min temp. observations exceeded', critSD, 'standard deviations'))
  input_df$maxTemp_C[maxtemp_devs > critSD] <- NA
  input_df$minTemp_C[mintemp_devs > critSD] <- NA
  if(sum(maxtemp_devs > critSD) > 0) {
    maxTemp_fixes <- calc_daily_mean(input_df, input_df$maxTemp_C)
    input_df$maxTemp_C[maxTemp_fixes[[2]]] <- round(maxTemp_fixes[[1]], 1)
  }
  if(sum(mintemp_devs > critSD)) {
    minTemp_fixes <- calc_daily_mean(input_df, input_df$minTemp_C)
    input_df$minTemp_C[minTemp_fixes[[2]]] <- round(minTemp_fixes[[1]], 1)
  }
  input_df
}

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
