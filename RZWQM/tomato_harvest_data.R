workDir <- 'C:/Users/smdevine/Desktop/post doc/Dahlke/RZWQM/crop_data'
list.files(workDir)
tom_df <- read.csv(file.path(workDir, 'tomato_harvests_CenturyExp.csv'), stringsAsFactors = FALSE)
head(tom_df)
colnames(tom_df)
tom_df <- tom_df[,1:11]
tom_df$date <- as.Date(tom_df$date, '%m/%d/%Y')
tom_df$year <- format.Date(tom_df$date, '%Y')
tom_df <- tom_df[order(tom_df$system_name, tom_df$date),]
mean(tom_df$tomato_mh_dry_yield_Mg.ha, na.rm = TRUE)
mean(tom_df$tomato_mh_fresh_yield_Mg.ha) #71.74845
mean(tom_df$tomato_fruit_moisture_., na.rm = TRUE) #93.52985
tom_df$total_aboveground_dry <- tom_df$tomato_hh_vine_dry_yield_Mg.ha + tom_df$tomato_mh_dry_yield_Mg.ha 
mean(tom_df$total_aboveground_dry, na.rm = TRUE) #8.24
mean(tom_df$tomato_mh_dry_yield_Mg.ha, na.rm = TRUE) / mean(tom_df$total_aboveground_dry, na.rm = TRUE) #0.5592537
tapply(tom_df$tomato_mh_dry_yield_Mg.ha, tom_df$system_name, summary)
tapply(tom_df$tomato_mh_fresh_yield_Mg.ha, tom_df$system_name, summary)
tapply(tom_df$tomato_mh_fresh_yield_Mg.ha, tom_df$year, summary)
tapply(tom_df$tomato_mh_dry_yield_Mg.ha, tom_df$year, summary)
tapply(tom_df$total_aboveground_dry, tom_df$year, summary)
