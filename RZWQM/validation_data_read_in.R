#core ID is incorrect
#missing NO3 data for all Terranova samples
#finish rules at line 71
library(soilDB)
library(aqp)
BD_assumption <- 1.5
#functions to aggregate soil data
wtd.mean <- function(x, y) {
  # use horizon thickness as a weight
  thick <- x$bottom_cm - x$top_cm
  # compute the weighted mean, accounting for the possibility of missing data
  m <- weighted.mean(horizons(x)[[y]], w=thick, na.rm=TRUE)
  m
}
data_depth <- function(x, varname) {
  thick <- x$bottom_cm - x$top_cm
  sum(thick[!is.na(x[[varname]])])
}
horizon_to_comp_simple <- function(horizon_SPC, depth, vars_of_interest = c('clay', 'silt', 'sand', 'NO3_ppm', 'NH4_ppm', 'TDN_ppm', 'pH', 'ec'), varnames = c('percent_clay', 'percent_silt', 'percent_sand', 'nitrate_ppm', 'ammonium_ppm', 'total_dissolved_N_ppm', 'soil_pH', 'electrical conductivity')) {
  # columnames <- paste0(varnames, '_', depth, 'cm')
  columnames <- varnames
  print(cbind(vars_of_interest, columnames)) #show that it's all lined up
  assign("depth", depth, envir = .GlobalEnv) #this necessary because slice can't find the variable otherwise
  sliced_SPC <- slice(horizon_SPC, 0:(depth-1) ~ .) #depth was '0:depth' in previous version
  stopifnot(unique(sliced_SPC$pedon_key)==site(sliced_SPC)$pedon_key)
  for (i in seq_along(vars_of_interest)) {
    s <- site(sliced_SPC)
    s[[columnames[i]]] <- profileApply(sliced_SPC, FUN = wtd.mean, y=vars_of_interest[i])
    site(sliced_SPC) <- s
  }
  s <- site(sliced_SPC)
  s[['nitrate_depth']] <- profileApply(sliced_SPC, FUN=data_depth, varname='NO3_ppm')
  rm(depth, envir = .GlobalEnv) #because we had to put it there earlier
  # s$mukey <- comp_df$mukey[match(s$cokey, comp_df$cokey)]
  # s$comppct <- comp_df$comppct_r[match(s$cokey, comp_df$cokey)]
  s
}

workDir <- 'C:/Users/smdevine/Desktop/post doc/Dahlke/data from Kira'
list.files(workDir)
options(max.print = 10000)
soilcores <- read.csv(file.path(workDir, 'soil_core_NO3.csv'), stringsAsFactors = FALSE, na.strings = c('', 'not enough sample', 'not enough sample. ', 'not data', 'n/a', 'not sample recover', 'NA', 'NA ', '#VALUE!'))
head(soilcores)
colnames(soilcores)
lapply(soilcores, class)
soilcores$ec <- as.numeric(gsub(",", "", soilcores$ec))
unique(soilcores$pH)
lapply(soilcores, summary)
unique(soilcores$id)
unique(soilcores$site)
sum(is.na(soilcores$site))
soilcores$site[soilcores$site=='TNC '] <- 'TNC'
unique(soilcores$core_no)
# soilcores$core_id <- paste(soilcores$id, soilcores$core_no, sep='_')

#these rules were determined by manually perusing the excel file
soilcores$core_id <- soilcores$id
unique(soilcores$core_id) #65, which is incorrect

#for site==Tulare and field==Alfalfa
soilcores$core_id[which(soilcores$site=='Tulare' & soilcores$field=='Alfalfa')] <- paste(soilcores$site[which(soilcores$site=='Tulare' & soilcores$field=='Alfalfa')], soilcores$time[which(soilcores$site=='Tulare' & soilcores$field=='Alfalfa')], soilcores$treatment[which(soilcores$site=='Tulare' & soilcores$field=='Alfalfa')], soilcores$field[which(soilcores$site=='Tulare' & soilcores$field=='Alfalfa')], soilcores$id[which(soilcores$site=='Tulare' & soilcores$field=='Alfalfa')], sep = '_')

#for site==Kings Orchard and field==Pecan
soilcores$core_id[which(soilcores$site=='Kings Orchard' & soilcores$field=='Pecan')] <- soilcores$core_no[which(soilcores$site=='Kings Orchard' & soilcores$field=='Pecan')]

#for site=='TNC Colusa"
soilcores$core_id[which(soilcores$site=='TNC Colusa')] <- paste(soilcores$site[which(soilcores$site=='TNC Colusa')], soilcores$time[which(soilcores$site=='TNC Colusa')], soilcores$field[which(soilcores$site=='TNC Colusa')], soilcores$id[which(soilcores$site=='TNC Colusa')], soilcores$core_no[which(soilcores$site=='TNC Colusa')], sep = '_')

#for site=='TNC'
soilcores$core_id[which(soilcores$site=='TNC')] <- paste(soilcores$site[which(soilcores$site=='TNC')], soilcores$time[which(soilcores$site=='TNC')], soilcores$treatment[which(soilcores$site=='TNC')], soilcores$field[which(soilcores$site=='TNC')], soilcores$id[which(soilcores$site=='TNC')], soilcores$core_no[which(soilcores$site=='TNC')], sep = '_')

#change several id's based on alignment of depth, sample no, site, time, treatment, and field
soilcores$id[which(soilcores$id=='R26 T15 flood' & soilcores$sample==21 & soilcores$core_no==1)] <- 'R26 T33 flood' #should be 'R26 T33 flood'
soilcores$id[which(soilcores$id=='R4 T15 control' & soilcores$sample==58)] <- 'R4 T33 control' #should be R4 T33 control
soilcores$id[which(soilcores$id=='R4 T15 control' & soilcores$sample==59)] <- 'R4 T33 control'  #should be R4 T33 control

#for sites==Delhi, Orland, or Modesto
soilcores$core_id[which(soilcores$site=='Delhi' | soilcores$site=='Orland' | soilcores$site=='Modesto' | soilcores$site=='Modesto (Para)')] <- paste(soilcores$id[which(soilcores$site=='Delhi' | soilcores$site=='Orland' | soilcores$site=='Modesto' | soilcores$site=='Modesto (Para)')], soilcores$time[which(soilcores$site=='Delhi' | soilcores$site=='Orland' | soilcores$site=='Modesto'| soilcores$site=='Modesto (Para)')], sep = '_')

#for site==Tulare & field==Pistachio
soilcores$core_id[which(soilcores$site=='Tulare' & soilcores$field=='Pistachio')] <- paste(soilcores$id[which(soilcores$site=='Tulare' & soilcores$field=='Pistachio')], soilcores$time[which(soilcores$site=='Tulare' & soilcores$field=='Pistachio')], sep = '_')

#for id=='R15 T48 Flood' & sample 44:56 & time=='Before'
soilcores$core_id[which(soilcores$id=='R15 T48 Flood' & soilcores$sample %in% as.character(44:56) & soilcores$time=='Before')] <- paste(soilcores$id[which(soilcores$id=='R15 T48 Flood' & soilcores$sample %in% as.character(44:56) & soilcores$time=='Before')], soilcores$time[which(soilcores$id=='R15 T48 Flood' & soilcores$sample %in% as.character(44:56) & soilcores$time=='Before')], 'A', sep = '_')

#for id=='R15 T48 Flood' & sample 57:68
soilcores$core_id[which(soilcores$id=='R15 T48 Flood' & soilcores$sample %in% as.character(57:68))] <- paste(soilcores$id[which(soilcores$id=='R15 T48 Flood' & soilcores$sample %in% as.character(57:68))], soilcores$time[which(soilcores$id=='R15 T48 Flood' & soilcores$sample %in% as.character(57:68))], 'B', sep = '_')

#for id=='R15 T48 Flood' & sample 36:49 & time=='After'
soilcores$core_id[which(soilcores$id=='R15 T48 Flood' & soilcores$sample %in% as.character(36:49) & soilcores$time=='After')] <- paste(soilcores$id[which(soilcores$id=='R15 T48 Flood' & soilcores$sample %in% as.character(36:49) & soilcores$time=='After')], soilcores$time[which(soilcores$id=='R15 T48 Flood' & soilcores$sample %in% as.character(36:49) & soilcores$time=='After')], 'C', sep = '_')

#for id=='R25 T20 flood' & sample 110:127
soilcores$core_id[which(soilcores$id=='R25 T20 flood' & soilcores$sample %in% as.character(110:127))] <- paste(soilcores$id[which(soilcores$id=='R25 T20 flood' & soilcores$sample %in% as.character(110:127))], soilcores$time[which(soilcores$id=='R25 T20 flood' & soilcores$sample %in% as.character(110:127))], 'A', sep = '_')

#for id=='R25 T20 flood' & sample 190:203
soilcores$core_id[which(soilcores$id=='R25 T20 flood' & soilcores$sample %in% as.character(190:203))] <- paste(soilcores$id[which(soilcores$id=='R25 T20 flood' & soilcores$sample %in% as.character(190:203))], soilcores$time[which(soilcores$id=='R25 T20 flood' & soilcores$sample %in% as.character(190:203))], 'B', sep = '_')

#fix a few depths in these
soilcores$bottom_cm[which(soilcores$id=='R25 T20 flood' & soilcores$sample=='114')] <- 81.63 #was 76.53
soilcores$bottom_cm[which(soilcores$id=='R25 T20 flood' & soilcores$sample=='194')] <- 86.96 #was 81.52

#delete duplicate row
soilcores <- soilcores[-which(soilcores$id=='R25 T20 flood' & soilcores$sample=='201' & soilcores$core_no==2),]

#fix another depth
soilcores$top_cm[which(soilcores$id=='R25 T20 flood' & soilcores$sample=='201')] <- 196.81 #was 200

#delete row in NA depth
soilcores <- soilcores[-which(is.na(soilcores$bottom_cm)),]

#delete a few more unnecessary rows
soilcores <- soilcores[-which(soilcores$id=='R4 T15 control' & soilcores$sample==52 & soilcores$core_no==1),]
soilcores <- soilcores[-which(soilcores$id=='R4 T15 control' & soilcores$sample==53 & soilcores$core_no==1),]
soilcores <- soilcores[-which(soilcores$id=='R4 T15 control' & soilcores$sample==47 & soilcores$core_no==4),]

#fix last depth
soilcores$bottom_cm[which(soilcores$id=='R5 T30 Control' & soilcores$sample==141 & soilcores$core_no==3)] <- 300 #was 280

#paste(soilcores$site, soilcores$time, soilcores$treatment, soilcores$field, soilcores$year, soilcores$id, soilcores$core_no, sep = '_')
unique(soilcores$core_id) #152
soilcores <- soilcores[soilcores$site!='Terranova',] #all Terranova sites have missing N data
unique(soilcores$core_id) #147
soilcores[soilcores$core_id=='TNC Colusa_After_Pasture_West_ 1 (2) ',]
soilcores_spc <- soilcores

head(soilcores_spc)
depths(soilcores_spc) <- core_id ~ top_cm + bottom_cm
depth_ck <- checkHzDepthLogic(soilcores_spc)
head(depth_ck)
sum(depth_ck$valid) #143 initially
sum(!depth_ck$valid) #4 bad, all alfalfa from Tulare
depth_ck[depth_ck$valid==TRUE,]
depth_ck[depth_ck$valid==FALSE,]
for(i in 1:sum(!depth_ck$valid)) {
  print(depth_ck$core_id[depth_ck$valid==FALSE][i])
  print(soilcores[soilcores$core_id==depth_ck$core_id[depth_ck$valid==FALSE][i], c('top_cm', 'bottom_cm')])
}
incomplete_soilcores <- soilcores[soilcores$core_id %in% depth_ck$core_id[depth_ck$valid==FALSE],]
dim(incomplete_soilcores)
incomplete_soilcores <- incomplete_soilcores[order(incomplete_soilcores$core_id, incomplete_soilcores$top_cm), ]
incomplete_soilcores$depth_inconsistency <- c(0, incomplete_soilcores$bottom_cm[1:(length(incomplete_soilcores$top_cm)-1)] - incomplete_soilcores$top_cm[2:length(incomplete_soilcores$top_cm)])
summary(incomplete_soilcores$depth_inconsistency)
unique(incomplete_soilcores$site)
write.csv(incomplete_soilcores, file.path(workDir, 'intermediate results', 'incomplete_soilcores.csv'), row.names=FALSE)
#then made two manual changes to core id and three manual changes to depths, all noted in excel file
#added additional rules for core id naming conventions above
unique(soilcores$site[soilcores$core_id %in% depth_ck$core_id[depth_ck$valid==TRUE]])

#filter out incomplete cores
soilcores_complete <- soilcores[soilcores$core_id %in% depth_ck$core_id[depth_ck$valid==TRUE],]
checkSPC(soilcores_spc)
soil_depths <- profileApply(soilcores_spc, FUN = function(x) max(x$bottom_cm))
soil_surface <- profileApply(soilcores_spc, FUN = function(x) min(x$top_cm))
summary(soil_depths)
summary(soil_surface)
sum(soil_surface > 0)
soilcores_complete <- soilcores_complete[soilcores_complete$core_id != site(soilcores_spc)$core_id[soil_surface > 0],]
unique(soilcores_complete$core_id) #142

# soilcores_complete <- soilcores_complete[!is.na(soilcores_complete$NO3_ppm),]
summary(soilcores_complete$NO3_ppm)
hist(soilcores_complete$NO3_ppm)
sum(soilcores_complete$NO3_ppm < 0, na.rm = TRUE) #159
sum(soilcores_complete$NO3_ppm == 0, na.rm = TRUE) #227
soilcores_complete$NO3_ppm[which(soilcores_complete$NO3_ppm < 0)] <- 0
sum(soilcores_complete$NH4_ppm < 0, na.rm = TRUE)
soilcores_complete$NH4_ppm[which(soilcores_complete$NH4_ppm < 0)] <- 0
sum(soilcores_complete$TDN_ppm < 0, na.rm = TRUE)

summary(soilcores_complete$clay)
sum(soilcores_complete$clay < 0, na.rm = TRUE)
soilcores_complete[which(soilcores_complete$clay < 0),]
sum(soilcores_complete$silt > 100, na.rm = TRUE)
soilcores_complete$silt[which(soilcores_complete$silt > 100)] #assume this was an error of data entry (wrong decimal place)
soilcores_complete$silt[which(soilcores_complete$silt > 100)] <- soilcores_complete$silt[which(soilcores_complete$silt > 100)]/100
summary(soilcores_complete$silt[which(soilcores_complete$clay < 0)] + soilcores_complete$sand[which(soilcores_complete$clay < 0)])
soilcores_complete$silt[which(soilcores_complete$clay < 0)] <- 100 - soilcores_complete$sand[which(soilcores_complete$clay < 0)]
soilcores_complete$clay[which(soilcores_complete$clay < 0)] <- 0 #all other minor
sum(soilcores_complete$silt < 0, na.rm = TRUE)
soilcores_complete$silt[which(soilcores_complete$silt < 0)]
soilcores_complete$silt[which(soilcores_complete$silt < 0)] <- 0
sum(soilcores_complete$sand < 0, na.rm = TRUE) #0
sum(soilcores_complete$clay > 100, na.rm = TRUE)
sum(soilcores_complete$silt > 100, na.rm = TRUE)
sum(soilcores_complete$sand > 100, na.rm = TRUE)
soilcores_complete$sand[which(soilcores_complete$sand > 100)] <- 100
soilcores_complete$texture_check <- soilcores_complete$clay + soilcores_complete$silt + soilcores_complete$sand
summary(soilcores_complete$texture_check)
sum(soilcores_complete$texture_check > 100, na.rm=TRUE) #225
sum(soilcores_complete$texture_check > 101, na.rm = TRUE) #2

colnames(soilcores_complete)
unique(soilcores_complete$texture)
summary(soilcores_complete$clay)
sum(soilcores_complete$clay < 0, na.rm = TRUE) #24
summary(soilcores_complete$silt)
sum(soilcores_complete$silt < 0, na.rm = TRUE) #7
sum(soilcores_complete$silt > 100, na.rm = TRUE) #2
summary(soilcores_complete$sand)
#need to fix textures not adding to 100%

#check again
soilcores_spc <- soilcores_complete
depths(soilcores_spc) <- core_id ~ top_cm + bottom_cm
depth_ck <- checkHzDepthLogic(soilcores_spc)
head(depth_ck)
lapply(depth_ck, summary)
sum(depth_ck$valid) #now 142
sum(!depth_ck$valid) #0

#filter out incomplete and bad texture data
# soilcores_complete <- soilcores_complete[soilcores_complete$core_id %in% depth_ck$core_id[depth_ck$valid==TRUE],]
# dim(soilcores_complete) #1887
# soilcores_complete <- soilcores_complete[!is.na(soilcores_complete$clay) & !is.na(soilcores_complete$silt) & !is.na(soilcores_complete$sand), ]
# dim(soilcores_complete) #1884
# soilcores_complete <- soilcores_complete[soilcores_complete$clay >= 0 & soilcores_complete$clay <= 100, ]
# dim(soilcores_complete) #1862
# soilcores_complete <- soilcores_complete[soilcores_complete$silt >= 0 & soilcores_complete$silt <= 100, ]
# dim(soilcores_complete) #1860
# soilcores_complete <- soilcores_complete[soilcores_complete$sand >= 0 & soilcores_complete$sand <= 100, ]
# dim(soilcores_complete) #same

#and again
# soilcores_spc <- soilcores_complete
# depths(soilcores_spc) <- core_id ~ top_cm + bottom_cm
# depth_ck <- checkHzDepthLogic(soilcores_spc)
# head(depth_ck)
# depth_ck
# sum(depth_ck$valid) #289
# sum(!depth_ck$valid) #7

#filter out incomplete one more time
# soilcores_complete <- soilcores_complete[soilcores_complete$core_id %in% depth_ck$core_id[depth_ck$valid==TRUE],]
# soilcores_spc <- soilcores_complete
# depths(soilcores_spc) <- core_id ~ top_cm + bottom_cm
# depth_ck <- checkHzDepthLogic(soilcores_spc)
# head(depth_ck)
# dim(depth_ck)
# sum(depth_ck$valid) #289
# sum(!depth_ck$valid) #0
# lapply(depth_ck, summary)

#aggregate by soil profile
soil_depths <- profileApply(soilcores_spc, FUN = function(x) max(x$bottom_cm))
soil_surface <- profileApply(soilcores_spc, FUN = function(x) min(x$top_cm))
summary(soil_surface)
soil_profile_summary <- horizon_to_comp_simple(horizon_SPC = soilcores_spc, depth = max(soil_depths))
head(soil_profile_summary)
all(names(soil_depths)==soil_profile_summary$core_id)
soil_profile_summary$total_depth <- soil_depths
summary(soil_profile_summary$nitrate_ppm)
summary(soil_profile_summary$nitrate_depth)
soil_profile_summary$kgNO3_ha <- soil_profile_summary$nitrate_ppm * BD_assumption * soil_profile_summary$nitrate_depth/10
summary(soil_profile_summary$kgNO3_ha)
hist(soil_profile_summary$kgNO3_ha)

hist(soil_profile_summary$nitrate_ppm)
hist(soil_profile_summary$percent_clay)
hist(soil_profile_summary$percent_silt)
plot(soil_profile_summary$percent_clay, soil_profile_summary$nitrate_ppm)
plot(soil_profile_summary$percent_clay, soil_profile_summary$kgNO3_ha)
plot(soil_profile_summary$nitrate_depth, soil_profile_summary$kgNO3_ha)
soil_profile_summary

plot(soil_profile_summary$percent_clay, soil_profile_summary$`electrical conductivity`)
plot(soil_profile_summary$percent_silt, soil_profile_summary$nitrate_ppm)
plot(soil_profile_summary$percent_clay, soil_profile_summary$ammonium_ppm)
plot(soil_profile_summary$total_depth, soil_profile_summary$nitrate_ppm)

#add relevant columns
soil_profile_summary$site <- soilcores_complete$site[match(soil_profile_summary$core_id, soilcores_complete$core_id)]
soil_profile_summary$field <- soilcores_complete$field[match(soil_profile_summary$core_id, soilcores_complete$core_id)]
soil_profile_summary$treatment <- soilcores_complete$treatment[match(soil_profile_summary$core_id, soilcores_complete$core_id)]
soil_profile_summary$time <- soilcores_complete$time[match(soil_profile_summary$core_id, soilcores_complete$core_id)]
table(soil_profile_summary$site)
table(soil_profile_summary$time)
unique(soil_profile_summary$time)
sum(is.na(soil_profile_summary$time)) #15 are NA
soil_profile_summary$time[which(soil_profile_summary$time=='Before ')] <- 'Before'
soil_profile_summary$core_id[is.na(soil_profile_summary$time)]
soil_profile_summary[is.na(soil_profile_summary$time),]
soil_profile_summary$year <- soilcores_complete$year[match(soil_profile_summary$core_id, soilcores_complete$core_id)]
soil_profile_summary[is.na(soil_profile_summary$time),]
soil_profile_summary$time[is.na(soil_profile_summary$time)] <- ifelse(soil_profile_summary$year[is.na(soil_profile_summary$time)]=='2019', 'Before', 'After')
table(soil_profile_summary$time)
sum(soil_profile_summary$nitrate_ppm > 10) #29
sum(soil_profile_summary$nitrate_ppm[soil_profile_summary$time=='Before'] > 10) #14
summary(soil_profile_summary$nitrate_ppm[soil_profile_summary$time=='Before'])
summary(soil_profile_summary$nitrate_ppm[soil_profile_summary$time=='After'])
summary(soil_profile_summary$kgNO3_ha[soil_profile_summary$time=='Before'])
summary(soil_profile_summary$kgNO3_ha[soil_profile_summary$time=='After'])
table(soil_profile_summary$field)
soil_profile_summary[soil_profile_summary$field=='Davis',]
soil_profile_summary[soil_profile_summary$field=='Gobel',]
soil_profile_summary[soil_profile_summary$field=='Lewis',]
soil_profile_summary$field[soil_profile_summary$field=='Almonds'] <- 'Almond'
table(soil_profile_summary$field)
table(soil_profile_summary$treatment)
sum(is.na(soil_profile_summary$treatment)) #13
soil_profile_summary[which(soil_profile_summary$treatment=='high'),] #all alfalfa
soil_profile_summary[which(soil_profile_summary$treatment=='low'),]
soil_profile_summary[is.na(soil_profile_summary$treatment),]
soil_profile_summary[soil_profile_summary$site=='TNC Colusa',]

tapply(soil_profile_summary$kgNO3_ha, soil_profile_summary$field, summary)
tapply(soil_profile_summary$percent_clay, soil_profile_summary$field, summary)
table(soil_profile_summary$site[soil_profile_summary$field=='Almond'])
tapply(soil_profile_summary$kgNO3_ha[soil_profile_summary$field=='Almond'], soil_profile_summary$site[soil_profile_summary$field=='Almond'], summary)
tapply(soil_profile_summary$kgNO3_ha[soil_profile_summary$field=='Pecan'], soil_profile_summary$site[soil_profile_summary$field=='Pecan'], summary)
tapply(soil_profile_summary$kgNO3_ha[soil_profile_summary$field=='Pistachio'], soil_profile_summary$site[soil_profile_summary$field=='Pistachio'], summary)
summary(soil_profile_summary$kgNO3_ha[which(soil_profile_summary$field=='Alfalfa')])

#make tree crop only dataset
soil_profile_summary_tree_crops <- soil_profile_summary[which(soil_profile_summary$field=='Almond' | soil_profile_summary$field=='Pistachio' | soil_profile_summary$field=='Pecan'),]
table(soil_profile_summary_tree_crops$field)
tapply(soil_profile_summary_tree_crops$kgNO3_ha, soil_profile_summary_tree_crops$time, summary)
tapply(soil_profile_summary_tree_crops$percent_clay,soil_profile_summary_tree_crops$site, summary)

unique(soil_profile_summary_tree_crops$treatment)
unique(soil_profile_summary_tree_crops$site)
#assume 'None' is "Control"
table(soil_profile_summary_tree_crops$treatment)
20+14+6
soil_profile_summary_tree_crops[soil_profile_summary_tree_crops$treatment=='None',]
soil_profile_summary_tree_crops[soil_profile_summary_tree_crops$field=='Pecan',] #shows that None is likely the same as Control
sum(is.na(soil_profile_summary_tree_crops$treatment))
soil_profile_summary_tree_crops$treatment[soil_profile_summary_tree_crops$treatment=='control' | soil_profile_summary_tree_crops$treatment=='None'] <- 'Control'
sum(soil_profile_summary_tree_crops$treatment=='Control') #40
sum(grepl('[?]', soil_profile_summary_tree_crops$treatment))
soil_profile_summary_tree_crops$treatment[soil_profile_summary_tree_crops$treatment!='Control'] <- 'Flood' #converts two with ? mark
table(soil_profile_summary_tree_crops$treatment)
soil_profile_summary_tree_crops$time_treatment <- paste0(soil_profile_summary_tree_crops$time, '_', soil_profile_summary_tree_crops$treatment)
table(soil_profile_summary_tree_crops$time_treatment)
tapply(soil_profile_summary_tree_crops$kgNO3_ha, soil_profile_summary_tree_crops$time_treatment, summary)
boxplot(kgNO3_ha ~ time_treatment, data = soil_profile_summary_tree_crops)

soil_profile_summary_tree_crops_no_flood <- soil_profile_summary_tree_crops[soil_profile_summary_tree_crops$time_treatment!='After_Flood',]
dim(soil_profile_summary_tree_crops_no_flood)
plot(soil_profile_summary_tree_crops_no_flood$percent_clay, soil_profile_summary_tree_crops_no_flood$kgNO3_ha)
plot(soil_profile_summary_tree_crops_no_flood$`electrical conductivity`, soil_profile_summary_tree_crops_no_flood$kgNO3_ha)


table(soil_profile_summary_tree_crops$time)
plot(soil_profile_summary_tree_crops$percent_clay[soil_profile_summary_tree_crops$time=='After'], soil_profile_summary_tree_crops$kgNO3_ha[soil_profile_summary_tree_crops$time=='After'])
plot(soil_profile_summary_tree_crops$percent_clay[soil_profile_summary_tree_crops$time=='Before'], soil_profile_summary_tree_crops$kgNO3_ha[soil_profile_summary_tree_crops$time=='Before'])
soil_profile_summary_tree_crops[soil_profile_summary_tree_crops$site=='Modesto' | soil_profile_summary_tree_crops$site=='Modesto (Para)',]
list.files(workDir)
write.csv(soil_profile_summary_tree_crops, file.path(workDir, 'summaries', 'tree_crop_soilcores_all.csv'), row.names = FALSE)


sum(soil_depths < 200) #6
soil_200cm_summary <- horizon_to_comp_simple(horizon_SPC = soilcores_spc[which(soil_depths >= 200)], depth = 200)
soil_200cm_summary$total_depth <- soil_depths[which(soil_depths >= 200)]
summary(soil_200cm_summary$nitrate_ppm)
hist(soil_200cm_summary$nitrate_ppm)
hist(soil_200cm_summary$percent_clay)
hist(soil_200cm_summary$percent_silt)
plot(soil_200cm_summary$percent_clay, soil_200cm_summary$nitrate_ppm)
plot(soil_200cm_summary$percent_clay, soil_200cm_summary$`electrical conductivity`)
plot(soil_200cm_summary$percent_silt, soil_200cm_summary$nitrate_ppm)
plot(soil_200cm_summary$percent_clay, soil_200cm_summary$ammonium_ppm)
plot(soil_200cm_summary$total_depth, soil_200cm_summary$nitrate_ppm)

soil_200cm_summary$site <- soilcores_complete$site[match(soil_200cm_summary$core_id, soilcores_complete$core_id)]
soil_200cm_summary$field <- soilcores_complete$field[match(soil_200cm_summary$core_id, soilcores_complete$core_id)]
soil_200cm_summary$treatment <- soilcores_complete$treatment[match(soil_200cm_summary$core_id, soilcores_complete$core_id)]
soil_200cm_summary$time <- soilcores_complete$time[match(soil_200cm_summary$core_id, soilcores_complete$core_id)]
table(soil_200cm_summary$site)
table(soil_200cm_summary$treatment)
table(soil_200cm_summary$time)
unique(soil_200cm_summary$time)
sum(is.na(soil_200cm_summary$time)) #15 are NA
soil_200cm_summary$time[which(soil_200cm_summary$time=='Before ')] <- 'Before'
soil_200cm_summary$core_id[is.na(soil_200cm_summary$time)]
soil_200cm_summary[is.na(soil_200cm_summary$time),]
soil_200cm_summary$year <- soilcores_complete$year[match(soil_200cm_summary$core_id, soilcores_complete$core_id)]
soil_200cm_summary[is.na(soil_200cm_summary$time),]
soil_200cm_summary$time[is.na(soil_200cm_summary$time)] <- ifelse(soil_200cm_summary$year[is.na(soil_200cm_summary$time)]=='2019', 'Before', 'After')
table(soil_200cm_summary$time)

tapply(soil_200cm_summary$nitrate_ppm, soil_200cm_summary$site, summary)
tapply(soil_200cm_summary$nitrate_ppm, soil_200cm_summary$treatment, summary)
plot(soil_200cm_summary$percent_clay[soil_200cm_summary$time=='Before'], soil_200cm_summary$nitrate_ppm[soil_200cm_summary$time=='Before'])
plot(soil_200cm_summary$percent_clay[soil_200cm_summary$time=='After'], soil_200cm_summary$nitrate_ppm[soil_200cm_summary$time=='After'])

summary(soil_200cm_summary$nitrate_ppm[soil_200cm_summary$time=='Before'])
summary(soil_200cm_summary$nitrate_ppm[soil_200cm_summary$time=='After'])
sum(soil_200cm_summary$time=='Before') #59
sum(soil_200cm_summary$time=='Before' & soil_200cm_summary$nitrate_ppm > 10) #11
11/59
sum(soil_200cm_summary$time=='Before' & soil_200cm_summary$nitrate_ppm > 20) #8
soil_200cm_summary[soil_200cm_summary$time=='Before' & soil_200cm_summary$nitrate_ppm > 10,]
sum(soil_200cm_summary$time=='After' & soil_200cm_summary$nitrate_ppm > 10) #14
soil_200cm_summary[soil_200cm_summary$time=='After' & soil_200cm_summary$nitrate_ppm > 10,]
sum(soil_200cm_summary$time=='After' & soil_200cm_summary$nitrate_ppm > 20) #9
soil_200cm_summary[soil_200cm_summary$time=='After' & soil_200cm_summary$nitrate_ppm > 20,]

unique(soilcores_complete$treatment)
unique(soilcores_complete$field)
unique(soilcores_complete$time)
unique(soilcores_complete$year)

unique(soilcores_complete$site[which(soilcores_complete$treatment=='Fallow')])
unique(soilcores_complete$treatment[which(soilcores_complete$site=='TNC')])
