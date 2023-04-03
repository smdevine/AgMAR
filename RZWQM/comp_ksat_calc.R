#TO-DO
#calc profile weighted texture, wilting point, and field capacity
#read-in microporosity estimates from rzqwm created input files
#this file was used to 
library(soilDB)
library(aqp)
ssurgoDir <- 'C:/Users/smdevine/Desktop/post doc/Dahlke/RZWQM/ssurgo_profile_data/RZWQM input'
textural.class.calc <- function(sand, silt, clay) {
  ifelse(is.na(sand) | is.na(silt) | is.na(clay), NA,
         ifelse(sand + silt + clay > 101 |
                  sand + silt + clay < 99, 'proportions do not sum to 100+-1',
                ifelse(silt + 1.5 * clay < 15, 'sand',
                       ifelse(silt + 1.5 * clay >= 15 & silt + 2 * clay < 30, 'loamy sand',
                              ifelse((clay >= 7 & clay < 20 & sand > 52 & silt + 2 * clay >= 30) | 
                                       (clay < 7 & silt < 50 & silt + 2 * clay >= 30), 'sandy loam',
                                     ifelse(clay >= 7 & clay < 27 & silt >=28 & silt < 50 & sand <= 52, 'loam',
                                            ifelse((silt >= 50 & clay >= 12 & clay < 27) | 
                                                     (silt >=50 & silt < 80 & clay < 12), 'silt loam',
                                                   ifelse(silt >= 80 & clay < 12, 'silt',
                                                          ifelse(clay >= 20 & clay < 35 & silt < 28 & sand > 45, 'sandy clay loam',
                                                                 ifelse(clay >= 27 & clay < 40 & sand > 20 & sand <= 45, 'clay loam',
                                                                        ifelse(clay >= 27 & clay < 40 & sand <= 20, 'silty clay loam',
                                                                               ifelse(clay >= 35 & sand > 45, 'sandy clay',
                                                                                      ifelse(clay >= 40 & silt >= 40, 'silty clay',
                                                                                             ifelse(clay >= 40 & sand <= 45 & silt < 40, 'clay',
                                                                                                    'undefined textural class'))))))))))))))
}

#read-in SSURGO horizon data for those soils included in modeling
#read-in SSURGO data
ssurgo_horizons <- read.csv(file.path(ssurgoDir, 'SSURGO_soils_to_initialize.csv'), stringsAsFactors = FALSE)
colnames(ssurgo_horizons)
unique(ssurgo_horizons$compname)
dim(ssurgo_horizons)
ssurgo_horizons2 <- read.csv(file.path(ssurgoDir, 'SSURGO_soils_to_initialize_part2.csv'), stringsAsFactors = FALSE)
colnames(ssurgo_horizons2)
unique(ssurgo_horizons2$compname)
dim(ssurgo_horizons2)
ssurgo_horizons3 <- read.csv(file.path(ssurgoDir, 'SSURGO_soils_to_initialize_part3.csv'), stringsAsFactors = FALSE)
colnames(ssurgo_horizons3)
unique(ssurgo_horizons3$compname)
dim(ssurgo_horizons3)

ssurgo_horizons <- rbind(ssurgo_horizons, ssurgo_horizons2, ssurgo_horizons3)
unique(ssurgo_horizons$compname)
dim(ssurgo_horizons)
# ssurgo_horizons$texture <- textural.class.calc(sand = ssurgo_horizons$sandtotal_r, silt = ssurgo_horizons$silttotal_r, clay = ssurgo_horizons$claytotal_r)
comp_data <- data.frame(compnames=unique(ssurgo_horizons$compname), stringsAsFactors = FALSE)
comp_data$SHR <- ssurgo_horizons$SHRname[match(comp_data$compnames, ssurgo_horizons$compname)]
dim(comp_data)
length(unique(ssurgo_horizons$compname))
lapply(ssurgo_horizons, class)
# ssurgo_horizons[ssurgo_horizons$compname=='Colpien',]
# ssurgo_horizons[ssurgo_horizons$compname=='Tehama',]
# ssurgo_horizons[ssurgo_horizons$compname=='Pleito',]
# ssurgo_horizons[ssurgo_horizons$compname=='Columbia',]
# ssurgo_horizons[ssurgo_horizons$compname=='Westhaven',]

#create soil profile collection object
#initialize spc object
SSURGO_spc <- ssurgo_horizons
depths(SSURGO_spc) <- cokey ~ hzdept_r + hzdepb_r
SSURGO_spc
depth_ck <- checkHzDepthLogic(SSURGO_spc)
depth_ck
all(depth_ck$valid) #all good
names(SSURGO_spc)
#calculate profile weighted ksat to calc approx. days needed to apply 15 cm water
profile_wtd_ksat <- profileApply(SSURGO_spc, FUN=function(x) {
  profile_depth <- max(x$hzdepb_r)
  horizon_wts <- (x$hzdepb_r - x$hzdept_r) / profile_depth
  sum(x$ksat_r * horizon_wts)*24*3600/10000
})
profile_wtd_clay <- profileApply(SSURGO_spc, FUN=function(x) {
  profile_depth <- max(x$hzdepb_r)
  horizon_wts <- (x$hzdepb_r - x$hzdept_r) / profile_depth
  sum(x$claytotal_r * horizon_wts)
})
profile_wtd_silt <- profileApply(SSURGO_spc, FUN=function(x) {
  profile_depth <- max(x$hzdepb_r)
  horizon_wts <- (x$hzdepb_r - x$hzdept_r) / profile_depth
  sum(x$silttotal_r * horizon_wts)
})
profile_wtd_sand <- profileApply(SSURGO_spc, FUN=function(x) {
  profile_depth <- max(x$hzdepb_r)
  horizon_wts <- (x$hzdepb_r - x$hzdept_r) / profile_depth
  sum(x$sandtotal_r * horizon_wts)
})
profile_wtd_WP <- profileApply(SSURGO_spc, FUN=function(x) {
  profile_depth <- max(x$hzdepb_r)
  horizon_wts <- (x$hzdepb_r - x$hzdept_r) / profile_depth
  sum(x$wfifteenbar_r * horizon_wts)
})
profile_wtd_FC <- profileApply(SSURGO_spc, FUN=function(x) {
  profile_depth <- max(x$hzdepb_r)
  horizon_wts <- (x$hzdepb_r - x$hzdept_r) / profile_depth
  sum(x$wthirdbar_r * horizon_wts)
})
profile_wtd_microporosity <- profileApply(SSURGO_spc, FUN=function(x) {
  profile_depth <- max(x$hzdepb_r)
  horizon_wts <- (x$hzdepb_r - x$hzdept_r) / profile_depth
  sum(x$Microporosity * horizon_wts)
})
comp_data$cokey <- ssurgo_horizons$cokey[match(comp_data$compnames, ssurgo_horizons$compname)]
comp_data$ksat_cm_day <- profile_wtd_ksat[match(comp_data$cokey, as.integer(names(profile_wtd_ksat)))]
comp_data$irrdays <- ifelse(comp_data$ksat_cm_day >= 15, 1, ceiling(15 / comp_data$ksat_cm_day))
comp_data$clay <- profile_wtd_clay[match(comp_data$cokey, as.integer(names(profile_wtd_clay)))]
comp_data$silt <- profile_wtd_silt[match(comp_data$cokey, as.integer(names(profile_wtd_silt)))]
comp_data$sand <- profile_wtd_sand[match(comp_data$cokey, as.integer(names(profile_wtd_sand)))]
comp_data$theta_15b <- profile_wtd_WP[match(comp_data$cokey, as.integer(names(profile_wtd_WP)))]
comp_data$theta_0.3b <- profile_wtd_FC[match(comp_data$cokey, as.integer(names(profile_wtd_FC)))]
comp_data$microporosity <- profile_wtd_microporosity[match(comp_data$cokey, as.integer(names(profile_wtd_microporosity)))]
comp_data[order(comp_data$SHR),]

# write.csv(comp_data, file.path(ssurgoDir, 'comp_data_RZWQMruns_Feb22.csv'), row.names = FALSE)
write.csv(comp_data, file.path(ssurgoDir, 'comp_data_RZWQMruns_Jun22.csv'), row.names = FALSE)
utils::View(comp_data[order(comp_data$SHR),])
