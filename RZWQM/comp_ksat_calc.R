library(soilDB)
library(aqp)
ssurgoDir <- 'C:/Users/smdevine/Desktop/post doc/Dahlke/RZWQM/ssurgo_profile_data/RZWQM input'
#read-in SSURGO horizon data for those soils included in modeling
#read-in SSURGO data
ssurgo_horizons <- read.csv(file.path(ssurgoDir, 'SSURGO_soils_to_initialize.csv'), stringsAsFactors = FALSE)
colnames(ssurgo_horizons)
ssurgo_horizons2 <- read.csv(file.path(ssurgoDir, 'SSURGO_soils_to_initialize_part2.csv'), stringsAsFactors = FALSE)
colnames(ssurgo_horizons2)

ssurgo_horizons <- rbind(ssurgo_horizons, ssurgo_horizons2)
ssurgo_horizons$texture <- textural.class.calc(sand = ssurgo_horizons$sandtotal_r, silt = ssurgo_horizons$silttotal_r, clay = ssurgo_horizons$claytotal_r)
comp_data <- data.frame(compnames=unique(ssurgo_horizons$compname), stringsAsFactors = FALSE)
comp_data$SHR <- ssurgo_horizons$SHRname[match(comp_data$compnames, ssurgo_horizons$compname)]

#create soil profile collection object
#initialize spc object
SSURGO_spc <- ssurgo_horizons
depths(SSURGO_spc) <- cokey ~ hzdept_r + hzdepb_r
depth_ck <- checkHzDepthLogic(SSURGO_spc)
head(depth_ck)
all(depth_ck$valid) #all good
names(SSURGO_spc)
#calculate profile weighted ksat to calc approx. days needed to apply 15 cm water
profile_wtd_ksat <- profileApply(SSURGO_spc, FUN=function(x) {
  profile_depth <- max(x$hzdepb_r)
  horizon_wts <- (x$hzdepb_r - x$hzdept_r) / profile_depth
  sum(x$ksat_r * horizon_wts)*24*3600/10000
})

comp_data$cokey <- ssurgo_horizons$cokey[match(comp_data$compnames, ssurgo_horizons$compname)]
comp_data$ksat_cm_day <- profile_wtd_ksat[match(comp_data$cokey, as.integer(names(profile_wtd_ksat)))]

comp_data$irrdays <- ifelse(comp_data$ksat_cm_day >= 15, 1, ceiling(15 / comp_data$ksat_cm_day))
comp_data[order(comp_data$SHR),]
write.csv(comp_data, file.path(ssurgoDir, 'comp_data_RZWQMruns_Feb22.csv'), row.names = FALSE)
