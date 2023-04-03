library(soilDB)
library(aqp)
ssurgoDir <- 'C:/Users/smdevine/Desktop/post doc/Dahlke/RZWQM/ssurgo_profile_data/RZWQM input'

#define functions
wtd.mean <- function(x, y) {
  # use horizon thickness as a weight
  thick <- x$hzdepb_r - x$hzdept_r
  # compute the weighted mean, accounting for the possibility of missing data
  m <- weighted.mean(horizons(x)[[y]], w=thick, na.rm=TRUE)
  m
}
horizon_to_comp <- function(horizon_SPC, depth, comp_df, vars_of_interest = c('claytotal_r', 'silttotal_r', 'sandtotal_r', 'ksat_r', 'ph1to1h2o_r', 'om_r', 'dbthirdbar_r', 'wthirdbar_r', 'wfifteenbar_r'), varnames = c('clay', 'silt', 'sand', 'ksat', 'pH', 'SOM', 'BD', 'theta_0.3b', 'theta_15b'), SOC_content=FALSE, sum_AWC=FALSE) {
  columnames <- paste0(varnames, '_', depth, 'cm')
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
  if (SOC_content) {
    s[[paste0('kgOrg.m2_', depth, 'cm')]] <- profileApply(sliced_SPC, FUN = kgOrgC_sum)
    columnames <- c(columnames, paste0('kgOrg.m2_', depth, 'cm'))
  }
  if (sum_AWC) {
    s[[paste0('awc_', depth, 'cm')]] <- profileApply(sliced_SPC, FUN = awc_sum)
    columnames <- c(columnames, paste0('awc_', depth, 'cm'))
  }
  rm(depth, envir = .GlobalEnv) #because we had to put it there earlier
  s$compname <- comp_df$compname[match(s$cokey, comp_df$cokey)]
  # s$mukey <- comp_df$mukey[match(s$cokey, comp_df$cokey)]
  # s$comppct <- comp_df$comppct_r[match(s$cokey, comp_df$cokey)]
  s$SHR <- comp_df$SHR[match(s$cokey, comp_df$cokey)]
  s <- s[,c('compname', 'SHR', columnames)]
  s
}

#read-in SSURGO horizon data for those soils included in modeling
#read-in SSURGO data
ssurgo_horizons <- read.csv(file.path(ssurgoDir, 'SSURGO_soils_to_initialize.csv'), stringsAsFactors = FALSE)
colnames(ssurgo_horizons)
ssurgo_horizons2 <- read.csv(file.path(ssurgoDir, 'SSURGO_soils_to_initialize_part2.csv'), stringsAsFactors = FALSE)
colnames(ssurgo_horizons2)
ssurgo_horizons <- rbind(ssurgo_horizons, ssurgo_horizons2)

comp_data <- data.frame(compnames=unique(ssurgo_horizons$compname), stringsAsFactors = FALSE)
comp_data$cokey <- ssurgo_horizons$cokey[match(comp_data$compnames, ssurgo_horizons$compname)]
comp_data$SHR <- ssurgo_horizons$SHRname[match(comp_data$compnames, ssurgo_horizons$compname)]
ssurgo_horizons[ssurgo_horizons$compname=='Colpien',]
ssurgo_horizons[ssurgo_horizons$compname=='Tehama',]
ssurgo_horizons[ssurgo_horizons$compname=='Pleito',]
ssurgo_horizons[ssurgo_horizons$compname=='Columbia',]
ssurgo_horizons[ssurgo_horizons$compname=='Westhaven',]

#create soil profile collection object
#initialize spc object
SSURGO_spc <- ssurgo_horizons
depths(SSURGO_spc) <- cokey ~ hzdept_r + hzdepb_r
depth_ck <- checkHzDepthLogic(SSURGO_spc)
head(depth_ck)
all(depth_ck$valid) #all good
names(SSURGO_spc)

#aggregate SOM data
comp_30cm <- horizon_to_comp(horizon_SPC = SSURGO_spc, depth = 30, comp_df = comp_data)
head(comp_30cm)
summary(comp_30cm$SOM_30cm)
tapply(comp_30cm$SOM_30cm, comp_30cm$SHR, median)
tapply(comp_30cm$SOM_30cm, comp_30cm$SHR, summary)
comp_30cm[comp_30cm$SHR=='1. Coarse with no restrictions',]
write.csv(comp_30cm, file.path(ssurgoDir, 'comp_30cm_summary.csv'), row.names = FALSE)
