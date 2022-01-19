library(soilDB)
library(aqp)
laptop <- TRUE
if (laptop) {
  mainDir <- 'C:/Users/smdevine/Desktop/post doc'
} else { #on UCD desktop
  mainDir <- 'C:/Users/smdevine/Desktop/PostDoc'
}
ssurgoDir <- file.path(mainDir, 'soil health/ssurgo_data')
summaryDir <- file.path(mainDir, 'soil health/summaries/valley_final')
resultsDir <- 'C:/Users/smdevine/Desktop/post doc/Dahlke/RZWQM/ssurgo_profile_data'

#texture function
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

#read-in data
SSURGO_horizons <- read.csv(file.path(resultsDir, 'SSURGO_horizons_SHR7_final.csv'), stringsAsFactors = FALSE)
head(SSURGO_horizons)
nrow(SSURGO_horizons) #20274
SSURGO_horizons <- SSURGO_horizons[!(SSURGO_horizons$hzname %in% c('H1', 'H2', 'H3', 'H4', 'H5')),]
nrow(SSURGO_horizons) #11116 horizons
length(unique(SSURGO_horizons$cokey)) #2516 unique profiles
length(unique(SSURGO_horizons$compname)) #578 unique compnames
SSURGO_horizons <- SSURGO_horizons[order(SSURGO_horizons$cokey, SSURGO_horizons$hzdept_r),]
head(SSURGO_horizons, 20)

#initialize spc object
SSURGO_spc <- SSURGO_horizons
depths(SSURGO_spc) <- cokey ~ hzdept_r + hzdepb_r
depth_ck <- checkHzDepthLogic(SSURGO_spc)
head(depth_ck)
all(depth_ck$valid) #all good
# site(SSURGO_spc) <- ~ SHRname

compnames <- c('Hanford', 'Delhi', 'Wasco', 'Hesperia', 'Milham', 'Tujunga', 'Panoche', 'Cerini', 'Yolo', 'Colpien', 'Channac', 'Tehama', 'Capay', 'Clear Lake', 'Willows', 'Tulare')
print_soils_by_compname <- function(x) {
  y <- SSURGO_horizons[SSURGO_horizons$compname==x,]
  print(paste(x, length(unique(y$cokey))))
  # print(y)
}
for(i in seq_along(compnames)) {
  print_soils_by_compname(compnames[i])
}
compnames <- compnames[compnames != 'Channac']
return_soil_by_compname <- function(x) {
  y <- SSURGO_horizons[SSURGO_horizons$compname==x,]
  cokeys <- as.character(unique(y$cokey))
  cokey <- sample(cokeys, size = 1)
  soil <- y[as.character(y$cokey)==cokey,]
  soil
}
soils_to_run <- do.call(rbind, lapply(compnames, return_soil_by_compname))
soils_to_run
dim(soils_to_run)
unique(soils_to_run$compname)
length(unique(soils_to_run$cokey))
soils_to_run$texture <- textural.class.calc(sand = soils_to_run$sandtotal_r, silt = soils_to_run$silttotal_r, clay = soils_to_run$claytotal_r)
write.csv(soils_to_run, file.path(resultsDir, 'RZWQM input', 'SSURGO_soils_to_initialize.csv'), row.names = FALSE)

#capay re-run
capay <- return_soil_by_compname('Capay')
write.csv(capay, file.path(resultsDir, 'RZWQM input', 'soils to rerun', 'Capay.csv'), row.names = FALSE) #this one worked better but insufficient irrigation due to smr most likely constrained tomato yields

#tehama re-run
tehama <- return_soil_by_compname('Tehama')
tehama
write.csv(tehama, file.path(resultsDir, 'RZWQM input', 'soils to rerun', 'Tehama.csv'), row.names = FALSE)

#tujunga re-run
tujunga <- return_soil_by_compname('Tujunga')
tujunga
write.csv(tujunga, file.path(resultsDir, 'RZWQM input', 'soils to rerun', 'Tujunga.csv'), row.names = FALSE)
