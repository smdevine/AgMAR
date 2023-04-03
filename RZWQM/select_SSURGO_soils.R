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
SSURGO_horizons[SSURGO_horizons$cokey==19574236,]
#initialize spc object
SSURGO_spc <- SSURGO_horizons
depths(SSURGO_spc) <- cokey ~ hzdept_r + hzdepb_r
depth_ck <- checkHzDepthLogic(SSURGO_spc)
head(depth_ck)
all(depth_ck$valid) #all good
# site(SSURGO_spc) <- ~ SHRname
return_soil_by_compname <- function(x) {
  y <- SSURGO_horizons[SSURGO_horizons$compname==x,]
  cokeys <- as.character(unique(y$cokey))
  cokey <- sample(cokeys, size = 1)
  soil <- y[as.character(y$cokey)==cokey,]
  soil
}
compnames <- c('Hanford', 'Delhi', 'Wasco', 'Hesperia', 'Milham', 'Tujunga', 'Panoche', 'Cerini', 'Yolo', 'Colpien', 'Channac', 'Tehama', 'Capay', 'Clear Lake', 'Willows', 'Tulare')
compnames2 <- c('Kimberlina', 'Columbia', 'Excelsior', 'Atwater', 'Rincon', 'Sycamore', 'Conejo', 'Westhaven', 'Pleito', 'Jacktone', 'Porterville', 'Lokern', 'Myers', 'Esquon', 'Hollenbeck')
print_soils_by_compname <- function(x) {
  y <- SSURGO_horizons[SSURGO_horizons$compname==x,]
  print(paste(x, length(unique(y$cokey))))
  # print(y)
}
for(i in seq_along(compnames2)) {
  print_soils_by_compname(compnames2[i])
}
compnames <- compnames[compnames != 'Channac']
soils_to_run <- do.call(rbind, lapply(compnames2, return_soil_by_compname))
soils_to_run
dim(soils_to_run)
unique(soils_to_run$compname)
length(unique(soils_to_run$cokey))
soils_to_run$texture <- textural.class.calc(sand = soils_to_run$sandtotal_r, silt = soils_to_run$silttotal_r, clay = soils_to_run$claytotal_r)
write.csv(soils_to_run, file.path(resultsDir, 'RZWQM input', 'SSURGO_soils_to_initialize_part2.csv'), row.names = FALSE) #note that compnames2 produced this file

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

#kimberlina re-run
kimberlina <- return_soil_by_compname('Kimberlina')
kimberlina
write.csv(kimberlina, file.path(resultsDir, 'RZWQM input', 'soils to rerun part2', 'Kimberlina.csv'), row.names = FALSE)

#columbia re-run
columbia <- return_soil_by_compname('Columbia')
columbia
write.csv(columbia, file.path(resultsDir, 'RZWQM input', 'soils to rerun part2', 'columbia_Coarse.csv'), row.names = FALSE)

#westhaven re-run
westhaven <- return_soil_by_compname('Westhaven')
westhaven
write.csv(westhaven, file.path(resultsDir, 'RZWQM input', 'soils to rerun part2', 'westhaven.csv'), row.names = FALSE)

#tachi re-run
tachi <- return_soil_by_compname('Tachi')
tachi
write.csv(tachi, file.path(resultsDir, 'RZWQM input', 'soils to rerun part2', 'Tachi.csv'), row.names = FALSE)

#cropley re-run
cropley <- return_soil_by_compname('Cropley')
cropley
write.csv(cropley, file.path(resultsDir, 'RZWQM input', 'soils to rerun part2', 'cropley.csv'), row.names = FALSE)

#conejo re-run
conejo <- return_soil_by_compname('Conejo')
conejo
write.csv(conejo, file.path(resultsDir, 'RZWQM input', 'soils to rerun part2', 'conejo.csv'), row.names = FALSE)

#excelsior re-run
excelsior <- return_soil_by_compname('Excelsior')
excelsior
write.csv(excelsior, file.path(resultsDir, 'RZWQM input', 'soils to rerun part2', 'excelsior_Coarse.csv'), row.names = FALSE)

#pleito re-run
pleito <- return_soil_by_compname('Pleito')
pleito
write.csv(pleito, file.path(resultsDir, 'RZWQM input', 'soils to rerun part2', 'pleito_Loamy.csv'), row.names = FALSE)

#esquon re-run
esquon <- return_soil_by_compname('Esquon')
esquon
write.csv(esquon, file.path(resultsDir, 'RZWQM input', 'soils to rerun part2', 'esquon_Fine.csv'), row.names = FALSE)

#merced get
merced <- return_soil_by_compname('Merced')
merced
write.csv(merced, file.path(resultsDir, 'RZWQM input', 'soils to rerun part2', 'Merced_Fine.csv'), row.names = FALSE)

#lofgren get
lofgren <- return_soil_by_compname('Lofgren')
write.csv(lofgren, file.path(resultsDir, 'RZWQM input', 'soils to rerun part2', 'Lofgren_Fine.csv'), row.names = FALSE)

#wekoda get
print_soils_by_compname('Wekoda')
wekoda <- return_soil_by_compname('Wekoda')
write.csv(wekoda, file.path(resultsDir, 'RZWQM input', 'soils to rerun part2', 'Wekoda_Fine.csv'), row.names = FALSE)

#granoso get
print_soils_by_compname('Granoso')
Granoso <- return_soil_by_compname('Granoso')
write.csv(Granoso, file.path(resultsDir, 'RZWQM input', 'soils to run part3', 'Granoso_coarse.csv'), row.names = FALSE)

#Delano get
print_soils_by_compname('Delano')
Delano <- return_soil_by_compname('Delano')
Delano
write.csv(Delano, file.path(resultsDir, 'RZWQM input', 'soils to run part3', 'Delano_coarse.csv'), row.names = FALSE)

#Chanac get
print_soils_by_compname('Chanac')
Chanac <- return_soil_by_compname('Chanac')
Chanac
write.csv(Chanac, file.path(resultsDir, 'RZWQM input', 'soils to run part3', 'Chanac_loamy.csv'), row.names = FALSE)

#Dinuba get
print_soils_by_compname('Dinuba')
Dinuba <- return_soil_by_compname('Dinuba')
Dinuba #OM is NA in 2Bk3 and 4, so cannot use Dinuba
write.csv(Dinuba, file.path(resultsDir, 'RZWQM input', 'soils to run part3', 'Dinuba_coarse.csv'), row.names = FALSE)

print_soils_by_compname('Guijarral')
Guijarral <- return_soil_by_compname('Guijarral')
Guijarral
write.csv(Guijarral, file.path(resultsDir, 'RZWQM input', 'soils to run part3', 'Guijarral_coarse.csv'), row.names = FALSE)
