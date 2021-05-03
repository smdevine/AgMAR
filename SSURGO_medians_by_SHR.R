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

#define function 
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

#read in final soil health regions csv by mukey
clus_7_names <- c('6. Fine salt-affected', '3. Low OM with restrictive horizons', '4. High OM with restrictive horizons', '1. Coarse with no restrictions', '2. Loamy with no restrictions', '7. Shrink-swell', '5. Coarse-loamy salt-affected')
valley30cm_by_mukey <- read.csv(file.path(summaryDir, 'FINAL results', 'valley30cm_by_mukey_cluster_FINAL.csv'), stringsAsFactors = FALSE)
length(unique(valley30cm_by_mukey$mukey)) #4595
valley30cm_by_mukey$SHR7name <- clus_7_names[valley30cm_by_mukey$cluster_7]


#read in map unit (mu) tabular data
mu_data <- read.csv(file.path(ssurgoDir, 'ca_mapunit_data.csv'), stringsAsFactors = FALSE)
mukeys_valley <- read.csv(file.path(summaryDir, 'mukeys_unique_valley.csv'), stringsAsFactors=FALSE)
mu_data_valley <- mu_data[mu_data$mukey %in% mukeys_valley$x, ]

#read in component (comp) data
#valley exploratory version replaces comp_data_Fresno with comp_data_valley
list.files(file.path(ssurgoDir, 'component_data'))
comp_data <- read.csv(file.path(ssurgoDir, 'component_data', 'ca_component_data.csv'), stringsAsFactors = FALSE, na.strings = c('', ' ')) #treat blanks or a space as a NA
colnames(comp_data)
comp_data_valley <- comp_data[comp_data$mukey %in% mu_data_valley$mukey,]
if(sum(is.na(comp_data_valley$majcompflag)) > 0) {stop(print('there are NAs in majcomp column!'))}
if(sum(is.na(comp_data_valley$comppct_r)) > 0) {stop(print('there are NAs in the comppct column!'))}
sum(is.na(comp_data_valley$comppct_r))
comp_data_valley <- comp_data_valley[!is.na(comp_data_valley$comppct_r),]
dim(comp_data_valley) #23493 rows
length(unique(comp_data_valley$mukey)) #5043 map units match above
length(unique(comp_data_valley$cokey)) #23493 unique components
unique(comp_data_valley$taxorder) #9 soil orders here and NA
length(unique(comp_data_valley$compname)) #1153 unique component names
unique(comp_data_valley$taxgrtgroup)
comp_data_valley[comp_data_valley$majcompflag=='No ' & !is.na(comp_data_valley$castorieindex), ]
summary(comp_data_valley$comppct_r[comp_data_valley$majcompflag=='Yes'])
sum(comp_data_valley$comppct_r[comp_data_valley$majcompflag=='Yes'] < 15) #10 instance of <15% comppct_r flagged as majcomps
comp_data_valley[comp_data_valley$majcompflag=='Yes' & comp_data_valley$comppct_r < 15,]
sum(comp_data_valley$comppct_r[comp_data_valley$majcompflag=='No '] >= 15) #110 not flagged as majcomp in these valleys with > 15%
comp_data_valley[comp_data_valley$majcompflag=='No ' & comp_data_valley$comppct_r>=15, ]
sum(comp_data_valley$majcompflag=='No ' & comp_data_valley$comppct_r>=15 & !is.na(comp_data_valley$castorieindex)) #7

#check length of each cokey
cokey_lengths <- unlist(lapply(strsplit(as.character(comp_data_valley$cokey[1:2]), ""), length))
summary(cokey_lengths) #all 8 so slice won't create problems later
#fix a few majcompflag errors
comp_data_valley$majcompflag[comp_data_valley$majcompflag=='No ' & comp_data_valley$comppct_r>=15 & !is.na(comp_data_valley$castorieindex)] <- 'Yes'
comp_data_valley$majcompflag[comp_data_valley$majcompflag=='Yes' & comp_data_valley$comppct_r < 15] <- 'No '

#re-query SSURGO to get water contents at 1/3 and 15 bars
soil_names <- unique(comp_data_valley$compname[comp_data_valley$mukey %in% valley30cm_by_mukey$mukey])
length(soil_names) #1091 compnames to query from final SHR delineation
soilname_to_cokey <- function(x) {paste0("SELECT co.mukey, cokey, compname, comppct_r
FROM legend
INNER JOIN mapunit mu ON mu.lkey = legend.lkey
INNER JOIN component co ON mu.mukey = co.mukey 
WHERE
legend.areasymbol != 'US'
AND compname ='", x,  "';")}
soilname_queries <- lapply(soil_names, soilname_to_cokey)
comps_of_interest <- do.call(rbind, lapply(soilname_queries, SDA_query))
head(comps_of_interest)

#select only those components with mukey in SHR delineation (assumes no change in mukey)
all(valley30cm_by_mukey$mukey %in% comps_of_interest$mukey)
comps_of_interest_final <- comps_of_interest[comps_of_interest$mukey %in% valley30cm_by_mukey$mukey, ]
length(comps_of_interest_final$cokey) #26527
comps_of_interest_final <- comps_of_interest_final[which(comps_of_interest_final$comppct_r >= 15), ]
length(comps_of_interest_final$cokey) #6350
length(unique(comps_of_interest_final$mukey)) #4595, same as above in valley30cm_by_mukey

write.csv(comps_of_interest, file.path(resultsDir, 'comps_of_interest_raw.csv'), row.names = FALSE)
write.csv(comps_of_interest_final, file.path(resultsDir, 'comps_of_interest_final.csv'), row.names = FALSE)

query_horizon <- function(x) { #this will not return cokey NAs
  print(x)
  SDA_query(paste0("SELECT comp.compname, comp.mukey, comp.cokey, ch.chkey, hzname, hzdept_r, hzdepb_r, awc_r, ec_r, claytotal_r, silttotal_r, sandtotal_r, dbthirdbar_r, wsatiated_r, wthirdbar_r, wfifteenbar_r, ksat_r, om_r, ph1to1h2o_r, cec7_r
    FROM component comp
      LEFT OUTER JOIN chorizon ch on ch.cokey = comp.cokey
    WHERE comp.cokey = '", x, "'"))
}

SSURGO_horizons <- do.call(rbind, lapply(comps_of_interest_final$cokey, query_horizon))
SSURGO_horizons$SHRname <- valley30cm_by_mukey$SHR7name[match(SSURGO_horizons$mukey, valley30cm_by_mukey$mukey)]
tapply(SSURGO_horizons$claytotal_r, SSURGO_horizons$SHRname, mean, na.rm=TRUE)
tapply(SSURGO_horizons$om_r, SSURGO_horizons$SHRname, mean, na.rm=TRUE)
tapply(SSURGO_horizons$ksat_r, SSURGO_horizons$SHRname, mean, na.rm=TRUE)
SSURGO_horizons$SHRname <- as.factor(SSURGO_horizons$SHRname)
write.csv(SSURGO_horizons, file.path(resultsDir, 'SSURGO_horizons_data.csv'), row.names=FALSE)

#read in SSURGO data created from database query above
SSURGO_horizons <- read.csv(file.path(resultsDir, 'SSURGO_horizons_data.csv'), stringsAsFactors = FALSE)

unique_chkeys <- unique(SSURGO_horizons$chkey)
SSURGO_horizons <- SSURGO_horizons[match(unique_chkeys, SSURGO_horizons$chkey),]
SSURGO_horizons <- SSURGO_horizons[!SSURGO_horizons$compname=='Rock outcrop',]
length(unique(SSURGO_horizons$mukey)) #4595
sum(is.na(SSURGO_horizons$hzdept_r)) #1
sum(is.na(SSURGO_horizons$hzdepb_r)) #1
SSURGO_horizons[is.na(SSURGO_horizons$hzdepb_r),]
SSURGO_horizons[SSURGO_horizons$cokey==19571021,]
SSURGO_horizons <- SSURGO_horizons[!is.na(SSURGO_horizons$hzdepb_r), ]
colnames(SSURGO_horizons)
lapply(SSURGO_horizons[,8:20], summary)
lapply(SSURGO_horizons[,8:20], function(x) sum(x==0, na.rm = TRUE))
#convert ksat, cec, om, and water retention that are 0 to NA
SSURGO_horizons$ksat_r[SSURGO_horizons$ksat_r==0] <- NA
SSURGO_horizons$om_r[SSURGO_horizons$om_r==0] <- NA
SSURGO_horizons$cec7_r[SSURGO_horizons$cec7_r==0] <- NA
SSURGO_horizons$wsatiated_r[SSURGO_horizons$wsatiated_r==0] <- NA
SSURGO_horizons$wthirdbar_r[SSURGO_horizons$wthirdbar_r==0] <- NA
SSURGO_horizons$wfifteenbar_r[SSURGO_horizons$wfifteenbar_r==0] <- NA

write.csv(SSURGO_horizons, file.path(resultsDir, 'SSURGO_horizons_SHR7_final.csv'), row.names=FALSE)

depths(SSURGO_horizons) <- cokey ~ hzdept_r + hzdepb_r
site(SSURGO_horizons) <- ~ SHRname

#determine stats by depth increments and SHR to use as RZWQM soil property parameters
SSURGO_horizons.slab <- slab(SSURGO_horizons, SHRname ~ claytotal_r + silttotal_r + sandtotal_r + ph1to1h2o_r + om_r + ec_r + cec7_r + dbthirdbar_r + wsatiated_r + wthirdbar_r + wfifteenbar_r + ksat_r, slab.structure = 1, strict = TRUE)

SSURGO_horizons.slab <- SSURGO_horizons.slab[SSURGO_horizons.slab$top <=149,]
levels(SSURGO_horizons.slab$variable)
unique(SSURGO_horizons.slab$SHRname)
colnames(SSURGO_horizons.slab)

writeCSV_bySHR <- function(SHR, stat, depth_top=c(0,10,30,50,75,100,125), depth_bot=c(10,30,50,75,100,125,150), fname) {
  df <- SSURGO_horizons.slab[SSURGO_horizons.slab$SHRname==SHR,]
  result <- do.call(cbind, lapply(unique(as.character(SSURGO_horizons.slab$variable)), function(x) {
    y <- df[df$variable==x, stat]
    aggregate(y, by=list(do.call(c, sapply(seq_along(depth_bot), function(x) rep(x, times=(depth_bot - depth_top)[x]), simplify = TRUE))), FUN=mean, na.rm=TRUE)[,2]
  }))
  result <- as.data.frame(cbind(depth_top=depth_top, depth_bot=depth_bot, result))
  colnames(result)[3:ncol(result)] <- unique(as.character(SSURGO_horizons.slab$variable))
  texture_sums <- result$claytotal_r + result$silttotal_r + result$sandtotal_r
  print(texture_sums)
  result$claytotal_r <- result$claytotal_r * 100 / texture_sums
  result$silttotal_r <- result$silttotal_r * 100 / texture_sums
  result$sandtotal_r <- result$sandtotal_r * 100 / texture_sums
  result$textural_class <- textural.class.calc(sand = result$sandtotal_r, silt = result$silttotal_r, clay = result$claytotal_r)
  print(result)
  write.csv(result, file.path(resultsDir, fname), row.names = FALSE)
}

writeCSV_bySHR(SHR='1. Coarse with no restrictions', stat='p.q50', fname = 'coarse_w_no_res_median.csv')
writeCSV_bySHR(SHR='2. Loamy with no restrictions', stat = 'p.q50', fname =  'loamy_w_no_res_median.csv')
writeCSV_bySHR(SHR='3. Low OM with restrictive horizons', stat='p.q50', fname= 'low_om_w__res_median.csv')
writeCSV_bySHR(SHR='4. High OM with restrictive horizons', stat='p.q50', fname = 'high_om_w__res_median.csv')
writeCSV_bySHR(SHR='5. Coarse-loamy salt-affected', stat='p.q50', fname= 'coarse_loamy_salt_aff_median.csv')
writeCSV_bySHR(SHR='6. Fine salt-affected', stat='p.q50', fname= 'fine_salt_aff_median.csv')
writeCSV_bySHR(SHR='7. Shrink-swell', stat='p.q50', fname= 'shrink_swell_median.csv')

#