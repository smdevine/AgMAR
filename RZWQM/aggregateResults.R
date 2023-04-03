#TO-DO: 
#1. Calculate net effect of Jan vs. Mar timing and 3d vs. 7d vs. 21d frequency on nitrate leaching, net mineralization, and denitrification, using scenario results
#2. Summarize residual soil nitrate by year
#3. Summarize harvest biomass by year and specifically aboveground N
#4. Make barplots by soil textural class showing sum of leaching, denitrification, aboveground N removal, and volatization versus total fert N + net mineralization

library(soilDB)
library(aqp)
# workDir <- 'C:/Users/smdevine/Desktop/post doc/Dahlke/RZWQM/projects/PulseSoilClimate/ClimateRuns'
workDir #read-in from other scripts (e.g. ..., etc.)
resultsDir <- file.path(workDir, 'Results')
ssurgoDir <- 'C:/Users/smdevine/Desktop/post doc/Dahlke/RZWQM/ssurgo_profile_data/RZWQM input'
comp_partclass <- read.csv(file.path(ssurgoDir, 'comp_taxpartclass.csv'), stringsAsFactors = FALSE)
met_stn
resultsDir
list.files(resultsDir, recursive = FALSE)

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
# test <- list.files(file.path(resultsDir, 'Overall'), full.names = FALSE)
# test2 <- unlist(strsplit(test[1], '_'))

overall_results <- do.call(rbind, mapply(function(x, y) {
  if(grepl('AgMAR', y)) {
    soilname <- unlist(strsplit(y, '_'))[1]
    scenario <- unlist(strsplit(y, '_'))[3]
  } else {
      soilname <- unlist(strsplit(y, '_'))[1]
      scenario <- 'Control'
  }
  z <- read.csv(x, stringsAsFactors = FALSE)
  z$soil <- soilname
  z$scenario <- scenario
  z
}, x=list.files(file.path(resultsDir, 'Overall', met_stn), full.names = TRUE), y=list.files(file.path(resultsDir, 'Overall', met_stn), full.names = FALSE), SIMPLIFY = FALSE))
# head(overall_results)
overall_results$X <- NULL
# dim(overall_results)
# unique(overall_results$soil)
# lapply(overall_results, summary)
overall_results <- overall_results[order(overall_results$NO3_leached_kgN_ha),]
utils::View(overall_results)

#read-in SSURGO data
ssurgo_horizons <- read.csv(file.path(ssurgoDir, 'SSURGO_soils_to_initialize.csv'), stringsAsFactors = FALSE)
# colnames(ssurgo_horizons)
ssurgo_horizons2 <- read.csv(file.path(ssurgoDir, 'SSURGO_soils_to_initialize_part2.csv'), stringsAsFactors = FALSE)
# colnames(ssurgo_horizons2)
ssurgo_horizons3 <- read.csv(file.path(ssurgoDir, 'SSURGO_soils_to_initialize_part3.csv'), stringsAsFactors = FALSE)
# colnames(ssurgo_horizons3)

ssurgo_horizons <- rbind(ssurgo_horizons, ssurgo_horizons2, ssurgo_horizons3)
ssurgo_horizons$texture <- textural.class.calc(sand = ssurgo_horizons$sandtotal_r, silt = ssurgo_horizons$silttotal_r, clay = ssurgo_horizons$claytotal_r)
length(unique(ssurgo_horizons$compname))

#organize by compname
comp_data <- data.frame(compnames=unique(ssurgo_horizons$compname), stringsAsFactors = FALSE)
comp_data$SHR <- ssurgo_horizons$SHRname[match(comp_data$compnames, ssurgo_horizons$compname)]
comp_data[order(comp_data$SHR),]
# table(comp_data$SHR)

#add SHR info
overall_results$SHR <- comp_data$SHR[match(overall_results$soil, comp_data$compnames)]
rownames(overall_results) <- 1:nrow(overall_results)
# table(overall_results$SHR[overall_results$scenario=='Control'])

#add taxonomic particle size class info (derived from query_SSURGO_taxonomy.R manually)
overall_results$taxpartclass <- comp_partclass$taxpartclass[match(overall_results$soil, comp_partclass$compname)]
overall_results$taxpartclass_simplified <- ifelse(overall_results$taxpartclass %in% c('Sandy', 'Coarse-loamy'), 1, ifelse(overall_results$taxpartclass %in% c('Fine', 'Very-fine'), 3, 2))
# table(overall_results$taxpartclass[overall_results$scenario=='Control'])

#add net min column
overall_results$net_min_kgN_ha <- overall_results$N_min_kgN_ha - overall_results$N_imm_kgN_ha

#add net soil N increase
overall_results$increase_totalsoilkgN_ha <- overall_results$final_totalsoilkgN_ha - overall_results$initial_totalsoilkgN_ha

#add net soil C increase
overall_results$delta_SOC <- overall_results$final_totalsoilMgC_ha - overall_results$initial_totalsoilMgC_ha

#add initial soil org N and final soil org N and the change
overall_results$final_orgN <- overall_results$final_totalsoilkgN_ha - overall_results$final_kgNO3_ha
overall_results$initial_orgN <- overall_results$initial_totalsoilkgN_ha - overall_results$initial_kgNO3_ha
overall_results$delta_orgN <- overall_results$final_orgN - overall_results$initial_orgN

#add some more columns
overall_results$NO3_leached_delta_kgN_ha <- sapply(1:nrow(overall_results), function(x) {
  control_leaching <- overall_results[overall_results$scenario=='Control',]
  overall_results$NO3_leached_kgN_ha[x] - control_leaching$NO3_leached_kgN_ha[match(overall_results$soil[x], control_leaching$soil)]
})
overall_results$NO3_leached_delta_percent <- sapply(1:nrow(overall_results), function(x) {
  control_leaching <- overall_results[overall_results$scenario=='Control',]
  round(100*(overall_results$NO3_leached_kgN_ha[x] - control_leaching$NO3_leached_kgN_ha[match(overall_results$soil[x], control_leaching$soil)]) / control_leaching$NO3_leached_kgN_ha[match(overall_results$soil[x], control_leaching$soil)], 2)
})

#add harvest results
harvest_results <- do.call(rbind, mapply(function(x, y) {
  if(grepl('AgMAR', y)) {
    soilname <- unlist(strsplit(y, '_'))[1]
    scenario <- unlist(strsplit(y, '_'))[3]
  } else {
    soilname <- unlist(strsplit(y, '_'))[1]
    scenario <- 'Control'
  }
  z <- read.csv(x, stringsAsFactors = FALSE)
  y <- data.frame(soil=soilname, scenario=scenario, mean_abovegrnd_biomass_kg_ha=mean(z$biomass_kg_ha), total_abovegrnd_biomass_N_kg_ha=sum(z$above_biomass_N_kg_ha))
  y
}, x=list.files(file.path(resultsDir, 'Yields', met_stn), full.names = TRUE), y=list.files(file.path(resultsDir, 'Yields', met_stn), full.names = FALSE), SIMPLIFY = FALSE))
row.names(harvest_results) <- NULL
head(harvest_results)

overall_results <- merge(overall_results, harvest_results, by=c('soil', 'scenario'))

#write to file
write.csv(overall_results, file.path(resultsDir, 'Summaries', paste0('overall_results_', met_stn, '.csv')), row.names = FALSE)