met_stn <- 'Shafter'
workDir <- 'C:/Users/smdevine/Desktop/post doc/Dahlke/RZWQM/projects/PulseSoilClimate/ClimateRuns_Silage/'
FiguresDir <- file.path(workDir, 'Figures')
if(!dir.exists(file.path(FiguresDir, met_stn))) {
  dir.create(file.path(FiguresDir, met_stn))
}
resultsDir <- file.path(workDir, 'Results')
ssurgoDir2 <- 'C:/Users/smdevine/Desktop/post doc/Dahlke/RZWQM/ssurgo_profile_data/RZWQM input'
comp_ksat <- read.csv(file.path(ssurgoDir2, 'comp_data_RZWQMruns_Feb22.csv'), stringsAsFactors = FALSE)
#read-in results file
overall_results <- read.csv(file.path(resultsDir, 'Summaries', paste0('overall_results_', met_stn, '.csv')), stringsAsFactors = FALSE)
overall_results$SHRcolor <- ifelse(overall_results$SHR=='1. Coarse with no restrictions', 'lightgoldenrod', ifelse(overall_results$SHR=='2. Loamy with no restrictions', 'tan4', ifelse(overall_results$SHR=='7. Shrink-swell', 'violetred', NA)))
overall_results$SHR[overall_results$SHR=='1. Coarse with no restrictions'] <- 'Region 1'
overall_results$SHR[overall_results$SHR=='2. Loamy with no restrictions'] <- 'Region 2'
overall_results$SHR[overall_results$SHR=='7. Shrink-swell'] <- 'Region 7'
overall_results$ksat_cm_day <- comp_ksat$ksat_cm_day[match(overall_results$soil, comp_ksat$compnames)]
overall_results$clay <- comp_ksat$clay[match(overall_results$soil, comp_ksat$compnames)]
overall_results$silt <- comp_ksat$silt[match(overall_results$soil, comp_ksat$compnames)]
overall_results$sand <- comp_ksat$sand[match(overall_results$soil, comp_ksat$compnames)]
overall_results$theta_15b <- comp_ksat$theta_15b[match(overall_results$soil, comp_ksat$compnames)]
overall_results$theta_0.3b <- comp_ksat$theta_0.3b[match(overall_results$soil, comp_ksat$compnames)]
overall_results$microporosity <- comp_ksat$microporosity[match(overall_results$soil, comp_ksat$compnames)]
head(overall_results)

#look at distribution by SHR
# x <- overall_results
print_var <- function(x, scenario, var) {
  print('Region 1')
  print(x[[var]][x$SHR=='Region 1' & x$scenario==scenario][order(x[[var]][x$SHR=='Region 1' & x$scenario==scenario])])
  print(x$soil[x$SHR=='Region 1' & x$scenario==scenario][order(x[[var]][x$SHR=='Region 1' & x$scenario==scenario])])
  print('Region 2')
  print(x[[var]][x$SHR=='Region 2' & x$scenario==scenario][order(x[[var]][x$SHR=='Region 2' & x$scenario==scenario])])
  print(x$soil[x$SHR=='Region 2' & x$scenario==scenario][order(x[[var]][x$SHR=='Region 2' & x$scenario==scenario])])
  print('Region 7')
  print(x[[var]][x$SHR=='Region 7' & x$scenario==scenario][order(x[[var]][x$SHR=='Region 7' & x$scenario==scenario])])
  print(x$soil[x$SHR=='Region 7' & x$scenario==scenario][order(x[[var]][x$SHR=='Region 7' & x$scenario==scenario])])
}
colnames(overall_results)
print_var(overall_results, 'Jan7d', 'NO3_leached_kgN_ha')
print_var(overall_results, 'Control', 'NO3_leached_kgN_ha')

print_var(overall_results, 'Jan7d', 'NO3_leached_delta_kgN_ha')
print_var(overall_results, 'Mar7d', 'NO3_leached_delta_kgN_ha')
#aov analysis
unique(overall_results$scenario)
overall_results$AgMAR <- ifelse(grepl('Control', overall_results$scenario), 'Cntrl', 'AgMAR')
overall_results$AgMAR_timing <- ifelse(grepl('Mar', overall_results$scenario), 'Mar', ifelse(grepl('Jan', overall_results$scenario), 'Jan', ifelse(grepl('21d', overall_results$scenario), 'Jan_Mar', 'Cntrl')))
overall_results$AgMAR_freq <- ifelse(grepl('3d', overall_results$scenario), 'VHF', ifelse(grepl('7d', overall_results$scenario), 'HF', ifelse(grepl('21d', overall_results$scenario), 'LF', 'Cntrl')))
AgMAR_aov <- aov(NO3_leached_kgN_ha ~ soil + AgMAR * AgMAR_timing, data = overall_results)
replications(NO3_leached_kgN_ha ~ soil + AgMAR * AgMAR_timing, data = overall_results)
summary(AgMAR_aov)
TukeyHSD(AgMAR_aov, which = c('AgMAR', 'AgMAR_timing'))

#this is the way
AgMAR_aov <- aov(NO3_leached_kgN_ha ~ soil + scenario, data = overall_results)
summary(AgMAR_aov)
TukeyHSD(AgMAR_aov, which = c('scenario'))

AgMAR_aov <- aov(NO3_leached_delta_kgN_ha ~ SHR * scenario, data = overall_results)
AgMAR_aov
summary(AgMAR_aov)
TukeyHSD(AgMAR_aov, which = c('SHR', 'scenario'))

AgMAR_aov <- aov(NO3_leached_delta_kgN_ha ~ SHR + AgMAR_timing * AgMAR_freq, data = overall_results[!(overall_results$scenario %in% c('Control', '21d')),])
AgMAR_aov
summary(AgMAR_aov)
TukeyHSD(AgMAR_aov, which = c('AgMAR_timing', 'AgMAR_freq'))

#examine combined effect of Jan vs. Mar and 3d vs. 7d frequency
AgMAR_aov <- aov(NO3_leached_kgN_ha ~ soil + AgMAR_timing * AgMAR_freq, data = overall_results[!(overall_results$scenario %in% c('Control', '21d')),])
AgMAR_aov
summary(AgMAR_aov)
TukeyHSD(AgMAR_aov, which = c('AgMAR_timing', 'AgMAR_freq'))

library(car)
AgMAR_lm <- lm(NO3_leached_kgN_ha ~ soil + AgMAR_timing, data = overall_results, contrasts=list(AgMAR=contr.sum, AgMAR_timing=contr.sum))
Anova(AgMAR_lm, type=3)
alias(lm(NO3_leached_kgN_ha ~ soil + AgMAR * AgMAR_timing, data = overall_results, contrasts=list(AgMAR=contr.sum, AgMAR_timing=contr.sum)))


pairwise.t.test()
overall_results
