#can use N_MBL_monthly function to plot denitrification, mineralization, and nitrate stored curves during Flood-MAR
#add delta_orgN and total aboveground biomass to summary of Flood-MAR strategy effects, including overall effect by strategy, regardless of soil
met_stns <- c('Durham', 'Davis', 'Parlier', 'FivePoints', 'Shafter')
workDir <- 'C:/Users/smdevine/Desktop/post doc/Dahlke/RZWQM/projects/PulseSoilClimate/ClimateRuns_Silage/'
tablesDir <- 'C:/Users/smdevine/Desktop/post doc/Dahlke/RZWQM/publication/tables'
resultsDir <- file.path(workDir, 'Results')
met_stn <- met_stns[1]
# lapply(met_stns, function(x) dir.create(file.path(tablesDir, 'timing effects', x)))
overall_results <- read.csv(file.path(resultsDir, 'Summaries', paste0('overall_results_', met_stn, '.csv')), stringsAsFactors = FALSE)
colnames(overall_results)
rm(overall_results)
rm(met_stn)

CIupper <- function(mean_est, sd_est, n=11, probability=0.975) {
  mean_est + qt(p=probability, df=n-1) * sd_est / sqrt(n)
}
CIlower <-  function(mean_est, sd_est, n=11, probability=0.025) {
  mean_est + qt(p=probability, df=n-1) * sd_est / sqrt(n)
}

summarize_a_scenario <- function(met_no, scn, print_resNO3=FALSE, print_control=TRUE) {
  met_stn <- met_stns[met_no]
  print(met_stn)
  overall_results <- read.csv(file.path(resultsDir, 'Summaries', paste0('overall_results_', met_stn, '.csv')), stringsAsFactors = FALSE)
  if(print_resNO3) {
    print('These are the residual NO3 stats at the beginning of the scenario')
    print(tapply(overall_results$initial_kgNO3_ha[overall_results$scenario==scn], overall_results$taxpartclass_simplified[overall_results$scenario==scn], function(x) {x[order(x)]}))
    print(tapply(overall_results$initial_kgNO3_ha[overall_results$scenario==scn], overall_results$taxpartclass_simplified[overall_results$scenario==scn], summary))
}
  print(paste('These are the additional nitrate leaching risks for Flood-MAR scenario', scn))
  print(tapply(overall_results$NO3_leached_delta_kgN_ha[overall_results$scenario==scn], overall_results$taxpartclass_simplified[overall_results$scenario==scn], function(x) {x[order(x)]}))
  print(tapply(overall_results$NO3_leached_delta_kgN_ha[overall_results$scenario==scn], overall_results$taxpartclass_simplified[overall_results$scenario==scn], summary))
  if(print_control) {
    print(paste('These are the nitrate leached in business-as-usual:'))
    print(tapply(overall_results$NO3_leached_kgN_ha[overall_results$scenario=='Control'], overall_results$taxpartclass_simplified[overall_results$scenario=='Control'], summary))
  }
  print(paste('This is the total denitrification in business-as-usual by soil texture group:'))
  Control <- overall_results[overall_results$scenario=='Control',]
  Control <- Control[order(Control$soil),]
  print(tapply(Control$denitrification_kgN_ha, Control$taxpartclass_simplified, summary))
  print(paste('These is the total decrease in denitrification, comparing the', scn, 'Flood-MAr scenario with business-as-usual (negative indicates less denitrification with Flood-MAR), by soil texture group:'))
  Flood_MAR <- overall_results[overall_results$scenario==scn,]
  Flood_MAR[order(Flood_MAR$soil),]
  Control$denitrification_effect <- Flood_MAR$denitrification_kgN_ha - Control$denitrification_kgN_ha
  print(tapply(Control$denitrification_effect, Control$taxpartclass_simplified, summary))
}
summarize_a_scenario(met_no = 1, scn = '21d', print_resNO3 = TRUE)
summarize_a_scenario(met_no = 2, scn = '21d', print_resNO3 = TRUE)
summarize_a_scenario(met_no = 3, scn = '21d', print_resNO3 = TRUE)
summarize_a_scenario(met_no = 4, scn = '21d', print_resNO3 = TRUE)
summarize_a_scenario(met_no = 5, scn = '21d', print_resNO3 = TRUE)

summarize_a_strategy_effect <- function(met_no, writeResults=FALSE) {
  met_stn <- met_stns[met_no]
  print(met_stn)
  overall_results <- read.csv(file.path(resultsDir, 'Summaries', paste0('overall_results_', met_stn, '.csv')), stringsAsFactors = FALSE)
  overall_results$delta_resNO3 <- overall_results$final_kgNO3_ha - overall_results$initial_kgNO3_ha #thus, positive is accumulation
  Jan3d <- overall_results[overall_results$scenario=='Jan3d',]
  Jan3d <- Jan3d[order(Jan3d$soil), ]
  Jan7d <- overall_results[overall_results$scenario=='Jan7d',]
  Jan7d <- Jan7d[order(Jan7d$soil), ]
  Jan7d$leaching_freq_effect <-  Jan7d$NO3_leached_delta_kgN_ha - Jan3d$NO3_leached_delta_kgN_ha
  print('These are the Jan 3d frequency vs. Jan 7d frequency effects (negative means 3d frequency leached more), by textural group:')
  # print(Jan_3d_vs_7d)
  # print(summary(Jan_3d_vs_7d))
  print(tapply(Jan7d$leaching_freq_effect, Jan7d$taxpartclass_simplified, summary))
  print('These are the Jan 3d frequency vs. Jan 7d frequency effects (negative means 3d frequency leached more), overall:')
  print(summary(Jan7d$leaching_freq_effect))
  Mar3d <- overall_results[overall_results$scenario=='Mar3d',]
  Mar3d <- Mar3d[order(Mar3d$soil),]
  Mar7d <- overall_results[overall_results$scenario=='Mar7d',]
  Mar7d <- Mar7d[order(Mar7d$soil),]
  # Mar_3d_vs_7d <- Mar7d$NO3_leached_delta_kgN_ha - Mar3d$NO3_leached_delta_kgN_ha
  Mar7d$leaching_freq_effect <- Mar7d$NO3_leached_delta_kgN_ha - Mar3d$NO3_leached_delta_kgN_ha
  print('These are the Mar 3d frequency vs. Mar 7d frequency effects (negative means 3d frequency leached more), by textural group:')
  print(tapply(Mar7d$leaching_freq_effect, Mar7d$taxpartclass_simplified, summary))
  print('These are the Mar 3d frequency vs. Mar 7d frequency effects (negative means 3d frequency leached more), overall:')
  print(summary(Mar7d$leaching_freq_effect))
  
  Jan7d$Jan_NO3_leached_delta_kgN_ha <- apply(cbind(Jan7d$NO3_leached_delta_kgN_ha, Jan3d$NO3_leached_delta_kgN_ha), 1, mean)
  Jan7d$Jan_final_delta_orgN <- apply(cbind(Jan7d$delta_orgN, Jan3d$delta_orgN), 1, mean)
  Jan7d$Jan_net_min <- apply(cbind(Jan7d$net_min_kgN_ha, Jan3d$net_min_kgN_ha), 1, mean)
  Jan7d$Jan_denitrification <- apply(cbind(Jan7d$denitrification_kgN_ha, Jan3d$denitrification_kgN_ha), 1, mean)
  Jan7d$Jan_delta_resNO3 <- apply(cbind(Jan7d$delta_resNO3, Jan3d$delta_resNO3), 1, mean)
  Mar7d$Mar_NO3_leached_delta_kgN_ha <- apply(cbind(Mar7d$NO3_leached_delta_kgN_ha, Mar3d$NO3_leached_delta_kgN_ha), 1, mean)
  Mar7d$Mar_final_delta_orgN <- apply(cbind(Mar7d$delta_orgN, Mar3d$delta_orgN), 1, mean)
  Mar7d$Mar_net_min <- apply(cbind(Mar7d$net_min_kgN_ha, Mar3d$net_min_kgN_ha), 1, mean)
  Mar7d$Mar_denitrification <- apply(cbind(Mar7d$denitrification_kgN_ha, Mar3d$denitrification_kgN_ha), 1, mean)
  Mar7d$Mar_delta_resNO3 <- apply(cbind(Mar7d$delta_resNO3, Mar3d$delta_resNO3), 1, mean)
  Mar7d$late_vs_early_effect_NO3 <- Mar7d$Mar_NO3_leached_delta_kgN_ha - Jan7d$Jan_NO3_leached_delta_kgN_ha
  Mar7d$late_vs_early_effect_delta_orgN <- Mar7d$Mar_final_delta_orgN - Jan7d$Jan_final_delta_orgN
  Mar7d$late_vs_early_effect_net_min <- Mar7d$Mar_net_min - Jan7d$Jan_net_min
  Mar7d$late_vs_early_denitrification <- Mar7d$Mar_denitrification - Jan7d$Jan_denitrification
  Mar7d$late_vs_early_delta_resNO3 <- Mar7d$Mar_delta_resNO3 - Jan7d$delta_resNO3
  
  print(paste('These are the early (Jan) vs. late (Mar) effects of Flood-MAR strategy on additional nitrate leaching risks (negative means Jan leached more), by textural group:'))
  print(tapply(Mar7d$late_vs_early_effect_NO3, Mar7d$taxpartclass_simplified, summary))
  print(paste('These are the early (Jan) vs. late (Mar) effects of Flood-MAR strategy on additional nitrate leaching risks (negative means Jan leached more), overall:'))
  print(summary(Mar7d$late_vs_early_effect_NO3))
  
  print(paste('These are the early (Jan) vs. late (Mar) effects of Flood-MAR strategy on organic N (negative means Jan accumulated more SON), by textural group:'))
  print(tapply(Mar7d$late_vs_early_effect_delta_orgN, Mar7d$taxpartclass_simplified, summary))
  print(paste('These are the early (Jan) vs. late (Mar) effects of Flood-MAR strategy on organic N (negative means Jan accumulated more SON), overall:'))
  print(summary(Mar7d$late_vs_early_effect_delta_orgN))
  
  print(paste('These are the early (Jan) vs. late (Mar) effects of Flood-MAR strategy on net mineralization (negative means Jan had more net min), by textural group:'))
  print(tapply(Mar7d$late_vs_early_effect_net_min, Mar7d$taxpartclass_simplified, summary))
  print(paste('These are the early (Jan) vs. late (Mar) effects of Flood-MAR strategy on net mineralization (negative means Jan had more net min), overall:'))
  print(summary(Mar7d$late_vs_early_effect_net_min))
  
  print(paste('These are the early (Jan) vs. late (Mar) effects of Flood-MAR strategy on denitrification (negative means Jan had more denitrification), by textural group:'))
  print(tapply(Mar7d$late_vs_early_denitrification, Mar7d$taxpartclass_simplified, summary))
  print(paste('These are the early (Jan) vs. late (Mar) effects of Flood-MAR strategy on denitrification (negative means Jan had more denitrification), overall:'))
  print(summary(Mar7d$late_vs_early_denitrification))
  if(writeResults) {
    #leaching effect is calc'd above
    if(!all(Jan7d$soil==Jan3d$soil)){stop('Error, soils are not in order!')}
    Jan7d$denitrification_effect <- Jan7d$denitrification_kgN_ha - Jan3d$denitrification_kgN_ha
    Jan7d$net_min_effect <- Jan7d$net_min_kgN_ha - Jan3d$net_min_kgN_ha
    Jan7d$delta_orgN_effect <- Jan7d$delta_orgN - Jan3d$delta_orgN
    Jan7d$delta_resNO3_effect <- Jan7d$delta_resNO3 - Jan3d$delta_resNO3
    Jan3d_vs_Jan7d <- data.frame(soil=c(1:3, 'overall'), addNO3leached_mean=NA, addNO3leached_sd=NA, addNO3leached_upperCI=NA, addNO3leached_lowerCI=NA, denitrification_mean=NA, denitrification_sd=NA, denitrification_upperCI=NA, denitrification_lowerCI=NA, net_min_mean=NA, net_min_sd=NA, net_min_upperCI=NA, net_min_lowerCI=NA, delta_orgN_mean=NA, delta_orgN_sd=NA, delta_orgN_upperCI=NA, delta_orgN_lowerCI=NA, delta_resNO3_mean=NA, delta_resNO3_sd=NA, delta_resNO3_upperCI=NA, delta_resNO3_lowerCI=NA)
    Jan3d_vs_Jan7d$addNO3leached_mean <- c(tapply(Jan7d$leaching_freq_effect, Jan7d$taxpartclass_simplified, mean), mean(Jan7d$leaching_freq_effect))
    Jan3d_vs_Jan7d$addNO3leached_sd <- c(tapply(Jan7d$leaching_freq_effect, Jan7d$taxpartclass_simplified, sd), sd(Jan7d$leaching_freq_effect))
    Jan3d_vs_Jan7d$addNO3leached_upperCI <- CIupper(mean_est = Jan3d_vs_Jan7d$addNO3leached_mean, sd_est = Jan3d_vs_Jan7d$addNO3leached_sd, n=c(rep(11, 3), 33))
    Jan3d_vs_Jan7d$addNO3leached_lowerCI <- CIlower(mean_est = Jan3d_vs_Jan7d$addNO3leached_mean, sd_est = Jan3d_vs_Jan7d$addNO3leached_sd, n=c(rep(11, 3), 33))
    Jan3d_vs_Jan7d$denitrification_mean <- c(tapply(Jan7d$denitrification_effect, Jan7d$taxpartclass_simplified, mean), mean(Jan7d$denitrification_effect))
    Jan3d_vs_Jan7d$denitrification_sd <- c(tapply(Jan7d$denitrification_effect, Jan7d$taxpartclass_simplified, sd), sd(Jan7d$denitrification_effect))
    Jan3d_vs_Jan7d$denitrification_upperCI <- CIupper(mean_est = Jan3d_vs_Jan7d$denitrification_mean, sd_est = Jan3d_vs_Jan7d$denitrification_sd, n=c(rep(11, 3), 33))
    Jan3d_vs_Jan7d$denitrification_lowerCI <- CIlower(mean_est = Jan3d_vs_Jan7d$denitrification_mean, sd_est = Jan3d_vs_Jan7d$denitrification_sd, n=c(rep(11, 3), 33))
    Jan3d_vs_Jan7d$net_min_mean <- c(tapply(Jan7d$net_min_effect, Jan7d$taxpartclass_simplified, mean), mean(Jan7d$net_min_effect))
    Jan3d_vs_Jan7d$net_min_sd <- c(tapply(Jan7d$net_min_effect, Jan7d$taxpartclass_simplified, sd), sd(Jan7d$net_min_effect))
    Jan3d_vs_Jan7d$net_min_upperCI <- CIupper(mean_est = Jan3d_vs_Jan7d$net_min_mean, sd_est = Jan3d_vs_Jan7d$net_min_sd, n=c(rep(11, 3), 33))
    Jan3d_vs_Jan7d$net_min_lowerCI <- CIlower(mean_est = Jan3d_vs_Jan7d$net_min_mean, sd_est = Jan3d_vs_Jan7d$net_min_sd, n=c(rep(11, 3), 33))
    Jan3d_vs_Jan7d$delta_orgN_mean <- c(tapply(Jan7d$delta_orgN_effect, Jan7d$taxpartclass_simplified, mean), mean(Jan7d$delta_orgN_effect))
    Jan3d_vs_Jan7d$delta_orgN_sd <- c(tapply(Jan7d$delta_orgN_effect, Jan7d$taxpartclass_simplified, sd), sd(Jan7d$delta_orgN_effect))
    Jan3d_vs_Jan7d$delta_orgN_upperCI <- CIupper(mean_est = Jan3d_vs_Jan7d$delta_orgN_mean, sd_est = Jan3d_vs_Jan7d$delta_orgN_sd, n=c(rep(11, 3), 33))
    Jan3d_vs_Jan7d$delta_orgN_lowerCI <- CIlower(mean_est = Jan3d_vs_Jan7d$delta_orgN_mean, sd_est = Jan3d_vs_Jan7d$delta_orgN_sd, n=c(rep(11, 3), 33))
    Jan3d_vs_Jan7d$delta_resNO3_mean <- c(tapply(Jan7d$delta_resNO3_effect, Jan7d$taxpartclass_simplified, mean), mean(Jan7d$delta_resNO3_effect))
    Jan3d_vs_Jan7d$delta_resNO3_sd <- c(tapply(Jan7d$delta_resNO3_effect, Jan7d$taxpartclass_simplified, sd), sd(Jan7d$delta_resNO3_effect))
    Jan3d_vs_Jan7d$delta_resNO3_upperCI <- CIupper(mean_est = Jan3d_vs_Jan7d$delta_resNO3_mean, sd_est = Jan3d_vs_Jan7d$delta_resNO3_sd, n=c(rep(11, 3), 33))
    Jan3d_vs_Jan7d$delta_resNO3_lowerCI <- CIlower(mean_est = Jan3d_vs_Jan7d$delta_resNO3_mean, sd_est = Jan3d_vs_Jan7d$delta_resNO3_sd, n=c(rep(11, 3), 33))
    Jan3d_vs_Jan7d <- cbind(soil=Jan3d_vs_Jan7d[,1], round(Jan3d_vs_Jan7d[,2:ncol(Jan3d_vs_Jan7d)], 3))
    
    if(!all(Mar7d$soil==Mar3d$soil)){stop('Error, soils are not in order!')}
    Mar7d$denitrification_effect <- Mar7d$denitrification_kgN_ha - Mar3d$denitrification_kgN_ha
    Mar7d$net_min_effect <- Mar7d$net_min_kgN_ha - Mar3d$net_min_kgN_ha
    Mar7d$delta_orgN_effect <- Mar7d$delta_orgN - Mar3d$delta_orgN
    Mar7d$delta_resNO3_effect <- Mar7d$delta_resNO3 - Mar3d$delta_resNO3
    Mar3d_vs_Mar7d <- data.frame(soil=c(1:3, 'overall'), addNO3leached_mean=NA, addNO3leached_sd=NA, addNO3leached_upperCI=NA, addNO3leached_lowerCI=NA, denitrification_mean=NA, denitrification_sd=NA, denitrification_upperCI=NA, denitrification_lowerCI=NA, net_min_mean=NA, net_min_sd=NA, net_min_upperCI=NA, net_min_lowerCI=NA, delta_orgN_mean=NA, delta_orgN_sd=NA, delta_orgN_upperCI=NA, delta_orgN_lowerCI=NA, delta_resNO3_mean=NA, delta_resNO3_sd=NA, delta_resNO3_upperCI=NA, delta_resNO3_lowerCI=NA)
    Mar3d_vs_Mar7d$addNO3leached_mean <- c(tapply(Mar7d$leaching_freq_effect, Mar7d$taxpartclass_simplified, mean), mean(Mar7d$leaching_freq_effect))
    Mar3d_vs_Mar7d$addNO3leached_sd <- c(tapply(Mar7d$leaching_freq_effect, Mar7d$taxpartclass_simplified, sd), sd(Mar7d$leaching_freq_effect))
    Mar3d_vs_Mar7d$addNO3leached_upperCI <- CIupper(mean_est = Mar3d_vs_Mar7d$addNO3leached_mean, sd_est = Mar3d_vs_Mar7d$addNO3leached_sd, n=c(rep(11, 3), 33))
    Mar3d_vs_Mar7d$addNO3leached_lowerCI <- CIlower(mean_est = Mar3d_vs_Mar7d$addNO3leached_mean, sd_est = Mar3d_vs_Mar7d$addNO3leached_sd, n=c(rep(11, 3), 33))
    Mar3d_vs_Mar7d$denitrification_mean <- c(tapply(Mar7d$denitrification_effect, Mar7d$taxpartclass_simplified, mean), mean(Mar7d$denitrification_effect))
    Mar3d_vs_Mar7d$denitrification_sd <- c(tapply(Mar7d$denitrification_effect, Mar7d$taxpartclass_simplified, sd), sd(Mar7d$denitrification_effect))
    Mar3d_vs_Mar7d$denitrification_upperCI <- CIupper(mean_est = Mar3d_vs_Mar7d$denitrification_mean, sd_est = Mar3d_vs_Mar7d$denitrification_sd, n=c(rep(11, 3), 33))
    Mar3d_vs_Mar7d$denitrification_lowerCI <- CIlower(mean_est = Mar3d_vs_Mar7d$denitrification_mean, sd_est = Mar3d_vs_Mar7d$denitrification_sd, n=c(rep(11, 3), 33))
    Mar3d_vs_Mar7d$net_min_mean <- c(tapply(Mar7d$net_min_effect, Mar7d$taxpartclass_simplified, mean), mean(Mar7d$net_min_effect))
    Mar3d_vs_Mar7d$net_min_sd <- c(tapply(Mar7d$net_min_effect, Mar7d$taxpartclass_simplified, sd), sd(Mar7d$net_min_effect))
    Mar3d_vs_Mar7d$net_min_upperCI <- CIupper(mean_est = Mar3d_vs_Mar7d$net_min_mean, sd_est = Mar3d_vs_Mar7d$net_min_sd, n=c(rep(11, 3), 33))
    Mar3d_vs_Mar7d$net_min_lowerCI <- CIlower(mean_est = Mar3d_vs_Mar7d$net_min_mean, sd_est = Mar3d_vs_Mar7d$net_min_sd, n=c(rep(11, 3), 33))
    Mar3d_vs_Mar7d$delta_orgN_mean <- c(tapply(Mar7d$delta_orgN_effect, Mar7d$taxpartclass_simplified, mean), mean(Mar7d$delta_orgN_effect))
    Mar3d_vs_Mar7d$delta_orgN_sd <- c(tapply(Mar7d$delta_orgN_effect, Mar7d$taxpartclass_simplified, sd), sd(Mar7d$delta_orgN_effect))
    Mar3d_vs_Mar7d$delta_orgN_upperCI <- CIupper(mean_est = Mar3d_vs_Mar7d$delta_orgN_mean, sd_est = Mar3d_vs_Mar7d$delta_orgN_sd, n=c(rep(11, 3), 33))
    Mar3d_vs_Mar7d$delta_orgN_lowerCI <- CIlower(mean_est = Mar3d_vs_Mar7d$delta_orgN_mean, sd_est = Mar3d_vs_Mar7d$delta_orgN_sd, n=c(rep(11, 3), 33))
    Mar3d_vs_Mar7d$delta_resNO3_mean <- c(tapply(Mar7d$delta_resNO3_effect, Mar7d$taxpartclass_simplified, mean), mean(Mar7d$delta_resNO3_effect))
    Mar3d_vs_Mar7d$delta_resNO3_sd <- c(tapply(Mar7d$delta_resNO3_effect, Mar7d$taxpartclass_simplified, sd), sd(Mar7d$delta_resNO3_effect))
    Mar3d_vs_Mar7d$delta_resNO3_upperCI <- CIupper(mean_est = Mar3d_vs_Mar7d$delta_resNO3_mean, sd_est = Mar3d_vs_Mar7d$delta_resNO3_sd, n=c(rep(11, 3), 33))
    Mar3d_vs_Mar7d$delta_resNO3_lowerCI <- CIlower(mean_est = Mar3d_vs_Mar7d$delta_resNO3_mean, sd_est = Mar3d_vs_Mar7d$delta_resNO3_sd, n=c(rep(11, 3), 33))
    Mar3d_vs_Mar7d <- cbind(soil=Mar3d_vs_Mar7d[,1], round(Mar3d_vs_Mar7d[,2:ncol(Mar3d_vs_Mar7d)], 3))
    
    #finish adding CIs for this block
    Jan_vs_Mar <- data.frame(soil=c(1:3, 'overall'), addNO3leached_mean=NA, addNO3leached_sd=NA, addNO3leached_upperCI=NA, addNO3leached_lowerCI=NA, denitrification_mean=NA, denitrification_sd=NA, denitrification_upperCI=NA, denitrification_lowerCI=NA, net_min_mean=NA, net_min_sd=NA, net_min_upperCI=NA, net_min_lowerCI=NA, delta_orgN_mean=NA, delta_orgN_sd=NA, delta_orgN_upperCI=NA, delta_orgN_lowerCI=NA, delta_resNO3_mean=NA, delta_resNO3_sd=NA, delta_resNO3_upperCI=NA, delta_resNO3_lowerCI=NA)
    Jan_vs_Mar$addNO3leached_mean <- c(tapply(Mar7d$late_vs_early_effect_NO3, Mar7d$taxpartclass_simplified, mean), mean(Mar7d$late_vs_early_effect_NO3))
    Jan_vs_Mar$addNO3leached_sd <- c(tapply(Mar7d$late_vs_early_effect_NO3, Mar7d$taxpartclass_simplified, sd), sd(Mar7d$late_vs_early_effect_NO3))
    Jan_vs_Mar$addNO3leached_upperCI <- CIupper(mean_est = Jan_vs_Mar$addNO3leached_mean, sd_est = Jan_vs_Mar$addNO3leached_sd, n=c(rep(11, 3), 33))
    Jan_vs_Mar$addNO3leached_lowerCI <- CIlower(mean_est = Jan_vs_Mar$addNO3leached_mean, sd_est = Jan_vs_Mar$addNO3leached_sd, n=c(rep(11, 3), 33))
    
    Jan_vs_Mar$denitrification_mean <- c(tapply(Mar7d$late_vs_early_denitrification, Mar7d$taxpartclass_simplified, mean), mean(Mar7d$late_vs_early_denitrification))
    Jan_vs_Mar$denitrification_sd <- c(tapply(Mar7d$late_vs_early_denitrification, Mar7d$taxpartclass_simplified, sd), sd(Mar7d$late_vs_early_denitrification))
    Jan_vs_Mar$denitrification_upperCI <- CIupper(mean_est = Jan_vs_Mar$denitrification_mean, sd_est = Jan_vs_Mar$denitrification_sd, n=c(rep(11, 3), 33))
    Jan_vs_Mar$denitrification_lowerCI <- CIlower(mean_est = Jan_vs_Mar$denitrification_mean, sd_est = Jan_vs_Mar$denitrification_sd, n=c(rep(11, 3), 33))
    
    Jan_vs_Mar$net_min_mean <- c(tapply(Mar7d$late_vs_early_effect_net_min, Mar7d$taxpartclass_simplified, mean), mean(Mar7d$late_vs_early_effect_net_min))
    Jan_vs_Mar$net_min_sd <- c(tapply(Mar7d$late_vs_early_effect_net_min, Mar7d$taxpartclass_simplified, sd), sd(Mar7d$late_vs_early_effect_net_min))
    Jan_vs_Mar$net_min_upperCI <- CIupper(mean_est = Jan_vs_Mar$net_min_mean, sd_est = Jan_vs_Mar$net_min_sd, n=c(rep(11, 3), 33))
    Jan_vs_Mar$net_min_lowerCI <- CIlower(mean_est = Jan_vs_Mar$net_min_mean, sd_est = Jan_vs_Mar$net_min_sd, n=c(rep(11, 3), 33))
    
    Jan_vs_Mar$delta_orgN_mean <- c(tapply(Mar7d$late_vs_early_effect_delta_orgN, Mar7d$taxpartclass_simplified, mean), mean(Mar7d$late_vs_early_effect_delta_orgN))
    Jan_vs_Mar$delta_orgN_sd <- c(tapply(Mar7d$late_vs_early_effect_delta_orgN, Mar7d$taxpartclass_simplified, sd), sd(Mar7d$late_vs_early_effect_delta_orgN))
    Jan_vs_Mar$delta_orgN_upperCI <- CIupper(mean_est = Jan_vs_Mar$delta_orgN_mean, sd_est = Jan_vs_Mar$delta_orgN_sd, n=c(rep(11, 3), 33))
    Jan_vs_Mar$delta_orgN_lowerCI <- CIlower(mean_est = Jan_vs_Mar$delta_orgN_mean, sd_est = Jan_vs_Mar$delta_orgN_sd, n=c(rep(11, 3), 33))
    
    Jan_vs_Mar$delta_resNO3_mean <- c(tapply(Mar7d$late_vs_early_delta_resNO3, Mar7d$taxpartclass_simplified, mean), mean(Mar7d$late_vs_early_delta_resNO3))
    Jan_vs_Mar$delta_resNO3_sd <- c(tapply(Mar7d$late_vs_early_delta_resNO3, Mar7d$taxpartclass_simplified, sd), sd(Mar7d$late_vs_early_delta_resNO3))
    Jan_vs_Mar$delta_resNO3_upperCI <- CIupper(mean_est = Jan_vs_Mar$delta_resNO3_mean, sd_est = Jan_vs_Mar$delta_resNO3_sd, n=c(rep(11, 3), 33))
    Jan_vs_Mar$delta_resNO3_lowerCI <- CIlower(mean_est = Jan_vs_Mar$delta_resNO3_mean, sd_est = Jan_vs_Mar$delta_resNO3_sd, n=c(rep(11, 3), 33))
    
    write.csv(Jan3d_vs_Jan7d, file.path(tablesDir, 'timing effects', met_stn, paste0(met_stn, '_Jan3d_vs_Jan7d_summary.csv')), row.names=FALSE)
    write.csv(Mar3d_vs_Mar7d, file.path(tablesDir, 'timing effects', met_stn, paste0(met_stn, '_Mar3d_vs_Mar7d_summary.csv')), row.names=FALSE)
    write.csv(Jan_vs_Mar, file.path(tablesDir, 'timing effects', met_stn, paste0(met_stn, '_Jan_vs_Mar_summary.csv')), row.names=FALSE)
  }
}
summarize_a_strategy_effect(1, writeResults = TRUE)
summarize_a_strategy_effect(2, writeResults = TRUE)
summarize_a_strategy_effect(3, writeResults = TRUE)
summarize_a_strategy_effect(4, writeResults = TRUE)
summarize_a_strategy_effect(5, writeResults = TRUE)

#from napa_lodi_SOM_FINAL.R
#see confidence interval formula for unknown mean and standard deviation at http://www.stat.yale.edu/Courses/1997-98/101/confint.htm
plot_da_error <- function(barnum, barcenters, df) {
  segments(x0=barcenters[barnum,], y0=df$means[barnum] + qt(p=0.975, df=df$n[barnum]-1) * df$sd[barnum] / sqrt(df$n[barnum]), x1=barcenters[barnum,], y1=df$means[barnum] + qt(p=0.025, df=df$n[barnum]-1) * df$sd[barnum] / sqrt(df$n[barnum]), lwd = 1.2)
}
# http://www.stat.yale.edu/Courses/1997-98/101/confint.htm