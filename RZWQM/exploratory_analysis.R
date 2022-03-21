#look at distribution by SHR
x <- overall_results
print_NO3_leached_delta <- function(x, scenario) {
  print('Region 1')
  print(x$NO3_leached_delta_kgN_ha[x$SHR=='Region 1' & x$scenario==scenario][order(x$NO3_leached_delta_kgN_ha[x$SHR=='Region 1' & x$scenario==scenario])])
  print(x$soil[x$SHR=='Region 1' & x$scenario==scenario][order(x$NO3_leached_delta_kgN_ha[x$SHR=='Region 1' & x$scenario==scenario])])
  print('Region 2')
  print(x$NO3_leached_delta_kgN_ha[x$SHR=='Region 2' & x$scenario==scenario][order(x$NO3_leached_delta_kgN_ha[x$SHR=='Region 2' & x$scenario==scenario])])
  print(x$soil[x$SHR=='Region 2' & x$scenario==scenario][order(x$NO3_leached_delta_kgN_ha[x$SHR=='Region 2' & x$scenario==scenario])])
  print('Region 7')
  print(x$NO3_leached_delta_kgN_ha[x$SHR=='Region 7' & x$scenario==scenario][order(x$NO3_leached_delta_kgN_ha[x$SHR=='Region 7' & x$scenario==scenario])])
  print(x$soil[x$SHR=='Region 7' & x$scenario==scenario][order(x$NO3_leached_delta_kgN_ha[x$SHR=='Region 7' & x$scenario==scenario])])
}
print_NO3_leached_delta(overall_results, 'Jan7d')

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
