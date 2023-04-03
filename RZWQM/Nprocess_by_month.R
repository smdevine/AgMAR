monthlyFluxDir <- 'C:/Users/smdevine/Desktop/post doc/Dahlke/RZWQM/projects/PulseSoilClimate/ClimateRuns_Silage/Results/MonthlyFluxes'
summaryDir <- file.path(monthlyFluxDir, 'Aggregated')
met_stns <- c('Durham', 'Davis', 'Parlier', 'FivePoints', 'Shafter')
met_stns <- met_stns[order(met_stns)]
scenarios <- c('SteadyStateRuns', 'AgMAR_Jan3d', 'AgMAR_Jan7d', 'AgMAR_Mar3d', 'AgMAR_Mar7d', 'AgMAR_21d')
compnames <- c('Hanford', 'Delhi', 'Wasco', 'Hesperia', 'Milham', 'Tujunga', 'Panoche', 'Cerini', 'Yolo', 'Colpien', 'Tehama', 'Capay', 'Clear Lake', 'Tulare', 'Kimberlina', 'Columbia', 'Excelsior', 'Atwater', 'Rincon', 'Sycamore', 'Conejo', 'Westhaven', 'Pleito', 'Lokern', 'Merced', 'Cropley', 'Tachi', 'Myers', 'Lofgren', 'Wekoda', 'Guijarral', 'Granoso', 'Chanac')
compnames <- compnames[order(compnames)]
lapply(met_stns, function(x) length(list.files(file.path(monthlyFluxDir, x))))
# lapply(met_stns, function(x) dir.create(file.path(summaryDir, x)))
# lapply(met_stns, function(x) {
#   dir.create(file.path(summaryDir, x, 'SteadyStateRuns'))
#   dir.create(file.path(summaryDir, x, 'AgMAR_Jan3d'))
#   dir.create(file.path(summaryDir, x, 'AgMAR_Jan7d'))
#   dir.create(file.path(summaryDir, x, 'AgMAR_Mar3d'))
#   dir.create(file.path(summaryDir, x, 'AgMAR_Mar7d'))
#   dir.create(file.path(summaryDir, x, 'AgMAR_21d'))
# })
#aggregate data by stn x scenario and write to file
sapply(met_stns, function(x) {
  print(paste('Aggregating data for station', x))
  sapply(scenarios, function(y) {
    print(paste('Aggregating data for scenario', y))
    denitrification <- data.frame(month=1:12, stringsAsFactors = FALSE)
    mineralization <- data.frame(month=1:12, stringsAsFactors = FALSE)
    immobilization <- data.frame(month=1:12, stringsAsFactors = FALSE)
    net_min <- data.frame(month=1:12, stringsAsFactors = FALSE)
    leaching <- data.frame(month=1:12, stringsAsFactors = FALSE)
    volatilization <- data.frame(month=1:12, stringsAsFactors = FALSE)
    for(i in seq_along(compnames)) {
      result <- read.csv(file.path(monthlyFluxDir, x, paste0(compnames[i], '_', y, '_', x, '.csv')), row.names=1)
      denitrification <- cbind(denitrification, result$denitrification_kg_ha[1:12])
      mineralization <- cbind(mineralization, result$mineralization_kg_ha[1:12])
      immobilization <- cbind(immobilization, result$immobilization_kg_ha[1:12]) 
      net_min <- cbind(net_min, result$net_min_kg_ha[1:12])
      leaching <- cbind(leaching, result$leaching_kg_ha[1:12])
      volatilization <- cbind(volatilization, result$volatilization_kg_ha[1:12])
    }
    colnames(denitrification)[2:ncol(denitrification)] <- compnames
    colnames(mineralization)[2:ncol(mineralization)] <- compnames
    colnames(immobilization)[2:ncol(immobilization)] <- compnames
    colnames(net_min)[2:ncol(net_min)] <- compnames
    colnames(leaching)[2:ncol(leaching)] <- compnames
    colnames(volatilization)[2:ncol(volatilization)] <- compnames
    write.csv(denitrification, file.path(summaryDir, x, y, paste0('denitrification_', x, '_', y, '.csv')), row.names = FALSE)
    write.csv(mineralization, file.path(summaryDir, x, y, paste0('mineralization_', x, '_', y, '.csv')), row.names = FALSE)
    write.csv(denitrification, file.path(summaryDir, x, y, paste0('denitrification_', x, '_', y, '.csv')), row.names = FALSE)
    write.csv(immobilization, file.path(summaryDir, x, y, paste0('immobilization_', x, '_', y, '.csv')), row.names = FALSE)
    write.csv(net_min, file.path(summaryDir, x, y, paste0('net_min_', x, '_', y, '.csv')), row.names = FALSE)
    write.csv(volatilization, file.path(summaryDir, x, y, paste0('volatilization_', x, '_', y, '.csv')), row.names = FALSE)
    print(paste('Finished with scenario', y))
  })
})

