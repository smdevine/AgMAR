#
climates <- c('Durham', 'Davis', 'Parlier', 'FivePoints', 'Shafter')
compnames <- c('Hanford', 'Delhi', 'Wasco', 'Hesperia', 'Milham', 'Tujunga', 'Panoche', 'Cerini', 'Yolo', 'Colpien', 'Tehama', 'Capay', 'Clear Lake', 'Tulare', 'Kimberlina', 'Columbia', 'Excelsior', 'Atwater', 'Rincon', 'Sycamore', 'Conejo', 'Westhaven', 'Pleito', 'Lokern', 'Merced', 'Cropley', 'Tachi', 'Myers', 'Lofgren', 'Wekoda')
compnames <- compnames[order(compnames)]
workDir
master_df <- read.csv(file.path(resultsDir, 'Summaries', paste0('overall_results_Parlier.csv')), stringsAsFactors = FALSE)
color_legend <- data.frame(soil=compnames, SHR=master_df$SHR[match(compnames, master_df$soil)])
color_legend$SHR_color <- ifelse(color_legend$SHR=='1. Coarse with no restrictions', 'lightgoldenrod', ifelse(color_legend$SHR=='2. Loamy with no restrictions', 'tan4', ifelse(color_legend$SHR=='7. Shrink-swell', 'violetred', NA)))

aggregate_all_climates <- function(var, scn, soilnames=compnames, clims=climates) {
  results_df <- data.frame(matrix(data=NA, nrow=length(clims), ncol = length(soilnames)))
  colnames(results_df) <- soilnames
  for(i in 1:length(climates)) {
    results_temp <- read.csv(file.path(resultsDir, 'Summaries', paste0('overall_results_', climates[i], '.csv')), stringsAsFactors = FALSE)
    results_temp   <- results_temp[order(results_temp$scenario, results_temp$soil),]
    # print(results_temp$soil)
    results_df[i,] <- results_temp[[var]][results_temp$scenario==scn]
  }
  results_df
}

Jan7d_NO3_delta <- aggregate_all_climates('NO3_leached_delta_kgN_ha', 'Jan7d')
Jan7d_NO3_delta

Mar7d_NO3_delta <- aggregate_all_climates('NO3_leached_delta_kgN_ha', 'Mar7d')

Mar7d_NO3_delta - Jan7d_NO3_delta
lapply(1:ncol(Jan7d_NO3_delta), function(i) {
  if(i==1) {
    plot(1:5, Jan7d_NO3_delta[,i], type = 'l', ylim = c(500, max(Jan7d_NO3_delta)), col=color_legend$SHR_color[match(colnames(Jan7d_NO3_delta)[i], color_legend$soil)])
  } else {lines(Jan7d_NO3_delta[,i], col=color_legend$SHR_color[match(colnames(Jan7d_NO3_delta)[i], color_legend$soil)])}
  if(Jan7d_NO3_delta[5,i]>550){
    text(x=5, y=Jan7d_NO3_delta[5,i], labels=colnames(Jan7d_NO3_delta)[i], pos=2, offset=0.1, cex=0.5)
  }
})
Jan7d_NO3_delta

master_df[master_df$soil=='',]
