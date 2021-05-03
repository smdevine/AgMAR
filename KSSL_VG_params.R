#to use ROSETTA, input data must be in this order: sand, silt, clay, bulk density, volumetric water content at 33kPa (1/3 bar), and volumetric water content at 1500kPa (15 bar)
library(soilDB)
workDir <- 'C:/Users/smdevine/Desktop/post doc/Dahlke/RZWQM/kssl_profile_data'
list.files(workDir)
fine_data <- read.csv(file.path(workDir, 'shrink_swell_median.csv'), stringsAsFactors = FALSE)
fine_data$w3cld <- fine_data$w3cld / 100
fine_data$w15l2 <- fine_data$w15l2 / 100
coarse_data <- read.csv(file.path(workDir, 'coarse_w_no_res_median.csv'), stringsAsFactors = FALSE)
coarse_data$w3cld <- coarse_data$w3cld / 100
coarse_data$w15l2 <- coarse_data$w15l2 / 100

loamy_data <- read.csv(file.path(workDir, 'loamy_w_no_res_median.csv'), stringsAsFactors = FALSE)
loamy_data$w3cld <- loamy_data$w3cld / 100
loamy_data$w15l2 <- loamy_data$w15l2 / 100

colnames(fine_data)
colnames(coarse_data)
colnames(loamy_data)

writeClipboard(as.character(fine_data[1,c('sand', 'silt', 'clay', 'db_13b', 'w3cld', 'w15l2')]))
VGparams_fine_v1 <- ROSETTA(x=fine_data, vars=c('sand', 'silt', 'clay', 'db_13b', 'w3cld', 'w15l2'), v='1')
VGparams_fine_v1$alpha <- 10^VGparams_fine_v1$alpha
VGparams_fine_v1$npar <- 10^VGparams_fine_v1$npar
VGparams_fine_v1$ksat <- 10^VGparams_fine_v1$ksat
VGparams_fine_v1[1,22:26]
write.csv(VGparams_fine_v1, file.path(workDir, 'ROSETTA_v1', 'fine_VGparams_v1.csv'), row.names=FALSE)

VGparams_fine_v2 <- ROSETTA(x=fine_data, vars=c('sand', 'silt', 'clay', 'db_13b', 'w3cld', 'w15l2'), v='2')
VGparams_fine_v2$alpha <- 10^VGparams_fine_v2$alpha
VGparams_fine_v2$npar <- 10^VGparams_fine_v2$npar
VGparams_fine_v2$ksat <- 10^VGparams_fine_v2$ksat
VGparams_fine_v2[1,22:26]
write.csv(VGparams_fine_v2, file.path(workDir, 'ROSETTA_v2', 'fine_VGparams_v2.csv'), row.names=FALSE)

VGparams_fine_v3 <- ROSETTA(x=fine_data, vars=c('sand', 'silt', 'clay', 'db_13b', 'w3cld', 'w15l2'), v='3')
VGparams_fine_v3$alpha <- 10^VGparams_fine_v3$alpha
VGparams_fine_v3$npar <- 10^VGparams_fine_v3$npar
VGparams_fine_v3$ksat <- 10^VGparams_fine_v3$ksat
VGparams_fine_v3[1,22:26]
write.csv(VGparams_fine_v3, file.path(workDir, 'ROSETTA_v3', 'fine_VGparams_v3.csv'), row.names=FALSE)

#loamy
VGparams_loamy_v1 <- ROSETTA(x=loamy_data, vars=c('sand', 'silt', 'clay', 'db_13b', 'w3cld', 'w15l2'), v='1')
VGparams_loamy_v1$alpha <- 10^VGparams_loamy_v1$alpha
VGparams_loamy_v1$npar <- 10^VGparams_loamy_v1$npar
VGparams_loamy_v1$ksat <- 10^VGparams_loamy_v1$ksat
VGparams_loamy_v1[1,22:26]
write.csv(VGparams_loamy_v1, file.path(workDir, 'ROSETTA_v1', 'loamy_VGparams_v1.csv'), row.names=FALSE)

VGparams_loamy_v2 <- ROSETTA(x=loamy_data, vars=c('sand', 'silt', 'clay', 'db_13b', 'w3cld', 'w15l2'), v='2')
VGparams_loamy_v2$alpha <- 10^VGparams_loamy_v2$alpha
VGparams_loamy_v2$npar <- 10^VGparams_loamy_v2$npar
VGparams_loamy_v2$ksat <- 10^VGparams_loamy_v2$ksat
VGparams_loamy_v2[1,22:26]
write.csv(VGparams_loamy_v2, file.path(workDir, 'ROSETTA_v2', 'loamy_VGparams_v2.csv'), row.names=FALSE)

VGparams_loamy_v3 <- ROSETTA(x=loamy_data, vars=c('sand', 'silt', 'clay', 'db_13b', 'w3cld', 'w15l2'), v='3')
VGparams_loamy_v3$alpha <- 10^VGparams_loamy_v3$alpha
VGparams_loamy_v3$npar <- 10^VGparams_loamy_v3$npar
VGparams_loamy_v3$ksat <- 10^VGparams_loamy_v3$ksat
VGparams_loamy_v3[1,22:26]
write.csv(VGparams_loamy_v3, file.path(workDir, 'ROSETTA_v3', 'loamy_VGparams_v3.csv'), row.names=FALSE)


#coarse
VGparams_coarse_v1 <- ROSETTA(x=coarse_data, vars=c('sand', 'silt', 'clay', 'db_13b', 'w3cld', 'w15l2'), v='1')
VGparams_coarse_v1$alpha <- 10^VGparams_coarse_v1$alpha
VGparams_coarse_v1$npar <- 10^VGparams_coarse_v1$npar
VGparams_coarse_v1$ksat <- 10^VGparams_coarse_v1$ksat
VGparams_coarse_v1[1,22:26]
write.csv(VGparams_coarse_v1, file.path(workDir, 'ROSETTA_v1', 'coarse_VGparams_v1.csv'), row.names=FALSE)

VGparams_coarse_v2 <- ROSETTA(x=coarse_data, vars=c('sand', 'silt', 'clay', 'db_13b', 'w3cld', 'w15l2'), v='2')
VGparams_coarse_v2$alpha <- 10^VGparams_coarse_v2$alpha
VGparams_coarse_v2$npar <- 10^VGparams_coarse_v2$npar
VGparams_coarse_v2$ksat <- 10^VGparams_coarse_v2$ksat
VGparams_coarse_v2[1,22:26]
write.csv(VGparams_coarse_v2, file.path(workDir, 'ROSETTA_v2', 'coarse_VGparams_v2.csv'), row.names=FALSE)

VGparams_coarse_v3 <- ROSETTA(x=coarse_data, vars=c('sand', 'silt', 'clay', 'db_13b', 'w3cld', 'w15l2'), v='3')
VGparams_coarse_v3$alpha <- 10^VGparams_coarse_v3$alpha
VGparams_coarse_v3$npar <- 10^VGparams_coarse_v3$npar
VGparams_coarse_v3$ksat <- 10^VGparams_coarse_v3$ksat
VGparams_coarse_v3[1,22:26]
write.csv(VGparams_coarse_v3, file.path(workDir, 'ROSETTA_v3', 'coarse_VGparams_v3.csv'), row.names=FALSE)
