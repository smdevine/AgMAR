library(aqp)
library(soilDB)
library(lattice)
library(extrafont)
library(extrafontdb)
loadfonts(device = 'win')
ksslDir <- 'C:/Users/smdevine/Desktop/post doc/soil health/kssl'
# FiguresDir <- '#'
resultsDir <- 'C:/Users/smdevine/Desktop/post doc/Dahlke/RZWQM/kssl_profile_data'
# Newville_data <- fetchKSSL(series='Newville')
# Newville_data
# site(Newville_data)
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

clus_7_names <- c('6. Fine salt-affected', '3. Low OM with restrictive horizons', '4. High OM with restrictive horizons', '1. Coarse with no restrictions', '2. Loamy with no restrictions', '7. Shrink-swell', '5. Coarse-loamy salt-affected')
clus_7_colors <- c('deepskyblue', 'olivedrab3', 'firebrick3', 'lightgoldenrod', 'tan4', 'violetred', 'lightblue1')
kssl_horizons_SHR <- read.csv(file.path(ksslDir, 'kssl_horizons_SHRonly.csv'), stringsAsFactors = FALSE)
horizons_to_exclude <- c('91P02043', '91P02044', '91P02045', '15N01738', '15N01739', '94P02038', '94P02028', '94P02018', '94P02019', '94P02020', '93P02017', '93P02018',  '91P04438', '91P04439', '91P04440', '91P04441', '91P04442', '91P04443', '91P04444', '91P04445', '88P01488', '85P05356', '79P01239', '79P01240', '79P01241', '79P01242', '79P01177', '79P00471', '79P00472', '40A22466', '40A22467', '40A22468', '87P02575', '87P02577', '87P02578', '87P02583', '94P00526', '94P00527', '94P00528', '94P00529', '83P01308', '88P01429', '40A23004', '93P01981', '93P01982', '88P01449')
sum(horizons_to_exclude %in% kssl_horizons_SHR$labsampnum) #these had been removed
colnames(kssl_horizons_SHR)
#fix a couple horizonation errors not previously detected (duplicate horizon data)
horizons_to_exclude <- c("88P01468", "88P01469", "16N03041" ,"40A23759", "40A23760", "16N03042")
kssl_horizons_SHR[kssl_horizons_SHR$pedon_key==73289,]
kssl_horizons_SHR <- kssl_horizons_SHR[!(kssl_horizons_SHR$labsampnum %in% horizons_to_exclude), ]

summary(kssl_horizons_SHR$oc) #497 NAs
summary(kssl_horizons_SHR$oc_est) #this was was produced in kssl_validation_FINAL.R
kssl_horizons_SHR$SHR7name <- clus_7_names[kssl_horizons_SHR$SHR7code]
kssl_horizons_SHR$SHR7name <- as.factor(kssl_horizons_SHR$SHR7name)
depths(kssl_horizons_SHR) <- pedon_key ~ hzn_top + hzn_bot
site(kssl_horizons_SHR) <- ~ SHR7name
kssl_horizons_SHR.slab <- slab(kssl_horizons_SHR, SHR7name ~ clay + silt + sand + frags + ph_h2o + ec_12pre + oc_est + n_tot + cec7 + ex_ca + ex_mg + ex_na + ex_k + caco3 + db_13b + w3cld + w15l2, slab.structure = 1, strict = TRUE) #in 15 cm increments, to produce 10 horizons from 0-150 cm
dim(kssl_horizons_SHR.slab)
str(kssl_horizons_SHR.slab)
kssl_horizons_SHR.slab <- kssl_horizons_SHR.slab[kssl_horizons_SHR.slab$top <=149,]
levels(kssl_horizons_SHR.slab$variable)
unique(kssl_horizons_SHR.slab$SHR7name)
colnames(kssl_horizons_SHR.slab)

writeCSV_bySHR <- function(SHR, stat, depth_top=c(0,10,30,50,75,100,125), depth_bot=c(10,30,50,75,100,125,150), fname) {
  df <- kssl_horizons_SHR.slab[kssl_horizons_SHR.slab$SHR7name==SHR,]
  result <- do.call(cbind, lapply(unique(as.character(kssl_horizons_SHR.slab$variable)), function(x) {
      y <- df[df$variable==x, stat]
      aggregate(y, by=list(do.call(c, sapply(seq_along(depth_bot), function(x) rep(x, times=(depth_bot - depth_top)[x]), simplify = TRUE))), FUN=mean, na.rm=TRUE)[,2]
  }))
  result <- as.data.frame(cbind(depth_top=depth_top, depth_bot=depth_bot, result))
  colnames(result)[3:ncol(result)] <- unique(as.character(kssl_horizons_SHR.slab$variable))
  texture_sums <- result$clay + result$silt + result$sand
  print(texture_sums)
  result$clay <- result$clay * 100 / texture_sums
  result$silt <- result$silt * 100 / texture_sums
  result$sand <- result$sand * 100 / texture_sums
  result$textural_class <- textural.class.calc(sand = result$sand, silt = result$silt, clay = result$clay)
  result$C_to_N <- round(result$oc_est / result$n_tot, 1)
  print(result)
  write.csv(result, file.path(resultsDir, fname), row.names = FALSE)
}
unique(kssl_horizons_SHR.slab$SHR7name)
writeCSV_bySHR(SHR='1. Coarse with no restrictions', stat='p.q50', fname = 'coarse_w_no_res_median.csv')
writeCSV_bySHR(SHR='2. Loamy with no restrictions', stat = 'p.q50', fname =  'loamy_w_no_res_median.csv')
writeCSV_bySHR(SHR='3. Low OM with restrictive horizons', stat='p.q50', fname= 'low_om_w__res_median.csv')
writeCSV_bySHR(SHR='4. High OM with restrictive horizons', stat='p.q50', fname = 'high_om_w__res_median.csv')
writeCSV_bySHR(SHR='5. Coarse-loamy salt-affected', stat='p.q50', fname= 'coarse_loamy_salt_aff_median.csv')
writeCSV_bySHR(SHR='6. Fine salt-affected', stat='p.q50', fname= 'fine_salt_aff_median.csv')
writeCSV_bySHR(SHR='7. Shrink-swell', stat='p.q50', fname= 'shrink_swell_median.csv')

#


kssl_horizons_SHR.slab[kssl_horizons_SHR.slab$SHR7name=='4. High OM with restrictive horizons',]

#stopped here
levels(kssl_horizons_SHR.slab$variable) <- c('Clay (%)', 'Soil pH', 'Soil EC', 'Soil organic carbon (%)')
tps <- list(superpose.line=list(col=clus_7_colors[order(clus_7_names)], lwd=2))


#plot
tiff(file = file.path(FiguresDir, 'CalAg', 'KSSL', 'soil_properties_KSSL_profiles.tif'), family = 'Times New Roman', width = 9, height = 5, pointsize = 12, units = 'in', res=800, compression='lzw')
xyplot(top ~ p.q50 | variable, groups=SHR7name, data=kssl_horizons_SHR.slab, ylab='Depth (cm)', xlab='KSSL validation data median bounded by 25th and 75th percentiles', lower=kssl_horizons_SHR.slab$p.q25, upper=kssl_horizons_SHR.slab$p.q75, ylim=c(155,-5), xlim = list(c(2,55), c(5.5, 8.9), c(0,12), c(0,2)), panel=panel.depth_function, alpha=0.4, sync.colors=TRUE, prepanel=prepanel.depth_function, par.strip.text=list(cex=0.8), strip=strip.custom(bg=grey(0.85)), layout=c(4,1), scales=list(x=list(alternating=1, relation='free'), y=list(alternating=3)), par.settings=tps, auto.key=list(columns=3, lines=TRUE, points=FALSE, lwd=2))
dev.off()
#key=list(text=list(levels(dframe$Z)), space='top', points=list(pch=1:nlevels(dframe$Z), col=col), lines=list(col=col), columns=nlevels(dframe$Z))

kssl_horizons_SHR.slab_v2 <- slab(kssl_horizons_SHR, SHR7name ~ clay + ph_h2o + oc_est, slab.structure = 1)
levels(kssl_horizons_SHR.slab_v2$variable) <- c('Clay (%)', 'Soil pH', 'Soil organic carbon (%)')
tiff(file = file.path(FiguresDir, 'CalAg', 'KSSL', 'soil_properties_KSSL_profiles_v2.tif'), family = 'Times New Roman', width = 9, height = 5, pointsize = 12, units = 'in', res=800, compression='lzw')
xyplot(top ~ p.q50 | variable, groups=SHR7name, data=kssl_horizons_SHR.slab_v2, ylab='Depth (cm)', xlab='KSSL validation data median bounded by 25th and 75th percentiles', lower=kssl_horizons_SHR.slab_v2$p.q25, upper=kssl_horizons_SHR.slab_v2$p.q75, ylim=c(155,-5), xlim = list(c(2,55), c(5.5, 8.9), c(0,2)), panel=panel.depth_function, alpha=0.4, sync.colors=TRUE, prepanel=prepanel.depth_function, par.strip.text=list(cex=0.8), strip=strip.custom(bg=grey(0.85)), layout=c(3,1), scales=list(x=list(alternating=1, relation='free'), y=list(alternating=3)), par.settings=tps, auto.key=list(columns=3, lines=TRUE, points=FALSE))
dev.off()

mean.and.sd <- function(values) {
  m <- mean(values, na.rm=TRUE)
  s <- sd(values, na.rm=TRUE)
  upper <- m + s
  lower <- m - s
  res <- c(mean=m, lower=lower, upper=upper)
  return(res)
}
kssl_horizons_SHR.slab_v3 <- slab(kssl_horizons_SHR, SHR7name ~ clay + ph_h2o + oc_est, slab.structure = 1, slab.fun = mean.and.sd)
levels(kssl_horizons_SHR.slab_v3$variable) <- c('Clay (%)', 'Soil pH', 'Soil organic carbon (%)')
tiff(file = file.path(FiguresDir, 'CalAg', 'KSSL', 'soil_properties_KSSL_profiles_v3.tif'), family = 'Times New Roman', width = 9, height = 5, pointsize = 12, units = 'in', res=800, compression='lzw')
xyplot(top ~ mean | variable, groups=SHR7name, data=kssl_horizons_SHR.slab_v3, ylab='Depth (cm)', xlab='KSSL validation data mean \u00B1 1 standard deviation', lower=kssl_horizons_SHR.slab_v3$lower, upper=kssl_horizons_SHR.slab_v3$upper, ylim=c(155,-5), xlim = list(c(2,55), c(5.5, 8.9), c(0,2)), panel=panel.depth_function, alpha=0.4, sync.colors=TRUE, prepanel=prepanel.depth_function, par.strip.text=list(cex=0.8), strip=strip.custom(bg=grey(0.85)), layout=c(3,1), scales=list(x=list(alternating=1, relation='free'), y=list(alternating=3)), par.settings=tps, auto.key=list(columns=3, lines=TRUE, points=FALSE))
dev.off()
