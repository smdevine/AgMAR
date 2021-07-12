library(extrafont)
library(extrafontdb)
# font_import() #only needs to be done one time after updating and re-installing R and moving and updating packages
loadfonts(device = 'win')
workDir <- 'C:/Users/smdevine/Desktop/post doc/Dahlke/RZWQM/projects/tests'
FiguresDir <- 'C:/Users/smdevine/Desktop/post doc/Dahlke/RZWQM/summaries/Figures'

#for scenario names:
#v1 is soil properties from textural class look-up tables in RZWQM
#v2 is soil properties from KSSL and Rosetta-estimated Ks and theta at 0.33 bar and 15 bars
#v3 is soil properties, including Ks and moisture retention, from SSURGO

cumulativeFluxes <- function(projectName, soil) {
  scenarioDirs <- list.dirs(file.path(workDir, projectName), recursive = FALSE)
  result <- lapply(scenarioDirs, function(x) {
  leachingDF <- read.table(file.path(x, soil, 'CLEACH.OUT'), col.names = c('DAY', 'H', 'CA', 'NA', 'MG', 'CL', 'HCO3', 'SO4', 'AL', 'NO3-N', 'NH4-N', 'CO3', 'UREA-N', 'PEST #1', 'PEST #2', 'PEST #3'), header = FALSE, skip=6)
  leachingDF$date <- seq(as.Date("2016/10/1"), as.Date("2017/4/30"), "days")
  leachingDF
  })
  names(result) <- basename(scenarioDirs)
  result
}

CoarseSHR_SSURGO_10ppm <- cumulativeFluxes('10ppmNO3', 'CoarseSHR_v2')
LoamySHR_SSURGO_10ppm <- cumulativeFluxes('10ppmNO3', 'LoamySHR_v2')
FineSHR_SSURGO_10ppm <- cumulativeFluxes('10ppmNO3', 'FineSHR_v2')

CoarseSHR_SSURGO_30ppm <- cumulativeFluxes('30ppmNO3', 'CoarseSHR_v2')
LoamySHR_SSURGO_30ppm <- cumulativeFluxes('30ppmNO3', 'LoamySHR_v2')
FineSHR_SSURGO_30ppm <- cumulativeFluxes('30ppmNO3', 'FineSHR_v2')

CoarseSHR_KSSL_10ppm <- cumulativeFluxes('10ppmNO3', 'CoarseSHR_v1')
LoamySHR_KSSL_10ppm <- cumulativeFluxes('10ppmNO3', 'LoamySHR_v1')
FineSHR_KSSL_10ppm <- cumulativeFluxes('10ppmNO3', 'FineSHR_v1')

CoarseSHR_KSSL_30ppm <- cumulativeFluxes('30ppmNO3', 'CoarseSHR_v1')
LoamySHR_KSSL_30ppm <- cumulativeFluxes('30ppmNO3', 'LoamySHR_v1')
FineSHR_KSSL_30ppm <- cumulativeFluxes('30ppmNO3', 'FineSHR_v1')

# soilName <- CoarseSHR_SSURGO_10ppm
# ymax <- 120
# ResidualNO3 <- 68.4
makeFigure <- function(fname, soilName, ymax, makeLegend=FALSE, ResidualNO3, FigLab, FigLabHt, width, startime, xmax) {
  tiff(file = file.path(FiguresDir, fname), family = 'Times New Roman', width = width, height = 3.5, pointsize = 12, units = 'in', res=800, compression = 'lzw')
  par(mar=c(2,3.5,0.25,0.25))
  days <- nrow(soilName$`Control CC`)
  plot(soilName$`Control CC`$date, soilName$`Flood-MAR Fallow LF`$NO3.N/10, type = 'l', ylab = '', xlab = '', col='red', ylim = c(0, ymax), xlim = c(as.Date(startime), xmax), lty=2)
  mtext(expression('NO'[3]*'-N leached (kg ha'^-1*')'), side = 2, line=2.25)
  lines(x=c(as.Date('2017-05-02'), seq(as.Date('2017-05-02'), as.Date('2017-05-05'), 'days')), y=c(0,rep(ResidualNO3, 3), 0), type='s', col='orange', lty=1)
  lines(soilName$`Control CC`$date, soilName$`Flood-MAR Fallow-HF`$NO3.N/10, col='red', lty=3)
  lines(soilName$`Control CC`$date, soilName$`Flood-MAR Fallow-VHF-Jan`$NO3.N/10, col='red', lty=4)
  lines(soilName$`Control CC`$date, soilName$`Flood-MAR Fallow-VHF-Feb`$NO3.N/10, col='red', lty=5)
  lines(soilName$`Control CC`$date, soilName$`Flood-MAR Fallow-VHF-Mar`$NO3.N/10, col='red', lty=6)
  lines(soilName$`Control CC`$date, soilName$`Flood-MAR CC-LF`$NO3.N/10, col='green', lty=2)
  lines(soilName$`Control CC`$date, soilName$`Flood-MAR CC-HF`$NO3.N/10, col='green', lty=3)
  lines(soilName$`Control CC`$date, soilName$`Flood-MAR CC-VHF-Jan`$NO3.N/10, col='green', lty=4)
  lines(soilName$`Control CC`$date, soilName$`Flood-MAR CC-VHF-Feb`$NO3.N/10, col='green', lty=5)
  lines(soilName$`Control CC`$date, soilName$`Flood-MAR CC-VHF-Mar`$NO3.N/10, col='green', lty=6)
  lines(soilName$`Control CC`$date, soilName$`Control Fallow`$NO3.N/10, col='black', lty=1)
  lines(soilName$`Control CC`$date, soilName$`Control CC`$NO3.N/10, col='grey', lty=1)
  if(makeLegend) {
    legend('topleft', legend = c('Fallow-LF', 'Fallow-HF', 'Fallow-VHF-Jan', 'Fallow-VHF-Feb', 'Fallow-VHF-Mar', 'CC-LF', 'CC-HF', 'CC-VHF-Jan', 'CC-VHF-Feb', 'CC-VHF-Mar', 'Fallow-Control', 'CC-Control', expression('initial NO'[3]*'-N')), col=c(rep('red', 5), rep('green', 5), 'black', 'grey', 'orange'), lty=c(2:6, 2:6, rep(1, 3)), bty = 'n', cex=0.85, inset = -0.02)
  }
  text(x=as.Date('2017-04-25'), y=FigLabHt, label=FigLab)
  dev.off()
}
makeFigure('coarse_SSURGO_10ppm.tif', CoarseSHR_SSURGO_10ppm, ymax = 124, makeLegend=TRUE, ResidualNO3 = 68.4, FigLab = 'a', FigLabHt = 120, width = 4, startime = '2016-10-01', xmax = as.Date('2017-05-03'))
makeFigure('loamy_SSURGO_10ppm.tif', LoamySHR_SSURGO_10ppm, ymax = 124, ResidualNO3 = 64.1, FigLab = 'b', FigLabHt = 120, width = 2.8, startime = '2016-12-20', xmax = as.Date('2017-05-06'))
makeFigure('fine_SSURGO_10ppm.tif', FineSHR_SSURGO_10ppm, ymax = 124, ResidualNO3 = 59.8, FigLab = 'c', FigLabHt = 120, width = 2.8, startime = '2016-12-20', xmax = as.Date('2017-05-06'))

makeFigure('coarse_SSURGO_30ppm.tif', CoarseSHR_SSURGO_30ppm, ymax = 255, ResidualNO3 = 205.1, makeLegend = TRUE, FigLab = 'a', FigLabHt = 250, width = 4, startime = '2016-10-01', xmax = as.Date('2017-05-03'))
makeFigure('loamy_SSURGO_30ppm.tif', LoamySHR_SSURGO_30ppm, ymax = 255, ResidualNO3 = 192.2, FigLab = 'b', FigLabHt = 250, width = 2.8, startime = '2016-12-20', xmax = as.Date('2017-05-06'))
makeFigure('fine_SSURGO_30ppm.tif', FineSHR_SSURGO_30ppm, ymax = 255, ResidualNO3 = 179.3, FigLab = 'c', FigLabHt = 250, width = 2.8, startime = '2016-12-20', xmax = as.Date('2017-05-06'))

makeFigure('coarse_KSSL_10ppm.tif', CoarseSHR_KSSL_10ppm, ymax = 124, makeLegend=TRUE, ResidualNO3 = 68.4, FigLab = 'a', FigLabHt = 120, width = 4, startime = '2016-10-01', xmax = as.Date('2017-05-03'))
makeFigure('loamy_KSSL_10ppm.tif', LoamySHR_KSSL_10ppm, ymax = 124, ResidualNO3 = 64.1, FigLab = 'b', FigLabHt = 120, width = 2.8, startime = '2016-12-20', xmax = as.Date('2017-05-06'))
makeFigure('fine_KSSL_10ppm.tif', FineSHR_KSSL_10ppm, ymax = 124, ResidualNO3 = 59.8, FigLab = 'c', FigLabHt = 120, width = 2.8, startime = '2016-12-20', xmax = as.Date('2017-05-06'))

makeFigure('coarse_KSSL_30ppm.tif', CoarseSHR_KSSL_30ppm, ymax = 255, ResidualNO3 = 205.1, makeLegend = TRUE, FigLab = 'a', FigLabHt = 250, width = 4, startime = '2016-10-01', xmax = as.Date('2017-05-03'))
makeFigure('loamy_KSSL_30ppm.tif', LoamySHR_KSSL_30ppm, ymax = 255, ResidualNO3 = 192.2, FigLab = 'b', FigLabHt = 250, width = 2.8, startime = '2016-12-20', xmax = as.Date('2017-05-06'))
makeFigure('fine_KSSL_30ppm.tif', FineSHR_KSSL_30ppm, ymax = 255, ResidualNO3 = 179.3, FigLab = 'c', FigLabHt = 250, width = 2.8, startime = '2016-12-20', xmax = as.Date('2017-05-06'))