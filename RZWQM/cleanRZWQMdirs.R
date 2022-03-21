# met_stn <- 'Shafter'
# workDir <- 'C:/Users/smdevine/Desktop/post doc/Dahlke/RZWQM/projects/PulseSoilClimate/ClimateRuns_Silage'
# compnames <- c('Hanford', 'Delhi', 'Wasco', 'Hesperia', 'Milham', 'Tujunga', 'Panoche', 'Cerini', 'Yolo', 'Colpien', 'Tehama', 'Capay', 'Clear Lake', 'Tulare', 'Kimberlina', 'Columbia', 'Excelsior', 'Atwater', 'Rincon', 'Sycamore', 'Conejo', 'Westhaven', 'Pleito', 'Lokern', 'Merced', 'Cropley', 'Tachi', 'Myers', 'Lofgren', 'Wekoda')
cleanRZWQMdirs <- function(stn, scn, soilnames, fnames=c('LAYER.PLT', 'MBLCARBON.OUT', 'RESIDUES.OUT', 'MBLWAT.OUT')) {
  for(j in seq_along(soilnames)) {
    for(i in seq_along(fnames)) {
      file.remove(file.path(workDir, stn, scn, soilnames[j], fnames[i]))
    }
  }
}
cleanRZWQMdirs(stn = met_stn, scn = 'SteadyStateRuns', soilnames = compnames)
cleanRZWQMdirs(stn = met_stn, scn = 'AgMAR_Jan3d', soilnames = compnames)
cleanRZWQMdirs(stn = met_stn, scn = 'AgMAR_Jan7d', soilnames = compnames)
cleanRZWQMdirs(stn = met_stn, scn = 'AgMAR_Mar3d', soilnames = compnames)
cleanRZWQMdirs(stn = met_stn, scn = 'AgMAR_Mar7d', soilnames = compnames)
cleanRZWQMdirs(stn = met_stn, scn = 'AgMAR_21d', soilnames = compnames)
