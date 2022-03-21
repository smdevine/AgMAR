compnames
soilname_to_cokey <- function(x) {paste0("SELECT co.mukey, cokey, compname, comppct_r
FROM legend
INNER JOIN mapunit mu ON mu.lkey = legend.lkey
INNER JOIN component co ON mu.mukey = co.mukey 
WHERE
legend.areasymbol != 'US'
AND compname ='", x,  "';")}

soilname_queries <- lapply(compnames, soilname_to_cokey)
lofgren_comps <- SDA_query(soilname_to_cokey('Lofgren'))
lofgren_comps
comps_of_interest <- do.call(rbind, lapply(soilname_queries, SDA_query))
head(comps_of_interest)
comps_of_interest <- comps_of_interest[which(comps_of_interest$comppct_r >= 50), ]
length(comps_of_interest$cokey) #704
write.csv(comps_of_interest, file.path(ssurgoDir, 'comps_of_interest_query.csv'), row.names = FALSE)

#database query only needs to be done once
library(soilDB)

query_comp <- function(x) {
  SDA_query(paste0("SELECT
    cokey, comppct_r, compname, taxclname, taxorder, taxsuborder, taxgrtgroup, taxsubgrp, taxpartsize, taxpartsizemod, hydgrp
    FROM component
  WHERE cokey = '", x, "'"))
}
query_comp(comps_of_interest$cokey[1])
comp_taxonomy <- do.call(rbind, lapply(comps_of_interest$cokey, query_comp))
head(comp_taxonomy)
write.csv(comp_taxonomy, file.path(ssurgoDir, 'comps_of_interest_taxonomy.csv'), row.names = FALSE)
lofgren_taxonomy <-  do.call(rbind, lapply(lofgren_comps$cokey, query_comp))
lofgren_
unique(comp_taxonomy$taxpartsize)
unique(comp_taxonomy$taxgrtgroup)
tapply(comp_taxonomy$taxpartsize, comp_taxonomy$compname, table)
tapply(comp_taxonomy$taxpartsizemod, comp_taxonomy$compname, table)
tapply(comp_taxonomy$hydgrp, comp_taxonomy$compname, table)
compnames_query <- comps_of_interest$cokey[comps_of_interest$compname %in% c('Capay', 'Columbia', 'Tehama', 'Yolo')]
query_horizon <- function(x) { #this will not return cokey NAs
  print(x)
  SDA_query(paste0("SELECT comp.compname, comp.mukey, comp.cokey, ch.chkey, hzname, hzdept_r, hzdepb_r, awc_r, ec_r, claytotal_r, silttotal_r, sandtotal_r, dbthirdbar_r, wsatiated_r, wthirdbar_r, wfifteenbar_r, ksat_r, om_r, ll_r, pi_r
    FROM component comp
      LEFT OUTER JOIN chorizon ch on ch.cokey = comp.cokey
    WHERE comp.cokey = '", x, "'"))
}
SSURGO_horizons <- do.call(rbind, lapply(compnames_query, query_horizon))
head(SSURGO_horizons)
SSURGO_horizons[SSURGO_horizons$compname=='Yolo',]
SSURGO_horizons[SSURGO_horizons$cokey=='21463472',]
comp_taxonomy[comp_taxonomy$cokey=='21463472',]

SSURGO_horizons[SSURGO_horizons$compname=='Capay',]
Capay <- SSURGO_horizons[SSURGO_horizons$cokey=='21463816',]
Capay <- Capay[order(Capay$hzdept_r),]
Capay
comp_taxonomy[comp_taxonomy$cokey=='21463816',]

SSURGO_horizons[SSURGO_horizons$compname=='Columbia',]
Columbia <- SSURGO_horizons[SSURGO_horizons$cokey=='21460915',]
Columbia <- Columbia[order(Columbia$hzdept_r),]
Columbia
comp_taxonomy[comp_taxonomy$cokey=='21460915',]

SSURGO_horizons[SSURGO_horizons$compname=='Tehama',]
Columbia <- SSURGO_horizons[SSURGO_horizons$cokey=='21460915',]
Columbia <- Columbia[order(Columbia$hzdept_r),]
Columbia
comp_taxonomy[comp_taxonomy$cokey=='21460915',]

SSURGO_horizons[SSURGO_horizons$compname=='Tehama',]
Tehama <- SSURGO_horizons[SSURGO_horizons$cokey=='21469329',]
Tehama <- Tehama[order(Tehama$hzdept_r),]
Tehama
comp_taxonomy[comp_taxonomy$cokey=='21469329',]

# write.csv(SSURGO_horizons, file.path(ssurgoDir, 'comps_of_interest_query.csv'), row.names = FALSE)