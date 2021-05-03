library(soilDB)
soilname_to_cokey <- function(x) {paste0("SELECT co.mukey, cokey, compname, comppct_r
FROM legend
INNER JOIN mapunit mu ON mu.lkey = legend.lkey
INNER JOIN component co ON mu.mukey = co.mukey 
WHERE
legend.areasymbol != 'US'
AND compname ='", x,  "';")}

comps_of_interest <- SDA_query(soilname_to_cokey('Dinuba'))
comps_of_interest <- comps_of_interest[which(comps_of_interest$comppct_r >= 50), ]
comps_of_interest

query_horizon <- function(x) { #this will not return cokey NAs
  print(x)
  SDA_query(paste0("SELECT comp.compname, comp.mukey, comp.cokey, ch.chkey, hzname, hzdept_r, hzdepb_r, awc_r, ec_r, claytotal_r, silttotal_r, sandtotal_r, dbthirdbar_r, wsatiated_r, wthirdbar_r, wfifteenbar_r, ksat_r, om_r, ll_r, pi_r
    FROM component comp
      LEFT OUTER JOIN chorizon ch on ch.cokey = comp.cokey
    WHERE comp.cokey = '", x, "'"))
}

SSURGO_horizons <- do.call(rbind, lapply(comps_of_interest$cokey, query_horizon))
dim(SSURGO_horizons)
SSURGO_horizons
