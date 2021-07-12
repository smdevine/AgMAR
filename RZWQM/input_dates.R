workDir <- 'C:/Users/smdevine/Desktop/post doc/Dahlke/RZWQM/nick_murphy_experiments'
hours <- rep(seq(0,23, by=1), 27)
days <- do.call(c, lapply(as.character(format.Date(seq.Date(from=as.Date('2020/1/1'), to=as.Date('2020/1/27'), by='day'), format='%d/%m/%Y')), function(x) rep(x, 24)))
df_climate <- data.frame(date=days, hour=hours, airtemp=22, wind=0, radiation=0, rel_humidity=45, precip=0)
df_climate$precip[df_climate$date=='01/01/2020' & df_climate$hour==0] <- 190.8 #actually 0-40 minutes of experiment
df_climate$precip[df_climate$date=='02/01/2020' & df_climate$hour==1] <- 152.4 #actually 1480-1520 minutes of experiment
df_climate$precip[df_climate$date=='10/01/2020' & df_climate$hour==1] <- 152.4 #actually 12998-13038 minutes of experiment
df_climate$precip[df_climate$date=='24/01/2020' & df_climate$hour==0] <- 152.4 #actually 12998-13038 minutes of experiment
write.csv(df_climate, file.path(workDir, 'climate_input_data.csv'), row.names = FALSE)

#soil theta assumptions
theta_fc_est <- function(theta_s, theta_r, Ks, n) {
  (n^(-0.6*(2+log10(Ks))))*(theta_s - theta_r) + theta_r
}
vg_theta <- function(theta_r, theta_s, alpha, n, h) {
  theta_r + (theta_s - theta_r) / (1 + (alpha * h)^n)^(1-1/n)
}
find_h_at_theta_fc <- function(h, theta_r, theta_s, alpha, n, theta_fc){
  abs(theta_fc  - theta_r - ((theta_s - theta_r) / (1 + (alpha * h)^n)^(1-1/n)))
}
0.06309*60*24 #cm min-1 to cm d-1
theta_fc_est(theta_s = 0.3221, theta_r = 0.033607, Ks=90.8496, n=1.799) #0.1051287
optimize(find_h_at_theta_fc, interval = c(10,10000), theta_r = 0.033607, theta_s = 0.3221, alpha = 0.5074, n=1.799, theta_fc=0.1051287) #11.0164
vg_theta(theta_r = 0.033607, theta_s = 0.3221, alpha = 0.5074, n=1.799, h=0) #0.3221
vg_theta(theta_r = 0.033607, theta_s = 0.3221, alpha = 0.5074, n=1.799, h=336.5) #0.0383
vg_theta(theta_r = 0.033607, theta_s = 0.3221, alpha = 0.5074, n=1.799, h=15295.8) #0.0338
)
