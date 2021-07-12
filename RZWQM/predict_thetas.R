#tuned parameter from Nick Murphy
# Theta_s - 0.3197
# theta_r - 0.03177
# alpha - 0.07586
# n - 1.860
# Ks - 0.05358 
# l - -0.05507
0.05358*60
#van Genuchten moisture retention function
vg_theta <- function(theta_r, theta_s, alpha, n, h) {
  theta_r + (theta_s - theta_r) / (1 + (alpha * h)^n)^(1-1/n)
}

#VG parameters for Nick's sandy loam
vg_theta(theta_r = 0.03177, theta_s = 0.3197, alpha = 0.07586, n=1.860, h=1)
vg_theta(theta_r = 0.03177, theta_s = 0.3197, alpha = 0.07586, n=1.860, h=10)
vg_theta(theta_r = 0.03177, theta_s = 0.3197, alpha = 0.07586, n=1.860, h=100)
vg_theta(theta_r = 0.03177, theta_s = 0.3197, alpha = 0.07586, n=1.860, h=336)
vg_theta(theta_r = 0.03177, theta_s = 0.3197, alpha = 0.07586, n=1.860, h=1000)
vg_theta(theta_r = 0.03177, theta_s = 0.3197, alpha = 0.07586, n=1.860, h=15000)


#from median sandy loam in trafficability study
vg_theta(theta_r = 0.041, theta_s=0.378,	alpha=0.037, n=1.36, h=1)
vg_theta(theta_r = 0.041, theta_s=0.378,	alpha=0.037, n=1.36, h=10)
vg_theta(theta_r = 0.041, theta_s=0.378,	alpha=0.037, n=1.36, h=100)
vg_theta(theta_r = 0.041, theta_s=0.378,	alpha=0.037, n=1.36, h=167)
vg_theta(theta_r = 0.041, theta_s=0.378,	alpha=0.037, n=1.36, h=333)
vg_theta(theta_r = 0.041, theta_s=0.378,	alpha=0.037, n=1.36, h=1000)
vg_theta(theta_r = 0.041, theta_s=0.378,	alpha=0.037, n=1.36, h=15000)
