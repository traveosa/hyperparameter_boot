#Bootstrap confidence bounds
library(openEBGM) #package containing functions and data

theta_init <- data.frame(
  alpha1 = c(0.2, 0.1),
  beta1  = c(0.1, 0.1),
  alpha2 = c(2,   10),
  beta2  = c(4,   10),
  p      = c(1/3, 0.2)
)
data(caers)
B <- 2000
for(i in 1:B) {
    resamp <- caers[sample.int(n = nrow(caers), replace = TRUE),]
    proc <- processRaw(resamp)
    squash <- squashData(proc)
    hypers <- try(autoHyper(data = squash, theta_init = theta_init, 
                            squashed = TRUE, zeroes = FALSE, N_star = 1)$estimates,
                  silent = TRUE)
    a1 <- hypers[1]
    b1 <- hypers[2]
    a2 <- hypers[3]
    b2 <- hypers[4]
    p <- hypers[5]
}