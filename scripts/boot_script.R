#Bootstrap confidence bounds
library(openEBGM) #package containing functions and data

#Initial guesses for each run
theta_init <- data.frame(
  alpha1 = c(0.2, 0.1),
  beta1  = c(0.1, 0.1),
  alpha2 = c(2,   10),
  beta2  = c(4,   10),
  p      = c(1/3, 0.2)
)
#relevant dataset
data(caers)
B <- 2000 #number of resamples
#initialize vectors
a1 <- numeric(B)
b1 <- numeric(B)
a2 <- numeric(B)
b2 <- numeric(B)
p <- numeric(B)
#bootstrap sampling
for(i in 1:B) {
    resamp <- caers[sample.int(n = nrow(caers), replace = TRUE),]
    proc <- processRaw(resamp)
    squash <- squashData(proc)
    #estimate parameters
    hypers <- try(autoHyper(data = squash, theta_init = theta_init, 
                            squashed = TRUE, zeroes = FALSE, N_star = 1)$estimates,
                  silent = TRUE)
    a1[i] <- hypers[1]
    b1[i] <- hypers[2]
    a2[i] <- hypers[3]
    b2[i] <- hypers[4]
    p[i] <- hypers[5]
    if(i %% 100 == 0) print(i)
}