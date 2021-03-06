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

df <- data.frame(a1 = a1, b1 = b1, a2 = a2, b2 = b2, p = p)

df_sub <- df[!grepl("autoHyper", df$a1),]
mat_out <- apply(df_sub, 2, as.numeric)
mat_out <- as.data.frame(mat_out)
#mat_out <- mat_out[mat_out>0,]
# mat_out <- mat_out[mat_out$a1>0,]
# mat_out <- mat_out[mat_out$a2>0,]
apply(mat_out, 2, min)
hist(mat_out$a1[mat_out$a1<4])
hist(mat_out$b1[mat_out$b1<1.2])
hist(mat_out$a2[mat_out$a2<20])
hist(mat_out$b2[mat_out$b2<20])
hist(mat_out$p)
mat_out <- mat_out[mat_out$a1 < 4,]
mat_out <- mat_out[mat_out$b1 < 1.2,]
mat_out <- mat_out[mat_out$a2 < 20,]
mat_out <- mat_out[mat_out$b2 < 20,]
pairs(mat_out)
apply(mat_out, 2, quantile, probs = c(0.025, 0.975))
#write.csv(mat_out, "data/boot_sub.csv", row.names = FALSE)
proc <- processRaw(caers)
squash <- squashData(proc)
autoHyper(data = squash, theta_init = theta_init, squashed = TRUE, zeroes = FALSE, N_star = 1, conf_ints = TRUE)
clt_ci <- apply(mat_out, 2, function(x) mean(x) + c(-1, 1) * 1.96 * sd(x))
boot_ci <- apply(mat_out, 2, quantile, c(0.025, 0.975))
hist(mat_out$a1)
abline(v = boot_ci[,1], lwd = 2)
abline(v = clt_ci[,1], lty = 2, lwd = 2, col = "red")
hist(mat_out$p)
abline(v = boot_ci[,5], lwd = 2)
abline(v = clt_ci[,5], lwd = 2, lty = 2, col = "red")
hist(mat_out$a2)
abline(v = boot_ci[,3], lwd = 2)
abline(v = clt_ci[,3], lwd = 2, lty = 2, col = "red")
cor(mat_out)
