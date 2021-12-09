library(dplyr)
set.seed(5400)
### aircon data (small original sample)
aircon <- c(3, 5, 7, 18, 43, 85, 91, 98, 100, 130, 230, 487)
xbar1 <- mean(aircon) #true mean to measure against
boot_sample_prop1 <- seq(.5, 2.5, .05) #proportion of original sample size to be used in bootstrap sample
iters1 <- 500 #number of iterations
replicates1 <- matrix(NA, nrow = iters1, ncol = length(boot_sample_prop1)) #matrix to house realizations
#bootstrap procedure
for (i in seq_along(boot_sample_prop1)) {
  #set resampling size
  resample_size1 <- (length(aircon) * boot_sample_prop1[i]) %>% round()
  for (j in 1:iters1) {
    #resample from aircon with proportion of original sample size
    bootsamp1 <- sample(aircon, size = resample_size1, replace = TRUE)
    #apply estimator to resample and store it in the matrix 
    replicates1[j, i] <- mean(bootsamp1)
  }
}
#investigate bais
expects1 <- colMeans(replicates1) #expected value of bootstrap estimator
bias1 <- expects1 - xbar1 #bais of the bootstrap estimator
plot(bias1 ~ boot_sample_prop1, type = 'b',
     main = "Bias vs. Proportion of Sample Size Used in Resample",
     ylab = "Bias", xlab = "Proportion of Sample Size")
abline(h = 0, lty = 3, col = 'red')
#investigate standard error
se1 <- apply(replicates1, 2, sd)
plot(se1 ~ boot_sample_prop1, type = 'b',
     main = "Standard Error vs. Proportion of Sample Size Used in Resample",
     ylab = "Standard Error", xlab = "Proportion of Sample Size")
#investigate MSE
mse1 <- se1^2 + bias1^2
plot(mse1 ~ boot_sample_prop1, type = 'b',
     main = "MSE vs. Proportion of Sample Size Used in Resample",
     ylab = "MSE", xlab = "Proportion of Sample Size")


### law data
#load in library to get law data
library(bootstrap)
#using law82$GPA data for larger sample size
xbar2 <- mean(law82$GPA) #true mean to measure against
boot_sample_prop2 <- seq(.5, 2.5, .05) #proportion of original sample size to be used in bootstrap sample
iters2 <- 500 #number of iterations
replicates2 <- matrix(NA, nrow = iters2, ncol = length(boot_sample_prop2)) #matrix to house realizations
#bootstrap procedure
for (i in seq_along(boot_sample_prop2)) {
  #set resampling size
  resample_size2 <- (length(law82$GPA) * boot_sample_prop2[i]) %>% round()
  for (j in 1:iters2) {
    #resample from law82$GPA with proportion of original sample size
    bootsamp2 <- sample(law82$GPA, size = resample_size2, replace = TRUE)
    #apply estimator to resample and store it in the matrix 
    replicates2[j, i] <- mean(bootsamp2)
  }
}
#investigate bais
expects2 <- colMeans(replicates2) #expected value of bootstrap estimator
bias2 <- expects2 - xbar2 #bais of the bootstrap estimator
plot(bias2 ~ boot_sample_prop2, type = 'b',
     main = "Bias vs. Proportion of Sample Size Used in Resample",
     ylab = "Bias", xlab = "Proportion of Sample Size", col = "blue")
abline(h = 0, lty = 3, col = 'red')
#investigate standard error
se2 <- apply(replicates2, 2, sd)
plot(se2 ~ boot_sample_prop2, type = 'b',
     main = "Standard Error vs. Proportion of Sample Size Used in Resample",
     ylab = "Standard Error", xlab = "Proportion of Sample Size", col = "blue")
#investigate MSE
mse2 <- se2^2 + bias2^2
plot(mse2 ~ boot_sample_prop2, type = 'b',
     main = "MSE vs. Proportion of Sample Size Used in Resample",
     ylab = "MSE", xlab = "Proportion of Sample Size", col = "blue")

### exponential data
exp_dat <- rexp(50, rate = 2)
#using exp_dat data for larger sample size
xbar3 <- mean(exp_dat) #true mean to measure against
boot_sample_prop3 <- seq(.5, 2.5, .05) #proportion of original sample size to be used in bootstrap sample
iters3 <- 500 #number of iterations
replicates3 <- matrix(NA, nrow = iters3, ncol = length(boot_sample_prop3)) #matrix to house realizations
#bootstrap procedure
for (i in seq_along(boot_sample_prop3)) {
  #set resampling size
  resample_size3 <- (length(exp_dat) * boot_sample_prop3[i]) %>% round()
  for (j in 1:iters3) {
    #resample from exp_dat with proportion of original sample size
    bootsamp3 <- sample(exp_dat, size = resample_size3, replace = TRUE)
    #apply estimator to resample and store it in the matrix 
    replicates3[j, i] <- mean(bootsamp3)
  }
}
#investigate bais
expects3 <- colMeans(replicates3) #expected value of bootstrap estimator
bias3 <- expects3 - xbar3 #bais of the bootstrap estimator
plot(bias3 ~ boot_sample_prop3, type = 'b',
     main = "Bias vs. Proportion of Sample Size Used in Resample",
     ylab = "Bias", xlab = "Proportion of Sample Size", col = "blue")
abline(h = 0, lty = 3, col = 'red')
#investigate standard error
se3 <- apply(replicates3, 2, sd)
plot(se3 ~ boot_sample_prop3, type = 'b',
     main = "Standard Error vs. Proportion of Sample Size Used in Resample",
     ylab = "Standard Error", xlab = "Proportion of Sample Size", col = "blue")
#investigate MSE
mse3 <- se3^2 + bias3^2
plot(mse3 ~ boot_sample_prop3, type = 'b',
     main = "MSE vs. Proportion of Sample Size Used in Resample",
     ylab = "MSE", xlab = "Proportion of Sample Size", col = "blue")


### beta data
bet_dat <- rbeta(50, shape1 = .5, shape2 = .5)
#using bet_dat data for larger sample size
xbar4 <- mean(bet_dat) #true mean to measure against
boot_sample_prop4 <- seq(.5, 4, .05) #proportion of original sample size to be used in bootstrap sample
iters4 <- 500 #number of iterations
replicates4 <- matrix(NA, nrow = iters4, ncol = length(boot_sample_prop4)) #matrix to house realizations
#bootstrap procedure
for (i in seq_along(boot_sample_prop4)) {
  #set resampling size
  resample_size4 <- (length(bet_dat) * boot_sample_prop4[i]) %>% round()
  for (j in 1:iters4) {
    #resample from bet_dat with proportion of original sample size
    bootsamp4 <- sample(bet_dat, size = resample_size4, replace = TRUE)
    #apply estimator to resample and store it in the matrix 
    replicates4[j, i] <- mean(bootsamp4)
  }
}
#investigate bais
expects4 <- colMeans(replicates4) #expected value of bootstrap estimator
bias4 <- expects4 - xbar4 #bais of the bootstrap estimator
plot(bias4 ~ boot_sample_prop4, type = 'b',
     main = "Bias vs. Proportion of Sample Size Used in Resample",
     ylab = "Bias", xlab = "Proportion of Sample Size", col = "blue")
abline(h = 0, lty = 4, col = 'red')
#investigate standard error
se4 <- apply(replicates4, 2, sd)
plot(se4 ~ boot_sample_prop4, type = 'b',
     main = "Standard Error vs. Proportion of Sample Size Used in Resample",
     ylab = "Standard Error", xlab = "Proportion of Sample Size", col = "blue")
#investigate MSE
mse4 <- se4^2 + bias4^2
plot(mse4 ~ boot_sample_prop4, type = 'b',
     main = "MSE vs. Proportion of Sample Size Used in Resample",
     ylab = "MSE", xlab = "Proportion of Sample Size", col = "blue")


### bimodal data
bmod_dat <- c(rnorm(10, mean = 10, sd = 2), rnorm(10, mean = 50, sd = 2))
#using bmod_dat data for larger sample size
xbar5 <- mean(bmod_dat) #true mean to measure against
boot_sample_prop5 <- seq(.5, 4, .05) #proportion of original sample size to be used in bootstrap sample
iters5 <- 200 #number of iterations
replicates5 <- matrix(NA, nrow = iters5, ncol = length(boot_sample_prop5)) #matrix to house realizations
#bootstrap procedure
for (i in seq_along(boot_sample_prop5)) {
  #set resampling size
  resample_size5 <- (length(bmod_dat) * boot_sample_prop5[i]) %>% round()
  for (j in 1:iters5) {
    #resample from bmod_dat with proportion of original sample size
    bootsamp5 <- sample(bmod_dat, size = resample_size5, replace = TRUE)
    #apply estimator to resample and store it in the matrix 
    replicates5[j, i] <- mean(bootsamp5)
  }
}
#investigate bais
expects5 <- colMeans(replicates5) #expected value of bootstrap estimator
bias5 <- expects5 - xbar5 #bais of the bootstrap estimator
plot(bias5 ~ boot_sample_prop5, type = 'b',
     main = "Bias vs. Proportion of Sample Size Used in Resample",
     ylab = "Bias", xlab = "Proportion of Sample Size", col = "blue")
abline(h = 0, lty = 4, col = 'red')
#investigate standard error
se5 <- apply(replicates5, 2, sd)
plot(se5 ~ boot_sample_prop5, type = 'b',
     main = "Standard Error vs. Proportion of Sample Size Used in Resample",
     ylab = "Standard Error", xlab = "Proportion of Sample Size", col = "blue")
#investigate MSE
mse5 <- se5^2 + bias5^2
plot(mse5 ~ boot_sample_prop5, type = 'b',
     main = "MSE vs. Proportion of Sample Size Used in Resample",
     ylab = "MSE", xlab = "Proportion of Sample Size", col = "blue")
