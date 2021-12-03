library(dplyr)
set.seed(5400)
### aircon data (small original sample)
aircon <- c(3, 5, 7, 18, 43, 85, 91, 98, 100, 130, 230, 487)
xbar1 <- mean(aircon) #true mean to measure against
boot_sample_prop1 <- seq(.5, 2.5, .05) #proportion of original sample size to be used in bootstrap sample
iters1 <- 5000 #number of iterations
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
iters2 <- 5000 #number of iterations
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