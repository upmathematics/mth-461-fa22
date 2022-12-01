###
# Simulating the Central Limit Theorem
###

# Generate synthetic population data taken from a uniform distribution

n_pop <- 100000
min_number = 0 ### <- Modify here
max_number = 1 ### <- Modify here
X_pop <- runif(n_pop,min_number,max_number) ### <- Modify here
X_pop_mean <- mean(X_pop) # the true mean

# take N random samples with k samples each from the population and take the mean for each sample

k <- 10 # number of items in each sample ### <- Modify here
N <- 1000 # total samples ### <- Modify here

Y <- c() # means
Y_mean_track <- c() # tracking the mean of means
for(i in 1:N){
  sample_i <- sample(X_pop,k,replace = T) # sample item from the population with replacement
  sample_i_mean <- mean(sample_i)
  Y <- c(Y,sample_i_mean)
  Y_mean_track <- c(Y_mean_track, mean(Y))
}
Y_mean <- mean(Y) # sample mean of means

## plotting
par(mfrow=c(1,3))

# plot synthetic population data
hist(X_pop,main="Histogram of the Population")
abline(v=X_pop_mean,col="blue",lwd=2)
legend("topleft", legend = "Population Mean", pch = "|", col = "blue")

# plot distribution of means
hist(Y,main=paste("Distribution of Means (k = ",k," and N = ",N,")"))
abline(v=Y_mean,col="red",lwd=2)
legend("topleft", legend = "Sample Mean of Means", pch = "|", col = "red")

# plot mean of means tracking
plot(1:N,Y_mean_track,type="l", ylab="mean of means", xlab = "N", col = "red")
abline(h=Y_mean,col="blue",lwd=2)
legend("topleft", legend = "Population Mean", pch = "|", col = "blue")

