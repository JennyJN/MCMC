#make some data


set.seed(50)
Ndat <- 10
x <- sort(runif(Ndat, 0, 10))
sigTrue <- 1
modMat <- c(0,1) # 1 x P vector: coefficients, a_p, of polynomial sum_{p=0} a_p*x^p
y <- cbind(1,x) %*% as.matrix(modMat) + rnorm(Ndat, 0, sigTrue)
# Dimensions in matrix multiplication: [Ndat x 1] = [Ndat x P] %*% [P x 1] + [Ndat]
# cbind does the logical thing combining a scalar and vector; then vector addition
y <- drop(y) # converts into a vector
plot(x, y)


data <- data.frame(cbind(x,y))

library(MCMCpack)
library(mnormt)

# try it out on std norm

true.mean <- 0.0
true.sd <- 1.0
initial.x <- 3
mcmc.out <- MCMCmetrop1R(dnorm, theta.init=initial.x, burnin=500, mcmc=50000,
                         mean=true.mean, sd=true.sd, verbose=5000, logfun=FALSE)
summary(mcmc.out)
plot(mcmc.out)

# now try it out on two dimensions

# define 2-component 2D Gaussian
myprob <- function(theta) {
  # define properties of two 2D Gaussians via their means and covariances
  # I could have passed these in via the ... argument in MCMCmetrop1R instead
  mean1 <- c(0,0)
  cov1 <- matrix(data=c(1,0,0,1), nrow=2, ncol=2)
  wt1 <- 0.5
  mean2 <- c(4,0)
  cov2 <- matrix(data=c(2,0.8,0.8,2), nrow=2, ncol=2)
  wt2 <- 1
  p1 <- wt1*dmnorm(theta, mean1, cov1)
  p2 <- wt2*dmnorm(theta, mean2, cov2)
  return(p1 + p2)
}
# Run MCMC
initial.theta <- c(1,0)
mcmc.out <- MCMCmetrop1R(myprob, theta.init=initial.theta, burnin=500, mcmc=10000,
                         logfun=FALSE)
# Examine results
summary(mcmc.out)
plot(mcmc.out)
plot.default(mcmc.out[,1], mcmc.out[,2], xlab="theta1", ylab="theta2")
f <- kde2d(mcmc.out[,1], mcmc.out[,2], n=100, h=c(0.5, 0.5))
image(f, zlim = c(0, 0.075), xlab="theta1", ylab="theta2")
f2 <- kde2d(mcmc.out[,1], mcmc.out[,2], n=100, h=c(1, 1))
persp(f2, phi=30, theta=20, d=5, xlab="theta1", ylab="theta2", zlab="Density")
HPDinterval(mcmc.out, prob=0.5)


