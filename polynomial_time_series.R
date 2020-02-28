

a <- 0.3
b <- 0.1
std <- 0.7

n <- 5000
x <- rep(NA, n+1)
x[1] <- rnorm(1, 0, std)

for(i in 2:I(n+1)){
  x[i] = a*x[i-1] + b*x[i-1]^2 + rnorm(1, 0, std)
}

optim(par = c(0.4, 0.2, 0.6), fn = likelihood_ts, x = x, control = list(fnscale = -1, factr = 1e1), lower = c(0,0,0), method = "L-BFGS-B")




a <- 0.3
b <- 0
std <- 0.7

n <- 5000
x <- rep(NA, n+1)
x[1] <- rnorm(1, 0, std)

for(i in 2:I(n+1)){
  x[i] = a*x[i-1] + b*x[i-1]^2 + rnorm(1, 0, std)
}

x <- x[seq(1, n, by = 2)]

optim(par = c(0.4, 0.6), fn = likelihood_ts_m, x = x, control = list(fnscale = -1, factr = 1e1), lower = c(0,0), method = "L-BFGS-B")
