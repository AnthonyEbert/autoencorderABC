
likelihood_ts <- function(theta, x){
  a = theta[1]
  b = theta[2]
  std = theta[3]

  n <- length(x) - 1

  output <- -1/(2*std^2) *
    (
      (1 + a^2) * sum(x^2) +
      -2 * a * sum(x[1:n] * x[2:I(n+1)]) +
      -2 * b * sum(x[1:n]^2 * x[2:I(n+1)]) +
      2 * a * b * sum(x^3) +
      b^2 * sum(x^4)
    ) - (n+1) * log(std)

  return(output)
}

likelihood_ts_m <- function(theta, x){
  a = theta[1]
  std = theta[2]

  n <- length(x) - 1

  output <- -1/(2*std^2) *
    (
      (1 + a^4) / (1 + a^2) * sum(x^2) +
      -(2 * a^2) / (1 + a^2) * sum(x[1:n] * x[2:I(n+1)])
    ) - (n+1) * log(std)

  return(output)
}

likelihood_ts_m <- function(theta, x){
  a = theta[1]
  std = theta[2]

  n <- length(x)
  output <- dnorm(x[1], 0, std, log = TRUE)

  for(i in 1:I(n-1)){
    output <- output + dnorm(x[i+1], a^2 * x[i], sd = sqrt( (1 + a^2) * std^2), log = TRUE )
    # output <- output + dnorm(x[i+1]/a, a*x[i], sd = sqrt(std^2 / a^2 + std^2), log = TRUE)
  }

  return(output)
}

likelihood_good <- function(theta, x){
  a = theta[1]
  std = sqrt(theta[2]^2 * (1 + a^2))

  n <- length(x) - 1

  output <- -1/(2*std^2) *
    (
      (1 + a^4) * sum(x^2) +
        -2 * a^2 * sum(x[1:n] * x[2:I(n+1)])
    ) - (n+1) * log(std)

  return(output)
}

