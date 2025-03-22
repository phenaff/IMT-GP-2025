library(fExoticOptions)

source("random_tools.r")

digital.payoff <- function(S, K, r) {
  if(S>K) {
    V <- 1
  } else {
    V <- 0
  }
  V
}

expiry.payoff <- function(S) {
  if(S>50) {
    V = 142
  } else {
    V = S
  }
  V
}

digital_put_price <- function(S, K, r, sigma, T) {
  exp(-r*T) - digital_call_price(S, K, r, sigma, T)
}

digital_call_price <- function(S, K, r, sigma, T) {
  # Inputs:
  #   S: Stock price
  #   K: Strike price
  #   r: Risk-free interest rate
  #   sigma: Volatility
  #   T: Time to maturity
  
  d2 <- (log(S / K) + (r-d - 0.5 * sigma^2) * T) / (sigma * sqrt(T))
  
  price <- exp(-r * T) * pnorm(d2)
  
  return(price)
}


p.digital.T <- function(T) {
  digital_call_price(100,100,r,sigma,T)
}

faf.payoff <- function(S, r) {
  if(S[2]>100) {
    V = 107 * exp(-r)
  } else if(S[3]>100) {
    V = 114 * exp(-2*r)
  } else if(S[4]>100){
    V = 121 * exp(-3*r)
  } else if(S[5]>100) {
    V = 128 * exp(-4*r)
  } else if(S[6]>100) {
    V = 135 * exp(-5*r)
  } else if(S[7]>50) {
    V = 142 * exp(-6*r)
  } else {
    V = S[7] * exp(-6*r)
  }
  V
}

r = .05
d = .0
sigma = .25
nbPaths = 200000
S <- pathSimulator(horizon=6, delta.t=1, nbPaths = nbPaths, 
                   path.param = list(mu=r-d, sigma=sigma), S0=100)

payoff <- as.matrix(apply(S, 2, faf.payoff, r=r))
er <- sqrt(1/nbPaths) * sd(payoff)

print(paste("Expected value", round(mean(payoff),2), "+/-", round(er,2)))

p.sigma <- function(sigma) {
S <- pathSimulator(horizon=6, delta.t=1, nbPaths = nbPaths, 
                   path.param = list(mu=r-d, sigma=sigma), S0=100)
cf <- apply(S, 2, faf.payoff, r=r)
mean(cf)
}

if(FALSE) {
  sigma.range <- seq(from=.10, to=.40, by=.05)
price.range <- lapply(sigma.range, p.sigma)
}

# control variates with digital options
K = 100
T.range <- seq(2,6)
digital.exact <- sapply(T.range, 
                        FUN=function(T) {digital_call_price(100,100,r,sigma,T)})

expiry.exact = 
  142*digital_call_price(100, 50, r, sigma, 6) - 
  50*digital_put_price(100,50,r,sigma,6) - 
  GBSOption("p", S=100, X=K, Time=6, r=r, b=r, sigma=sigma)@price

#control.variate.exact = c(digital.exact, expiry.exact)
control.variate.exact = digital.exact

digital.cf <- t(ifelse(S[2:6,]>K,1,0))*exp(-r*seq(2,6))
expiry.cf <- sapply(S[7,], FUN=expiry.payoff)

#control.variate.cf <- cbind(digital.cf, expiry.cf)
control.variate.cf <- digital.cf

control.variate.sim <- colMeans(control.variate.cf)
res = lm(matrix(payoff, ncol=1) ~ control.variate.cf)
summary(res)

#control.variate.analytic = matrix(rep(control.variate.exact, nbPaths),
#                          ncol=6, byrow=T)
control.variate.analytic = matrix(rep(control.variate.exact, nbPaths),
                          ncol=5, byrow=T)
beta = res$coefficients[-1]
adjusted.payoff <- payoff - (control.variate.cf - control.variate.analytic) %*% beta
                               
adj.faf.price <- mean(adjusted.payoff)
er = sqrt(1/nbPaths) * sd(adjusted.payoff)

print(paste("Adjusted Expected value", round(adj.faf.price,2), "+/-", round(er,2)))
# correction
correction <- sum((control.variate.exact-control.variate.sim)*beta)
print(paste("correction", round(correction, 2)))
print(cbind(control.variate.exact, control.variate.sim))
print(summary(res))

