N <- 100000
r = .02
sigma = .3
T = 1
dT = 1
S.0 = 100

eps = rnorm(N,0,1)
z = (r - (1/2)*sigma^2)*dT + sigma * sqrt(dT)*eps
S.T = S.0 * exp(z)

print(paste("E(ST) analytique", S.0*exp(r*T)))
print(paste("E(ST) MC", mean(S.T)))

payoff.call <- function(S) {
  pmax(S-K,0)
}

K = 105
v.exer <- payoff.call(S.T)
V.call = mean(v.exer) * exp(-r*T)
print(paste("Call 105 MC", V.call, "+/-", sd(v.exer)*sqrt(1/N)))

V.exact = GBSOption("c", S=100, X=K, Time=1, r=r, b=r, sigma=sigma )@price
print(paste("Call 105 Analytique", V.exact))