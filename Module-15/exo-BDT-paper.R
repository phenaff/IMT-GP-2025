# The Black-Derman-Toy model

library(linprog)
library(xtable)
library(nleqslv)
                                                                                                                                                                                                                  options(knitr.kable.NA = "")
data.f <- data.frame(mat = seq(5), z=c(10,11,12,12.5,13), 
                 b=c(NA, 19,18,17.5,16))
print(df)

# step 1

df <- function(r) 1/(1+r)

obj <- function(r.vec) {
  r.u <- r.vec[1]
  r.d <- r.vec[2]
  res <- numeric(2)
  res[1] <- df(.1) * (1/2) * (df(r.u) + df(r.d)) - df(.11) **2
  res[2] <- (1/2) * log(r.u/r.d) - 0.19
  res
}

sol <- nleqslv(as.vector(c(.1, .1)), obj)
r.u <- sol$x[1]
r.d <- sol$x[2]

print(paste("r.u", r.u, "r.d", r.d))

# step 2

obj <- function(r.vec) {
  r.dd <- r.vec[1]
  alpha <- r.vec[2]
  r.ud <- r.dd*alpha
  r.uu <- r.ud*alpha
  res <- numeric(2)
  P.A <- df(r.u) * (1/2) * (df(r.uu) + df(r.ud))
  P.B <- df(r.d) * (1/2) * (df(r.ud) + df(r.dd))
  
  res[1] <- df(.1) * (1/2) * ( P.A + P.B) - df(.12)^3
  Y.A <- sqrt(1/P.A) - 1
  Y.B <- sqrt(1/P.B) - 1
  res[2] <- (1/2) * log(Y.A / Y.B) - 0.18
  res
}

sol <- nleqslv(as.vector(c(.1, 1)), obj)
r.dd <- sol$x[1]
alpha <- sol$x[2]

df.2 <- data.frame(r = c(r.dd*alpha^2, r.dd*alpha, r.dd))
rownames(df.2) =  c("$r_{uu}$", "$r_{ud}$", "$r_{dd}$")
print(df.2)