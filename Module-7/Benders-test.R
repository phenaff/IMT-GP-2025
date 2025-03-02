# Original problem

library(pracma)

c <- c(1,2,3)
f <- c(4,5)
A <- matrix(c(1,2,3,4,5,6), nrow=2, byrow=T)
b <- c(6,15)
B <- matrix(c(2,3,4,5,6,7), nrow=2, byrow=T)
D <- matrix(c(4,5,6,7), nrow=2, byrow=T)
d <- c(18, 31)

R1 <- cbind(A, matrix(0, nrow=2, ncol=2))
R2 <- cbind(B, D)
const.mat <- rbind(R1, R2)
const.rhs <- c(b,d)
obj <- c(c,f)

sol <- solveLP(cvec=obj, Amat=const.mat, 
          bvec=const.rhs, const.dir = rep("==", nrow(const.mat)),
          lpSolve=TRUE)

print("full master problem")
print(sol$solution)
print(paste("opt", sol$opt))

LB = -Inf
UB = Inf
BigNumber <- 1.e+4

solve.subproblem <- function(x.bar) {
  obj <- d - B %*% x.bar
  obj <- c(obj, -obj)
  Amat <- cbind(t(D), -t(D))
  n <- length(d)
  z.plus <- cbind(diag(n), matrix(0, n, n))
  z.minus <- cbind(matrix(0,n,n), diag(n))
  Amat <- rbind(Amat, z.plus, z.minus)
  bvec <- c(f, rep(BigNumber, 2*n))
  sub <- solveLP(cvec=obj, Amat=Amat, maximum=TRUE, 
                   bvec=bvec, lpSolve=T)
  
  if(sub$status != 0) {
    print(sub)
    stop()
  }
  
  p <- sub$solution[1:n] - sub$solution[(n+1):(2*n)]
  
  # extreme point if BigNumber constraint is not active
  
  if(all(abs(p) < BigNumber)) {
    status = "extreme.point"
  } else {
    p <- p / BigNumber
    status = "extreme.ray"
  }
  list(p=p, status=status)
}

solve.rmp <- function(rmp.A, rmp.b, rmp.dir, p=NULL, r=NULL) {
  
  if(!is.null(p)) {
    new.const <- c(-p %*% B, -1, 1)
    rmp.A <- rbind(rmp.A, new.const)
    new.rhs <- -p %*% d
    rmp.b <- c(rmp.b, new.rhs)
    rmp.dir <- c(rmp.dir, "<=")
  }
  
  if(!is.null(r)) {
    new.const <- c(-p %*% B, 0, 0)
    rmp.A <- rbind(rmp.A, new.const)
    new.rhs <- 0
    rmp.b <- c(rmp.b, new.rhs)
    rmp.dir <- c(rmp.dir, "<=")
  }
  
  sol <- solveLP(cvec=rmp.obj, Amat=rmp.A, bvec=rmp.b,
               const.dir=rmp.dir, lpSolve=TRUE)

  list(sol=sol, A=rmp.A, b=rmp.b, dir=rmp.dir)  
}
  
iter = 1
print(paste("Iter", iter))

# Restricted master program

res <- solve.rmp(rmp.A=A, rmp.b=b, rmp.dir=rep("==", 2))
x.bar <- res$sol$solution             

# sub-problem

sub <- solve.subproblem(x.bar)

if(sub$status == "extreme.point") {

  # termination test 
  
  # add optimality constraint
  
  }

if(sub$status == "extreme.ray") {
  
  # add feasibility constraint
  
}

