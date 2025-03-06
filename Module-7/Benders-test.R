# Original problem

library(linprog)

cost <- c(1,2,3)
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
obj <- c(cost,f)

sol <- solveLP(cvec=obj, Amat=const.mat, 
          bvec=const.rhs, const.dir = rep("==", nrow(const.mat)),
          lpSolve=TRUE)

print("full master problem")
print(sol$solution)
print(paste("opt", sol$opt))

LB = -Inf
UB = Inf
BigNumber <- 1.e+4

# LP subproblem dual. 
# Variable p is unbounded, modeled as p+ - p-

solve.subproblem <- function(x.bar) {
  obj <- d - B %*% x.bar
  obj <- c(obj, -obj)
  Amat <- cbind(t(D), -t(D))
  n <- length(d)
  # BigM >= p >= -BigM to control unbounded solutions
  p.plus <- cbind(diag(n), matrix(0, n, n))
  p.minus <- cbind(matrix(0,n,n), diag(n))
  Amat <- rbind(Amat, p.plus, p.minus)
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

# Add constraint (extreme ray r or extreme point p to 
# restricted master problem and solve

solve.rmp <- function(rmp.model, p=NULL, r=NULL) {
  
  has.p <- rmp.model$has.p
  rmp.obj <- rmp.model$obj
  rmp.Amat <- rmp.model$Amat
  rmp.b <- rmp.model$b
  
  # if this is the first extreme point constraint, add z variable to
  # objective function and constraints
  
  if(!has.p && !is.null(p)) {
    rmp.obj <- c(rmp.obj, 1, -1)
    rmp.Amat <- cbind(rmp.Amat, matrix(0, nrow=nrow(rmp.Amat), ncol=2))
    has.p <- TRUE
  }
  
  if(!is.null(p)) {
    new.const <- c(-p %*% B, -1, 1)
    rmp.Amat <- rbind(rmp.Amat, new.const)
    new.rhs <- -p %*% d
    rmp.b <- c(rmp.b, new.rhs)
    rmp.dir <- c(rmp.dir, "<=")
  }
  
  if(!is.null(r)) {
    if(has.p) {
      new.const <- c(-p %*% B, 0, 0)
    } else {
      new.const <- -p %*% B
    }
    
    rmp.Amat <- rbind(rmp.Amat, new.const)
    new.rhs <- 0
    rmp.b <- c(rmp.b, new.rhs)
    rmp.dir <- c(rmp.dir, "<=")
  }
  
  sol <- solveLP(cvec=rmp.obj, Amat=rmp.Amat, bvec=rmp.b,
               const.dir=rmp.dir, lpSolve=TRUE)

  new.rmp.model <- list(has.p=has.p, obj=rmp.obj, Amat=rmp.Amat, b=rmp.b)
  
  list(sol=sol, rmp.model=new.rmp.model)
}

convergence <- FALSE
max.iter <- 100
nb.iter <- 0

rmp.model <- list(has.p=FALSE, obj=cost, Amat=A, b=b, dir=rep("=", 2))
p <- 0
r <- 0

while((!convergence) && (nb.iter < max.iter) ) {  
  nb.iter <- nb.iter + 1
  print(paste("Iter", iter))
  
  # Restricted master program
  
  res <- solve.rmp(rmp.model, p=p, r=r)
  x.bar <- res$sol$solution   
  opt <- res$sol$opt
  rmp.model <- res$rmp.model
  LB <- max(LB, opt)
  
  # sub-problem
  
  sub <- solve.subproblem(x.bar)
  p <- NULL
  r <- NULL
  
  if(sub$status == "extreme.point") {
    # termination test 
    UB <- min(UB, sub$sol$opt)
    if(abs(UB-LB) < eps) { convergence <- TRUE}
    
    p <- sub$sol$solution
  }
  
  if(sub$status == "extreme.ray") {
    # feasibility constraint
    r <- sub$sol$solution 
  }
}


