library(linprog)

r.up <- c(1.25, 1.14)
r.down <- c(1.06, 1.12)
G = 130
W.0 = 100
r = .02
q = .05

A <- matrix(0, nrow=7, ncol=14)
A[1,1:2] = 1
A[2,1:2] = r.up
A[2,3:4] = -1
A[3,1:2] = r.down
A[3,5:6] = -1
A[4, 3:4] = r.up
A[5, 3:4] = r.down
A[6, 5:6] = r.up
A[7, 5:6] = r.down
A[4, 7:8] = c(-1,1)
A[5, 9:10] = c(-1,1)
A[6, 11:12] = c(-1,1)
A[7, 13:14] = c(-1,1)
B = c(W.0, 0, 0, rep(G,4))  

obj = c(rep(0,6), rep(c(r,-q),4))

res = solveLP(cvec=obj, bvec=B, Amat=A, maximum = TRUE, 
        lpSolve=TRUE, const.dir=rep("=",7))