# Aula 05.2
A <- matrix(c(10,20,30,40,
              5,5,5,5,
              5,15,25,35,
              1,2,3,4), ncol = 4, byrow = T); A

B <- matrix(c(1,5,9,
              2,8,3,
              10,4,7,
              6,2,5), nrow = 3); B

C <- matrix(c(1,3,
              5,2,
              8,10,
              4,6), nrow = 2, ncol = 4); C

# traço
sum(diag(A))
sum(diag(B))
sum(diag(C))

# Posto
qr(A)$rank
qr(B)$rank
qr(C)$rank

A*B

B*A

dim(A)
dim(B)

A*B%*%t(C)

C%*%t(B)

v <- c(2,4,6,8)
sum(v)

u <- c(1,2,0,-1)

