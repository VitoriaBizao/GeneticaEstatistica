# Aula 05
A = matrix(c(
  11.2, 15.3,
  10.4, 15.6, 
  8.2, 12.9,
  8.4,11.7
), ncol = 2, byrow = TRUE)

# Matriz transposta
Aprime = t(A);Aprime

t(Aprime)

B = matrix(c(3, 2, 6, 2, 10, -7, 6, -7, 9), nrow = 3)
t(B)

diag(B)

# Matriz identidade
diag(nrow = 5)

B[lower.tri(B)]

B[upper.tri(B)]

m1 = c(1, 2, 3, 4)
m2 = c(5, 6, 7, 8)
m3 = c(9, 10, 11, 12)
cbind(m1, m2, m3)
M = rbind(m1, m2, m3, m4);M

# Traço da matriz
sum(diag(B))
sum(diag(B)) == sum(diag(t(B)))

# Soma de matrizes
A = matrix(round(runif(8, min = 1, max = 20)), nrow = 2)
B = matrix(round(runif(8, min = 1, max = 20)), nrow =2)
C = A + B;C

A + B == B + A
t(A + B) == t(A) + t(B)

# Produtos
10 * A == A * 10
D = matrix(round(runif(8, min = 1, max = 20)), nrow = 4)
A %*% D
E = D %*% A; E
E %*% diag(nrow = 4) == diag(nrow = 4) %*% E

A = matrix(round(runif(16, min = 1, max = 20)), nrow = 4)
B = matrix(round(runif(16, min = 1, max = 20)), nrow = 4)
C = matrix(round(runif(16, min = 1, max = 20)), nrow = 4)
D = matrix(round(runif(16, min = 1, max = 20)), nrow = 4)

(A-B)%*%(C-D)
A = matrix(round(runif(18, min = 1, max = 20)), nrow = 3)
b = round(runif(6, min = 1, max = 20))
c = round(runif(6, min = 1, max = 20))
d = round(runif(3, min = 1, max = 20))
A%*%b
d%*%A
b%*%c
b%*%t(c)
c%*%t(d)
crossprod(b)
tcrossprod(b)
sqrt(crossprod(b))
sqrt(sum(b^2))
norm(x = as.matrix(b), type = "F")

j = rep(1, length(b))
crossprod(b,j)
crossprod(j,b)

one_R = matrix(1, nrow = 3)
one_R2 = matrix(1, nrow = 4)
J1 = one_R %*% t(one_R2)
J2 = one_R2 %*% t(one_R)
J1 %*% J2
t(one_R) %*% J1
J1 %*% one_R2

I = diag(nrow = 5)
J = matrix(1, nrow = 5, ncol = 5)
Jbar = 1/5*J
C = I - Jbar; C
x = rnorm(5, mean = 5, sd = 1)
t(x)%*%C
x-mean(x)
t(x)%*%C%*%x
sum((x-mean(x))^2)

j = rep(1, nrow(E))
t(j) %*% E
colSums(E)
E %*% j
rowSums(E)

G = matrix(round(runif(12, min = 1, max = 20)), nrow = 6)
t(A %*% G) == t(G) %*% t(A)
t(A) %*% A
crossprod(A) == t(A) %*% A
A %*% t(A)
tcrossprod(A) == A %*% t

tcrossprod(A) == A %*% t(A)
maize = read.csv("../../Misc/Z005.csv",row.names = 1)
maize = as.matrix(maize[,-seq(ncol(maize)-32, ncol(maize), 1)])
maize = maize[1:20, 1:100]
maize[is.na(maize)] = mean(maize, na.rm = TRUE)
round(tcrossprod(maize)[1:10,1:7])

n = 20
y = rnorm(n, mean = 10, sd = 2)
var(y)
1/(n-1) * (t(y) %*% (diag(nrow = n) - (1/n)*matrix(1, nrow = n, ncol = n)) %*% y)

x = rnorm(n, mean = 10, sd = 2)
cov(x,y)
1/(n-1) * (t(x) %*% (diag(nrow = n) - (1/n)*matrix(1, nrow = n, ncol = n)) %*% y)

B * D