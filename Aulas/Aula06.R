maize = read.csv(file = "Aulas/maize.csv", row.names = 1)
str(maize)

set.seed(7)
base_mu = 10
g1 = base_mu + rnorm(2, mean = 5, sd = 2)
g2 = base_mu + rnorm(2, mean = 1, sd = 2)
g3 = base_mu + rnorm(2, mean = 10, sd = 2)
dat = data.frame(clone = rep(c("G1", "G2", "G3"), each = 2), 
                 y = round(c(g1, g2, g3), 3))
dat

y = maize[,"GY"]
X = as.matrix(cbind(1, maize[,1]))
XlX = crossprod(X)
Xly = crossprod(X, y)
beta = solve(XlX) %*% Xly; round(beta, 3)

mod = lm(GY ~ M1, data = maize)
mod$coefficients

library(ggplot2)
ggplot(data = maize, aes(x = M1, y = GY)) + 
  geom_jitter() + 
  geom_smooth(method = 'lm', color = "#10342d") + 
  scale_x_continuous(breaks = c(0, 1, 2)) + 
  theme_minimal() + 
  theme(text = element_text(size = 20))


y = maize[,"GY"]
X = as.matrix(cbind(1, maize[,c(1,3)]))
XlX = crossprod(X)
Xly = crossprod(X, y)
beta = solve(XlX) %*% Xly; round(beta,3)

mod = lm(GY ~ M1 + M3, data = maize)
mod$coefficients

library(ggplot2)
ggplot(data = maize, aes(x = M1, y = GY)) + 
  geom_jitter() + 
  geom_smooth(method = 'lm', color = "#10342d") + 
  scale_x_continuous(breaks = c(0, 1, 2)) + 
  theme_minimal() + 
  theme(text = element_text(size = 20))

y = maize[,"GY"]
X = as.matrix(cbind(1, maize[,c(1,3)]))
XlX = crossprod(X)
Xly = crossprod(X, y)
beta = solve(XlX) %*% Xly; round(beta,3)

mod = lm(GY ~ M1 + M3, data = maize)
mod$coefficients

t(head(X %*% beta))
head(mod$fitted.values)

H = X %*% solve(XlX) %*% t(X)

n = length(y)
I = diag(nrow = n)
rX = qr(X)$rank
SSresid =t(y) %*% (I - H) %*% y
df_res = n-rX
sigma2 = SSresid/df_res; sigma2

sum(mod$residuals^2)/mod$df.residual

varbeta = as.numeric(sigma2) * solve(XlX)
SE_beta = sqrt(diag(varbeta))
alpha = 0.05/2
t_crit = qt(p = alpha, df = df_res)
data.frame(beta, upperCI = beta + t_crit * SE_beta, lowerCI = beta - t_crit * SE_beta)

confint(mod)

J = matrix(1, nrow = n, ncol = n)
SSreg = t(y) %*% (H - (1/n)*J) %*% y
MSreg = SSreg/qr((H - (1/n)*J))$rank
Ftest = MSreg/sigma2
1-pf(q = Ftest, df1 = qr((H - (1/n)*J))$rank, df2 = df_res)

summa_mod = summary(mod)
summa_mod$fstatistic

t_calc = beta/SE_beta
(1-pt(t_calc, df = df_res))*2

summa_mod$coefficients

I = diag(nrow = n)
SStot = t(y) %*% (I - (1/n)*J) %*% y
R2 = (1-(sigma2 * df_res/SStot)); R2

summa_mod$r.squared

library(ggfortify)
autoplot(mod, which = 1, nrow = 1, ncol = 1) + 
  theme_bw() + 
  theme(text = element_text(size = 14))

autoplot(mod, which = 2, nrow = 1, ncol = 1) + 
  theme_bw() + 
  theme(text = element_text(size = 14))

autoplot(mod, which = 3, nrow = 1, ncol = 1) + 
  theme_bw() + 
  theme(text = element_text(size = 14))

autoplot(mod, which = c(4,5,6), nrow = 1, ncol = 3) + 
  theme_bw() + 
  theme(text = element_text(size = 15))

library(performance)
check_model(model = mod)

x1 = maize$M3 - 1
mod = lm(y ~ x1 + I(x1^2))
summary(mod)

ggplot(data = maize, aes(x = M3, y = GY))+ 
  geom_smooth(method = 'lm', formula = y ~ x + I(x^2), se = TRUE, colour = 'firebrick') + 
  geom_jitter(width = .2, size = 2.4) + 
  theme_minimal() + 
  theme(text = element_text(size = 15)) +
  scale_x_continuous("M3", breaks = c(0, 1, 2))

y = dat$y
X = model.matrix(~-1 + clone, data = dat)
colnames(X) = gsub("clone", "", colnames(X))
X = cbind(mu = 1, X)

qr(X)$rank

XlX = crossprod(X)
det(XlX)

solve(XlX)

X_c1 = model.matrix(~-1 + clone, data = dat)
colnames(X_c1) = gsub("clone", "", colnames(X_c1))
qr(X_c1)$rank
XlX = crossprod(X_c1)
beta_c1 = solve(XlX) %*% crossprod(X_c1, y); beta_c1
tapply(dat$y, dat$clone, mean)

X_c2 = model.matrix(~ clone, data = dat)
colnames(X_c2) = gsub("clone", "", colnames(X_c2))
qr(X_c2)$rank

XlX = crossprod(X_c2)
beta_c2 = solve(XlX) %*% crossprod(X_c2, y); beta_c2

mod = lm(y ~ clone, data = dat)
mod

