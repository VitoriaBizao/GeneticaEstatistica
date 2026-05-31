set.seed(77)
sigma2u = 3.89
sigma2e = 2
mu = 10
dat = data.frame(
  gen = factor(rep(paste0("P", sprintf(paste0('%0', 2,'d'), seq(1:50))), each = 3)),
  gen_eff = rep(rnorm(50, mean = 0, sd = sqrt(sigma2u)), each = 3),
  noise = rnorm(50*3, mean = 0, sd = sqrt(sigma2e))
)
dat$y = dat$gen_eff + dat$noise + mu

Z = model.matrix(~-1 + gen, data = dat)
Z[1:9, 1:3]

ZlZ = crossprod(Z)
ZlZ[1:5, 1:5]

ZlZ_inv = solve(ZlZ)
round(ZlZ_inv[1:5, 1:5])

Hz = Z %*% ZlZ_inv %*% t(Z)
round(Hz[1:9, 1:9], 2)

X = matrix(1, nrow = length(dat$y))
Hx = X %*% solve(crossprod(X)) %*% t(X)
round(Hx[1:5, 1:5], 2)

sum(diag(Hz))

sum(diag(Hx))

mod = lm(y ~ gen, data = dat)
an = anova(mod); an

mst = an["gen", "Mean Sq"]
mse = an["Residuals", "Mean Sq"]
sigma2u_hat = (mst - mse)/3

real_h2 = sigma2u/(sigma2u+sigma2e)
anova_h2 = sigma2u_hat/(sigma2u_hat + mse)

G = diag(nrow = nlevels(dat$gen)) * sigma2u
R = diag(nrow = nrow(dat)) * sigma2e
V = Z %*% G %*% t(Z) + R 
blue = solve(t(X) %*% solve(V) %*% X) %*% t(X) %*% solve(V) %*% dat$y
blup = G %*% t(Z) %*% solve(V) %*% (dat$y - X %*% blue)

G = diag(nrow = nlevels(dat$gen)) * sigma2u_hat
R = diag(nrow = nrow(dat)) * mse
V = Z %*% G %*% t(Z) + R 
eblue = solve(t(X) %*% solve(V) %*% X) %*% t(X) %*% solve(V) %*% dat$y
eblup = G %*% t(Z) %*% solve(V) %*% (dat$y - X %*% eblue)

library(ggplot2)
aux = cbind(unique(dat[,c("gen","gen_eff")]), solu = c(blup, eblup), 
            method = rep(c("Known", "ANOVA"), each = 50))
aux$method = factor(aux$method, levels = c("Known", "ANOVA"))

ggplot(data = aux, aes(x = solu, y = gen_eff)) + 
  facet_wrap(.~method) + 
  labs(x = "BLUP", y = "Real effect") + 
  geom_point(size = 3, alpha = .75, color = "#10342d") + 
  theme_bw() + 
  theme(text = element_text(size = 20))

herita = data.frame(method = c("Known", "ANOVA"), 
                    value = c(real_h2, anova_h2))
herita$method = factor(herita$method, levels = c("Known", "ANOVA"))

ggplot(data = herita, aes(x = method, y = value)) + 
  geom_segment(aes(x = method, xend = method, y = 0, yend = value), 
               linewidth = 1.5) + 
  geom_point(shape = 19, size = 3.2) + 
  labs(x = "Method", y = "Heritability") + 
  theme_minimal() + 
  theme(text = element_text(size = 20))

(1/(sqrt(2*pi*20))) * exp(-0.5*((90-100)/sqrt(20))^2)

ggplot() +
  geom_segment(
    aes(
      x = 90,
      xend = 90,
      y = 0,
      yend = dnorm(90, mean = 100, sd = sqrt(20))
    ),
    color = "#10342d",
    linewidth = 1.2
  ) +
  stat_function(
    fun = function(x)
      dnorm(x, mean = 100, sd = sqrt(20)),
    colour = '#b2bc63',
    linewidth = 1.5,
    xlim = c(80, 120)
  ) +
  theme_bw() +
  theme(text = element_text(size = 18)) 

(1/(sqrt(2*pi*20))) * exp(-0.5*((90-100)/sqrt(20))^2) * 
  (1/(sqrt(2*pi*20))) * exp(-0.5*((110-100)/sqrt(20))^2)

set.seed(9)
ggplot() + 
  geom_histogram(aes(x = rnorm(750, mean = 0, sd = sqrt(3)),
                     y = after_stat(density)), 
                 color = 'white', fill = "#10342d")+
  theme_minimal() + 
  labs(x = "Real genetic value", y = "Count") + 
  theme(text = element_text(size = 20)) 

set.seed(9)
ggplot() + 
  geom_histogram(aes(x = rnorm(750, mean = 0, sd = sqrt(3)),
                     y = after_stat(density)), 
                 color = 'white', fill = "#10342d") +
  theme_minimal() + 
  labs(x = "Real genetic value", y = "Density") + 
  theme(text = element_text(size = 20)) +
  stat_function(
    fun = function(x)
      dnorm(x, mean = 2, sd = sqrt(2)),
    colour = '#b2bc63',
    linewidth = 1.5
  ) 

set.seed(9)
ggplot() + 
  geom_histogram(aes(x = rnorm(750, mean = 0, sd = sqrt(3)),
                     y = after_stat(density)), 
                 color = 'white', fill = "#10342d") +
  theme_minimal() + 
  labs(x = "Real genetic value", y = "Count") + 
  theme(text = element_text(size = 20)) +
  stat_function(
    fun = function(x)
      dnorm(x, mean = 1, sd = sqrt(1)),
    colour = '#b2bc63',
    linewidth = 1.5
  ) 

set.seed(9)
ggplot() + 
  geom_histogram(aes(x = rnorm(750, mean = 0, sd = sqrt(3)),
                     y = after_stat(density)), 
                 color = 'white', fill = "#10342d") +
  theme_minimal() + 
  labs(x = "Real genetic value", y = "Count") + 
  theme(text = element_text(size = 20)) +
  stat_function(
    fun = function(x)
      dnorm(x, mean = 0, sd = sqrt(3)),
    colour = '#b2bc63',
    linewidth = 1.5
  ) 


s2u = 1
s2e = 4
beta = mean(dat$y)
niter = 100
tol = 1e-6

progress = list()
progress[[1]] = data.frame(iter = 0, beta = beta, s2u = s2u, s2e = s2e)
for (i in 1:niter) {
  # Expectation step
  G = diag(nrow = nlevels(dat$gen)) * s2u
  R = diag(nrow = nrow(dat)) * s2e
  V = Z %*% G %*% t(Z) + R 
  Vinv = solve(V)
  uhat = s2u * t(Z) %*% Vinv %*% (dat$y - X %*% beta)
  Var_u_y = G - G %*% t(Z) %*% Vinv %*% Z %*% G
  
  # Maximization step
  beta_new = solve(t(X) %*% X) %*% t(X) %*% (dat$y - Z %*% uhat)
  s2u_new = as.numeric(t(uhat) %*% uhat + sum(diag(Var_u_y)))/length(unique(dat$gen))
  e_hat = dat$y - X %*% beta_new - Z %*% uhat
  s2e_new = as.numeric(t(e_hat) %*% e_hat + sum(diag(Z %*% Var_u_y %*% t(Z)))) / nrow(dat)
  
  progress[[i + 1]] = data.frame(iter = i, beta = beta_new,
                                 s2u = s2u, s2e = s2e
  )
  if (abs(s2u_new - s2u) < tol && abs(s2e_new - s2e) < tol) {
    break
  } else{
    beta = beta_new
    s2u = s2u_new
    s2e = s2e_new
  }
}

progress = do.call(rbind, progress)
ggplot(data = progress, aes(x = iter, y = s2u)) + 
  geom_point(aes(color = "ML Var(e)", shape = "ML Var(e)"), size = 3) + 
  geom_point(aes(x = iter, y = s2e, color = "ML Var(u)", shape = "ML Var(u)"), size = 3) + 
  geom_hline(aes(yintercept = sigma2u, linetype = "Real Var(u)")) +
  geom_hline(aes(yintercept = sigma2e, linetype = "Real Var(e)")) + 
  geom_hline(aes(yintercept = mse, linetype = "ANOVA Var(e)"))+
  geom_hline(aes(yintercept = sigma2u_hat, linetype = "ANOVA Var(u)")) + 
  labs(shape = "", color = "", linetype = "", x = "Iteration") + 
  theme_bw() + 
  theme(text = element_text(size = 20), legend.position = 'right', 
        legend.title = element_blank(), 
        axis.title.y = element_blank()) + 
  scale_color_manual(values=c("#10342d", "#b2bc63"))

herita = rbind(herita, data.frame(method = factor("ML"), 
                                  value = progress[nrow(progress),3]/
                                    sum(progress[nrow(progress),c(3,4)])))

ggplot(data = herita, aes(x = method, y = value)) + 
  geom_segment(aes(x = method, xend = method, y = 0, yend = value), 
               linewidth = 1.5) + 
  geom_point(shape = 19, size = 3.2) + 
  labs(x = "Method", y = "Heritability") + 
  theme_minimal() + 
  theme(text = element_text(size = 20))

G = diag(nrow = nlevels(dat$gen)) * progress$s2u[nrow(progress)]
R = diag(nrow = nrow(dat)) * progress$s2e[nrow(progress)]
V = Z %*% G %*% t(Z) + R 
eblue = solve(t(X) %*% solve(V) %*% X) %*% t(X) %*% solve(V) %*% dat$y
eblup = G %*% t(Z) %*% solve(V) %*% (dat$y - X %*% eblue)
aux = rbind(aux, cbind(unique(dat[,c("gen","gen_eff")]), solu = eblup, 
                       method = factor("ML")))

ggplot(data = aux, aes(x = solu, y = gen_eff)) + 
  facet_wrap(.~method) + 
  labs(x = "BLUP", y = "Real effect") + 
  geom_point(size = 3, alpha = .75, color = "#10342d") + 
  theme_bw() + 
  theme(text = element_text(size = 20))


s2u = 1
s2e = 4
beta = mean(dat$y)
niter = 100
tol = 1e-6

progress2 = list()
progress2[[1]] = data.frame(iter = 0, beta = beta, s2u = s2u, s2e = s2e)
for (i in 1:niter) {
  
  # Expectation step
  G = diag(nrow = nlevels(dat$gen)) * s2u
  R = diag(nrow = nrow(dat)) * s2e
  V = Z %*% G %*% t(Z) + R 
  Vinv = solve(V)
  beta_new = solve(t(X) %*% Vinv %*% X) %*% t(X) %*% Vinv %*% dat$y
  P = Vinv - Vinv %*% X %*% solve(t(X) %*% Vinv %*% X) %*% t(X) %*% Vinv
  uhat = s2u * t(Z) %*% Vinv %*% (dat$y - X %*% beta)
  Var_u_y = G - G %*% t(Z) %*% Vinv %*% Z %*% G
  e_hat = dat$y - X %*% beta_new - Z %*% uhat
  Var_e_y = R - R %*% P %*% R
  
  
  # Maximization step
  s2u_new = as.numeric(t(uhat) %*% uhat + sum(diag(Var_u_y)))/length(unique(dat$gen))
  s2e_new = as.numeric(t(e_hat) %*% e_hat + sum(diag(Var_e_y))) / nrow(dat)
  
  progress2[[i + 1]] = data.frame(
    iter = i,
    beta = beta_new,
    s2u = s2u,
    s2e = s2e
  )
  
  if (abs(s2u_new - s2u) < tol && abs(s2e_new - s2e) < tol) {
    break
  } else{
    beta = beta_new
    s2u = s2u_new
    s2e = s2e_new
  }
}


progress2 = do.call(rbind, progress2)
progress2$method = "REML"
progress$method = "ML"
progress = rbind(progress, progress2)
ggplot(data = progress, aes(x = iter, y = s2u)) +
  facet_wrap(.~method) + 
  geom_point(aes(color = "(RE)ML Var(e)", shape = "(RE)ML Var(e)"), size = 3) +
  geom_point(aes(x = iter, y = s2e, color = "(RE)ML Var(u)", shape = "(RE)ML Var(u)"), size = 3) + 
  geom_hline(aes(yintercept = sigma2u, linetype = "Real Var(u)")) +
  geom_hline(aes(yintercept = sigma2e, linetype = "Real Var(e)")) + 
  geom_hline(aes(yintercept = mse, linetype = "ANOVA Var(e)"))+
  geom_hline(aes(yintercept = sigma2u_hat, linetype = "ANOVA Var(u)")) + 
  labs(shape = "", color = "", linetype = "", x = "Iteration") + 
  theme_bw() + 
  theme(text = element_text(size = 20), legend.position = 'right', 
        legend.title = element_blank(), 
        axis.title.y = element_blank()) + 
  scale_color_manual(values=c("#10342d", "#b2bc63"))


herita = rbind(herita, data.frame(method = factor("REML"), 
                                  value = progress[nrow(progress),3]/
                                    sum(progress[nrow(progress),c(3,4)])))

ggplot(data = herita, aes(x = method, y = value)) + 
  geom_segment(aes(x = method, xend = method, y = 0, yend = value), 
               linewidth = 1.5) + 
  geom_point(shape = 19, size = 3.2) + 
  labs(x = "Method", y = "Heritability") + 
  theme_minimal() + 
  theme(text = element_text(size = 20))

G = diag(nrow = nlevels(dat$gen)) * progress$s2u[nrow(progress)]
R = diag(nrow = nrow(dat)) * progress$s2e[nrow(progress)]
V = Z %*% G %*% t(Z) + R 
eblue = solve(t(X) %*% solve(V) %*% X) %*% t(X) %*% solve(V) %*% dat$y
eblup = G %*% t(Z) %*% solve(V) %*% (dat$y - X %*% eblue)
aux = rbind(aux, cbind(unique(dat[,c("gen","gen_eff")]), solu = eblup, 
                       method = factor("REML")))

ggplot(data = aux, aes(x = solu, y = gen_eff)) + 
  facet_wrap(.~method) + 
  labs(x = "BLUP", y = "Real effect") + 
  geom_point(size = 3, alpha = .75, color = "#10342d") + 
  theme_bw() + 
  theme(text = element_text(size = 20))

set.seed(233)
dat2 = dat
dat2$y[sample(length(dat2$y), 40)] = NA
Zd = model.matrix(~-1 + gen, 
                  data = na.exclude(dat2))
Xd = matrix(1, nrow = nrow(na.exclude(dat2)))

table(na.exclude(dat2)$gen)

mod = lm(y ~ gen, data = dat2, contrasts = list(gen = "contr.sum"))
an = car::Anova(mod, type = "II")

mst = an["gen", 1]/an["gen", 2]
mse = an["Residuals", 1]/an["Residuals", 2]

# New denominator
den = (1/(nlevels(dat$gen)))*(nrow(dat)-(sum(table(na.exclude(dat2)$gen))/nrow(dat)))

sigma2u_hat = (mst - mse)/den

G = diag(nrow = nlevels(dat$gen)) * sigma2u
R = diag(nrow = nrow(dat)) * sigma2e
V = Z %*% G %*% t(Z) + R 
blue = solve(t(X) %*% solve(V) %*% X) %*% t(X) %*% solve(V) %*% dat$y
blup = G %*% t(Z) %*% solve(V) %*% (dat$y - X %*% blue)

Gd = diag(nrow = nlevels(dat2$gen)) * sigma2u_hat
Rd = diag(nrow = nrow(na.exclude(dat2))) * mse
Vd = Zd %*% Gd %*% t(Zd) + Rd 
eblue = solve(t(Xd) %*% solve(Vd) %*% Xd) %*% t(Xd) %*% solve(Vd) %*% na.exclude(dat2)$y
eblup = G %*% t(Zd) %*% solve(Vd) %*% (na.exclude(dat2)$y - Xd %*% eblue)

aux = cbind(unique(dat[,c("gen","gen_eff")]), solu = c(blup, eblup), 
            method = rep(c("Known", "ANOVA"), each = 50))
aux$method = factor(aux$method, levels = c("Known", "ANOVA"))

ggplot(data = aux, aes(x = solu, y = gen_eff)) + 
  facet_wrap(.~method) + 
  labs(x = "BLUP", y = "Real effect") + 
  geom_point(size = 3, alpha = .75, color = "#10342d") + 
  theme_bw() + 
  theme(text = element_text(size = 20))

herita = data.frame(method = c("Known", "ANOVA"), 
                    value = c(real_h2, sigma2u_hat/(sigma2u_hat+mse)))
herita$method = factor(herita$method, levels = c("Known", "ANOVA"))

ggplot(data = herita, aes(x = method, y = value)) + 
  geom_segment(aes(x = method, xend = method, y = 0, yend = value), 
               linewidth = 1.5) + 
  geom_point(shape = 19, size = 3.2) + 
  labs(x = "Method", y = "Heritability") + 
  theme_minimal() + 
  theme(text = element_text(size = 20))

s2u = 1
s2e = 4
y = na.exclude(dat2)$y
beta = mean(y)
niter = 100
tol = 1e-6

progress = list()
progress[[1]] = data.frame(iter = 0, beta = beta, s2u = s2u, s2e = s2e)
for (i in 1:niter) {
  
  # Expectation step
  Gd = diag(nrow = nlevels(dat2$gen)) * s2u
  Rd = diag(nrow = nrow(na.exclude(dat2))) * s2e
  Vd = Zd %*% Gd %*% t(Zd) + Rd 
  Vinv = solve(Vd)
  uhat = s2u * t(Zd) %*% Vinv %*% (y - Xd %*% beta)
  Var_u_y = Gd - Gd %*% t(Zd) %*% Vinv %*% Zd %*% Gd
  
  # Maximization step
  beta_new = solve(t(Xd) %*% Xd) %*% t(Xd) %*% (y - Zd %*% uhat)
  s2u_new = as.numeric(t(uhat) %*% uhat + sum(diag(Var_u_y)))/length(unique(dat$gen))
  e_hat = y - Xd %*% beta_new - Zd %*% uhat
  s2e_new = as.numeric(t(e_hat) %*% e_hat + sum(diag(Zd %*% Var_u_y %*% t(Zd)))) / length(y)
  
  if (abs(s2u_new - s2u) < tol && abs(s2e_new - s2e) < tol) {
    beta = beta_new
    s2u = s2u_new
    s2e = s2e_new
    progress[[i + 1]] = data.frame(
      iter = i,
      beta = beta_new,
      s2u = s2u,
      s2e = s2e
    )
    break
  } else{
    beta = beta_new
    s2u = s2u_new
    s2e = s2e_new
    progress[[i + 1]] = data.frame(
      iter = i,
      beta = beta_new,
      s2u = s2u,
      s2e = s2e
    )
  }
}

progress = do.call(rbind, progress)
Gd = diag(nrow = nlevels(dat2$gen)) * progress$s2u[nrow(progress)]
Rd = diag(nrow = nrow(na.exclude(dat2))) * progress$s2e[nrow(progress)]
Vd = Zd %*% Gd %*% t(Zd) + Rd 
eblue = solve(t(Xd) %*% solve(Vd) %*% Xd) %*% t(Xd) %*% solve(Vd) %*% y
eblup = Gd %*% t(Zd) %*% solve(Vd) %*% (y - Xd %*% eblue)
aux = rbind(aux, cbind(unique(dat[,c("gen","gen_eff")]), solu = eblup, 
                       method = factor("ML")))

ggplot(data = aux, aes(x = solu, y = gen_eff)) + 
  facet_wrap(.~method) + 
  labs(x = "BLUP", y = "Real effect") + 
  geom_point(size = 3, alpha = .75, color = "#10342d") + 
  theme_bw() + 
  theme(text = element_text(size = 20))

herita = rbind(herita, data.frame(method = factor("ML"), 
                                  value = progress[nrow(progress),3]/
                                    sum(progress[nrow(progress),c(3,4)])))

ggplot(data = herita, aes(x = method, y = value)) + 
  geom_segment(aes(x = method, xend = method, y = 0, yend = value), 
               linewidth = 1.5) + 
  geom_point(shape = 19, size = 3.2) + 
  labs(x = "Method", y = "Heritability") + 
  theme_minimal() + 
  theme(text = element_text(size = 20))

s2u = 1
s2e = 4
y = na.exclude(dat2)$y
beta = mean(y)
niter = 100
tol = 1e-6

progress2 = list()
progress2[[1]] = data.frame(iter = 0, beta = beta, s2u = s2u, s2e = s2e)
for (i in 1:niter) {
  
  # Expectation step
  Gd = diag(nrow = nlevels(dat2$gen)) * s2u
  Rd = diag(nrow = nrow(na.exclude(dat2))) * s2e
  Vd = Zd %*% Gd %*% t(Zd) + Rd 
  Vinv = solve(Vd)
  beta_new = solve(t(Xd) %*% Vinv %*% Xd) %*% t(Xd) %*% Vinv %*% y
  P = Vinv - Vinv %*% Xd %*% solve(t(Xd) %*% Vinv %*% Xd) %*% t(Xd) %*% Vinv
  uhat = s2u * t(Zd) %*% Vinv %*% (y - Xd %*% beta)
  Var_u_y = Gd - Gd %*% t(Zd) %*% Vinv %*% Zd %*% Gd
  e_hat = y - Xd %*% beta_new - Zd %*% uhat
  Var_e_y = Rd - Rd %*% P %*% Rd
  
  
  # Maximization step
  s2u_new = as.numeric(t(uhat) %*% uhat + sum(diag(Var_u_y)))/length(unique(dat$gen))
  s2e_new = as.numeric(t(e_hat) %*% e_hat + sum(diag(Var_e_y))) / length(y)
  
  progress2[[i + 1]] = data.frame(
    iter = i,
    beta = beta_new,
    s2u = s2u,
    s2e = s2e
  )
  
  if (abs(s2u_new - s2u) < tol && abs(s2e_new - s2e) < tol) {
    break
  } else{
    beta = beta_new
    s2u = s2u_new
    s2e = s2e_new
  }
}

progress2 = do.call(rbind, progress2)
progress2$method = "REML"
progress$method = "ML"
progress = rbind(progress, progress2)

Gd = diag(nrow = nlevels(dat2$gen)) * progress$s2u[nrow(progress)]
Rd = diag(nrow = nrow(na.exclude(dat2))) * progress$s2e[nrow(progress)]
Vd = Zd %*% Gd %*% t(Zd) + Rd
eblue = solve(t(Xd) %*% solve(Vd) %*% Xd) %*% t(Xd) %*% solve(Vd) %*% y
eblup = Gd %*% t(Zd) %*% solve(Vd) %*% (y - Xd %*% eblue)
aux = rbind(aux, cbind(unique(dat[,c("gen","gen_eff")]), solu = eblup, 
                       method = factor("REML")))

ggplot(data = aux, aes(x = solu, y = gen_eff)) + 
  facet_wrap(.~method) + 
  labs(x = "BLUP", y = "Real effect") + 
  geom_point(size = 3, alpha = .75, color = "#10342d") + 
  theme_bw() + 
  theme(text = element_text(size = 20))

herita = rbind(herita, data.frame(method = factor("REML"), 
                                  value = progress[nrow(progress),3]/
                                    sum(progress[nrow(progress),c(3,4)])))

ggplot(data = herita, aes(x = method, y = value)) + 
  geom_segment(aes(x = method, xend = method, y = 0, yend = value), 
               linewidth = 1.5) + 
  geom_point(shape = 19, size = 3.2) + 
  labs(x = "Method", y = "Heritability") + 
  theme_minimal() + 
  theme(text = element_text(size = 20))

