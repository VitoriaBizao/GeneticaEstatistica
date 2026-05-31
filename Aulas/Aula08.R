beta0 = 10
sigma = 5
epsilons = rnorm(10, mean = 0, sd = sigma)
y = beta0 + epsilons
y

sigmab = 3
beta0 = rnorm(10, mean = 0, sd = sigmab)
epsilons = rnorm(10, mean = 0, sd = sigma)
y = beta0 + epsilons
y

sigma2u = 3.89
sigma2e = 2
mu = 10

set.seed(77)
dat = data.frame(
  gen = factor(paste0("P", 1:5)),
  gen_eff = rnorm(5, mean = 0, sd = sqrt(sigma2u)),
  noise = rnorm(5, mean = 0, sd = sqrt(sigma2e))
)
dat$y = dat$gen_eff + dat$noise + mu

Z = model.matrix(~-1 +dat$gen)
X = matrix(1, nrow = length(dat$y))

G = diag(nrow = nlevels(dat$gen)) * sigma2u
R = diag(nrow = nrow(dat)) * sigma2e

C11 = crossprod(X)

C21 = crossprod(Z, X)
C12 = crossprod(X,Z)

C22 = crossprod(Z) + sigma2e/sigma2u * diag(nrow = nlevels(dat$gen))

coefmat = rbind(cbind(C11, C12),cbind(C21, C22))

rhs = rbind(crossprod(X, dat$y), crossprod(Z, dat$y))

inv_coefmat %*% rhs

V = Z %*% G %*% t(Z) + R    # Take a closer look at V
blue = solve(t(X) %*% solve(V) %*% X) %*% t(X) %*% solve(V) %*% dat$y; blue

blup = G %*% t(Z) %*% solve(V) %*% (dat$y - X %*% blue); blup

sigma2u/(sigma2u + sigma2e) * (tapply(dat$y, dat$gen, mean)[1] - blue)

sigma2u/(sigma2u + sigma2e) * (tapply(dat$y, dat$gen, mean)[4] - blue)

library(tidyverse)
h2 = c(seq(0.01,1,0.01),1)
lambdas = (1-h2)/h2
enco = list()
for (i in seq_along(lambdas)) {
  enco[[i]] = data.frame(h2 = h2[i],
                         lambda = lambdas[i],
                         blup = solve(crossprod(Z) + lambdas[i] *
                                        diag(nrow = nlevels(dat$gen))) %*% (rhs[-1] -
                                                                              C21 %*% blue)) |> 
    rownames_to_column("gen") |> mutate(gen = gsub("dat\\$gen","",gen))
}
enco = do.call(rbind, enco)

ggplot(data = enco, aes(x = h2, y = lambda)) + 
  geom_point(size = 3, alpha = .3) + 
  theme_minimal() + 
  labs(x = "Heritability", y = "Shrinkage factor") + 
  theme(text = element_text(size = 25))

library(gganimate)
library(ggimage)
enco$lambda = round(enco$lambda, 2)
temp = left_join(enco, dat[,c("gen", "gen_eff")], by = "gen")
p = ggplot(data = temp, 
           aes(x = gen_eff, y = blup)) + 
  geom_point(size = 3) +
  theme_minimal() + 
  labs(x = "True value", y = "BLUP", caption = "Shrinkage factor = {closest_state}") + 
  theme(text = element_text(size = 20))+ 
  geom_hline(aes(yintercept = 0), linetype = "dashed") 

anim = p + transition_states(
  lambda,
  transition_length = 1.5,
  state_length = 1,
  wrap = FALSE
) + enter_fade() +
  exit_shrink()
anim

set.seed(77)
dat = data.frame(
  gen = factor(rep(paste0("P", sprintf(paste0('%0', 2,'d'), seq(1:50))), each = 3)),
  gen_eff = rep(rnorm(50, mean = 0, sd = sqrt(sigma2u)), each = 3),
  noise = rnorm(50*3, mean = 0, sd = sqrt(sigma2e))
)
dat$y = dat$gen_eff + dat$noise + mu
str(dat)

Z = model.matrix(~-1 +dat$gen)
X = matrix(1, nrow = length(dat$y))
G = diag(nrow = nlevels(dat$gen)) * sigma2u
R = diag(nrow = nrow(dat)) * sigma2e
V = Z %*% G %*% t(Z) + R
lambda = sigma2e/sigma2u
C11 = crossprod(X)
C21 = crossprod(Z, X)
C12 = crossprod(X,Z)
C22 = crossprod(Z) + lambda *
  diag(nrow = nlevels(dat$gen))
coefmat = rbind(cbind(C11, C12),
                cbind(C21, C22))
inv_coefmat = solve(coefmat)
rhs = rbind(crossprod(X, dat$y), 
            crossprod(Z, dat$y))
solu = inv_coefmat %*% rhs; solu

sigma2u/(sigma2u + (sigma2e/3)) * (tapply(dat$y, dat$gen, mean)[1] - mean(dat$y))

sigma2u/(sigma2u + (sigma2e/3)) * (tapply(dat$y, dat$gen, mean)[4] - mean(dat$y))

set.seed(12)
dat2 = dat
dat2$y[sample(length(dat2$y), 20)] = NA
G = diag(nrow = nlevels(dat2$gen)) * sigma2u
R = diag(nrow = nrow(na.exclude(dat2))) * 
  sigma2e
Z = model.matrix(~-1 + gen, 
                 data = na.exclude(dat2))
X = matrix(1, nrow = nrow(na.exclude(dat2)))
Vd = Z %*% G %*% t(Z) + R
C11d = crossprod(X)
C21d = crossprod(Z, X)
C12d = crossprod(X,Z)
C22d = crossprod(Z) + lambda * 
  diag(nrow = nlevels(dat2$gen))
coefmat = rbind(cbind(C11d, C12d),
                cbind(C21d, C22d))
inv_coefmat = solve(coefmat)
rhsd = rbind(crossprod(X,na.exclude(dat2)$y), 
             crossprod(Z,na.exclude(dat2)$y))
solud = inv_coefmat %*% rhsd; solud

nrept = tapply(dat2$y, dat2$gen, function(x) sum(!is.na(x)))
sigma2u/(sigma2u + (sigma2e/nrept["P01"])) * 
  (tapply(dat2$y, dat2$gen, function(x) mean(x, na.rm=TRUE))["P01"] - solud[1])

sigma2u/(sigma2u + (sigma2e/nrept["P30"])) * 
  (tapply(dat2$y, dat2$gen, function(x) mean(x, na.rm=TRUE))["P30"] - solud[1])

unique(diag(C22))

unique(diag(C22d))

unique(1-(diag(solve(C22d))/sigma2u))

library(ComplexHeatmap)
library(gridGraphics)

h1 = grid.grabExpr(draw(Heatmap(
  matrix = V[1:30, 1:30],
  row_order = rownames(V[1:30, 1:30]),
  column_order = colnames(V[1:30, 1:30]),
  row_split = dat$gen[1:30],
  column_split = dat$gen[1:30],
  show_heatmap_legend = FALSE,
  col = viridis::mako(20)
)))
h2 = grid.grabExpr(draw(Heatmap(
  matrix = Vd[1:30, 1:30],
  row_order = rownames(Vd[1:30, 1:30]),
  column_order = colnames(Vd[1:30, 1:30]),
  row_split = na.exclude(dat2)$gen[1:30],
  column_split = na.exclude(dat2)$gen[1:30],
  show_heatmap_legend = FALSE,
  col = viridis::mako(20)
)))

patchwork::wrap_plots(h1,h2)

h2 = c(seq(0.01,1,0.01),1)
enco = list()
for (i in seq_along(h2)){
  nrept1 = tapply(dat$y, dat$gen, function(x) sum(!is.na(x)))
  lambda1 = (nrept1*h2[i])/(1+(nrept1 - 1) * h2[i])
  nrept2 = tapply(dat2$y, dat2$gen, function(x) sum(!is.na(x)))
  lambda2 = (nrept2*h2[i])/(1+(nrept2 - 1) * h2[i])
  enco[[i]] = rbind(
    data.frame(
      h2 = h2[i],
      lambda = lambda1,
      nrept = nrept1,
      blup = lambda1 *
        (tapply(dat$y, dat$gen, mean) - solu[1])
    ) |>
      rownames_to_column("gen") |>
      mutate(gen = gsub("dat\\$gen", "", gen), simu = "Balanced"), data.frame(
        h2 = h2[i],
        lambda = lambda2,
        nrept = nrept2,
        blup = lambda2 *
          (tapply(dat$y, dat$gen, mean) - solu[1])
      ) |>
      rownames_to_column("gen") |>
      mutate(gen = gsub("dat\\$gen", "", gen), simu = "Unbalanced")
  )
}

enco = do.call(rbind, enco)

temp = left_join(enco, unique(dat[,c("gen", "gen_eff")]), by = "gen") 
p = ggplot(data = temp, 
           aes(x = gen_eff, y = blup)) + 
  facet_wrap(.~simu) +
  geom_point(size = 3, aes(color = as.factor(nrept))) +
  theme_minimal() + 
  labs(x = "True value", y = "BLUP", caption = "Heritability = {closest_state}",
       color = "No. replicates") + 
  theme(text = element_text(size = 20), legend.position = "top")+ 
  geom_hline(aes(yintercept = 0), linetype = "dashed") +
  scale_color_manual(values = rev(c("#10342d", "#517300", "#b2bc63")))

anim = p + transition_states(
  h2,
  transition_length = 1.5,
  state_length = 1,
  wrap = FALSE
) + enter_fade() +
  exit_shrink()
anim

