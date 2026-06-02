library(dplyr)
library(ggplot2)

mod = lm(y ~ clone, data = dat)
mod$residuals

nrept = 8
geno_eff = data.frame(clone = as.factor(c(paste0("C0", 1:9), "C10")),
                      g = sample(seq(1,20, 0.5), 10))
dat = expand.grid(row = 1:nrept, col = 1:10) |> 
  mutate(random_noise = rnorm(nrept*10, mean = 0, sd = 1)) |> 
  left_join(cbind(geno_eff, col = 1:10), by = "col") |> 
  mutate(y = random_noise + g, y = ifelse(y<0, 0, y))

ggplot(data = dat, aes(x = as.factor(col), y = as.factor(row))) + 
  geom_tile(aes(fill = y)) + 
  geom_text(aes(label = paste0(clone, "\n(",round(y,2),")"))) + 
  scale_fill_gradient(high = "#b2bc63", low = "#10342d") + 
  theme_minimal() + 
  theme(text = element_text(size = 18), legend.position = "none") + 
  labs(x = "Column", y = "Row")

mod2 = lm(y ~ clone, data = dat)
anova(mod1)["Residuals",]

anova(mod2)["Residuals",]

smut_pop1 = rnorm(10000, mean = 5, sd = 1)
smut_pop2 = rnorm(10000, mean = 5.3, sd = 1)
alpha1 = 0.05
alpha2 = 0.01
power1 = vector()
power2 = vector()
for (i in seq(10, 1000, 10)) {
  sample_size1 = sample_size2 = i
  pwr = replicate(100, expr = {
    smut_sam1 = sample(x = smut_pop1,
                       size = sample_size1,
                       replace = FALSE)
    smut_sam2 = sample(x = smut_pop2,
                       size = sample_size2,
                       replace = FALSE)
    pval = t.test(smut_sam1, smut_sam2)$p.value
    return(pval)
  })
  power1 = c(power1, mean(pwr <= alpha1))
  power2 = c(power2, mean(pwr <= alpha2))
}

ggplot(data = data.frame(
  power = c(power1, power2), 
  size = rep(seq(10, 1000, 10),2),
  alpha = rep(c("0.05", "0.01"), each = length(power1))
), aes(x = size, y = power, color = alpha, shape = alpha)) + 
  geom_point(size = 4, alpha = .7) + 
  labs(x = "Sample size", y = "Power", color = expression(alpha),
       shape = expression(alpha)) + 
  theme_minimal() + 
  theme(text = element_text(size = 24), legend.position = 'top') + 
  scale_color_manual(values = c('#10342d', '#b2bc63'))

set.seed(53)
n_plant = 50
trayA = 5 
trayB = -5
hybA = rnorm(n_plant, mean = 50 + trayA, 
             sd = 2)  
hybB = rnorm(n_plant, mean = 50 + trayB ,
             sd = 2) 
dat1 = data.frame(
  hybrid = rep(c("H1", "H2"), each = n_plant),
  tray = rep(c("A", "B"), each = n_plant), 
  value = c(hybA, hybB)
)
t.test(value ~ hybrid, data = dat1)

set.seed(53)
n_tray = 10

hybA = rnorm(n_tray, mean = 50, sd = 2)
hybB = rnorm(n_tray, mean = 50, sd = 2)
dat2 = data.frame(
  hybrid = rep(c("H1", "H2"), each = n_tray),
  value = c(hybA, hybB)
)

t.test(value ~ hybrid, data = dat2)

library(ggpubr)
ggarrange(
  ggplot(data = dat1, aes(x = hybrid, y = value)) +
    geom_boxplot(aes(color = tray), linewidth = 1) +
    geom_jitter(
      width = .3,
      alpha = .5,
      aes(color = tray),
      size = 2
    ) +
    theme_minimal() +
    theme(legend.position = "top", text = element_text(size = 18)) +
    labs(x = "Hybrid", y = "Root length (cm)") +
    scale_color_manual("Tray", values = c('#10342d', '#b2bc63')),
  ggplot(data = dat2, aes(x = hybrid, y = value)) +
    geom_boxplot(linewidth = 1) +
    geom_jitter(
      width = .3,
      alpha = .5,
      size = 2
    ) +
    theme_minimal() +
    theme(
      legend.position = "top",
      legend.title = element_blank(),
      text = element_text(size = 18)
    ) +
    labs(x = "Hybrid", y = "Root length (cm)"),
  common.legend = TRUE
)

set.seed(11)
rows = 10
cols = 10
n_plot = rows * cols

grid = expand.grid(row = 1:rows, col = 1:cols) |> 
  mutate(
    gradient = rev(col) * 2.5,
    random_error = rnorm(n_plot, mean = 0, sd = 2),
    y = gradient + random_error
  )

ggplot(data = grid, aes(x = col, y = row, fill = y)) +
  geom_tile(color = "black") + 
  scale_fill_viridis_c(option = "magma") + 
  theme_minimal() + 
  scale_x_continuous("Column",breaks = grid$col) + 
  scale_y_continuous("Row",breaks = grid$row) + 
  theme(legend.position = 'top', legend.title = element_blank(), 
        text = element_text(size = 20), legend.text = element_text(size = 12))

systbias = cbind(geno_eff, col = 1:nlevels(dat$clone)) |>
  left_join(grid, by = "col") |> mutate(y = y + g, clone = as.factor(clone))

ggplot(data = systbias, aes(x = col, y = row, fill = y)) +
  geom_tile(color = "black") +
  geom_text(aes(label = clone)) +
  scale_fill_gradient2(midpoint = mean(systbias$y),
                       low = "#A51122",
                       mid = "#FEFDBE",
                       high = "#006228"
  ) +
  theme_minimal() +
  scale_x_continuous("Column", breaks = grid$col) +
  scale_y_continuous("Row", breaks = grid$row) +
  theme(
    legend.position = 'top',
    legend.title = element_blank(),
    text = element_text(size = 20),
    legend.text = element_text(size = 12)
  )

mod1 = lm(y~clone, data = systbias)
agricolae::HSD.test(y = mod1, trt = "clone")$groups

set.seed(77)

selbias = rbind(
  cbind(geno_eff[which(geno_eff$clone %in% c("C03", "C07")),],
        col = c(1, 2)),
  cbind(geno_eff[which(!geno_eff$clone %in% c("C03", "C07")),],
        col = sample(seq(3,cols,1)))
) |> right_join(grid) |> 
  mutate(y = y + g, clone = as.factor(clone))

ggplot(data = selbias, aes(x = col, y = row, fill = y)) +
  geom_tile(color = "black") +
  geom_text(aes(label = clone)) +
  scale_fill_gradient2(midpoint = mean(systbias$y),
                       low = "#A51122",
                       mid = "#FEFDBE",
                       high = "#006228"
  ) +
  theme_minimal() +
  scale_x_continuous("Column", breaks = grid$col) +
  scale_y_continuous("Row", breaks = grid$row) +
  theme(
    legend.position = 'top',
    legend.title = element_blank(),
    text = element_text(size = 20),
    legend.text = element_text(size = 12)
  )

mod2 = lm(y~clone, data = selbias)
agricolae::HSD.test(y = mod2, trt = "clone")$groups

accbias = selbias
accbias$y = ifelse(accbias$col %in% c(1,2), accbias$y - 25, accbias$y)

ggplot(data = accbias, aes(x = col, y = row, fill = y)) +
  geom_tile(color = "black") +
  geom_text(aes(label = clone)) +
  scale_fill_gradient2(midpoint = mean(systbias$y),
                       low = "#A51122",
                       mid = "#FEFDBE",
                       high = "#006228"
  ) +
  theme_minimal() +
  scale_x_continuous("Column", breaks = grid$col) +
  scale_y_continuous("Row", breaks = grid$row) +
  theme(
    legend.position = 'top',
    legend.title = element_blank(),
    text = element_text(size = 20),
    legend.text = element_text(size = 12)
  )

mod3 = lm(y~clone, data = accbias)
agricolae::HSD.test(y = mod3, trt = "clone")$groups

set.seed(11)
nobias = grid |> mutate(
  clone = sample(rep(levels(dat$clone), n_plot/10))
) |> left_join(geno_eff) |> 
  mutate(y = y + g, clone = as.factor(clone))

ggplot(data = nobias, aes(x = col, y = row, fill = y)) +
  geom_tile(color = "black") +
  geom_text(aes(label = clone)) +
  scale_fill_gradient2(midpoint = mean(systbias$y),
                       low = "#A51122",
                       mid = "#FEFDBE",
                       high = "#006228"
  ) +
  theme_minimal() +
  scale_x_continuous("Column", breaks = grid$col) +
  scale_y_continuous("Row", breaks = grid$row) +
  theme(
    legend.position = 'top',
    legend.title = element_blank(),
    text = element_text(size = 20),
    legend.text = element_text(size = 12)
  )

mod4 = lm(y~clone, data = nobias)
agricolae::HSD.test(y = mod4, trt = "clone")$groups

set.seed(11)
rows = 5
cols = 10
n_plot = rows * cols
n_block = 5

grid = expand.grid(row = 1:rows, col = 1:cols) |> 
  mutate(
    gradient = rev(col) * 2.5,
    random_error = rnorm(n_plot, mean = 0, sd = 2),
    y = gradient + random_error
  )
grid$block = rep(paste0("B", 1:n_block), each = 10)

ggplot(data = grid, aes(x = col, y = row, fill = y)) +
  geom_tile(color = "black") + 
  scale_fill_viridis_c(option = "magma") + 
  theme_minimal() + 
  scale_x_continuous("Column",breaks = grid$col) + 
  scale_y_continuous("Row",breaks = grid$row) + 
  theme(legend.position = 'top', legend.title = element_blank(), 
        text = element_text(size = 20), legend.text = element_text(size = 12)) +
  facet_wrap(.~block, scales = "free_x", nrow = 1)

nobias = do.call(rbind, lapply(split(grid, grid$block), function(x) {
  x |> mutate(clone = sample(rep(levels(dat$clone), n_plot / n_block / 10))) |> left_join(geno_eff, by = "clone") |>
    mutate(y = y + g, clone = as.factor(clone))
}))

ggplot(data = nobias, aes(x = col, y = row, fill = y)) +
  geom_tile(color = "black") +
  geom_text(aes(label = clone)) +
  scale_fill_gradient2(midpoint = mean(systbias$y),
                       low = "#A51122",
                       mid = "#FEFDBE",
                       high = "#006228"
  ) +
  theme_minimal() +
  scale_x_continuous("Column", breaks = grid$col) +
  scale_y_continuous("Row", breaks = grid$row) +
  theme(
    legend.position = 'top',
    legend.title = element_blank(),
    text = element_text(size = 20),
    legend.text = element_text(size = 12)
  ) +
  facet_wrap(.~block, scales = "free_x", nrow = 1)

anova(lm(y ~ clone , data = nobias))

anova(lm(y ~ clone , data = nobias))

set.seed(53)
tray_effect = rnorm(n_tray, mean = 0, sd = 2)
hybA_eff = rnorm(50*n_tray, mean = 50, sd = 2)
hybB_eff = rnorm(50*n_tray, mean = 50, sd = 2)

dat3 = data.frame(
  hybrid = rep(c("H1", "H2"), 
               times = n_plant*n_tray),
  tray = rep(LETTERS[1:n_tray], 
             each = n_plant*2), 
  value = c(hybA_eff, hybB_eff)
)

t.test(value ~ hybrid, data = dat3)

ggplot(data = dat3, aes(x = hybrid, y = value)) +
  facet_wrap(.~tray, nrow = 2) +
  geom_boxplot(aes(color = tray), linewidth = 1) +
  geom_jitter(width = .3,
              alpha = .5,
              aes(color = tray),
              size = 2) +
  theme_bw() +
  theme(legend.position = "none", text = element_text(size = 18)) +
  labs(x = "Hybrid", y = "Root length (cm)") + 
  scale_color_viridis_d(option = "turbo")

