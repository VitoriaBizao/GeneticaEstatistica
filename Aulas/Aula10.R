library(tidyverse)
library(ggpubr)

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

ggplot(data = grid, aes(x = col, y = row, fill = y)) +
  geom_tile(color = "black") + 
  scale_fill_viridis_c(option = "magma") + 
  theme_minimal() + 
  scale_x_continuous("Column",breaks = grid$col) + 
  scale_y_continuous("Row",breaks = grid$row) + 
  theme(legend.position = 'none', legend.title = element_blank(), 
        text = element_text(size = 20), legend.text = element_text(size = 12))

set.seed(77)
treats = paste0("P", sprintf(paste0('%0', 2,'d'), seq(1:50)))
blocks = 5
expansion = split(rep(treats, each = blocks), 1:blocks)
randomization = lapply(expansion, sample)
randomization[[1]]

numrow = 10
numcol = 25
grid = expand.grid(row = 1:numrow, col = 1:numcol)
grid$blocks = rep(1:blocks, each = length(treats))
grid$geno = NA
for (i in 1:blocks) grid[grid$blocks == i,'geno'] = randomization[[i]]

ggplot(data = grid, aes(x = col, y = row, labels = geno)) + 
  geom_tile(aes(fill = factor(blocks)), color = "black") + 
  geom_text() + 
  theme_minimal() +
  theme(legend.position = "top", text = element_text(size = 20)) + 
  labs(x = "Column", y = "Row", fill = "Block") +
  scale_y_continuous(breaks = seq(1, numrow, 1))+
  scale_x_continuous(breaks = seq(1, numcol, 1)) + 
  ggsci::scale_fill_locuszoom()


library(agricolae)
agr_rcbd = design.rcbd(trt = treats, r = blocks, seed = 55, serie = 2, 
                       first = TRUE, continue = FALSE, randomization = TRUE)
## agr_rcbd$book
## zigzag(agr_rcbd)
agr_rcbd$sketch

library(FielDHub)
fd_rcbd = RCBD(t = treats, reps = blocks, l = 1, planter = "serpentine",
               seed = 55, locationNames = "ESALQ")
## fd_rcbd$fieldBook
## fd_rcbd$infoDesign
## fd_rcbd$layoutRandom
## fd_rcbd$plotNumber
plot(fd_rcbd)

sigma2g = 5
sigma2p = 3
sigma2e = 10
mu = 50
set.seed(777)

gen_eff = data.frame(geno = treats,
                     gen_eff = rnorm(
                       n = length(treats),
                       mean = 0,
                       sd = sqrt(sigma2g)
                     ))
block_eff = data.frame(blocks = 1:blocks,
                       block_eff = rnorm(
                         n = blocks,
                         mean = 0,
                         sd = sqrt(sigma2p)
                       ))
noise = rnorm(nrow(grid), mean = 0, sd = sqrt(sigma2e))

grid$noise = noise
grid = merge(grid, gen_eff, by = "geno")
grid = merge(grid, block_eff, by = "blocks")
grid$y = grid$noise + grid$gen_eff + grid$block_eff + mu
grid = transform(grid, blocks = as.factor(blocks), geno = as.factor(geno))

library(asreml)
mod1 = asreml(fixed = y ~ 1, random = ~ geno, data = grid)
mod2 = asreml(fixed = y ~ blocks, random = ~ geno, data = grid)
mod3 = asreml(fixed = y ~ 1, random = ~ geno+blocks, data = grid)

plot(mod1)
plot(mod2)
plot(mod3)

summary(mod1)$varcomp
summary(mod2)$varcomp
summary(mod3)$varcomp

vpredict(mod1, H2 ~ V1/(V1+V2))
vpredict(mod2, H2 ~ V1/(V1+V2))
vpredict(mod3, H2 ~ V2/(V2+V3))

wald(mod1)
wald(mod2)

redmod1 = asreml(fixed = y ~ 1, data = grid)
redmod2 = asreml(fixed = y ~ blocks, data = grid)
redmod3 = asreml(fixed = y ~ 1, random = ~geno, data = grid)
redmod4 = asreml(fixed = y ~ 1, random = ~blocks, data = grid)

lrt.asreml(mod1, redmod1, boundary = TRUE)
lrt.asreml(mod2, redmod2, boundary = TRUE)

pred1 = predict.asreml(object = mod1, classify = "geno")
pred2 = predict.asreml(object = mod2, classify = "geno")
pred3 = predict.asreml(object = mod3, classify = "geno")

aux = cbind(pred1$pvals[,1:2], pred2$pvals[,2], pred3$pvals[,2], gen_eff[,2])
colnames(aux) = c("geno", "mod1", "mod2", "mod3", "real")

GGally::ggpairs(data = aux[,-1]) + theme(text = element_text(size = 20))

ggplot(data = pred2$pvals, aes(x = reorder(geno, -predicted.value), y = predicted.value)) +
  geom_col(color = "white", fill = "#10342d") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1),
        text = element_text(size = 18)) +
  geom_errorbar(aes(ymin = predicted.value - std.error, ymax = predicted.value + std.error)) +
  labs(x = "Family", y = "BLUP") 

set.seed(123)
dat2 = grid
dat2$y[sample(length(dat2$y), 100)] = NA
table(na.exclude(dat2)$geno)

mod1 = asreml(fixed = y ~ 1, random = ~ geno, data = dat2)
mod2 = asreml(fixed = y ~ blocks, random = ~ geno, data = dat2)
mod3 = asreml(fixed = y ~ 1, random = ~ geno+blocks, data = dat2)

summary(mod1)$varcomp
summary(mod2)$varcomp
summary(mod3)$varcomp

vpredict(mod1, H2 ~ V1/(V1+V2))
vpredict(mod2, H2 ~ V1/(V1+V2))
vpredict(mod3, H2 ~ V2/(V2+V3))

pred1 = predict.asreml(object = mod1, classify = "geno")
pred2 = predict.asreml(object = mod2, classify = "geno")
pred3 = predict.asreml(object = mod3, classify = "geno")

aux = data.frame(
  model = rep(c("mod1", "mod2", "mod3"), each = nlevels(dat2$geno)),
  reliability = c(pred1$pvals$std.error^2/summary(mod1)$varcomp[1,1],
                  pred2$pvals$std.error^2/summary(mod2)$varcomp[1,1],
                  pred3$pvals$std.error^2/summary(mod3)$varcomp[2,1])
)

ggplot(data = aux, aes(x = model, y = reliability)) + 
  geom_boxplot(aes(fill = model)) + 
  theme_minimal() + 
  theme(text=element_text(size= 20), legend.position = 'none') + 
  ggsci::scale_fill_locuszoom()  + 
  labs(x = "Model", y = "Reliability")

aux = cbind(pred1$pvals[,1:2], pred2$pvals[,2], pred3$pvals[,2], gen_eff[,2])
colnames(aux) = c("geno", "mod1", "mod2", "mod3", "real")

GGally::ggpairs(data = aux[,-1]) + theme(text = element_text(size = 20))

ggplot(data = pred3$pvals, aes(x = reorder(geno, -predicted.value), y = predicted.value)) +
  geom_col(color = "white", fill = "#10342d") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1),
        text = element_text(size = 18)) +
  geom_errorbar(aes(ymin = predicted.value - std.error, ymax = predicted.value + std.error)) +
  labs(x = "Family", y = "BLUP") 

