# Passo 1: modelo diagonal (sem covariância entre traits)
m_diag <- asreml(
  fixed  = cbind(GY_sc, PH_R8_sc) ~ trait + trait:env +
    trait:check +
    trait:bloco:env,
  random = ~ gen:diag(trait) +
    fa(env, 1):gen:diag(trait),
  rcov   = ~ units:diag(trait),
  data   = data,
  na.action = na.method(x = "include", y = "include"),
  maxit  = 300
)
pred <- predict(m1_GY, classify = "gen:env", sed = TRUE)$pvals
pred <- predict(m0_PH, classify = "gen:env", sed = TRUE)$pvals
write.csv(pred, "BLUPGE_PH.csv")

blup_ge <- read.csv("blup_env_GY.csv")

blup_ge <- blup_ge %>% 
  rename(GEN = gen, ENV = env, BLUP = predicted.value)

# matriz ambiente × genótipo
mat_env <- blup_ge %>%
  select(ENV, GEN, BLUP) %>%
  pivot_wider(names_from = GEN, values_from = BLUP)

# colocar ENV como rownames
rownames(mat_env) <- mat_env$ENV
mat_env$ENV <- NULL

mat_env <- mat_env[rowSums(is.na(mat_env)) < ncol(mat_env), ]

mat_env <- mat_env[, apply(mat_env, 2, var, na.rm = TRUE) > 0]

for (j in 1:ncol(mat_env)) {
  na_idx <- is.na(mat_env[, j])
  if (any(na_idx)) {
    mat_env[na_idx, j] <- mean(mat_env[, j], na.rm = TRUE)
  }
}

scale_safe <- function(x){
  sd_x <- sd(x, na.rm = TRUE)
  if (sd_x == 0) return(x - mean(x, na.rm = TRUE))
  (x - mean(x, na.rm = TRUE)) / sd_x
}

mat_env_scaled <- apply(mat_env, 2, scale_safe)

dist_env <- dist(mat_env_scaled, method = "euclidean")

any(is.na(dist_env))
any(is.infinite(dist_env))
any(is.nan(dist_env))
hc_env <- hclust(dist_env, method = "ward.D2")

plot(hc_env, main = "Clusterização de Ambientes (Ward.D2)", 
     xlab = "Ambientes", sub = "")

grupos <- cutree(hc_env, k = 3)
grupos

cluster_env <- data.frame(
  Ambiente = rownames(mat_env),
  Cluster  = grupos
)
write.csv(cluster_env, "clusterswardd2.csv")


# Componentes da variância
var_res <- summary(m1_GY)$varcomp["units!R","component"]

mean_GY <- mean(data$GY, na.rm = TRUE)

CV_res <- 100 * sqrt(var_res) / mean_GY
CV_res
summary(m1_GY)$varcomp

vc <- summary(m1_GY)$varcomp

# linhas da interação (FA)
ge_rows <- grep("fa\\(env, 1\\):gen", rownames(vc), value = TRUE)

# variância da interação (somatória)
var_ge <- sum(vc[ge_rows, "component"], na.rm = TRUE)

var_ge
PG <- var_g / (var_g + var_ge + var_res)
PG*100
PG_GE <- (var_g + var_ge) / (var_g + var_ge + var_res)
PG_GE

# variância explicada pelos fixos
vf <- var(predict(m1_GY, classify="env")$pvals$predicted.value)

# R2 marg (fixos)
R2_m <- vf / (vf + var_g + var_ge + var_res)
R2_m


GE_mat <- reshape(
  blup_ge[, c("GEN", "ENV", "BLUP")],
  idvar = "GEN",
  timevar = "ENV",
  direction = "wide"
)
library(reshape2)

# remover coluna "gen"
rownames(GE_mat) <- GE_mat$gen
GE_mat <- GE_mat[ , -1 ]

cor_env <- cor(GE_mat, use = "pairwise.complete.obs")
cor_env

GE_num <- as.data.frame(GE_mat)
GE_num[, -1] <- lapply(GE_num[, -1], as.numeric)
cor_env <- cor(GE_num[, -1], use = "pairwise.complete.obs")


library(dplyr)
library(tidyr)

cor_df <- as.data.frame(cor_env)

# Remover diagonal
cor_df$env1 <- rownames(cor_df)
cor_long <- cor_df %>%
  pivot_longer(-env1, names_to = "env2", values_to = "cor") %>%
  filter(env1 != env2)
max_cor <- cor_long %>% 
  arrange(desc(cor)) %>% 
  slice(1)

min_cor <- cor_long %>% 
  arrange(cor) %>% 
  slice(1)
max_cor
min_cor


library(pheatmap)

library(ggplot2)
library(reshape2)

# Transformar para formato longo
cor_long <- melt(cor_env)
colnames(cor_long) <- c("Env1", "Env2", "Correlation")

ggplot(cor_long, aes(Env1, Env2, fill = Correlation)) +
  geom_tile() +
  scale_fill_gradient2(
    low = "red", mid = "white", high = "blue",
    midpoint = 0, limits = c(min(cor_long$Correlation), max(cor_long$Correlation))
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    text = element_text(size = 12)
  ) +
  labs(
    title = "Heatmap of Genotypic Correlations Among Environments",
    x = "Environment",
    y = "Environment"
  )

library(dplyr)
library(tidyr)

# Garantir que cor_env é data.frame
cor_df <- as.data.frame(cor_env)

# Adicionar coluna com nome das linhas
cor_df$env1 <- rownames(cor_df)

# Converter para formato longo
cor_long <- cor_df %>%
  pivot_longer(
    cols = -env1,
    names_to = "env2",
    values_to = "cor"
  ) %>%
  filter(env1 != env2)
env_contrast <- cor_long %>%
  group_by(env1) %>%
  summarise(mean_cor = mean(cor, na.rm = TRUE)) %>%
  arrange(mean_cor)
head(env_contrast)
tail(env_contrast)
