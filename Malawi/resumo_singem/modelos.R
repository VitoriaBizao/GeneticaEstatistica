library(tidyverse)
library(asreml)
getwd()
setwd("C:/Users/ph408/OneDrive/Documentos/esalq/GEVitoria/Malawi/resumo_singem")
data  <- read.csv2("dataoutr.csv")
 

m1_GY <- asreml(
  fixed   = GY ~ env + check + bloco:env,
  random  = ~ gen + 
    fa(env,1):gen,
  rcov    = ~ at(env):units,
  data    = data,
  na.action = na.method(x = "include", y = "include"),
  maxit = 300)

qqnorm(residuals(m1_GY)); qqline(residuals(m1_GY))
m1_GY <- update(m1_GY)
asreml.options(maxit = 100, workspace = '1gb', pworkspace = '2gb')
pred_ge <- predict(m0_GY,
                   classify = "gen:env",
                   sed = TRUE)$pvals

#remover outliers arquivo teste modelos

m1_PH <- asreml(
  fixed   = PH_R8 ~ env + check + bloco:env,
  random  = ~ gen + 
    fa(env,1):gen,
  rcov    = ~ at(env):units,
  data    = data,
  na.action = na.method(x = "include", y = "include"),
  maxit = 300)
qqnorm(residuals(m1_PH)); qqline(residuals(m1_PH))

summary(m0_GY)$aic
plot(residuals(M1))
qqnorm(residuals(m0_PH)); qqline(residuals(m0_PH))

blup_gxe <- predict(m1_GY, classify = "gen:env", sed = TRUE)$pvals
write.csv(blup_gen, "blupsPH_R8_gen")



# herdabilidade -----------------------------------------------------------
#GY
varcomp <- summary(m1_GY)$varcomp
var_g  <- varcomp["gen","component"]
pred <- read.csv("BLUPs_gen.csv")
pred$PEV <- pred$std.error^2
H2_Cullis <- 1 - mean(pred$PEV) / var_g

saveRDS(H2_Cullis, "h2cullis.rds")

#PH_R*
varcomp <- summary(m0_PH)$varcomp
var_g  <- varcomp["gen","component"]
pred <- read.csv("blupsPH_R8_gen.csv")
pred$PEV <- pred$std.error^2
H2_Cullis <- 1 - mean(pred$PEV) / var_g
saveRDS(H2_Cullis, "h2cullis_PHR8")
h2gy <- readRDS("h2cullis_GY.rds")


# h2 GxE ------------------------------------------------------------------
pred <-  read.csv('BLUPs_GY_gen.csv')
sed_matrix <- pred$sed 

# waasb -------------------------------------------------------------------


pred_GE <- predict(
  m1_GY,
  classify = "gen:env"
)$pvals

head(pred_GE)



# 1. Montar a matriz GxE a partir dos predicted values do ASReml
ge_matrix <- pred_GE %>%
  # 'predicted.value' é a coluna padrão do asreml::predict
  select(gen, env, predicted.value) %>%
  pivot_wider(names_from = env, values_from = predicted.value) %>%
  column_to_rownames("gen") %>%
  as.matrix()

# 2. Centralizar por ambiente (desvio em relação à média do ambiente)
ge_centered <- scale(ge_matrix, center = TRUE, scale = FALSE)

# 3. SVD da matriz GxE centralizada
svd_ge <- svd(ge_centered)

# número de ambientes
nenv <- ncol(ge_matrix)

# 4. Calcular WAASB para cada genótipo
# Escores absolutos de cada PC, ponderados pela variância explicada
singular_values <- svd_ge$d
varexp <- singular_values^2 / sum(singular_values^2)  # proporção de variância

# Escores dos genótipos (matriz U * D)
scores_gen <- svd_ge$u %*% diag(singular_values)  # dim: ngen x nenv

# WAASB = soma ponderada dos |escores| por PC
waasb_values <- apply(abs(scores_gen), 1, function(x) {
  sum(x * varexp) / sum(varexp)
})

# 5. Montar resultado
waasb_result <- tibble(
  gen = rownames(ge_matrix),
  waasb = waasb_values
) %>%
  arrange(waasb)

print(waasb_result)

# biplot ------------------------------------------------------------------

library(tidyverse)

# ── 1. Matriz GxE ──────────────────────────────────────────────────────────────
ge_matrix <- pred_GE %>%
  select(gen, env, predicted.value) %>%
  pivot_wider(names_from = env, values_from = predicted.value) %>%
  column_to_rownames("gen") %>%
  as.matrix()

# ── 2. SVD da matriz centralizada ──────────────────────────────────────────────
ge_centered <- scale(ge_matrix, center = TRUE, scale = FALSE)
svd_ge      <- svd(ge_centered)

nenv          <- ncol(ge_matrix)
singular_vals <- svd_ge$d
varexp        <- singular_vals^2 / sum(singular_vals^2)
scores_gen    <- svd_ge$u %*% diag(singular_vals)

# ── 3. WAASB ───────────────────────────────────────────────────────────────────
waasb_values <- apply(abs(scores_gen), 1, function(x) {
  sum(x * varexp) / sum(varexp)
})

mean_gen <- rowMeans(ge_matrix)

waasb_df <- tibble(
  gen      = rownames(ge_matrix),
  waasb    = waasb_values,
  mean_gy  = mean_gen
) %>%
  mutate(
    # Escala 0-100 para o índice composto (WAASBY)
    waasb_sc  = (waasb   - min(waasb))   / (max(waasb)   - min(waasb)) * 100,
    mean_sc   = (mean_gy - min(mean_gy)) / (max(mean_gy) - min(mean_gy)) * 100,
    waasby    = (waasb_sc + (100 - mean_sc)) / 2   # menor = melhor
  )

# ── 4. Biplot ──────────────────────────────────────────────────────────────────

library(tidyverse)

# ── 1. Matriz GxE ──────────────────────────────────────────────────────────────
ge_matrix <- pred_GE %>%
  select(gen, env, predicted.value) %>%
  pivot_wider(names_from = env, values_from = predicted.value) %>%
  column_to_rownames("gen") %>%
  as.matrix()

# ── 2. SVD da matriz centralizada ──────────────────────────────────────────────
ge_centered <- scale(ge_matrix, center = TRUE, scale = FALSE)
svd_ge      <- svd(ge_centered)

nenv          <- ncol(ge_matrix)
singular_vals <- svd_ge$d
varexp        <- singular_vals^2 / sum(singular_vals^2)
scores_gen    <- svd_ge$u %*% diag(singular_vals)

# ── 3. WAASB ───────────────────────────────────────────────────────────────────
waasb_values <- apply(abs(scores_gen), 1, function(x) {
  sum(x * varexp) / sum(varexp)
})

mean_gen <- rowMeans(ge_matrix)

waasb_df <- tibble(
  gen      = rownames(ge_matrix),
  waasb    = waasb_values,
  mean_gy  = mean_gen
) %>%
  mutate(
    # Escala 0-100 para o índice composto (WAASBY)
    waasb_sc  = (waasb   - min(waasb))   / (max(waasb)   - min(waasb)) * 100,
    mean_sc   = (mean_gy - min(mean_gy)) / (max(mean_gy) - min(mean_gy)) * 100,
    waasby    = (waasb_sc + (100 - mean_sc)) / 2   # menor = melhor
  )

# ── 4. Biplot ──────────────────────────────────────────────────────────────────
# Médias e WAASB médios (linhas de corte)
mean_waasb  <- mean(waasb_df$waasb)
mean_gy_all <- mean(waasb_df$mean_gy)


# Quadrantes:
# I   (dir-cima):  alta produção + baixa estabilidade
# II  (dir-baixo): alta produção + alta estabilidade  ← IDEAL
# III (esq-baixo): baixa produção + alta estabilidade
# IV  (esq-cima):  baixa produção + baixa estabilidade

p <- ggplot(waasb_df, aes(x = mean_gy, y = waasb)) +
  # Linhas de corte
  geom_vline(xintercept = mean_gy_all, linetype = "dashed",
             color = "gray50", linewidth = 0.5) +
  geom_hline(yintercept = mean_waasb,  linetype = "dashed",
             color = "gray50", linewidth = 0.5) +
  # Pontos coloridos pelo waasby
  geom_point(aes(fill = waasby), shape = 21, size = 3.5,
             color = "white", stroke = 0.4) +
  scale_fill_gradient2(
    low      = "#1B7837",   # verde = ideal
    mid      = "#F7F7F7",
    high     = "#762A83",   # roxo = ruim
    midpoint = median(waasb_df$waasby),
    name     = "WAASBY\n(menor = melhor)"
  ) +
  # Rótulos dos genótipos
  ggrepel::geom_text_repel(
    aes(label = gen), size = 2.8, max.overlaps = 30,
    segment.color = "gray70", segment.size = 0.3
  ) +
  # Anotações dos quadrantes
  annotate("text", x = Inf,  y = Inf,   label = "I",   hjust = 1.3, vjust = 1.5,
           size = 5, color = "gray60", fontface = "bold") +
  annotate("text", x = Inf,  y = -Inf,  label = "II ★", hjust = 1.2, vjust = -0.5,
           size = 5, color = "#1B7837", fontface = "bold") +
  annotate("text", x = -Inf, y = -Inf,  label = "III",  hjust = -0.3, vjust = -0.5,
           size = 5, color = "gray60", fontface = "bold") +
  annotate("text", x = -Inf, y = Inf,   label = "IV",   hjust = -0.3, vjust = 1.5,
           size = 5, color = "gray60", fontface = "bold") +
  labs(
    title    = "WAASB Biplot",
    subtitle = "Weighted Average of Absolute Scores (ASReml FA1 BLUPs)",
    x        = "Média de GY (BLUP)",
    y        = "WAASB"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold"),
    plot.subtitle = element_text(color = "gray40"),
    legend.position = "right",
    panel.grid    = element_blank()
  )

print(p)

# Salvar
ggsave("waasb_biplot.png", p, width = 8, height = 7, dpi = 300)

readRDS("h2cullis_PHR8.rds")
readRDS("h2cullis_GY.rds")

