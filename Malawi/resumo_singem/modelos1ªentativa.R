library(tidyverse)
library(asreml)
getwd()
setwd("C:/Users/ph408/OneDrive/Documentos/esalq/GEVitoria/Malawi/resumo_singem")
data <- readRDS("asfactor.rds")
boxplot(GY ~ env, data = data)
boxplot(NDM ~ env, data = data)
boxplot(PH_R8 ~ env, data = data)
head(data$)



# GY ----------------------------------------------------------------------


M0 <- asreml(
  
  fixed = GY ~ env,
  
  random = ~ gen + gen:env,
  
  residual = ~ dsum(~id(units)|env),
  
  data = data,
  
  maxit = 200
)

plot(residuals(M0))
qqnorm(residuals(M0)); qqline(residuals(M0))
rm(M0)

M1 <- asreml(
  fixed = GY ~ env,
  
  random = ~
    gen:fa(env,1) +
    bloco:env,
  
  residual = ~ dsum(~id(units) | env),
  
  data = data,
  
  maxit = 300

)

plot(residuals(M1))
qqnorm(residuals(M1)); qqline(residuals(M1))


data$GY_log <- log(data$GY)


M4 <- asreml(
  fixed  = GY ~ env,,
  random = ~ gen + fa(env,2):gen,
  rcov   = ~ units,
  data   = data
)

plot(residuals(M4))
qqnorm(residuals(M4)); qqline(residuals(M4))

M3 = asreml(
  fixed = GY ~ rep:env + env,
  random = ~ gen:fa(env, 3),
  data = data,
  residual = ~ dsum(~id(units) | env),
  

plot(residuals(M3))
qqnorm(residuals(M3)); qqline(residuals(M3))

# PHR8---------------------------------------------------------------------

phm1 <-  asreml(
  fixed  = PH_R8 ~ env,,
  random = ~ gen + fa(env,2):gen,
  rcov   = ~ units,
  data   = data
)

plot(residuals(phm1))
qqnorm(residuals(phm1)); qqline(residuals(phm1))

# ndm ---------------------------------------------------------------------

ndmm0 <-  asreml(
  fixed  = PH_R8 ~ env,,
  random = ~ gen + fa(env,2):gen,
  rcov   = ~ units,
  data   = datax''
)

plot(residuals(phm1))
qqnorm(residuals(phm1)); qqline(residuals(phm1))
# blups -------------------------------------------------------------------


blups_gen <- predict(M1, classify = "gen")$pvals
blups_adj <- predict(M1, classify = "gen", sed = TRUE)$pvals
blup_ge <- predict(M1, classify = "gen:env")$pvals
head(blups_gen)
head(blup_ge)
head(blups_adj)
write.csv(blups_gen, "BLUPs_gen.csv", row.names = FALSE)

# WAASB -------------------------------------------------------------------

var_g <- summary(M1)$varcomp["gen","component"]
pred_gen <- predict(M1, classify="gen", sed=TRUE)$pvals
pred_gen$PEV <- pred_gen$std.error^2
pred_gen$accuracy <- sqrt(1 - pred_gen$PEV / var_g)

GE_mat <- reshape(
  blup_ge[, c("gen", "env", "predicted.value")],
  idvar = "gen", timevar = "env", direction = "wide"
)

rownames(GE_mat) <- GE_mat$gen
GE_mat$gen <- NULL

GE_centered <- scale(GE_mat, center = TRUE, scale = FALSE)

svd_res <- svd(GE_centered)

var_exp <- (svd_res$d^2) / sum(svd_res$d^2)

scores <- svd_res$u %*% diag(svd_res$d)

WAASB <- rowSums( abs(scores) * matrix(var_exp, 
                                       nrow = nrow(scores), 
                                       ncol = length(var_exp), 
                                       byrow = TRUE) )

WAASB_df <- data.frame(
  gen = rownames(GE_mat),
  WAASB = WAASB
)

WAASB_df <- WAASB_df[order(WAASB_df$WAASB), ]  # menor = mais estável

# BLUPs principais (gen)
blup_gen <- predict(M4, classify = "gen")$pvals

media_blup <- aggregate(predicted.value ~ gen, blups_gen, mean)
colnames(media_blup)[2] <- "BLUP_mean"
WAASB_df2 <- merge(WAASB_df, media_blup, by = "gen")



library(ggplot2)
library(ggrepel)

ggplot(WAASB_df2, aes(x = WAASB, y = BLUP_mean, label = gen)) +
  geom_point(size = 3, color = "blue") +
  geom_text_repel(max.overlaps = Inf, size = 4) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Estabilidade (WAASB) × Produtividade (BLUP)",
    x = "WAASB (menor = mais estável)",
    y = "Média dos BLUPs (produtividade)"
  )


kmean <- kmeans(WAASB_df2[, c("WAASB", "BLUP_mean")], centers = 4)
WAASB_df2$cluster <- factor(kmean$cluster)

ggplot(WAASB_df2, aes(WAASB, BLUP_mean, color = cluster, label = gen)) +
  labs(title = "WAASB GY",
       y = 'BLUP mean') +
  geom_point(size = 3) +
  geom_text_repel(size = 3) +
  theme_minimal()

