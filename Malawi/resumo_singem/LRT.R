
# preparação --------------------------------------------------------------
library(dplyr)
library(asreml)
getwd()
setwd("C:/Users/ph408/OneDrive/Documentos/esalq/GEVitoria/data/resumo_singem")
data <- read.csv("data_sem_outliers.csv")
source("https://raw.githubusercontent.com/saulo-chaves/May_b_useful/refs/heads/main/fa_outs.R")

data <- data %>%    mutate(
  env = as.factor(env),
  gen = as.factor(gen),
  bloco = as.factor(rep),
  check = as.factor(check),
  SEASON= as.factor(SEASON),
  YEAR= as.factor(YEAR),
  loc = as.factor(loc),
  RAINFED = as.factor(RAINFED),
  
  GY = as.numeric(GY),
  NDM = as.numeric(NDM),
  PROT = as.numeric(PROT),
  
)

num.env <- nlevels(data$env)
num.gen <- nlevels(data$gen)
num.bloco <- nlevels(data$bloco)
num.SEASON <- nlevels(data$SEASON)
num.YEAR <- nlevels(data$YEAR)
num.loc <- nlevels(data$loc)
num.RAINFED <- nlevels(data$RAINFED)

num.GY <- length(data$GY)
num.NDM <- length(data$NDM)
num.PROT <- length(data$PROT)

library(tidyverse)
library(asreml)
data

# composição variancia ----------------------------------------------------
library(asreml)
asreml.options(workspace = '1gb', pworkspace = '2gb')

mod_id <- asreml(
  fixed  = GY ~ env + bloco:env + check,
  random = ~ gen + gen:env,     # variância comum para cada gen:env
  rcov   = ~ units,
  data   = data
)

mod_che <- asreml(
  fixed  = GY ~ env + bloco:env,
  random = ~ gen + gen:env,     # variância comum para cada gen:env
  rcov   = ~ units,
  data   = data
)


mod_diag <- asreml(
  fixed  = GY ~ env,,
  random = ~ gen + diag(env):gen,   # uma variância específica por ambiente
  rcov   = ~ units,
  data   = data,
  maxit = 100,
qqnorm(residuals(mod_fa1)); qqline(residuals(mod_fa1))

mod_corgh <- asreml(
  fixed  = GY ~ env,,
  random = ~ gen + corgh(env):gen,  # matriz não estruturada
  rcov   = ~ units,
  data   = data
  maxit = 100,
  na.action = na.method(x="include", y = "include"))
)
mod_fa1 <- asreml(
  na.action = na.method(x="include", y = "include"))
)

  fixed  = GY ~ env + bloco:env + check,
  random = ~ gen + fa(env,1):gen,
  data   = data,
  maxit = 200,
  na.action = na.method(x="include", y = "include"))

mod_fa2 <- asreml(
  fixed  = GY ~ env + bloco:env + check,
  random = ~ gen + fa(env,2):gen,
  data   = data,
  rcov   = ~ at(env):units,
    maxit = 200,
  na.action = na.method(x="include", y = "include"))

mod_fa3 <- asreml(
  fixed  = GY ~ env + bloco: env + check,
  random = ~ gen + fa(env,3):gen,
  data   = data,
  maxit = 300,
  na.action = na.method(x="include", y = "include"))

mod_fa4 <- asreml(
  fixed  = GY ~ env + bloco:env + check,
  random = ~ gen + fa(env,4):gen,
  data   = data,
  maxit = 300,
  na.action = na.method(x="include", y = "include"))


lrt(mod_id,   mod_fa1)
lrt(mod_diag, mod_corgh)
lrt(mod_id,   mod_corgh)
lrt(mod_corgh, mod_fa1)
lrt(mod_fa3, mod_fa4)

res_std <- scale(residuals(mod_fa4))


mod_fa2, mod_fa3, mod_fa4
fa.models = list(mod_fa1)
fa.res = lapply(fa.models, function(x) fa.outs(x, name.env = "env",
                                               name.gen = "gen"))
diagnos = do.call(rbind,lapply(fa.res, function(x) x$diagnostics))
rownames(diagnos) = c("fa1", "fa2", "fa3", "fa4"); diagnos[,'ASVR']
write.csv(diagnos, "diagnose.csv")

rm(mod_fa1,mod_fa2,mod_fa3)

perc_explicada <- function(mod, k) {
  vc  <- summary(mod)$varcomp
  nms <- rownames(vc)
  psi <- vc[grepl("!var$", nms), "component"]
  
  fat <- lapply(1:k, function(i) {
    vc[grepl(paste0("!fa", i, "$"), nms), "component"]
  })
  
  var_comum <- Reduce("+", lapply(fat, function(f) f^2))
  var_total  <- var_comum + psi
  
  mean(var_comum / var_total) * 100
}

data.frame(
  modelo = paste0("FA", 1:4),
  xperc   = c(perc_explicada(mod_fa1, 1),
             perc_explicada(mod_fa2, 2),
             perc_explicada(mod_fa3, 3),
             perc_explicada(mod_fa4, 4))
)
             
# efeito de bloco ---------------------------------------------------------

#loop

results <- data.frame(site = character(), 
                      P_valor = numeric(), 
                      signn = character(), 
                      stringsAsFactors = FALSE)

for (i in levels(data$env)) {
  
  soydata <- subset(data, env == i)
  
  
  try({
    Mod_f <- asreml(GY ~ rep,
                    random = ~ gen,
                    data = soydata,
                    maxit = 100,
                    na.action = na.method(x="include", y = "include"))
    
    Mod_r <- asreml(GY ~ rep,
                    data = soydata,
                    maxit = 100,
                    na.action = na.method(x="include", y = "include"))
    
    
    p_valor <- lrt(Mod_f, Mod_r)[3]
    
    
    sig <- if (p_valor <= 0.001) {
      "***"
    } else if (p_valor <= 0.01) {
      "**"
    } else if (p_valor <= 0.05) {
      "*"
    } else {
      "ns"
    }
    
    
    results <- rbind(results, 
                     data.frame(site = i, 
                                P_valor = p_valor, 
                                signn = sig))
  }, silent = TRUE)
}

table(results$signn)
view(results)


#interação genótipo ambiente
mod_full <- asreml(
  fixed = GY ~ env,
  random = ~ gen + gen:env,
  data = data
)
mod_red <- asreml(
  fixed = GY ~ env,
  random = ~ gen,
  data = data
)

lrt(mod_full, mod_red)

# residual covariance -----------------------------------------------------

mod_idv <- asreml(
  fixed  = GY ~ env,,
  random = ~ gen + fa(env,1):gen,
  rcov   = ~ dsum(idv(units) | env),
  data   = data,
  na.action = na.method(x="include", y = "include"),
  maxit = 300)


mod_unit <- asreml(
  fixed  = GY ~ env,,
  random = ~ gen + fa(env,1):gen,
  rcov   =  ~ units,
  data   = data,
  na.action = na.method(x="include", y = "include"),
  maxit = 300)
lrt(mod_idv, mod_unit)

