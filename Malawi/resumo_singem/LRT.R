
# preparação --------------------------------------------------------------


library(tidyverse)
getwd()
setwd("C:/Users/ph408/OneDrive/Documentos/esalq/GEVitoria/Malawi/resumo_singem")
data <- readRDS("treateddata.rds")
malawi <- data %>%    mutate(
  env = as.factor(env),
  gen = as.factor(gen),
  bloco = as.factor(rep),
  SEASON= as.factor(SEASON),
  YEAR= as.factor(YEAR),
  loc = as.factor(loc),
  RAINFED = as.factor(RAINFED),
  
  GY = as.numeric(GY),
  NDM = as.numeric(NDM),
  PROT = as.numeric(PROT),
  
)
saveRDS(malawi, "asfactor.rds")

malawi <- readRDS('asfactor.rds')
num.env <- nlevels(malawi$env)
num.gen <- nlevels(malawi$gen)
num.bloco <- nlevels(malawi$bloco)
num.SEASON <- nlevels(malawi$SEASON)
num.YEAR <- nlevels(malawi$YEAR)
num.loc <- nlevels(malawi$loc)
num.RAINFED <- nlevels(malawi$RAINFED)

num.GY <- length(malawi$GY)
num.NDM <- length(malawi$NDM)
num.PROT <- length(malawi$PROT)

library(tidyverse)
library(asreml)
malawi
#teste

soydata<- subset(malawi, env == "E0137")
Mod_f <- asreml(GY ~ rep,
                random = ~ gen,
                data = soydata,
                maxit = 100,
                na.action = na.method(x="include", y = "include"))

Mod_r <- asreml(GY ~ rep,
                data = soydata,
                maxit = 100,
                na.action = na.method(x="include", y = "include"))

# composição variancia ----------------------------------------------------
library(asreml)

mod_id <- asreml(
  fixed  = GY ~ env,
  random = ~ gen + gen:env,     # variância comum para cada gen:env
  rcov   = ~ units,
  data   = malawi
)
rm(mod_id)


mod_diag <- asreml(
  fixed  = GY ~ env,,
  random = ~ gen + diag(env):gen,   # uma variância específica por ambiente
  rcov   = ~ units,
  data   = malawi
)



mod_corgh <- asreml(
  fixed  = GY ~ env,,
  random = ~ gen + corgh(env):gen,  # matriz não estruturada
  rcov   = ~ units,
  data   = malawi
)


mod_fa1 <- asreml(
  fixed  = GY ~ env,,
  random = ~ gen + fa(env,1):gen,
  rcov   = ~ units,
  data   = malawi
)

mod_fa2 <- asreml(
  fixed  = GY ~ env,,
  random = ~ gen + fa(env,2):gen,
  rcov   = ~ units,
  data   = malawi
)


lrt(mod_id,   mod_diag)
lrt(mod_diag, mod_corgh)
lrt(mod_id,   mod_corgh)
lrt(mod_corgh, mod_fa1)
lrt(mod_fa1, mod_fa2)


# efeito de bloco ---------------------------------------------------------

#loop

results <- data.frame(site = character(), 
                      P_valor = numeric(), 
                      signn = character(), 
                      stringsAsFactors = FALSE)

for (i in levels(malawi$env)) {
  
  soydata <- subset(malawi, env == i)
  
  
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
  data = malawi
)
mod_red <- asreml(
  fixed = GY ~ env,
  random = ~ gen,
  data = malawi
)

lrt(mod_full, mod_red)
