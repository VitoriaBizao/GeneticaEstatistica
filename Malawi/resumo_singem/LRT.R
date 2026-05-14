library(tidyverse)
getwd()
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


library(asreml)
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
