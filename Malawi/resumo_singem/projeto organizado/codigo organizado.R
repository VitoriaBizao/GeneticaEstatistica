library(asreml)
library(dplyr)
data <- read.csv("data_sem_outliers.csv")

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
nlevels(data$env)
#organização da coluna

data <- relocate(data, GY, .after = check)

#valor negativo
data$GY[data$GY < 0] <- NA

boxplot(GY ~ env, data)

data[, ]

boxplot(GY ~ gen, subset(data, env == "E0199"))

outliers <- data %>%
  filter(
    (env == "E0290" & gen == "G338" & rep == 3) |
    (env == "E0289" & gen == "G056" & rep %in% c(1, 2)) |
    (env == "E0199" & gen == "G136" & rep == 3) |
    (env == "E0199" & gen == "G136" & rep == 3) 
  )

data <- data %>%
  mutate(GY = replace(GY, interaction(env, gen, rep) %in%
                        interaction(outliers$env, outliers$gen, outliers$rep),
NA_real_))


# Função para calcular % explicada
