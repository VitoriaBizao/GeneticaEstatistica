library(asreml)
library(dplyr)
data1 = read.csv2("https://raw.githubusercontent.com/mauricioaraujj/Pan_African_Trials_Network/refs/heads/main/data/data.csv")
#subset malawi
data1 <- subset(data1, COUNTRY == "Malawi")

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
#organização da coluna

data <- relocate(data, GY, .after = check)

#valor negativo
data$GY[data$GY < 0] <- NA

boxplot(GY ~ env, data)

visualize <- filter(data, env == "E0199" & gen == "G136"  )

inspect(data, GY)
find_outliers(data, GY)
data[, ]

boxplot(GY ~ gen, subset(data, env == "E0199"))
# 
# outliers <- data %>%
#   filter(
#     (env == "E0290" & gen == "G338" & rep == 3) |
#     (env == "E0289" & gen == "G056" & rep %in% c(1, 2)) |
#     (env == "E0199" & gen == "G136" & rep == 3) |
#     (env == "E0199" & gen == "G136" & rep == 3) |
#     (env == "E0160")
#   )
# 
# data <- data %>%
#   mutate(GY = replace(GY, interaction(env, gen, rep) %in%
#                         interaction(outliers$env, outliers$gen, outliers$rep),
#                       NA_real_))


vc <- summary(mod_fa4)$varcomp
fa_rows <- grep("fa", rownames(vc), value = TRUE)

fa_rows

loadings <- sum.M4[grep('fa4',rownames(summary(mod_fa4)$varcomp)),1]


specific <- vc[grep("specific|s\\.|env", fa_rows, ignore.case = TRUE), "component"]
