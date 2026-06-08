data <-  read.csv2("https://raw.githubusercontent.com/VitoriaBizao/GeneticaEstatistica/refs/heads/main/Malawi/resumo_singem/dataoutr.csv")
data <- read.csv('https://raw.githubusercontent.com/mauricioaraujj/Pan_African_Trials_Network/refs/heads/main/data/data.csv', sep = ';')

library(asreml)

# GY ----------------------------------------------------------------------


m0_GY <- asreml(
  fixed   = GY ~ env + check,
  random  = ~ gen + 
    gen:env +
    bloco:env,
  rcov    = ~ at(env):units,
  data    = data
)

summary(m0_GY)$aic
plot(residuals(M1))
qqnorm(residuals(m0_GY)); qqline(residuals(m0_GY))


m1_GY <- asreml(
  fixed   = GY ~ env + check,
  random  = ~ gen + 
    fa(env,1):gen +
    bloco:env,
  rcov    = ~ at(env):units,
  data    = data,
  na.action = na.method(x = "include", y = "include"),
  maxit = 100)

m2_GY <- asreml(
  fixed   = GY ~ env + check,
  random  = ~ gen + 
    fa(env,2):gen +
    bloco:env,
  rcov    = ~ at(env):units,
  data    = data,
  na.action = na.method(x = "include", y = "include"),
  maxit = 300)

m3_GY <- asreml(
  fixed   = GY ~ env + check,
  random  = ~ gen + 
    fa(env,3):gen +
    bloco:env,
  rcov    = ~ at(env):units,
  data    = data,
  na.action = na.method(x = "include", y = "include"),
  maxit = 300)

m4_GY <- asreml(
  fixed   = GY ~ env + check,
  random  = ~ gen + 
    fa(env,4):gen +
    bloco:env,
  rcov    = ~ at(env):units,
  data    = data,
  na.action = na.method(x = "include", y = "include"),
  maxit = 300)

m4_GY <- asreml(
  fixed   = GY ~ env + check,
  random  = ~ gen + 
    fa(env,5):gen +
    bloco:env,
  rcov    = ~ at(env):units,
  data    = data,
  na.action = na.method(x = "include", y = "include"),
  maxit = 300)

m5_GY <- asreml(
  fixed   = GY ~ env + check,
  random  = ~ gen + 
    fa(env,5):gen +
    bloco:env,
  rcov    = ~ at(env):units,
  data    = data,
  na.action = na.method(x = "include", y = "include"),
  maxit = 300)

lrt(m1_GY, m2_GY)

qqnorm(residuals(m1_GY)); qqline(residuals(m1_GY))


# NDM ---------------------------------------------------------------------
boxplot(NDM ~ env, data )
m0_NDM <- asreml(
  fixed   = NDM ~ env + check + bloco:env,
  random  = ~ gen + 
    gen:env,
  rcov    = ~ at(env):units,
  data    = data,
  na.action = na.method(x = "include", y = "include"),
  maxit = 300)

summary(m0_GY)$aic
plot(residuals(M1))
qqnorm(residuals(m0_NDM)); qqline(residuals(m0_NDM))


m1_NDM <- asreml(
  fixed   = NDM ~ env + check + bloco:env,
  random  = ~ gen + 
    fa(env, 1):gen,
  rcov    = ~ at(env):units,
  data    = data,
  na.action = na.method(x = "include", y = "include"),
  maxit = 300)

summary(m1_NDM)$aic
plot(residuals(M1))
qqnorm(residuals(m1_NDM)); qqline(residuals(m1_NDM))

lrt(m0_NDM, m1_NDM)
plot(residuals(m1_NDM))
qqnorm(residuals(m1_NDM)); qqline(residuals(m1_NDM))

data$resid_std <- residuals(m0_NDM, type = "stdCond")

boxplot(
  resid_std ~ env,
  data = data,
  las = 2
)
abline(h = c(-5, 5), col = "red")
#AMBIENTE MUITO RUIM
data$NDM[data$env == "E0249"] <- NA
data <- data %>%
  filter(env != "E0249")  

outliers <- data %>%
  filter(abs(resid_std) > 7)

data <- data %>%
  mutate(
    outlier = abs(resid_std) > 5,
    NDM = ifelse(outlier, NA, NDM)
  )
# PH_R8 -------------------------------------------------------------------
boxplot(PH_R8 ~ env, data)
m0_PH <- asreml(
  fixed   = PH_R8 ~ env + check,
  random  = ~ gen + 
    gen:env +
    bloco:env,
  rcov    = ~ at(env):units,
  data    = data
)

summary(m0_GY)$aic
plot(residuals(M1))
qqnorm(residuals(m0_PH)); qqline(residuals(m0_PH))


m1_PH <- asreml(
  fixed   = PH_R8 ~ env + check + bloco:env,
  random  = ~ gen + 
    fa(env,1):gen,
  rcov    = ~ at(env):units,
  data    = data,
  na.action = na.method(x = "include", y = "include"),
  maxit = 300)
qqnorm(residuals(m1_PH)); qqline(residuals(m1_PH))

m2_PH <- asreml(
  fixed   = PH_R8 ~ env + check + bloco:env,
  random  = ~ gen + 
    fa(env,2):gen,
  rcov    = ~ at(env):units,
  data    = data,
  na.action = na.method(x = "include", y = "include"),
  maxit = 300)
qqnorm(residuals(m2_PH)); qqline(residuals(m2_PH))

lrt(m0_PH, m1_PH)

data <- data_clean
boxplot(PH_R8 ~ env, data)
boxplot(PH_R8 ~ env, data_clean)

data$resid_std <- residuals(m1_PH, type = "stdCond")

boxplot(
  resid_std ~ env,
  data = data,
  las = 2
)

abline(h = c(-7, 7), col = "red")

outliers <- data %>%
  filter(abs(resid_std) > 7)

data <- data %>%
  mutate(
    outlier = abs(resid_std) > 7,
    PH_R8 = ifelse(outlier, NA, PH_R8)
  )

