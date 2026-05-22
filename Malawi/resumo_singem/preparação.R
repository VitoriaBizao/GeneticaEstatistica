library(tidyverse)

# preparation -------------------------------------------------------------


data <- read.csv('https://raw.githubusercontent.com/mauricioaraujj/Pan_African_Trials_Network/refs/heads/main/data/data.csv', sep = ';')

malawi <- subset(data, COUNTRY == "Malawi")

srm(data)

#verificando variaveis por  NA
table(malawi$YEAR, useNA = "ifany")
table(malawi$COUNTRY, useNA = "ifany")
table(malawi$env, useNA = "ifany")
table(malawi$gen, useNA = "ifany")
sum(is.na(table(malawi$GY)))
sd(malawi$GY, na.rm = TRUE)

# testing -----------------------------------------------------------------



temp <- filter(malawi, YEAR == 2023, env == "E0274")

temp <- malawi %>%
  filter(YEAR == 2023, env == "E0274") %>%
  select(YEAR, gen, rep, GY, NDM, PH_R8)

#TESTANDO PARA UM AMBIENTE
DP <- sd(malawi$GY, na.rm = TRUE)
média <- mean(malawi$GY, na.rm = TRUE)


(temp$GY >  média+(2.5*DP))
(temp$GY <  média-(2.5*DP))
temp[53,]
temp[61,]$GY <- NA

outliers <- malawi %>%
  group_by(env) %>%
  mutate(
    media = mean(GY, na.rm = TRUE),
    DP = sd(GY, na.rm = TRUE),
    
    outlier = GY > (media + 2.5 * DP) |
      GY < (media - 2.5 * DP)
  ) %>%
  filter(outlier)

#outlier GY
boxplot(GY ~ gen, data = malawi)

# gy ----------------------------------------------------------------------
#valor negativo
malawi$GY[malawi$GY < 0] <- NA
#REMOVENDO OUTLIERS GY
malawi_sem_outliers <- malawi %>%
  group_by(env) %>%
  mutate(
    media = mean(GY, na.rm = TRUE),
    DP = sd(GY, na.rm = TRUE),
    
    outlier = GY > (media + 2 * DP) |
      GY < (media - 2 * DP)
  ) %>%
  filter(!outlier) %>%
  ungroup()

boxplot(GY ~ gen, data = malawi_sem_outliers)



# NDM ---------------------------------------------------------------------


boxplot(NDM ~ gen, data = malawi)

#remove 0
malawi$NDM[malawi$NDM == 0] <- NA
malawi_sem_outliers$NDM[malawi_sem_outliers$NDM == 0] <- NA

table(malawi$NDM, useNA = "ifany")
#411 na em 3870 obs. (que tem leitura de PROT)
# 496 TOTAL (7190)

média <- mean(malawi_sem_outliers$NDM)
DP <- sd(malawi_sem_outliers$NDM, na.rm = TRUE)

malawi_sem_outliers <- malawi_sem_outliers %>%
  group_by(gen, SEASON) %>%
  mutate(
    media = mean(NDM, na.rm = TRUE),
    DP = sd(NDM, na.rm = TRUE),
    
    outlier = NDM > (media + 6.6 * DP) |
      NDM < (media - 2.5 * DP)
  ) %>%
  filter(!outlier) %>%
  ungroup()

boxplot(NDM ~ gen, data = malawi_sem_outliers)

# PROT --------------------------------------------------------------------

#PROT
boxplot(PROT ~ gen, data = malawi_sem_outliers)
sum(is.na(malawi_sem_outliers$PROT))

malawi_PROT_outliersrem <- malawi_sem_outliers %>%
  group_by(gen, env) %>%
  mutate(
    media = mean(PROT, na.rm = TRUE),
    DP = sd(PROT, na.rm = TRUE),
    
    outlier = PROT > (media + 3 * DP) |
      PROT < (media - 3 * DP)
  ) %>%
  filter(!outlier) %>%
  ungroup()



boxplot(PROT ~ env, data = malawi_sem_outliers)

boxplot(PROT ~ env, data = malawi_PROT_outliersrem)

média <- mean(malawi_sem_outliers$PROT)
DP <- sd(malawi_sem_outliers$PROT, na.rm = TRUE)

# MOSTRAR OUTLIERS DE PROT

outliers <- malawi_sem_outliers %>%
  group_by(env) %>%
  mutate(
    media = mean(PROT, na.rm = TRUE),
    DP = sd(PROT, na.rm = TRUE),
    
    limite_superior = media + 3 * DP,
    limite_inferior = media - 3 * DP,
    
    outlier = ifelse(
      is.na(DP) | DP == 0,
      FALSE,
      PROT > limite_superior |
        PROT < limite_inferior
    )
  ) %>%
  filter(outlier) %>%
  ungroup()

3338 - 3259
#altura de planta 
boxplot(PH_R8 ~ env,data = malawi_sem_outliers)

# phr8 --------------------------------------------------------------------

#remove 0
malawi$PH_R8[malawi$PH_R8 == 0] <- NA
malawi_sem_outliers$PH_R8[malawi_sem_outliers$PH_R8 == 0] <- NA

média = mean(malawi_sem_outliers$PH_R8, na.rm = TRUE)
DP = sd(malawi_sem_outliers$PH_R8, na.rm = TRUE)

temp <- malawi_sem_outliers %>%
  group_by(gen, SEASON) %>%
  mutate(
    media = mean(PH_R8, na.rm = TRUE),
    DP = sd(PH_R8, na.rm = TRUE),
    
    outlier = PH_R8 > (media + 2 * DP) |
    PH_R8 < (media - 2 * DP)
  ) %>%
  filter(!outlier) %>%
  ungroup()

boxplot(PH_R8 ~ env, data = malawi_sem_outliers)
boxplot(PH_R8 ~ env, data = temp)

saveRDS(temp, file ="treateddata.rds")


# visualize ---------------------------------------------------------------
which(colnames(malawi) == "PH_R8")
visualize <- malawi [, -c(8:15)]
match("PROT", names(malawi_sem_outliers))
