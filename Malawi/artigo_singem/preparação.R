library(tidyverse)

data <- read.csv('https://raw.githubusercontent.com/mauricioaraujj/Pan_African_Trials_Network/refs/heads/main/data/data.csv', sep = ';')

malawi <- subset(data, COUNTRY == "Malawi" & !is.na(PROT))

rm(data)

#verificando variaveis por  NA
table(malawi$YEAR, useNA = "ifany")
table(malawi$COUNTRY, useNA = "ifany")
table(malawi$env, useNA = "ifany")
table(malawi$gen, useNA = "ifany")
sum(is.na(table(malawi$GY)))
sd(malawi$GY, na.rm = TRUE)


temp <- filter(malawi, YEAR == 2023, env == "E0274")

temp <- malawi %>%
  filter(YEAR == 2023, env == "E0274") %>%
  select(YEAR, gen, rep, GY, NDM, PH_R8)

#valor negativo
malawi$GY[malawi$GY < 0] <- NA
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
#REMOVENDO OUTLIERS GY
malawi_sem_outliers <- malawi %>%
  group_by(env) %>%
  mutate(
    media = mean(GY, na.rm = TRUE),
    DP = sd(GY, na.rm = TRUE),
    
    outlier = GY > (media + 2.5 * DP) |
      GY < (media - 2.5 * DP)
  ) %>%
  filter(!outlier) %>%
  ungroup()

boxplot(GY ~ gen, data = malawi_sem_outliers)

#PARA AS DEMAIS

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
      NDM < (media - 4 * DP)
  ) %>%
  filter(!outlier) %>%
  ungroup()

boxplot(NDM ~ gen, data = malawi_sem_outliers)

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



boxplot(PROT ~ gen, data = malawi_sem_outliers)
boxplot(PROT ~ gen, data = malawi_PROT_outliersrem)

média <- mean(malawi_sem_outliers$PROT)
DP <- sd(malawi_sem_outliers$PROT, na.rm = TRUE)
#
visualize <- outliers [, -c(8:32)]
match("PROT", names(malawi_sem_outliers))


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