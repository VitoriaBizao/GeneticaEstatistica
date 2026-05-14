library(tidyverse)

data <- read.csv('https://raw.githubusercontent.com/mauricioaraujj/Pan_African_Trials_Network/refs/heads/main/data/data.csv', sep = ';')

malawi <- data |> subset(data$COUNTRY == "Malawi")

ocorrencias <- malawi %>%
  summarise(
    GY = sum(!is.na(GY)),
    OIL = sum(!is.na(OIL)),
    PH_R8 = sum(!is.na(PH_R8)),
    NDM = sum(!is.na(NDM)),
    RUST_SeV =sum(!is.na(R6_RUST_Sev)),
    RUST_RRT = sum(!is.na(R6_RUST_RRT)),
    PL_COUNT = sum(!is.na(PL_EMERG_COUNT)),
    
  ) %>%
  pivot_longer(cols = everything(),
               names_to = "variavel",
               values_to = "n_obs")
ggplot(ocorrencias, aes(x = variavel, y = n_obs, fill = variavel)) +
  geom_col() +
  labs(
    x = "Variável",
    y = "Número de observações (não-NA)"
  ) +
  theme_minimal()