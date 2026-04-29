library(tidyverse)
data <- read.csv('https://raw.githubusercontent.com/mauricioaraujj/Pan_African_Trials_Network/refs/heads/main/data/data.csv', sep = ';')
datsub <- data |> subset(data$COUNTRY == "Malawi")
unique(datsub$env) 