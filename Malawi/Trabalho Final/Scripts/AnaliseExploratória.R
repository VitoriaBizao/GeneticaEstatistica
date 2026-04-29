library(tidyverse)

data <- read.csv('https://raw.githubusercontent.com/mauricioaraujj/Pan_African_Trials_Network/refs/heads/main/data/data.csv', sep = ';')

malawi <- data |> subset(data$COUNTRY == "Malawi")

# contando ambientes
unique(malawi$env) %>% length()

# comparador
env_malawi <- unique(malawi$env)

comparador <- function(dataset, nome_dataset) {
  correspondencia <- sum(env_malawi %in% dataset$env)
  total <- length(env_malawi)
  
  print(paste(nome_dataset, ":", correspondencia, "/", total))
}

# datasets
tabelas <- list(
  weather = weather,
  Elevation = Elevation,
  bioclimatic = bioclimatic,
  coords = coords,
  soil = soil
)

for (nome in names(tabelas)) {
  comparador(tabelas[[nome]], nome)
}