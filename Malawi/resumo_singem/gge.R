library(tidyverse)
library(asreml)
library(metan)
data = read.csv2("https://raw.githubusercontent.com/mauricioaraujj/Pan_African_Trials_Network/refs/heads/main/data/data.csv")
data <- subset(data, COUNTRY == "Malawi")

inspect(data, GY)
outliers <- find_outliers(data, GY, by = env)
outliers <- find_outliers((filter(data, env == "E0154")), GY)
outliers <- [c(13, 16, 28, 31, 36, 41, 42, 54, 55, 66, 72, 77, 83, 88, 90), ]
env36 <- filter(data, env == "E036")
tabela_outs <- env36[c(13, 16, 28, 31, 36, 41, 42, 54, 55, 66, 72, 77, 83, 88, 90), ]
env154 <- filter(data, env == "E0154")
tabela_out <-  tabela_outs %>%  add_row(env154[c(7, 29, 38, 41, 57, 65, 80, 84, 91, 117, 118), ])
data <- anti_join(data, tabela_out)

gge_model <- gge(data, env, gen, resp = GY)
s <- plot(gge_model, type = 10)
t <- plot(gge_model,
          type = 10,
          col.gen = "black",
          title = FALSE)
arrange_ggplot(s, t, tag_levels = list(c("s", "t")))
t

# Extraindo os parâmetros FA
varcomp <- summary(mod_fa4)$varcomp

# Os loadings ficam nomeados como fa(env, 1)!env_nome_Comp1
loadings <- varcomp[grep("fa\\(env", rownames(varcomp)), ]

# Visualizando
print(loadings)

load_F1 <- loadings[,1]
ranking <- sort(abs(load_F1), decreasing = TRUE)
ranking
