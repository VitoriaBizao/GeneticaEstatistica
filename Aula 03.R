# Aula 03
library(ggplot2)
library(tidyverse)

set.seed(11480230)

## Amostra = 10
g <- rnorm(n = 10, mean = 0, sd = 100)
e <- rnorm(n = 10, mean = 0, sd = 10)
f <- g + e

Vg <- var(g); Vg
Ve <- var(e); Ve
Vf <- var(f); Vf

Mg <- mean(g); Mg
Me <- mean(e); Me
Mf <- mean(f); Mf

df <- data.frame(g, e, f); df

### Histogram
ggplot(data = data.frame(g)) + 
  geom_histogram(aes(x = g), bins = 20, color = 'white', fill = "#10342d") + 
  labs(x = "Seed yield (kg/ha)", y = "Count") + 
  theme_minimal() + theme(text = element_text(size = 18))

### Densidade


### Boxplot

### Scatterplot
