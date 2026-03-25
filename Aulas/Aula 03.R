# Aula 03
library(ggplot2)
library(tidyverse)
library(ggplot2)

# Probability
# Random variables
## Discrete
### Cumulative distribution function
crossprob = dbinom(0:4, size = 4, prob = 1/4); crossprob
cumuprob = cumsum(crossprob); cumuprob

ngreen = seq(0,4,1)
aux = data.frame(ngreen, crossprob, cumuprob)
ggplot(data = aux, aes(x = factor(ngreen))) + 
  geom_point(aes(y = crossprob, shape = "PMF"), size = 2.5, alpha = .8, color = "#10342d") + 
  geom_point(aes(y = cumuprob, shape = "CDF"), size = 2.5, alpha = .8, color = "#10342d") +
  theme_bw() + theme(legend.position = 'top', legend.title = element_blank(),
                     text = element_text(size = 18)) +
  labs(x = "Number of green seeds", y = "Probability of occurrence") +
  scale_y_continuous(breaks = seq(0,1,0.1))

### Expectation
expec = sum(ngreen * crossprob)
expec

### Expectated value estimator (sample mean)
set.seed(8)
sam = rbinom(n = 100000, size = 4, prob = 1/4)
tab = table(sam)
sum(ngreen*tab/sum(tab))

### Histogram
ggplot(data = data.frame(sam)) + 
  geom_histogram(aes(x = sam), color = "white", fill = "#10342d") + 
  theme_minimal() + 
  theme(text = element_text(size = 18)) + 
  labs(x = "Number of green seeds", y = "Counts")

### Median
median(sam)

## Mode
sort(tab, decreasing = TRUE)

### Variance
sum(ngreen^2*crossprob) - expec^2

#### Variance sample estimator
var(sam)

### Standard deviation
sd(sam)

### Sample Covariance Estimator
set.seed(99)
sam2 = rbinom(n = 100000, size = 4, prob = 1/4)
tab2 = table(sam2)
cov(sam, sam2)

### Scatterplot
ggplot(data = data.frame(sam, sam2), aes(x = sam, y = sam2)) + 
  geom_jitter(alpha = .3, color = "#10342d") + 
  labs(x = "Number of green seeds", y = "Number of wrinkled seeds") + 
  theme_minimal() + theme(text = element_text(size = 18))

### Correlation
cor(sam, sam2)

## Continuoues random variables
set.seed(8)
sam = rnorm(n = 1000, mean = 1000, sd = 100)
sam[1:10]
mean(sam)

### Histogram
ggplot(data = data.frame(sam)) + 
  geom_histogram(aes(x = sam), bins = 20, color = 'white', fill = "#10342d") + 
  labs(x = "Seed yield (kg/ha)", y = "Count") + 
  theme_minimal() + theme(text = element_text(size = 18))

### Density plot
ggplot(data = data.frame(sam)) + 
  geom_density(aes(x = sam),color = 'white', fill = '#10342d', alpha = .75) + 
  labs(x = "Seed yield (kg/ha)", y = "Density") + 
  theme_minimal() + theme(text = element_text(size = 18))

### Median
median(sam)

### Mode 
samdens = density(sam)
samdens$x[which.max(samdens$y)]

ggplot(data = data.frame(sam)) + 
  geom_density(aes(x = sam),color = 'white', fill = '#10342d', alpha = .75)  + 
  theme_minimal() + theme(text = element_text(size = 18), legend.position = 'top') +
  geom_vline(aes(xintercept = mean(sam), linetype = "Mean", color = "Mean"), linewidth = 1)+
  geom_vline(aes(xintercept = median(sam), linetype = "Median", color = "Median"), linewidth = 1)+
  geom_vline(aes(xintercept = samdens$x[which.max(samdens$y)], linetype = "Mode", color = "Mode"), linewidth = 1)+ 
  labs(x = "Seed yield (kg/ha)", y = "Density", linetype = "Parameter", colour = "Parameter")

var(sam) # Variance
sd(sam) # Standard deviation

ggplot(data = data.frame(sam)) + 
  geom_density(aes(x = sam),color = 'white', fill = '#10342d', alpha = .75)  + 
  theme_minimal() + theme(text = element_text(size = 18), legend.position = 'top',
                          legend.title = element_blank()) +
  geom_vline(aes(xintercept = mean(sam), linetype = "Mean", color = "Mean"), linewidth = 1)+
  labs(x = "Seed yield (kg/ha)", y = "Density", linetype = "Parameter", colour = "Parameter") +
  geom_vline(aes(xintercept = mean(sam) + sd(sam), linetype = "Mean + SD", color = "Mean + SD"), linewidth = 1) +
  geom_vline(aes(xintercept = mean(sam) - sd(sam), linetype = "Mean - SD", color = "Mean - SD"), linewidth = 1)

### Boxplot
ggplot(data = data.frame(sam)) +
  geom_boxplot(aes(x = sam), fill = "#b2bc63") + 
  annotate(geom = 'point', x = mean(sam), y = 0, shape = 17, color = "#10342d", size = 3)+
  theme_minimal() + theme(axis.text.y = element_blank(), text = element_text(size = 18)) +
  labs(x = "Seed yield (kg/ha)")

### Covariance and correlation
rho = .75
s21 = 1.5
s22 = 4
cov12 = rho*sqrt(s21*s22)
Sigma = matrix(c(
  s21, cov12, 
  cov12, s22
), nrow = 2)

set.seed(16735969)
dat = MASS::mvrnorm(n = 1000, mu = c(5.5, 25), Sigma = Sigma)
colnames(dat) = c("hgw", "npods")

cov(dat[,1], dat[,2]) # covariance
cor(dat[,1], dat[,2]) # correlation

### Central Limit Theorem
E_x = sum(ngreen * crossprob)
V_x = sum(ngreen^2*crossprob) - expec^2
n = 5
iter = 10000
zn = NULL
for (i in 1:iter){
  y = sum(rbinom(n = n, size = 4, prob = 1/4))
  E_y = n * E_x
  V_y = n * V_x
  zn[i] = (y - E_y) / sqrt(V_y)
}

ggplot() +
  geom_histogram(aes(x = zn, y = after_stat(density)), color = 'black') + 
  stat_function(fun = function(x) dnorm(x, mean = 0, sd = 1), xlim = c(-3,3),
                geom = "polygon", alpha = .5, color = 'black') + 
  theme_bw() + 
  theme(text = element_text(size = 18)) + 
  labs(x = "Standardized variable", y = "Density", title = paste("Sample size: ", n))

# EDA
dat = read.csv("https://raw.githubusercontent.com/mauricioaraujj/Pan_African_Trials_Network/refs/heads/main/data/data.csv", 
               sep = ';')
dim(dat)

dat_sub = subset(dat, env == "E007")
dim(dat_sub)

head(dat_sub)
str(dat_sub)
table(dat_sub$rep, dat_sub$gen)
summary(dat_sub$GY)
tapply(dat_sub$GY, dat_sub$gen, function(x) mean(x, na.rm = TRUE))
tapply(dat_sub$GY, dat_sub$gen, function(x) sd(x, na.rm = TRUE))

ggplot(data = dat_sub, aes(x = GY)) + 
  geom_histogram(bins = 25, color = 'black', fill="#10342d",
                 aes(y=after_stat(density))) + 
  geom_density(fill = "#10342d", alpha = .4, color = "black")+
  theme_bw() + 
  theme(text = element_text(size = 18)) + 
  labs(y = "Density", x = "GY (kg/ha)")

ggplot(data = dat_sub, aes(x = as.factor(rep), y = GY)) +
  geom_boxplot(fill = "#10342d", color = "black", alpha = .6) +
  geom_jitter(width = .2, size = 2, alpha = .75, color = "black") +
  theme_bw() + 
  theme(text = element_text(size = 18)) + 
  labs(x = "Replicates", y = "GY (kg/ha)")

ggplot(data = dat_sub, aes(x = as.factor(gen), y = GY)) +
  geom_boxplot(fill = "#10342d", color = "black", alpha = .6) +
  geom_jitter(width = .2, size = 2, alpha = .75, color = "black") +
  theme_bw() + 
  theme(text = element_text(size = 18), 
        axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1)) + 
  labs(x = "Genotypes", y = "GY (kg/ha)")

ggplot(data = dat_sub, aes(x = as.factor(gen), y = GY)) +
  geom_boxplot(aes(fill = as.factor(check)), color = "black", alpha = .6) +
  geom_jitter(width = .2, size = 2, alpha = .75, color = "black") +
  theme_bw() + 
  theme(text = element_text(size = 18), legend.position = 'top', legend.title = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1)) + 
  scale_fill_manual(labels = c("0" = "Treatment", "1" = "Check"),
                    values = c("0" = "#10342d", "1" = "#b2bc63"))+
  labs(x = "Genotypes", y = "GY (kg/ha)")

ggplot(data = dat_sub, aes(x = as.factor(gen), y = GY)) +
  geom_boxplot(aes(fill = as.factor(COMPANY)), color = "black", alpha = .6) +
  geom_jitter(width = .2, size = 2, alpha = .75, color = "black") +
  theme_bw() + 
  theme(text = element_text(size = 18), legend.position = 'top', 
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1)) + 
  scale_fill_manual(values = c("#10342d", "#b2bc63"))+
  labs(x = "Genotypes", y = "GY (kg/ha)")

dat_sub$ord = 1:nrow(dat_sub)

ggplot(data = dat_sub, aes(x = ord, y = GY)) +
  geom_point(size = 2.5, alpha = .75, color = "black") + 
  facet_wrap(.~rep, scales = 'free_x')+
  theme_bw() + 
  theme(text = element_text(size = 18)) + 
  labs(x = "Experimental units", y = "GY (kg/ha)")

ggplot(data = dat_sub, aes(x = PH_R8)) + 
  geom_histogram(bins = 25, color = 'black', fill="#10342d",
                 aes(y=after_stat(density))) + 
  geom_density(fill = "#10342d", alpha = .4, color = "black")+
  theme_bw() + 
  theme(text = element_text(size = 18)) + 
  labs(y = "Density", x = "Plant height (cm)")

ggplot(data = dat_sub, aes(x = as.factor(rep), y = PH_R8)) +
  geom_boxplot(fill = "#10342d", color = "black", alpha = .6) +
  geom_jitter(width = .2, size = 2, alpha = .75, color = "black") +
  theme_bw() + 
  theme(text = element_text(size = 18)) + 
  labs(x = "Replicates", y = "Plant height (cm)")

ggplot(data = dat_sub, aes(x = as.factor(gen), y = PH_R8)) +
  geom_boxplot(fill = "#10342d", color = "black", alpha = .6) +
  geom_jitter(width = .2, size = 2, alpha = .75, color = "black") +
  theme_bw() + 
  theme(text = element_text(size = 18), 
        axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1)) + 
  labs(x = "Genotypes", y = "Plant height (cm)")

ggplot(data = dat_sub, aes(x = GY, y = PH_R8)) +
  geom_point(size = 2.5, alpha = .75, color = "black")+
  theme_bw() + 
  theme(text = element_text(size = 18)) + 
  labs(x = "GY (kg/ha)", y = "Plant height (cm)",
       caption = paste("Correlation: ",
                       round(cor(dat_sub$GY, dat_sub$PH_R8, use = "na.or.complete"), 2)))


# Hands on
## Using your USP number as seed, simulate phenotypic values (don’t forget, P=G+E) using the normal distribution (rnorm).
## Then, compute the mean and the variance of P, G and E, and build a histogram, a density plot, 
## a box plot and a scatter plot depicting the relationship between P and G.
## Do it for three different sample sizes: 10, 100 and 1000.
set.seed(11480230)

## Amostra = 10
Genotype <- rnorm(n = 10, mean = 0, sd = 100)
Environment <- rnorm(n = 10, mean = 0, sd = 10)
Phenotype <- Genotype + Environment

df <- data.frame(Genotype, Environment, Phenotype); df

### Histogram
df |> 
  # Transforming to longer format
  pivot_longer(cols = c(Genotype, Environment, Phenotype), 
               names_to = "origem", 
               values_to = "Caracter") |> 
  # Creating the graphic
  ggplot(aes(x = Caracter, fill = origem)) + 
  geom_histogram(bins = 20, color = 'white', show.legend = FALSE) + 
  
  # Faceting
  facet_wrap(~ origem, ncol = 1, scales = "free_y") + 
  
  # Design
  scale_fill_manual(values = c("Genotype" = "#cb73fa",
                               "Environment" = "purple",
                               "Phenotype" = "purple4")) +
  labs(x = "Caracter",
       y = "Count",
       title = "G, P and E - Histogram") + 
  theme_minimal() + 
  theme(plot.title = element_text(size = 14, face = "bold"),
        strip.text = element_text(face = "bold")) 

### Density
df |> 
  # Transforming to longer format
  pivot_longer(cols = c(Genotype, Environment, Phenotype), 
               names_to = "origem", 
               values_to = "Caracter") |> 
  
  # Creating graphic
  ggplot(aes(x = Caracter, color = origem)) + 
  geom_density(linewidth = 1) + 
  
  # Design
  scale_color_manual(values = c("Genotype" = "#cb73fa",
                                "Environment" = "purple",
                                "Phenotype" = "purple4")) +
  labs(x = "Caracter", 
       y = "Count",
       title = "G, E and P - Density",
       color = "Origem") + 
  theme_minimal() + 
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        plot.title = element_text(face = "bold", size = 14))

### Boxplot
p3 <- df |> 
  # 1. Formato longo para g, e, p
  pivot_longer(cols = c(Genotype, Environment, Phenotype),
               names_to = "origem",
               values_to = "Caracter") |> 
  
  # 2. Início do Plot
  ggplot(aes(x = Caracter, y = 1, fill = origem)) + 
  
  # Boxplot horizontal
  geom_boxplot(color = "black", alpha = 0.8, show.legend = FALSE) + 
  
  # 3. Substituir o annotate por stat_summary para calcular a média por facet
  stat_summary(fun = mean, geom = "point", shape = 17, size = 4, color = "purple4") +
  
  # 4. Facets: uma linha para cada origem
  facet_wrap(~ origem, ncol = 1, strip.position = "left") + 
  
  # Estética
  scale_fill_manual(values = c("Genotype" = "#cb73fa",
                               "Environment" = "purple",
                               "Phenotype" = "purple4")) +
  labs(x = "Caracter",
       y = NULL,
       title = "G, E, and P - Boxplot") +
  theme_minimal() + 
  theme(legend.position = "none",
        legend.title = element_blank(),
        plot.title = element_text(size = 14, face = "bold"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.text = element_text(face = "bold"))

### Scatterplot GxP
p4 <- df |> ggplot(aes(x = Genotype, y = Phenotype)) + 
  geom_jitter(alpha = .3, color = "purple") + 
  labs(x = "Genotype",
       y = "Phenotype",
       title = "Genotype vs. Phenotype - Scatterplot") + 
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"))

# Grid
grid.arrange(p1, p2, p3, p4,
             ncol = 2,
             top = grid::textGrob("Plots for n =",
                                  gp = grid::gpar(fontsize = 14, fontface = "bold")))

# Transforming in a function
samples_dif <- function(n1, n2, n3) {
  set.seed(11480230)   # Setting seed
  sample_sizes <- c(n1, n2, n3) # Receive the sample sizes 
  results <- list() # Store results
  plots <- list() # Store plots
  
  for (i in seq_along(sample_sizes)) {
    n <- sample_sizes[i]
    
    # Generating the data
    Genotype <- rnorm(n = n, mean = 0, sd = 10)
    Environment <- rnorm(n = n, mean = 0, sd = 20)
    Phenotype <- Genotype + Environment
    df <- data.frame(Genotype, Environment, Phenotype)
    results[[i]] <- df
    
    # Plots
    ### Histogram
    p1 <- df |> 
      pivot_longer(cols = c(Genotype, Environment, Phenotype), 
                   names_to = "origem", 
                   values_to = "Caracter") |> 
      ggplot(aes(x = Caracter, fill = origem)) + 
      geom_histogram(bins = 20, color = 'white', show.legend = FALSE) + 
      facet_wrap(~ origem, ncol = 1, scales = "free_y") + 
      scale_fill_manual(values = c("Genotype" = "#cb73fa",
                                   "Environment" = "purple",
                                   "Phenotype" = "purple4")) +
      labs(x = "Caracter",
           y = "Count",
           title = "G, P and E - Histogram") + 
      theme_minimal() + 
      theme(plot.title = element_text(size = 14, face = "bold"),
            strip.text = element_text(face = "bold"))
    
    ### Density
    p2 <- df |> 
      pivot_longer(cols = c(Genotype, Environment, Phenotype), 
                   names_to = "origem", 
                   values_to = "Caracter") |> 
      ggplot(aes(x = Caracter, color = origem)) + 
      geom_density(linewidth = 1) + 
      scale_color_manual(values = c("Genotype" = "#cb73fa",
                                    "Environment" = "purple",
                                    "Phenotype" = "purple4")) +
      labs(x = "Caracter", 
           y = "Count",
           title = "G, E and P - Density",
           color = "Origem") + 
      theme_minimal() + 
      theme(legend.position = "bottom",
            legend.title = element_blank(),
            plot.title = element_text(face = "bold", size = 14))
    
    ### Boxplot
    p3 <- df |> 
      pivot_longer(cols = c(Genotype, Environment, Phenotype),
                   names_to = "origem",
                   values_to = "Caracter") |> 
      ggplot(aes(x = Caracter, y = 1, fill = origem)) + 
      geom_boxplot(color = "black", alpha = 0.8, show.legend = FALSE) + 
      stat_summary(fun = mean, geom = "point", shape = 17, size = 4, color = "purple4") +
      facet_wrap(~ origem, ncol = 1, strip.position = "left") + 
      scale_fill_manual(values = c("Genotype" = "#cb73fa",
                                   "Environment" = "purple",
                                   "Phenotype" = "purple4")) +
      labs(x = "Caracter",
           y = NULL,
           title = "G, E, and P - Boxplot") +
      theme_minimal() + 
      theme(legend.position = "none",
            legend.title = element_blank(),
            plot.title = element_text(size = 14, face = "bold"),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            strip.text = element_text(face = "bold"))
    
    ### Scatterplot GxP
    p4 <- df |> ggplot(aes(x = Genotype, y = Phenotype)) + 
      geom_jitter(alpha = .3, color = "purple") + 
      labs(x = "Genotype",
           y = "Phenotype",
           title = "Genotype vs. Phenotype - Scatterplot") + 
      theme_minimal() +
      theme(plot.title = element_text(size = 14, face = "bold"))
    
    # 2. Criar o Grid para esta amostra n
    grid_n <- grid.arrange(p1, p2, p3, p4,
                           ncol = 2,
                           top = grid::textGrob(paste("Plots for n =", n),
                                                gp = grid::gpar(fontsize = 14, fontface = "bold")))
    
    plots[[i]] <- grid_n
  }
  
  # 3. Tabela Resumo (sample_summary)
  sample_summary <- data.frame(
    "Sample_Size" = sample_sizes,
    "Genotypic_Mean" = sapply(results, function(x) mean(x$Genotype)),
    "Genotypic_Var" = sapply(results, function(x) var(x$Genotype)),
    "Env_Mean" = sapply(results, function(x) mean(x$Environment)),
    "Env_Var" = sapply(results, function(x) var(x$Environment)),
    "Pheno_Mean" = sapply(results, function(x) mean(x$Phenotype)),
    "Pheno_Var" = sapply(results, function(x) var(x$Phenotype))
  )
  
  return(list(table = sample_summary, plots = plots))
}

# Testing function
output <- samples_dif(10, 100, 1000)

# Results summary
output$table

# Plots n = 10
plot(output$plots[[1]])

# Plots n = 100
plot(output$plots[[2]])

# Plots n = 1000
plot(output$plots[[3]])

## Choose a random trial and perform exploratory data analysis with three traits: GY, PH and NDM.
## Then, compute the correlation between these traits and build pairwise scatter plots.
df = read.csv("https://raw.githubusercontent.com/mauricioaraujj/Pan_African_Trials_Network/refs/heads/main/data/data.csv", 
               sep = ';')

# Data structure
str(df)

# selecting the traits in trial 2
df <- df |> 
  filter(rep == 2) |> 
  select(gen, GY, PH_R8, NDM, OIL) |> 
  mutate(gen = as.factor(gen))

str(df)

# EDA 
# Histograms
h_GY <- df |> ggplot(aes(x = GY)) +
    geom_histogram(aes(y = ..density..), fill = "purple4", alpha = 0.75, binwidth = 70) +
    geom_density(fill = "purple", alpha = 0.2, color = "black") +
    labs(title = "GY",
         x = "GY",
         y = "Frequency") +
    theme_minimal() + 
    theme(plot.title = element_text(face = "bold", size = 14))

h_PH <- df |> ggplot(aes(x = PH_R8)) +
  geom_histogram(aes(y = ..density..), fill = "purple4", alpha = 0.75, binwidth = 10) +
  geom_density(fill = "purple", alpha = 0.2, color = "black") +
  labs(title = "PH_R8",
       x = "PH_R8",
       y = "Frequency") +
  theme_minimal() + 
  theme(plot.title = element_text(face = "bold", size = 14))

h_NDM <- df |> ggplot(aes(x = NDM)) +
  geom_histogram(aes(y = ..density..), fill = "purple4", alpha = 0.75, binwidth = 8) +
  geom_density(fill = "purple", alpha = 0.2, color = "black") +
  labs(title = "NDM",
       x = "NDM",
       y = "Frequency") +
  theme_minimal() + 
  theme(plot.title = element_text(face = "bold", size = 14))

h_OIL <- df |> ggplot(aes(x = OIL)) +
  geom_histogram(aes(y = ..density..), fill = "purple4", alpha = 0.75, binwidth = 0.5) +
  geom_density(fill = "purple", alpha = 0.2, color = "black") +
  labs(title = "OIL",
       x = "OIL",
       y = "Frequency") +
  theme_minimal() + 
  theme(plot.title = element_text(face = "bold", size = 14))

grid.arrange(h_GY, h_PH, h_NDM, h_OIL, ncol = 2)

# Loop - Scatterplots
vars <- c("GY", "PH_R8", "NDM", "OIL")
pares <- combn(vars, 2, simplify = FALSE)
resultados <- list()

for (par in pares) {
  x_var <- par[1]
  y_var <- par[2]
  nome_plot <- paste0("scatter_", x_var, "_vs_", y_var)
  
  message("Gerando Scatter: ", x_var, " vs ", y_var)
  
  resultados[[nome_plot]] <- ggplot(df, aes(x = .data[[x_var]], y = .data[[y_var]])) + 
    geom_jitter(color = "gray20", size = 0.75, alpha = 0.2) + 
    geom_smooth(method = "auto", se = TRUE, color = "#cb73fa") +
    labs(title = paste(x_var, "vs", y_var),
         x = x_var,
         y = y_var) + 
    theme_classic() + 
    theme(axis.title = element_text(size = 12, face = "bold"),
          plot.title = element_text(size = 13, face = "bold"))
}

scatters_only <- resultados[grep("scatter", names(resultados))]
grid.arrange(grobs = scatters_only, ncol = 3)

# Correlation
df[,-1] |> GGally::ggpairs()