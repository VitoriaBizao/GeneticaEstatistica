# Teste de hipótese
cups = combn(8,4)

correct = c(1,3,4,8)
right_choice = apply(cups, 2, function(x) x %in% correct)
right_choice

cs = colSums(right_choice);cs
tcs = table(cs);tcs

library(ggplot2)
ggplot() +
  geom_bar(aes(x = cs), color = "white", fill = "#10342d") +
  theme_minimal() +
  theme(text = element_text(size = 18)) +
  labs(x = "Success count", y = "No. combinations") +
  geom_text(aes(x = c(0, 1, 2, 3, 4), y = tcs + 1,
                label = paste0(round(tcs / sum(tcs) * 100, 2), "%")
  ))

set.seed(56)
smut_pop = rnorm(10000, mean = 5, sd = 2)
smut_pop = smut_pop[-which(smut_pop < 0)]

sample_size = 20
smut_sam = sample(x = smut_pop, size = sample_size, replace = FALSE)
c("mean" = mean(smut_sam),"sd"= sd(smut_sam))

t_stat = (mean(smut_sam) - 5)/(sd(smut_sam)/sqrt(sample_size))
t_stat

t_dist = NULL
sample_size_pop = 20
for (i in 1:1000) {
  set.seed(10*i)
  smut_sam2 = sample(x = smut_pop, size = sample_size_pop, replace = FALSE)
  t_dist[i] = (mean(smut_sam2) - mean(smut_pop))/(sd(smut_sam2)/sqrt(sample_size_pop))
}

ggplot() + 
  geom_histogram(aes(x = t_dist, after_stat(density)), fill = '#10342d',
                 color = 'white', bins = 20) + 
  stat_function(fun = function(x) dt(x, sample_size_pop-1), colour = '#b2bc63', 
                linewidth = 1.5) +
  theme_bw() + 
  theme(text = element_text(size = 18)) +
  labs(x = "t statistic")

pval = pt(t_stat, df = sample_size-1, lower.tail = FALSE)

ggplot() + 
  stat_function(fun = function(x) dt(x, sample_size-1), colour = '#10342d', 
                linewidth = 1.5) + 
  stat_function(fun = function(x) dt(x, sample_size-1), geom="area",
                xlim = c(t_stat, 3), fill = '#10342d', alpha = .7) +
  xlim(-3,3) +
  theme_bw() + 
  theme(text = element_text(size = 18)) +
  labs(x = "t statistic")

t.test(x = smut_sam, alternative = "greater", mu = 5)

set.seed(50)
smut_pop1 = rnorm(10000, mean = 5, sd = 2)
smut_pop1 = smut_pop1[-which(smut_pop1 < 0)]
smut_pop2 = rnorm(10000, mean = 5.75, sd = 2.1)
if(any(smut_pop2 < 0)) smut_pop2 = smut_pop2[-which(smut_pop2 < 0)]

sample_size1 = 20
sample_size2 = 30

smut_sam1 = sample(x = smut_pop1, size = sample_size1, replace = FALSE)
smut_sam2 = sample(x = smut_pop2, size = sample_size2, replace = FALSE)

dat = data.frame(sample = c(smut_sam1, smut_sam2),
                 clone = c(rep("C1", length(smut_sam1)), rep("C2", length(smut_sam2))))
ggplot(data = dat, aes(x = clone, y = sample)) + 
  geom_boxplot(fill = "#b2bc63") + 
  theme_minimal() + 
  theme(text = element_text(size = 18)) +
  labs(x = "Clone", y = "Smut incidence")

t_stat = (mean(smut_sam1) - mean(smut_sam2))/
  (sqrt((var(smut_sam1)/sample_size1)+(var(smut_sam2)/sample_size2)))
df_dif = ((var(smut_sam1)/sample_size1) + (var(smut_sam2)/sample_size2))^2/
  (((var(smut_sam1)/sample_size1)^2/(sample_size1-1)) + 
     ((var(smut_sam2)/sample_size2)^2/(sample_size2-1)))

pval_dif = pt(q = abs(t_stat), df = df_dif, lower.tail = FALSE)

ggplot() + 
  stat_function(fun = function(x) dt(x, df_dif), colour = '#10342d', 
                linewidth = 1.5) + 
  stat_function(fun = function(x) dt(x, df_dif), geom="area",
                xlim = c(abs(t_stat), 3), fill = '#10342d', alpha = .7) +
  stat_function(fun = function(x) dt(x, df_dif), geom="area",
                xlim = c(-3, t_stat), fill = '#10342d', alpha = .7) +
  xlim(-3,3) +
  theme_bw() + 
  theme(text = element_text(size = 18)) +
  labs(x = "t statistic",subtitle = paste("P-value =", round(pval_dif*2,4)))

t.test(smut_sam1, smut_sam2, alternative = "two.sided", var.equal = TRUE)
t.test(smut_sam1, smut_sam2, alternative = "two.sided", var.equal = FALSE)

# Nível de significância
alpha = .05
area = qt(p = 1-alpha, df = sample_size-1)

p1=ggplot() + 
  stat_function(fun = function(x) dt(x, sample_size-1), colour = '#10342d', 
                linewidth = 1.5) + 
  stat_function(fun = function(x) dt(x, sample_size-1), geom="area",
                xlim = c(area, 3), fill = '#10342d', alpha = .7) +
  xlim(-3,3) +
  theme_bw() + 
  theme(text = element_text(size = 18)) +
  labs(x = "t statistic", title = "One-sample unilateral t-test",
       subtitle = expression(alpha==0.05)) + 
  geom_vline(xintercept = area, linetype = "dashed") + 
  annotate(geom = "text", label = expression(NRH[0]), x = 0, y = 0.2, size = 10)+ 
  annotate(geom = "text", label = expression(RH[0]), x = area+.6, y = 0.2, size = 9)

area1 = qt(p = 1-alpha/2, df = df_dif)
area2 = qt(p = alpha/2, df = df_dif)
p2 = ggplot() + 
  stat_function(fun = function(x) dt(x, df_dif), colour = '#10342d', 
                linewidth = 1.5) + 
  stat_function(fun = function(x) dt(x, df_dif), geom="area",
                xlim = c(area1, 3), fill = '#10342d', alpha = .7) +
  stat_function(fun = function(x) dt(x, df_dif), geom="area",
                xlim = c(-3, area2), fill = '#10342d', alpha = .7) +
  xlim(-3,3) +
  theme_bw() + 
  theme(text = element_text(size = 18)) + 
  labs(x = "t statistic", title = "Two-sample bilateral t-test", 
       subtitle = expression(alpha/2==0.025)) +
  geom_vline(xintercept = area1, linetype = "dashed")+
  geom_vline(xintercept = area2, linetype = "dashed") + 
  annotate(geom = "text", label = expression(NRH[0]), x = 0, y = 0.2, size = 9)+ 
  annotate(geom = "text", label = expression(RH[0]), x = area1+.6, y = 0.2, size = 9)+ 
  annotate(geom = "text", label = expression(RH[0]), x = area2-.6, y = 0.2, size = 9)

ggpubr::ggarrange(p1, p2)

ggplot() + 
  stat_function(fun = function(x) dt(x, sample_size-1), colour = '#10342d', 
                linewidth = 1.5) + 
  stat_function(fun = function(x) dt(x, sample_size-1), geom="area",
                xlim = c(qt(p = 1-.03, df = sample_size-1), 3),
                fill = 'forestgreen', alpha = .7)+ 
  stat_function(fun = function(x) dt(x, sample_size-1), geom="area",
                xlim = c(qt(p = 1-.10, df = sample_size-1), 3), 
                fill = 'tomato', alpha = .4) +
  xlim(-3,3) +
  theme_bw() + 
  theme(text = element_text(size = 18)) +
  labs(x = "t statistic", title = "One-sample unilateral t-test",
       subtitle = expression(alpha==0.05)) + 
  geom_vline(xintercept = area, linetype = "dashed") + 
  annotate(geom = "text", label = expression(NRH[0]), x = 0, y = 0.2, size = 10)+ 
  annotate(geom = "text", label = expression(RH[0]), x = area+.6, y = 0.2, size = 9)

set.seed(50)
smut_pop1 = rnorm(10000, mean = 5, sd = 1)
smut_pop2 = rnorm(10000, mean = 5.3, sd = 1)

sample_size1 = 10
sample_size2 = 10

smut_sam1 = sample(x = smut_pop1, size = sample_size1, replace = FALSE)
smut_sam2 = sample(x = smut_pop2, size = sample_size2, replace = FALSE)

p1 = ggplot() + 
  geom_histogram(aes(x = smut_sam1), fill = '#10342d', color = "white", 
                 bins = 20, alpha = .75) + 
  geom_histogram(aes(x = smut_sam2), fill = '#b2bc63', color = "black",
                 bins = 20, alpha = .75) + 
  theme_minimal() +
  theme(text = element_text(size = 18)) + 
  labs(x = "Smut incidence", y = "Count")

p2 = ggplot() + 
  geom_density(aes(x = smut_sam1), fill = '#10342d', color = "white", alpha = .5) + 
  geom_density(aes(x = smut_sam2), fill = '#b2bc63', color = "black", alpha = .5) + 
  theme_minimal() +
  theme(text = element_text(size = 18)) + 
  labs(x = "Smut incidence", y = "Density")

ggpubr::ggarrange(p1, p2)

t.test(smut_sam1, smut_sam2)

sample_size1 = sample_size2 = 1000

smut_sam1 = sample(x = smut_pop1, size = sample_size1, replace = FALSE)
smut_sam2 = sample(x = smut_pop2, size = sample_size2, replace = FALSE)

p1 = ggplot() + 
  geom_histogram(aes(x = smut_sam1), fill = '#10342d', color = "white", 
                 bins = 20, alpha = .5) + 
  geom_histogram(aes(x = smut_sam2), fill = '#b2bc63', color = "black", 
                 bins = 20, alpha = .5) + 
  theme_minimal() +
  theme(text = element_text(size = 18)) + 
  labs(x = "Smut incidence", y = "Count")

p2 = ggplot() + 
  geom_density(aes(x = smut_sam1), fill = '#10342d', color = "white", alpha = .5) + 
  geom_density(aes(x = smut_sam2), fill = '#b2bc63', color = "black", alpha = .5) + 
  theme_minimal() +
  theme(text = element_text(size = 18)) + 
  labs(x = "Smut incidence", y = "Density")

ggpubr::ggarrange(p1, p2)

t.test(smut_sam1, smut_sam2)

alpha1 = 0.05
alpha2 = 0.01
power1 = vector()
power2 = vector()
for (i in seq(10, 1000, 10)) {
  sample_size1 = sample_size2 = i
  pwr = replicate(100, expr = {
    smut_sam1 = sample(x = smut_pop1,
                       size = sample_size1,
                       replace = FALSE)
    smut_sam2 = sample(x = smut_pop2,
                       size = sample_size2,
                       replace = FALSE)
    pval = t.test(smut_sam1, smut_sam2)$p.value
    return(pval)
  })
  power1 = c(power1, mean(pwr <= alpha1))
  power2 = c(power2, mean(pwr <= alpha2))
}

dat = data.frame(
  power = c(power1, power2), 
  size = rep(seq(10, 1000, 10),2),
  alpha = rep(c("0.05", "0.01"), each = length(power1))
)
ggplot(data = dat, aes(x = size, y = power, color = alpha, shape = alpha)) + 
  geom_point(size = 4, alpha = .7) + 
  labs(x = "Sample size", y = "Power", color = expression(alpha),
       shape = expression(alpha)) + 
  theme_minimal() + 
  theme(text = element_text(size = 24), legend.position = 'top') + 
  scale_color_manual(values = c('#10342d', '#b2bc63'))

set.seed(50)
smut_pop1 = rnorm(10000, mean = 5, sd = 2)
smut_pop1 = smut_pop1[-which(smut_pop1 < 0)]
smut_pop2 = rnorm(10000, mean = 10, sd = 2)
if(any(smut_pop2 < 0)) smut_pop2 = smut_pop2[-which(smut_pop2 < 0)]

sample_size1 = 10
sample_size2 = 10

smut_sam1 = sample(x = smut_pop1, size = sample_size1, replace = FALSE)
smut_sam2 = sample(x = smut_pop2, size = sample_size2, replace = FALSE)

p1 = ggplot() + 
  geom_histogram(aes(x = smut_sam1), fill = '#10342d', color = "white", 
                 bins = 20, alpha = .5) + 
  geom_histogram(aes(x = smut_sam2), fill = '#b2bc63', color = "black", 
                 bins = 20, alpha = .5) + 
  theme_minimal() +
  theme(text = element_text(size = 18)) + 
  labs(x = "Smut incidence", y = "Count")

p2 = ggplot() + 
  geom_density(aes(x = smut_sam1), fill = '#10342d', color = "white", alpha = .5) + 
  geom_density(aes(x = smut_sam2), fill = '#b2bc63', color = "black", alpha = .5) + 
  theme_minimal() +
  theme(text = element_text(size = 18)) + 
  labs(x = "Smut incidence", y = "Density")

ggpubr::ggarrange(p1, p2)

t.test(smut_sam1, smut_sam2)

alpha1 = 0.05
alpha2 = 0.01
power1 = vector()
power2 = vector()
for (i in seq(1, 10, 0.5)) {
  sample_size1 = sample_size2 = 10
  smut_pop1 = rnorm(10000, mean = 5, sd = 2)
  smut_pop2 = rnorm(10000, mean = 5+i, sd = 2)
  pwr = replicate(100, expr = {
    smut_sam1 = sample(x = smut_pop1,
                       size = sample_size1,
                       replace = FALSE)
    smut_sam2 = sample(x = smut_pop2,
                       size = sample_size2,
                       replace = FALSE)
    pval = t.test(smut_sam1, smut_sam2)$p.value
    return(pval)
  })
  power1 = c(power1, mean(pwr <= alpha1))
  power2 = c(power2, mean(pwr <= alpha2))
}

dat = data.frame(
  power = c(power1, power2), 
  size = rep(seq(1, 10, 0.5),2),
  alpha = rep(c("0.05", "0.01"), each = length(power1))
)
ggplot(data = dat, aes(x = size, y = power, color = alpha, shape = alpha)) + 
  geom_point(size = 4, alpha = .7) + 
  labs(x = "Sample size", y = "Power", color = expression(alpha),
       shape = expression(alpha)) + 
  theme_minimal() + 
  theme(text = element_text(size = 24), legend.position = 'top') + 
  scale_color_manual(values = c('#10342d', '#b2bc63'))

smut_pop1 = rnorm(10000, mean = 5, sd = 2)
smut_pop1 = smut_pop1[-which(smut_pop1 < 0)]
smut_pop2 = rnorm(10000, mean = 7, sd = 2)
if(any(smut_pop2 < 0)) smut_pop2 = smut_pop2[-which(smut_pop2 < 0)]

sample_size1 = 100
sample_size2 = 100

smut_sam1 = sample(x = smut_pop1, size = sample_size1, replace = FALSE)
smut_sam2 = sample(x = smut_pop2, size = sample_size2, replace = FALSE)

p1 = ggplot() + 
  geom_histogram(aes(x = smut_sam1), fill = '#10342d', color = "white", 
                 bins = 20, alpha = .5) + 
  geom_histogram(aes(x = smut_sam2), fill = '#b2bc63', color = "black", 
                 bins = 20, alpha = .5) + 
  theme_minimal() +
  theme(text = element_text(size = 18)) + 
  labs(x = "Smut incidence", y = "Count")

p2 = ggplot() + 
  geom_density(aes(x = smut_sam1), fill = '#10342d', color = "white", alpha = .5) + 
  geom_density(aes(x = smut_sam2), fill = '#b2bc63', color = "black", alpha = .5) + 
  theme_minimal() +
  theme(text = element_text(size = 18)) + 
  labs(x = "Smut incidence", y = "Density")

ggpubr::ggarrange(p1, p2)

sample_size = 100
ntest = 5000
multcom = matrix(ncol = 2, nrow = ntest)
colnames(multcom) = c("tstat", "pvalue")
for (i in 1:ntest) {
  set.seed(10*i)
  smut_sam1 = sample(x = smut_pop1, size = sample_size, replace = FALSE)
  smut_sam2 = sample(x = smut_pop1, size = sample_size, replace = FALSE)
  ttest = t.test(smut_sam1, smut_sam2, var.equal = TRUE)
  multcom[i,] = c(ttest$statistic, ttest$p.value)
}
multcom = as.data.frame(multcom)

p1 = ggplot(data = multcom) + 
  geom_histogram(aes(x = tstat), fill = '#10342d', color = "white", bins = 25, 
                 alpha = .7) + 
  theme_minimal() +
  theme(text = element_text(size = 18)) + 
  labs(x = "t statistic", y = "Count")

p2 = ggplot(data = multcom) + 
  geom_histogram(aes(x = pvalue), fill = '#10342d', color = "white", bins = 30, 
                 alpha = .7) + 
  theme_minimal() +
  theme(text = element_text(size = 18)) + 
  labs(x = "p-value", y = "Count",
       caption = paste0("False positive rate = ", 
                        round(mean(multcom$pvalue <= 0.05) * 100,2), "%")) +
  geom_vline(xintercept = 0.05, linewidth = 1, linetype='dashed')

ggpubr::ggarrange(p1, p2)

# Problema da comparação múltipla
mean(multcom$pvalue <= 0.05/nrow(multcom))

multcom = multcom[order(multcom$pvalue, decreasing = FALSE),]
multcom$rank = seq_along(multcom$pvalue)
q = 0.05
multcom$crit = (multcom$rank/nrow(multcom))*q
multcom$decision = multcom$pvalue <= multcom$crit
unique(multcom$decision)

multcom$adjust_pval = p.adjust(p = multcom$pvalue, method = "BH")
range(multcom$adjust_pval)

maize = read.csv("../../Misc/Z005.csv",row.names = 1)
maize = maize[,-seq(ncol(maize)-32, ncol(maize), 1)]
maize[maize == 1] = NA
obs = apply(maize, 2, table)
chisq = apply(obs, 2, function(x) chisq.test(x, p = c(0.5, 0.5))$p.value)

dat = data.frame(pval = sort(chisq), index = seq_along(chisq))

mean(dat$pval<=0.05)
mean(dat$pval<=0.05/nrow(maize))
mean(p.adjust(dat$pval, method = "BH")<=0.05)

ggplot(data = dat, aes(x = index, y = pval)) +
  geom_point(size = 1, alpha = .3) + 
  geom_hline(aes(yintercept = 0.05, color = "alpha", linetype = "alpha"),
             linewidth = 1, alpha = .7) + 
  geom_hline(aes(yintercept = 0.05/nrow(dat), color = "Bonferroni", linetype = "Bonferroni"),
             linewidth = 1, alpha = .7)+
  geom_line(aes(y = (index/nrow(dat))*0.05, x = index, 
                color = "FDR", linetype = "FDR"),
            linewidth = 1, alpha = .7) + 
  theme_minimal() +
  theme(text = element_text(size = 18), legend.position = 'top', 
        legend.title = element_blank()) + 
  labs(x = "Index", y = "p-value", linetype = "", colour = "") + 
  scale_color_manual(values = rev(c("#10342d", "#517300", "#b2bc63")), 
                     labels = c(expression(alpha), "Bonferroni", "FDR")) +
  scale_linetype_manual(values = c("solid", 'dashed', 'dotted'),
                        labels = c(expression(alpha), "Bonferroni", "FDR")) +
  ggmagnify::geom_magnify(aes(from = index <=min(dat[which(pval >= 0.05), 'index'])), to = list(0, 300, .25, .5))
