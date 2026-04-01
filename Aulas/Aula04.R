maize = read.csv("~/Documents/GeneticaEstatistica/Aulas/Z005.csv",row.names = 1)
maize = maize[,-seq(ncol(maize)-32, ncol(maize), 1)]
maize[maize == 1] = NA
obs = apply(maize, 2, table)
chisq = apply(obs, 2, function(x) chisq.test(x, p = c(0.5, 0.5))$p.value)

dat = data.frame(pval = sort(chisq), index = seq_along(chisq))
head(dat)

mean(dat$pval<=0.05)
