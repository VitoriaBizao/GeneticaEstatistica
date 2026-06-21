library(asreml)
library(tidyverse)
library(lmmtools)

data = read.csv("https://raw.githubusercontent.com/ufv-molecular-breeding-lab/schaves_sweetpotato_two_stage/refs/heads/main/Data/Phenotype.csv")

data = data %>% mutate(
  geno = as.factor(geno),
  env = as.factor(env),
  rn = as.factor(row_number),
  cn = as.factor(col_number),
  colgroup = as.factor(colgroup),
  rowgroup = as.factor(rowgroup),
  rc.group = as.factor(rc.group),
  pool = as.factor(pool),
  check = as.factor(check),
  geno.cod = as.factor(geno.cod),
  check.cod = as.factor(check.cod)
  )
nlevels(data$env)
table(data$env)
testenv <- subset(data, env == "OT1NAM22")

asreml.options(workspace = '1gb', pworkspace = '2gb')

summary(data$check)

m1 <- asreml(
  fixed = rytha ~ at(check, "Check"):geno,
  random = ~ at(check, "Candidate"): geno,
  residual = ~ units,
  na.action = na.method(x = "include"),
  data = testenv,
  maxit = 30
)

m2 <- asreml(
  fixed = rytha ~ at(check, "Check"):geno,
  random = ~ at(check, "Candidate"): geno,
  residual = ~ ar1v(rn):ar1(cn),
  na.action = na.method(x = "include"),
  data = testenv,
  maxit = 50
)

m3 <- asreml(
  fixed = rytha ~ at(check, "Check"):geno,
  random = ~ at(check, "Candidate"): geno + units,
  residual = ~ ar1v(rn):ar1(cn),
  na.action = na.method(x = "include"),
  data = testenv,
  maxit = 50
  )

m4 <- asreml(
  fixed = rytha ~ at(check, "Check"):geno,
  random = ~ at(check, "Candidate"): geno + units + rn + cn,
  residual = ~ ar1v(rn):ar1(cn),
  na.action = na.method(x = "include"),
  data = testenv,
  maxit = 300
)

plot(varioGram(m0))

icREML(list(m1,m2,m3, m4))
