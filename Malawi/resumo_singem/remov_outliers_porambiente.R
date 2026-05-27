library(asreml)
library(dplyr)
data = read.csv2("https://raw.githubusercontent.com/mauricioaraujj/Pan_African_Trials_Network/refs/heads/main/data/data.csv")
#subset malawi
data <- subset(data, COUNTRY == "Malawi")


remover_outliers_sd <- function(data){
  
  resumo <- data.frame(env = character(),
                       n_outliers = integer(),
                       stringsAsFactors = FALSE)
  
  data_limpo <- data
  outliers_all <- data.frame()   # ← onde ficará a tabela única de outliers
  
  for(e in unique(data$env)){
    
    subconjunto <- data_limpo %>% filter(env == e)
    
    # ajusta modelo
    mod <- asreml(
      fixed   = GY ~ bloco,
      random  = ~ gen,
      data    = subconjunto,
      na.action = na.method(x = "include", y = "include"),
      maxit = 100
    )
    
    # resíduos padronizados
    subconjunto$res <- residuals(mod, type = "stdCond")
    
    sd_res <- sd(subconjunto$res, na.rm = TRUE)
    lim_sup <-  2.8 * sd_res
    lim_inf <- -2.8 * sd_res
    
    # marca outliers
    subconjunto$outlier <- subconjunto$res > lim_sup |
      subconjunto$res < lim_inf
    
    # extrair tabela de outliers do ambiente
    out_env <- subconjunto %>% filter(outlier)
    
    # acumular tudo em uma única tabela
    outliers_all <- bind_rows(outliers_all, out_env)
    
    # resumo
    resumo <- rbind(resumo,
                    data.frame(env = e,
                               n_outliers = nrow(out_env)))
    
    # remover do dataset
    data_limpo <- anti_join(data_limpo, out_env,
                            by = colnames(data_limpo))
  }
  
  return(list(
    data_sem_outliers      = data_limpo,
    tabela_outliers_resumo = resumo,
    outliers               = outliers_all   # ← tabela única de outliers
  ))
}
resultado <- remover_outliers_sd(data)

# Outliers 1 a 1 (tabela completa)
outliersdetail <-  resultado$outliers
outliersdetail <-  resultado$tabela_outliers_resumo
getwd()
write.csv(resultado$data_sem_outliers,"data_sem_outliers.csv",row.names = FALSE)
data <- resultado$data_sem_outliers
