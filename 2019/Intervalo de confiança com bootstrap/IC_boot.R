### Script para postagem sobre intervalos de confiança com bootstrap ###
### http://www.rleripio.com.br/blog
### Contato: leripiorenato@gmail.com

### 1. Configurações preliminares

setwd("C:\\Users\\36277\\Desktop\\Posts\\2019\\Janeiro\\Intervalos de confiança")

library(forecast)
library(tidyverse)
library(scales)

### 2. Importar dados

caged <- readxl::read_excel("exemplo_caged.xlsx")

caged_ts <- ts(caged$Total, start = c(2004,1), frequency = 12)

### 3. Ajustar o modelo de previsão para 500 amostras geradas via bootstrap

caged_boot <- forecast::bld.mbb.bootstrap(caged_ts, 500)

caged_boot_plot <- autoplot(caged_ts) + 
  
  autolayer(ts(as.data.frame(caged_boot), start = c(2004,1), freq = 12), lwd = 1) +
  
  autolayer(caged_ts, colour = "FALSE", lwd = 1) +
  
  guides(color = "none") +
  
  labs(title = "Séries calculadas por Bootstrap",
       x = "", y = "")

### 4. Gerar a previsão através de um modelo ETS para as 500 séries

caged_ets <- purrr::map(.x = caged_boot, .f = function(x) forecast(ets(x), h = 12))

### 5. Plotar

names(caged_ets) <- paste("Série", 1:length(caged_ets), sep = "_")

caged_ets_mean <- purrr::map(.x = caged_ets, .f = function(x){x[["mean"]] %>% as.numeric()})

caged_ets_prev <- plyr::ldply(caged_ets_mean, rbind)

prev <- caged_ets_prev %>%
  
  tidyr::gather(key = h, value = valor, -.id) %>%
  
  dplyr::mutate(h = as.numeric(h)) %>%
  
  dplyr::group_by(h) %>%
  
  dplyr::summarise(Centro = mean(valor),
                   Inferior = quantile(valor, prob = 0.05),
                   Superior = quantile(valor, prob = 0.95))

prev %>% ggplot(aes(h, Centro)) +
  
  geom_line(lwd = 1.2, color = "steelblue3") +
  
  geom_ribbon(aes(ymin = Inferior, ymax = Superior), alpha=0.3, fill = "steelblue2") +
  
  labs(title = "Previsões a partir de modelo ets", 
       subtitle = "Valores obtidos via bootstrap",
       y = "",
       x = "Períodos") +
  
  scale_x_continuous(breaks = 1:12)
