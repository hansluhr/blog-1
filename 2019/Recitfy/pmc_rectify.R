### Script para abordagem rectify com a PMC 
### Autor: João Renato Leripio

library(forecast)
library(tidyverse)
library(sidrar)

## Importar dados da PMC

pmc <- sidrar::get_sidra(api = "/t/3417/n1/all/v/1186/p/all/c11046/40312/d/v1186%201")

pmc_ts <- ts(pmc$Valor, start = c(2003, 01), freq = 12)

## Gerar um conjunto para treino e outro para teste

pmc_treino <- window(pmc_ts, end = c(2017,12))

pmc_teste <- window(pmc_ts, start = c(2018,01))

## Ajustar modelo ETS e gerar erros por CV

fc_fun <- function(x,h) forecast(ets(x), h = h)

fc_erros <- forecast::tsCV(pmc_treino, forecastfunction = fc_fun, window = 100, h = 12)

## Observar o MSE em função do horizonte

fc_erros %>% 
  
  as_tibble() %>%
  
  tidyr::drop_na() %>%
  
  dplyr::summarise_all(funs(mean(.^2))) %>%
  
  tidyr::gather(key = horizonte, value = mse) %>% 
  
  dplyr::mutate(horizonte = readr::parse_number(horizonte)) %>%
  
  ggplot(aes(x = horizonte, y = mse)) + 
  
  geom_point(size = 2) +
  
  scale_x_continuous(breaks = 1:12) +
  
  labs(title = "MSE para cada horizonte de previsão")

## Modelar a estrutura para os erros em cada horizonte

erros_stl <- purrr::map(.x = as.list(fc_erros), .f = mstl)

erros_arima <- purrr::map(.x = as.list(fc_erros), .f = function(x) forecast(auto.arima(x), h = 1))

## Gerar as previsões finais

pmc_fc <- pmc_treino %>% ets() %>% forecast(h = 12) %$% mean %>% as.data.frame()

erros_fc <- purrr::map(.x = erros_arima, .f = function(x) x[["mean"]]) %>% plyr::ldply()

prev_final <- tibble(horizonte = erros_fc$.id,
                     fc_serie = pmc_fc$x,
                     fc_erro = erros_fc$V1,
                     fc_final = fc_serie + fc_erro)

## Observando a acurácia

accuracy(c(pmc_teste), prev_final$fc_final)

accuracy(c(pmc_teste), prev_final$fc_serie)