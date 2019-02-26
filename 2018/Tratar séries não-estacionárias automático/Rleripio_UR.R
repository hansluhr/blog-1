## Script para testar a presença de raiz unitária e diferenciar as séries que tiverem raiz.
## Link: http://rleripio.com.br/blog/
## Postado em:
## Autor: J. Renato Leripio - www.rleripio.com.br / contato@rleripio.com.br

## 1. Configurações iniciais 

## 1.1 Carregar pacotes necessários

library(tidyverse)
library(forecast)
library(timetk)

## 2. Simular séries estacionárias e não-estacionárias

set.seed(1)

mu <- list(-2, 0, 2, 4, 6)

sigma <- list(1, 2, 3, 4, 5)

series <- purrr::map2_dfc(mu, sigma, rnorm, n = 100) %>% dplyr::mutate_all(funs(timetk::tk_ts))

## 2.1. Definir séries 1 e 3 como não-estacionárias

series_aux <- series %>% dplyr::mutate_at(vars(V1,V3), funs(cumsum))

## 2.2. Plotar o gráfico

series_aux %>% tidyr::gather(key = "Série", value = "valor") %>%
  
  ggplot(aes(x = rep(seq(1,100),5), y = valor, color = Série)) + 
  
  geom_line(lwd = 1) +
  
  labs(x = "", y = "", color = "")

## 3. Testar a presença de raiz unitária nas séries e atribuir "SIM" àquelas 
## que são I(1) em ao menos 2 testes.

testes <- list(kpss = "kpss", pp = "pp", adf = "adf")

ur_map <- function(x) purrr::map(testes, function(y){forecast::ndiffs(x, alpha = 0.05, y)})

series_ndiffs <- series_aux %>% 
  
  purrr::map(.f = ur_map) %>%
  
  plyr::ldply(bind_rows) %>%
  
  dplyr::mutate(Diferenciar = ifelse(kpss + adf + pp >= 2, "SIM", "NÃO"))

## 4. Diferenciar séries não-estacionárias

## 4.1. Selecionar as séries I(1)

series_labs <- series_ndiffs %>% 
  
  dplyr::filter(Diferenciar == "SIM") %>% 
  
  dplyr::select(.id)

## 4.2. Criar função auxiliar para diferenciar

dif <- function(x){x-dplyr::lag(x)}

## 4.3. Diferenciar 

series_dif <- series_aux %>% 
  
  dplyr::mutate_at(vars(series_labs$.id), funs(dif))


