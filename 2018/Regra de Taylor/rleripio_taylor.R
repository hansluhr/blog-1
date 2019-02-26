### Script para estimar uma regra de Taylor para o Brasil ###
### Autor: João Renato Leripio - leripiorenato@gmail.com ###

## Definir diretório

setwd("C:\\Users\\36277\\Desktop\\Posts\\Julho")

## Carregar as bibliotecas necessárias

library(rbcb)
library(tidyverse)
library(lubridate)
library(timetk)
library(gridExtra)
library(scales)
library(TSdist)

## Importar os dados do BC

ini <- "2000-01-01"
fim <- Sys.Date()

dados_bc <- rbcb::get_series(c(IPCA = 13522, Selic = 4189, IBC = 24364, Meta = 13521, EX = 3698),
                             start_date = ini,
                             end_date = fim) %>%
  
  purrr::reduce(full_join) %>%
  
  tidyr::fill(Meta) %>%
  
  tidyr::drop_na()


dados_bc_aux <- dados_bc %>%
  
  dplyr::mutate(IBC_12 = log(IBC/lag(IBC, 12))*100) %>%

  tidyr::drop_na() %>%
  
  dplyr::mutate(IBC_ts = tk_ts(IBC_12, start = zoo::as.yearmon(first(date)), freq = 12),
                IBC_hp = mFilter::hpfilter(IBC_ts)$cycle,
                Desvio = IPCA - Meta)

## Bernanke: Regra de Taylor original

## Eq: r = p + 0.5y + 0.5(p-m) + 2

## Vetor com valores para os parâmetros 

taylor_dados <- dados_bc_aux %>% 
  
  dplyr::select(date, Selic, IPCA, IBC_hp, Desvio) %>%
  
  dplyr::mutate("Taylor original" = IPCA + 0.5*IBC_hp + 0.5*Desvio + 5.0,
                "Taylor Bernanke" = IPCA + 1*IBC_hp + 0.5*Desvio + 5.0,
                "Taylor Modificada" = IPCA + 0.5*IBC_hp + 1.0*Desvio + 5.0)
  
p1 <- taylor_dados %>% 
  
  ggplot(aes(x = date)) + 
  
  geom_line(aes(y = Selic), color = "steelblue3", linetype = 2, lwd = 1) +
  
  geom_line(aes(y = `Taylor Bernanke`), color = "red", lwd = 1) +
  
  scale_x_date(date_breaks = "1 year", 
               labels = date_format("%Y")) +

  labs(x = "", y = "", 
       title = "Selic vs. Regra de Taylor (maior peso para o produto)",
       subtitle = expression(paste("r = p + ", "1.0" , "y + " , "0.5", "(p-4.5) + 5.0"), sep = "")) +
  
  ylim(0,20)

p2 <- taylor_dados %>% 
  
  ggplot(aes(x = date)) + 
  
  geom_line(aes(y = Selic), color = "steelblue3", linetype = 2, lwd = 1) +
  
  geom_line(aes(y = `Taylor original`), color = "red", lwd = 1) +
  
  scale_x_date(date_breaks = "1 year", 
               labels = date_format("%Y")) +
  
  labs(x = "", y = "", 
       title = "Selic vs. Regra de Taylor (original)",
       subtitle = expression(paste("r = p + ", "0.5" , "y + " , "0.5", "(p-4.5) + 5.0"), sep = "")) +
  
  ylim(0,20)

p3 <- taylor_dados %>% 
  
  ggplot(aes(x = date)) + 
  
  geom_line(aes(y = Selic), color = "steelblue3", linetype = 2, lwd = 1) +
  
  geom_line(aes(y = `Taylor Modificada`), color = "red", lwd = 1) +
  
  scale_x_date(date_breaks = "1 year", 
               labels = date_format("%Y")) +
  
  labs(x = "", y = "", 
       title = "Selic vs. Regra de Taylor (maior peso para inflação)",
       subtitle = expression(paste("r = p + ", "0.5" , "y + " , "1.0", "(p-4.5) + 5.0"), sep = "")) +
  
  ylim(0,20)

grid.arrange(p2, p1, p3)

## Estatísticas de distância

purrr::map2(taylor_dados[, 2], taylor_dados[, c(6,7,8)], CorDistance) %>% plyr::ldply()
