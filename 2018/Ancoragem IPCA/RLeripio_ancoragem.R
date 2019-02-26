## Medidas de ancoragem de expectativas.
## Inspirado no exercício do WEO do IMF, outubro de 2018.
## Autor: J. Renato Leripio

#### 1. Configurações preliminares

## Definir diretório

setwd("C:\\Users\\36277\\Desktop\\Renato\\Ancoragem IPCA")

## Carregar pacotes

library(tidyverse)
library(readxl)
library(sweep)
library(tibbletime)
library(scales)
library(rbcb)

## Importar os dados e arrumar os dados da seguinte maneira:
## Colocar em frequência mensal e adicionar variável h para indicar a quantidade 
## de anos à frente da previsão. 

exp <- list.files(pattern = "exp") %>% 
  
  purrr::map(.f = readxl::read_excel, skip = 1) %>%
  
  purrr::map_df(.f = tidyr::gather, key = ano, value = valor, -Data) %>%
  
  dplyr::mutate(Data = as.Date(Data, format = "%d/%m/%Y")) %>%
  
  
  dplyr::mutate(h = case_when(as.numeric(lubridate::year(Data)) == as.numeric(ano) ~ "h0",
                              as.numeric(lubridate::year(Data)) == as.numeric(ano)-1 ~ "h1",
                              as.numeric(lubridate::year(Data)) == as.numeric(ano)-2 ~ "h2",
                              as.numeric(lubridate::year(Data)) == as.numeric(ano)-3 ~ "h3")) %>%
  
  tidyr::drop_na() %>%
  
  dplyr::mutate(Year = lubridate::year(Data),
                Month = lubridate::month(Data)) %>%
  
  dplyr::group_by(Year, Month, ano, h) %>%
  
  dplyr::summarise(valor = mean(valor)) %>%
  
  dplyr::mutate(Data = lubridate::make_date(year = Year, month = Month, day = 1L))  %>% 
  
  dplyr::ungroup() %>%
  
  dplyr::select(Data, h, valor) %>%
  
  tidyr::spread(key = h, value = valor) %>%
  
  dplyr::mutate(d_h3 = h3 - lag(h3),
                d_h0 = h0 - lag(h0))

## Modelo que vai ser estimado

## Janela

k <- 60

modelo_roll <- tibbletime::rollify(~(summary(lm(.y ~ .x))$coefficients[2]), 
                                     window = k, unlist = TRUE)

exp_aux <- exp %>% dplyr::mutate(coef = modelo_roll(d_h0, d_h3))

exp_aux %>% 
  
  dplyr::filter(Data >= "2006-12-01") %>%
  
  ggplot(aes(x = Data)) + 
  
  geom_line(aes(y = coef), lwd = 1.2, color = "steelblue3") +
  
  scale_x_date(date_breaks = "1 year",
               labels = date_format("%Y")) +
  
  labs(title = "Desancoragem das expectativas de inflação",
       subtitle = paste("Janela móvel de", k, "meses", sep = " "),
       color = "",
       x = "",
       y = "") +

  theme_light() +
  
  geom_vline(xintercept = as.Date("2003-01-01"), linetype = 2) +
  
  geom_vline(xintercept = as.Date("2010-12-31"), linetype = 2) +
  
  geom_vline(xintercept = as.Date("2016-06-09"), linetype = 2) +
  
  ggplot2::annotate(geom = "text", 
                    x = as.Date("2008-02-09"), 
                    y = 0.02, 
                    label= "Henrique \n Meirelles", 
                    size = 5) +
  
  ggplot2::annotate(geom = "text", 
                    x = as.Date("2013-06-09"), 
                    y = 0.02, 
                    label= "Alexandre \n Tombini",
                    size = 5) +
  
  ggplot2::annotate(geom = "text", 
                    x = as.Date("2018-02-09"), 
                    y = 0.05, 
                    label= "Ilan \n Goldfajn",
                    size = 5)

