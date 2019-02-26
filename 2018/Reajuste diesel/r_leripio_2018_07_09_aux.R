### Scrip para calcular preços contrafactuais da gasolina
### utilizando metodologias descritas no WP do FMI

#### 1. Configurações preliminares ####

### 1.1. Definir diretório de trabalho 

setwd("C:\\Users\\36277\\Desktop\\Posts\\Junho")

### 1.2. Carregar bibliotecas necessárias

library(tidyverse)
library(readxl)
library(alfred)
library(rbcb)
library(scales)

### 1.3. Importar dados de preços e margens para gasolina e diesel (ANP)

anp <- read_excel("fuel_pricing.xlsx") %>%
  
  dplyr::mutate(ano = lubridate::year(date),
                mês = lubridate::month(date),
                semana = lubridate::week(date))

diesel_anp <- anp %>% 
  
  dplyr::filter(tipo == "ÓLEO DIESEL") %>%
  
  dplyr::mutate(preço_ptb = preço*0.52,
                impostos = preço*0.28) %>%
  
  dplyr::select(c(ano, mês, semana, preço, preço_ptb, impostos))

### 1.4. Importar dados de câmbio e cotação internacional do barril

oil <- get_fred_series(series_id = "DCOILBRENTEU", 
                       series_name = "oil",
                       observation_start = "2013-01-05", 
                       observation_end = "2018-06-18")

oil <- oil %>% 
  
  dplyr::mutate(ano = lubridate::year(date),
                             mês = lubridate::month(date),
                             dia = lubridate::day(date)) %>%
  
  dplyr::select(-c(date))

### 1.5. Importar dados de câmbio

ex <- get_series(code = 1, 
                 start_date = "2013-01-05", 
                 end_date = "2018-06-18")

ex <- ex %>% 
  
  dplyr::mutate(ano = lubridate::year(date),
                mês = lubridate::month(date),
                dia = lubridate::day(date))

### 1.4. Reunir os dados

dados <- ex %>% 
  
  dplyr::select(c(ano, mês, dia, ex = `1`)) %>% 
  
  dplyr::left_join(oil) %>%
  
  dplyr::slice(925:n()) %>%
  
  tidyr::drop_na() %>%
  
  dplyr::mutate(oil_rs = oil*ex)

### O Full pass-through (FPT) será quando o valor do Diesel incorporar exatamente 
### a variação do preço do petróleo. 
### No reajuste semanal, será variável toda semana; na regra mensal, o reajuste será o acumulado
### no mês anterior;

mensal <- dados %>% 
  
  dplyr::group_by(ano, mês) %>%
  
  dplyr::summarise(Mensal_aux = last(oil_rs)) %>%
  
  dplyr::ungroup() %>%
  
  dplyr::mutate(Mensal_aux = lag(Mensal_aux))

### Reunir os dados e criar a regra mensal

regra <- dplyr::left_join(dados, mensal) %>%
  
  dplyr::mutate(Mensal = case_when(!is.na(Mensal_aux) ~ Mensal_aux,
                                   is.na(Mensal_aux) ~ oil_rs),
                
                Dia = 1:n()) %>%
  
  dplyr::select(c(ano, mês, dia, Diário = oil_rs, Mensal, Dia)) %>%
  
  dplyr::slice(20:n()) %>%
  
  dplyr::ungroup() %>%
  
  dplyr::mutate_at(vars(c("Diário", "Mensal")), funs(((./first(.))*100)))

## Plotar o reajuste diário e mensal

regra %>% 
  
  tidyr::gather(key = var, value = valor, -c(ano, mês, dia, Dia)) %>%
  
  ggplot(aes(x = Dia, y = valor, color = var)) + 
  
  geom_line(lwd = 1.0) + 
  
  theme_light() +
  
  labs(x = "", y = "", color = "Reajuste", 
       title = "Evolução do preço do Diesel",
       subtitle = "Número-índice (outubro/2016 = 100)",
       caption = "Elaboração: RLeripio.com.br com dados do FRED e do BCB")

## Criar o frame de variações

change <- regra %>% 
  
  dplyr::mutate_at(c("Mensal", "Diário"), funs(log(./lag(.))*100)) %>%
  
  tidyr::drop_na()


change %>% 
  
  tidyr::gather(key = var, value = valor, -c(ano, mês, ano, dia, Dia)) %>%
  
  ggplot(aes(x = Dia, y = valor)) + 
  
  geom_line(lwd = 1.0, color = "steelblue3") +
  
  theme_light() +
  
  labs(x = "", y = "", 
       title = "Variação no preço do Diesel por frequência do reajuste",
       subtitle = "% a.d",
       caption = "Elaboração: RLeripio.com.br com dados do FRED e do BCB") +
  
  geom_hline(yintercept = c(-2.5, 2.5), lty = 2) +
  
  facet_wrap(~ var, scales = "fixed") +
  
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())
  

















