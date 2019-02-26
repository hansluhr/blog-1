### Script para estimar modelo explicativo de juros no mercado de crédito
### Autor: João Renato Leripio - www.rleripio.com.br - leripiorenato@gmail.com

#### 1. Configurações preliminares ####

## Definir diretório de trabalho

setwd("C:\\Users\\36277\\Desktop\\Renato\\Crédito")

## Carregar pacotes

library(tidyverse)
library(timetk)
library(rbcb)
library(forecast)
library(readxl)

## Importar dados utilizados

## Taxa média de juros - recursos livres - pessoa física - total: 20740
## Inadimplência - pessoa física - total: 21112
## Endividamento das famílias - 19882
## Oferta observada de crédito consumo - 21393
## Demanda de crédito consumo - 21385
## Selic - 4189
## Incluir swap pré-DI: 30, 60, 90, 360

iie <- read_excel("iie.xlsx") %>%
  
  dplyr::mutate(quarter = lubridate::quarter(Data, with_year = TRUE)) %>%
  
  dplyr::group_by(quarter) %>%
  
  dplyr::summarise(iie = log(mean(iie)))


embi <- read_excel("embi.xls") %>%
  
  dplyr::mutate(Data = lubridate::dmy(date),
                quarter = lubridate::quarter(Data, with_year = T)) %>%
  
  dplyr::group_by(quarter) %>%
  
  dplyr::summarise(embi = log(mean(embi, na.rm = T)))

series <- list("Juros" = 20740,
               "Inad" = 21112,
               "Endi" = 20400,
               "DI_30" = 7816,
               "DI_60" = 7817,
               "DI_90" = 7818,
               "DI_120" = 7819,
               "DI_180" = 7820,
               "DI_360" = 7821,
               "Selic" = 4189)

dados <- rbcb::get_series(series) %>% 
  
  purrr::reduce(inner_join) %>%
  
  dplyr::mutate(quarter = lubridate::quarter(date, with_year = T)) %>%
  
  dplyr::group_by(quarter) %>%
  
  dplyr::summarise_at(vars(-quarter), funs(mean)) %>%
  
  dplyr::select(-date)

dados_aux <- dplyr::left_join(dados, iie) %>% dplyr::left_join(embi)

## Observar as séries

dados_aux %>% 
  
  tidyr::gather(key = var, value = valor, -quarter) %>%
  
  ggplot(aes(x = quarter, y = valor)) + 
  
  geom_line() +
  
  facet_wrap( ~ factor(var, levels = c("Juros", "Selic", "DI_30", "DI_60", "DI_90",
                                       "DI_120","DI_180", "DI_360", "Inad", "Endi", "iie",
                                       "embi")),
              scales = "free")

## Realizar teste de raiz unitária e diferenciar as séries não-estacionárias

testes <- list(kpss = "kpss", pp = "pp", adf = "adf")

ur_map <- function(x) purrr::map(testes, function(y){forecast::ndiffs(x, alpha = 0.05, y)})

dados_ndiffs <- dados_aux[-1] %>% 
  
  purrr::map(.f = ur_map) %>%
  
  plyr::ldply(bind_rows) %>%
  
  dplyr::mutate(Diferenciar = ifelse(kpss + adf + pp >= 2, "SIM", "NÃO"))

dados_labs <- dados_ndiffs %>% 
  
  dplyr::filter(Diferenciar == "SIM") %>% 
  
  dplyr::select(.id)

## Criar função auxiliar para diferenciar

dif <- function(x){x-dplyr::lag(x)}

## Diferenciar 

dados_dif <- dados_aux %>% 
  
  dplyr::mutate_at(vars(dados_labs$.id), funs(dif)) %>%
  
  tidyr::drop_na() %>%
  
  dplyr::mutate(tri = sub('.*\\.', '', quarter) %>% as.numeric())

#### 3. Modelo ####

modelo_lm <- lm(Juros ~ lag(DI_90, 1) + lag(Inad,1) + lag(iie,3) + factor(tri), 
                data = dados_dif)

modelo_tbl <- sweep::sw_augment(modelo_lm) %>%
  
  dplyr::select(`Juros Observado` = Juros, `Modelo` = .fitted) %>%
  
  dplyr::mutate(quarter = dados_dif$quarter[4:29])

modelo_tbl %>% tidyr::gather(key = var, value = valor, -quarter) %>%
  
  ggplot(aes(x = quarter, y = valor, linetype = var, color = var), group = 1) + 
  
  geom_line(lwd = 1) +
  
  labs(title = "Juros do crédito livre para pessoa física",
       subtitle = "Variação trimestral (p.p)",
       color = "",
       linetype = "",
       x = "",
       y = "") +
  
  theme_get()


#### 2. Gráfico juros crédito vs. juros referência ####

series_graf <- list("Juros" = 20740,
               "DI_30" = 7816,
               "DI_60" = 7817,
               "DI_90" = 7818,
               "DI_120" = 7819,
               "DI_180" = 7820,
               "DI_360" = 7821,
               "Selic" = 4189)

dados_graf <- rbcb::get_series(series_graf) %>% 
  
  purrr::reduce(inner_join)


p <- dados_graf %>% ggplot(aes(x = date))

p <- p + geom_line(aes(y = Juros, color = "Juros - Crédito livre PF"), 
                   lwd = 1)

p <- p + geom_line(aes(y = Selic*5, color = "Selic"), 
                   lwd = 1)

p <- p + scale_y_continuous(sec.axis = sec_axis(~./5, name = "Selic")) +
  
  labs(x = "", 
       y = "Juros - Crédito livre PF",
       title = "Juros do crédito a pessoas físicas vs. Selic",
       subtitle = "% a.a - valores mensais",
       color = "") +
  
  theme(legend.position = "bottom")
  
     

#### Tabela lm ####

sjt.lm(modelo_lm)
