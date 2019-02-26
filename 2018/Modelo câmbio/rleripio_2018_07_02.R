### Script da publicação de 02-07-2018 ###
### http://www.rleripio.com.br/blog
### Autor: João Renato Leripio
### Dúvidas e sugestões: contato@rleripio.com.br

### 1. Configuração preliminares

### 1.1. Carregar bibliotecas necessárias (caso não estejam instaladas, é preciso fazê-lo.)

library(tidyverse)
library(rbcb)
library(alfred)
library(readxl)
library(rvest)
library(scales)

setwd("C:\\Users\\36277\\Downloads\\Posts-20190226T131437Z-001\\Posts\\Junho")

### 1.2. Importar os dados do FRED: 10-year yields, VIX, TWETB

## Government bond yields 10-years, mensal.

ini <- "2002-01-01"
fim <- "2018-06-01"

tb_10yr <- get_fred_series(series_id = "GS10", 
                              series_name = "tb_10yr", 
                              observation_start = ini,
                              observation_end = fim) %>% dplyr::select(c(date, tb_10yr))

## Trade-weighted exchange rate.

twex <- get_fred_series(series_id = "TWEXBMTH",
                        series_name = "twex",
                        observation_start = ini,
                        observation_end = fim)

## CBOE Volatility index

vix_daily <- get_fred_series(series_id = "VIXCLS",
                       series_name = "vix_daily",
                       observation_start = ini,
                       observation_end = fim)

vix <- vix_daily %>% 
  
  dplyr::mutate(year = lubridate::year(date),
                                   month = lubridate::month(date)) %>%
  
  dplyr::group_by(year, month) %>% 
  
  dplyr::summarise(vix = mean(vix_daily, na.rm = TRUE)) %>%
  
  dplyr::mutate(date = lubridate::ymd(paste(year, month, "01", sep = "-"))) %>%
  
  dplyr::ungroup() %>%
  
  dplyr::select(c(date, vix))

## Reunir os dados

dados_fred <- dplyr::left_join(tb_10yr, twex) %>% dplyr::left_join(vix)

### 1.3. Importar dados do BCB: câmbio (R$/US$) média mensal - 3697;

dados_bc <- get_series(c(3697, 27574),
                  start_date = ini,
                  end_date = fim) %>% purrr::reduce(left_join)

colnames(dados_bc) <- c("date", "brl", "ic")

### 1.4. Importar dados do Embi (risco país)

embi_daily <- read_excel("embi.xls")

embi_daily$date <- seq(from = lubridate::ymd("2002-01-01"), 
                 to = lubridate::ymd("2018-06-11"), 
                 by = "day")

embi <- embi_daily %>% 
  
  dplyr::mutate(year = lubridate::year(date),
                month = lubridate::month(date)) %>%
  
  dplyr::group_by(year, month) %>% 
  
  dplyr::summarise(embi = mean(embi, na.rm = TRUE)) %>%
  
  dplyr::mutate(date = lubridate::ymd(paste(year, month, "01", sep = "-"))) %>%
  
  dplyr::ungroup() %>%
  
  dplyr::select(c(date, embi))

### Reunir todos os dados

dados_reg <- dplyr::left_join(dados_fred, dados_bc) %>% 
  
  dplyr::left_join(embi) %>%
  
  dplyr::mutate(brl_l1 = lag(brl))
  
### Observar séries

dados_reg %>% 
  
  #dplyr::mutate_at(vars(twex:embi), log) %>% 
  
  tidyr::gather(key = var, value = valor, -date, -brl_l1) %>%
  
  dplyr::filter(date >= "2015-01-01") %>%
  
  ggplot(aes(x = date, y = valor)) + 
  
  geom_line(lwd = 1.0) +
  
  facet_wrap(~ var, scales = "free") +
  
  labs(x = "", y = "",
       title = "Variáveis selecionadas") +
  
  scale_x_date(date_labels = "%b-%y", date_breaks = "6 months") +
  
  geom_vline(xintercept = as.Date("2018-01-01"), lwd = 1, lty = 2, color = "red")
          
  
## Observar correlações

dados_reg %>% 
  
  dplyr::mutate_at(vars(twex:embi), log) %>%
  
  tidyr::gather(key = var, value = valor, -date, -brl) %>%
  
  ggplot(aes(x = brl, y = valor)) + 
  
  geom_point() + 
  
  stat_smooth(se = F, method = "lm") +
  
  facet_wrap( ~ var, scales = "free") +
  
  labs(x = "", y = "",
       title = "Correlação entre variáveis",
       subtitle = "Eixos em logaritmo")

#### 1.4. Ajustar o modelo

modelo <- lm(log(brl) ~  log(twex) + log(embi) + tb_10yr + log(vix) + log(ic), 
             data = dados_reg)

### Visualizar ajuste

modelo_fit <- tibble(Data = dados_reg$date,
                     Observado = dados_reg$brl,
                     Ajustado = exp(modelo$fitted.values))

modelo_fit %>% tidyr::gather(key = var, value = valor, -Data) %>%
  
  ggplot(aes(x = Data, y = valor, color = var)) + 
  
  geom_line(lwd = 1.0) +
  
  labs(x = "", y = "", color = "",
       title = "Taxa de câmbio (R$/US$)") +
  
  theme_light() +
  
  theme(legend.position = "bottom")
