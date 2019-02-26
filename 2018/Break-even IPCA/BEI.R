### Script para post sobre Inflação implícita ###

#### 1. Configurações preliminares

setwd("C:\\Users\\36277\\Desktop\\Posts\\Rmd\\BEI")

library(tidyverse)
library(rbcb)
library(readxl)
library(forecast)
library(lubridate)
library(readr)

#### 2. Importar dados (já foi feito e só basta importar)

## Dados até 2016/10

# bei <- readxl::read_excel("dados.xlsx", sheet = "BEI")
# 
# bei_aux <- bei %>% 
#   
#   dplyr::mutate(Data = substr(Data, 1, nchar(Data)-2) %>% 
#                   
#                   paste("01", sep = "-") %>% 
#                   
#                   lubridate::ymd()) %>%
#   
#   dplyr::select(Data, bei_12)
# 
# ## Dados de 2016/10 a 2018/06
# 
# bei_novo <- read_delim("bei_novo_2.txt", ";", 
#                        escape_double = FALSE, locale = locale(encoding = "ISO-8859-1",
#                                                               decimal_mark = ","), 
#                        trim_ws = TRUE)
# 
# bei_novo_aux <- bei_novo %>% 
#   
#   dplyr::filter(`Código da taxa` %in% c("PRE", "DIC"),
#                 between(`Número de dias corridos`, 360, 365)) %>%
#   
#   dplyr::select(Data = `Data de referência`, 
#                 Tipo = `Código da taxa`, 
#                 Valor = `Valor da taxa`) %>%
#   
#   dplyr::mutate(Data = as.Date(Data, format = "%d/%m/%Y"),
#                 year = lubridate::year(Data),
#                 month = lubridate::month(Data)) %>%
#   
#   dplyr::group_by(year, month, Tipo) %>%
#   
#   dplyr::summarise(Taxa = mean(Valor)) %>%
#   
#   dplyr::ungroup() %>%
#   
#   dplyr::mutate(Data = lubridate::make_date(year = year, month = month)) %>%
#   
#   tidyr::spread(key = Tipo, value = Taxa) %>%
#   
#   dplyr::select(-month, -year) %>%
#   
#   dplyr::mutate(bei_12 = (((1+(PRE/100))/(1+(DIC/100)))-1)*100) %>%
#   
#   dplyr::select(Data, bei_12)
# 
# ## Reunir os dados
# 
# bei_total <- dplyr::bind_rows(bei_aux, bei_novo_aux)

bei_total <- readxl::read_excel("bei_total.xlsx") %>% dplyr::mutate(Data = lubridate::ymd(Data))

## Importar dados de expectativa de IPCA (Focus) e IPCA realizado

einf <- rbcb::get_twelve_months_inflation_expectations(indic = "IPCA", 
                                                       start_date = first(bei_total$Data),
                                                       end_date = last(bei_total$Data))

einf_aux <- einf %>%
  
  dplyr::filter(smoothed == "N") %>%
  
  dplyr::select("Data" = date, median) %>%
  
  dplyr::mutate(year = lubridate::year(Data),
                month = lubridate::month(Data)) %>%
  
  dplyr::group_by(year, month) %>%
  
  dplyr::summarise(einf_12 = mean(median)) %>%
  
  dplyr::mutate(Data = lubridate::make_date(year = year, month = month)) %>%
  
  dplyr::ungroup() %>%
  
  dplyr::select(Data, einf_12)

ipca_12 <- rbcb::get_series(code = list("ipca_12" = 13522), 
                            start_date = first(bei_total$Data))

## Reunir os dados de expectativa focus e bei
## Restringir ao final da série do IPCA - 11

dados <- dplyr::inner_join(bei_total, einf_aux) %>%
  
  dplyr::select(Data, einf_12, bei_12) %>%
  
  dplyr::mutate(pr_12 = bei_12 - einf_12) %>%
  
  dplyr::filter(Data %in% c(ipca_12$date %m-% months(11)))
  
summary(dados) 

## Definir uma amostra de treino

dados_treino <- dados %>% dplyr::filter(Data <= last(dados$Data) %m-% months(24))

dados_teste <- dados %>% dplyr::filter(!Data %in% dados_treino$Data)

## Modelar o Prêmio a partir de auto.arima

premio_auto <- dados_treino$pr_12 %>% auto.arima(max.p = 2)

summary(premio_auto)

checkresiduals(premio_auto)

## Observar a acurácia da expectativa a partir da Expectativa dos títulos e do Focus

# OBS: vamos considerar o IPCA observado na data da previsão do Focus e da BEI, isto é,
#      para o acumulado em 12 meses 11 meses à frente.

ipca_12_aux <- ipca_12 %>% 
  
  dplyr::mutate(Data = date %m-% months(11)) %>%
  
  dplyr::select(Data, "ipca_obs" = ipca_12)


pr_12_fc <- forecast(premio_auto, h = nrow(dados_teste))

dados_teste_aux <- dados_teste %>% 
  
  dplyr::select(Data, einf_12, bei_12) %>%
  
  dplyr::mutate(pr_12_fc = c(pr_12_fc$mean)) %>%
  
  dplyr::mutate(exp_bei_12 = bei_12 - pr_12_fc)

dados_final <- dados_teste_aux %>% dplyr::inner_join(ipca_12_aux)

dados_final %>%
  
  #dplyr::filter(Data >= "2016-12-01") %>%
  
  dplyr::mutate(Data = Data %m+% months(11)) %>%
  
  dplyr::select(Data, "Focus" = einf_12, "BEI" = exp_bei_12, "Observado" = ipca_obs) %>%
  
  tidyr::gather(key = var, value = valor, -Data) %>%
  
  ggplot(aes(x = Data, y = valor, color = var, linetype = var)) + 
  
  scale_linetype_manual(values = c(2,2,1)) +
  
  scale_x_date(date_labels = "%b-%Y",
               date_breaks = "3 month") +
  
  geom_line(lwd = 1) +
  
  labs(title = "IPCA 12 meses à frente - valores observados vs. esperados",
       subtitle = "% acumulado em 12 meses",
       color = "",
       linetype = "",
       x = "",
       y = "")

## Medidas de acurácia

accuracy(dados_final$einf_12, dados_final$ipca_obs)

accuracy(dados_final$exp_bei_12, dados_final$ipca_obs)

## Diebold-Mariano

e_focus <- dados_final$einf_12-dados_final$ipca_obs 

e_bei <- dados_final$exp_bei_12-dados_final$ipca_obs 

forecast::dm.test(e_focus, e_bei, h = 24, alternative = "less")


###

gecon
.G3c0n@2015
