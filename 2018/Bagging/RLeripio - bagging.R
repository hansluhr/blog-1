## Carregar os pacotes necessários

library(sidrar)
library(tidyverse)
library(forecast)
library(timetk)

## Importar os dados da PNAD 

pnad <- sidrar::get_sidra(api = "/t/6381/n1/all/v/4099/p/all/d/v4099%201")

## Visualizar os dados 

pnad_aux <- pnad %>% 
  
  dplyr::select(Data = `Trimestre Móvel (Código)`, TD = Valor) %>%
  
  dplyr::mutate(Data = lubridate::ymd(paste(Data, "01", sep = "")))

pnad_aux %>% ggplot(aes(x = Data, y = TD)) + 
  
  geom_line(lwd = 1, color = "darkblue") +
  
  labs(title = "PNAD - Taxa de desocupação (%)",
       subtitle = "Média móvel de 3 meses",
       x = "",
       y = "")

## Criar série de tempo 

pnad_ts <- ts(pnad$Valor, start = c(2012,3), freq = 12)

## Definir amostras de treino e de teste

pnad_treino <- window(pnad_ts, end = c(2016,7))

pnad_teste <- window(pnad_ts, start = c(2016,8))

## Definir o número de bootstraps

k <- 10

## Computar as séries através de "blocked bootstrap"

pnad_boot <- forecast::bld.mbb.bootstrap(pnad_treino, k) %>%
  
  purrr::map(.f = ts, start = c(2012,3), freq = 12)

## Visualizar

autoplot(pnad_treino) + 
  
  autolayer(ts(as.data.frame(pnad_boot), start = c(2012,3), freq = 12), lwd = 1) +
  
  autolayer(pnad_treino, colour = "FALSE", lwd = 1) +
  
  guides(color = "none") +
  
  labs(title = "Séries calculadas por Bootstrap",
       x = "", y = "")

## Computar as previsões das séries 

baggedModel(pnad_ts, bootstrapped_series = bld.mbb.bootstrap(pnad_ts, 100), fn = "auto.arima")


aa_fc <- function(x){forecast(auto.arima(x, max.d = 1), n = 24)[["mean"]]}

pnad_boot_fc <- purrr::map(.x = pnad_boot, .f = aa_fc)

## Computar a previsão pelo método bagging

fc_original <- pnad_boot_fc[[1]]

fc_bagged <- pnad_boot_fc %>% purrr::reduce(`+`) %>% `/`(k)

## Comparar os dois

accuracy(fc_original, pnad_teste)
accuracy(fc_bagged, pnad_teste)




