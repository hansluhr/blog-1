library(forecast)
library(rbcb)
library(tidyverse)
library(zoo)

## 1.2 Importar dados do IPCA núcleo EX-3 do BCB

core <- rbcb::get_series(code = 27839)

core_ts <- ts(core$`27839`, start = first(zoo::as.yearmon(core$date)), freq = 12)

## 1.3 Definir período de treino e de teste (vamos utilizar 1 ano para o teste, ou 12 observações)

core_treino <- window(core_ts, end = c(2017,07))

core_teste <- window(core_ts, start = c(2017,08))

mod_fun <- list("Arima" = auto.arima, 
                "Tbats" = tbats, 
                "ETS" = ets, 
                "Rede neural AR" = nnetar, 
                "Holt-Winters" = hw)

modelos <- purrr::invoke_map(.f = mod_fun, .x = list(core_treino))

modelos_fc <- purrr::map(.f = forecast, .x = modelos, h = 12)

## 3.2 Obter medidas de acurácia fora da amostra (conjunto de teste)

modelos_ac <- purrr::map(.f = accuracy, modelos_fc, core_teste)

## 3.3 Selecionar apenas as medidas de RMSE e MAE para o conjunto de teste (fora da amostra)

modelos_ac_teste <- purrr::map(.x = modelos_ac, .f = function(x){
  
  x["Test set", c("RMSE", "MAE")]
  
}
  )

## 3.4 Colocando em formato de tibble para comparar, ordenando por ordem crescente do MAE

modelos_ac_tbl <- modelos_ac_teste %>% plyr::ldply(.id = "Modelo") %>% dplyr::arrange(MAE)

fc_ets <- forecast(ets(core_ts), h = 12)
fc_tbats <- forecast(tbats(core_ts), h = 12)
fc_arima <- forecast(Arima(core_ts, order = c(1,1,1), seasonal = c(0,0,1)), h = 12)

fc_media <- (fc_ets[["mean"]]+fc_tbats[["mean"]]+fc_arima[["mean"]])/3

autoplot(core_ts) +
  
  autolayer(fc_ets, series="ETS", PI = FALSE) +
  
  autolayer(fc_tbats, series = "TBATS", PI = FALSE) +
  
  autolayer(fc_arima, series = "Arima", PI = FALSE) +
  
  autolayer(fc_media, series = "Média") +
  
  labs(title = "IPCA - Núcleo EX-3 (% a.m)",
       x = "",
       y = "",
       color = "Previsão")
