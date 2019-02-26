### Script para nowcast da atividade ###
### Autor: J. Renato Leripio
### Contato: leripiorenato@gmail.com / www.rleripio.com/contato

#### 1. Configurações preliminares ####

## Definir diretório de trabalho

setwd("C:\\Users\\36277\\Desktop\\Renato\\Nowcasting")

## Carregar pacotes

library(tidyverse)
library(rbcb)
library(seasonal)
library(timetk)
library(readxl)
library(forecast)

## Baixar os dados 

series_bcb <- list(#"PMS" = 23982,
                   "PMC" = 1455,
                   #"Credito" = 20631,
                   "Inf_ex3" = 27839,
                   "Energia" = 1404,
                   "Industria" = 21862,
                   "Capacidade" = 24352,
                   "Consumidor" = 4393
                   #"Servicos" = 17660
)

dados <- rbcb::get_series(code = series_bcb) %>% purrr::reduce(inner_join, by = "date")

caged <- readxl::read_excel("caged_jul18.xlsx") %>% 
  
  dplyr::mutate(date = as.Date(Data)) %>%
  
  dplyr::select(date, caged = Dentro)

confianca <- readxl::read_excel("confiança.xlsx") %>%
  
  dplyr::mutate(date = as.Date(Data)) %>%
  
  dplyr::select(-Data)

dados_join <- dplyr::inner_join(dados, confianca, by = "date") %>% 
  
  dplyr::mutate_at(vars(-date), funs(as.numeric))

## Aplicar tratamento sazonal

fun_ts <- function(x){timetk::tk_ts(x, start = c(2006,7), frequency = 12)}

dados_ts <- dados_join %>% dplyr::mutate_at(vars(-date), funs(fun_ts))

fun_sa <- function(x){final(seas(x))}

dados_sa <- dados_ts %>% dplyr::mutate_at(vars(-date, -Consumidor, -Termos, -Cons_ISA,
                                               Cons_EXP, Ind_ISA, Ind_EXP), funs(fun_sa))

## Aplicar log, box-cox e calcular o PC para cada período.

log_1 <- function(x){log(1+x)}

dados_aux <- dados_sa %>% dplyr::mutate_at(vars(-date), funs(log_1)) %>% tidyr::drop_na()

dados_pca <- prcomp(dados_aux[,-1], center = TRUE, scale. = TRUE)

## Criar as variáveis-índice

var_index <- as.matrix(dados_aux[, -1]) %*% dados_pca$rotation %>% 
  
  as.tibble()
                               
#### 2. Adequar as séries ####

## Calcular o acumulado em 3 meses

dados_3m <- var_index %>% 
  
  #dplyr::mutate_all(funs(./first(.))) %>%
  
  dplyr::mutate(Data = dados_aux$date) %>%
  
  dplyr::mutate(quarter = lubridate::quarter(Data, with_year = T)) %>%
  
  dplyr::group_by(quarter) %>% 
  
  dplyr::summarise_at(vars(-c(Data, quarter)), funs(mean))

## Baixar o dado do PIB e agregar em frame para regressão

pib <- rbcb::get_series(code = list("pib" = 22109)) %>%
  
  dplyr::mutate(quarter = lubridate::quarter(date, with_year = TRUE)) %>%
  
  dplyr::select(quarter, pib)

dados_reg <- dplyr::inner_join(pib, dados_3m) %>%
  
  dplyr::mutate(tri = sub('.*\\.', '', quarter) %>% as.factor())

reg_treino <- dados_reg %>% dplyr::filter(quarter < "2016.2")

reg_teste <- dados_reg %>% dplyr::filter(quarter >= "2016.2")

## Rodar modelo e observar ajuste

modelo <- lm(pib ~ PC1 + PC2 + PC3 + PC4, data = reg_treino)

modelo_tbl <- sweep::sw_augment(modelo) %>%
  
  dplyr::select(`PIB Observado` = pib, `Modelo` = .fitted) %>%
  
  dplyr::mutate(quarter = reg_treino$quarter)

modelo_tbl %>% tidyr::gather(key = var, value = valor, -quarter) %>%
  
  dplyr::mutate(var = factor(var, levels = c("PIB Observado", "Modelo"))) %>%
  
  ggplot(aes(x = quarter, y = valor, linetype = var, color = var), group = 1) + 
  
  geom_line(lwd = 1) +
  
  labs(title = "Produto Interno Bruto",
       subtitle = "Índice",
       color = "",
       linetype = "",
       x = "",
       y = "") +
  
  theme_get()

## Gerar as previsões e acurácia

modelo_fc <- forecast(modelo, newdata = reg_teste)

accuracy(modelo_fc, pib$pib[82:90])

dados_fora <- reg_teste %>% 
  
  dplyr::mutate(Nowcast = round(modelo_fc$mean,2)) %>% 
  
  dplyr::mutate(quarter = gsub("\\.", " Q", quarter)) %>%
  
  dplyr::select(quarter, PIB = pib, Nowcast)
  
  
dados_fora %>% tidyr::gather(key = var, value = valor, -quarter) %>%
  
  ggplot(aes(x = quarter, y = valor, color = var, group = var)) + 
  
  geom_line(lwd = 1) +
  
  labs(x = "", y = "",
       color = "",
       title = "Nowcast do Produto Interno Bruto",
       subtitle = "Índice")
