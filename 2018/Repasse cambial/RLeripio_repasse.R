### Script para estimar o pass-through cambial a partir da CP ###
### Autor: João Renato Leripio - www.rleripio.com.br ###

#### 1. Configurações preliminares ####

setwd("C:\\Users\\36277\\Desktop\\Posts\\Novembro")

## 1.1 Carregar pacotes

library(devtools)
library(tidyverse)
library(lubridate)
library(readxl)
library(restriktor)
library(broom)
library(scales)

devtools::install_github('wilsonfreitas/rbcb')

library(rbcb)

#### 2. Importar dados ####

## 2.1 Importar dados de expectativas de inflação

exp_ipca <- rbcb::get_monthly_market_expectations("IPCA", end_date = "2018-09-31")


diff_quarter <- function(end_date, start_date){
  
  year_to_quarters <- (floor(end_date)*4 + (end_date %% 1)*10) - (floor(start_date)*4 + (start_date %% 1)*10)
  
  return(year_to_quarters)
  
}

exp_ipca_aux <- exp_ipca %>%
  
  dplyr::select(date, reference_month, median) %>%
  
  dplyr::mutate(reference_month = lubridate::ymd(paste(reference_month, "01", sep = "-"))) %>%
  
  dplyr::mutate(date_year = lubridate::year(date),
                date_month = lubridate::month(date)) %>%
  
  dplyr::group_by(date_year, date_month, reference_month) %>%
  
  dplyr::summarise(median_month = mean(median)) %>%
  
  dplyr::ungroup() %>%
  
  dplyr::mutate(date = lubridate::make_date(year = date_year, month = date_month)) %>%
  
  dplyr::select(date, reference_month, median_month) %>%
  
  dplyr::filter(date > "2001-12-01") %>%
  
  dplyr::mutate(ref_quarter = lubridate::quarter(reference_month, with_year = T)) %>%
  
  dplyr::group_by(date, ref_quarter) %>%
  
  dplyr::summarise(median_quarter = last(((cumprod(1+(median_month/100)))-1)*100)) %>%
  
  dplyr::mutate(date_quarter = lubridate::quarter(date, with_year = T)) %>%
  
  dplyr::group_by(date_quarter, ref_quarter, add = F) %>%
  
  dplyr::summarise(median_quarter = mean(median_quarter)) %>%
  
  dplyr::filter(ref_quarter > date_quarter) %>%
  
  dplyr::mutate(diff = round(diff_quarter(ref_quarter, date_quarter)),1) %>%
  
  dplyr::select(-ref_quarter) %>%
  
  tidyr::spread(key = diff, value = median_quarter)

colnames(exp_ipca_aux)[-1] <- paste("EInf_t+", colnames(exp_ipca_aux)[-1], sep = "")

## Importar dados do BCB: ipca livre, ipca total e IC

series <- list("ipca_livres" = 11428,
               "ipca_total" = 433)

acum_quarter <- function(x){
  
  x_fac <- 1+(x/100)
  
  x_cum <- RcppRoll::roll_prodr(x_fac, n = 3)
  
  x_qr <- last((x_cum-1)*100)
  
  return(x_qr)
  
}

acum_ic <- function(x){
  
  x_diff <- log(x/first(x))*100
  
  x_acum <- last(x_diff)
  
  return(x_acum)
  
}

dados_ipca <- rbcb::get_series(series) %>% 
  
  purrr::reduce(inner_join) %>%
  
  dplyr::mutate(date_quarter = lubridate::quarter(date, with_year = TRUE)) %>%
  
  dplyr::group_by(date_quarter) %>%
  
  dplyr::summarise_at(vars(ipca_total, ipca_livres), funs(acum_quarter))


dados_ic <- rbcb::get_series(list("ic_br" = 27574)) %>%
  
  dplyr::mutate(date_quarter = lubridate::quarter(date, with_year = TRUE)) %>%
  
  dplyr::group_by(date_quarter) %>%
  
  dplyr::summarise_at(vars(ic_br), funs(acum_ic))

## Importar dados de hiato

hiato <- read_excel("hiato.xlsx") %>%
  
  dplyr::mutate(date_quarter = as.numeric(gsub(" T", ".", date_qr)),
                hiato = hiato*100) %>%
  
  dplyr::select(date_quarter, hiato)

## Reunir dados

dados_reg <- dplyr::inner_join(dados_ipca, dados_ic) %>% 
  
  dplyr::inner_join(hiato) %>% 
  
  dplyr::inner_join(exp_ipca_aux) %>%
  
  dplyr::mutate(quarter = sub('.*\\.', '', date_quarter)) %>%
  
  dplyr::filter(date_quarter >= 2002.1) %>%
  
  dplyr::mutate(ipca_l1 = lag(ipca_total, 1),
                ipca_l2 = lag(ipca_total, 2),
                hiato_l1 = lag(hiato, 1),
                hiato_l2 = lag(hiato, 2),
                hiato_l3 = lag(hiato, 3),
                ic_l1 = lag(ic_br,1),
                ic_l2 = lag(ic_br, 2),
                Einf_1 = `EInf_t+1`,
                Einf_2 = `EInf_t+2`)

## Modelo

modelo_unr <- lm(ipca_livres ~ -1 + ipca_l1 + ipca_l2 + Einf_1 + hiato_l3 + ic_l1 + quarter, data = dados_reg)

unr_tidy <- broom::tidy(modelo_unr)

modelo_res <- restriktor(modelo_unr, constraints = ' ipca_l1 + ipca_l2 + Einf_1 + ic_l1 == 1 ')

## Criar gráficos dos coeficientes

res_tidy <- tibble(term = unr_tidy$term,
                   estimate = modelo_res$b.restr,
                   std.error = summary(modelo_res)$coefficients[, 2],
                   low = estimate - 1.64*std.error,
                   high = estimate + 1.64*std.error)


res_tidy %>% dplyr::filter(!term %in% c("quarter1", "quarter2", "quarter3", "quarter4")) %>%
  
ggplot(aes(estimate, term, xmin = low, xmax = high, height = 0)) +
  
  geom_point() +
  
  geom_vline(xintercept = 0) +
  
  geom_errorbarh() +
  
  labs(x = "",
       y = "",
       title = "Coeficientes estimados da Curva de Phillips",
       subtitle = "Modelo com restrição de verticalidade") +
  
  scale_x_continuous(breaks = seq(-0.5,1,0.25))

## Gráfico do ajuste

acum_4 <- function(x){
  
  x_fac <- 1+(x/100)
  
  x_acum <- RcppRoll::roll_prodr(x_fac, 4)
  
  x_fim <- (x_acum-1)*100
  
  return(x_fim)
  
}

unr_aug <- broom::augment(modelo_unr) %>% dplyr::mutate(res_fit = modelo_res$fitted[,1],
                                                        date = dados_reg$date_quarter[4:64]) %>%
  
  dplyr::select(date, "IPCA Livres" = ipca_livres, "Ajustado" = res_fit) %>%
  
  dplyr::mutate_at(vars(-date), funs(acum_4))
  
unr_aug %>%
  
  tidyr::gather(key = var, value = valor, -date) %>%
  
  ggplot(aes(x = date, y = valor, color = var)) + 
  
  geom_line(lwd = 1) +
  
  labs(x = "", y = "",
       title = "Valores ajustados pelo modelo",
       subtitle = "Acumulado em quatro trimestres (%)")
