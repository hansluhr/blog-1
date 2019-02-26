### Programa para replicar o modelo do BC para efeito de segunda ordem
### Autor: J. Renato Leripio - www.rleripio.com.br
### Contato: leripiorenato@gmail.com

#### 1. Configurações preliminares ####

library(tidyverse)
library(sidrar)
library(stringr)
library(RcppRoll)
library(vars)
library(seasonal)
library(timetk)

#### 2. Importar dados

## Alimentação no domicílio e núcleo do IPCA ex-alimento e ex-energia

ipca_itens <- sidrar::get_sidra(api = "/t/1419/n1/all/v/63,66/p/all/c315/7172,7184,7200,7219,7241,7254,7283,7303,7335,7349,7356,7372,7384,7389,7401,7415,7433,7447,7454,7461,7480,7484,7488,7495,7517,7522,7541,7549,7560,7572,7587,7605,7616,7621,7627,7640,7656,7662,7684,7690,7695,7698,7714,7730,7758,7760,7777,7782,7788,12427,107678,109464/d/v63%202,v66%204")

ipca_itens <- sidrar::get_sidra(api = "/t/1419/n6/5300108/v/63,66/p/all/c315/7172,7184,7200,7219,7241,7254,7283,7303,7335,7349,7356,7372,7384,7389,7401,7415,7433,7447,7454,7461,7480,7484,7488,7495,7517,7522,7541,7549,7560,7572,7587,7605,7616,7621,7627,7640,7656,7662,7684,7690,7695,7698,7714,7730,7758,7760,7777,7782,7788,12427,107678,109464/d/v63%202,v66%204")

ipca_itens_aux <- ipca_itens %>%

  dplyr::mutate(Data = lubridate::ymd(paste(`Mês (Código)`, "01")),
                Item = `Geral, grupo, subgrupo, item e subitem`) %>%

  dplyr::select(Data, Variável, Item, Valor)

## Vetor com itens que serão excluídos

ipca_core <- ipca_itens_aux %>%

  dplyr::filter(!str_detect(Item, "^1"),
                !Item %in% c("5104.Combustíveis (veículos)",
                             "2201.Combustíveis (domésticos)")) %>%

  tidyr::spread(key = Variável, value = Valor) %>%

  dplyr::mutate("IPCA" = (`IPCA - Variação mensal`*`IPCA - Peso mensal`)/100) %>%

  dplyr::group_by(Data) %>%

  dplyr::summarise("Núcleo" = sum(IPCA))


ipca_ali <- ipca_itens_aux %>% dplyr::filter(str_detect(Item, "^1")) %>%

  tidyr::spread(key = Variável, value = Valor) %>%

  dplyr::mutate("IPCA" = (`IPCA - Variação mensal`*`IPCA - Peso mensal`)/100) %>%

  dplyr::group_by(Data) %>%

  dplyr::summarise("Alimento" = sum(IPCA))

dados <- dplyr::inner_join(ipca_core, ipca_ali)

## Criar função para gerar valores acumulados

acum <- function(x,n){

  fator <- (1+(x/100))

  prod <- RcppRoll::roll_prodr(fator, n = n)

  fim <- (prod-1)*100

  return(fim)

}

dados_3m <- dados %>%

  dplyr::filter(Data <= "2018-06-01") %>%

  dplyr::mutate(Trimestre = lubridate::quarter(Data, with_year = TRUE)) %>%

  dplyr::group_by(Trimestre) %>%

  dplyr::summarise_at(vars(Núcleo, Alimento), funs(last(acum(., n = 3))))

dados_3m_sa <- dados_3m %>%

  dplyr::mutate_at(vars(Núcleo, Alimento),
                   funs(timetk::tk_ts(., start = c(2012,01), frequency = 4))) %>%

  dplyr::mutate_at(vars(Núcleo, Alimento), funs(final(seas(.))))

## Rodar VAR

modelo_var <- vars::VAR(dados_3m_sa[, -1], lag.max = 3)

modelo_irf <- vars::irf(modelo_var,
                        impulse = "Alimento",
                        response = "Núcleo",
                        n.ahead = 12,
                        cumulative = TRUE)

## Colocar em gráfico melhor

irf_gg <- tibble(N = 0:12,
                 Resposta = modelo_irf$irf$Alimento[,1],
                 Low = modelo_irf$Lower$Alimento[,1],
                 Upp = modelo_irf$Upper$Alimento[,1])

ggplot(irf_gg, aes(x = N, y = Resposta)) +

  geom_line(lwd = 1.2, color = "orange") +

  geom_ribbon(aes(ymin = Low, ymax = Upp), alpha = 0.5, fill = "lightblue2") +

  labs(x = "",
       y = "",
       title = "Resposta acumulada do núcleo a um choque na inflação de alimentos",
       subtitle = "Em p.p da inflação trimestral") +

  scale_x_continuous(breaks = 0:12) +

  theme_minimal()

## Gráfico de barras

irf_gg %>% dplyr::filter(N %in% 1:4) %>%
  
  ggplot(aes(x = N, y = Resposta)) + 
  
  geom_col(fill = "steelblue3") +
  
  labs(x = "Trimestres",
       y = "Efeito sobre o núcleo do IPCA (p.p acumulado)") +
  
  geom_text(aes(label = format(round(Resposta,2), nsmall = 2)), position = position_dodge(0.9), vjust = -0.6) +
  
  theme_minimal() +
  
  theme(axis.text.y = element_blank())
  
