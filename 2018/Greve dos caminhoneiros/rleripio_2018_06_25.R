### Script da publicação de 25-06-2018 ###
### http://www.rleripio.com.br/blog
### Autor: João Renato Leripio
### Dúvidas e sugestões: contato@rleripio.com.br

#### 1. Configurações preliminares

### 1.1. Carregar bibliotecas necessárias (caso não estejam instaladas, é preciso fazê-lo.)

library(tidyverse) # Coleção de funções para data science;
library(rbcb)      # Baixar dados do BCB

### 1.2. Definir diretório de trabalho

setwd("C:\\Users\\36277\\Desktop\\Posts\\Junho")

### 1.3. Importar os dados de expectativas: IPCA, Câmbio, Juros, Atividade e Dívida líquida

vars <- c("IPCA", "PIB Total", "Taxa de câmbio", "Meta para taxa over-selic", "Fiscal")

dados_bcb <- rbcb::get_annual_market_expectations(vars, end_date = "2018-07-04")

dados_sel <- dados_bcb %>% 
  
  dplyr::filter(reference_year == "2018", 
                indic_detail %in% c(NA, "Fim do ano", "Dívida líquida do setor público")) %>%
  
  dplyr::select(c(Data = "date", var = "indic", valor = "mean")) %>%
  
  tidyr::spread(key = var, value = valor) %>%
  
  dplyr::arrange(Data) %>%
  
  dplyr::filter(lubridate::year(Data) == "2018") %>%
  
  dplyr::mutate(trend = seq(from = 1, to = n(), by = 1))

#### 2. Visualizar os dados

dados_sel %>% 
  
  tidyr::gather(key = var, value = valor, 
                -Data, -trend) %>% 
  
  ggplot(aes(x = Data, y = valor, color = var)) + 
  
  geom_line(lwd = 1.0) +
  
  facet_wrap(~ var, scales = "free") +
  
  theme_light() +
  
  theme(legend.position = "none") +
  
  labs(x = "", y = "",
       title = "Expectativa de mercado para 2018 (%)",
       subtitle = "Focus/BCB 01-01-2018 a 11-06-2018") 

#### 4. Ajustar modelo

modelo <- lm(IPCA ~ trend + log(`Taxa de câmbio`), data = dados_sel)

#### 5. Plotar dados ajustados e observados

modelo_fit <- tibble(Data = dados_sel$Data,
                     Observado = dados_sel$IPCA,
                     Ajustado = modelo$fitted.values,
                     Resíduos = round(modelo$residuals, 2))

modelo_fit %>% tidyr::gather(key = var, value = valor, -Data, -Resíduos) %>%
  
  ggplot(aes(x = Data, y = valor, color = var)) + 
  
  geom_line(lwd = 1) +
  
  theme_light() +
  
  geom_vline(xintercept = as.Date("2018-05-21"), lty = 2) +
  
  annotate("text", x = as.Date("2018-05-16"), y = 3.65, label = "Greve") +
  
  labs(x = "", y = "", color = "",
       title = "Resultado do modelo para expectativa do IPCA",
       subtitle = "Expectativas para 2018 (%)")