## Script para análise do IPCA 
## Link: http://rleripio.com.br/como-ter-uma-analise-rapida-da-inflacao-em-3-graficos/
## Postado em: 13/08/2018
## Autor: J. Renato Leripio - www.rleripio.com.br / contato@rleripio.com.br

## 1. Configurações iniciais 

## 1.1 Carregar pacotes necessários

library(sidrar)
library(rbcb)
library(tidyverse)
library(gridExtra)

## 1.2 Importar dados IPCA índice geral, variação mensal, acumulado no ano e 12 meses.

dados_ipca <- sidrar::get_sidra(api = "/t/1419/n1/all/v/all/p/last%2012/c315/all/d/v63%202,v66%204,v69%202,v2265%202")

## 2. Gerar gráficos e tabelas 

## 2.1 Gráfico 1: IPCA índice geral mensal, acumulado no ano e em 12 meses.

dados_ipca_geral <- dados_ipca %>% 
  
  dplyr::mutate(Data = lubridate::ymd(paste(`Mês (Código)`, "01", sep = ""))) %>%
  
  dplyr::filter(`Geral, grupo, subgrupo, item e subitem` == "Índice geral",
                Variável != "IPCA - Peso mensal") %>%
  
  dplyr::select(Data, Variável, Valor)

graf_1 <- dados_ipca_geral %>%
  
  dplyr::filter(lubridate::year(Data) == 2018) %>%
  
  ggplot(aes(x = Data, y = Valor)) +
  
  geom_col(aes(alpha = Variável), position = "identity", fill = "seagreen3") +
  
  theme_minimal() +
  
  theme(legend.position = "bottom") +
  
  geom_text(aes(label = Valor), position = position_dodge(0.9)) +
  
  scale_x_date(date_breaks = "1 month", date_labels = "%b/%Y") +
  
  labs(title = "IPCA - Índice Nacional de Preços ao Consumidor Amplo (%)", x = "", y = "", alpha = "")
  
## 2.2. Gráfico 2: IPCA por grupos no mês, acumulado no ano e em 12 meses

dados_ipca_grupos <- dados_ipca %>%
  
  dplyr::filter(!stringr::str_detect(`Geral, grupo, subgrupo, item e subitem`, "\\d{2}"),
                
                Variável != "IPCA - Peso mensal",
                
                `Mês (Código)` == dplyr::last(`Mês (Código)`)) %>%
  
  dplyr::select(Data = `Mês (Código)`, 
                Variável, 
                Grupo = `Geral, grupo, subgrupo, item e subitem`,
                Valor)

graf_2 <- dados_ipca_grupos %>%
  
  dplyr::filter(Grupo != "Índice geral") %>%
  
  ggplot(aes(x = Grupo, y = Valor, fill = Variável)) + 
  
  geom_bar(stat = "identity", position = "dodge") +
  
  theme_light() +
  
  labs(x = "", y = "", title = "IPCA - Índice Nacional de Preços ao Consumidor Amplo por grupos (%)",
       fill = "") +
  
  theme(axis.text.y = element_text(angle = 0, size = 11, face = "bold"),
        axis.text.x = element_blank(),
        legend.position = "bottom") +
  
  geom_text(aes(label = Valor), position = position_dodge(width=0.9), hjust=-0.25) +
  
  coord_flip()
  
## 2.3. Tabela com maiores e menores contribuições mensais

tabela_3 <- dados_ipca %>% 
  
  dplyr::filter(Variável %in% c("IPCA - Variação mensal", "IPCA - Peso mensal"),
                stringr::str_detect(`Geral, grupo, subgrupo, item e subitem`, stringr::regex("(\\d{7})")),
                `Mês (Código)` == dplyr::last(`Mês (Código)`)) %>% 
  
  dplyr::select(`Geral, grupo, subgrupo, item e subitem`, Variável, Valor) %>%
  
  tidyr::spread(key = Variável, value = Valor) %>%
  
  tidyr::drop_na() %>%
  
  dplyr::mutate(Contribuição = (`IPCA - Variação mensal`)*(`IPCA - Peso mensal`/100)) %>%
  
  dplyr::arrange(desc(Contribuição)) %>%
  
  dplyr::slice(c(1:5, (n()-4):n())) %>% 
  
  dplyr::select(`Geral, grupo, subgrupo, item e subitem`, Variação = `IPCA - Variação mensal`, Peso = `IPCA - Peso mensal`, Contribuição) %>%
  
  dplyr::mutate_at(vars(Variação:Contribuição), funs(round(., 2)))

## 3. Extra: reunir os gráficos e tabela em apenas uma imagem

grid.arrange(arrangeGrob(graf_1,tableGrob(tabela_3), ncol=2, nrow=1),
             arrangeGrob(graf_2, ncol=1, nrow=1))

  
  
