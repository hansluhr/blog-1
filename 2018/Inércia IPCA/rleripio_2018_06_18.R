### Script da publicação de 18-06-2018 ###
### http://www.rleripio.com.br/blog
### Autor: João Renato Leripio
### Dúvidas e sugestões: contato@rleripio.com.br

#### 1. Configurações preliminares ####

### 1.1. Carregar bibliotecas necessárias (caso não estejam instaladas, é preciso fazê-lo.)

library(tidyverse) # Coleção de funções para data science;
library(sidrar)    # Baixar dados do SIDRA/IBGE
library(tibbletime)
library(urca)
library(scales)

### 1.2. Definir diretório de trabalho

setwd("C:\\Users\\36277\\Desktop\\Posts\\Junho")

### 2. Importar os dados do IPCA mensal dessazonalizado (IBGE) ####

dados_ipca <- get_sidra(api = "/t/118/n1/all/v/all/p/all/d/v306%202")

dados_ipca <- dados_ipca %>% dplyr::select(c(`Mês (Código)`, Valor))

dados_ipca$Data <- dados_ipca$`Mês (Código)` %>% paste("01") %>% lubridate::ymd()

### 3. Criar função com janela móvel para calcular o coeficiente "p" da equação do teste dickey-fuller

k <- 60

ur_roll <- rollify(~ (summary(ur.df(.x, selectlags = "BIC"))@testreg$coefficients[1]+1),
                   window = k, unlist = TRUE)

#### 4. Aplicar a função criada sobre os dados do IPCA

dados_sarc <- dados_ipca %>% dplyr::mutate(sarc = ur_roll(Valor))

#### 5. Plotar os valores

dados_sarc %>% 
  
  dplyr::filter(Data >= "2002-01-01") %>%
  
  ggplot(aes(x = Data, y = sarc, group = 1)) +
  
  geom_line(lwd = 1.0) + 
  
  geom_smooth(se = FALSE) +
  
  scale_x_date(breaks = date_breaks(paste('12',"months",sep=" ")),
               labels = date_format("%b/%Y")) +
  
  labs(title = "Persistência inflacionária - SARC",
       subtitle = paste("janela móvel de", k, "meses", sep = " "),
       color = "",
       x = "",
       y = "") +
  
  theme_light()