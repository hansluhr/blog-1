### Script da publicação de 11-06-2018 ###
### http://www.rleripio.com.br/blog
### Autor: João Renato Leripio
### Dúvidas e sugestões: contato@rleripio.com.br

### 1. Configuração preliminares

### 1.1. Carregar bibliotecas necessárias (caso não estejam instaladas, é preciso fazê-lo.)

library(tidyverse) # Coleção de funções para data science;
library(sidrar)    # Baixar dados do SIDRA/IBGE

### 1.2. Definir diretório de trabalho

setwd("C:\\Users\\36277\\Desktop\\Posts\\Junho")

### 2. Importar os dados da PNADC/T (IBGE) ####

dados_pnad <- get_sidra(api = "/t/4093/n1/all/v/1641,4090,4096,4099/p/all/c2/6794/d/v4096%201,v4099%201")

### 2.1 Selecionar apenas as colunas que interessam: Variável, Trimestre (Código), Valor

dados_pnad <- dados_pnad %>% dplyr::select(c(`Trimestre (Código)`, Variável, Valor))

### 3. Criar os termos da equação

## A equação que decompõe a taxa de desocupação é dada por: 
## d_TD = -(1-TD)d_PO/PO + (1-TD)d_PIA/PIA + (1-TD)d_TA/TA

## Do lado direito da equação, já temos TD, PIA e TA; Precisamos das suas diferenças;
## Vamos colocar o frame no formato "wide" (Variáveis como colunas), isso vai facilitar o processo;

dados_pnad_wide <- dados_pnad %>% tidyr::spread(key = "Variável", value = "Valor")

## Vamos renomear as colunas para facilitar a manipulação

colnames(dados_pnad_wide) <- c("Trimestre", "PIA", "PO", "TD", "TA")

## Agora vamos criar os termos de que precisamos: as diferenças das variáveis e o termo (1-TD) que
## multiplica os demais termos. Este último vou chamar de peso;

q <- 1

termos <- dados_pnad_wide %>% dplyr::mutate(d_TD = TD - lag(TD, q),
                                            d_PIA = PIA - lag(PIA, q),
                                            d_PO = PO - lag(PO, q),
                                            d_TA = TA - lag(TA, q),
                                            PIA_q = lag(PIA, q),
                                            PO_q = lag(PO, q),
                                            TA_q = lag(TA, q),
                                            peso = (1-(TD/100))
                                            )

## Agora, vamos remover as linhas que não contém valores por conta da diferenciação

termos <- termos %>% tidyr::drop_na()

## Agora podemos criar cada termo do lado direito da equação, os quais chamaremos pelo prefixo "e_*"

termos_e <- termos %>% dplyr::mutate(e_pia = (peso*(d_PIA/PIA_q))*100,
                                     e_po = -(peso*(d_PO/PO_q))*100,
                                     e_ta = (peso*(d_TA/TA_q))*100
                                     )

## Vamos, por fim, selecionar apenas as variáveis que interessam (os temos propriamente ditos da equação)

termos_eq <- termos_e %>% 
  
  dplyr::select(c("Trimestre", "d_TD", PIA = e_pia, PO = e_po, TP = e_ta)) %>%
  
  dplyr::mutate_at(vars(PIA:TP), funs(round(., 1)))

### 4. Plotar os resultados 

## Aqui é mais conveniente colocar as variáveis que representam os termos da equação no formato "long",
## isto é, empilhadas em uma única coluna. O ggplot lida melhor com dados assim.

## Entretanto, vamos deixar separadas as colunas para o Trimestre e para d_TD;
## Isto porque eu quero colocar as contribuições de cada termo como barras e depois
## colocar uma linha para a variação total do desemprego. 

termos_eq %>% 
  
  tidyr::gather(key = Variável, value = Valor, -c(Trimestre, d_TD)) %>%
  
  dplyr::filter(Trimestre >= "201701") %>%
  
  ggplot(aes(x = Trimestre, y = Valor, fill = Variável)) + 
  
  geom_col(position = "dodge") +
  
  geom_line(aes(x = Trimestre, y = d_TD), group = 1, lwd = 1) +
  
  labs(title = "Decomposição da taxa de desocupação",
       subtitle = paste("Variação em", q, "trimestre (s) - p.p", sep = " "), 
       x = "", y = "", fill = "") +
  
  theme_light() +
  
  theme(legend.position = "bottom")

## Criar uma tabela

termos_eq %>% dplyr::filter(Trimestre >= "201701")
