### Script para projeção de taxas de participação por grupo
### Contato: joao.gomes@codeplan.df.gov.br

### 1. Preliminar ----

setwd("W:\\Núcleo de análises econômicas\\Projeção TP")

library(tidyverse)
library(readxl)
library(zoo)

### 2. Importar arquivos ----

pea_pia <- readxl::read_excel("Brasil/pea_pia_BR_2.xlsx")

proj_ibge_h <- readxl::read_excel("Brasil/proj_pop_ibge_BR.xlsx", sheet = "Homens")

proj_ibge_m <- readxl::read_excel("Brasil/proj_pop_ibge_BR.xlsx", sheet = "Mulheres")

### 3. Adequar os dados ----

## Criar a variável theta_ig

pea_pia_aux <- pea_pia %>%
  
  dplyr::mutate(Ano = 2018) %>%
  
  dplyr::select(Ano, everything()) %>%
  
  dplyr::group_by(Ano, Sexo)

## Ajustar os dados de projeção para conformar com as colunas de pea_pia_aux

proj_ibge_h_aux <- proj_ibge_h %>% 
  
  tidyr::gather(key = Faixa, value = PIA, -Ano) %>%
  
  dplyr::mutate(Sexo = "Homem")

proj_ibge_m_aux <- proj_ibge_m %>% 
  
  tidyr::gather(key = Faixa, value = PIA, -Ano) %>%
  
  dplyr::mutate(Sexo = "Mulher")

proj_ibge <- dplyr::bind_rows(proj_ibge_h_aux, proj_ibge_m_aux) %>%
  
  dplyr::group_by(Ano, Sexo) %>%
  
  dplyr::filter(Ano > 2018) %>%
  
  dplyr::group_by(Faixa, add = T) %>%
  
  dplyr::summarise(PIA = sum(PIA)) %>%
  
  dplyr::ungroup() %>%
  
  dplyr::group_by(Ano, Sexo) %>%
  
  dplyr::ungroup() %>%
  
  dplyr::group_by(Ano)

dados <- dplyr::bind_rows(pea_pia_aux, proj_ibge)

### 5. Cenário 1 - Taxas de participação constantes ----

## Cenário 1: Taxa de participação de cada grupo permanece constante e PEA é obtida por resíduo.

dados_cen1 <- dados %>%
  
  dplyr::mutate(TP = (PEA/PIA)) %>%
  
  dplyr::group_by(Sexo, Faixa) %>%
  
  dplyr::mutate(TP = ifelse(is.na(TP), zoo::na.locf(TP), TP)) %>%
  
  dplyr::mutate(PEA = ifelse(is.na(PEA), TP*PIA, PEA)) %>%
  
  dplyr::group_by(Ano) %>%
  
  dplyr::mutate(PIA_total = sum(PIA)) %>%
  
  dplyr::mutate(theta = (PIA/PIA_total)) %>%
  
  dplyr::group_by(Sexo, Faixa) %>%
  
  dplyr::mutate(d_theta = (theta - dplyr::lag(theta,1)),
                
                d_TP = (TP - dplyr::lag(TP,1)),
                
                TP_l1 = dplyr::lag(TP,1),
                
                theta_l1 = dplyr::lag(theta,1)) %>%
  
  dplyr::mutate(EC = d_theta*TP_l1*100,
                
                EP = d_TP*theta_l1*100) %>%
  
  dplyr::ungroup()

cen_1 <- dados_cen1 %>%
  
  dplyr::group_by(Ano) %>%
  
  dplyr::summarise(EC = sum(EC)) %>%
  
  dplyr::ungroup() %>%
  
  dplyr::filter(Ano >= 2020) %>%
  
  dplyr::mutate("EC acumulado" = cumsum(dplyr::coalesce(EC, 0)))

### Gráfico 1: contribuição agregada ----

cen_1 %>%
  
  tidyr::drop_na() %>%
  
  dplyr::filter(Ano <= 2040) %>%
  
  ggplot(aes(x = lubridate::make_date(year = Ano))) + 
  
  geom_col(aes(y = `EC acumulado`), fill = "steelblue3") +
  
  labs(title = "Efeito acumulado da composição demográfica sobre a taxa de participação (p.p)",
       subtitle = "Hipótese: taxas de participação constantes no nível de 2018Q4",
       caption = "Elaboração: rleripio.com.br com dados da PNAD/IBGE",
       x = "", y = "") +
  
  theme_minimal() +
  
  geom_hline(yintercept = 0, size = 1) +
  
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  
  ylim(c(-6.5,0))
  
### Gráfico 2: Contribuição por faixa e  sexo ----

dados_cen1 %>%
  
  dplyr::filter(Ano >= 2020) %>%
  
  dplyr::group_by(Sexo, Faixa) %>%
  
  dplyr::mutate(EC_acum = cumsum(EC)) %>%
  
  ggplot(aes(x = Ano, y = EC_acum, color = Sexo)) + 
  
  geom_line(lwd = 1) +
  
  scale_color_manual(values = c("steelblue3", "orange")) +
  
  geom_hline(yintercept = 0, lwd = 1) +
  
  theme(legend.position = "top", legend.text = element_text(size = 15)) +
  
  facet_wrap(~ Faixa) +
  
  labs(title = "Contribuição acumulada para a taxa de participação por faixa etária e sexo (p.p) - Brasil",
       subtitle = "Hipótese: taxas de participação constantes no nível de 2018T4",
       x = "", y = "", color = "")

### Gráfico 3: Evolução da POP por faixa e Sexo ----

dados_cen1 %>%
  
  dplyr::filter(Ano >= 2020) %>%
  
  dplyr::group_by(Sexo, Faixa) %>%
  
  ggplot(aes(x = Ano, y = PIA/1000, color = Sexo)) + 
  
  geom_line(lwd = 1) +
  
  scale_color_manual(values = c("steelblue3", "orange")) +
  
  theme(legend.position = "top", legend.text = element_text(size = 15)) +
  
  facet_wrap(~ Faixa, scales = "free") +
  
  labs(title = "População por faixa etária e sexo (p.p) - Brasil",
       subtitle = "Estimativas do IBGE",
       x = "", y = "x1000", color = "")

### Gráfico 4: Evolução da PEA por faixa etária ----

dados_cen1 %>%
  
  dplyr::filter(Ano >= 2020) %>%
  
  dplyr::group_by(Sexo, Faixa) %>%
  
  ggplot(aes(x = Ano, y = PEA/1000, color = Sexo)) + 
  
  geom_line(lwd = 1) +
  
  scale_color_manual(values = c("steelblue3", "orange")) +
  
  theme(legend.position = "top", legend.text = element_text(size = 15)) +
  
  facet_wrap(~ Faixa, scales = "fixed") +
  
  labs(title = "PEA por faixa etária e sexo (p.p) - Brasil",
       subtitle = "Hipótese: taxas de participação constantes no nível de 2018T4",
       x = "", y = "x1000", color = "")


### Gráfico 5: Taxa de participação por faixa e sexo ----

dados_cen1 %>%
  
  dplyr::filter(Ano == 2018) %>%
  
  ggplot(aes(x = Faixa, y = TP*100, fill = Sexo)) + 
  
  geom_col(lwd = 1, position = position_dodge()) +
  
  scale_fill_manual(values = c("steelblue3", "orange")) +
  
  
  labs(title = "Taxa de participação por faixa etária e sexo (%) - Brasil",
       subtitle = "2018T4",
       x = "", y = "", fill = "") +
  
  ylim(c(0, 100)) +
  
  theme_minimal() +
  
  theme(legend.position = "top")

## Observar diferenças

dados_cen1 %>% 
  
  dplyr::filter(Ano == 2018) %>% 
  
  dplyr::group_by(Sexo, Faixa) %>% 
  
  dplyr::summarise(Mean = mean(TP*100, na.rm=T)) %>% 
  
  tidyr::spread(key = Sexo, value = Mean) %>%
  
  dplyr::mutate(Diferença = Homem - Mulher)

