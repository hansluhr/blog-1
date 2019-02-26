
setwd("C:\\Users\\36277\\Desktop\\Posts\\2019\\Fevereiro")

### 1. Importar pacotes necessários

library(tidyverse)
library(rbcb)
library(forecast)

### 2. Importar dados do IPCA por componentes

dados <- rbcb::get_series(code = list("Serviços" = 10844,
                                      "Industriais" = 27863,
                                      "Alimentação no domicílio" = 27864),
                          
                          start_date = "2007-01-01") %>% 
  
  purrr::reduce(inner_join) %>% 
  
  dplyr::select(-date)

### 3. Gerar versões bootstrap para cada série

n <- 50

dados_ts <- purrr::map(.x = dados,
                       .f = function(x) ts(x, start = c(2007, 01), freq = 12))
             

dados_boot <- purrr::map(.x = dados_ts,
                         .f = forecast::bld.mbb.bootstrap, num = n)

### 4. Gerar previsões um passo à frente para cada série de cada categoria

dados_boot_fc <- purrr::modify_depth(.x = dados_boot, 
                                     .depth = 2, 
                                     .f = function(x){forecast(x, h = 1)$mean})
  
dados_boot_fc <- dados_boot_fc %>% as_tibble() %>% dplyr::mutate_all(funs(unlist)) 

### 5. Gerar todas as somas possíveis

index <- expand.grid(ind_1 = 1:n, 
                     ind_2 = 1:n, 
                     ind_3 = 1:n) %>% as.list()

prev <- purrr::pmap_dfr(.l = index, 
                        
.f = function(ind_1,ind_2,ind_3){
                          
dados_boot_fc[ind_1, "Serviços"]*(35.2/72.8) + dados_boot_fc[ind_2, "Industriais"]*(21.9/72.8) + dados_boot_fc[ind_3, "Alimentação no domicílio"]*(15.7/72.8)
                          
})


ggplot(data = prev, aes(x = `Serviços`)) + 
  
  geom_density(lwd = 1, fill = "steelblue4", color = "steelblue3", alpha = 0.5) +
  
  theme_light() +
  
  labs(x = "", y = "", 
       title = "Densidade das projeções para o IPCA dos preços livres") +
  
  theme(axis.text.y = element_blank())

                          