### Função para criar séries acumuladas em n períodos ###
### Autor: João Renato Leripio - joao.gomes@codeplan.df.gov.br ###

acum <- function(x, n){

require(pacman)
  
pacman::p_load(tidyverse, zoo)
    
rollapply(x, width = n, align = "right", FUN = function(y){
  
  last((cumprod(1+(y/100))-1)*100)
   
})

}

