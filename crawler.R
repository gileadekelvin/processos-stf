#' @title Recupera todas as partes envolvidas em Ações movidas no STF
#' @description A partir da URL de uma ação movida no STF recupera dados das partes
#' @return Dataframe contendo informações das partes envolvidas: nome da parte, nome
#' @examples
#' partes <- fetch_todas_partes(url)
fetch_todas_partes <- function(url) {
  library(tidyverse)
  library(rvest)
  library(httr)
  
  id <- urltools::param_get(url, "incidente")
  
  url_partes <- paste0("http://portal.stf.jus.br/processos/abaPartes.asp?incidente=", id)
  
  page <- url_partes %>% 
    httr::GET() %>%
    xml2::read_html()
  
  resumo_partes <- page %>% 
    html_nodes("div.processo-partes.lista-dados") %>%
    map(function(x) {
      parte <- x %>% 
        html_nodes("div.detalhe-parte") %>%
        html_text()
      
      nome <- x %>% 
        html_nodes("div.nome-parte") %>%
        html_text()
      
      return(tibble(parte, nome))
    })
  
  partes <- do.call(rbind.data.frame, resumo_partes) %>% 
    mutate(nome = str_replace(nome, "&nbsp", ""))
  
  return(partes)  
}

#' @title Recupera informações das decisões tomadas em Ações movidas no STF
#' @description A partir do URL de uma ação movida no STF recupera todas as decisões tomadas
#' @return Dataframe contendo informações das decisções incluindo data, título, texto e Julgador.
#' @examples
#' deputados <- fetch_decisoes(56)
fetch_decisoes <- function(url) {
  library(tidyverse)
  library(rvest)
  library(httr)
  
  id <- urltools::param_get(url, "incidente")
  
  url_partes <- paste0("http://portal.stf.jus.br/processos/abaDecisoes.asp?incidente=", id)
  
  page <- url_partes %>% 
    httr::GET() %>%
    xml2::read_html()
  
  decisoes <- page %>% 
    html_nodes("div.processo-andamentos.m-t-8") %>%
    map(function(x) {
      data <- x %>% 
        html_nodes("div.andamento-data") %>% 
        html_text()
      
      nome <- x %>% 
        html_nodes("h5.andamento-nome") %>% 
        html_text()
      
      julgador <- x %>% 
        html_nodes("span.andamento-julgador") %>% 
        html_text()
      
      texto <- x %>% 
        html_nodes("div.col-md-9.p-0") %>% 
        html_text()
      
      return(tibble(data, nome, texto, julgador))
    })
  
  decisoes_df <- do.call(rbind.data.frame, decisoes)

  return(decisoes_df)
}
