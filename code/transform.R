library(tidyverse)
library(readxl)
library(here)

source(here::here("code/crawler.R"))

transform_processos_stf <- function(processos_datapath = here::here("data/input/processos_tramitacao.csv")) {
  processos_stf <- read_csv(processos_datapath) %>% 
    head(10)
  
  stf_lista <- processos_stf %>% 
    distinct(Link, Url) %>% 
    rowwise() %>% 
    mutate(id_incidente = as.character(urltools::param_get(Url, "incidente"))) %>% 
    mutate(url = paste0("http://portal.stf.jus.br/processos/detalhe.asp?incidente=", id_incidente))
  
  partes <- tibble(url = stf_lista$url) %>%
    mutate(dados = map(
      url,
      fetch_todas_partes
    )) %>% 
    unnest(dados)
  
  partes_wide <- partes %>% 
    group_by(url, parte) %>% 
    summarise(nomes = paste0(nome, collapse = ";")) %>% 
    ungroup() %>% 
    distinct(url, parte, nomes) %>% 
    spread(key = parte, value = nomes)
  
  decisoes <- tibble(url = stf_lista$url) %>%
    mutate(dados = map(
      url,
      fetch_decisoes
    )) %>% 
    unnest(dados) %>% 
    mutate(decisao = "decisoes")
  
  decisoes_wide <- decisoes %>% 
    mutate(value = paste0("Data: ", data, " Nome: ", nome, 
                          " Texto: ", texto, " Julgador: ", julgador)) %>% 
    group_by(url, decisao) %>% 
    summarise(decisoes = paste0(value, collapse = ";")) %>% 
    ungroup() %>% 
    distinct(url, decisao, decisoes) %>% 
    spread(key = decisao, value = decisoes)
  
  detalhes <- tibble(url = stf_lista$url) %>%
    mutate(dados = map(
      url,
      fetch_detalhes
    )) %>% 
    unnest(dados)
  
  detalhes_wide <- detalhes %>% 
    group_by(url, papel) %>% 
    summarise(nomes = paste0(nome_papel, collapse = ";")) %>% 
    ungroup() %>% 
    distinct(url, papel, nomes) %>% 
    spread(key = papel, value = nomes)
  
  processos_stf_merge <- processos_stf %>% 
    rowwise() %>% 
    mutate(id_incidente = as.character(urltools::param_get(Url, "incidente"))) %>% 
    mutate(url = paste0("http://portal.stf.jus.br/processos/detalhe.asp?incidente=", id_incidente)) %>% 
    select(-c(`Origem:`, `Relator:`, `Redator do acórdão:`, `Relator do último incidente:`, `REQTE.(S)`, `ADV.(A/S)`, `INTDO.(A/S)`, `PROC.(A/S)(ES)`)) %>% 
    left_join(partes_wide, by = "url") %>% 
    left_join(decisoes_wide, by = "url") %>% 
    left_join(detalhes_wide, by = "url")

  return(processos_stf_merge)  
}


