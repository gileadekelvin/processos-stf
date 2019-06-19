library(tidyverse)
library(here)

source(here("crawler.R"))

processos_stf <- read_csv(here("processos_stf.csv"))

stf_lista <- processos_stf %>% 
  distinct(`Link Processo`, link) %>% 
  rowwise() %>% 
  mutate(id_incidente = as.character(urltools::param_get(link, "incidente"))) %>% 
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

processos_stf_merge <- processos_stf %>% 
  rowwise() %>% 
  mutate(id_incidente = as.character(urltools::param_get(link, "incidente"))) %>% 
  mutate(url = paste0("http://portal.stf.jus.br/processos/detalhe.asp?incidente=", id_incidente)) %>% 
  select(-c(`REQTE.(S)`, `ADV.(A/S)`, `INTDO.(A/S)`, `PROC.(A/S)(ES)`, `REQTE.(S)_1`, `Decisões`)) %>% 
  left_join(partes_wide, by = "url") %>% 
  left_join(decisoes_wide, by = "url") %>% 
  select(Classe, `Número`, `Link Processo`, link, `ADV.(A/S)`:`decisoes`, dplyr::everything())

