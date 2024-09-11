###tratamento bancos sifilis

#pacotes ----
library(rio)
library(tidyverse)
library(stringr)

#banco ----
sifilisAdquirida <- import("E:/Bianca/Sifilis/SifilisAdquirida.dbf")
sifilisCongenita <- import("E:/Bianca/Sifilis/SifilisCongênita.dbf")
sifilisGestante <- import("E:/Bianca/Sifilis/SifilisGestante.dbf")

#tratamento ----
ID_AGRAVO

## classificação final ----
sifilis <- sifilis %>% 
  mutate(classificacao = recode(CLASSI_FIN,
                                "1" = "Confirmado",
                                "2" = "Descartado"))

sifilis <- sifilis %>% 
  mutate(classificacao = replace_na(classificacao, "Suspeito"))


## evolução ----
sifilis <- sifilis %>% 
  mutate(evolucao = recode(EVOLUCAO,
                           "1" = "Cura",
                           "2" = "Óbito por sifilisspirose",
                           "3" = "Óbito por outras causas",
                           "9" = "ignorado"))


## semana de notificação (SEM_NOT/SEM_PRI) ----
sifilis <- sifilis %>%
  mutate(semanaNot = substr(SEM_NOT, 5,6),
         anoNot = substr(SEM_NOT, 1,4))


## idade/faixa etária ----
#idade
sifilis$idade <- as.numeric((sifilis$DT_SIN_PRI - sifilis$DT_NASC) /365)

#faixa etária 
sifilis <- sifilis %>% 
  mutate(faixa_etaria = age_categories(idade, 
                                       lower=0, 
                                       upper=70, 
                                       by=10))


## semana epiemiológica ----
sifilis <- sifilis %>% 
  mutate(epi_semana = floor_date(DT_SIN_PRI, unit= "week"),
         Ano = floor_date(DT_SIN_PRI, unit = "year"),
         ano = substr(Ano, 1, 4),
         DT_SIN_PRI = as.Date(DT_SIN_PRI))


## raca ----
sifilis <- sifilis %>% 
  mutate(raca_cat = recode(CS_RACA,
                           # for reference: OLD = NEW
                           "1" = "Branca",
                           "2" = "Preta",
                           "3" = "Amarela",
                           "4" = "Parda",
                           "5" = "Indígena",
                           "9" = "Ignorado"))


## sexo ----
sifilis <- sifilis |> 
  mutate(CS_SEXO = replace(CS_SEXO, CS_SEXO== "I",NA))                                    


sifilis <- sifilis %>% 
  mutate(sexo = recode(CS_SEXO,
                       # for reference: OLD = NEW
                       "F" = "Feminino",
                       "M" = "Masculino"))


## escolaridade ----
sifilis <- sifilis %>% 
  mutate(escola = recode(CS_ESCOL_N,
                         # for reference: OLD = NEW
                         "00" = "Analfabeto",
                         "01" = "1 a 4 série incompleta",
                         "02" = "Até a 4 série completa",
                         "03" = "4 a 8 série incompleta",
                         "04" = "Ensino fundamental completo",
                         "05" = "Ensino Médio incompleto",
                         "06" = "Ensino Médio completo",
                         "07" = "EnsinoSuperior incompleto",
                         "08" = "Ensino Superior completo",
                         "09" = "Ignorado",
                         "10" = "Não se aplica"))


## gestante ----
sifilis <- sifilis %>% 
  mutate(gestante = recode(CS_GESTANT,
                           # for reference: OLD = NEW
                           "1" = "1 trimestre",
                           "2" = "2 trimestre",
                           "3" = "3 trimestre",
                           "4" = "Idade gestacional ignorada",
                           "5" = "Não",
                           "6" = "Não se aplica",
                           "9" = "Ignorado"))


