

# Painel de monitoramento da Sífilis
# Alterado para shiny em 26/06/2024
# Área técnica: GEIST - Carina 
# Mensagem da Carina a bases de dados: sifilis net (é a congênita), sifilis adquirida (notindivi -a539), e sifilis em gestante Gestsifnet
# A GANDT respondeu que o banco nomeado como SIFGNET é o sífilis gestante e o SIFCNEt é o sífilis congênita
# Os óbitos são contabilizados pelo SIM


mycolors <- c("#6969B3",  "#e5590f", "darkblue", "#647494")


# Pacotes -----------------------------------------------------------------



pacman::p_load(
  echarts4r,
  tidyverse,
  stringi,
  xts,
  rio,
  lubridate,
  here,
  stringr,
  foreign,
  fs, 
  sf,
  epikit,
  plotly,
  geobr,
  DT, 
  gtsummary,
  gt,
  shinydashboard,
  shiny,
  leaflet,
  #leaflet.extras2,
  colorspace
  # rgdal
) 




# import ------------------------------------------------------------------

# Para importar dados disponíveis no transferencia interna

  # gestante <- import("Z:/TransferenciaInterna/BaseDBF_SinanNet/SIFGENET.DBF")
  # congenita <- import("Z:/TransferenciaInterna/BaseDBF_SinanNet/SIFICNET.DBF")
  # adquirida <- import("adquirida.dbf")


  # importar dados sinan 
  
  adquirida <- import("SifilisAdquirida.dbf")
  congenita <- import("SifilisCongênita.dbf")
  gestante <- import("SifilisGestante.dbf")
  
  

# filtro residência  -------------------------------------------------


adquirida <- adquirida %>% filter(SG_UF == "42")
congenita <- congenita %>% filter(SG_UF == "42")
gestante <- gestante %>% filter(SG_UF == "42")




# Joins -------------------------------------------------------------------



populacao <- import("populacao.xlsx")  # import população
regiao <- import("regionais.csv") # import região de saúde
regiao <- regiao %>% mutate_if(is.character, toupper) #transforma tudo para letra maiuscula
regiao$NM_MUNICIP <- regiao$NM_MUNICIP %>% stri_trans_general("Latin-ASCII")  #Retira os acentos da variável selecionada
regiao$NM_MUNICIP <- replace(regiao$NM_MUNICIP, regiao$NM_MUNICIP == "HERVAL D'OESTE", "HERVAL D OESTE") #Renomear dois municipíos com apóstlofos
regiao$NM_MUNICIP <- replace(regiao$NM_MUNICIP, regiao$NM_MUNICIP == "SAO MIGUEL D'OESTE", "SAO MIGUEL DO OESTE")

# join população + regiao 
populacao <- left_join(populacao, regiao, by = c("nom_municipio" = "NM_MUNICIP"))
populacao <- populacao %>% select(cod_municipio_ibge,nom_municipio,pop_2010, pop_2022, CD_GEOCMU, reg_saude) # selecionando as variáveis de interesse 

# join  banco população + agravos

adquirida <- left_join(adquirida, populacao, by = c("ID_MN_RESI" = "cod_municipio_ibge"))
congenita <- left_join(congenita, populacao, by = c("ID_MN_RESI" = "cod_municipio_ibge"))
gestante  <- left_join(gestante, populacao,  by = c("ID_MN_RESI" = "cod_municipio_ibge"))


#rename 
adquirida <- adquirida %>% rename(municipio_residencia = nom_municipio, populacao_residente  = pop_2022)
congenita <- congenita %>% rename(municipio_residencia = nom_municipio, populacao_residente  = pop_2022)
gestante  <- gestante  %>% rename(municipio_residencia = nom_municipio, populacao_residente  = pop_2022)




# Campos de datas ----------------------------------------------------


adquirida <- adquirida %>% 
  mutate(DT_SIN_PRI = as.Date(DT_SIN_PRI),
    epi_semana = floor_date(DT_SIN_PRI, unit= "week"), # Data de início da semana epidemiológica
    epi_week = epiweek(DT_SIN_PRI), # Número da semana epidemiológica
    mes_diag = floor_date(DT_SIN_PRI, unit= "month"),
    ano_epi = year(DT_SIN_PRI), 
    ano = substr(SEM_PRI,1,4), 
    Ano = substr(epi_semana, 1, 4)) 


adquirida <- adquirida %>% 
  filter(DT_SIN_PRI >= "2013-01-01")

congenita <- congenita %>% 
  mutate(DT_DIAG = as.Date(DT_DIAG),
          epi_semana = floor_date(DT_DIAG, unit= "week"),
          mes_diag = floor_date(DT_DIAG, unit= "month"),
          ano_epi = year(DT_DIAG),
          ano = substr(SEM_PRI, 1, 4))

gestante <- gestante %>% 
  mutate( DT_DIAG = as.Date(DT_DIAG),
          epi_semana = floor_date(DT_DIAG, unit= "week"),
          mes_diag = floor_date(DT_DIAG, unit= "month"),
          ano = substr(SEM_DIAG, 1, 4))


# faixa etaria ------------------------------------------------------------


# idade congenita
  congenita <- congenita %>% 
  mutate(idade = substr(NU_IDADE_N, 1, 2),
         faixa_etaria = recode(idade,
                               "20" = "< 30 dias",
                               "30" = "> 30 dias e < 1 ano",
                               "40" = "> 1 ano"))

  # criando a variável de idade a partir da data de nascimento
  adquirida$idade <- as.numeric((adquirida$DT_SIN_PRI - adquirida$DT_NASC) /365)
  
  
  # faixa etária 
  adquirida <- adquirida %>% 
    mutate(faixa_etaria = age_categories(idade, 
                                         lower=0, 
                                         upper=60, 
                                         by=20))
  
  


# Classificação final --------------------------------------------------------

adquirida <- adquirida %>% 
  mutate(classificacao = recode(CLASSI_FIN,
                                "1" = "Confirmado",
                                "2" = "Descartado",
                                "8" = "ignorados"))

adquirida <- adquirida %>% 
  mutate(classificacao = replace_na(classificacao, "Suspeito"))



gestante <- gestante %>% 
  mutate(classificacao = recode(CLASSI_FIN,
                                "1" = "Confirmado",
                                "2" = "Descartado"))

gestante <- gestante %>% 
  mutate(classificacao = replace_na(classificacao, "Suspeito"))



## evolução ----


adquirida <- adquirida %>% 
  mutate(evolucao = recode(EVOLUCAO,
                           "1" = "Cura",
                           "2" = "Óbito por sifilis",
                           "3" = "Óbito por outras causas",
                           "9" = "ignorado"))

congenita <- congenita %>% 
  mutate(evolucao = recode(EVOLUCAO,
                           "1" = "Vivo",
                           "2" = "Óbito por sífilis congênita",
                           "3" = "Óbito por outras causas",
                           "4" = "Aborto",
                           "5" = "Natimorto",
                           "9" = "Ignorado"))



# sexo --------------------------------------------------------------------

adquirida <- adquirida %>% 
  mutate(sexo = recode(CS_SEXO,
                           "F" = "Feminino",
                           "M" = "Masculino",
                           "I" = "Ignorado"))

congenita <- congenita %>% 
  mutate(sexo = recode(CS_SEXO,
                       "F" = "Feminino",
                       "M" = "Masculino",
                       "I" = "Ignorado"))



# raca --------------------------------------------------------------------

adquirida <- adquirida %>% 
  mutate(raca_cat = recode(CS_RACA,
                           # for reference: OLD = NEW
                           "1" = "Branca",
                           "2" = "Preta",
                           "3" = "Amarela",
                           "4" = "Parda",
                           "5" = "Indígena",
                           "9" = "Ignorado"))


congenita <- congenita %>% 
  mutate(raca_cat = recode(CS_RACA,
                           # for reference: OLD = NEW
                           "1" = "Branca",
                           "2" = "Preta",
                           "3" = "Amarela",
                           "4" = "Parda",
                           "5" = "Indígena",
                           "9" = "Ignorado"))

gestante <- gestante %>% 
  mutate(raca_cat = recode(CS_RACA,
                           # for reference: OLD = NEW
                           "1" = "Branca",
                           "2" = "Preta",
                           "3" = "Amarela",
                           "4" = "Parda",
                           "5" = "Indígena",
                           "9" = "Ignorado"))



# escolaridade ------------------------------------------------------------

adquirida <- adquirida %>% 
  mutate(escola = recode(CS_ESCOL_N,
                         # for reference: OLD = NEW
                         "00" = "Fundamental incompleto",
                         "01" = "Fundamental incompleto",
                         "02" = "Fundamental incompleto",
                         "03" = "Fundamental incompleto",
                         "04" = "Fundamental completo",
                         "05" = "Fundamental completo",
                         "06" = "Médio completo",
                         "07" = "Médio completo",
                         "08" = "Ensino Superior",
                         "09" = "Ignorado",
                         "10" = "Não se aplica"))

# variáveis específicas da  sifilis congênita ---------------------------------------------

# Antecedentes Epid. da gestante / mãe

# Idade da mãe 

# Realizou Pré-Natal nesta gestação1-Sim 2-Não 9-Ignorado

congenita <- congenita %>% 
  mutate(pre_natal = recode(ANT_PRE_NA,
                       "1" = "Sim",
                       "2" = "Não",
                       "I" = "Ignorado"))




# Diagnóstico de sífilis materna 
congenita <- congenita %>% 
  mutate(diagnostico_mae = recode(ANTSIFIL_N,
                            "1" = "Durante o pré-natal",
                            "2" = "No momento do parto/curetagem",
                            "3" = "Após o parto",
                            "4" = "Não realizado",
                            "9" = "Ignorado"))



# Esquema de tratamento
congenita <- congenita %>% 
  mutate(tratamento_mae = recode(TRA_ESQUEM,
                                  "1" = "Adequado",
                                  "2" = "Inadequado",
                                  "3" = "Não realizado",
                                  "9" = "Ignorado"))






# Percentual de Gestantes com Sífilis Não Tratadas, por Regiao, Brasil, 2016 - media do estado
# Figura 6. Ranking da taxa de detecção (por 100.000 hab.) de sífilis adquirida segundo município de residência com população maior
# 100.000 habitantes. Pernambuco, 2014 a 2018*
# 
