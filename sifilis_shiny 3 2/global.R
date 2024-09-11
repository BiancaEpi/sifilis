

# Painel de monitoramento da Sífilis
# Alterado para shiny em 26/06/2024
# Área técnica: GEIST - Carina 
# Mensagem da Carina a bases de dados: sifilis net (é a congênita),
# sifilis adquirida (notindivi -a539), e sifilis em gestante Gestsifnet
# A GANDT respondeu que o banco nomeado como SIFGNET é o sífilis gestante e o SIFCNEt é o sífilis congênita
# Os óbitos são contabilizados pelo SIM


# Nº de notificação e campos que correspondem aos campos de 1 a 30 dos blocos “Dados Gerais”, “Notificação Individual” e
# “Dados de residência” correspondem aos
# mesmos campos da ficha de notificação (ver dicionário de dados da ficha de notificação), exceto a data de diagnóstico


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


# Para importar dados disponíveis no comum de dentro do servidor

   # gestante <- import("/home/biancab/Documentos/pasta_mapeada/TransferenciaInterna/BaseDBF_SinanNet/SIFGENET.DBF")
   # congenita <- import("/home/biancab/Documentos/pasta_mapeada/TransferenciaInterna/BaseDBF_SinanNet/SIFICNET.DBF")
   # #load("/home/biancab/Documentos/pasta_mapeada/CIEVS/GEDIC/sifilis/adquirida_trat.RData")
   # adquirida <- adquirida_trat 
  


# import dos arquivos do comum no pc cievs

# congenita <- import("k:/COMUM/TransferenciaInterna/BaseDBF_SinanNet/SIFICNET.DBF")


# importar dados do arquivo na pasta de trabalho
  
   adquirida <- import("SifilisAdquirida.dbf")
   congenita <- import("SifilisCongênita.dbf")
   gestante <- import("SifilisGestante.dbf")

  

# filtro UF de residência  -------------------------------------------------


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



# faixa etaria ------------------------------------------------------------



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




# sexo --------------------------------------------------------------------

adquirida <- adquirida %>% 
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




# escolaridade ------------------------------------------------------------

adquirida <- adquirida %>% 
  mutate(escola = recode(CS_ESCOL_N,
                         # for reference: OLD = NEW
                         "00" = "Fund. incompleto",
                         "01" = "Fund. incompleto",
                         "02" = "Fund. incompleto",
                         "03" = "Fund. incompleto",
                         "04" = "Fund. completo",
                         "05" = "Fund. completo",
                         "06" = "Médio",
                         "07" = "Médio",
                         "08" = "Superior",
                         "09" = "Ignorado",
                         "10" = "Não se aplica"))

# variáveis  congênita ---------------------------------------------

#Data de diagnóstico
congenita <- congenita %>% 
  mutate(DT_DIAG = as.Date(DT_DIAG),
         epi_semana = floor_date(DT_DIAG, unit= "week"),
         mes_diag = floor_date(DT_DIAG, unit= "month"),
         ano_epi = year(DT_DIAG),
         ano = substr(SEM_PRI, 1, 4))



# idade da criança
congenita <- congenita %>% 
  mutate(idade = substr(NU_IDADE_N, 1, 2),
         faixa_etaria = recode(idade,
                               "20" = "< 30 dias",
                               "30" = "> 30 dias e < 1 ano",
                               "40" = "> 1 ano"))

# sexo da crianca 
congenita <- congenita %>% 
  mutate(sexo = recode(CS_SEXO,
                       "F" = "Feminino",
                       "M" = "Masculino",
                       "I" = "Ignorado"))


#Raça

congenita <- congenita %>% 
  mutate(raca_ca = recode(CS_RACA,
                           # for reference: OLD = NEW
                           "1" = "Branca",
                           "2" = "Preta",
                           "3" = "Amarela",
                           "4" = "Parda",
                           "5" = "Indígena",
                           "9" = "Ignorado"))

# Antecedentes Epid. da gestante / mãe


# Raça mae 
congenita <- congenita %>% 
  mutate(raca_cat_mae = recode(ANT_RACA,
                           # for reference: OLD = NEW
                           "1" = "Branca",
                           "2" = "Preta",
                           "3" = "Amarela",
                           "4" = "Parda",
                           "5" = "Indígena",
                           "9" = "Ignorado"))




# Idade da mãe 

congenita <- congenita %>% 
  mutate(faixa_etaria_mae = age_categories(ANT_IDADE, 
                                       lower=10, 
                                       upper=40, 
                                       by=10))



# escola mae
congenita <- congenita %>% 
  mutate(escola_mae = recode(ESCOLMAE,
                         # for reference: OLD = NEW
                         "00" = "Fund. incompleto",
                         "01" = "Fund. incompleto",
                         "02" = "Fund. incompleto",
                         "03" = "Fund. incompleto",
                         "04" = "Fund. completo",
                         "05" = "Fund. completo",
                         "06" = "Médio",
                         "07" = "Médio",
                         "08" = "Superior",
                         "09" = "Ignorado",
                         "10" = "Não se aplica"))


# Realizou Pré-Natal nesta gestação

congenita <- congenita %>% 
  mutate(pre_natal = recode(ANT_PRE_NA,
                       "1" = "Sim",
                       "2" = "Não",
                       "9" = "Ignorado"))




# Teste não treponêmico no parto/curetagem mae

congenita <- congenita %>% 
  mutate(nao_trepo_mae = recode(LAB_PARTO,
                                  "1" = "Reagente",
                                  "2" = "Não reagente",
                                  "3" = "Não realizado",
                                  "9" = "Ignorado"))

#crianca
congenita <- congenita %>% 
  mutate(nao_trepo_crianca = recode(LABC_SANGU,
                                "1" = "Reagente",
                                "2" = "Não reagente",
                                "3" = "Não realizado",
                                "9" = "Ignorado"))

# Diagnóstico de sífilis materna 
congenita <- congenita %>% 
  mutate(diagnostico_mae = recode(ANTSIFIL_N,
                            "1" = "Durante o pré-natal",
                            "2" = "No momento do parto/curetagem",
                            "3" = "Após o parto",
                            "4" = "Não realizado",
                            "9" = "Ignorado"))



# Esquema de tratamento da mae
congenita <- congenita %>% 
  mutate(tratamento_mae = recode(TRA_ESQUEM,
                                  "1" = "Adequado",
                                  "2" = "Inadequado",
                                  "3" = "Não realizado",
                                  "9" = "Ignorado"))


# dados clinicos crianca
# diagnostico clinico crianca
congenita <- congenita %>% 
  mutate(diag_crianca = recode(CLI_ASSINT,
                                 "1" = "assintomático",
                                 "2" = "sintomático",
                                 "3" = "Não se aplica",
                                 "9" = "Ignorado"))


#Diagnóstico Final
congenita <- congenita %>% 
  mutate(diag_definit = recode(EVO_DIAG_N,
                               "1" = "sífilis congênita recente",
                               "2" = "sífilis congênita tardia",
                               "3" = "aborto",
                               "4" = "natimorto",
                               "5" = "descartado", 
                               "9" = "Ignorado"))



#evolucao
congenita <- congenita %>% 
  mutate(evolucao = recode(EVOLUCAO,
                           "1" = "Vivo",
                           "2" = "Óbito por sífilis congênita",
                           "3" = "Óbito por outras causas",
                           "4" = "Aborto",
                           "5" = "Natimorto",
                           "9" = "Ignorado"))

# nascidos vivos
# library(readr)
# sinasc_regiao_saude <- read_csv("sinasc_regiao_saude.csv")
library(readxl)
sinasc_municipios <- read_excel("sinasc_municipios.xlsx")

# Transformar a tabela de nascidos vivos para o formato longo
nascidos_vivos_long <- sinasc_municipios %>%
  pivot_longer(cols = -municipio, names_to = "ano", values_to = "nascidos_vivos") %>%
  mutate(ano = as.integer(ano))






# Percentual de Gestantes com Sífilis Não Tratadas, por Regiao, 
# Brasil, 2016 - media do estado



# variaveis gestante ------------------------------------------------------


gestante <- gestante %>% 
  mutate( DT_DIAG = as.Date(DT_DIAG),
          epi_semana = floor_date(DT_DIAG, unit= "week"),
          mes_diag = floor_date(DT_DIAG, unit= "month"),
          ano_epi = year(DT_DIAG), 
          ano = substr(SEM_DIAG, 1, 4))

# criando a variável de idade a partir da data de nascimento
gestante$idade <- as.numeric((gestante$DT_DIAG - gestante$DT_NASC) /365)


# faixa etária 
gestante <- gestante %>% 
  mutate(faixa_etaria = age_categories(idade, 
                                       lower=0, 
                                       upper=60, 
                                       by=20))


gestante <- gestante %>% 
  mutate(classificacao = recode(CLASSI_FIN,
                                "1" = "Confirmado",
                                "2" = "Descartado"))

gestante <- gestante %>% 
  mutate(classificacao = replace_na(classificacao, "Suspeito"))


# escola mae
gestante <- gestante %>% 
  mutate(escola_mae = recode(CS_ESCOL_N,
                             # for reference: OLD = NEW
                             "00" = "Fund. incompleto",
                             "01" = "Fund. incompleto",
                             "02" = "Fund. incompleto",
                             "03" = "Fund. incompleto",
                             "04" = "Fund. completo",
                             "05" = "Fund. completo",
                             "06" = "Médio",
                             "07" = "Médio",
                             "08" = "Superior",
                             "09" = "Ignorado",
                             "10" = "Não se aplica"))



gestante <- gestante %>% 
  mutate(raca_cat = recode(CS_RACA,
                           # for reference: OLD = NEW
                           "1" = "Branca",
                           "2" = "Preta",
                           "3" = "Amarela",
                           "4" = "Parda",
                           "5" = "Indígena",
                           "9" = "Ignorado"))

# Classificação Clinica

gestante <- gestante %>% 
  mutate(classi_clinica = recode(TPEVIDENCI,
                           # for reference: OLD = NEW
                           "1" = "Primária",
                           "2" = "Secundária",
                           "3" = "Terciária",
                           "4" = "Latente",
                           "5" = "Indígena",
                           "9" = "Ignorado"))




# teste nao treponêmico no pré-natal

gestante <- gestante %>% 
  mutate(teste_naotrep = recode(TPTESTE1,
                           # for reference: OLD = NEW
                           "1" = "Reagente",
                           "2" = "Não reagente",
                           "3" = "Não realizado",
                           "9" = "Ignorado"))




# teste treponêmico no pré-natal

gestante <- gestante %>% 
  mutate(teste_trep = recode(TPCONFIRMA,
                                # for reference: OLD = NEW
                                "1" = "Reagente",
                                "2" = "Não reagente",
                                "3" = "Não realizado",
                                "9" = "Ignorado"))



# Esquema de tratamento

gestante <- gestante %>% 
  mutate(tratamento = recode(TPESQUEMA,
                             # for reference: OLD = NEW
                             "1" = "Penicilina 2.400.000 UI",
                             "2" = "Penicilina 4.800.000 UI",
                             "3" = "Penicilina 7.200.000 UI",
                             "4" = "Outro esquema",
                             "5" = "Não realizado",
                             "9" = "Ignorado"))


# Trimestre gestacional

gestante <- gestante %>% 
  mutate(idade_gestacional = recode(CS_GESTANT,
                             "1" = "1 trimestre",
                             "2" = "2 trimestre",
                             "3" = "3 trimestre",
                             "4" = "Idade gestacional Ignorada"))








