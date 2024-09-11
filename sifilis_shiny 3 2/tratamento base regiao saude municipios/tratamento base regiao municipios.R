library(rio)
library(tidyverse)
library(janitor)


#dados <- import("E:/Analises Bi/Sifilis/sinasc_cnv_nvsc_regiaosaude.csv",
#                encoding = "UTF-8")

dados <- import("E:/Analises Bi/Sifilis/sinasc_cnv_nvsc_regiaosaude.csv", #alterar caminho
                  encoding = "Latin-1")

glimpse(dados)

names(dados)[1] <- "regiao_saude"

dados$regiao_saude <- str_replace_all(dados$regiao_saude, "[0-9]", "")

export(dados, "E:/Analises Bi/Sifilis/sinasc_regiao_saude.csv") #alterar caminho

# -------------------------

muni <- import("E:/Analises Bi/Sifilis/sinasc_nvsc_municipio.csv",
               encoding = "Latin-1")

glimpse(muni)

muni$Município <- str_replace_all(muni$Município, "[0-9]", "")

export(muni, "E:/Analises Bi/Sifilis/sinasc_municipios.csv") #alterar caminho
