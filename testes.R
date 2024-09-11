# Unir as tabelas com base no nome do município
sinasc_regiao <- sinasc_municipios %>%
  left_join(regiao %>% select(NM_MUNICIP, reg_saude), 
            by = c("municipio" = "NM_MUNICIP"))

# Somar os nascidos vivos por região de saúde
resultado <- sinasc_regiao %>%
  group_by(reg_saude) %>%
  summarise(across(starts_with("20"), sum, na.rm = TRUE))

# Exibir o resultado
print(resultado)


# Pivotar a tabela de nascidos vivos
resultado_pivot <- resultado %>%
  pivot_longer(cols = starts_with("20"), # Pivota as colunas que começam com "20"
               names_to = "ano", 
               values_to = "populacao") %>%
  mutate(ano = as.numeric(ano))  # Converte o ano para numérico



tabela_regiao <- congenita %>% 
  #filter(ano == input$filtro2) %>%  # Aplicar o filtro de ano
  group_by(reg_saude) %>% 
  summarise(
    cong_recente = sum(diag_definit == "sífilis congênita recente", na.rm = TRUE),
    cong_tardia = sum(diag_definit == "sífilis congênita tardia", na.rm = TRUE),
    aborto = sum(diag_definit == "aborto", na.rm = TRUE),
    natimorto = sum(diag_definit == "natimorto", na.rm = TRUE),
    casos = sum(cong_recente + cong_tardia + aborto + natimorto)
  ) %>% 
  left_join(resultado_pivot, by = "reg_saude") %>%  # Unir com a tabela de nascidos vivos
  mutate(
    reg_saude = str_to_title(reg_saude),  # Transformar os nomes da coluna reg_saude para minúsculas
    taxa_detecao = round(casos / populacao * 1000)  # Calcular a taxa de detecção por 100.000 habitantes
  )





tabela_regiao <- gestante %>% 
  group_by(reg_saude) %>% 
  summarise(casos = sum(CLASSI_FIN== "1", na.rm = TRUE),
            descartados = sum(CLASSI_FIN == "2", na.rm = TRUE)) %>% 
  left_join(resultado_pivot, by = "reg_saude") %>%  # Unir com a tabela de nascidos vivos
  mutate(
    reg_saude = str_to_title(reg_saude),  # Transformar os nomes da coluna reg_saude para minúsculas
    taxa_detecao = round(casos / populacao * 1000)  # Calcular a taxa de detecção por 100.000 habitantes
  )




# tabela de contagem 
casos_gestante <- gestante %>% 
  #filter(ano == input$filtro4) %>% #filtro
  group_by(municipio_residencia, ano_epi) %>% 
  summarise(casos = sum(CLASSI_FIN== "1", na.rm = TRUE),
            descartados = sum(CLASSI_FIN == "2", na.rm = TRUE)) 
  
  # Juntar as duas tabelas pelo município e ano
  tab_muni_gest <- casos_gestante %>%
  left_join(nascidos_vivos_long,
            by = c("municipio_residencia" = "municipio", "ano_epi" = "ano")) 


# Calcular a taxa de detecção
tab_muni_gest <- tab_muni_gest %>%
  mutate(taxa_deteccao = round((casos / nascidos_vivos) * 1000, 1))  # Arredondar para 2 casas decimais

# Transformar os nomes da coluna reg_saude para minúsculas
tab_muni_gest <- tab_muni_gest %>%
  mutate(municipio_residencia = str_to_title(municipio_residencia))






# Filtrar e contar os casos confirmados em cada base de dados
gestante_confirmados <- gestante %>%
  filter(ano_epi >= "2013") %>% 
  filter(CLASSI_FIN == "1") %>%
  mutate(ano_epi = as.integer(ano_epi)) %>%
  count(ano_epi)

congenita_confirmados <- congenita %>%
  mutate(ano_epi = as.integer(ano_epi)) %>%
  filter(ano_epi >= "2013") %>% 
  
  #filter(CLASSI_FIN == "Confirmados") %>%
  count(ano_epi)

adquirida_confirmados <- adquirida %>%
  filter(ano_epi >= "2013") %>%
  mutate(ano_epi = as.integer(ano_epi)) %>%
  filter(CLASSI_FIN == "1") %>%
  count(ano_epi)

# Unir os dados em uma única tabela
dados_combinados <- gestante_confirmados %>%
  rename(gestante = n) %>%
  full_join(congenita_confirmados %>% rename(congenita = n), by = "ano_epi") %>%
  full_join(adquirida_confirmados %>% rename(adquirida = n), by = "ano_epi") %>%
  replace_na(list(gestante = 0, congenita = 0, adquirida = 0))  # Substituir valores NA por 0

# Criar o gráfico de barras
dados_combinados %>%
  e_charts(ano_epi) %>%
  e_bar(adquirida, name = "Adquirida") %>%
  e_bar(gestante, name = "Gestante") %>%
  e_bar(congenita, name = "Congênita") %>%
  e_title("Casos Confirmados por Ano Epidemiológico") %>%
  e_tooltip(trigger = "axis") %>%
  e_legend(right = "10%") %>%
  e_x_axis(name = "Ano Epidemiológico", type="category") %>%
  e_y_axis(name = "Número de Casos")



### facetwrap



# Contagem de casos confirmados por reg_saude e ano_epi para cada base
gestante_confirmados <- gestante %>%
  filter(CLASSI_FIN == "1") %>%
  group_by(reg_saude, ano_epi) %>%
  summarise(gestante = n(), .groups = "drop")

congenita_confirmados <- congenita %>%
  group_by(reg_saude, ano_epi) %>%
  summarise(congenita = n(), .groups = "drop")

adquirida_confirmados <- adquirida %>%
  filter(CLASSI_FIN == "1") %>%
  group_by(reg_saude, ano_epi) %>%
  summarise(adquirida = n(), .groups = "drop")

# Combinar as contagens em uma única tabela
dados_combinados <- gestante_confirmados %>%
  full_join(congenita_confirmados, by = c("reg_saude", "ano_epi")) %>%
  full_join(adquirida_confirmados, by = c("reg_saude", "ano_epi")) %>%
  replace_na(list(gestante = 0, congenita = 0, adquirida = 0)) %>%
  pivot_longer(cols = c(gestante, congenita, adquirida),
               names_to = "tipo", values_to = "casos")

# Criar o gráfico com facet_wrap por reg_saude
ggplot(dados_combinados, aes(x = ano_epi, y = casos, fill = tipo)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ reg_saude) +
  labs(title = "Casos Confirmados por Região de Saúde",
       x = "Ano Epidemiológico",
       y = "Número de Casos",
       fill = "Tipo de Sífilis") +
  theme_minimal()

#echart


# Contagem de casos confirmados por reg_saude e ano_epi para cada base
gestante_confirmados <- gestante %>%
  filter(CLASSI_FIN == "1") %>%
  group_by(reg_saude, ano_epi) %>%
  summarise(gestante = n(), .groups = "drop")

congenita_confirmados <- congenita %>%
  group_by(reg_saude, ano_epi) %>%
  summarise(congenita = n(), .groups = "drop")

adquirida_confirmados <- adquirida %>%
  filter(CLASSI_FIN == "1") %>%
  group_by(reg_saude, ano_epi) %>%
  summarise(adquirida = n(), .groups = "drop")

# Combinar as contagens em uma única tabela
dados_combinados <- gestante_confirmados %>%
  full_join(congenita_confirmados, by = c("reg_saude", "ano_epi")) %>%
  full_join(adquirida_confirmados, by = c("reg_saude", "ano_epi")) %>%
  replace_na(list(gestante = 0, congenita = 0, adquirida = 0))


# Separando os dados por reg_saude
graficos_por_regiao <- dados_combinados %>%
  group_split(reg_saude)

# Criando um gráfico para cada região de saúde
graficos <- lapply(graficos_por_regiao, function(data) {
  regiao <- unique(data$reg_saude)
  
  data %>%
    e_charts(ano_epi) %>%
    e_bar(gestante, name = "Gestante") %>%
    e_bar(congenita, name = "Congênita") %>%
    e_bar(adquirida, name = "Adquirida") %>%
    e_title(text = regiao, subtext = "Casos Confirmados") %>%
    e_tooltip(trigger = "axis") %>%
    e_x_axis(name = "Ano Epidemiológico", type = "category") %>%
    e_y_axis(name = "Número de Casos") %>%
    e_legend(show = TRUE)

})

e_arrange(graficos)


