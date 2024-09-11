
########### SERVER ########################


function(input, output, session) {


# adquirida ---------------------------------------------------------------

  
       
  
  output$adquirida_plot <- echarts4r::renderEcharts4r({
    # Supondo que sua tabela 'adquirida' tenha uma coluna 'taxa_detecao'
    dados_plot <- adquirida %>%
      filter(ano == input$ano_adquirida) %>% 
      filter(classificacao == "Confirmado") %>%
      group_by(mes_diag) %>%
      summarise(
        casos = n()) %>% 
      mutate(incidencia_mes = round(casos/7610361*100000)) 
    
    dados_plot %>%
      e_charts(x = mes_diag) %>%
      e_bar(serie = casos, name = "Casos") %>%
      e_line(serie = incidencia_mes, name = "Taxa de Detecção", y_index = 1) %>% # Adiciona a linha e o segundo eixo y
      e_y_axis(
        index = 1,
        name = "Taxa de Detecção",
        axisLabel = list(formatter = '{value}')
      ) %>%
      e_tooltip(trigger = "axis") %>%
      e_color(mycolors) %>%
      e_legend(show = FALSE) %>%
      e_title(
        text = "Casos de Sífilis Adquirida",
        subtext = "Segundo mês de início de sintomas",
        left = "center"
      ) %>%
      e_format_y_axis(suffix = "", prefix = "", formatter = e_axis_formatter(locale = "PT", digits = 0)) %>%
      e_animation(duration = 8000)
  })
  
  
  
  
        
        output$sexo_adquirida <- echarts4r::renderEcharts4r({
            
               adquirida %>%
                filter(ano == input$ano_adquirida) %>% 
                filter(classificacao == "Confirmado") %>%
                filter(sexo != "Ignorado") %>% 
                group_by(sexo) %>%
                summarise(casos = n()) %>%
                e_charts(x = sexo) %>%
                e_bar(serie = casos) %>%
                e_tooltip(trigger = "axis") %>%
                e_color("rgba(105, 105, 179, 0.7)") %>%  # Cor com transparência
                e_legend(show=FALSE) %>%
                e_title(
                    text = "Sexo",
                    subtext = "Casos confirmados",
                    left = "center"
                ) %>%
                e_format_y_axis(suffix = "",prefix = "",
                                formatter = e_axis_formatter(locale = "PT",digits = 0)) %>% 
                e_animation(duration = 8000)
        })
        
      
        
        
        output$faixa_etaria_adquirida <- echarts4r::renderEcharts4r({
            
            adquirida %>%
                filter(ano == input$ano_adquirida) %>% 
                filter(classificacao == "Confirmado", !is.na(faixa_etaria)) %>%
                group_by(faixa_etaria) %>%
                summarise(casos = n()) %>%
                e_charts(x = faixa_etaria) %>%
                e_bar(serie = casos) %>%
                e_tooltip(trigger = "axis") %>%
                e_color("rgba(105, 105, 179, 0.7)") %>%  # Cor com transparência
                e_legend(show=FALSE) %>%
                e_title(
                    text = "Faixa etária",
                    subtext = "Casos confirmados",
                    left = "center"
                ) %>%
                e_format_y_axis(suffix = "",prefix = "",
                                formatter = e_axis_formatter(locale = "PT",digits = 0)) %>% 
                e_animation(duration = 8000) 
        })
        
        
        output$raca_adquirida <- echarts4r::renderEcharts4r({
            
                adquirida %>%
                filter(ano == input$ano_adquirida) %>% 
                filter(classificacao == "Confirmado", !is.na(raca_cat)) %>%
                filter(raca_cat != "Ignorado") %>% 
                group_by(raca_cat) %>%
                summarise(casos = n()) %>%
                arrange(desc(casos)) %>%
                e_charts(x = raca_cat) %>%
                e_bar(serie = casos) %>%
                e_tooltip(trigger = "axis") %>%
                e_color("rgba(105, 105, 179, 0.7)") %>%  # Cor com transparência
                e_legend(show=FALSE) %>%
                e_title(
                    text = "Raça",
                    subtext = "Casos confirmados",
                    left = "center"
                ) %>%
                e_animation(duration = 8000) %>%  
                e_format_y_axis(suffix = "",prefix = "",
                                formatter = e_axis_formatter(locale = "PT",digits = 0)) %>% 
                
                 e_flip_coords() 

        })
        
        
        output$escola_adquirida <- echarts4r::renderEcharts4r({
            
                adquirida %>%
                filter(ano == input$ano_adquirida) %>% 
                filter(classificacao == "Confirmado", !is.na(escola)) %>%
                filter(escola != "Ignorado") %>% 
                group_by(escola) %>%
                summarise(casos = n()) %>%
                arrange(desc(casos)) %>%
                e_charts(x = escola) %>%
                e_bar(serie = casos) %>%
                e_tooltip(trigger = "axis") %>%
                e_color("rgba(105, 105, 179, 0.7)") %>%  # Cor com transparência
                e_legend(show=FALSE) %>%
                e_title(
                    text = "Escolaridade",
                    subtext = "Casos confirmados",
                    left = "center"
                ) %>%
                e_animation(duration = 8000) %>%  
                e_format_y_axis(suffix = "",prefix = "",
                                formatter = e_axis_formatter(locale = "PT",digits = 0))  
                
               # e_flip_coords() 
            
        })
        
        
        
        output$regiao_adquirida <- echarts4r::renderEcharts4r({
            
            adquirida %>%
                filter(ano == input$ano_adquirida) %>% 
                filter(classificacao == "Confirmado", !is.na(reg_saude)) %>%
                group_by(reg_saude) %>%
                summarise(casos = n()) %>%
                arrange(desc(casos)) %>%
                mutate(reg_saude = str_to_title(reg_saude)) %>%  # Transformando para minúsculas
                e_charts(x = reg_saude) %>%
                e_bar(serie = casos) %>%
                e_tooltip(trigger = "axis") %>%
                e_color(mycolors) %>%
                e_legend(show=FALSE) %>%
                e_title(
                    text = "Região de Saúde",
                    subtext = "Casos confirmados",
                    left = "center") %>%
                e_animation(duration = 8000) %>%  
                e_x_axis(axisLabel = list(fontSize = 10)) %>%  # Ajuste o tamanho da fonte aqui
                e_format_y_axis(suffix = "", prefix = "",
                                formatter = e_axis_formatter(locale = "PT",digits = 0)) %>%  
                e_grid(left = "15%", right = "15%", bottom = "10%") %>%  # Ajuste as margens aqui
                e_flip_coords() 
            
        })
        
        
        
        
        
# mapa adquirida --------------------------------------------------------------------
        
        output$mapa_adq <- renderLeaflet({
            
            # shapefile 
            
            sc <- read_municipality(code_muni = 'SC', year=2018)
            sc$codigo <- substr(sc$code_muni,1,6) # retira o ultimo algarismo da variável code_muni 
            sc$codigo <- as.numeric(sc$codigo) 
            
            # contingencia ------------------------------------------------------------
            
            casos_adquirida <- adquirida %>%
                filter(ano == input$ano_adquirida) %>% 
                filter(classificacao == "Confirmado") %>%
                group_by(municipio_residencia,ID_MN_RESI,populacao_residente) %>%
                summarise(casos = n()) %>%
              mutate(incidencia = round((casos / populacao_residente) * 100000, 1)) %>%
              rename(nom_municipio = municipio_residencia,
                       codigo = ID_MN_RESI) 
            
              casos_adquirida$codigo <- as.numeric(casos_adquirida$codigo)
            
            # No banco dengue_2023 ja foi feito o join de regiao e populacao, vou fazer novamente pra pegar
            # os 295 municipios mesmo que nao tenham casos confirmados. depois transformar os NA para o valor 0
            # assim o mapa nao fica municipios sem preenchimento de cor.
            
            casos_adquirida <- left_join(populacao, casos_adquirida, by="nom_municipio")
            
            
            #join do shapefile e da tabela de contigencia
            casos_adquirida <- left_join(sc,casos_adquirida, by="codigo")
            
            # Inclui o valor 0 para os municipios que estao como NA (nesse caso n contabilizaram casos)
            casos_adquirida <- casos_adquirida %>%
                mutate(incidencia = replace_na(incidencia, 0))
            
            
            
            # bins e colors   -------------------------------------------------------
            
            #cria funcoes para estratificar a incidenia
            bins <- c(0,50,100,300,500,1000, Inf)
            pal <- colorBin("YlOrRd", domain = casos_adquirida$incidencia, bins=bins)
            
            
            label <- sprintf(
              "<strong>%s</strong><br/>%g Casos de Sífilis<br/>%g Taxa de Detecção",
              casos_adquirida$nom_municipio, casos_adquirida$casos, casos_adquirida$incidencia
            ) %>% 
              lapply(htmltools::HTML)
            
            
            map = leaflet(casos_adquirida) %>%
                addTiles() %>%
                addProviderTiles(providers$CartoDB) 
            
            
            
            map %>% addPolygons(
                fillColor = ~pal(incidencia),
                weight = 2,
                opacity = 0, #opacidade da linha
                color= "white", #cor da linha
                dashArray = "1",
                fillOpacity = 0.7, # cor do fundo
                highlight = highlightOptions( #destacar o mapa quando passa o mouse por cima
                    weight = 5,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                label = label,
                labelOptions = labelOptions(
                    style = list("font-weight"= "normal", padding ="3px 8px"),
                    textsize = "15px",
                    direction = "auto"
                )) %>%
                addLegend(pal=pal,  #legenda
                          values = ~incidencia,
                          opacity = 0.7,
                          title = "Taxa de detecção (100 mil hab.)",
                          position = "bottomleft") 
            
        })
        
        
# tabela 1 adquirida muni -------------------------------------------------------
        
        output$tabela_res_adq <- renderDataTable ({
            
                tabela_municipio_residencia <- adquirida %>% 
                filter(ano == input$ano_adquirida) %>% 
                group_by(municipio_residencia, ano,populacao_residente, pop_2010) %>%
                summarise(casos = sum(classificacao =="Confirmado", na.rm = T),
                          descartados = sum(classificacao =="Descartado", na.rm = T),
                          suspeito = sum(classificacao =="Suspeito", na.rm = T)) %>% 
                  mutate(
                    populacao_utilizada = case_when(
                      ano >= "2010" & ano <= "2021" ~ pop_2010,
                      ano >= "2022" & ano <= "2024" ~ populacao_residente,
                      TRUE ~ NA_real_  ), # Ou outra lógica que faça sentido para os anos fora dos intervalos
                    incidencia_geral = round(casos / populacao_utilizada * 100000)) %>% 
                  mutate(municipio_residencia = str_to_title(municipio_residencia)) %>% 
                
                  select(municipio_residencia, casos, 
                         descartados, suspeito,
                         incidencia_geral)
                
            
            
            
            DT::datatable(tabela_municipio_residencia,
                          
                          rownames= FALSE,
                          colnames = c (
                            "Ano",
                            "pop.",
                            "Município de residência",
                                         "Casos confirmados",
                                         "Casos descartados",
                                         "Casos Suspeitos",
                                         "Tx. Detecção"),
                          extensions = "Buttons",
                          options = list(dom = "Blfrtip", 
                                         pageLength = 7, # Define o número de linhas por página. #inserido wm 01/06/2024
                                         scrollX = TRUE,  # coloca o scroll no eixo x                                          
                                         
                                         buttons = c('excel'),
                                         lengthMenu = list(c(10, 25, 50, -1), c(10, 25, 50, "Todos")),
                                         language = list(paginate = 
                                                             list('next'="Próximo",
                                                                  'previous'="Anterior"),
                                                         search = "Pesquisar:")
                          ))
        })
        
  

        
        

        
        
# tabela 2 adquirida regiao --------------------------------------------------

        output$tabela_regiao_adq <- renderDataTable ({
          
          
          tabela_regiao <- adquirida %>% 
            filter(ano == input$ano_adquirida) %>% 
            group_by(reg_saude) %>% 
            summarise(casos = sum(classificacao =="Confirmado", na.rm = T),
                      descartados = sum(classificacao =="Descartado", na.rm = T),
                      suspeito = sum(classificacao =="Suspeito", na.rm = T)) %>% 
            # Transformar os nomes da coluna reg_saude para minúsculas
            mutate(reg_saude = str_to_title(reg_saude))
          
          
          
          
          DT::datatable(tabela_regiao,
                        
                        rownames= FALSE,
                        colnames = c (  "Região de Saúde",
                                        "Casos confirmados",
                                        "Casos descartados",
                                        "Casos Suspeitos"),
                        extensions = "Buttons",
                        options = list(dom = "Blfrtip", 
                                       pageLength = 7, # Define o número de linhas por página. #inserido wm 01/06/2024
                                       scrollX = TRUE,  # coloca o scroll no eixo x                                          
                                       
                                       buttons = c('excel'),
                                       lengthMenu = list(c(10, 25, 50, -1), c(10, 25, 50, "Todos")),
                                       language = list(paginate = 
                                                         list('next'="Próximo",
                                                              'previous'="Anterior"),
                                                       search = "Pesquisar:")
                        ))
        })
        
        
        
        
        
        
        
# congenita ---------------------------------------------------------------

        output$congenita_plot <- echarts4r::renderEcharts4r({
          
         # tabela de contagem 
          casos_congenita <- congenita %>% 
            filter(ano == input$filtro2) %>% 
            group_by(ano_epi,mes_diag) %>% 
            summarise(cong_recente = sum(diag_definit =="sífilis congênita recente", na.rm = T),
                      cong_tardia = sum(diag_definit =="sífilis congênita tardia", na.rm = T),
                      aborto = sum(diag_definit =="aborto", na.rm = T),
                      natimorto = sum(diag_definit =="natimorto", na.rm = T),
                      casos = sum(aborto+natimorto + cong_tardia + cong_recente)) 
          
          nasc <- nascidos_vivos_long %>% 
            group_by(ano) %>% 
            summarise(pop = sum(nascidos_vivos))
          
          # Juntar as duas tabelas pelo município e ano
          casos_congenita <- casos_congenita %>%
            left_join(nasc, by = c("ano_epi" = "ano"))
          
          # Calcular a taxa de detecção
          casos_congenita <- casos_congenita %>%
            mutate(taxa_deteccao = (casos / pop) * 1000)
          
          
          # Garantir que não haja valores NA na taxa de detecção
          casos_congenita <- casos_congenita %>%
            mutate(taxa_deteccao = replace_na(taxa_deteccao, 0))
          
          
            
          plot1 <- casos_congenita %>%
            e_charts(x= mes_diag) %>%
            e_bar(serie = casos, name = "Casos") %>%
            e_line(serie = taxa_deteccao, name = "Taxa de Detecção", y_index = 1) %>% # Adiciona a linha e o segundo eixo y
            e_y_axis( index = 1, name = "Taxa de Detecção",axisLabel = list(formatter = '{value}')) %>%
            e_tooltip(trigger = "axis") %>%
            e_color(mycolors) %>%
            e_legend(show = FALSE) %>%
            e_title(
              text = "Sífilis Congênita",
              subtext = "Segundo mês de diagnóstico",
              left = "center"
            ) %>%
            e_format_y_axis(suffix = "", prefix = "", formatter = e_axis_formatter(locale = "PT", digits = 0)) %>%
            e_animation(duration = 8000)
          
          plot1 
          
        })
            
        
        output$sexo_congenita <- echarts4r::renderEcharts4r({
            
                congenita %>%
                filter(ano == input$filtro2) %>% 
                filter(sexo != "Ignorado") %>% 
                group_by(sexo) %>%
                summarise(casos = n()) %>%
                e_charts(x = sexo) %>%
                e_bar(serie = casos) %>%
                e_tooltip(trigger = "axis") %>%
                e_color(mycolors) %>%
                e_legend(show=FALSE) %>%
                e_title(
                    text = "Sexo da criança",
                    subtext = "",
                    left = "center"
                ) %>%
                e_animation(duration = 8000)
        })  
        
        output$faixa_etaria_congenita <- echarts4r::renderEcharts4r({

               congenita %>%
               filter(ano == input$filtro2) %>% 
                group_by(faixa_etaria) %>%
                summarise(casos = n()) %>%
                e_charts( faixa_etaria) %>%
                e_pie(casos, radius = c("50%", "70%"),legend = FALSE) |> 
                e_title("Idade da criança")  %>%
                e_color(mycolors) %>% 
                e_animation(duration = 8000)
                
           
            
        })

        output$faixa_etaria_mae <- echarts4r::renderEcharts4r({
            
                 congenita %>%
            filter(ano == input$filtro2) %>% 
            filter(faixa_etaria_mae != "Ignorado") %>% 
                group_by(faixa_etaria_mae) %>%
                summarise(casos = n()) %>%
                arrange(desc(casos)) %>%
                e_charts(x = faixa_etaria_mae) %>%
                e_bar(serie = casos) %>%
                e_tooltip(trigger = "axis") %>%
                e_color(mycolors) %>%
                e_legend(show=FALSE) %>%
                e_title(
                    text = "Faixa etária da mãe",
                    subtext = " ",
                    left = "center"
                ) %>%
                e_animation(duration = 8000)
        }) 

        output$escola_mae <- echarts4r::renderEcharts4r({
          
          congenita %>%
            filter(ano == input$filtro2) %>% 
            filter(escola_mae != "Ignorado") %>% 
            group_by(escola_mae) %>%
            summarise(casos = n()) %>%
            arrange(desc(casos)) %>%
            e_charts(x = escola_mae) %>%
            e_bar(serie = casos) %>%
            e_tooltip(trigger = "axis") %>%
            e_color(mycolors) %>%
            e_legend(show=FALSE) %>%
            e_title(
              text = "Escolaridade da mãe",
              subtext = " ",
              left = "center"
            ) %>%
            e_animation(duration = 8000)
        }) 
        
        output$pre_natal <- echarts4r::renderEcharts4r({
          
          congenita %>%
            filter(ano == input$filtro2) %>% 
           # filter(pre_natal != "Ignorado") %>% 
            group_by(pre_natal) %>%
            summarise(casos = n()) %>%
            arrange(desc(casos)) %>%
            e_charts(x = pre_natal) %>%
            e_bar(serie = casos) %>%
            e_tooltip(trigger = "axis") %>%
            e_color(mycolors) %>%
            e_legend(show=FALSE) %>%
            e_title(
              text = "Realizou Pré-Natal nesta gestação",
              subtext = " ",
              left = "center"
            ) %>%
            e_animation(duration = 8000)
        }) 
        
        output$diagnostico_mae <- echarts4r::renderEcharts4r({
          
          congenita %>%
            filter(ano == input$filtro2) %>% 
            filter(diagnostico_mae != "Ignorado") %>% 
            group_by(diagnostico_mae) %>%
            summarise(casos = n()) %>%
            arrange(desc(casos)) %>%
            e_charts(x = diagnostico_mae) %>%
            e_bar(serie = casos) %>%
            e_tooltip(trigger = "axis") %>%
            e_color(mycolors) %>%
            e_legend(show=FALSE) %>%
            e_title(
              text = "Diagnóstico de sífilis materna",
              subtext = " ",
              left = "center"
            ) %>%
            e_animation(duration = 8000)
        })  
        
        output$nao_trepo_mae <- echarts4r::renderEcharts4r({
          
          congenita %>%
            filter(ano == input$filtro2) %>% 
            filter(nao_trepo_mae != "Ignorado") %>% 
            group_by(nao_trepo_mae) %>%
            summarise(casos = n()) %>%
            arrange(desc(casos)) %>%
            e_charts(x = nao_trepo_mae) %>%
            e_bar(serie = casos) %>%
            e_tooltip(trigger = "axis") %>%
            e_color(mycolors) %>%
            e_legend(show=FALSE) %>%
            e_title(
              text = "Teste não treponêmico no parto/curetagem - mãe",
              subtext = " ",
              left = "center"
            ) %>%
            e_animation(duration = 8000)
        })  
        
        output$tratamento_mae <- echarts4r::renderEcharts4r({
          
          congenita %>%
            filter(ano == input$filtro2) %>% 
            filter(tratamento_mae != "Ignorado") %>% 
            group_by(tratamento_mae) %>%
            summarise(casos = n()) %>%
            arrange(desc(casos)) %>%
            e_charts(x = tratamento_mae) %>%
            e_bar(serie = casos) %>%
            e_tooltip(trigger = "axis") %>%
            e_color(mycolors) %>%
            e_legend(show=FALSE) %>%
            e_title(
              text = "Esquema de tratamento da mãe",
              subtext = " ",
              left = "center"
            ) %>%
            e_animation(duration = 8000)
        })  
        
        output$diag_crianca <- echarts4r::renderEcharts4r({
          
          congenita %>%
            filter(ano == input$filtro2) %>% 
            filter(diag_crianca != "Ignorado") %>% 
            group_by(diag_crianca) %>%
            summarise(casos = n()) %>%
            arrange(desc(casos)) %>%
            e_charts(x = diag_crianca) %>%
            e_bar(serie = casos) %>%
            e_tooltip(trigger = "axis") %>%
            e_color(mycolors) %>%
            e_legend(show=FALSE) %>%
            e_title(
              text = "Diagnóstico clínico criança",
              subtext = " ",
              left = "center"
            ) %>%
            e_animation(duration = 8000)
        })  
        
        output$diag_definit <- echarts4r::renderEcharts4r({
          
          congenita %>%
            filter(ano == input$filtro2) %>% 
            filter(diag_definit != "Ignorado") %>% 
            group_by(diag_definit) %>%
            summarise(casos = n()) %>%
            arrange(desc(casos)) %>%
            e_charts(x = diag_definit) %>%
            e_bar(serie = casos) %>%
            e_tooltip(trigger = "axis") %>%
            e_color(mycolors) %>%
            e_legend(show=FALSE) %>%
            e_title(
              text = "Diagnóstico Final",
              subtext = " ",
              left = "center"
            ) %>%
            e_animation(duration = 8000)
        })  
        
        output$reg_saude_plot <- echarts4r::renderEcharts4r({
          
          congenita %>%
            filter(ano == input$filtro2) %>% 
            group_by(reg_saude) %>%
            summarise(casos = n()) %>%
            arrange(desc(casos)) %>%
            mutate(reg_saude = str_to_title(reg_saude)) %>%  # Transformando para minúsculas
            e_charts(x = reg_saude) %>%
            e_bar(serie = casos) %>%
            e_tooltip(trigger = "axis") %>%
            e_color(mycolors) %>%
            e_legend(show=FALSE) %>%
            e_title(
              text = "Região de Saúde",
              subtext = "Casos confirmados",
              left = "center") %>%
            e_animation(duration = 8000) %>%  
            e_x_axis(axisLabel = list(fontSize = 10)) %>%  # Ajuste o tamanho da fonte aqui
            e_format_y_axis(suffix = "", prefix = "",
                            formatter = e_axis_formatter(locale = "PT",digits = 0)) %>%  
            e_grid(left = "15%", right = "15%", bottom = "10%") %>%  # Ajuste as margens aqui
            e_flip_coords() 
          
        })
        
        output$reg_saude_tab <- renderDataTable ({

            tabela_regiao <- congenita %>% 
              filter(ano == input$filtro2) %>% # filtro
            group_by(reg_saude) %>% 
            summarise(cong_recente = sum(diag_definit =="sífilis congênita recente", na.rm = T),
                      cong_tardia = sum(diag_definit =="sífilis congênita tardia", na.rm = T),
                      aborto = sum(diag_definit =="aborto", na.rm = T),
                      natimorto = sum(diag_definit =="natimorto", na.rm = T)
                      ) %>% 
           # Transformar os nomes da coluna reg_saude para minúsculas
            mutate(reg_saude = str_to_title(reg_saude))
          
          
          DT::datatable(tabela_regiao,
                        
                        rownames= FALSE,
                        colnames = c (  "Região de Saúde",
                                        "Cong. recente",
                                        "Cong. tardia",
                                        "Aborto",
                                        "Natimorto"),
                        extensions = "Buttons",
                        options = list(dom = "Blfrtip", 
                                       pageLength = 7, # Define o número de linhas por página. #inserido wm 01/06/2024
                                       scrollX = TRUE,  # coloca o scroll no eixo x                                          
                                       
                                       buttons = c('excel'),
                                       lengthMenu = list(c(5,10, 25, -1), c(5,10, 25, "Todos")),
                                       language = list(paginate = 
                                                         list('next'="Próximo",
                                                              'previous'="Anterior"),
                                                       search = "Pesquisar:")
                        ))
        })
        
        output$tab_muni_cong <- renderDataTable ({
          
          # tabela de contagem 
          casos_congenita <- congenita %>% 
            filter(ano == input$filtro2) %>% # filtro
            group_by(municipio_residencia, ano_epi) %>% 
            summarise(cong_recente = sum(diag_definit =="sífilis congênita recente", na.rm = T),
                      cong_tardia = sum(diag_definit =="sífilis congênita tardia", na.rm = T),
                      aborto = sum(diag_definit =="aborto", na.rm = T),
                      natimorto = sum(diag_definit =="natimorto", na.rm = T),
                      casos = sum(aborto+natimorto + cong_tardia + cong_recente)) 
          
          # Juntar as duas tabelas pelo município e ano
          tab_muni_cong <- casos_congenita %>%
            left_join(nascidos_vivos_long,
                      by = c("municipio_residencia" = "municipio", "ano_epi" = "ano"))
          
          
          # Calcular a taxa de detecção
          tab_muni_cong <- tab_muni_cong %>%
            mutate(taxa_deteccao = round((casos / nascidos_vivos) * 1000, 1))  # Arredondar para 2 casas decimais
          
          # Transformar os nomes da coluna reg_saude para minúsculas
          tab_muni_cong <- tab_muni_cong %>%
            mutate(municipio_residencia = str_to_title(municipio_residencia))
          
          
          
          # datatable      
          DT::datatable(tab_muni_cong,
                        
                        rownames= FALSE,
                        colnames = c (
                          "Município de residência",
                          "Ano",
                          "Cong. recente",
                          "Cong. tardia",
                          "Aborto",
                          "Natimorto",
                          "Casos",
                          "pop.",
                          "Tx detecção"),
                        extensions = "Buttons",
                        options = list(dom = "Blfrtip", 
                                       pageLength = 7, # Define o número de linhas por página. #inserido wm 01/06/2024
                                       scrollX = TRUE,  # coloca o scroll no eixo x                                          
                                       
                                       buttons = c('excel'),
                                       lengthMenu = list(c(3, 25, 50, -1), c(3, 25, 50, "Todos")),
                                       language = list(paginate = 
                                                         list('next'="Próximo",
                                                              'previous'="Anterior"),
                                                       search = "Pesquisar:")
                        ))
        })
        
        
        
        

# mapa congenita ----------------------------------------------------------

        output$mapa_cong <- renderLeaflet({  
        
        # tabela de contagem 
        casos_congenita <- congenita %>% 
          filter(ano == input$filtro2) %>% #filtro
          group_by(municipio_residencia, ID_MN_RESI, ano_epi) %>% 
          summarise(casos = n()) 
        
        casos_congenita$codigo <- as.numeric(casos_congenita$ID_MN_RESI) # 6 digitos
        
        # Juntar as duas tabelas pelo município e ano
        taxa_deteccao <- casos_congenita %>%
          left_join(nascidos_vivos_long, by = c("municipio_residencia" = "municipio", "ano_epi" = "ano"))
        
        # Calcular a taxa de detecção
        taxa_deteccao <- taxa_deteccao %>%
          mutate(taxa_deteccao = (casos / nascidos_vivos) * 1000)
        
        
        # shape
        
        sc <- read_municipality(code_muni = 'SC', year=2018)
        sc$codigo <- substr(sc$code_muni,1,6) # retira o ultimo algarismo da variável code_muni 
        sc$codigo <- as.numeric(sc$codigo) 
        
        
        #join do shapefile e da tabela de contigencia
        taxa_deteccao <- left_join(sc,taxa_deteccao, by= c( "codigo"))
        
        # Inclui o valor 0 para os municipios que estao como NA (nesse caso n contabilizaram casos)
        taxa_deteccao <- taxa_deteccao %>%
          mutate(taxa_deteccao = replace_na(taxa_deteccao, 0))
        
        
        # bins e colors   -------------------------------------------------------
        
        #cria funcoes para estratificar a incidenia
        bins <- c(0,10,30,50,100, Inf)
        pal <- colorBin("YlOrRd", domain = taxa_deteccao$taxa_deteccao, bins=bins)
        
        
        label <- sprintf(
          "<strong>%s</strong><br/>%g Casos de Sífilis<br/>%g Taxa de Detecção",
          taxa_deteccao$municipio_residencia, taxa_deteccao$casos, taxa_deteccao$taxa_deteccao
        ) %>% 
          lapply(htmltools::HTML)
        
        
        map = leaflet(taxa_deteccao) %>%
          addTiles() %>%
          addProviderTiles(providers$CartoDB) 
        
        
        
        map %>% addPolygons(
          fillColor = ~pal(taxa_deteccao),
          weight = 2,
          opacity = 0, #opacidade da linha
          color= "white", #cor da linha
          dashArray = "1",
          fillOpacity = 0.7, # cor do fundo
          highlight = highlightOptions( #destacar o mapa quando passa o mouse por cima
            weight = 5,
            color = "#666",
            dashArray = "",
            fillOpacity = 0.7,
            bringToFront = TRUE),
          label = label,
          labelOptions = labelOptions(
            style = list("font-weight"= "normal", padding ="3px 8px"),
            textsize = "15px",
            direction = "auto"
          )) %>%
          addLegend(pal=pal,  #legenda
                    values = ~taxa_deteccao,
                    opacity = 0.7,
                    title = "Taxa de detecção (100 mil hab.)",
                    position = "bottomleft") 
        
})
        
        
         
        
        
# gestante ----------------------------------------------------------------

        
        
        output$gestante_plot <- echarts4r::renderEcharts4r({
        
            gestante %>%
            filter(ano == input$filtro4) %>% 
            filter(classificacao== "Confirmado") %>%
            dplyr::group_by(mes_diag) %>%
            summarise(casos= n()) %>%
            e_charts(x= mes_diag) %>%
            e_bar(serie= casos) %>%
            e_tooltip(trigger = "axis") %>%
            e_color(mycolors) %>%
                e_legend(show=FALSE) %>%
                e_title(text= "Casos de Sífilis em gestantes",
                    subtext = "Segundo mês de diagnóstico",
                    left = "center") %>%
            e_animation(duration = 8000)
          
          
          
          
        
})
        
        # faixa etaria 
        
        output$faixa_etaria_gestante <- echarts4r::renderEcharts4r({
          
        
        gestante %>%
          filter(ano == input$filtro4) %>% 
            filter(faixa_etaria != "Ignorado") %>% 
            filter(!is.na(faixa_etaria)) %>%  # Remove valores ausentes
            group_by(faixa_etaria) %>%
            summarise(casos = n()) %>%
            arrange(desc(casos)) %>%
            e_charts(x = faixa_etaria) %>%
            e_bar(serie = casos) %>%
            e_tooltip(trigger = "axis") %>%
            e_color(mycolors) %>%
            e_legend(show=FALSE) %>%
            e_title(
              text = "Faixa etária",
              subtext = " ",
              left = "center"
            ) %>%
            e_animation(duration = 8000)
          
        
        
        })   
        
        # escolaridade
        
        output$escola_gestante <- echarts4r::renderEcharts4r({
          
          
          gestante %>%
            filter(ano == input$filtro4) %>% 
            filter(escola_mae != "Ignorado") %>% 
            filter(!is.na(escola_mae)) %>%  # Remove valores ausentes
            group_by(escola_mae) %>%
            summarise(casos = n()) %>%
            arrange(desc(casos)) %>%
            e_charts(x = escola_mae) %>%
            e_bar(serie = casos) %>%
            e_tooltip(trigger = "axis") %>%
            e_color(mycolors) %>%
            e_legend(show=FALSE) %>%
            e_title(
              text = "Escolaridade",
              subtext = " ",
              left = "center"
            ) %>%
            e_animation(duration = 8000)
          
          
        })   
        
        output$tratamento_gestante <- echarts4r::renderEcharts4r({
          
          
            gestante %>%
              filter(ano == input$filtro4) %>% 
              filter(tratamento != "Ignorado") %>% 
              filter(!is.na(tratamento)) %>%  # Remove valores ausentes
              group_by(tratamento) %>%
              summarise(casos = n()) %>%
              arrange(desc(casos)) %>% 
              e_charts(x = tratamento) %>%
              e_bar(serie = casos) %>%
              e_tooltip(trigger = "axis") %>%
              e_color(mycolors) %>%
              e_legend(show=FALSE) %>%
              e_title(
                text = "Tratamento Penicilina G benzatina",
                subtext = " ",
                left = "center"
              ) %>%
              e_animation(duration = 8000)
            
            
          })   
        
        
        output$clinica_gestante <- echarts4r::renderEcharts4r({
          
          
          gestante %>%
            filter(ano == input$filtro4) %>% 
            filter(classi_clinica != "Ignorado") %>% 
            filter(!is.na(classi_clinica)) %>%  # Remove valores ausentes
            
            group_by(classi_clinica) %>%
            summarise(casos = n()) %>%
            arrange(desc(casos)) %>%
            e_charts(x = classi_clinica) %>%
            e_bar(serie = casos) %>%
            e_tooltip(trigger = "axis") %>%
            e_color(mycolors) %>%
            e_legend(show=FALSE) %>%
            e_title(
              text = "Clínica",
              subtext = " ",
              left = "center"
            ) %>%
            e_animation(duration = 8000)
          
          
        })  
        
        output$nao_trepo <- echarts4r::renderEcharts4r({
          
          
          gestante %>%
            filter(ano == input$filtro4) %>% 
            filter(teste_naotrep != "Ignorado") %>% 
            filter(!is.na(teste_naotrep)) %>%  # Remove valores ausentes
            group_by(teste_naotrep) %>%
            summarise(casos = n()) %>%
            arrange(desc(casos)) %>%
            e_charts(x = teste_naotrep) %>%
            e_bar(serie = casos) %>%
            e_tooltip(trigger = "axis") %>%
            e_color(mycolors) %>%
            e_legend(show=FALSE) %>%
            e_title(
              text = "Teste não treponêmico",
              subtext = " ",
              left = "center"
            ) %>%
            e_animation(duration = 8000)
          
          
        })  
        

        output$treponemico <- echarts4r::renderEcharts4r({
          
          
          gestante %>%
            filter(ano == input$filtro4) %>% 
            filter(teste_trep != "Ignorado") %>% 
            filter(!is.na(teste_trep)) %>%  # Remove valores ausentes
            group_by(teste_trep) %>%
            summarise(casos = n()) %>%
            arrange(desc(casos)) %>%
            e_charts(x = teste_trep) %>%
            e_bar(serie = casos) %>%
            e_tooltip(trigger = "axis") %>%
            e_color(mycolors) %>%
            e_legend(show=FALSE) %>%
            e_title(
              text = "Teste treponêmico",
              subtext = " ",
              left = "center"
            ) %>%
            e_animation(duration = 8000)
          
          
        })  
        
        
        output$reg_saude_gest <- echarts4r::renderEcharts4r({
          
          
          gestante %>%
            filter(ano == input$filtro4) %>% 
            group_by(reg_saude) %>%
            summarise(casos = n()) %>%
            arrange(desc(casos)) %>%
            mutate(reg_saude = str_to_title(reg_saude)) %>%  # Transformando para minúsculas
            e_charts(x = reg_saude) %>%
            e_bar(serie = casos) %>%
            e_tooltip(trigger = "axis") %>%
            e_color(mycolors) %>%
            e_legend(show=FALSE) %>%
            e_title(
              text = "Região de Saúde",
              subtext = "Casos confirmados",
              left = "center") %>%
            e_animation(duration = 8000) %>%  
            e_x_axis(axisLabel = list(fontSize = 10)) %>%  # Ajuste o tamanho da fonte aqui
            e_format_y_axis(suffix = "", prefix = "",
                            formatter = e_axis_formatter(locale = "PT",digits = 0)) %>%  
            e_grid(left = "15%", right = "15%", bottom = "10%") %>%  # Ajuste as margens aqui
            e_flip_coords() 
          
          
          
        })  
        
        
       
        
        output$mapa_gest <- renderLeaflet({  
          
          # tabela de contagem 
          casos_gestante <- gestante %>% 
            filter(ano == input$filtro4) %>% #filtro
            group_by(municipio_residencia, ID_MN_RESI, ano_epi) %>% 
            summarise(casos = n()) 
          
          casos_gestante$codigo <- as.numeric(casos_gestante$ID_MN_RESI) # 6 digitos
          
          # Juntar as duas tabelas pelo município e ano
          taxa_deteccao <- casos_gestante %>%
            left_join(nascidos_vivos_long, by = c("municipio_residencia" = "municipio", "ano_epi" = "ano"))
          
          # Calcular a taxa de detecção
          taxa_deteccao <- taxa_deteccao %>%
            mutate(taxa_deteccao = (casos / nascidos_vivos) * 1000)
          
          
          # shape
          
          sc <- read_municipality(code_muni = 'SC', year=2018)
          sc$codigo <- substr(sc$code_muni,1,6) # retira o ultimo algarismo da variável code_muni 
          sc$codigo <- as.numeric(sc$codigo) 
          
          
          #join do shapefile e da tabela de contigencia
          taxa_deteccao <- left_join(sc,taxa_deteccao, by= c( "codigo"))
          
          # Inclui o valor 0 para os municipios que estao como NA (nesse caso n contabilizaram casos)
          taxa_deteccao <- taxa_deteccao %>%
            mutate(taxa_deteccao = replace_na(taxa_deteccao, 0))
          
          
          # bins e colors   -------------------------------------------------------
          
          #cria funcoes para estratificar a incidenia
          bins <- c(0,10,30,50, Inf)
          pal <- colorBin("YlOrRd", domain = taxa_deteccao$taxa_deteccao, bins=bins)
          
          
          label <- sprintf(
            "<strong>%s</strong><br/>%g Casos de Sífilis<br/>%g Taxa de Detecção",
            taxa_deteccao$municipio_residencia, taxa_deteccao$casos, taxa_deteccao$taxa_deteccao
          ) %>% 
            lapply(htmltools::HTML)
          
          
          map = leaflet(taxa_deteccao) %>%
            addTiles() %>%
            addProviderTiles(providers$CartoDB) 
          
          
          
          map %>% addPolygons(
            fillColor = ~pal(taxa_deteccao),
            weight = 2,
            opacity = 0, #opacidade da linha
            color= "white", #cor da linha
            dashArray = "1",
            fillOpacity = 0.7, # cor do fundo
            highlight = highlightOptions( #destacar o mapa quando passa o mouse por cima
              weight = 5,
              color = "#666",
              dashArray = "",
              fillOpacity = 0.7,
              bringToFront = TRUE),
            label = label,
            labelOptions = labelOptions(
              style = list("font-weight"= "normal", padding ="3px 8px"),
              textsize = "15px",
              direction = "auto"
            )) %>%
            addLegend(pal=pal,  #legenda
                      values = ~taxa_deteccao,
                      opacity = 0.7,
                      title = "Taxa de detecção (100 mil hab.)",
                      position = "bottomleft") 
          
        })
        
        
       
        
        
}
