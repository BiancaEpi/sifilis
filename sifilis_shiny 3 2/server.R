
########### SERVER ########################


function(input, output, session) {


# adquirida ---------------------------------------------------------------

  
      # serie temporal 
  
  output$adquirida_plot <- echarts4r::renderEcharts4r({
    dados_plot <- adquirida %>%
      filter(ano == input$ano_adquirida) %>% # input
      filter(classificacao == "Confirmado") %>% # filtro confirmados
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
        left = "center") %>%
      e_format_y_axis(suffix = "", prefix = "", formatter = e_axis_formatter(locale = "PT", digits = 0)) %>%
      e_animation(duration = 8000) %>% 
      e_toolbox_feature(feature = "saveAsImage")  # Adiciona o botão para baixar a imagem
  })
  
  
  #sexo
  
        
  output$sexo_adquirida <- echarts4r::renderEcharts4r({
            
               adquirida %>%
                filter(ano == input$ano_adquirida) %>% # input
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
                    left = "center") %>%
                e_format_y_axis(suffix = "",prefix = "",
                                formatter = e_axis_formatter(locale = "PT",digits = 0)) %>% 
                e_animation(duration = 8000)  %>% 
                e_toolbox_feature(feature = "saveAsImage")
    
        })
        
  #faixa etária    
        
        
  output$faixa_etaria_adquirida <- echarts4r::renderEcharts4r({
            
            adquirida %>%
                filter(ano == input$ano_adquirida) %>% # input
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
                    left = "center") %>%
                e_format_y_axis(suffix = "",prefix = "",
                                formatter = e_axis_formatter(locale = "PT",digits = 0)) %>% 
                e_animation(duration = 8000)  %>% 
                e_toolbox_feature(feature = "saveAsImage")
        })
        
  # Raça 
  
  output$raca_adquirida <- echarts4r::renderEcharts4r({
            
                adquirida %>%
                filter(ano == input$ano_adquirida) %>% # input
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
                    left = "center") %>%
                
                e_format_y_axis(suffix = "",prefix = "",
                                formatter = e_axis_formatter(locale = "PT",digits = 0)) %>% 
                e_animation(duration = 8000)  %>% 
                e_toolbox_feature(feature = "saveAsImage") %>% 
                e_flip_coords() 

        })
  
  # escolaridade      
        
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
                e_toolbox_feature(feature = "saveAsImage") %>% 
                e_format_y_axis(suffix = "",prefix = "",
                                formatter = e_axis_formatter(locale = "PT",digits = 0))
    
                
               # e_flip_coords() 
            
        })
        
        
# região de saude   
  
  output$regiao_adquirida <- echarts4r::renderEcharts4r({
            
                adquirida %>%
                filter(ano == input$ano_adquirida) %>% #input
                filter(classificacao == "Confirmado", !is.na(reg_saude)) %>%
                group_by(reg_saude) %>%
                summarise(casos = n(),
                          populacao_total = sum(populacao_residente, na.rm = TRUE)) %>%
                mutate(taxa_detecao = (casos / populacao_total) * 100000, # Calcular a taxa de detecção por 100.000 habitantes
                       reg_saude = str_to_title(reg_saude)) %>%  # Transformando para minúsculas
                       
                arrange(desc(casos)) %>% # ordena 
                # gráfico
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
                e_toolbox_feature(feature = "saveAsImage") %>% 
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
                                         pageLength = 10, # Define o número de linhas por página. #inserido wm 01/06/2024
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
          
          
          populacao_regiao <- populacao %>%
                 group_by(reg_saude) %>%
                 summarise(populacao_2022 = sum(pop_2022, na.rm = TRUE))
          
          tabela_regiao <- adquirida %>% 
            filter(ano == input$ano_adquirida) %>% 
            group_by(reg_saude) %>% 
            summarise(casos = sum(classificacao =="Confirmado", na.rm = T),
                      descartados = sum(classificacao =="Descartado", na.rm = T),
                      suspeito = sum(classificacao =="Suspeito", na.rm = T)) %>%
            left_join(populacao_regiao, by = "reg_saude") %>%
            mutate(
              reg_saude = str_to_title(reg_saude),  # Transformar os nomes da coluna reg_saude para minúsculas
              taxa_detecao = round(casos / populacao_2022 * 100000))  # Calcular a taxa de detecção por 100.000 habitantes
            
          
          
          DT::datatable(tabela_regiao,
                        
                        rownames= FALSE,
                        colnames = c (  "Região de Saúde",
                                        "Casos confirmados",
                                        "Casos descartados",
                                        "Casos Suspeitos", 
                                        "população",
                                        "Taxa de detecção"),
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

  # série temporal congênita
  
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
            e_line(serie = taxa_deteccao, name = "Taxa de incidência", y_index = 1) %>% # Adiciona a linha e o segundo eixo y
            e_y_axis( index = 1, name = "Taxa de incidência",axisLabel = list(formatter = '{value}')) %>%
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
                e_color("rgba(105, 105, 179, 0.7)") %>%  # Cor com transparência
            
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
                e_color("rgba(105, 105, 179, 0.7)") %>%  # Cor com transparência
            
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
            e_color("rgba(105, 105, 179, 0.7)") %>%  # Cor com transparência
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
            e_color("rgba(105, 105, 179, 0.7)") %>%  # Cor com transparência
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
            e_color("rgba(105, 105, 179, 0.7)") %>%  # Cor com transparência
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
            e_color("rgba(105, 105, 179, 0.7)") %>%  # Cor com transparência
            e_legend(show=FALSE) %>%
            e_title(
              text = "Teste não treponêmico no parto/curetagem - mãe",
              subtext = " ",
              left = "center") %>%
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
            e_color("rgba(105, 105, 179, 0.7)") %>%  # Cor com transparência
            e_legend(show=FALSE) %>%
            e_title(
              text = "Esquema de tratamento da mãe",
              subtext = " ",
              left = "center" ) %>%
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
            e_color("rgba(105, 105, 179, 0.7)") %>%  # Cor com transparência
            
            e_legend(show=FALSE) %>%
            e_title(
              text = "Diagnóstico clínico criança",
              subtext = " ",
              left = "center") %>%
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
            e_color("rgba(105, 105, 179, 0.7)") %>%  # Cor com transparência
            
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
            e_animation(duration = 8000) %>% e_toolbox_feature(feature = "saveAsImage") %>% 
            e_x_axis(axisLabel = list(fontSize = 10)) %>%  # Ajuste o tamanho da fonte aqui
            e_format_y_axis(suffix = "", prefix = "",
                            formatter = e_axis_formatter(locale = "PT",digits = 0)) %>%  
            e_grid(left = "15%", right = "15%", bottom = "10%") %>%  # Ajuste as margens aqui
            e_flip_coords() 
          
        })
        
        output$reg_saude_tab <- renderDataTable ({
          
###########  inclusion em 14.08.2024 - taxa de incidencia nascidos vivos ####
          
          # Unir as tabelas com base no nome do município
          sinasc_regiao <- sinasc_municipios %>%
            left_join(regiao %>% select(NM_MUNICIP, reg_saude), 
                      by = c("municipio" = "NM_MUNICIP"))
          
          # Somar os nascidos vivos por região de saúde
          resultado <- sinasc_regiao %>%
            group_by(reg_saude) %>%
            summarise(across(starts_with("20"), sum, na.rm = TRUE))
          
          # Pivotar a tabela de nascidos vivos
          resultado_pivot <- resultado %>%
            pivot_longer(cols = starts_with("20"), # Pivota as colunas que começam com "20"
                         names_to = "ano", 
                         values_to = "populacao")  %>%
             mutate(ano = as.numeric(ano))  # Converte o ano para numérico
          #
          
        
tabela_regiao <- congenita %>% 
           # filter(ano == input$filtro2) %>%  # Aplicar o filtro de ano
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
         

tabela_regiao <- tabela_regiao %>% 
  filter(ano == input$filtro2)  
          
 DT::datatable(tabela_regiao,
                        
                        rownames= FALSE,
                        colnames = c (  "Região de Saúde",
                                        "Cong. recente",
                                        "Cong. tardia",
                                        "Aborto",
                                        "Natimorto", 
                                        "Casos",
                                       "Ano", "Nasc. vivos", "Tx incidência" ),
                        extensions = "Buttons",
                        options = list(dom = "Blfrtip", 
                                       pageLength = 5, # Define o número de linhas por página. #inserido wm 01/06/2024
                                       scrollX = TRUE,  # coloca o scroll no eixo x                                          
                                       
                                       buttons = c('excel'),
                                       lengthMenu = list(c(5,10, 25, -1), c(5,10, 25, "Todos")),
                                       language = list(paginate = 
                                                         list('next'="Próximo",
                                                              'previous'="Anterior"),
                                                       search = "Pesquisar:")))
 
 
        })
        
        
        
# Tabela municipio congenita
        
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
                          "Nascidos vivos", # inserido em 14.08
                          "Tx incidência"),
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
        bins <- c(0,10,20,30, Inf)
        pal <- colorBin("YlOrRd", domain = taxa_deteccao$taxa_deteccao, bins=bins)
        
        
        label <- sprintf(
          "<strong>%s</strong><br/>%g Casos de Sífilis<br/>%g Taxa de incidência",
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
                    title = "Taxa de incidência (1.000 nasc vivos)",
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
                text = "Esquema de tratamento prescrito a gestante",
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
              text = "Classificação Clínica",
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
              text = "Teste não treponêmico no pré-natal",
              subtext = " ",
              left = "center"
            ) %>%
            e_animation(duration = 8000)
          
          
        })  
        
        
        output$idade_gestac <- echarts4r::renderEcharts4r({
          
          
          gestante %>%
            filter(ano == input$filtro4) %>% 
            filter(idade_gestacional != "Ignorado") %>% 
            filter(!is.na(idade_gestacional)) %>%  # Remove valores ausentes
            group_by(idade_gestacional) %>%
            summarise(casos = n()) %>%
            arrange(desc(casos)) %>%
            e_charts(x = idade_gestacional) %>%
            e_bar(serie = casos) %>%
            e_tooltip(trigger = "axis") %>%
            e_color(mycolors) %>%
            e_legend(show=FALSE) %>%
            e_title(
              text = "Idade Gestacional",
              subtext = " ",
              left = "center") %>%
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
              text = "Teste treponêmico no pré-natal",
              subtext = " ",
              left = "center"
            ) %>%
            e_animation(duration = 8000)
          
          
        })  
        
        
        output$trat_parceiro <- echarts4r::renderEcharts4r({
          
          
          gestante %>%
            filter(ano == input$filtro4) %>% 
            #filter(TRATPARC != "Ignorado") %>% 
            filter(!is.na(TRATPARC)) %>%  # Remove valores ausentes
            mutate(TRATPARC = case_when(
                TRATPARC == 1 ~ "Sim",
                TRATPARC == 2 ~ "Não",
                TRATPARC == 9 ~ "Ignorado",
                TRUE ~ as.character(TRATPARC)  # Mantém outros valores, se houver
              )) %>%
            group_by(TRATPARC) %>%
            summarise(casos = n()) %>%
            arrange(desc(casos)) %>%
            e_charts(x = TRATPARC) %>%
            e_bar(serie = casos) %>%
            e_tooltip(trigger = "axis") %>%
            e_color(mycolors) %>%
            e_legend(show=FALSE) %>%
            e_title(
              text = "Parceiro tratado concomitantemente à gestante",
              subtext = " ",
              left = "center"
            ) %>%
            e_animation(duration = 8000)
          
          
        })  
        
        
        output$esquema_parceiro <- echarts4r::renderEcharts4r({
          
          
          gestante %>%
            filter(ano == input$filtro4) %>% 
            #filter(TRATPARC != "Ignorado") %>% 
            filter(!is.na(TPESQPAR)) %>%  # Remove valores ausentes
            mutate(TPESQPAR = case_when(
              TPESQPAR == 1 ~ "P.B 2.400.000 ui",
              TPESQPAR == 2 ~ "P.B 4.800.000 ui",
              TPESQPAR == 3 ~ "P.B 7.200.000 ui",
              TPESQPAR == 4 ~ "Outro esquema",
              TPESQPAR == 5 ~ "Não realizado",
              TPESQPAR == 9 ~ "Ignorado",
              
              TRUE ~ as.character(TPESQPAR)  # Mantém outros valores, se houver
            )) %>%
            group_by(TPESQPAR) %>%
            summarise(casos = n()) %>%
            arrange(desc(casos)) %>%
            e_charts(x = TPESQPAR) %>%
            e_bar(serie = casos) %>%
            e_tooltip(trigger = "axis") %>%
            e_color(mycolors) %>%
            e_legend(show=FALSE) %>%
            e_title(
              text = "Esquema de tratamento prescrito ao parceiro",
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
            "<strong>%s</strong><br/>%g Casos de Sífilis<br/>%g Taxa de Incidência",
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
                      title = "Taxa de incidência (1.000 nasc vivos)",
                      position = "bottomleft") 
          
        })
   
###########  inclusion em 14.08.2024      
    # tabela regiao de saude gestante 
        
        output$tab_1_reg_saude <- renderDataTable ({
        
        # Unir as tabelas com base no nome do município
        sinasc_regiao <- sinasc_municipios %>%
          left_join(regiao %>% select(NM_MUNICIP, reg_saude), 
                    by = c("municipio" = "NM_MUNICIP"))
        
        # Somar os nascidos vivos por região de saúde
        resultado <- sinasc_regiao %>%
          group_by(reg_saude) %>%
          summarise(across(starts_with("20"), sum, na.rm = TRUE))
        
        # Pivotar a tabela de nascidos vivos
        resultado_pivot <- resultado %>%
          pivot_longer(cols = starts_with("20"), # Pivota as colunas que começam com "20"
                       names_to = "ano", 
                       values_to = "populacao")  %>%
          mutate(ano = as.numeric(ano))  # Converte o ano para numérico
        
        
        
        tabela_regiao <- gestante %>% 
          group_by(reg_saude) %>% 
          summarise(casos = sum(CLASSI_FIN== "1", na.rm = TRUE),
                    descartados = sum(CLASSI_FIN == "2", na.rm = TRUE)) %>% 
          left_join(resultado_pivot, by = "reg_saude") %>%  # Unir com a tabela de nascidos vivos
          mutate(
            reg_saude = str_to_title(reg_saude),  # Transformar os nomes da coluna reg_saude para minúsculas
            taxa_detecao = round(casos / populacao * 1000)  # Calcular a taxa de detecção por 100.000 habitantes
          )
        
        
        tabela_regiao <- tabela_regiao %>% 
          filter(ano == input$filtro4)  # filtro output
        
        DT::datatable(tabela_regiao,
                      
                      rownames= FALSE,
                      colnames = c (  "Região de Saúde",
                                      "Casos", "Descartados",
                                      "Ano", "Nasc. vivos", "Tx incidência" ),
                      extensions = "Buttons",
                      options = list(dom = "Blfrtip", 
                                     pageLength = 5, # Define o número de linhas por página. #inserido wm 01/06/2024
                                     scrollX = TRUE,  # coloca o scroll no eixo x                                          
                                     
                                     buttons = c('excel'),
                                     lengthMenu = list(c(5,10, 25, -1), c(5,10, 25, "Todos")),
                                     language = list(paginate = 
                                                       list('next'="Próximo",
                                                            'previous'="Anterior"),
                                                     search = "Pesquisar:")))
        
        
})
       
        
    # tabela por município residência
        
        output$tab_2_muni <- renderDataTable ({
          
          # tabela de contagem 
          casos_gestante <- gestante %>% 
          filter(ano == input$filtro4) %>% #filtro
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
          
          
          # datatable      
          DT::datatable(tab_muni_gest,
                        
                        rownames= FALSE,
                        colnames = c (
                          "Município de residência",
                          "Ano",
                          "Casos",
                          "Descartados",
                          "Nascidos vivos", # inserido em 14.08
                          "Tx incidência"),
                        extensions = "Buttons",
                        options = list(dom = "Blfrtip", 
                                       pageLength = 5, # Define o número de linhas por página. #inserido wm 01/06/2024
                                       scrollX = TRUE,  # coloca o scroll no eixo x                                          
                                       
                                       buttons = c('excel'),
                                       lengthMenu = list(c(5, 25, 50, -1), c(3, 25, 50, "Todos")),
                                       language = list(paginate = 
                                                         list('next'="Próximo",
                                                              'previous'="Anterior"),
                                                       search = "Pesquisar:")
                        ))
        })
        


# Outras análises  --------------------------------------------------------

## gráfico 1: serie temporal do estado
        
        output$plot1 <- echarts4r::renderEcharts4r({
          
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
            e_line(adquirida, name = "Adquirida") %>%
            e_line(gestante, name = "Gestante") %>%
            e_line(congenita, name = "Congênita") %>%
            e_title("Casos Confirmados de Sífilis") %>%
            e_tooltip(trigger = "axis") %>%
            e_legend(right = "10%") %>%
            e_color(mycolors) %>%
            e_x_axis(name = "Ano Epidemiológico", type="category") %>%
            e_y_axis(name = "Número de Casos") %>%
            e_format_y_axis(suffix = "", prefix = "", formatter = e_axis_formatter(locale = "PT", digits = 0)) %>%
            e_animation(duration = 8000)
          
          
        })
        
      
## gráfico 2: série temporal por região de saúde 
          
        # Carregar os dados combinados
        dados_combinados <- reactive({
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
          gestante_confirmados %>%
            full_join(congenita_confirmados, by = c("reg_saude", "ano_epi")) %>%
            full_join(adquirida_confirmados, by = c("reg_saude", "ano_epi")) %>%
            replace_na(list(gestante = 0, congenita = 0, adquirida = 0))
        })
        
        # Atualizar as escolhas do selectInput com as regiões de saúde disponíveis
        observe({
          updateSelectInput(session, "regiao_saude",
                            choices = unique(dados_combinados()$reg_saude))
        })
        
        output$plot2 <- echarts4r::renderEcharts4r({
          
          # Filtrar os dados com base na região de saúde selecionada
          dados_filtrados <- dados_combinados() %>%
            filter(reg_saude == input$regiao_saude)
          
          # Verificar se a região selecionada tem dados
          if (nrow(dados_filtrados) == 0) {
            return(NULL)
          }
          
          # Gerar o gráfico para a região selecionada
          dados_filtrados %>%
            e_charts(ano_epi) %>%
            e_line(gestante, name = "Gestante") %>%
            e_line(congenita, name = "Congênita") %>%
            e_line(adquirida, name = "Adquirida") %>%
            e_title(text = input$regiao_saude, subtext = "") %>%
            e_tooltip(trigger = "axis") %>%
            e_x_axis(name = "Ano Epidemiológico", type = "category") %>%
            e_y_axis(name = "Número de casos") %>%
            e_color(mycolors) %>%
            e_legend(show = TRUE) %>% 
            e_legend(right = "10%") %>%
            e_format_y_axis(suffix = "", prefix = "", formatter = e_axis_formatter(locale = "PT", digits = 0)) %>%
            e_animation(duration = 8000)
        })
         

          
        
        
}
