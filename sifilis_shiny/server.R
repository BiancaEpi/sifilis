
########### SERVER ########################


function(input, output, session) {


# adquirida ---------------------------------------------------------------

  
        output$adquirida_plot <- echarts4r::renderEcharts4r({
            
                adquirida %>%
                filter(ano == input$ano_adquirida) %>% 
                filter(classificacao == "Confirmado") %>%
                group_by(mes_diag) %>%
                summarise(casos = n()) %>%
                e_charts(x = mes_diag) %>%
                e_bar(serie = casos) %>%
                e_tooltip(trigger = "axis") %>%
                e_color(mycolors) %>%
                e_legend(show=FALSE) %>%
                e_title(
                    text = "Casos de Sífilis Adquirida",
                    subtext = "Segundo mês de início de sintomas",
                    left = "center"
                ) %>%
                e_format_y_axis(suffix = "",prefix = "",
                                formatter = e_axis_formatter(locale = "PT",digits = 0)) %>% 
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
                e_color(mycolors) %>%
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
                e_color(mycolors) %>%
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
                e_color(mycolors) %>%
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
                e_color(mycolors) %>%
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
                e_charts(x = reg_saude) %>%
                e_bar(serie = casos) %>%
                e_tooltip(trigger = "axis") %>%
                e_color(mycolors) %>%
                e_legend(show=FALSE) %>%
                e_title(
                    text = "Região de Saúde",
                    subtext = "Casos confirmados",
                    left = "center"
                ) %>%
                e_animation(duration = 8000) %>%  
                e_format_y_axis(suffix = "",prefix = "",
                                formatter = e_axis_formatter(locale = "PT",digits = 0)) %>%  
            
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
                mutate(incidencia = (casos/populacao_residente)*100000) %>%
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
            bins <- c(0,50,100,300,500,1000,3000, Inf)
            pal <- colorBin("YlOrRd", domain = casos_adquirida$incidencia, bins=bins)
            
            
            #customizando infos # criando label
            label <- sprintf(
                "<strong>%s</strong></br>%g Incidência</br>",
                casos_adquirida$nom_municipio, casos_adquirida$incidencia
            ) %>%  lapply(htmltools::HTML)
            
            
            
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
        
        
        # tabela residencia -------------------------------------------------------
        
        output$tabela_res_adq <- renderDataTable ({
            
            tabela_municipio_residencia <- adquirida %>% 
                group_by(municipio_residencia, populacao_residente) %>%
                summarise(casos = sum(classificacao =="Confirmado", na.rm = T),
                          descartados = sum(classificacao =="Descartado", na.rm = T),
                          suspeito = sum(classificacao =="Suspeito", na.rm = T)) %>% 
                
                
                mutate(incidencia_geral = round(casos/populacao_residente*100000)) %>%
                select(municipio_residencia, casos, 
                       descartados, suspeito,
                       incidencia_geral)
            
            
            
            
            DT::datatable(tabela_municipio_residencia,
                          
                          rownames= FALSE,
                          colnames = c ( "Município de residência" ,
                                         "Casos confirmados" ,
                                         "Casos descartados",
                                         "Casos Suspeitos",
                                         "Incidência"),
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
            
            congenita %>%
                dplyr::group_by(mes_diag) %>%
                summarise(casos= n()) %>%
                e_charts(x= mes_diag) %>%
                e_bar(serie= casos) %>%
                e_tooltip(trigger = "axis") %>%
                e_color(mycolors) %>%
                e_legend(show=FALSE) %>%
                e_title(text= "Casos de Sífilis congênita",
                        subtext = "Segundo mês de diagnóstico",
                        left = "center") %>%
                e_animation(duration = 8000)
            
        })      
        
        
        output$sexo_congenita <- echarts4r::renderEcharts4r({
            
                congenita %>%
                #filter(ano == input$ano_adquirida) %>% 
                #filter(classificacao == "Confirmado") %>%
                filter(sexo != "Ignorado") %>% 
                group_by(sexo) %>%
                summarise(casos = n()) %>%
                e_charts(x = sexo) %>%
                e_bar(serie = casos) %>%
                e_tooltip(trigger = "axis") %>%
                e_color(mycolors) %>%
                e_legend(show=FALSE) %>%
                e_title(
                    text = "Sexo",
                    subtext = "",
                    left = "center"
                ) %>%
                e_animation(duration = 8000)
        })  
        

        output$faixa_etaria_congenita <- echarts4r::renderEcharts4r({

               congenita %>%
                #filter(ano == input$ano_adquirida) %>%
                #filter(classificacao == "Confirmado") %>%
                group_by(faixa_etaria) %>%
                summarise(casos = n()) %>%
                e_charts( faixa_etaria) %>%
                e_pie(casos, radius = c("50%", "70%"),legend = FALSE) |> 
                e_title("Idade")  %>%
                e_color(mycolors) %>% 
                e_animation(duration = 8000)
                
            #     e_bar(serie = casos) %>%
            #     e_tooltip(trigger = "axis") %>%
            #     e_color(mycolors) %>%
            #     e_legend(orientation = "vertical", right = "5%", top = "15%") %>%
            #     e_title(
            #         text = "Grupos de idade",
            #         subtext = " ",
            #         left = "center"
            #     ) %>%
            #     e_animation(duration = 8000)
            # 
            # 
            # mtcars |> 
            #     head() |> 
            #     tibble::rownames_to_column("model") |> 
            #     e_charts(model) |> 
            #     e_pie(carb) |> 
            #     e_title("Pie chart")
            # 
            
        })


        
        
        
        output$evolucao_congenita <- echarts4r::renderEcharts4r({
            
                 congenita %>%
                #filter(ano == input$ano_adquirida) %>% 
                filter(evolucao != "Ignorado") %>% 
                group_by(evolucao) %>%
                summarise(casos = n()) %>%
                arrange(desc(casos)) %>%
                e_charts(x = evolucao) %>%
                e_bar(serie = casos) %>%
                e_tooltip(trigger = "axis") %>%
                e_color(mycolors) %>%
                e_legend(show=FALSE) %>%
                e_title(
                    text = "Evolução",
                    subtext = " ",
                    left = "center"
                ) %>%
                e_animation(duration = 8000)
        }) 

        
        
        
        
# gestante ----------------------------------------------------------------

        
        
        output$gestante_plot <- echarts4r::renderEcharts4r({
        
            gestante %>%
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
        
        
      
        
        
        
}
