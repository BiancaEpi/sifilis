#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)


# Define UI
ui <- navbarPage(
    "Painel de Monitoramento da Sífilis",
    

# adquirida ---------------------------------------------------------------

    
    
    tabPanel("Sífilis Adquirida",
             fluidPage(
                 fluidRow(
                     column(width = 4,
                            selectInput("ano_adquirida", "Ano epidemiológico:", 
                                        choices = unique(sort(adquirida$ano_epi)),
                                        selected = "2022"))),
                 
                 fluidRow(
                     column(width = 12,
                            echarts4rOutput("adquirida_plot", height = 500))),
                 hr(),
                 fluidRow(
                     column(width = 6, echarts4rOutput("sexo_adquirida", height = 300)),
                     column(width = 6, echarts4rOutput("faixa_etaria_adquirida", height = 300))),
                 hr(),
                 fluidRow(
                     column(width = 6, echarts4rOutput("raca_adquirida", height = 300)),
                     column(width = 6, echarts4rOutput("escola_adquirida", height = 300))),
                 hr(),
                 fluidRow(
                     column(width = 6, echarts4rOutput("regiao_adquirida", height = 400)),
                     column(width = 6, DTOutput("tabela_regiao_adq", height = 400))),
             
                 hr(),
                 fluidRow(column(width = 6, leafletOutput("mapa_adq", height = 500)),
                          column(width = 6, DTOutput("tabela_res_adq", height = 600)))
             )),
    

# congênita ---------------------------------------------------------------


    
    tabPanel("Sífilis Congênita",
             fluidPage(
                 fluidRow(
                     column(width = 4,
                            selectInput("filtro2", "Ano epidemiológico",
                                        choices = unique(sort(congenita$ano_epi)),
                                        selected = "2022")
                     )
                 ),
                 
                 fluidRow(
                     column(width = 12,
                            echarts4rOutput("congenita_plot", height = 400))),
                 hr(),

                 fluidRow(
                     column(width = 6, echarts4rOutput("sexo_congenita", height = 300)),
                     column(width = 6, echarts4rOutput("faixa_etaria_congenita", height = 300))),
                 hr(),
                 fluidRow(
                     column(width = 6, echarts4rOutput("escola_mae", height = 300)),
                     column(width = 6, echarts4rOutput("faixa_etaria_mae", height = 300))),
                 hr(),
                 fluidRow(
                     column(width = 6, echarts4rOutput("pre_natal", height = 300)),
                     column(width = 6, echarts4rOutput("nao_trepo_mae", height = 300))),
                 hr(),
                 
                 fluidRow(
                     column(width = 6, echarts4rOutput("diagnostico_mae", height = 300)),
                     column(width = 6, echarts4rOutput("tratamento_mae", height = 300))),
                 hr(),
                 fluidRow(
                     column(width = 6, echarts4rOutput("diag_crianca", height = 300)),
                     column(width = 6, echarts4rOutput("diag_definit", height = 300))),
                 hr(),
                 fluidRow(
                     column(width = 6, echarts4rOutput("reg_saude_plot", height = 300)),
                     column(width = 6, DTOutput("reg_saude_tab", height = 300))),
                 hr(),
                 fluidRow(
                     column(width = 6, leafletOutput("mapa_cong", height = 400)),
                     column(width = 6, DTOutput("tab_muni_cong", height = 400)))
                 
                 
                 )),
                 
                 

# gestantes ---------------------------------------------------------------


                 
    tabPanel("Sífilis em Gestantes",
             fluidPage(
                 fluidRow(
                     column(width = 4,
                            selectInput("filtro4", "Ano Epidemiológico", 
                                        choices = unique(sort(gestante$ano_epi)),
                                        selected = "2022"))),
                 
                 fluidRow(
                     column(width = 12,
                            echarts4rOutput("gestante_plot", height = 400))),
                 hr(),
                 fluidRow(
                     column(width = 4, echarts4rOutput("faixa_etaria_gestante", height = 300)),
                     column(width = 4, echarts4rOutput("idade_gestac", height = 300)),
                     column(width = 4, echarts4rOutput("escola_gestante", height = 300))),
                 hr(),
                 fluidRow(
                     column(width = 6, echarts4rOutput("tratamento_gestante", height = 300)),
                     column(width = 6, echarts4rOutput("clinica_gestante", height = 300))),
                 hr(),
                 fluidRow(
                     column(width = 6, echarts4rOutput("nao_trepo", height = 300)),
                     column(width = 6, echarts4rOutput("treponemico", height = 300))),
                 hr(),
                 fluidRow(
                     column(width = 6, echarts4rOutput("reg_saude_gest", height = 400)),
                     column(width = 6, leafletOutput("mapa_gest", height = 400))),
                
                 ## incluido em 14.08.2024 ###
                 hr(),
                 fluidRow(
                     column(width = 6, DTOutput("tab_1_reg_saude", height = 400)),
                     column(width = 6, DTOutput("tab_2_muni", height = 400)))
                 
             )),

tabPanel("Outras análises",
         fluidPage(
             fluidRow(
                 column(width = 12, echarts4rOutput("plot1", height = 400))),
             fluidRow(     
                 column(width = 12, echarts4rOutput("plot2", height = 400)))
                 
                 )
             )

)