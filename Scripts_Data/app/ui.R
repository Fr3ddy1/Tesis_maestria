shinyUI(
  dashboardPage(
    #Header
    dashboardHeader(title = NULL, titleWidth = 188,
              dropdownMenu(type = "messages",
                           messageItem(
                                      from = "Alerta",
                                      message = "Niveles de Riesgo Atípicos",
                                      icon = icon("exclamation-triangle"),
                                      time = "2018-05-12"
                                      ),#final messageitem
                          
                          messageItem(
                                     from = "Señal",
                                     message = "Volatilidad Anormal",
                                     icon = icon("life-ring"),
                                     time = "2018-05-12"
                                      )#final messageitem
                          )#final dropdownmenu
                  ),#final dashboardheader
    #Sidebar
    dashboardSidebar(
      
                sidebarSearchForm(label = "Ingrese un Número", "searchText", "searchButton"),
              
                sidebarMenu(id = "tabs",
                          
                menuItem("Curva de Rendimiento", icon = icon("bar-chart-o"),
                         
                            menuSubItem("Nelson y Siegel", tabName = "subitem1", icon = icon("circle-o")),
                            
                            menuSubItem("Svensson", tabName = "subitem2", icon = icon("circle-o")),
                            
                            menuSubItem("Diebold-Li", tabName = "subitem3", icon = icon("circle-o")),
                            
                            menuSubItem("Splines", tabName = "subitem4", icon = icon("circle-o"))
                       
                          ),#fin menuitem 
                
                            #menuItem("Comparativo", icon = icon("circle-o"), tabName = "comparativo"),
                menuItem("Comparativo", icon = icon("bar-chart-o"), 
                         
                            menuSubItem("Datos", tabName = "datos", icon = icon("circle-o")),
                         
                            menuSubItem("Metodologías", tabName = "metodologias", icon = icon("circle-o")),
                         
                            menuSubItem("Precios estimados", tabName = "precios", icon = icon("circle-o")),
                         
                            menuSubItem("Curvas", tabName = "curvas", icon = icon("circle-o"))
                         
                          )#fin menuitem 
               
                       
                )
                  
                ), #final dashboardsidebar
    #Body
    dashboardBody(VisionHeader(),
            tabItems(
              #NELSON Y SIEGEL
              
               tabItem(tabName = "subitem1",
                 fluidRow(    h2("Nelson y Siegel"),
                         fluidRow(column(width = 6,box( width = 12, background = "navy",
                                                          dateInput(inputId="n2", label="Por favor, seleccionar una fecha", language= "es",
                                                                    width = "100%")#final dateimput 
                                                      )#final box
                                          ),#final column
                                  box( width = 6,height = 2,title = "Fecha de valoración: ",verbatimTextOutput('p2')) #final box
                                  ),#final fluidrow
                          h2("  Títulos"), h5("  Favor seleccionar los títulos a considerar: "),
                          fluidRow(tabBox( width = 12, title = "Títulos", id = "tab1", height = "50px", 
                                     tabPanel("TIF",
                                              fluidRow(column(width = 3,checkboxGroupInput( inputId = "t1_ns", label = " ",
                                                              choices=tit[1:7])#final checkboxgroupimput
                                                              ),#final column
                                                       column(width = 3,checkboxGroupInput( inputId = "t2_ns", label = " ",
                                                              choices=tit[8:13])#final checkboxgroupimput
                                                              ),#final column
                                                       column(width = 3,checkboxGroupInput( inputId = "t3_ns", label = " ",
                                                              choices=tit[14:19])#final checkboxgroupimput
                                                              ),#final column
                                                       column(width = 3,checkboxGroupInput( inputId = "t4_ns", label = " ",
                                                              choices=tit[20:25])#final checkboxgroupimput
                                                              ) #final column
                                                       ),#final fluidrow
                                                        verbatimTextOutput("q1_ns"),h2(" Precios Promedios"),verbatimTextOutput("pre1_ns"),
                                                        
                                              #withMathJax(),
                                              #helpText('\\(\\beta_{0}\\) \\(\\beta_{1}\\) \\(\\beta_{2}\\) \\(\\tau_{1}\\) '),
                                              #uiOutput('ex1')
                                              
                                                        h2(" Características"),box(style="overflow-x:scroll",width = 12,dataTableOutput("Ca_ns")),
                                                        h2(" Parámetros"), 
                                              fluidRow(
                                                        tabBox( width = 12, title = "Parámetros", id = "tab1_ns_tif", height = "50px", 
                                                       
                                                        tabPanel(" Parámetros Iniciales ",
                                                        verbatimTextOutput("pa_tif_ns"),
                                                        h2(" Precios estimados iniciales"),box(style="overflow-x:scroll",width = 12,dataTableOutput("p_est_tif_ns")),
                                                        h2(" Curva de rendimientos inicial TIF"),
                                                        plotOutput("c_tif_ns")
                                                        
                                                        ),# final tabpanel pa iniciales 
                                                        
                                                        tabPanel(" Elegir Parámetros",
                                                                 # verbatimTextOutput("pa_tif_ns_el"),
                                                                 # h2(" Precios estimados"),dataTableOutput("p_est_tif_ns_el"),
                                                                 # h2(" Curva de rendimientos TIF"),
                                                                 # plotOutput("c_tif_ns_el")
                                                                 h4(" Por favor, insertar parámetros"),
                                                                 box(width=12,title="Importante",status="primary",solidHeader=TRUE ,collapsible = TRUE,
                                                                     collapse= TRUE,"Al ingresar los parámetros considere las siguientes restricciones, ",br(),withMathJax(helpText("$$1) \\quad \\beta_{0} > 0$$")),
                                                                     withMathJax(helpText("$$2) \\quad \\beta_{0}+\\beta_{1} > 0$$")),withMathJax(helpText("$$3) \\quad \\tau > 0$$"))),#final box
                                                                 column(width = 3,numericInput( inputId = "ns_b0_tif", label="B0: ", min = 0, max = 50,step = 0.1, value = 5, width = "40%")
                                                                        , verbatimTextOutput("num_ns_b0_tif")),#final column,
                                                                 column(width = 3,numericInput( inputId = "ns_b1_tif", label="B1: ", min = -10, max = 50,step = 0.1, value = 5, width = "40%"),
                                                                        verbatimTextOutput("num_ns_b1_tif")),#final column
                                                                 column(width = 3,numericInput( inputId = "ns_b2_tif", label="B2: ", min = -10, max = 50,step = 0.1, value = 5, width = "40%"),
                                                                        verbatimTextOutput("num_ns_b2_tif")),#final column
                                                                 column(width = 3,numericInput( inputId = "ns_t_tif", label="T: ", min = 0, max = 50,step = 0.1, value = 5, width = "40%"),
                                                                        verbatimTextOutput("num_ns_t_tif")),#final column
                                                                 h4(" Los nuevos parámetros considerados son, "),
                                                                 verbatimTextOutput("new_ns_tif"),
                                                                 h4(" Verificación, "),
                                                                 verbatimTextOutput("ver_ns_tif"),
                                                                 h2(" Precios estimados"),box(style="overflow-x:scroll",width = 12,dataTableOutput("p_est_tif_ns_el")),
                                                                 h4(" Curva de Rendimiento"),
                                                                 plotOutput("c_tif_ns1_new")
                                                                 ),# final tabpanel pa elegir
                                                         
                                                        tabPanel(" Parámetros Optimizados",
                                                                 # verbatimTextOutput("pa_tif_ns_el"),
                                                                 # h2(" Precios estimados iniciales"),dataTableOutput("p_est_tif_ns_el"),
                                                                 # h2(" Curva de rendimientos inicial"),
                                                                 # plotOutput("c_tif_ns_el")
                                                                 h2(" Precios estimados optimizados"),
                                                                 radioButtons( inputId = "opt_tif_ns",label = "Desea optimizar los precios obtenidos:", 
                                                                               choices = c("Si"=1, "No"=0),
                                                                               selected=0), #finalradiobuttons
                                                                 box(style="overflow-x:scroll",width = 12,dataTableOutput("p_est_tif_opt_ns")),
                                                                 h2(" Parámetros optimizados"),
                                                                 verbatimTextOutput("par_tif_ns_op"),
                                                                 h2(" Curva de rendimientos TIF"),
                                                                 plotOutput("c_tif_ns_op")
                                                                  )#final tabpanel
                                                               
                                                                 )#final tabbox
                                              )#final fluidrow
                                                        
                                              ),#final tabpanel tif
                                  
                                           tabPanel("VEBONO",fluidRow(
                                                      column(width = 3,checkboxGroupInput(inputId = "v1_ns", label = " ",
                                                                choices=tit1[1:8])#final checkboxgroupimput
                                                             ),#final column
                                                      column(width = 3,checkboxGroupInput( inputId = "v2_ns", label = " ",
                                                                choices=tit1[9:16])#final checkboxgroupimput
                                                             ),#final column
                                                      column(width = 3,checkboxGroupInput( inputId = "v3_ns",label = " ",
                                                                choices=tit1[17:24])#final checkboxgroupimput
                                                             ),#final column
                                                      column(width = 3,checkboxGroupInput( inputId = "v4_ns", label = " ",
                                                                choices=tit1[25:29])#final checkboxgroupimput
                                                             )#final column
                                                                ),#final fluidrow
                                                        verbatimTextOutput("q2_ns"), h2(" Precios Promedios"),verbatimTextOutput("pre2_ns"),
                                                       
                                                        h2(" Características"),box(style="overflow-x:scroll",width = 12,dataTableOutput("Ca1_ns")),
                                                    fluidRow(
                                                    tabBox( width = 12, title = "Parámetros", id = "tab1_ns_veb", height = "50px", 
                                                            tabPanel(" Parámetros Iniciales ",
                                                        h2(" Parámetros iniciales"),verbatimTextOutput("pa_veb_ns") ,
                                                        h2(" Precios estimados iniciales"),box(style="overflow-x:scroll",width = 12,dataTableOutput("p_est_veb_ns")),
                                                        h2(" Curva de rendimientos inicial"),
                                                        plotOutput("c_veb_ns")
                                                            ),#final tabpanel iniciales
                                                        tabPanel(" Elegir Parámetros ",
                                                                 # h2(" Parámetros iniciales"),verbatimTextOutput("pa_veb_ns_el") ,
                                                                 # h2(" Precios estimados iniciales"),dataTableOutput("p_est_veb_ns_el"),
                                                                 # h2(" Curva de rendimientos inicial"),
                                                                 # plotOutput("c_veb_ns_el")
                                                                 h4(" Por favor, insertar parámetros"),
                                                                 box(width=12,title="Importante",status="primary",solidHeader=TRUE ,collapsible = TRUE,
                                                                     collapse= TRUE,"Al ingresar los parámetros considere las siguientes restricciones, ",br(),withMathJax(helpText("$$1) \\quad \\beta_{0} > 0$$")),
                                                                     withMathJax(helpText("$$2) \\quad \\beta_{0}+\\beta_{1} > 0$$")),withMathJax(helpText("$$3) \\quad \\tau > 0$$"))),#final box
                                                                 column(width = 3,numericInput( inputId = "ns_b0_veb", label="B0: ", min = 0, max = 50,step = 0.1, value = 5, width = "40%")
                                                                        , verbatimTextOutput("num_ns_b0_veb")),#final column,
                                                                 column(width = 3,numericInput( inputId = "ns_b1_veb", label="B1: ", min = -10, max = 50,step = 0.1, value = 5, width = "40%"),
                                                                        verbatimTextOutput("num_ns_b1_veb")),#final column
                                                                 column(width = 3,numericInput( inputId = "ns_b2_veb", label="B2: ", min = -10, max = 50,step = 0.1, value = 5, width = "40%"),
                                                                        verbatimTextOutput("num_ns_b2_veb")),#final column
                                                                 column(width = 3,numericInput( inputId = "ns_t_veb", label="T: ", min = 0, max = 50,step = 0.1, value = 5, width = "40%"),
                                                                        verbatimTextOutput("num_ns_t_veb")),#final column
                                                                 h4(" Los nuevos parámetros considerados son, "),
                                                                 verbatimTextOutput("new_ns_veb"),
                                                                 h4(" Verificación, "),
                                                                 verbatimTextOutput("ver_ns_veb"),
                                                                 h2(" Precios estimados"),box(style="overflow-x:scroll",width = 12,dataTableOutput("p_est_veb_ns_el")),
                                                                 h4(" Curva de Rendimiento"),
                                                                 plotOutput("c_veb_ns1_new")
                                                        ),#final tabpanel elegir
                                                        tabPanel(" Parámetros Optimizados ",
                                                        h2(" Precios estimados optimizados"),
                                                        radioButtons( inputId = "opt_veb_ns",label = "Desea optimizar los precios obtenidos:", 
                                                                      choices = c("Si"=1, "No"=0),
                                                                      selected=" "),#final radiobuttons
                                                        box(style="overflow-x:scroll",width = 12,dataTableOutput("p_est_veb_opt_ns")),
                                                        h2(" Parámetros optimizados"),
                                                        verbatimTextOutput("par_veb_ns_op"),
                                                        h2(" Curva de rendimientos"),
                                                        plotOutput("c_veb_ns_op")
                                                        )#final tabpanel
                                                        )#final tabbox
                                                        
                                                        )#final tabitem vebono,
                                           )#final fluidrow
                                              
                                        )#final tabbox
                                  )#final fluidrow
                      )#final fluidrow 
              ),#final tabitem
               #SVENSSON
               tabItem(tabName = "subitem2",
                       fluidRow(h2("  Svensson"),
                                fluidRow(column(width = 6,box( width = 12, background = "navy",
                                                               dateInput(inputId="n1", label="Por favor, seleccionar una fecha", language= "es",
                                                                         width = "100%")#final dateimput 
                                )#final box
                                ),#final column
                                column(width = 6,box(width = 12,height = 2,title = "Fecha de valoración: ",verbatimTextOutput('p1')
                                )#final box
                                )#final column
                                ),#final fluidrow
                                h2("  Títulos"), h5("  Favor seleccionar los títulos a considerar: "),
                                fluidRow(tabBox(width = 12, title = "Títulos", id = "tab1", height = "50px", 
                                                tabPanel("TIF",fluidRow(column(width = 3,checkboxGroupInput( inputId = "t1", label = " ",
                                                                                                             choices=tit[1:7])#final checkboximput
                                                ),#final column
                                                column(width = 3,checkboxGroupInput( inputId = "t2", label = " ",
                                                                                     choices=tit[8:13])#final checkboximput
                                                ),#final column
                                                column(width = 3,checkboxGroupInput( inputId = "t3",label = " ", 
                                                                                     choices=tit[14:19])#final checkboximput
                                                ),#final column
                                                column(width = 3,checkboxGroupInput( inputId = "t4", label = " ",
                                                                                     choices=tit[20:25])#final checkboximput
                                                )#final column
                                                ),#final fluidrow 
                                                verbatimTextOutput("q1"),h2(" Precios Promedios"),verbatimTextOutput("pre1"),
                                                
                                                h2(" Características"),box(style="overflow-x:scroll",width = 12,dataTableOutput("Ca")),
                                                h2(" Parámetros"),
                                                fluidRow(
                                                  tabBox( width = 12, title = "Parámetros", id = "tab1_sven_tif", height = "50px", 
                                                          tabPanel(" Parámetros Iniciales ",verbatimTextOutput("pa_tif"),#withMathJax(uiOutput("formula")),
                                                                   h2(" Precios estimados iniciales"),box(style="overflow-x:scroll",width = 12,dataTableOutput("p_est_tif")),
                                                                   h2(" Curva de rendimientos inicial"),
                                                                   plotOutput("c_tif_sven")
                                                          ),#final tabpanel p iniciales
                                                          tabPanel(" Elegir Parámetros ",
                                                                   # verbatimTextOutput("pa_tif"),#withMathJax(uiOutput("formula")),
                                                                   # h2(" Precios estimados iniciales"),dataTableOutput("p_est_tif"),
                                                                   # h2(" Curva de rendimientos inicial"),
                                                                   # plotOutput("c_tif_sven")
                                                                   h4(" Por favor, insertar parámetros"),
                                                                   box(width=12,title="Importante",status="primary",solidHeader=TRUE ,collapsible = TRUE,
                                                                       collapse= TRUE,"Al ingresar los parámetros considere las siguientes restricciones, ",br(),withMathJax(helpText("$$1) \\quad \\beta_{0} > 0$$")),
                                                                       withMathJax(helpText("$$2) \\quad \\beta_{0}+\\beta_{1} > 0$$")),withMathJax(helpText("$$3) \\quad \\tau_{1} > 0$$")),
                                                                       withMathJax(helpText("$$3) \\quad \\tau_{2} > 0$$"))),#final box
                                                                   column(width = 2,numericInput( inputId = "sven_b0_tif", label="B0: ", min = 0, max = 50,step = 0.1, value = 5, width = "40%")
                                                                          , verbatimTextOutput("num_sven_b0_tif")),#final column,
                                                                   column(width = 2,numericInput( inputId = "sven_b1_tif", label="B1: ", min = -10, max = 50,step = 0.1, value = 5, width = "40%"),
                                                                          verbatimTextOutput("num_sven_b1_tif")),#final column
                                                                   column(width = 2,numericInput( inputId = "sven_b2_tif", label="B2: ", min = -10, max = 50,step = 0.1, value = 5, width = "40%"),
                                                                          verbatimTextOutput("num_sven_b2_tif")),#final column
                                                                   column(width = 2,numericInput( inputId = "sven_b3_tif", label="B3: ", min = -10, max = 50,step = 0.1, value = 5, width = "40%"),
                                                                          verbatimTextOutput("num_sven_b3_tif")),#final column
                                                                   column(width = 2,numericInput( inputId = "sven_t1_tif", label="T1: ", min = 0, max = 50,step = 0.1, value = 5, width = "40%"),
                                                                          verbatimTextOutput("num_sven_t1_tif")),#final column
                                                                   column(width = 2,numericInput( inputId = "sven_t2_tif", label="T2: ", min = 0, max = 50,step = 0.1, value = 5, width = "40%"),
                                                                          verbatimTextOutput("num_sven_t2_tif")),#final column
                                                                   h4(" Los nuevos parámetros considerados son, "),
                                                                   verbatimTextOutput("new_sven_tif"),
                                                                   h4(" Verificación, "),
                                                                   verbatimTextOutput("ver_sven_tif"),
                                                                   h2(" Precios estimados"),box(style="overflow-x:scroll",width = 12,dataTableOutput("p_est_tif_opt_sven_el")),
                                                                   h4(" Curva de Rendimiento"),
                                                                   plotOutput("c_tif_sven_new")
                                                                   
                                                          ),#final tabpanel p elegidos
                                                          tabPanel(" Parámetros Optimizados",
                                                                   h2(" Precios estimados optimizados"),
                                                                   radioButtons(inputId = "opt_tif_sven",label = "Desea optimizar los precios obtenidos:", 
                                                                                choices = c("Si"=1, "No"=0),
                                                                                selected=" "
                                                                   )#final radiobuttoms
                                                                   ,box(style="overflow-x:scroll",width = 12,dataTableOutput("p_est_tif_opt")),
                                                                   h2(" Parámetros optimizados"),
                                                                   verbatimTextOutput("par_tif_sven_op"),
                                                                   h2(" Curva de rendimientos TIF"),
                                                                   plotOutput("c_tif_sven_op")
                                                          )#final tabpanel p optimizados
                                                  )#final tabbox
                                                )#final fluidrow
                                                
                                                
                                                ),#final tabpanel tif
                                                tabPanel("VEBONO",fluidRow(column(width = 3,checkboxGroupInput( inputId = "v1", label = " ",
                                                                                                                choices=tit1[1:8])#final checkboximput
                                                ),#final column
                                                column(width = 3,checkboxGroupInput( inputId = "v2", label = " ",
                                                                                     choices=tit1[9:16])#final checkboximput
                                                ),#final column
                                                column(width = 3,checkboxGroupInput( inputId = "v3", label = " ",
                                                                                     choices=tit1[17:24])#final checkboximput
                                                ),#final column
                                                column(width = 3,checkboxGroupInput( inputId = "v4", label = " ",
                                                                                     choices=tit1[25:29])#final checkboximput
                                                )#final column
                                                ),#final fluidrow 
                                                verbatimTextOutput("q2"), h2(" Precios Promedios"),verbatimTextOutput("pre2"),
                                                 
                                                h2(" Características"),box(style="overflow-x:scroll",width = 12,dataTableOutput("Ca1")),
                                                h2(" Parámetros"),
                                                fluidRow(
                                                  tabBox( width = 12, title = "Parámetros", id = "tab1_sven_veb", height = "50px", 
                                                          tabPanel(" Parámetros Iniciales ",verbatimTextOutput("pa_veb"),#withMathJax(uiOutput("formula")),
                                                                   h2(" Precios estimados iniciales"),box(style="overflow-x:scroll",width = 12,dataTableOutput("p_est_veb")),
                                                                   h2(" Curva de rendimientos inicial"),
                                                                   plotOutput("c_veb_sven")
                                                          ),#final tabpanel p iniciales
                                                          tabPanel(" Elegir Parámetros ",
                                                                   # verbatimTextOutput("pa_tif"),#withMathJax(uiOutput("formula")),
                                                                   # h2(" Precios estimados iniciales"),dataTableOutput("p_est_tif"),
                                                                   # h2(" Curva de rendimientos inicial"),
                                                                   # plotOutput("c_tif_sven")
                                                                   h4(" Por favor, insertar parámetros"),
                                                                   box(width=12,title="Importante",status="primary",solidHeader=TRUE ,collapsible = TRUE,
                                                                       collapse= TRUE,"Al ingresar los parámetros considere las siguientes restricciones, ",br(),withMathJax(helpText("$$1) \\quad \\beta_{0} > 0$$")),
                                                                       withMathJax(helpText("$$2) \\quad \\beta_{0}+\\beta_{1} > 0$$")),withMathJax(helpText("$$3) \\quad \\tau_{1} > 0$$")),
                                                                       withMathJax(helpText("$$3) \\quad \\tau_{2} > 0$$"))),#final box
                                                                   column(width = 2,numericInput( inputId = "sven_b0_veb", label="B0: ", min = 0, max = 50,step = 0.1, value = 5, width = "40%")
                                                                          , verbatimTextOutput("num_sven_b0_veb")),#final column,
                                                                   column(width = 2,numericInput( inputId = "sven_b1_veb", label="B1: ", min = -10, max = 50,step = 0.1, value = 5, width = "40%"),
                                                                          verbatimTextOutput("num_sven_b1_veb")),#final column
                                                                   column(width = 2,numericInput( inputId = "sven_b2_veb", label="B2: ", min = -10, max = 50,step = 0.1, value = 5, width = "40%"),
                                                                          verbatimTextOutput("num_sven_b2_veb")),#final column
                                                                   column(width = 2,numericInput( inputId = "sven_b3_veb", label="B3: ", min = -10, max = 50,step = 0.1, value = 5, width = "40%"),
                                                                          verbatimTextOutput("num_sven_b3_veb")),#final column
                                                                   column(width = 2,numericInput( inputId = "sven_t1_veb", label="T1: ", min = 0, max = 50,step = 0.1, value = 5, width = "40%"),
                                                                          verbatimTextOutput("num_sven_t1_veb")),#final column
                                                                   column(width = 2,numericInput( inputId = "sven_t2_veb", label="T2: ", min = 0, max = 50,step = 0.1, value = 5, width = "40%"),
                                                                          verbatimTextOutput("num_sven_t2_veb")),#final column
                                                                   h4(" Los nuevos parámetros considerados son, "),
                                                                   verbatimTextOutput("new_sven_veb"),
                                                                   h4(" Verificación, "),
                                                                   verbatimTextOutput("ver_sven_veb"),
                                                                   h2(" Precios estimados"),box(style="overflow-x:scroll",width = 12,dataTableOutput("p_est_veb_opt_sven_el")),
                                                                   h4(" Curva de Rendimiento"),
                                                                   plotOutput("c_veb_sven_new")
                                                                   
                                                          ),#final tabpanel p elegidos
                                                          tabPanel(" Parámetros Optimizados",
                                                                   h2(" Precios estimados optimizados"),
                                                                   radioButtons(inputId = "opt_veb_sven",label = "Desea optimizar los precios obtenidos:", 
                                                                                choices = c("Si"=1, "No"=0),
                                                                                selected=" "),box(style="overflow-x:scroll",width = 12,dataTableOutput("p_est_veb_opt"))
                                                                   ,
                                                                   h2(" Parámetros optimizados"),
                                                                   verbatimTextOutput("par_veb_sven_op"),
                                                                   h2(" Curva de rendimientos VEBONOS"),
                                                                   plotOutput("c_veb_sven_op")
                                                          )#final tabpanel p optimizados
                                                         )#final tabbox
                                                      )#final fluidrow
                                               
                                  )#final tabitem vebono
                                )#final tabbox
                                )#finalfluidrow
                       )#finalfluidrow
               ),#final tabitem
               #DIEBOLD-LI
               tabItem(tabName = "subitem3",
                    h2("Diebold-Li"),
                          fluidRow(column( width = 6,box( width = 12, background = "navy",
                                                        dateInput(inputId="n3", label="Por favor, seleccionar una fecha", language= "es",
                                                               width = "100%")#final dateimput 
                                                        )#final box
                                          ),#final column
                                   box( width = 6,height = 2,title = "Fecha de valoración: ",verbatimTextOutput('p3')) #finalbox
                                   ),#final fluidrow
                                    h2("  Títulos"), h5("  Favor seleccionar los títulos a considerar: "),
                                    fluidRow(tabBox(width = 12, title = "Títulos", id = "tab3", height = "50px", 
                                                    tabPanel("TIF",fluidRow(column(width = 3,checkboxGroupInput( inputId = "t1_dl", label = " ",
                                                                                                                 choices=tit[1:7])#final checkboximput
                                                    ),#final column
                                                    column(width = 3,checkboxGroupInput( inputId = "t2_dl", label = " ",
                                                                                         choices=tit[8:13])#final checkboximput
                                                    ),#final column
                                                    column(width = 3,checkboxGroupInput( inputId = "t3_dl",label = " ", 
                                                                                         choices=tit[14:19])#final checkboximput
                                                    ),#final column
                                                    column(width = 3,checkboxGroupInput( inputId = "t4_dl", label = " ",
                                                                                         choices=tit[20:25])#final checkboximput
                                                    )#final column
                                                    ),#final fluidrow 
                                                    verbatimTextOutput("q1_dl"),h2(" Precios Promedios"),verbatimTextOutput("pre1_dl"),
                                                    
                                                    h2(" Características"),box(style="overflow-x:scroll",width = 12,dataTableOutput("Ca_dl")),
                                                    h2(" Parámetro de suavizamiento"),
                                                    numericInput( inputId = "parametro_tif_dl", label="Parámetro: ", min = -10, max = 100,step = 0.1, value = 1, width = "40%"),
                                                    verbatimTextOutput("spar_tif_dl"),
                                                    h2(" Spline a usar"),
                                                    verbatimTextOutput("spline_tif"),
                                                    h2(" Curva spline Tif"),
                                                    rbokehOutput("c_tif_splines_dl"),
                                                    h2(" Precios estimados"),
                                                    box(style="overflow-x:scroll",width = 12,dataTableOutput("p_est_dl_tif")),
                                                    h2(" Curva de Rendimientos"),
                                                    plotlyOutput("curva_tif_dl")
                                                    
                                                    
                                                    ),#final tabpanel tif
                                                    
                                                    tabPanel("VEBONO",fluidRow(
                                                      column(width = 3,checkboxGroupInput(inputId = "v1_dl", label = " ",
                                                                                          choices=tit1[1:8])#final checkboxgroupimput
                                                      ),#final column
                                                      column(width = 3,checkboxGroupInput( inputId = "v2_dl", label = " ",
                                                                                           choices=tit1[9:16])#final checkboxgroupimput
                                                      ),#final column
                                                      column(width = 3,checkboxGroupInput( inputId = "v3_dl",label = " ",
                                                                                           choices=tit1[17:24])#final checkboxgroupimput
                                                      ),#final column
                                                      column(width = 3,checkboxGroupInput( inputId = "v4_dl", label = " ",
                                                                                           choices=tit1[25:29])#final checkboxgroupimput
                                                      )#final column
                                                    ),#final fluidrow
                                                    verbatimTextOutput("q2_dl"), h2(" Precios Promedios"),verbatimTextOutput("pre2_dl"),
                                                    
                                                    h2(" Características"),box(style="overflow-x:scroll",width = 12,dataTableOutput("Ca1_dl")),
                                                    h2(" Parámetro de suavizamiento"),
                                                    numericInput( inputId = "parametro_veb_dl", label="Parámetro: ", min = -10, max = 100,step = 0.1, value = 1, width = "40%"),
                                                    verbatimTextOutput("spar_veb_dl"),
                                                    h2(" Spline a usar"),
                                                    verbatimTextOutput("spline_veb"),
                                                    h2(" Curva spline Vebonos"),
                                                    rbokehOutput("c_veb_splines_dl"),
                                                    h2(" Precios estimados"),
                                                    box(style="overflow-x:scroll",width = 12,dataTableOutput("p_est_dl_veb")),
                                                    h2(" Curva de Rendimientos"),
                                                    plotlyOutput("curva_veb_dl")
                                                    
                                                    
                                                    )#final tabpanel veb
                                    )#final tabbox
                                    )#final fluidrow
                       ),#final tabitem dl
               #SPLINES
               tabItem(tabName = "subitem4",
                    h2("Splines"),
                          fluidRow(column( width = 6,box( width = 12, background = "navy",
                                                          dateInput(inputId="n4", label="Por favor, seleccionar una fecha", language= "es",
                                                                    width = "100%")#final dateimput 
                                                        )#final box
                                           ),#final column
                                   box( width = 6,height = 2,title = "Fecha de valoración: ",verbatimTextOutput('p4'))),
                    h2("  Títulos"), h5("  Favor seleccionar los títulos a considerar: "),
                    fluidRow(tabBox(width = 12, title = "Títulos", id = "tab3", height = "50px", 
                                    tabPanel("TIF",fluidRow(column(width = 3,checkboxGroupInput( inputId = "t1_sp", label = " ",
                                                                                                 choices=tit[1:7])#final checkboximput
                                    ),#final column
                                    column(width = 3,checkboxGroupInput( inputId = "t2_sp", label = " ",
                                                                         choices=tit[8:13])#final checkboximput
                                    ),#final column
                                    column(width = 3,checkboxGroupInput( inputId = "t3_sp",label = " ", 
                                                                         choices=tit[14:19])#final checkboximput
                                    ),#final column
                                    column(width = 3,checkboxGroupInput( inputId = "t4_sp", label = " ",
                                                                         choices=tit[20:25])#final checkboximput
                                    )#final column
                                    ),#final fluidrow 
                                    verbatimTextOutput("q1_sp"),h2(" Precios Promedios"),verbatimTextOutput("pre1_sp"),
                                    
                                    h2(" Características"),box(style="overflow-x:scroll",width = 12,dataTableOutput("Ca_sp")),
                                    h3(" Por favor seleccionar el cantidad de días "),
                                    box(width=12,title="Importante",status="primary",solidHeader=TRUE ,collapsible = TRUE,
                                        collapse= TRUE,"Recuerde que esta es la cantidad de días a considerar hacia atras en el tiempo
                                        con el fin de generar la data con la que se trabajará, por ejemplo si se elige 30 la data 
                                        a considerar serán las observaciones comprendidas entre la fecha de valoración y 
                                        la fecha resultante luego de restarle 30 días a la fecha de valoración" 
                                        ),#final box
                                    numericInput( inputId = "d_tif", label="Días: ", min = 1, max = 100,step = 1, value = 40, width = "40%"),
                                           verbatimTextOutput("dias_tif"),
                                    h2(" Títulos candidatos"),box(style="overflow-x:scroll",width = 12,dataTableOutput("tit_cand_tif")),
                                    h3(" Por favor ingresar el parámetro de suavizamiento "),
                                    box(width=12,title="Importante",status="primary",solidHeader=TRUE ,collapsible = TRUE,
                                        collapse= TRUE,"Recuerde que este valor corresponde al grado de suavidad que tendrá la curva de rendimientos resultante" 
                                    ),#final box
                                    numericInput( inputId = "parametro_tif", label="Parámetro: ", min = -10, max = 100,step = 0.1, value = 1, width = "40%"),
                                           verbatimTextOutput("spar_tif"),
                                    h2(" Precios Splines"),
                                    box(style="overflow-x:scroll",width = 12,dataTableOutput("pre_sp_tif")),
                                    h2(" Curva de rendimientos TIF")
                                    ,
                                    rbokehOutput("c_tif_splines"),#verbatimTextOutput("datos")
                                    h2(" Eliminar observaciones"),
                                    htmlOutput("selectUI_tif"),
                                    h3(" Títulos elegidos"),
                                    verbatimTextOutput("obs_elim_tif"),
                                    h2(" Nuevos títulos candidatos"),
                                    box(style="overflow-x:scroll",width = 12,dataTableOutput("tit_cand_tif_new")),
                                    h2(" Nuevos precios"),
                                    box(style="overflow-x:scroll",width = 12,dataTableOutput("precios_tif_nuevos")),
                                    h2(" Nueva curva de rendimientos"),
                                    rbokehOutput("c_tif_splines_new")
                                    #plotOutput("c_tif_splines_new"),
                                    
                                    #radioButtons(inputId = "elim_tif",label = "Desea eliminar alguna observación:", 
                                     #            choices = c("Si"=1, "No"=0),
                                      #           selected="0"),
                                    #box(style="overflow-x:scroll",width = 12,dataTableOutput("tabla_elim_tif"))
                                    
                                    
                                    
                                    
                                    ),#final tabpanel tif
                                    
                                    tabPanel("VEBONO",fluidRow(
                                      column(width = 3,checkboxGroupInput(inputId = "v1_sp", label = " ",
                                                                          choices=tit1[1:8])#final checkboxgroupimput
                                      ),#final column
                                      column(width = 3,checkboxGroupInput( inputId = "v2_sp", label = " ",
                                                                           choices=tit1[9:16])#final checkboxgroupimput
                                      ),#final column
                                      column(width = 3,checkboxGroupInput( inputId = "v3_sp",label = " ",
                                                                           choices=tit1[17:24])#final checkboxgroupimput
                                      ),#final column
                                      column(width = 3,checkboxGroupInput( inputId = "v4_sp", label = " ",
                                                                           choices=tit1[25:29])#final checkboxgroupimput
                                      )#final column
                                    ),#final fluidrow
                                    verbatimTextOutput("q2_sp"), h2(" Precios Promedios"),verbatimTextOutput("pre2_sp"),
                                    
                                    h2(" Características"),box(style="overflow-x:scroll",width = 12,dataTableOutput("Ca1_sp")),
                                    h3(" Por favor seleccionar el cantidad de días "),
                                    box(width=12,title="Importante",status="primary",solidHeader=TRUE ,collapsible = TRUE,
                                        collapse= TRUE,"Recuerde que esta es la cantidad de días a considerar hacia atras en el tiempo
                                        con el fin de generar la data con la que se trabajará, por ejemplo si se elige 30 la data 
                                        a considerar serán las observaciones comprendidas entre la fecha de valoración y 
                                        la fecha resultante luego de restarle 30 días a la fecha de valoración" 
                                    ),#final box
                                    numericInput( inputId = "d_veb", label="Días: ", min = 1, max = 100,step = 1, value = 40, width = "40%"),
                                    verbatimTextOutput("dias_veb"),
                                    h2(" Títulos candidatos"),
                                    box(style="overflow-x:scroll",width = 12,dataTableOutput("tit_cand_veb")),
                                    h3(" Por favor ingresar el parámetro de suavizamiento "),
                                    box(width=12,title="Importante",status="primary",solidHeader=TRUE ,collapsible = TRUE,
                                        collapse= TRUE,"Recuerde que este valor corresponde al grado de suavidad que tendrá la curva de rendimientos resultante" 
                                    ),#final box
                                    numericInput( inputId = "parametro_veb", label="Parámetro: ", min = -10, max = 100,step = 0.1, value = 1, width = "40%"),
                                    verbatimTextOutput("spar_veb"),
                                    h2(" Precios Splines"),
                                    box(style="overflow-x:scroll",width = 12,dataTableOutput("pre_sp_veb")),
                                    h2(" Curva de rendimientos VEBONO"),
                                    rbokehOutput("c_veb_splines"),#verbatimTextOutput("datos")
                                    h2(" Eliminar observaciones"),
                                    htmlOutput("selectUI_veb"),
                                    h3(" Títulos elegidos"),
                                    verbatimTextOutput("obs_elim_veb"),
                                    h2(" Nuevos títulos candidatos"),
                                    box(style="overflow-x:scroll",width = 12,dataTableOutput("tit_cand_veb_new")),
                                    h2(" Nuevos precios"),
                                    box(style="overflow-x:scroll",width = 12,dataTableOutput("precios_veb_nuevos")),
                                    h2(" Nueva curva de rendimientos"),
                                    rbokehOutput("c_veb_splines_new")
                                    )#final tabpanel veb
                    )#final tabbox
                    )#final fluidrow
                    
                    ),#final tabitem
           
              
              #COMPARATIVO
             
              tabItem(tabName = "datos",
                      h2(" Descarga de archivos"),
                      # Input: Choose dataset ----
                      selectInput("dataset", "Elegir un Archivo:",
                                  choices = c("0-22", "Caracteristicas"
                                             )),
                      
                      # Button
                      downloadButton("downloadData", "Descargar"),
                      h5(" Usted seleccionó"),
                      verbatimTextOutput("desc"),
                      h5(" Vista previa"),
                      tabsetPanel(type="pills",
                          tabPanel("Características",
                              h5("Documento Características"),
                              box(style="overflow-x:scroll",width = 12,
                              dataTableOutput("Ca_leida"))),
                          tabPanel("Operaciones BCV 022",
                              h5("Documento 0-22"),
                              box(style="overflow-x:scroll",width = 12,
                              dataTableOutput("docbcv"))
                                )
                                ),
                      h2("Calculo precio promedio"),
                      #tabBox(width = 12, title = "Títulos", height = "50px",
                      #mainPanel(
                        tabsetPanel(type="pills",
                      tabPanel("TIF",
                      verbatimTextOutput("pre_prom_tif")),
                      tabPanel("VEBONOS",
                               verbatimTextOutput("pre_prom_veb"))
                        )
                      #)
                      #)
                      
                      
                      ),
              
              
                tabItem(tabName = "metodologias",
                      fluidRow(
                       h2(" Comparativo"),
                      fluidRow(column(width = 6,box( width = 12, background = "navy",
                                                     dateInput(inputId="n5", label="Por favor, seleccionar una fecha", language= "es",
                                                               width = "100%")#final dateimput 
                      )#final box
                      ),#final column
                      box( width = 6,height = 2,title = "Fecha de valoración: ",verbatimTextOutput('p5')) #final box
                      ),#final fluidrow
                      h2("  Títulos"), h5("  Favor seleccionar los títulos a considerar: "),
                      fluidRow(
                        tabBox(width = 12, title = "Títulos", id = "tab5", height = "50px", 
                                      tabPanel("TIF",fluidRow(column(width = 3,checkboxGroupInput( inputId = "t1_comp", label = " ",
                                                                                                   choices=tit[1:7])#final checkboximput
                                      ),#final column
                                      column(width = 3,checkboxGroupInput( inputId = "t2_comp", label = " ",
                                                                           choices=tit[8:13])#final checkboximput
                                      ),#final column
                                      column(width = 3,checkboxGroupInput( inputId = "t3_comp",label = " ", 
                                                                           choices=tit[14:19])#final checkboximput
                                      ),#final column
                                      column(width = 3,checkboxGroupInput( inputId = "t4_comp", label = " ",
                                                                           choices=tit[20:25])#final checkboximput
                                      )#final column
                                      ),#final fluidrow 
                                      verbatimTextOutput("q1_comp"),
                                      h2(" Características"),box(style="overflow-x:scroll",width = 12,dataTableOutput("Ca_comp")),
                                      h2(" Metodologías"),
                                      fluidRow(tabBox(width = 12, title = " ", id = "tab5", height = "50px", 
                                             tabPanel("Nelson y siegel",
                                                      h2(" Precios Promedios"),verbatimTextOutput("pre_comp_tif_ns"),
                                                      fluidRow(
                                                        tabBox( width = 12, title = "Parámetros", id = "tab1_ns_tif_comp", height = "50px", 
                                                                
                                                                tabPanel(" Elegir parámetros ",
                                                                         h4(" Por favor, insertar parámetros"),
                                                                         box(width=12,title="Importante",status="primary",solidHeader=TRUE ,collapsible = TRUE,
                                                                             collapse= TRUE,"Al ingresar los parámetros considere las siguientes restricciones, ",br(),withMathJax(helpText("$$1) \\quad \\beta_{0} > 0$$")),
                                                                             withMathJax(helpText("$$2) \\quad \\beta_{0}+\\beta_{1} > 0$$")),withMathJax(helpText("$$3) \\quad \\tau > 0$$"))),#final box
                                                                         column(width = 3,numericInput( inputId = "ns_b0_tif_comp", label="B0: ", min = 0, max = 50,step = 0.1, value = 5, width = "40%")
                                                                                , verbatimTextOutput("num_ns_b0_tif_comp")),#final column,
                                                                         column(width = 3,numericInput( inputId = "ns_b1_tif_comp", label="B1: ", min = -10, max = 50,step = 0.1, value = 5, width = "40%"),
                                                                                verbatimTextOutput("num_ns_b1_tif_comp")),#final column
                                                                         column(width = 3,numericInput( inputId = "ns_b2_tif_comp", label="B2: ", min = -10, max = 50,step = 0.1, value = 5, width = "40%"),
                                                                                verbatimTextOutput("num_ns_b2_tif_comp")),#final column
                                                                         column(width = 3,numericInput( inputId = "ns_t_tif_comp", label="T: ", min = 0, max = 50,step = 0.1, value = 5, width = "40%"),
                                                                                verbatimTextOutput("num_ns_t_tif_comp")),#final column
                                                                         h4(" Los nuevos parámetros considerados son, "),
                                                                         verbatimTextOutput("new_ns_tif_comp"),
                                                                         h4(" Verificación, "),
                                                                         verbatimTextOutput("ver_ns_tif_comp"),
                                                                         h2(" Precios estimados"),box(style="overflow-x:scroll",width = 12,dataTableOutput("p_est_tif_ns_el_comp")),
                                                                         h4(" Curva de Rendimiento"),
                                                                         plotOutput("c_tif_ns1_new_comp")
                                                                       
                                                                         
                                                                ),# final tabpanel pa elegir 
                                                                tabPanel(" Parámetros optimizados",
                                                                         h2(" Precios estimados optimizados"),
                                                                         radioButtons( inputId = "opt_tif_ns_comp",label = "Desea optimizar los precios obtenidos:", 
                                                                                       choices = c("Si"=1, "No"=0),
                                                                                       selected=" "), #finalradiobuttons
                                                                         box(style="overflow-x:scroll",width = 12,dataTableOutput("p_est_tif_opt_ns_comp")),
                                                                         h2(" Parámetros optimizadoss"),
                                                                         verbatimTextOutput("par_tif_ns_op_comp"),
                                                                         h2(" Curva de rendimientos TIF"),
                                                                         plotOutput("c_tif_ns_op_comp")
                                                                         
                                                                )#final tabpabel pa optimizados
                                                                
                                                                )#final tabbox
                                                      )#final fluid row
                                                        
                                                      
                                             ),#final tabpanel Nelson y siegel
                                             
                                             tabPanel("Svensson",
                                                      h2(" Precios Promedios"),verbatimTextOutput("pre_comp_tif_sven"),
                                                      fluidRow(
                                                        tabBox( width = 12, title = "Parámetros", id = "tab1_sven_tif_comp", height = "50px", 
                                                                
                                                                tabPanel(" Elegir parámetros ",
                                                                         h4(" Por favor, insertar parámetros"),
                                                                         box(width=12,title="Importante",status="primary",solidHeader=TRUE ,collapsible = TRUE,
                                                                             collapse= TRUE,"Al ingresar los parámetros considere las siguientes restricciones, ",br(),withMathJax(helpText("$$1) \\quad \\beta_{0} > 0$$")),
                                                                             withMathJax(helpText("$$2) \\quad \\beta_{0}+\\beta_{1} > 0$$")),withMathJax(helpText("$$3) \\quad \\tau_{1} > 0$$")),
                                                                             withMathJax(helpText("$$3) \\quad \\tau_{2} > 0$$"))),#final box
                                                                         column(width = 2,numericInput( inputId = "sven_b0_tif_comp", label="B0: ", min = 0, max = 50,step = 0.1, value = 5, width = "40%")
                                                                                , verbatimTextOutput("num_sven_b0_tif_comp")),#final column,
                                                                         column(width = 2,numericInput( inputId = "sven_b1_tif_comp", label="B1: ", min = -10, max = 50,step = 0.1, value = 5, width = "40%"),
                                                                                verbatimTextOutput("num_sven_b1_tif_comp")),#final column
                                                                         column(width = 2,numericInput( inputId = "sven_b2_tif_comp", label="B2: ", min = -10, max = 50,step = 0.1, value = 5, width = "40%"),
                                                                                verbatimTextOutput("num_sven_b2_tif_comp")),#final column
                                                                         column(width = 2,numericInput( inputId = "sven_b3_tif_comp", label="B3: ", min = -10, max = 50,step = 0.1, value = 5, width = "40%"),
                                                                                verbatimTextOutput("num_sven_b3_tif_comp")),#final column
                                                                         column(width = 2,numericInput( inputId = "sven_t1_tif_comp", label="T1: ", min = 0, max = 50,step = 0.1, value = 5, width = "40%"),
                                                                                verbatimTextOutput("num_sven_t1_tif_comp")),#final column
                                                                         column(width = 2,numericInput( inputId = "sven_t2_tif_comp", label="T2: ", min = 0, max = 50,step = 0.1, value = 5, width = "40%"),
                                                                                verbatimTextOutput("num_sven_t2_tif_comp")),#final column
                                                                         h4(" Los nuevos parámetros considerados son, "),
                                                                         verbatimTextOutput("new_sven_tif_comp"),
                                                                         h4(" Verificación, "),
                                                                         verbatimTextOutput("ver_sven_tif_comp"),
                                                                         h2(" Precios estimados"),box(style="overflow-x:scroll",width = 12,dataTableOutput("p_est_tif_opt_sven_el_comp")),
                                                                         h4(" Curva de Rendimiento"),
                                                                         plotOutput("c_tif_sven_new_comp")
                                                                         
                                                                         
                                                                ),# final tabpanel pa elegir 
                                                                tabPanel(" Parámetros optimizados",
                                                                         h2(" Precios estimados optimizados"),
                                                                         radioButtons(inputId = "opt_tif_sven_comp",label = "Desea optimizar los precios obtenidos:", 
                                                                                      choices = c("Si"=1, "No"=0),
                                                                                      selected=" "
                                                                         )#final radiobuttoms
                                                                         ,box(style="overflow-x:scroll",width = 12,dataTableOutput("p_est_tif_opt_comp")),
                                                                         h2(" Parámetros optimizados"),
                                                                         verbatimTextOutput("par_tif_sven_op_comp"),
                                                                         h2(" Curva de rendimientos TIF"),
                                                                         plotOutput("c_tif_sven_op_comp")
                                                                         
                                                                )#final tabpabel pa optimizados
                                                                
                                                        )#final tabbox
                                                      )#final fluid row
                                                      
                                             ),#final tabpanel Svensson 
                                             tabPanel("Diebold Li",
                                                      h2(" Parámetro de suavizamiento"),
                                                      numericInput( inputId = "parametro_tif_dl_comp", label="Parámetro: ", min = -10, max = 100,step = 0.1, value = 1, width = "40%"),
                                                      verbatimTextOutput("spar_tif_dl_comp"),
                                                      h2(" Spline a usar"),
                                                      verbatimTextOutput("spline_tif_comp_out"),
                                                      h2(" Curva spline Tif"),
                                                      rbokehOutput("c_tif_splines_dl_comp"),
                                                      h2(" Precios estimados"),
                                                      box(style="overflow-x:scroll",width = 12,dataTableOutput("p_est_dl_tif_comp")),
                                                      h2(" Curva de Rendimientos"),
                                                      plotlyOutput("curva_tif_dl_comp")
                                                      
                                             ),#final tabpanel Diebold li 
                                             tabPanel("Splines",
                                                      h3(" Por favor seleccionar el cantidad de días "),
                                                      box(width=12,title="Importante",status="primary",solidHeader=TRUE ,collapsible = TRUE,
                                                          collapse= TRUE,"Recuerde que esta es la cantidad de días a considerar hacia atras en el tiempo
                                                          con el fin de generar la data con la que se trabajará, por ejemplo si se elige 30 la data 
                                                          a considerar serán las observaciones comprendidas entre la fecha de valoración y 
                                                          la fecha resultante luego de restarle 30 días a la fecha de valoración" 
                                                      ),#final box
                                                      numericInput( inputId = "d_tif_comp", label="Días: ", min = 1, max = 100,step = 1, value = 40, width = "40%"),
                                                      verbatimTextOutput("dias_tif_comp"),
                                                      h2(" Títulos candidatos"),box(style="overflow-x:scroll",width = 12,dataTableOutput("tit_cand_tif_comp")),
                                                      h3(" Por favor ingresar el parámetro de suavizamiento "),
                                                      box(width=12,title="Importante",status="primary",solidHeader=TRUE ,collapsible = TRUE,
                                                          collapse= TRUE,"Recuerde que este valor corresponde al grado de suavidad que tendrá la curva de rendimientos resultante" 
                                                      ),#final box
                                                      numericInput( inputId = "parametro_tif_comp", label="Parámetro: ", min = -10, max = 100,step = 0.1, value = 1, width = "40%"),
                                                      verbatimTextOutput("spar_tif_comp"),
                                                      h2(" Precios Splines"),box(style="overflow-x:scroll",width = 12,dataTableOutput("pre_sp_tif_comp")),
                                                      h2(" Curva de rendimientos TIF"),
                                                      rbokehOutput("c_tif_splines_comp"),#verbatimTextOutput("datos")
                                                      h2(" Eliminar observaciones"),
                                                      htmlOutput("selectUI_tif_comp"),
                                                      h3(" Títulos elegidos"),
                                                      verbatimTextOutput("obs_elim_tif_comp"),
                                                      h2(" Nuevos títulos candidatos"),
                                                      box(style="overflow-x:scroll",width = 12,dataTableOutput("tit_cand_tif_new_comp")),
                                                      h2(" Nuevos precios"),
                                                      box(style="overflow-x:scroll",width = 12,dataTableOutput("precios_tif_nuevos_comp")),
                                                      h2(" Nueva curva de rendimientos"),
                                                      rbokehOutput("c_tif_splines_new_comp")
                                                      
                                                      
                                                      
                                                      )#final tabpanel Splines 
                                      )#final tabbox
                                      )#final fluidrow
                                      ),#final tabpanel tif
                      
                      tabPanel("VEBONO",fluidRow(
                        column(width = 3,checkboxGroupInput(inputId = "v1_comp", label = " ",
                                                            choices=tit1[1:8])#final checkboxgroupimput
                        ),#final column
                        column(width = 3,checkboxGroupInput( inputId = "v2_comp", label = " ",
                                                             choices=tit1[9:16])#final checkboxgroupimput
                        ),#final column
                        column(width = 3,checkboxGroupInput( inputId = "v3_comp",label = " ",
                                                             choices=tit1[17:24])#final checkboxgroupimput
                        ),#final column
                        column(width = 3,checkboxGroupInput( inputId = "v4_comp", label = " ",
                                                             choices=tit1[25:29])#final checkboxgroupimput
                        )#final column
                      ),#final fluidrow
                      verbatimTextOutput("q2_comp"),
                      h2(" Características"),box(style="overflow-x:scroll",width = 12,dataTableOutput("Ca1_comp")),
                      h2(" Metodologías"),
                      fluidRow(tabBox(width = 12, title = " ", id = "tab5", height = "50px", 
                             tabPanel("Nelson y siegel",
                                      h2(" Precios Promedios"),verbatimTextOutput("pre_comp_veb_ns"),
                                      fluidRow(
                                        tabBox( width = 12, title = "Parámetros", id = "tab1_ns_veb_comp", height = "50px", 
                                                
                                                tabPanel(" Elegir parámetros ",
                                                         h4(" Por favor, insertar parámetros"),
                                                         box(width=12,title="Importante",status="primary",solidHeader=TRUE ,collapsible = TRUE,
                                                             collapse= TRUE,"Al ingresar los parámetros considere las siguientes restricciones, ",br(),withMathJax(helpText("$$1) \\quad \\beta_{0} > 0$$")),
                                                             withMathJax(helpText("$$2) \\quad \\beta_{0}+\\beta_{1} > 0$$")),withMathJax(helpText("$$3) \\quad \\tau > 0$$"))),#final box
                                                         column(width = 3,numericInput( inputId = "ns_b0_veb_comp", label="B0: ", min = 0, max = 50,step = 0.1, value = 5, width = "40%")
                                                                , verbatimTextOutput("num_ns_b0_veb_comp")),#final column,
                                                         column(width = 3,numericInput( inputId = "ns_b1_veb_comp", label="B1: ", min = -10, max = 50,step = 0.1, value = 5, width = "40%"),
                                                                verbatimTextOutput("num_ns_b1_veb_comp")),#final column
                                                         column(width = 3,numericInput( inputId = "ns_b2_veb_comp", label="B2: ", min = -10, max = 50,step = 0.1, value = 5, width = "40%"),
                                                                verbatimTextOutput("num_ns_b2_veb_comp")),#final column
                                                         column(width = 3,numericInput( inputId = "ns_t_veb_comp", label="T: ", min = 0, max = 50,step = 0.1, value = 5, width = "40%"),
                                                                verbatimTextOutput("num_ns_t_veb_comp")),#final column
                                                         h4(" Los nuevos parámetros considerados son, "),
                                                         verbatimTextOutput("new_ns_veb_comp"),
                                                         h4(" Verificación, "),
                                                         verbatimTextOutput("ver_ns_veb_comp"),
                                                         h2(" Precios estimados"),box(style="overflow-x:scroll",width = 12,dataTableOutput("p_est_veb_ns_el_comp")),
                                                         h4(" Curva de Rendimiento"),
                                                         plotOutput("c_veb_ns1_new_comp")
                                                         
                                                         
                                                         
                                                ),# final tabpanel pa elegir 
                                                tabPanel(" Parámetros optimizados",
                                                         h2(" Precios estimados optimizados"),
                                                         radioButtons( inputId = "opt_veb_ns_comp",label = "Desea optimizar los precios obtenidos:", 
                                                                       choices = c("Si"=1, "No"=0),
                                                                       selected=" "), #finalradiobuttons
                                                         box(style="overflow-x:scroll",width = 12,dataTableOutput("p_est_veb_opt_ns_comp")),
                                                         h2(" Parámetros optimizados"),
                                                         verbatimTextOutput("par_veb_ns_op_comp"),
                                                         h2(" Curva de rendimientos VEBONO"),
                                                         plotOutput("c_veb_ns_op_comp")
                                                         
                                                )#final tabpabel pa optimizados
                                                
                                        )#final tabbox
                                      )#final fluid row
                                      
                                      
                             ),#final tabpanel veb
                             tabPanel("Svensson",
                                      h2(" Precios Promedios"),verbatimTextOutput("pre_comp_veb_sven"),
                                      fluidRow(
                                        tabBox( width = 12, title = "Parámetros", id = "tab1_sven_veb_comp", height = "50px", 
                                                
                                                tabPanel(" Elegir parámetros ",
                                                         h4(" Por favor, insertar parámetros"),
                                                         box(width=12,title="Importante",status="primary",solidHeader=TRUE ,collapsible = TRUE,
                                                             collapse= TRUE,"Al ingresar los parámetros considere las siguientes restricciones, ",br(),withMathJax(helpText("$$1) \\quad \\beta_{0} > 0$$")),
                                                             withMathJax(helpText("$$2) \\quad \\beta_{0}+\\beta_{1} > 0$$")),withMathJax(helpText("$$3) \\quad \\tau_{1} > 0$$")),
                                                             withMathJax(helpText("$$3) \\quad \\tau_{2} > 0$$"))),#final box
                                                         column(width = 2,numericInput( inputId = "sven_b0_veb_comp", label="B0: ", min = 0, max = 50,step = 0.1, value = 5, width = "40%")
                                                                , verbatimTextOutput("num_sven_b0_veb_comp")),#final column,
                                                         column(width = 2,numericInput( inputId = "sven_b1_veb_comp", label="B1: ", min = -10, max = 50,step = 0.1, value = 5, width = "40%"),
                                                                verbatimTextOutput("num_sven_b1_veb_comp")),#final column
                                                         column(width = 2,numericInput( inputId = "sven_b2_veb_comp", label="B2: ", min = -10, max = 50,step = 0.1, value = 5, width = "40%"),
                                                                verbatimTextOutput("num_sven_b2_veb_comp")),#final column
                                                         column(width = 2,numericInput( inputId = "sven_b3_veb_comp", label="B3: ", min = -10, max = 50,step = 0.1, value = 5, width = "40%"),
                                                                verbatimTextOutput("num_sven_b3_veb_comp")),#final column
                                                         column(width = 2,numericInput( inputId = "sven_t1_veb_comp", label="T1: ", min = 0, max = 50,step = 0.1, value = 5, width = "40%"),
                                                                verbatimTextOutput("num_sven_t1_veb_comp")),#final column
                                                         column(width = 2,numericInput( inputId = "sven_t2_veb_comp", label="T2: ", min = 0, max = 50,step = 0.1, value = 5, width = "40%"),
                                                                verbatimTextOutput("num_sven_t2_veb_comp")),#final column
                                                         h4(" Los nuevos parámetros considerados son, "),
                                                         verbatimTextOutput("new_sven_veb_comp"),
                                                         h4(" Verificación, "),
                                                         verbatimTextOutput("ver_sven_veb_comp"),
                                                         h2(" Precios estimados"),box(style="overflow-x:scroll",width = 12,dataTableOutput("p_est_veb_opt_sven_el_comp")),
                                                         h4(" Curva de Rendimiento"),
                                                         plotOutput("c_veb_sven_new_comp")
                                                         
                                                         
                                                         
                                                ),# final tabpanel pa iniciales 
                                                tabPanel(" Parámetros optimizados",
                                                         h2(" Precios estimados optimizados"),
                                                         radioButtons(inputId = "opt_veb_sven_comp",label = "Desea optimizar los precios obtenidos:", 
                                                                      choices = c("Si"=1, "No"=0),
                                                                      selected=" "
                                                         )#final radiobuttoms
                                                         ,box(style="overflow-x:scroll",width = 12,dataTableOutput("p_est_veb_opt_comp")),
                                                         h2(" Parámetros optimizados"),
                                                         verbatimTextOutput("par_veb_sven_op_comp"),
                                                         h2(" Curva de rendimientos VEBONOS"),
                                                         plotOutput("c_veb_sven_op_comp")
                                                         
                                                )#final tabpabel pa optimizados
                                                
                                        )#final tabbox
                                      )#final fluid row
                             ),#final tabpanel Svensson 
                             tabPanel("Diebold Li",
                                      h2(" Parámetro de suavizamiento"),
                                      numericInput( inputId = "parametro_veb_dl_comp", label="Parámetro: ", min = -10, max = 100,step = 0.1, value = 1, width = "40%"),
                                      verbatimTextOutput("spar_veb_dl_comp"),
                                      h2(" Spline a usar"),
                                      verbatimTextOutput("spline_veb_comp_out"),
                                      h2(" Curva spline Tif"),
                                      rbokehOutput("c_veb_splines_dl_comp"),
                                      h2(" Precios estimados"),
                                      box(style="overflow-x:scroll",width = 12,dataTableOutput("p_est_dl_veb_comp")),
                                      h2(" Curva de Rendimientos"),
                                      plotlyOutput("curva_veb_dl_comp")
                             ),#final tabpanel Diebold li 
                             tabPanel("Splines",
                                      h3(" Por favor seleccionar el cantidad de días "),
                                      box(width=12,title="Importante",status="primary",solidHeader=TRUE ,collapsible = TRUE,
                                          collapse= TRUE,"Recuerde que esta es la cantidad de días a considerar hacia atras en el tiempo
                                          con el fin de generar la data con la que se trabajará, por ejemplo si se elige 30 la data 
                                          a considerar serán las observaciones comprendidas entre la fecha de valoración y 
                                          la fecha resultante luego de restarle 30 días a la fecha de valoración" 
                                      ),#final box
                                      numericInput( inputId = "d_veb_comp", label="Días: ", min = 1, max = 100,step = 1, value = 40, width = "40%"),
                                      verbatimTextOutput("dias_veb_comp"),
                                      h2(" Títulos candidatos"),box(style="overflow-x:scroll",width = 12,dataTableOutput("tit_cand_veb_comp")),
                                      h3(" Por favor ingresar el parámetro de suavizamiento "),
                                      box(width=12,title="Importante",status="primary",solidHeader=TRUE ,collapsible = TRUE,
                                          collapse= TRUE,"Recuerde que este valor corresponde al grado de suavidad que tendrá la curva de rendimientos resultante" 
                                      ),#final box
                                      numericInput( inputId = "parametro_veb_comp", label="Parámetro: ", min = -10, max = 100,step = 0.1, value = 1, width = "40%"),
                                      verbatimTextOutput("spar_veb_comp"),
                                      h2(" Precios Splines"),box(style="overflow-x:scroll",width = 12,dataTableOutput("pre_sp_veb_comp")),
                                      h2(" Curva de rendimientos VEBONOS"),
                                      rbokehOutput("c_veb_splines_comp"),#verbatimTextOutput("datos")
                                      h2(" Eliminar observaciones"),
                                      htmlOutput("selectUI_veb_comp"),
                                      h3(" Títulos elegidos"),
                                      verbatimTextOutput("obs_elim_veb_comp"),
                                      h2(" Nuevos títulos candidatos"),
                                      box(style="overflow-x:scroll",width = 12,dataTableOutput("tit_cand_veb_new_comp")),
                                      h2(" Nuevos precios"),
                                      box(style="overflow-x:scroll",width = 12,dataTableOutput("precios_veb_nuevos_comp")),
                                      h2(" Nueva curva de rendimientos"),
                                      rbokehOutput("c_veb_splines_new_comp")
                             )#final tabpanel Splines 
                      )#final tabbox
                      )#final fluid row
                      )#tabpanel veb
                    )#final tabbox tif y vebonos
                    
                  )#final fluidrow que engloba panel tif y veb
                  #  h2(" Comparativo de precios"),
                  # verbatimTextOutput("datos"),
                  #  h2(" Curva de rendimientos comparativa")
                      
              )#final fluid row  
              ),#final tabitem
              
              tabItem(tabName = "precios",
                      h2("Comparativo de precios"),
                      tabBox( width = 12, title = "Instrumentos", id = "precios_comp", height = "50px", 
                              tabPanel("TIF",
                                       box(style="overflow-x:scroll",width = 12,dataTableOutput("comparativo_precios_tif"))
                            
                              ),#final tabpanel
                              tabPanel("VEBONO",
                      
                                       box(style="overflow-x:scroll",width = 12,dataTableOutput("comparativo_precios_veb"))
                      
                              )#final tabpanel
                      )#final tabbox
                      
                      ),#final tabitem
              
              tabItem(tabName = "curvas",
                      h2("Comparativo de curvas"),
                      tabBox( width = 12, title = "Instrumentos", id = "curvas_comp", height = "50px", 
                              tabPanel("TIF",
                                       plotlyOutput("curva_comp_tif"),
                                       h2("Reporte"),
                                       downloadButton("report", "Descargar")
                                       
                                       
                                       
                              ),#final tabpanel
                              tabPanel("VEBONO",
                                       
                                       plotlyOutput("curva_comp_veb"),
                                       h2("Reporte"),
                                       downloadButton("report1", "Descargar")
                                       
                              )#final tabpanel
                      )#final tabbox
                      
                      
                      
              )#final tabitem

                    )#final tabitems
                  )#final dashboardbody
                )#final dashboardpage
        )#final shinyui

