#install.packages(c())
library(shiny)
library(DT)
library(leaflet)
library(geojsonio)
library(factoextra)
library(gtools)
library(shinyWidgets)
library(ggplot2)
library(shinythemes)
library(lattice) 
library(latticeExtra) 

#datos necesarios (variables, json con coordenadas, pca)
load('data.RData')
paises <- geojsonio::geojson_read("world.json", what = "sp") 
paises<- sp::merge(x = paises, y = data, by.x = 'iso_a3', by.y ='cod_pais', all.x = FALSE)
datos_aux <- paises[,c(1,19,65:88)]@data
datos_aux <- datos_aux[order(datos_aux$iso_a3),]
paises <- paises[order(paises$iso_a3),19]
data.pca <- prcomp(data[,2:25], center = TRUE, scale. = TRUE)


########
###UI###
########
ui <- navbarPage(theme = shinytheme("yeti"), 
                 "TFG", 
                 id="main",
                 
                 #DATA#
                 tabPanel(icon("home"),
                          fluidRow(column(br(),
                                          HTML('<center><img src="UB.png" width="50"></center>'),
                                          br(),
                                          br(),
                                          HTML('<center><img src="UPC.png" width="50"></center>'),
                                          width = 2
                                          ),
                                   column(p("Esta aplicación tiene por objetivo servir como complemento a la memoria del trabajo de fin grado", strong("insertar_titulo_aquí."),
                                            "De esta manera, el usuario podrá tanto ver e interactuar con resultados ya presentados, como explorar y visualizar nueva información más detalladamente." ,
                                            style="text-align:justify;color:black;background-color:#bfbff2;padding:15px;border-radius:10px"),
                                          br(),
                                          p("En esta primera página se presentan los datos preprocesados provenientes del", em("Databank"), "público del Banco Mundial. Estos datos se componen de una serie
                                             de variables sociales, económicas, sanitarias y demográficas relativas a la gran mayoría de países del mundo.",
                                            style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                                          width = 8
                                          ),
                                   column(br(),
                                          br(), 
                                          br(),
                                          br(),
                                          p(em("Desarrollado por"),
                                            br("Garnica Barco A."),
                                            style="text-align:center; font-family: times"),
                                          width = 2
                                          )
                                   ),
                          hr(),
                          fluidRow(column(DT::dataTableOutput("data"), 
                                          width = 12)
                                   ),
                          hr(),
                          p(em("TFG 2020-2021"),
                            style="text-align:center; font-family: times; font-size: 11px")
                          ),
                 
                 
                 
                 #EDA#
                 tabPanel(title = "EDA", 
                          fluidPage(fluidRow(br(),sidebarLayout(column(sidebarPanel(selectInput(inputId = "variable",
                                                                                    label = "Indicador",
                                                                                    choices = names(datos_aux)[3:26],
                                                                                    selected = 'tasa_fertilidad'
                                                                                    ),
                                                                                    width = 12),
                                                                       br(),
                                                                       br(),
                                                                       br(),
                                                                       br(),
                                                                       br(),
                                                                       br(),
                                                                       br(),
                                                                       br(),
                                                                       br(),
                                                                       p("Se muestra en la primera pestaña",em('Mapa'),"de esta página una vizualización geográfica de la variable seleccionada. En la pestaña", 
                                                                         em('Descriptiva univariante'),"se hace un análisis individual de dicha variable. Finalmente en la pestaña",em('Descriptiva multivariante'),
                                                                         "se muestra un gráfico de dispersión de la variable seleccionada junto con otra variable a seleccionar así como el mismo gráfico coloreado 
                                                                         en función de una tercera variable.",
                                                                         style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                                                                       width = 3
                                                                        ),
                                                                mainPanel(tabsetPanel(tabPanel("Mapa", 
                                                                                          br(), 
                                                                                          leafletOutput("mymap", height = 400)
                                                                                          ),
                                                                                      tabPanel("Descriptiva univariante", 
                                                                                          br(), 
                                                                                          fluidRow(column(width = 6, 
                                                                                                          plotOutput("hist")
                                                                                                          ),
                                                                                                   column(width = 6,
                                                                                                          plotOutput('boxplot')
                                                                                                          )
                                                                                                   ),
                                                                                          fluidRow(br(), column(offset = 3,
                                                                                                          width=6, 
                                                                                                          verbatimTextOutput("summary")
                                                                                                          )
                                                                                                   )
                                                                                          ),
                                                                                      tabPanel("Descriptiva multivariante",
                                                                                          br(), 
                                                                                          fluidRow(column(width = 6, 
                                                                                                          plotOutput("biplot"),
                                                                                                          br(),
                                                                                                          fluidRow(column(offset=2,
                                                                                                                          width= 10,
                                                                                                                          selectInput(inputId = "variable2",
                                                                                                                                      label = "Segundo indicador (eje y)",
                                                                                                                                      choices = names(datos_aux)[3:26],
                                                                                                                                      selected = 'prevalencia_sobrepeso')
                                                                                                                          )
                                                                                                                   )
                                                                                                          ),
                                                                                                   column(width = 6, 
                                                                                                          plotOutput("triplot"),
                                                                                                          br(),
                                                                                                          fluidRow(column(offset=2,
                                                                                                                          width= 10,
                                                                                                                          selectInput(inputId = "variable3",
                                                                                                                                      label = "Tercer indicador (nivel)",
                                                                                                                                      choices = names(datos_aux)[3:26],
                                                                                                                                      selected = 'esperanza_vida')
                                                                                                                          )
                                                                                                                   )
                                                                                                          )
                                                                                                   )
                                                                                          )
                                                                                 )
                                                                     )
                                                           ),
                                             br(), 
                                             br(), 
                                             br(), 
                                             hr(),  
                                             p(em("TFG 2020-2021"), 
                                               style="text-align:center; font-family: times; font-size: 11px")
                                             )
                                    )
                          ),
                 
                 #PCA#
                 tabPanel(title = "PCA",
                          fluidPage(fluidRow(br(),
                                             sidebarLayout(sidebarPanel(pickerInput("paises",
                                                                                    "Países",
                                                                                    choices=row.names(data), 
                                                                                    options = list(`actions-box` = TRUE),
                                                                                    multiple = TRUE,
                                                                                    selected = row.names(data)
                                                                                    ),
                                                                        selectInput(inputId = "dim1",
                                                                                    label = "Elige la dimensión a representar en el eje X",
                                                                                    choices = c(1:24),
                                                                                    selected = "1"),
                                                                        selectInput(inputId = "dim2",
                                                                                    label = "Elige la dimensión a representar en el eje Y",
                                                                                    choices = c(1:24),
                                                                                    selected = "2"),
                                                                        selectInput(inputId = "categoria", 
                                                                                    label = "Elige la variable por la que categorizar", 
                                                                                    choices = names(data[2:25]), 
                                                                                    selected = "esperanza_vida"),
                                                                        selectInput(inputId = "q", 
                                                                                    label = "¿Cuántos grupos quieres formar?", 
                                                                                    choices = c(1:50), 
                                                                                    selected = "3"),
                                                                        pickerInput("variables",
                                                                                    "Variables",
                                                                                    choices=colnames(data[2:25]),
                                                                                    options = list(`actions-box` = TRUE),
                                                                                    multiple = TRUE, 
                                                                                    selected = colnames(data[2:25])
                                                                                    )
                                                                        ),
                                                           mainPanel(tabsetPanel(tabPanel("Países",
                                                                                          plotOutput('pcaind')
                                                                                          ), 
                                                                                 tabPanel("Variables", 
                                                                                          plotOutput('pcavar')
                                                                                          ),
                                                                                 tabPanel("Biplot",
                                                                                          plotOutput('pcabiplot')
                                                                                          )
                                                                                 ),
                                                                                 p("Se puede en esta página interactuar con diferentes representaciones derivadas 
                                                                                   del Análisis de Componentes Principales aplicado a los datos en cuestión.
                                                                                   En la primera pestaña se puede elegir que países proyectar sobre que dimensión,
                                                                                   además de la posibilidad de categorizar por la variable y por el número de grupos que se desee. 
                                                                                   En la pestaña siguiente se proyectan las variables seleccionadas. Y en la última pestaña se combinan ambos elementos, países y variables.",
                                                                                   style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px;font-size:13px")
                                                                     )
                                                           ),
                                             br(),
                                             br(),
                                             hr(),
                                             p(em("TFG 2020-2021"),
                                               style="text-align:center; font-family: times; font-size: 11px")
                                             )
                                    )
                          )
                 )




############
###SERVER###
############
server <- function(input, output) {
  
  
  y<- reactive({ datos_aux[ ,input$variable] })
  y2<- reactive({ datos_aux[ ,input$variable2] })
  y3<- reactive({ datos_aux[ ,input$variable3] })
  
  pal <- reactive({ colorNumeric(palette = "Purples", 
                                 domain = y()) }) #paleta
  labels <- reactive({ sprintf("<strong>%s</strong><br/>%g", 
                               datos_aux$name, 
                               y() ) %>% 
      lapply(htmltools::HTML) })
  
  
  #######
  #tabla#
  #######
  output$data <- DT::renderDataTable(datatable(data, 
                                               options = list(scrollX = TRUE,
                                                              order = list(1, 'asc'),
                                                              initComplete = JS("function(settings, json) {",
                                                                                "$(this.api().table().header()).css({'background-color': '#000', 'color': '#f5f5dc'});",
                                                                                "}"), 
                                                              searching = FALSE
                                                              ), 
                                               colnames=attributes(data)$descripcion_variables,
                                               class = 'cell-border stripe') %>%
                                       formatRound(c(2:18,20:24), 2))
  
  
  #######
  #mapa # 
  #######
tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title { 
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 37%;
    top: 78%;
    margin-top: -100px;
    text-align: center;
    padding-left: 10px; 
    padding-right: 10px; 
    background: rgba(255,255,255,0.75);
    font-weight: bold;
    font-size: 16px;
  }
"))
 
tit1 <- reactive ({ attributes(data)$descripcion_variables[which(names(datos_aux) == input$variable) - 1] })
tit2 <- reactive ({ attributes(data)$unidad_variables[which(names(datos_aux) == input$variable) - 1] })
title <- reactive({ tags$div(tag.map.title, HTML(tit1(),"<br>","(",tit2(),")")) })
    
 
output$mymap <- renderLeaflet({
    leaflet(paises) %>%
    setView(0, 36, 1.26) %>% 
      addTiles() %>% 
      addPolygons(fillColor = ~pal()(y()), 
                  weight = 1,
                  opacity = 1,
                  color = "white",
                  dashArray = "1",
                  fillOpacity = 1,
                  highlight = highlightOptions(
                    weight = 3, color = "#FFFFB3", 
                    bringToFront = TRUE),
                  label = labels(),
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")) %>%
      addLegend(pal = pal(), values = y(), opacity = 1, title = NULL,
                position = "bottomright") %>%
      addMiniMap(toggleDisplay = TRUE, minimized = TRUE)  %>%
      addControl(title(), position = "topleft", className="map-title")
    
  })
  
 
 
  #######
  #histo#
  #######
  y.nombre <- reactive ({ input$variable })
  y2.nombre <- reactive ({ input$variable2 })
  y3.nombre <- reactive ({ input$variable3 })
  output$hist <- renderPlot({ qplot(y(), 
                                    geom ="histogram", 
                                    fill=I('white'), 
                                    color=I('black'), 
                                    xlab = y.nombre(), 
                                    bins=15) })

  ########
  #boxplt#
  ########
  output$boxplot <- renderPlot({ qplot(y(),
                                       geom ='boxplot', 
                                       fill=I('white'), 
                                       color=I('black'), 
                                       xlab = y.nombre()) })
  
  ########
  #summar#
  ########
  output$summary <- renderPrint({ summary(y()) })
  
  ########
  #biplot#
  ########
  output$biplot <- renderPlot({ ggplot(data = datos_aux, 
                                       aes(x=y(), y=y2())) + 
      geom_point() + 
      labs(x = y.nombre(), y = y2.nombre())})
  
  ########
  #triplot#
  ########
  output$triplot <- renderPlot({ levelplot(y3() ~ y() * y2(),
                                           panel = panel.levelplot.points,
                                           cex = 1.2,
                                           xlab = y.nombre(),
                                           ylab = y2.nombre()) + 
      layer_(panel.2dsmoother(..., n = 200)) })
  
  
  ########
  #pcaind#
  ########
  ejes<- reactive({ c(as.numeric(input$dim1), as.numeric(input$dim2)) })
  categoria <- reactive ({ data[,input$categoria] })
  q <- reactive({ as.numeric(input$q) })
  paises2 <- reactive({ input$paises })
  variables <- reactive({ input$variables })
  
  output$pcaind <- renderPlot({ fviz_pca_ind(data.pca, 
                                             axes = ejes(), 
                                             select.ind = list(name = paises2()),
                                             geom = 'text', 
                                             habillage = quantcut(categoria(),
                                                                  q=q()),
                                             palette="Dark2", 
                                             addEllipses=TRUE,
                                             ellipse.level=0.9) + labs(title =(""),
                                                                       x = colnames(data.pca$x)[ejes()][1],
                                                                       y = colnames(data.pca$x)[ejes()][2]) + theme(legend.title = element_blank()) + theme_linedraw(base_line_size = 1, 
                                                                                                                                                                     base_rect_size = 0.2) + xlim(-8.1,6) + ylim(-4,4)
                              })
  
  ########
  #pcavar#
  ########
  
  output$pcavar <- renderPlot({ fviz_pca_var(data.pca, 
                                             axes = ejes(),  
                                             select.var= list(name = variables())) + labs(title =(""),
                                                                                          x = colnames(data.pca$x)[ejes()][1],
                                                                                          y = colnames(data.pca$x)[ejes()][2]) + theme_linedraw(base_line_size = 1,
                                                                                                                                                base_rect_size = 0.5) + xlim(-1.5,1.5)})
  
  
  ########
  #pcabip#
  ########
  
  output$pcabiplot <- renderPlot ({fviz_pca_biplot(data.pca, 
                                                   axes = ejes(), 
                                                   select.var= list(name = variables()), 
                                                   select.ind = list(name = paises2()) ,
                                                   label = 'var', 
                                                   habillage = quantcut(categoria(), 
                                                                        q=q()), 
                                                   palette="Dark2") + labs(title =(""),
                                                                           x = colnames(data.pca$x)[ejes()][1], 
                                                                           y = colnames(data.pca$x)[ejes()][2]) + theme_linedraw(base_line_size = 1,
                                                                                                                                 base_rect_size = 0.2) + xlim(-7,6.5) + ylim(-3.9, 4)})
  
}


shinyApp(server = server, ui = ui)
