# Cargar las bibliotecas necesarias
library(shiny)
library(shinydashboard)
library(ggplot2)
library(PulsoSocialColombia)
library(glue)

# Simular algunos datos
datos <-PulsoSocialColombia::ds_pulso

# Definir la interfaz de usuario
ui <- dashboardPage(
  dashboardHeader(title = "Mi Dashboard"),
  dashboardSidebar(
    selectInput("funcion", "Elige una función:", choices = c("Mapas","Mapas Porcentual" ,
                                                             "Scatter","Scatter Time",
                                                             "Static","Trend")),
    selectInput("variable", "Elige una variable:", choices = datos$var_id),
    selectInput("year", "Elige una año:", choices = datos$time),
    downloadButton("descargarGrafico", "Descargar Gráfico"),
    downloadButton("descargarDatos", "Descargar Datos")
  ),
  dashboardBody(
    plotOutput("grafico")
  )
)

# Definir el servidor
server <- function(input, output) {
  grafico <- reactive({
    if (input$funcion == "Mapas") {
      pulso_map(id = glue("{input$variable}"), type_p="print")
    } else if (input$funcion == "Trend") {
      pulso_trend(id=glue("{input$variable}"),type="print")
    } else if  (input$funcion == "Mapas Porcentual"){
      pulso_map_change(id = glue("{input$variable}"), type_p="print")
    } else if  (input$funcion == "Scatter"){
      pulso_scatter(id = glue("{input$variable}"), type_p="print")
    } else if  (input$funcion == "Scatter Time"){
      pulso_scatter_time(id = glue("{input$variable}"), type_p="print")
    } else if (input$funcion == "Static"){
      pulso_static(id = glue("{input$variable}"), type_p="print")
    } else if (input$funcion == "Trend"){
      pulso_trend(id = glue("{input$variable}"), type_p="print")
    }
  })

  output$grafico <- renderPlot({
    grafico()
  })

  output$descargarGrafico <- downloadHandler(
    filename = function() {
      paste("grafico-", Sys.Date(), ".pdf", sep="")
    },
    content = function(file) {
      ggsave(file, plot = grafico(), device = "pdf", width = 10, height = 7)
    }
  )

  output$descargarDatos <- downloadHandler(
    filename = function() {
      paste("datos-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(datos[datos$var_id==glue("{input$variable}"),], file)
    }
  )
}

# Ejecutar la aplicación
shinyApp(ui = ui, server = server)
