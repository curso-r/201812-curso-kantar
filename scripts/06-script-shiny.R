library(shiny)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel = sidebarPanel(
      sliderInput(
        inputId = "num_obs",
        label = "Selecione o número de observações:",
        min = 1,
        max = 1000,
        value = 100
      )
    ),
    mainPanel = mainPanel(
      plotOutput(outputId = "histograma")
    )
  )
)

server <- function(input, output, session) {
  
  output$histograma <- renderPlot({
    
    amostra <- rnorm(n = input$num_obs)
    
    hist(amostra)
    
  })
  
}

shinyApp(ui, server)