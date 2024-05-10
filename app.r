library(shiny)

# UI 부분
ui <- fluidPage(
  titlePanel("간단한 색상 선택 앱"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("red", "빨강:", min = 0, max = 255, value = 128),
      sliderInput("green", "녹색:", min = 0, max = 255, value = 128),
      sliderInput("blue", "파랑:", min = 0, max = 255, value = 128)
    ),
    mainPanel(
      uiOutput("colorOutput")
    )
  )
)

# Server 부분
server <- function(input, output) {
  output$colorOutput <- renderUI({
    color <- rgb(input$red, input$green, input$blue, maxColorValue = 255)
    div(style = sprintf("background-color: %s; width: 200px; height: 200px", color))
  })
}

# Shiny 앱 실행
shinyApp(ui = ui, server = server)

