library(shiny)
library(dplyr)
library(DT)
library(leaflet)
library(sf)

# 데이터 불러오기 (최신 데이터 파일로 변경 필요)
data <- 최신_2  

# 데이터의 최소 계약년월 값 계산
min_contract_date <- min(data$ymd)

ui <- fluidPage(
  titlePanel("부동산 데이터 시각화"),
  
  tabsetPanel(
    tabPanel("지도", 
             sidebarLayout(
               sidebarPanel(
                 dateRangeInput("date_range", "계약년월 범위:", start = min_contract_date, end = NULL),
                 sliderInput("area_range", "전용면적 범위:", min = 0, max = 500, value = c(50, 100)),
                 sliderInput("lease_range", "전세가율 범위:", min = 0, max = 100, value = c(70, 100)),
                 sliderInput("price_gap_range", "최고가대비 범위:", min = 0, max = 200, value = c(0, 80)),
                 sliderInput("gap_range", "Gap 범위:", min = 0, max = 50000, value = c(0, 3000)),
                 sliderInput("years_range", "경과년수 범위:", min = 0, max = 50, value = c(0, 10)),
                 
                 downloadButton("download_data", "데이터 다운로드")
               ),
               mainPanel(
                 leafletOutput("map", width = "100%", height = "500px"),
                 DTOutput("map_datatable")
               )
             )),
    
    tabPanel("테이블", 
             DTOutput("datatable"))
  )
)

server <- function(input, output) {
  filtered_data <- reactive({
    filtered <- data %>%
      filter(전용면적_1 >= input$area_range[1] & 전용면적_1 <= input$area_range[2],
             전세가율 >= input$lease_range[1] & 전세가율 <= input$lease_range[2],
             최고가대비 >= input$price_gap_range[1] & 최고가대비 <= input$price_gap_range[2],
             Gap >= input$gap_range[1] & Gap <= input$gap_range[2],
             경과년수 >= input$years_range[1] & 경과년수 <= input$years_range[2],
             ymd >= input$date_range[1] & ymd <= input$date_range[2])  # 변경된 부분
    return(filtered)
  })
  
  output$map <- renderLeaflet({
    leaflet(filtered_data(), options = leafletOptions(language = 'ko')) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addCircleMarkers(
        ~st_coordinates(geometry)[, 1], 
        ~st_coordinates(geometry)[, 2], 
        popup = ~paste("주소:", juso_1, "<br>",
                       "거래금액:", 거래금액, "<br>",
                       "전세가율:", 전세가율, "<br>",
                       "최고가대비:", 최고가대비, "<br>",
                       "연식:", 경과년수, "<br>",
                       "Gap:", Gap),
        radius = 5,  # Adjust the radius to change marker size
        group = "markers"
      ) %>%
      setView(lng = 127.5, lat = 36.5, zoom = 6.5)
  })
  
  output$map_datatable <- renderDT({
    click_lat <- input$map_marker_click$lat
    click_lng <- input$map_marker_click$lng
    selected_data <- data %>%
      filter(st_coordinates(geometry)[, 1] == click_lng & st_coordinates(geometry)[, 2] == click_lat)
    datatable(selected_data, 
              options = list(columnDefs = list(list(visible = FALSE, targets = c(13))),
                             pageLength = 100))  # Default number of rows to display
  })
  
  output$datatable <- renderDT({
    datatable(filtered_data(), 
              options = list(columnDefs = list(list(visible = FALSE, targets = c(13))),
                             pageLength = 100))  # Default number of rows to display
  })
  
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste("filtered_data_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv", sep = "")
    },
    content = function(file) {
      filtered <- filtered_data() %>%
        select(-geometry)  # geometry 열 제외
      write.csv(filtered, file, row.names = FALSE, fileEncoding = "cp949")  # 변경된 부분
    }
  )
}

shinyApp(ui, server)
