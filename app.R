# 필요한 라이브러리를 불러옵니다.
library(shiny)
library(dplyr)
library(openxlsx)
library(DT)

# 식품_1 데이터를 불러옵니다. 파일 경로는 실제 데이터 파일의 경로로 바꿔주세요.
load("~/food_2022.rdata")
data <- 식품_2022

# 전역 변수로 filtered_data를 정의합니다.
filtered_data <- NULL

# UI 부분을 정의합니다.
ui <- fluidPage(
  titlePanel("데이터 필터링 및 시각화"),
  
  sidebarLayout(
    sidebarPanel(
      # 보고년도를 선택할 수 있는 입력 필드를 추가합니다.
      selectInput("year", "보고년도:", choices = c("전체", unique(data$보고년도))),
      # 회사명을 검색할 수 있는 입력 필드를 추가합니다.
      textInput("company", "회사명 검색:"),
      # 제품명을 검색할 수 있는 입력 필드를 추가합니다.
      textInput("product", "제품명 검색:"),
      # 원재료를 검색할 수 있는 입력 필드를 추가합니다.
      textInput("ingredient", "원재료 검색:"),
      # 검색 버튼을 추가합니다.
      actionButton("search", "검색")
    ),
    
    mainPanel(
      # DT 패키지를 사용하여 대화형 테이블을 출력합니다.
      DTOutput("filtered_table"),
      # 엑셀 다운로드 버튼을 추가합니다.
      downloadButton("download_excel", "엑셀 파일 다운로드")
    )
  )
)

# 서버 부분을 정의합니다.
server <- function(input, output) {
  # 검색 버튼을 클릭했을 때 실행되는 이벤트를 정의합니다.
  observeEvent(input$search, {
    # 선택한 보고년도에 맞게 데이터를 필터링합니다.
    if (input$year == "전체") {
      filtered_data <<- data %>%
        # 회사명 검색 필터를 적용합니다.
        filter(회사명 %like% input$company) %>%
        # 제품명 검색 필터를 적용합니다.
        filter(제품명 %like% input$product) %>%
        # 원재료 검색 필터를 적용합니다.
        filter(원재료 %like% input$ingredient) %>%
        select(회사명, 제품명, 보고년도, 생산량, 원재료)
    } else {
      filtered_data <<- data %>%
        filter(보고년도 == input$year) %>%
        # 회사명 검색 필터를 적용합니다.
        filter(회사명 %like% input$company) %>%
        # 제품명 검색 필터를 적용합니다.
        filter(제품명 %like% input$product) %>%
        # 원재료 검색 필터를 적용합니다.
        filter(원재료 %like% input$ingredient) %>%
        select(회사명, 제품명, 보고년도, 생산량, 원재료)
    }
    
    # 필터링된 결과를 DT로 출력합니다.
    output$filtered_table <- renderDT({
      datatable(filtered_data, options = list(pageLength = 10))  # 페이지당 행 수를 조절할 수 있습니다.
    })
  })
  
  # 엑셀 다운로드 버튼을 클릭했을 때 실행되는 이벤트를 정의합니다.
  output$download_excel <- downloadHandler(
    filename = function() {
      paste("filtered_data", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      write.xlsx(filtered_data, file)
    }
  )
}

# Shiny 앱을 실행합니다.
shinyApp(ui = ui, server = server)