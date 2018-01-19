library(shiny)

runExample("01_hello")

library(shiny)
#library(datasets)
source("C:/Users/01444878/Desktop/脚本/kjl_r/test.R",encoding = "UTF-8")
city_list <- read.table('C:/Users/01444878/Desktop/脚本/kjl_r/city.csv',sep=',',header=TRUE)

ui <- pageWithSidebar(
  
  # Application title.
  headerPanel("KJL"),
  
  sidebarPanel(
    selectInput("dataset", "请选择城市:", 
                choices = city_list[,3]),
    
    #numericInput("obs", "Number of observations to view:", 10),
    
    textInput("text", label = h3("请添加小区名："),value = "请添加小区名"),
    
    submitButton("提交")
  ),
  
  mainPanel(
    h4("查到的户型结果"),
    verbatimTextOutput("summary"),
    
    h4("详情："),
    tableOutput("view")
  )
)

server <- function(input, output) {
  
  # Return the requested dataset
  datasetInput <- reactive({
    switch(input$dataset,
           "rock" = rock,
           "pressure" = pressure,
           "cars" = cars)
  })
  
  # Generate a summary of the dataset
  output$summary <- renderText({paste("https://www.kujiale.com/huxing/","bj","-",input$text,sep="")
  })
  
  # Show the first "n" observations
  output$view <- renderTable({
    head(datasetInput(), n = input$obs)
  })
}

shinyApp(ui = ui, server = server)