library(shiny)

ui <- fluidPage(
  h1("Below plot describes a histogram plot based on the number and distribution selected by the user",
     style="color:blue;"),
  
  sliderInput(inputId = "num", label="Choose a number to create a distribution", value=20, min=1, max=100),
  actionButton(inputId="norm", label="Random Normal"),
  actionButton(inputId="pois", label="Poisson"),
  actionButton(inputId="unif", label="Random Uniform"),
  plotOutput(outputId = "hist"),
  verbatimTextOutput("stats")
  
)

server <- function(input, output) {
  rv <-reactiveValues(data=rnorm(100), title="")
  observeEvent(input$norm,{ 
    rv$data<-rnorm(input$num) 
    rv$title <- "Normal"
  })
  
  observeEvent(input$pois,{ 
    rv$data<-rpois(input$num, lambda=1)
    rv$title <- "Poisson"
  })
  
  observeEvent(input$unif,{ 
    rv$data<-runif(input$num)
    rv$title <- "Uniform"
  })
  
  output$hist <-renderPlot({
    hist(rv$data, main=paste("Histogram of", {input$num}, {rv$title}, "Numbers",sep=" "))
  })
  output$stats <- renderPrint({summary((rv$data))})
  
}

shinyApp(ui, server)