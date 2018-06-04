library(shiny)
library(ggplot2)
library(dplyr)

ui <- fluidPage(
  titlePanel("Hello World!"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "bins",
                  label = "Number of bins: ",
                  min = 1,
                  max = 100,
                  value = 30)
    ),
    mainPanel(
      plotOutput(outputId = "histogram")
    )
  )
)

server <- function(input,output) {
  output$histogram <- renderPlot({
    iris %>%
      ggplot(aes(x = Petal.Width)) +
      theme_classic() + 
      geom_histogram(colour = "black",fill = "blue",bins = input$bins + 1) +
      labs(title = "Histogram of Petal Width",
           subtitle = "Iris Dataset",
           x = "Petal Width",
           y = "Count")
  })
}


shinyApp(ui = ui,server = server)
