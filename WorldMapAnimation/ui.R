library(shiny)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("World's Life Expectancy"),
  
  # Sidebar with a slider input for number of bins 
    
       sliderInput("year",
                   "Year",
                   min = 1955,
                   max = 2100,
                   value = 2015,
                   step = 5,
                   animate = T),
    
    # Show a plot of the generated distribution
  mainPanel(
    plotOutput("world_map")
    )
  )
)

