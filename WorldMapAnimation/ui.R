library(shiny)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("World Population"),
  
  # Sidebar with a slider input for number of bins 
    
       sliderInput("year",
                   "Year",
                   min = 1950,
                   max = 2100,
                   value = 2017,
                   animate = T),
    
    # Show a plot of the generated distribution
  mainPanel(
    plotOutput("world_map")
    )
  )
)

