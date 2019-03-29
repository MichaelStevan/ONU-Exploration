library(shiny)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("World Population"),
  
  # Sidebar with a slider input for number of bins 
    
       sliderInput("year",
                   "Year",
                   min = 1800,
                   max = 2100,
                   value = 1800,
                   animate = T),
    
    # Show a plot of the generated distribution
       plotOutput("world_map")
  )
)

