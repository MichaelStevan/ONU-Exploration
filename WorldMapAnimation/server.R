library(shiny)

shinyServer(function(input, output) {
   
  output$world_map <- renderPlot({
    
    year = input$year
    
    plot(1:10)
    
  })
  
})
