library(data.table)
library(ggplot2)
library(rgdal)
library(shiny)

shinyServer(function(input, output) {
  
  # Map precomputations
  dt = fread('../data/poblacion_tratada_mapa.csv')
  dt[,Location:=factor(Location)]
  
  output$world_map <- renderPlot({
    
    mapYear = input$year
    
    dtYear = dt[Time==mapYear, .(Location,PopTotal)]

    map.world = fread('../data/map_world.csv')
    setkey(map.world,Location)
    setkey(dtYear,Location)
    map.world_joined = dtYear[map.world,allow.cartesian=T]
    
    ggplot() +
      geom_polygon(data = map.world_joined, aes(x = long, y = lat, group = group, fill = PopTotal)) +
      scale_fill_distiller(palette = "Spectral", na.value = "transparent")+
      labs(title = 'Population by Countries'
           ,subtitle = mapYear, fill="") +
      theme(text = element_text(family = "Gill Sans", color="#444444" )
            ,panel.grid = element_blank()
            ,plot.title = element_text(size = 20)
            ,plot.subtitle = element_text(size = 10)
            ,axis.text = element_blank()
            ,axis.title = element_blank()
            ,axis.ticks = element_blank()
            ,legend.position = "right"
      )
  })
  
})
