library(shiny)
library(ggplot2)
library(ggthemes)
library(png)
library(grid)
library(dplyr)
library(lubridate)
library(ggiraph)


rink <- readPNG("images/rink-cropped.png")
hit.table <- read.csv("data/hits.csv",header=T,stringsAsFactors=F) 
player.names <- sort(unique(hit.table$Player))


ui <- shinyUI(fluidPage(
  
  titlePanel("Graphical Representation of Individual Toughness (G.R.I.T.) Chart"),
  
  sidebarLayout(
    
    sidebarPanel(
      p("Data courtesy of NHLScrapR, ", a("war-on-ice.com",href="http://war-on-ice.com/")),
      p("Hover over points to get more info on the HITZ!!?#?!"),
      selectizeInput("player", "Player", player.names),
      downloadButton("downloadPlot","Download Chart")
    ),
    
    mainPanel(
      ggiraphOutput('hitplot')
    )
  )
  
))


server <- shinyServer(function(input, output) {
  
  makeReactiveBinding("nhltable")
  
  
  modifData <- reactive ({
    return(filter(hit.table,Player==input$player))
  })
  
  downloadHitPlot <- reactive ({
    
    hit.subtable <- modifData()
    
    plot <- ggplot(data=hit.subtable,aes(x=X,y=Y,fill=Zone)) + 
      annotation_custom(rasterGrob(rink, width=unit(1,"npc"), height=unit(1,"npc")),-Inf, Inf, -Inf, Inf) +
      geom_point(size=6,alpha=0.8,pch=21,color="black") + 
      scale_x_continuous(expand=c(0,0)) +
      scale_y_continuous(expand=c(0,0)) +
      theme_few() +
      scale_fill_manual(name="Zone",values=c("blue","green","purple")) +
      theme(axis.ticks.x=element_blank(),axis.ticks.y=element_blank(),axis.text.x=element_blank(),axis.text.y=element_blank(),axis.title.x=element_blank(),axis.title.y=element_blank())+
      ggplot2::annotate("text",x=0,y=0,col="red",label=paste0("MATHIEU BRAY ",year(today())),alpha=0.15,cex=20,fontface="bold",angle=30)
    
    return(plot)
    
  })
  
  
 output$hitplot <- renderggiraph({
  #output$hitplot <- renderPlot({
    
    hit.subtable <- modifData() %>%
      rowwise() %>%
      mutate(Tooltip = gsub(paste0("Victim: ",OpposingPlayer," (",OpposingTeam,")\n",
                              "Date: ",Date,"\n",
                              "Period: ",Period),pattern="'",replacement=""))
    
    plot <- ggplot(data=hit.subtable,aes(x=X,y=Y,fill=Zone,tooltip=Tooltip)) + 
      annotation_custom(rasterGrob(rink, width=unit(1,"npc"), height=unit(1,"npc")),-Inf, Inf, -Inf, Inf) +
      scale_x_continuous(expand=c(0,0)) +
      scale_y_continuous(expand=c(0,0)) +
      theme_few() +
      scale_fill_manual(name="Zone",values=c("blue","green","purple")) +
      theme(axis.ticks.x=element_blank(),axis.ticks.y=element_blank(),axis.text.x=element_blank(),axis.text.y=element_blank(),axis.title.x=element_blank(),axis.title.y=element_blank())+
      ggplot2::annotate("text",x=0,y=0,col="red",label=paste0("MATHIEU BRAY ",year(today())),alpha=0.15,cex=20,fontface="bold",angle=30) +
      geom_point_interactive(size=6,alpha=0.8,pch=21,color="black")
    
    ggiraph(code={print(plot)},width =0.8,width_svg=12,height_svg=6)
  
  })
  
  
  output$downloadPlot <- downloadHandler(
    
    filename <- reactive({ paste(input$player,".png",sep="") }),
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = 800, height = 400, units = "px",res=72)
      }
      ggsave(file, plot = downloadHitPlot(), device = device)
    },
    contentType = "image/png"
  )
})


shinyApp(ui = ui, server = server)




