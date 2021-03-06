library(shiny)
library(ggplot2)
library(ggthemes)
library(png)
library(grid)
library(dplyr)
library(lubridate)
library(ggiraph)

rink <- readPNG("images/rink-cropped.png")

hits <- read.csv("data/hits-corsica.csv",header=T,stringsAsFactors=F) 

player.names <- read.csv("data/players.csv",header=T,stringsAsFactors=F) %>%
  .$Player


ui <- shinyUI(fluidPage(
  
  titlePanel("Hit Chart"),
  
  sidebarLayout(
    
    sidebarPanel(
      p("Data courtesy of ", a("corsica.hockey",href="http://www.corsica.hockey/")),
      p("Hover over points to get more info"),
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
    
    hit.subtable <- hits %>%
      filter(Player == input$player,
             !is.na(X),
             !is.na(Y))
    
    return(hit.subtable)
    
  })
  
  downloadHitPlot <- reactive ({
    
    hit.subtable <- modifData()
    
    plot <- ggplot(data=hit.subtable,aes(x=X,y=Y,fill=Zone)) + 
      annotation_custom(rasterGrob(rink, width=unit(1,"npc"), height=unit(1,"npc")),-Inf,Inf,-Inf,Inf) +
      geom_point(size=6,alpha=0.8,pch=21,color="black") + 
      scale_x_continuous(expand=c(0,0)) +
      scale_y_continuous(expand=c(0,0)) +
      theme_few() +
      scale_fill_manual(name="Zone",values=c("blue","green","purple")) +
      theme(axis.ticks.x=element_blank(),axis.ticks.y=element_blank(),axis.text.x=element_blank(),axis.text.y=element_blank(),axis.title.x=element_blank(),axis.title.y=element_blank())+
      ggplot2::annotate("text",x=0,y=0,col="red",label=paste0("@mathieubray ",year(today())),alpha=0.15,cex=20,fontface="bold",angle=30)
    
    return(plot)
    
  })
  
  
 output$hitplot <- renderggiraph({
    
    hit.subtable <- modifData() %>%
      rowwise() %>%
      mutate(Tooltip = gsub(paste0("Victim: ",Victim," (",OpposingTeam,")\n",
                              "Date: ",Date,"\n",
                              "Period: ",Period),pattern="'",replacement=""))
    
    plot <- ggplot(data=hit.subtable,aes(x=X,y=Y,fill=Zone,tooltip=Tooltip)) + 
      annotation_custom(rasterGrob(rink, width=unit(1,"npc"), height=unit(1,"npc")),-Inf,Inf,-Inf,Inf) +
      scale_x_continuous(expand=c(0,0)) +
      scale_y_continuous(expand=c(0,0)) +
      theme_few() +
      scale_fill_manual(name="Zone",values=c("blue","green","purple")) +
      theme(axis.ticks.x=element_blank(),axis.ticks.y=element_blank(),axis.text.x=element_blank(),axis.text.y=element_blank(),axis.title.x=element_blank(),axis.title.y=element_blank())+
      ggplot2::annotate("text",x=0,y=0,col="red",label=paste0("@mathieubray ",year(today())),alpha=0.15,cex=20,fontface="bold",angle=30) +
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




