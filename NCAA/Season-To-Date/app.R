library(shiny)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(ggrepel)
library(lubridate)


r.function <- function(x,y){
  return(paste0("R^2: ",round(summary(lm(y ~ x))$r.squared,4)))
}


load.logos <- function(alpha){
  
  logo.info <- read.csv("../../Logos/LogoInfo.csv",header=T,stringsAsFactors=F) %>%
    filter(League == "NCAA")
  
  # Initialize list
  logo.list <- list()
  
  for (i in 1:nrow(logo.info)){
    
    # Open logo PNG
    logo <- readPNG(paste0("../../Logos/NCAA/",logo.info$FileName[i],".png"))
    
    # Convert to rasterGrob and save to list
    logo.list[[i]] <- rasterGrob(matrix(rgb(logo[,,1],logo[,,2],logo[,,3],logo[,,4]*alpha), nrow=dim(logo)[1]))
  }
  
  # Set colour
  logo.color <- logo.info$Color
  
  names(logo.list) <- logo.info$Team
  names(logo.color) <- logo.info$Team
  
  return(list(Logos=logo.list,LogoColors=logo.color)) 
  
}

logos <- load.logos(0.7)
logo.list <- logos$Logos

dates <- "2017-2018"
vars <- c("Pct","AdjPct","OppWinPct","OppOppWinPct","SOS","RPI","PairwiseRank")
var.names <- c("Win Pct.","Adj. Win Pct.","Opp. Win. Pct.","Opp. Opp. Win Pct.","SOS","RPI","Pairwise Rank")


ncaa.plot <- function(x.var,y.var,plot.data,plot.params){
  
  if (x.var == "PairwiseRank"){
    temp <- x.var
    x.var <- y.var
    y.var <- temp
  }
  
  x.params <- plot.params[[x.var]]
  y.params <- plot.params[[y.var]]
  
  x.val <- x.params$val
  x.label <- x.params$label
  x.column <- x.params$column
  x.stretch <- x.params$stretch
  
  y.val <- y.params$val
  y.label <- y.params$label
  y.column <- y.params$column
  y.stretch <- y.params$stretch
  
  if (x.var == "PairwiseRank" | y.var == "PairwiseRank"){
    
    p <- ggplot(data=plot.data,aes_string(x=x.var,y=y.var)) + 
      geom_point(size=0.01) +
      geom_vline(xintercept=x.val,color="red",linetype="dotted") +
      geom_hline(yintercept=y.val,color="red",linetype="dotted") +
      annotate("rect",xmin=x.val,xmax=Inf,ymin=y.val,ymax=Inf,fill="yellow",alpha=0.2) +
      annotate("rect",xmin=x.val,xmax=-Inf,ymin=y.val,ymax=Inf,fill="red",alpha=0.1) +
      annotate("rect",xmin=x.val,xmax=Inf,ymin=y.val,ymax=-Inf,fill="green",alpha=0.1) +
      annotate("rect",xmax=x.val,xmin=-Inf,ymax=y.val,ymin=-Inf,fill="yellow",alpha=0.2) +
      scale_y_continuous(trans = "reverse", breaks=1:60) +
      mapply(
        function(xx,yy,tt) {
          annotation_custom(logo.list[[tt]],xmin=xx-x.stretch, xmax=xx+x.stretch, ymin=yy-y.stretch, ymax=yy+y.stretch)
        }, x.column,(-1) * y.column, plot.data$Team
      ) +
      geom_label_repel(aes(label=Team),size=3,alpha=0.5) +
      theme_mathieu(base_size = 15,
                    title.label = paste0(tag,": ",x.label," vs. ",y.label),
                    subtitle.label = "",
                    x.label = x.label,
                    y.label = y.label,
                    x.watermark = (max(x.column) + min(x.column)) / 2,
                    y.watermark = 30,
                    watermark.size = 30)
    
  } else {
    
    x.stretch <- x.params$stretch
    y.stretch <- y.params$stretch
    
    p <- ggplot(data=plot.data,aes_string(x=x.var,y=y.var)) + 
      geom_point(size=0.01) +
      geom_smooth(method="lm",se=F,size=0.5) +
      geom_vline(xintercept=x.val,color="red",linetype="dotted") +
      geom_hline(yintercept=y.val,color="red",linetype="dotted") +
      annotate("rect",xmin=x.val,xmax=Inf,ymin=y.val,ymax=Inf,fill="green",alpha=0.1) +
      annotate("rect",xmin=x.val,xmax=-Inf,ymin=y.val,ymax=Inf,fill="yellow",alpha=0.2) +
      annotate("rect",xmin=x.val,xmax=Inf,ymin=y.val,ymax=-Inf,fill="yellow",alpha=0.2) +
      annotate("rect",xmax=x.val,xmin=-Inf,ymax=y.val,ymin=-Inf,fill="red",alpha=0.1) +
      mapply(
        function(xx,yy,tt) {
          annotation_custom(logo.list[[tt]],xmin=xx-x.stretch, xmax=xx+x.stretch, ymin=yy-y.stretch, ymax=yy+y.stretch)
        }, x.column, y.column, plot.data$Team
      ) +
      geom_label_repel(aes(label=Team),size=3,alpha=0.5) +
      theme_mathieu(base_size = 15,
                    title.label = paste0(tag,": ",y.label," vs. ",x.label," (",r.function(x.column, y.column),")"),
                    subtitle.label = "",
                    x.label = x.label,
                    y.label = y.label,
                    x.watermark = (max(x.column) + min(x.column)) / 2,
                    y.watermark = (max(y.column) + min(y.column)) / 2,
                    watermark.size = 30)
    
  } 
  
  return(p)
}

ui <- shinyUI(fluidPage(
  
  titlePanel("NCAA Plots"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectizeInput("plot.date", "Plot Season-To-Date", dates),
      selectizeInput("first.varname", "Plot", choices=var.names, selected="Adj. Win Pct."),
      selectizeInput("second.varname", "vs.", choices=var.names[1:6], selected="Win Pct."),
      
      br(),
      
      p("By Mathieu Bray. Data from ", a("College Hockey Stats",href="http://collegehockeystats.net/1718/schedules/all")),
      
      br(),
      
      downloadButton("downloadPlot","Download")
    ),
    
    mainPanel(
      plotOutput("NCAAPlot",width="1000px",height="800px")
    )
  )
))


server <- shinyServer(function(input, output) {
  
  update.data <- reactive ({
    
    directory <- paste0("../Season-To-Date/",input$plot.date)
    
    record <- read_csv(paste0(directory,"/Record.csv"))
    
    plot.data <- record %>% 
      select(Team,Pct:Pairwise) %>%
      arrange(desc(Pairwise),desc(RPI)) %>%
      mutate(PairwiseRank = 1:60,
             Top10 = PairwiseRank <= 10)
    
    top10 <- plot.data %>%
      filter(Top10)
    
    
    plot.parameters <- list(
      
      Pct = list(val = min(top10$Pct) - 0.005,
                 label = "Unadjusted Win Pct.",
                 column = plot.data$Pct,
                 stretch = 0.015),
      
      AdjPct = list(val = min(top10$AdjPct) - 0.005,
                    label = "Adjusted Win Pct.",
                    column = plot.data$AdjPct,
                    stretch = 0.015),
      
      OppWinPct = list(val = min(top10$OppWinPct) - 0.001,
                       label = "Opponent's Win Pct.",
                       column = plot.data$OppWinPct,
                       stretch = 0.005),
      
      OppOppWinPct = list(val = min(top10$OppOppWinPct) - 0.001,
                          label = "Opponent's Opponent's Win Pct.",
                          column = plot.data$OppOppWinPct,
                          stretch = 0.005),
      
      SOS = list(val = min(top10$SOS) - 0.001,
                 label = "Strength of Schedule",
                 column = plot.data$SOS,
                 stretch = 0.005),
      
      RPI = list(val = min(top10$RPI) - 0.001,
                 label = "Unadjusted RPI",
                 column = plot.data$RPI,
                 stretch = 0.005),
      
      PairwiseRank = list(val = 10.1,
                          label = "Pairwise Rank",
                          column = plot.data$PairwiseRank,
                          stretch = 1)
    )
    
    return(list(data=plot.data,
                parameters=plot.parameters))
    
  })
  
 
  
  output$NCAAPlot <- renderPlot({
    
    date <- input$plot.date
    
    plot <- update.data()
    
    plot.data <- plot$data
    plot.params <- plot$parameters
    
    y.var <- vars[which(var.names == input$first.varname)]
    x.var <- vars[which(var.names == input$second.varname)]
    
    ncaa.plot(x.var,y.var,plot.data,plot.params)
    
  })
  
  download.plot <- reactive({
    
    date <- input$plot.date
    
    plot <- update.data()
    
    plot.data <- plot$data
    plot.params <- plot$parameters
    
    y.var <- vars[which(var.names == input$first.varname)]
    x.var <- vars[which(var.names == input$second.varname)]
    
    p <- ncaa.plot(x.var,y.var,plot.data,plot.params)
    
    return(p)
  })
  
  
  output$downloadPlot <- downloadHandler(
    
    filename <- reactive({ paste0(vars[which(var.names == input$first.varname)],"vs",vars[which(var.names == input$second.varname)],".png") }),
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = 1000, height = 800, units = "px")
      }
      ggsave(file, plot = download.plot(), device = device)
    },
    contentType = "image/png"
  )
})


shinyApp(ui = ui, server = server)




