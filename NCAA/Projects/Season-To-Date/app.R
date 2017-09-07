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
  
  logo.info <- read.csv("../../../Logos/LogoInfo.csv",header=T,stringsAsFactors=F) %>%
    filter(League == "NCAA")
  
  # Initialize list
  logo.list <- list()
  
  for (i in 1:nrow(logo.info)){
    
    # Open logo PNG
    logo <- readPNG(paste0("../../../Logos/NCAA/",logo.info$FileName[i],".png"))
    
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

#dates <- list.dirs(full.names=F)
#dates <- dates[dates != ""]

dates <- "2016-2017_PreNCAA"

vars <- c("WinPct","AdjWinPct","SOS","RPI","QWBAdjRPI",
          "WinRank","AdjWinRank","SOSRank","RPIRank","QWBAdjRPIRank","PWRank")

var.names <- c("Win Pct.","Adj. Win Pct.","SOS","RPI","QWB Adj. RPI",
               "Win Rank","Adj. Win Rank","SOS Rank","RPI Rank","QWB Adj. RPI Rank","Pairwise Rank")

ncaa.plot <- function(x.var,y.var,rpi.data,plot.params,tag){
  
  x.params <- plot.params[[x.var]]
  y.params <- plot.params[[y.var]]
  
  x.val <- x.params$val
  x.label <- x.params$label
  x.column <- x.params$column
  
  y.val <- y.params$val
  y.label <- y.params$label
  y.column <- y.params$column
  
  if (x.params$rank & y.params$rank){
    
    x.annot <- x.params$annot
    y.annot <- y.params$annot
    
    p <- ggplot(data=rpi.data,aes_string(x=x.var,y=y.var)) + 
      geom_point(size=0.01) +
      xlab(x.label) +
      ylab(y.label) +
      geom_vline(xintercept=x.val,color="red",linetype="dotted") +
      geom_hline(yintercept=y.val,color="red",linetype="dotted") +
      ggtitle(paste0(tag,": ",y.label," vs. ",x.label)) +
      annotate("rect",ymin=y.val,ymax=Inf,xmin=x.val,xmax=Inf,fill="green",alpha=0.1) +
      annotate("rect",ymin=y.val,ymax=-Inf,xmin=x.val,xmax=Inf,fill="yellow",alpha=0.2) +
      annotate("rect",ymin=y.val,ymax=Inf,xmin=x.val,xmax=-Inf,fill="yellow",alpha=0.2) +
      annotate("rect",ymax=y.val,ymin=-Inf,xmax=x.val,xmin=-Inf,fill="red",alpha=0.1) +
      mapply(
        function(xx,yy,tt) {
          annotation_custom(logo.list[[tt]],xmin=x.annot-xx, xmax=x.annot-xx+2, ymin=y.annot-yy, ymax=y.annot-yy+2)
        }, x.column, y.column, rpi.data$Team
      ) +
      geom_label_repel(aes(label=Team),size=3,alpha=0.5) +
      annotate("text",x=median(as.numeric(as.character(x.column))),y=median(as.numeric(as.character(y.column))),col="red",label="@mathieubray",alpha=0.1,cex=30,fontface="bold",angle=30) +
      theme_few(15)
    
  } else if (!x.params$rank & !y.params$rank){
    
    x.stretch <- x.params$stretch
    y.stretch <- y.params$stretch
    
    p <- ggplot(data=rpi.data,aes_string(x=x.var,y=y.var)) + 
      geom_point(size=0.01) +
      geom_smooth(method="lm",se=F,size=0.5) +
      xlab(x.label) +
      ylab(y.label) +
      geom_vline(xintercept=x.val,color="red",linetype="dotted") +
      geom_hline(yintercept=y.val,color="red",linetype="dotted") +
      ggtitle(paste0(tag,": ",y.label," vs. ",x.label," (","R^2: ",round(summary(lm(y.column ~ x.column))$r.squared,4),")")) +
      annotate("rect",ymin=y.val,ymax=Inf,xmin=x.val,xmax=Inf,fill="green",alpha=0.1) +
      annotate("rect",ymin=y.val,ymax=-Inf,xmin=x.val,xmax=Inf,fill="yellow",alpha=0.2) +
      annotate("rect",ymin=y.val,ymax=Inf,xmin=x.val,xmax=-Inf,fill="yellow",alpha=0.2) +
      annotate("rect",ymax=y.val,ymin=-Inf,xmax=x.val,xmin=-Inf,fill="red",alpha=0.1) +
      mapply(
        function(xx,yy,tt) {
          annotation_custom(logo.list[[tt]],xmin=xx-x.stretch, xmax=xx+x.stretch, ymin=yy-y.stretch, ymax=yy+y.stretch)
        }, x.column, y.column, rpi.data$Team
      ) +
      geom_label_repel(aes(label=Team),size=3,alpha=0.5) +
      annotate("text",x=mean(x.column),y=mean(y.column),col="red",label="@mathieubray",alpha=0.1,cex=30,fontface="bold",angle=30) +
      theme_few(15)
    
  } else {
    
    if (y.params$rank){
      
      temp <- x.var
      x.var <- y.var
      y.var <- temp
      
      x.val <- y.params$val
      x.label <- y.params$label
      x.column <- y.params$column
      x.annot <- y.params$annot
      
      y.val <- x.params$val
      y.label <- x.params$label
      y.column <- x.params$column
      y.stretch <- x.params$stretch
      
    } else {
      x.annot <- x.params$annot
      y.stretch <- y.params$stretch
    }
    
    p <- ggplot(data=rpi.data,aes_string(x=x.var,y=y.var)) + 
      geom_point(size=0.01) +
      ggtitle(paste0(tag,": ",x.label," vs. ",y.label)) +
      xlab(x.label) +
      ylab(y.label) +
      geom_vline(xintercept=x.val,color="red",linetype="dotted") +
      geom_hline(yintercept=y.val,color="red",linetype="dotted") +
      annotate("rect",xmin=x.val,xmax=Inf,ymin=y.val,ymax=Inf,fill="green",alpha=0.1) +
      annotate("rect",xmin=x.val,xmax=-Inf,ymin=y.val,ymax=Inf,fill="yellow",alpha=0.2) +
      annotate("rect",xmin=x.val,xmax=Inf,ymin=y.val,ymax=-Inf,fill="yellow",alpha=0.2) +
      annotate("rect",xmax=x.val,xmin=-Inf,ymax=y.val,ymin=-Inf,fill="red",alpha=0.1) +
      mapply(
        function(xx,yy,tt) {
          annotation_custom(logo.list[[tt]],xmin=x.annot-xx, xmax=x.annot-xx+2, ymin=yy-0.05, ymax=yy+0.05)
        }, x.column, y.column, rpi.data$Team
      ) +
      geom_label_repel(aes(label=Team),size=3,alpha=0.5) +
      annotate("text",x=median(as.numeric(as.character(x.column))),y=mean(y.column),col="red",label="@mathieubray",alpha=0.1,cex=30,fontface="bold",angle=30) +
      coord_flip() +
      theme_few(15)
  }
  
  return(p)
}

ui <- shinyUI(fluidPage(
  
  titlePanel("NCAA Plots"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectizeInput("plot.date", "Plot Season-To-Date", dates),
      selectizeInput("first.varname", "Plot", choices=var.names, selected="Adj. Win Pct."),
      selectizeInput("second.varname", "vs.", choices=var.names, selected="Win Pct."),
      
      br(),
      
      p("By Mathieu Bray. Data from ", a("USCHO",href="http://www.uscho.com/rankings/rpi/d-i-men/")),
      
      br(),
      
      downloadButton("downloadPlot","Download")
    ),
    
    mainPanel(
      plotOutput("NCAAPlot",width="1000px",height="800px")
    )
  )
))


server <- shinyServer(function(input, output) {
  
  update.rpi.data <- reactive ({
    
    directory <- paste0("../../Projects/Season-To-Date/",input$plot.date)
    
    uscho <- read.csv(paste0(directory,"/USCHO.csv"),header=T,stringsAsFactors=F)
    
    rpi.data <- uscho %>% 
      rename(PW = PairwiseComparisonsWon,
             QWBAdjRPI = QWBAdjustedRPI,
             WinPct = UnadjustedWinPct,
             AdjWinPct = AdjustedWinPct,
             AdjWinRank = WinRank) %>%
      select(Team, WinPct, AdjWinPct, SOS, RPI, QWBAdjRPI, PW, QWBAdjRPIRank:AdjWinRank) %>%
      mutate(PWRank = factor(dense_rank(desc(PW)),levels=60:1),
             WinRank = factor(dense_rank(desc(WinPct)),levels=60:1),
             AdjWinRank = factor(AdjWinRank,levels=60:1),
             SOSRank = factor(SOSRank,levels=60:1),
             RPIRank = factor(RPIRank,levels=60:1),
             QWBAdjRPIRank = factor(QWBAdjRPIRank,levels=60:1),
             Top16 = PWRank %in% 1:16) %>%
      arrange(desc(PW),desc(QWBAdjRPI)) %>%
      select(Team:PW,WinRank,AdjWinRank,SOSRank,RPIRank,QWBAdjRPIRank,PWRank,Top16)
    
    
    top16 <- rpi.data %>%
      filter(Top16)
    
    plot.parameters <- list(
      
      WinPct = list(val = min(top16$WinPct) - 0.005,
                    label = "Unadjusted Win Pct.",
                    column = rpi.data$WinPct,
                    stretch = 0.01,
                    rank = F),
      
      AdjWinPct = list(val = min(top16$AdjWinPct) - 0.005,
                       label = "Adjusted Win Pct.",
                       column = rpi.data$AdjWinPct,
                       stretch = 0.01,
                       rank = F),
      
      SOS = list(val = min(top16$SOS) - 0.001,
                 label = "Strength of Schedule",
                 column = rpi.data$SOS,
                 stretch = 0.005,
                 rank = F),
      
      RPI = list(val = min(top16$RPI) - 0.001,
                 label = "Unadjusted RPI",
                 column = rpi.data$RPI,
                 stretch = 0.005,
                 rank = F),
      
      QWBAdjRPI = list(val = min(top16$QWBAdjRPI) - 0.001,
                       label = "QWB Adjusted RPI",
                       column = rpi.data$QWBAdjRPI,
                       stretch = 0.005,
                       rank = F),
      
      WinRank = list(val = max(as.numeric(as.character(rpi.data$WinRank))) - (max(as.numeric(as.character(top16$WinRank))) - 0.9),
                     label = "Unadjusted Win Pct. Rank",
                     column = as.numeric(as.character(rpi.data$WinRank)),
                     annot = max(as.numeric(as.character(rpi.data$WinRank))),
                     rank = T),
      
      AdjWinRank = list(val = max(as.numeric(as.character(rpi.data$AdjWinRank))) - (max(as.numeric(as.character(top16$AdjWinRank))) - 0.9),
                        label = "Adjusted Win Pct. Rank",
                        column = as.numeric(as.character(rpi.data$AdjWinRank)),
                        annot = max(as.numeric(as.character(rpi.data$AdjWinRank))),
                        rank = T),
      
      SOSRank = list(val = max(as.numeric(as.character(rpi.data$SOSRank))) - (max(as.numeric(as.character(top16$SOSRank))) - 0.9),
                     label = "Strength of Schedule Rank",
                     column = as.numeric(as.character(rpi.data$SOSRank)),
                     annot = max(as.numeric(as.character(rpi.data$SOSRank))),
                     rank = T),
      
      RPIRank = list(val = max(as.numeric(as.character(rpi.data$RPIRank))) - (max(as.numeric(as.character(top16$RPIRank))) - 0.9),
                     label = "RPI Rank",
                     column = as.numeric(as.character(rpi.data$RPIRank)),
                     annot = max(as.numeric(as.character(rpi.data$RPIRank))),
                     rank = T),
      
      QWBAdjRPIRank = list(val = max(as.numeric(as.character(rpi.data$QWBAdjRPIRank))) - (max(as.numeric(as.character(top16$QWBAdjRPIRank))) - 0.9),
                           label = "QWB Adjusted RPI Rank",
                           column = as.numeric(as.character(rpi.data$QWBAdjRPIRank)),
                           annot = max(as.numeric(as.character(rpi.data$QWBAdjRPIRank))),
                           rank = T),
      
      PWRank = list(val = max(as.numeric(as.character(rpi.data$PWRank))) - (max(as.numeric(as.character(top16$PWRank))) - 0.9),
                    label = "Pairwise Rank",
                    column = as.numeric(as.character(rpi.data$PWRank)),
                    annot = max(as.numeric(as.character(rpi.data$PWRank))),
                    rank = T)
    )
    
    return(list(data=rpi.data,parameters=plot.parameters))
    
  })
  
 
  
  output$NCAAPlot <- renderPlot({
    
    date <- input$plot.date
    
    plot <- update.rpi.data()
    
    rpi.data <- plot$data
    plot.params <- plot$parameters
    
    y.var <- vars[which(var.names == input$first.varname)]
    x.var <- vars[which(var.names == input$second.varname)]
    
    ncaa.plot(x.var,y.var,rpi.data,plot.params,date)
    
  })
  
  download.plot <- reactive({
    
    date <- input$plot.date
    
    plot <- update.rpi.data()
    
    rpi.data <- plot$data
    plot.params <- plot$parameters
    
    y.var <- vars[which(var.names == input$first.varname)]
    x.var <- vars[which(var.names == input$second.varname)]
    
    p <- ncaa.plot(x.var,y.var,rpi.data,plot.params,date)
    
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




