library(dplyr)
library(ggplot2)
library(ggmap)

# Load logo images

logo.images <- read.csv("Logos/LogoInfo.csv",header=T,stringsAsFactors=F) %>%
  filter(League == "NCAA") %>%
  mutate(Team = ifelse(Team == "Lake Superior","Lake Superior State",Team),
         Image = paste0("Logos/NCAA/",FileName,".png"),
         Color = ifelse(Team == "Miami","#EF0000",Color)) %>%
  select(Team,Image,Color)

# Load player info

players <- read.csv("NCAA/Projects/Maps/PlayersHometown.csv",header=T,stringsAsFactors=F) %>%
  left_join(logo.images,by="Team")

# Extract colors

team.colors <- logo.images$Color

# Add some jitter to the locations (so that players in one city aren't stacked on top of one another)

set.seed(90707)

players$Jitter.Lon <- runif(nrow(players),min=-0.025,max=0.025)
players$Jitter.Lat <- runif(nrow(players),min=-0.025,max=0.025)

# Options

teams <- unique(players$Team)
area <- "USA"
zoom <- 4

center <- geocode(area, output = "latlona", source = "google")
center.lon <- as.numeric(center[1])
center.lat <- as.numeric(center[2])

player.data <- players %>% filter(Team %in% teams)


### Plot map using packages 'ggmap' and 'ggiraph'

library(ggiraph)
library(ggimage)

# With logos

plot <- ggmap(get_map(area, zoom = zoom)) +
  geom_image(aes(x = Lon + Jitter.Lon, y = Lat + Jitter.Lat, image=Image), data = player.data, size=0.03, alpha=0.8) +
  ggplot2::annotate("text",x=center.lon,y=center.lat,col="red",label="@mathieubray",alpha=0.2,cex=30,fontface="bold",angle=30) +
  geom_point_interactive(aes(x=Lon + Jitter.Lon, y=Lat + Jitter.Lat, tooltip=Label),size=15,alpha=0.01,data=player.data) +
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())

ggiraph(code={print(plot)},width=1,width_svg=16,height_svg=12)


# Without logos
  
plot <- ggmap(get_map(area, zoom = zoom)) +
  #geom_image(aes(x = Lon + Jitter.Lon, y = Lat + Jitter.Lat, image=Image), data = player.data, size=0.03, alpha=0.8) +
  ggplot2::annotate("text",x=center.lon,y=center.lat,col="red",label="@mathieubray",alpha=0.2,cex=30,fontface="bold",angle=30) +
  geom_point_interactive(aes(x=Lon + Jitter.Lon, y=Lat + Jitter.Lat, tooltip=Label, color=Team),size=10,alpha=0.5,data=player.data) +
  scale_color_manual(values=team.colors) +
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank(),
        legend.position="bottom",legend.title = element_blank(),legend.text=element_text(size=20)) + 
  guides(color = guide_legend(override.aes = list(alpha = 1)))

ggiraph(code={print(plot)},width=1,width_svg=16,height_svg=12)


### Plot map using package 'leaflet'

library(leaflet)

get.file <- function(team){
  return(filter(logo.images,Team==team)$Image[1])
}

teamIcons <- iconList( # Is there a better way to do this?
  
  "Air Force" = makeIcon(get.file("Air Force")),
  "Alabama-Huntsville" = makeIcon(get.file("Alabama-Huntsville")),
  "Alaska" = makeIcon(get.file("Alaska")),
  "Alaska-Anchorage" = makeIcon(get.file("Alaska-Anchorage")), 
  "American International" = makeIcon(get.file("American International")),
  "Arizona State" = makeIcon(get.file("Arizona State")),
  "Army" = makeIcon(get.file("Army")),
  "Bemidji State" = makeIcon(get.file("Bemidji State")), 
  "Bentley" = makeIcon(get.file("Bentley")),
  "Boston College" = makeIcon(get.file("Boston College")),
  "Boston University" = makeIcon(get.file("Boston University")),
  "Bowling Green" = makeIcon(get.file("Bowling Green")),
  "Brown" = makeIcon(get.file("Brown")),
  "Canisius" = makeIcon(get.file("Canisius")),
  "Clarkson" = makeIcon(get.file("Clarkson")),
  "Colgate" = makeIcon(get.file("Colgate")),
  "Colorado College" = makeIcon(get.file("Colorado College")), 
  "Connecticut" = makeIcon(get.file("Connecticut")),
  "Cornell" = makeIcon(get.file("Cornell")),
  "Dartmouth" = makeIcon(get.file("Dartmouth")),
  "Denver" = makeIcon(get.file("Denver")),
  "Ferris State" = makeIcon(get.file("Ferris State")), 
  "Harvard" = makeIcon(get.file("Harvard")),
  "Holy Cross" = makeIcon(get.file("Holy Cross")),
  "Lake Superior State" = makeIcon(get.file("Lake Superior State")),
  "Maine" = makeIcon(get.file("Maine")),
  "Massachusetts" = makeIcon(get.file("Massachusetts")),
  "Massachusetts-Lowell" = makeIcon(get.file("Massachusetts-Lowell")),
  "Mercyhurst" = makeIcon(get.file("Mercyhurst")),
  "Merrimack" = makeIcon(get.file("Merrimack")),
  "Miami" = makeIcon(get.file("Miami")),
  "Michigan" = makeIcon(get.file("Michigan")),
  "Michigan State" = makeIcon(get.file("Michigan State")),
  "Michigan Tech" = makeIcon(get.file("Michigan Tech")),
  "Minnesota" = makeIcon(get.file("Minnesota")),
  "Minnesota State" = makeIcon(get.file("Minnesota State")),
  "Minnesota-Duluth" = makeIcon(get.file("Minnesota-Duluth")),
  "Nebraska-Omaha" = makeIcon(get.file("Nebraska-Omaha")),
  "New Hampshire" = makeIcon(get.file("New Hampshire")),
  "Niagara" = makeIcon(get.file("Niagara")),
  "North Dakota" = makeIcon(get.file("North Dakota")),
  "Northeastern" = makeIcon(get.file("Northeastern")),
  "Northern Michigan" = makeIcon(get.file("Northern Michigan")),
  "Notre Dame" = makeIcon(get.file("Notre Dame")),
  "Ohio State" = makeIcon(get.file("Ohio State")),
  "Penn State" = makeIcon(get.file("Penn State")),
  "Princeton" = makeIcon(get.file("Princeton")),
  "Providence" = makeIcon(get.file("Providence")),
  "Quinnipiac" = makeIcon(get.file("Quinnipiac")),
  "Rensselaer" = makeIcon(get.file("Rensselaer")),
  "RIT" = makeIcon(get.file("RIT")),
  "Robert Morris" = makeIcon(get.file("Robert Morris")),
  "Sacred Heart" = makeIcon(get.file("Sacred Heart")),
  "St. Cloud State" = makeIcon(get.file("St. Cloud State")),
  "St. Lawrence" = makeIcon(get.file("St. Lawrence")),
  "Union" = makeIcon(get.file("Union")),
  "Vermont" = makeIcon(get.file("Vermont")),
  "Western Michigan" = makeIcon(get.file("Western Michigan")),
  "Wisconsin" = makeIcon(get.file("Wisconsin")),
  "Yale" = makeIcon(get.file("Yale"))
)

getColor <- function(logos){
  
  lapply(logos$Team, function(team) {
   return((logos %>% filter(Team==team))$Color[1])
  })
  
}

icons <- awesomeIcons(
  
  icon = 'map-marker',
  iconColor = getColor(player.data),
  library = 'glyphicon',
  markerColor = 'white'
  
)


# With Logos

leaflet(data = player.data, options=leafletOptions(worldCopyJump=T)) %>%
  addTiles() %>% # Map
  addControl("<b><p style='color:red; font-family:arial; font-size:40px; opacity:0.2; '>@mathieubray</p>", position="bottomleft") %>% # Watermark
  addMarkers(~Lon+Jitter.Lon, ~Lat+Jitter.Lat, label = ~Label, icon=~teamIcons[Team], labelOptions=labelOptions(offset=c(20,0)), options = markerOptions(opacity=0.8)) %>% # Point with Tooltip
  setView(lng = center.lon, lat = center.lat, zoom = zoom)
  
# Without Logos

leaflet(player.data) %>%
  addTiles() %>% # Map
  addAwesomeMarkers(~Lon, ~Lat, icon = icons, label = ~as.character(Label))# Point with Tooltip
 











df.20 <- quakes[1:20,]

player.20 <- player.data[1:30]

getColor <- function(quakes) {
  sapply(quakes$mag, function(mag) {
    if(mag <= 4) {
      "green"
    } else if(mag <= 5) {
      "orange"
    } else {
      "red"
    } })
}

icons <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'black',
  library = 'ion',
  markerColor = getColor(df.20)
)

leaflet(df.20) %>% addTiles() %>%
  addAwesomeMarkers(~long, ~lat, icon=icons, label=~as.character(mag))
