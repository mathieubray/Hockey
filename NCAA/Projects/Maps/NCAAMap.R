library(dplyr)
library(ggplot2)
library(ggmap)
library(ggiraph)
library(ggimage)
library(leaflet)

# Load team logo images and colors

logo.images <- read.csv("Logos/LogoInfo.csv",header=T,stringsAsFactors=F) %>%
  filter(League == "NCAA") %>%
  mutate(Team = ifelse(Team == "Lake Superior","Lake Superior State",Team),
         Image = paste0("Logos/NCAA/",FileName,".png"),
         Color = ifelse(Team == "Miami","#EF0000",Color)) %>%
  select(Team,Image,Color)

all.teams <- unique(logo.images$Team)

player.colors <- logo.images$Color
names(player.colors) <- all.teams


# Map options

teams <- all.teams
area <- "USA"
zoom <- 4

center <- geocode(area, output = "latlona", source = "google")
center.lon <- as.numeric(center[1])
center.lat <- as.numeric(center[2])


# Load player info

set.seed(90707)

players <- read.csv("NCAA/Projects/Maps/PlayersHometown.csv",header=T,stringsAsFactors=F) %>%
  left_join(logo.images,by="Team") %>%
  rowwise() %>%
  mutate(Lon = Lon + runif(1,min=-0.025,max=0.025), # Add some jitter to coordinates (so that players in one city aren't stacked on top of one another)
         Lat = Lat + runif(1,min=-0.025,max=0.025)) %>%
  filter(Team %in% teams)




### Plot map using packages 'ggmap' and 'ggiraph' ###

# With logos

plot <- ggmap(get_map(area, zoom=zoom)) + # Load map of area
  geom_image(aes(x=Lon, y=Lat, image=Image), data=players, size=0.03, alpha=0.8) + # Add logos
  ggplot2::annotate("text", x=center.lon, y=center.lat, col="red", label="@mathieubray", alpha=0.2, cex=30, fontface="bold", angle=30) + # Watermark
  geom_point_interactive(aes(x=Lon, y=Lat, tooltip=Label),size=15, alpha=0.01, data=players) + # Add interactive points underneath logos
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), # Remove axes
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())

ggiraph(code={print(plot)}, width=1, width_svg=16, height_svg=12) # Interactive plot


# Without logos
  
plot <- ggmap(get_map(area, zoom=zoom)) + # Load map of area
  ggplot2::annotate("text", x=center.lon, y=center.lat, col="red", label="@mathieubray", alpha=0.2, cex=30, fontface="bold", angle=30) + # Watermark
  geom_point_interactive(aes(x=Lon, y=Lat, tooltip=Label, fill=Team), pch=21, color="black", size=10, alpha=0.5, data=players) + # Add interactive points
  scale_fill_manual(values=player.colors) + # Change color of points
  guides(fill=guide_legend(override.aes=list(alpha=1))) + # Format legend and remove axes
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(),
        legend.position="bottom", legend.title = element_blank(), legend.text=element_text(size=20))
  

ggiraph(code={print(plot)}, width=1, width_svg=16, height_svg=12) # Interactive plot



### Plot map using package 'leaflet' ###

# With Logos

get.file <- function(team){ # Extract image file
  
  image <- logo.images %>%
    filter(Team == team) %>%
    .$Image %>%
    unique
  
  return(image)
}

logos <- iconList( # Is there a better way to do this?
  
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

leaflet(data = players, options=leafletOptions(worldCopyJump=T)) %>%
  addTiles() %>% # Map
  addControl("<b><p style='font-family:arial; font-size:36px; opacity:0.2; '><a href='https://twitter.com/mathieubray' style='color:red; text-decoration:none; '>@mathieubray</a></p>", 
             position="bottomleft") %>% # Watermark
  addMarkers(~Lon, ~Lat, 
             label=~Label, 
             icon=~logos[Team],
             options = markerOptions(opacity=0.8),
             labelOptions=labelOptions(offset=c(20,0)), # Offset arrow in label
             clusterOptions=markerClusterOptions()) %>% # Group nearby points into clusters
  setView(lng=center.lon, lat=center.lat, zoom=zoom) # Set default view
  

# Without Logos

logo.color <- function(logos){
  
  lapply(logos$Team, function(team) { # Extract color
    
    color <- logos %>%
      filter(Team == team) %>%
      .$Color %>%
      unique
    
    return(color)
  })
  
}

icons <- awesomeIcons( # Assign color to each icon based on team
  
  icon = 'record',
  iconColor = logo.color(players),
  library = 'ion',
  markerColor = 'white'
  
)

leaflet(data=players, options=leafletOptions(worldCopyJump=T)) %>%
  addTiles() %>% # Map
  addControl("<b><p style='font-family:arial; font-size:36px; opacity:0.2; '><a href='https://twitter.com/mathieubray' style='color:red; text-decoration:none; '>@mathieubray</a></p>", 
             position="bottomleft") %>% # Watermark
  addAwesomeMarkers(~Lon, ~Lat, 
                    label = ~as.character(Label),
                    icon = icons, 
                    options = markerOptions(opacity=0.8),
                    clusterOptions=markerClusterOptions()) %>%
  setView(lng=center.lon, lat=center.lat, zoom=zoom)
 
