#Flight Travel plot through GGPLOT2
setwd('C:\\Asutosh\\Building-something\\Flight_travel_ggplot')

#install packages
#install package for ggmap plot visualize spatial data and models on top of static maps
install.packages("ggmap")
#The ggrepel package helps to prevent overplotting of text.
install.packages("ggrepel")

# Read flight list
flights <- read.csv("flights.csv", stringsAsFactors = FALSE)

# Lookup coordinates
library(ggplot2)
library(ggmap)

#check the data format of the dataset
str(flights)

#check the total number of rows in the dataset
nrow(flights)

#check the number of NA values in the dataset
sum(is.na(flights))

#check which column has how many NA values
sapply(flights, function(x) sum(is.na(x)))
## only Frequency column has NA values

#pct of NA values
25/103
# 24% NA values

#findout unique airports
airports <- unique(c(flights$From, flights$To))
airports
#get the geocodes for the airports # it fetches the geocodes from maps.googleapis.com/maps/api/geocode
coords <- geocode(airports)
#check th top 5 rows
head(coords)
## longitute and latitude values are fetched for the coordinates

#now form a airport data frame with the coordinates of the airports
airports <- data.frame(airport=airports, coords)

#check the top 5 records
head(airports)

# now ready to create a travel route map

#The data frames are merged so that we can create air travel route maps using the curve geom. 
#The borders function of ggplot2 creates the map data.
#The ggrepel package helps to prevent overplotting of text.

# Add coordinates to flight list
flights <- merge(flights, airports, by.x="To", by.y="airport")
flights <- merge(flights, airports, by.x="From", by.y="airport")

#check the data frame
head(flights)

# Plot flight routes
library(ggrepel)

worldmap <- borders("world", colour="#efede1", fill="#efede1") # create a layer of borders
ggplot() + worldmap + 
  geom_curve(data=flights, aes(x = lon.x, y = lat.x, xend = lon.y, yend = lat.y), col = "#b29e7d", size = 1, curvature = .2) + 
  geom_point(data=airports, aes(x = lon, y = lat), col = "#970027") + 
  geom_text_repel(data=airports, aes(x = lon, y = lat, label = airport), col = "black", size = 2, segment.color = NA) + 
  theme(panel.background = element_rect(fill="white"), 
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()
  )

# Alternative visualisation
## Download and install the package
install.packages("igraph")

library(igraph)

# Network visualisation
library(igraph)

# This visualisation shows the logic between the locations 
#and not their spatial locations.

edgelist <- as.matrix(flights[c("From", "To")])
#edgelist
g <- graph_from_edgelist(edgelist, directed = TRUE)
g <- simplify(g)
par(mar=rep(0,4))
plot.igraph(g, 
            edge.arrow.size=0,
            edge.color="black",
            edge.curved=TRUE,
            edge.width=2,
            vertex.size=3,
            vertex.color=NA, 
            vertex.frame.color=NA, 
            vertex.label=V(g)$name,
            vertex.label.cex=3,
            layout=layout.fruchterman.reingold
)

#igraph code needs enhancement for a legible plot