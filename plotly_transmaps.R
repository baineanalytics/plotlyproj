##Stuff Needed for all five maps##

library(dplyr)
library(plotly)

g <- list(
  showframe = FALSE,
  showcoastlines = FALSE,
  showland = TRUE,
  showcountries = TRUE,
  showocean = TRUE,
  countrywidth = 0.2,
  projection = list(type = 'Robinson'),
  landcolor = toRGB("gray75"),
  countrycolor = toRGB("black"),
  lakecolor = toRGB("gray92"),
  oceancolor = toRGB("gray92")
)


####TOTALS####


tots <- read.csv("tots_map.csv")

flight <- tots[,1:8]
flight$id <- seq_len(nrow(flight))
flight$text <- paste("Route: ", flight$route, ", Instances: ", flight$sum, sep = "")

cities <- tots[10:15]
cities <- cities[1:229,]

cities$text <- paste("City:", cities$Unique,
                     "</br>Origin of Trafficking Flights:", cities$city1.sum,
                     "</br>Destination of Trafficking Flights:", cities$city2.sum,
                     "</br>Total Trafficking Instances:", cities$total.sum)

p <- plot_geo(flight) %>%
  add_markers(
    data = cities, x = ~long, y = ~lat, text = ~text,
    size = ~total.sum, hoverinfo = "text", alpha = .85, color = I("Purple4")
  ) %>%
  add_segments(
    data = group_by(flight,id),
    x = ~c1long, xend = ~c2long,
    y = ~c1lat, yend = ~c2lat,
    alpha = .35, size = I(1.5), split = ~id,
    hoverinfo = "none", color = I("Purple4")
  ) %>%
  layout(
    title = "Flight Paths for Wildlife Traffickers",
    geo = g, showlegend = FALSE
  )

p

Sys.setenv("plotly_username"="C4ADSdata")
Sys.setenv("plotly_api_key"="jmaBJZcHAqtJwP8H2FGn")

plotly_POST(p, filename = "totalstransmap")


####IVORY####


tots <- read.csv("ivory_map.csv")

flight <- tots[,1:8]
flight$id <- seq_len(nrow(flight))
flight$text <- paste("Route: ", flight$route, ", Instances: ", flight$count, sep = "")

cities <- tots[10:15]
cities <- cities[1:85,]

cities$text <- paste("City:", cities$Unique,
                     "</br>Origin of Trafficking Flights:", cities$suma,
                     "</br>Destination of Trafficking Flights:", cities$sumb,
                     "</br>Total Trafficking Instances:", cities$sum)

p <- plot_geo(flight) %>%
  add_markers(
    data = cities, x = ~ulong, y = ~ulat, text = ~text,
    size = ~sum, hoverinfo = "text", alpha = .85, color = I("darkorange3")
  ) %>%
  add_segments(
    data = group_by(flight,id),
    x = ~c1long, xend = ~c2long,
    y = ~c1lat, yend = ~c2lat,
    alpha = .35, size = I(1.5), split = ~id,
    hoverinfo = "none", color = I("darkorange")
  ) %>%
  layout(
    title = "Flight Paths for Ivory Traffickers",
    geo = g, showlegend = FALSE
)

p

Sys.setenv("plotly_username"="C4ADSdata")
Sys.setenv("plotly_api_key"="jmaBJZcHAqtJwP8H2FGn")

plotly_POST(p, filename = "ivorytransmap")


####RHINO HORN####


tots <- read.csv("rh_map.csv")

flight <- tots[,1:8]
flight$id <- seq_len(nrow(flight))
flight$text <- paste("Route: ", flight$route, ", Instances: ", flight$count, sep = "")

cities <- tots[9:14]
cities <- cities[1:43,]
cities$text <- paste("City:", cities$Unique,
                     "</br>Origin of Trafficking Flights:", cities$counta,
                     "</br>Destination of Trafficking Flights:", cities$countb,
                     "</br>Total Trafficking Instances:", cities$sum)

p <- plot_geo(flight) %>%
  add_markers(
    data = cities, x = ~ulong, y = ~ulat, text = ~text,
    size = ~sum, hoverinfo = "text", alpha = .85, color = I("red4")
  ) %>%
  add_segments(
    data = group_by(flight,id),
    x = ~c1long, xend = ~c2long,
    y = ~c1lat, yend = ~c2lat,
    alpha = .35, size = I(1.5), split = ~id,
    hoverinfo = "none", color = I("red")
  ) %>%
  layout(
    title = "Flight Paths for Rhino Horn Traffickers",
    geo = g, showlegend = FALSE, autosize = T
  )

p

Sys.setenv("plotly_username"="C4ADSdata")
Sys.setenv("plotly_api_key"="jmaBJZcHAqtJwP8H2FGn")

plotly_POST(p, filename = "rhtransmap")


####REPTILES####


tots <- read.csv("rep_map.csv")

flight <- tots[,1:8]
flight$id <- seq_len(nrow(flight))
flight$text <- paste("Route: ", flight$route, ", Instances: ", flight$count, sep = "")

cities <- tots[10:15]
cities <- cities[1:119,]
cities$text <- paste("City:", cities$Unique,
                     "</br>Origin of Trafficking Flights:", cities$suma,
                     "</br>Destination of Trafficking Flights:", cities$sumb,
                     "</br>Total Trafficking Instances:", cities$sum)

p <- plot_geo(flight) %>%
  add_markers(
    data = cities, x = ~ulong, y = ~ulat, text = ~text,
    size = ~sum, hoverinfo = "text", alpha = .85, color = I("darkgreen")
  ) %>%
  add_segments(
    data = group_by(flight,id),
    x = ~c1long, xend = ~c2long,
    y = ~c1lat, yend = ~c2lat,
    alpha = .35, size = I(1.5), split = ~id,
    hoverinfo = "none", color = I("green4")
  ) %>%
  layout(
    title = "Flight Paths for Reptile Traffickers",
    geo = g, showlegend = FALSE
  )

p

Sys.setenv("plotly_username"="C4ADSdata")
Sys.setenv("plotly_api_key"="jmaBJZcHAqtJwP8H2FGn")

plotly_POST(p, filename = "reptransmap")


####BIRDS####


tots <- read.csv("bird_map.csv")

flight <- tots[,1:8]
flight$id <- seq_len(nrow(flight))
flight$text <- paste("Route: ", flight$route, ", Instances: ", flight$count, sep = "")

cities <- tots[10:15]
cities <- cities[1:106,]
cities$text <- paste("City:", cities$Unique,
                     "</br>Origin of Trafficking Flights:", cities$counta,
                     "</br>Destination of Trafficking Flights:", cities$countb,
                     "</br>Total Trafficking Instances:", cities$sum)

p <- plot_geo(flight) %>%
  add_markers(
    data = cities, x = ~ulong, y = ~ulat, text = ~text,
    size = ~sum, hoverinfo = "text", alpha = .85, color = I("royalblue")
  ) %>%
  add_segments(
    data = group_by(flight,id),
    x = ~c1long, xend = ~c2long,
    y = ~c1lat, yend = ~c2lat,
    alpha = .35, size = I(1.5), split = ~id,
    hoverinfo = "none", color = I("steelblue3")
  ) %>%
  layout(
    title = "Flight Paths for Bird Traffickers",
    geo = g, showlegend = FALSE
  )

p

Sys.setenv("plotly_username"="C4ADSdata")
Sys.setenv("plotly_api_key"="jmaBJZcHAqtJwP8H2FGn")

plotly_POST(p, filename = "birdtransmap")