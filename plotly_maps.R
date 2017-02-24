library(plotly)

####TOTALS HEATMAP####

##Filtering the Data##

trans <- read.csv("transittots_heat.csv")

countrylist <- data.frame(unique(trans$Unique))
names(countrylist) <- c("country")
countrylist <- data.frame(countrylist[!is.na(countrylist$country),])
names(countrylist) <- c("country")

seizloc <- data.frame(table(trans$Seizure.Country, exclude = ""))

orig1 <- data.frame(table(trans$Origin.Country1, exclude = ""))
names(orig1) <- c("country", "o1")
orig2 <- data.frame(table(trans$Origin.Country2, exclude = ""))
names(orig2) <- c("country", "o2")
orig3 <- data.frame(table(trans$Origin.Country3, exclude = ""))
names(orig3) <- c("country", "o3")

trans1 <- data.frame(table(trans$Transit.Country1, exclude = ""))
names(trans1) <- c("country", "t1")
trans2 <- data.frame(table(trans$Transit.Country2, exclude = ""))
names(trans2) <- c("country", "t2")
trans3 <- data.frame(table(trans$Transit.Country3, exclude = ""))
names(trans3) <- c("country", "t3")
trans4 <- data.frame(table(trans$Transit.Country4, exclude = ""))
names(trans4) <- c("country", "t4")

dest1 <- data.frame(table(trans$Destination.Country1, exclude = ""))
names(dest1) <- c("country", "d1")
dest2 <- data.frame(table(trans$Destination.Country2, exclude = ""))
names(dest2) <- c("country", "d2")
dest3 <- data.frame(table(trans$Destination.Country3, exclude = ""))
names(dest3) <- c("country", "d3")

seiz <- merge(countrylist, seizloc, by.x = "country", by.y = "Var1", all = T)
seiz <- data.frame(seiz[!is.na(seiz$country),])
names(seiz) <- c("country", "seizure")

orig <- merge(merge(merge(countrylist, orig1, by = "country", all = T), orig2, by = "country", all = T), orig3, by = "country", all = T)
orig$Origin <- rowSums(orig[,2:4], na.rm = T)
orig <- data.frame(orig[!is.na(orig$country),])
orig <- data.frame(subset(orig, select = c("country", "Origin")))

transit <- merge(merge(merge(merge(countrylist, trans1, by = "country", all = T), trans2, by = "country", all = T), trans3, by = "country", all = T), trans4, by = "country", all = T)
transit$Transit <- rowSums(transit[,2:5], na.rm = T)
transit <- data.frame(subset(transit, select = c("country", "Transit")))

dest <- merge(merge(merge(countrylist, dest1, by = "country", all = T), dest2, by = "country", all = T), dest3, by = "country", all = T)
dest$Destination <- rowSums(dest[,2:4], na.rm = T)
dest <- data.frame(subset(dest, select = c("country", "Destination")))

transitgraph <- merge(merge(orig, transit, by = "country", all = T), dest, by = "country", all = T)
transitgraph[is.na(transitgraph)] <- 0
transitgraph$sum <- rowSums(transitgraph[,2:4])

##Need ISO Codes##
cc <- read.csv("ccref.csv")
hmap <- merge(cc, transitgraph, by = "country", all = T)
hmap[is.na(hmap)] <- 0
hmap$text <- paste("Country:",hmap$country,
                   "</br>Origin Flights:", hmap$Origin,
                   "</br>Transit Flights:", hmap$Transit,
                   "</br>Destination Flights:", hmap$Destination,
                   "</br>Total Trafficking Instances:", hmap$sum)

##Generating the Map##

# light grey boundaries

l <- list(color = toRGB("grey"), width = 0.5)

# specify map projection/options
g <- list(
  showframe = FALSE,
  showcoastlines = FALSE,
  projection = list(type = 'Robinson')
)

p <- plot_geo(hmap) %>%
  add_trace(
    z = ~sum, color = ~sum, colors = 'OrRd',
    text = ~text, locations = ~code, marker = list(line = l)
  ) %>%
  colorbar(title = 'Trafficking Instances') %>%
  layout(
    title = 'Total Wildlife Trafficking Instances in the Air Transit Industry</br>Between 2009 and August 2016',
    geo = g
  )
p

Sys.setenv("plotly_username"="C4ADSdata")
Sys.setenv("plotly_api_key"="jmaBJZcHAqtJwP8H2FGn")

plotly_POST(p, filename = "totalsheatmap")

####IVORY HEATMAP####

trans <- read.csv("transittots_heat.csv")

countrylist <- data.frame(unique(trans$Unique))
names(countrylist) <- c("country")
countrylist <- data.frame(countrylist[!is.na(countrylist$country),])
names(countrylist) <- c("country")

trans <- subset(trans, Database == "Ivory")

seizloc <- data.frame(table(trans$Seizure.Country, exclude = ""))

orig1 <- data.frame(table(trans$Origin.Country1, exclude = ""))
names(orig1) <- c("country", "o1")
orig2 <- data.frame(table(trans$Origin.Country2, exclude = ""))
names(orig2) <- c("country", "o2")
orig3 <- data.frame(table(trans$Origin.Country3, exclude = ""))
names(orig3) <- c("country", "o3")

trans1 <- data.frame(table(trans$Transit.Country1, exclude = ""))
names(trans1) <- c("country", "t1")
trans2 <- data.frame(table(trans$Transit.Country2, exclude = ""))
names(trans2) <- c("country", "t2")
trans3 <- data.frame(table(trans$Transit.Country3, exclude = ""))
names(trans3) <- c("country", "t3")
trans4 <- data.frame(table(trans$Transit.Country4, exclude = ""))
names(trans4) <- c("country", "t4")

dest1 <- data.frame(table(trans$Destination.Country1, exclude = ""))
names(dest1) <- c("country", "d1")
dest2 <- data.frame(table(trans$Destination.Country2, exclude = ""))
names(dest2) <- c("country", "d2")
dest3 <- data.frame(table(trans$Destination.Country3, exclude = ""))
names(dest3) <- c("country", "d3")

seiz <- merge(countrylist, seizloc, by.x = "country", by.y = "Var1", all = T)
seiz <- data.frame(seiz[!is.na(seiz$country),])
names(seiz) <- c("country", "seizure")

orig <- merge(merge(merge(countrylist, orig1, by = "country", all = T), orig2, by = "country", all = T), orig3, by = "country", all = T)
orig$Origin <- rowSums(orig[,2:4], na.rm = T)
orig <- data.frame(orig[!is.na(orig$country),])
orig <- data.frame(subset(orig, select = c("country", "Origin")))

transit <- merge(merge(merge(merge(countrylist, trans1, by = "country", all = T), trans2, by = "country", all = T), trans3, by = "country", all = T), trans4, by = "country", all = T)
transit$Transit <- rowSums(transit[,2:5], na.rm = T)
transit <- data.frame(subset(transit, select = c("country", "Transit")))

dest <- merge(merge(merge(countrylist, dest1, by = "country", all = T), dest2, by = "country", all = T), dest3, by = "country", all = T)
dest$Destination <- rowSums(dest[,2:4], na.rm = T)
dest <- data.frame(subset(dest, select = c("country", "Destination")))

transitgraph <- merge(merge(orig, transit, by = "country", all = T), dest, by = "country", all = T)
transitgraph[is.na(transitgraph)] <- 0
transitgraph$sum <- rowSums(transitgraph[,2:4])

##Need ISO Codes##
cc <- read.csv("ccref.csv")
hmap <- merge(cc, transitgraph, by = "country", all = T)
hmap[is.na(hmap)] <- 0
hmap$text <- paste("Country:",hmap$country,
                   "</br>Origin Flights:", hmap$Origin,
                   "</br>Transit Flights:", hmap$Transit,
                   "</br>Destination Flights:", hmap$Destination,
                   "</br>Total Trafficking Instances:", hmap$sum)

##Generating the Map##

# light grey boundaries

l <- list(color = toRGB("grey"), width = 0.5)

# specify map projection/options
g <- list(
  showframe = FALSE,
  showcoastlines = FALSE,
  projection = list(type = 'Robinson')
)

p <- plot_geo(hmap) %>%
  add_trace(
    z = ~sum, color = ~sum, colors = 'Oranges',
    text = ~text, locations = ~code, marker = list(line = l)
  ) %>%
  colorbar(title = 'Trafficking Instances') %>%
  layout(
    title = 'Total Ivory Trafficking Instances in the Air Transit Industry</br>Between 2009 and August 2016',
    geo = g
  )
p

Sys.setenv("plotly_username"="C4ADSdata")
Sys.setenv("plotly_api_key"="jmaBJZcHAqtJwP8H2FGn")

plotly_POST(p, filename = "ivoryheatmap")


####RHINO HORN HEATMAP####


trans <- read.csv("transittots_heat.csv")

countrylist <- data.frame(unique(trans$Unique))
names(countrylist) <- c("country")
countrylist <- data.frame(countrylist[!is.na(countrylist$country),])
names(countrylist) <- c("country")

trans <- subset(trans, Database == "Rhino Horn")

seizloc <- data.frame(table(trans$Seizure.Country, exclude = ""))

orig1 <- data.frame(table(trans$Origin.Country1, exclude = ""))
names(orig1) <- c("country", "o1")
orig2 <- data.frame(table(trans$Origin.Country2, exclude = ""))
names(orig2) <- c("country", "o2")
orig3 <- data.frame(table(trans$Origin.Country3, exclude = ""))
names(orig3) <- c("country", "o3")

trans1 <- data.frame(table(trans$Transit.Country1, exclude = ""))
names(trans1) <- c("country", "t1")
trans2 <- data.frame(table(trans$Transit.Country2, exclude = ""))
names(trans2) <- c("country", "t2")
trans3 <- data.frame(table(trans$Transit.Country3, exclude = ""))
names(trans3) <- c("country", "t3")
trans4 <- data.frame(table(trans$Transit.Country4, exclude = ""))
names(trans4) <- c("country", "t4")

dest1 <- data.frame(table(trans$Destination.Country1, exclude = ""))
names(dest1) <- c("country", "d1")
dest2 <- data.frame(table(trans$Destination.Country2, exclude = ""))
names(dest2) <- c("country", "d2")
dest3 <- data.frame(table(trans$Destination.Country3, exclude = ""))
names(dest3) <- c("country", "d3")

seiz <- merge(countrylist, seizloc, by.x = "country", by.y = "Var1", all = T)
seiz <- data.frame(seiz[!is.na(seiz$country),])
names(seiz) <- c("country", "seizure")

orig <- merge(merge(merge(countrylist, orig1, by = "country", all = T), orig2, by = "country", all = T), orig3, by = "country", all = T)
orig$Origin <- rowSums(orig[,2:4], na.rm = T)
orig <- data.frame(orig[!is.na(orig$country),])
orig <- data.frame(subset(orig, select = c("country", "Origin")))

transit <- merge(merge(merge(merge(countrylist, trans1, by = "country", all = T), trans2, by = "country", all = T), trans3, by = "country", all = T), trans4, by = "country", all = T)
transit$Transit <- rowSums(transit[,2:5], na.rm = T)
transit <- data.frame(subset(transit, select = c("country", "Transit")))

dest <- merge(merge(merge(countrylist, dest1, by = "country", all = T), dest2, by = "country", all = T), dest3, by = "country", all = T)
dest$Destination <- rowSums(dest[,2:4], na.rm = T)
dest <- data.frame(subset(dest, select = c("country", "Destination")))

transitgraph <- merge(merge(orig, transit, by = "country", all = T), dest, by = "country", all = T)
transitgraph[is.na(transitgraph)] <- 0
transitgraph$sum <- rowSums(transitgraph[,2:4])

##Need ISO Codes##
cc <- read.csv("ccref.csv")
hmap <- merge(cc, transitgraph, by = "country", all = T)
hmap[is.na(hmap)] <- 0
hmap$text <- paste("Country:",hmap$country,
                   "</br>Origin Flights:", hmap$Origin,
                   "</br>Transit Flights:", hmap$Transit,
                   "</br>Destination Flights:", hmap$Destination,
                   "</br>Total Trafficking Instances:", hmap$sum)

##Generating the Map##

# light grey boundaries

l <- list(color = toRGB("grey"), width = 0.5)

# specify map projection/options
g <- list(
  showframe = FALSE,
  showcoastlines = FALSE,
  projection = list(type = 'Robinson')
)

p <- plot_geo(hmap) %>%
  add_trace(
    z = ~sum, color = ~sum, colors = 'Reds',
    text = ~text, locations = ~code, marker = list(line = l)
  ) %>%
  colorbar(title = 'Trafficking Instances') %>%
  layout(
    title = 'Total Rhino Horn Trafficking Instances in the Air Transit Industry</br>Between 2009 and August 2016',
    geo = g
  )
p

Sys.setenv("plotly_username"="C4ADSdata")
Sys.setenv("plotly_api_key"="jmaBJZcHAqtJwP8H2FGn")

plotly_POST(p, filename = "rhheatmap")


####REPTILES HEATMAP####


trans <- read.csv("transittots_heat.csv")

countrylist <- data.frame(unique(trans$Unique))
names(countrylist) <- c("country")
countrylist <- data.frame(countrylist[!is.na(countrylist$country),])
names(countrylist) <- c("country")

trans <- subset(trans, Database == "Reptiles")

seizloc <- data.frame(table(trans$Seizure.Country, exclude = ""))

orig1 <- data.frame(table(trans$Origin.Country1, exclude = ""))
names(orig1) <- c("country", "o1")
orig2 <- data.frame(table(trans$Origin.Country2, exclude = ""))
names(orig2) <- c("country", "o2")
orig3 <- data.frame(table(trans$Origin.Country3, exclude = ""))
names(orig3) <- c("country", "o3")

trans1 <- data.frame(table(trans$Transit.Country1, exclude = ""))
names(trans1) <- c("country", "t1")
trans2 <- data.frame(table(trans$Transit.Country2, exclude = ""))
names(trans2) <- c("country", "t2")
trans3 <- data.frame(table(trans$Transit.Country3, exclude = ""))
names(trans3) <- c("country", "t3")
trans4 <- data.frame(table(trans$Transit.Country4, exclude = ""))
names(trans4) <- c("country", "t4")

dest1 <- data.frame(table(trans$Destination.Country1, exclude = ""))
names(dest1) <- c("country", "d1")
dest2 <- data.frame(table(trans$Destination.Country2, exclude = ""))
names(dest2) <- c("country", "d2")
dest3 <- data.frame(table(trans$Destination.Country3, exclude = ""))
names(dest3) <- c("country", "d3")

seiz <- merge(countrylist, seizloc, by.x = "country", by.y = "Var1", all = T)
seiz <- data.frame(seiz[!is.na(seiz$country),])
names(seiz) <- c("country", "seizure")

orig <- merge(merge(merge(countrylist, orig1, by = "country", all = T), orig2, by = "country", all = T), orig3, by = "country", all = T)
orig$Origin <- rowSums(orig[,2:4], na.rm = T)
orig <- data.frame(orig[!is.na(orig$country),])
orig <- data.frame(subset(orig, select = c("country", "Origin")))

transit <- merge(merge(merge(merge(countrylist, trans1, by = "country", all = T), trans2, by = "country", all = T), trans3, by = "country", all = T), trans4, by = "country", all = T)
transit$Transit <- rowSums(transit[,2:5], na.rm = T)
transit <- data.frame(subset(transit, select = c("country", "Transit")))

dest <- merge(merge(merge(countrylist, dest1, by = "country", all = T), dest2, by = "country", all = T), dest3, by = "country", all = T)
dest$Destination <- rowSums(dest[,2:4], na.rm = T)
dest <- data.frame(subset(dest, select = c("country", "Destination")))

transitgraph <- merge(merge(orig, transit, by = "country", all = T), dest, by = "country", all = T)
transitgraph[is.na(transitgraph)] <- 0
transitgraph$sum <- rowSums(transitgraph[,2:4])

##Need ISO Codes##
cc <- read.csv("ccref.csv")
hmap <- merge(cc, transitgraph, by = "country", all = T)
hmap[is.na(hmap)] <- 0
hmap$text <- paste("Country:",hmap$country,
                   "</br>Origin Flights:", hmap$Origin,
                   "</br>Transit Flights:", hmap$Transit,
                   "</br>Destination Flights:", hmap$Destination,
                   "</br>Total Trafficking Instances:", hmap$sum)

##Generating the Map##

# light grey boundaries

l <- list(color = toRGB("grey"), width = 0.5)

# specify map projection/options
g <- list(
  showframe = FALSE,
  showcoastlines = FALSE,
  projection = list(type = 'Robinson')
)

p <- plot_geo(hmap) %>%
  add_trace(
    z = ~sum, color = ~sum, colors = 'Greens',
    text = ~text, locations = ~code, marker = list(line = l)
  ) %>%
  colorbar(title = 'Trafficking Instances') %>%
  layout(
    title = 'Total Reptile Trafficking Instances in the Air Transit Industry</br>Between 2009 and August 2016',
    geo = g
  )
p

Sys.setenv("plotly_username"="C4ADSdata")
Sys.setenv("plotly_api_key"="jmaBJZcHAqtJwP8H2FGn")

plotly_POST(p, filename = "reptileheatmap")


####BIRDS HEATMAP####


trans <- read.csv("transittots_heat.csv")

countrylist <- data.frame(unique(trans$Unique))
names(countrylist) <- c("country")
countrylist <- data.frame(countrylist[!is.na(countrylist$country),])
names(countrylist) <- c("country")

trans <- subset(trans, Database == "Birds")

seizloc <- data.frame(table(trans$Seizure.Country, exclude = ""))

orig1 <- data.frame(table(trans$Origin.Country1, exclude = ""))
names(orig1) <- c("country", "o1")
orig2 <- data.frame(table(trans$Origin.Country2, exclude = ""))
names(orig2) <- c("country", "o2")
orig3 <- data.frame(table(trans$Origin.Country3, exclude = ""))
names(orig3) <- c("country", "o3")

trans1 <- data.frame(table(trans$Transit.Country1, exclude = ""))
names(trans1) <- c("country", "t1")
trans2 <- data.frame(table(trans$Transit.Country2, exclude = ""))
names(trans2) <- c("country", "t2")
trans3 <- data.frame(table(trans$Transit.Country3, exclude = ""))
names(trans3) <- c("country", "t3")
trans4 <- data.frame(table(trans$Transit.Country4, exclude = ""))
names(trans4) <- c("country", "t4")

dest1 <- data.frame(table(trans$Destination.Country1, exclude = ""))
names(dest1) <- c("country", "d1")
dest2 <- data.frame(table(trans$Destination.Country2, exclude = ""))
names(dest2) <- c("country", "d2")
dest3 <- data.frame(table(trans$Destination.Country3, exclude = ""))
names(dest3) <- c("country", "d3")

seiz <- merge(countrylist, seizloc, by.x = "country", by.y = "Var1", all = T)
seiz <- data.frame(seiz[!is.na(seiz$country),])
names(seiz) <- c("country", "seizure")

orig <- merge(merge(merge(countrylist, orig1, by = "country", all = T), orig2, by = "country", all = T), orig3, by = "country", all = T)
orig$Origin <- rowSums(orig[,2:4], na.rm = T)
orig <- data.frame(orig[!is.na(orig$country),])
orig <- data.frame(subset(orig, select = c("country", "Origin")))

transit <- merge(merge(merge(merge(countrylist, trans1, by = "country", all = T), trans2, by = "country", all = T), trans3, by = "country", all = T), trans4, by = "country", all = T)
transit$Transit <- rowSums(transit[,2:5], na.rm = T)
transit <- data.frame(subset(transit, select = c("country", "Transit")))

dest <- merge(merge(merge(countrylist, dest1, by = "country", all = T), dest2, by = "country", all = T), dest3, by = "country", all = T)
dest$Destination <- rowSums(dest[,2:4], na.rm = T)
dest <- data.frame(subset(dest, select = c("country", "Destination")))

transitgraph <- merge(merge(orig, transit, by = "country", all = T), dest, by = "country", all = T)
transitgraph[is.na(transitgraph)] <- 0
transitgraph$sum <- rowSums(transitgraph[,2:4])

##Need ISO Codes##
cc <- read.csv("ccref.csv")
hmap <- merge(cc, transitgraph, by = "country", all = T)
hmap[is.na(hmap)] <- 0
hmap$text <- paste("Country:",hmap$country,
                   "</br>Origin Flights:", hmap$Origin,
                   "</br>Transit Flights:", hmap$Transit,
                   "</br>Destination Flights:", hmap$Destination,
                   "</br>Total Trafficking Instances:", hmap$sum)

##Generating the Map##

# light grey boundaries

l <- list(color = toRGB("grey"), width = 0.5)

# specify map projection/options
g <- list(
  showframe = FALSE,
  showcoastlines = FALSE,
  projection = list(type = 'Robinson')
)

p <- plot_geo(hmap) %>%
  add_trace(
    z = ~sum, color = ~sum, colors = 'Blues',
    text = ~text, locations = ~code, marker = list(line = l)
  ) %>%
  colorbar(title = 'Trafficking Instances') %>%
  layout(
    title = 'Total Bird Trafficking Instances in the Air Transit Industry</br>Between 2009 and August 2016',
    geo = g
  )
p

Sys.setenv("plotly_username"="C4ADSdata")
Sys.setenv("plotly_api_key"="jmaBJZcHAqtJwP8H2FGn")

plotly_POST(p, filename = "birdheatmap")