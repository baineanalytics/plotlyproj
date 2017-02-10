library(plotly)
library(extrafont)
library(reshape2)
library(dplyr)

Sys.setenv("plotly_username"="C4ADSdata")
Sys.setenv("plotly_api_key"="jmaBJZcHAqtJwP8H2FGn")


#Totals heatmap########### NOT WORKING ATM

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


transitgraph <- data.frame(subset(transitgraph, sum > 0))

library(maps)
library(RColorBrewer)
library(ggalt)

world_map <- map_data("world")
world_map <- subset(world_map, region!="Antarctica")

heat <- merge(transitgraph, world_map, by.x = "country", by.y = "region", all = T)
heat <- arrange(heat, group, order)

theme_clean <- function(base_size = 12) {
  require(grid)
  theme_grey(base_size) %+replace%
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.background = element_blank(),
      panel.grid = element_blank(),
      panel.margin = unit(0, "lines"),
      plot.margin = unit(c(0,0,0,0),"lines"),
      complete = TRUE)
}

gg <- ggplot(data = heat, aes(x = long, y = lat, group = group, fill = sum)) + 
  geom_map(dat = world_map, map = world_map, aes(map_id = region), color = "gray20", fill = "white", lwd = .05) + 
  theme_clean() +
  coord_proj("+proj=robin")

gg <- gg + geom_map(map = world_map, aes(map_id = country, fill = sum), color = "gray20", lwd = .05) + 
  scale_fill_distiller(palette = "YlOrRd", na.value = "gray87", direction = 1) + 
  theme(legend.position = c(0.5, 0.12),
        legend.direction = "horizontal",
        legend.background = element_rect(color = "grey20"),
        legend.title = element_blank(),
        legend.text = element_text(family = "Gill Sans MT"),
        legend.key.height = unit(.08,"in"),
        legend.key.width = unit(.25,"in"),
        panel.background = element_rect(size = 2, color = "black"))
gg
























xlim <- c(-170,170)
ylim <- c(-170,170)

gg <- ggplot(data = heat, aes(x = long, y = lat, group = group, fill = sum)) + 
  geom_map(dat = world_map, map = world_map, aes(map_id = region), color = "gray20", fill = "white", lwd = .05) + 
  theme_clean() 

gg <- gg + geom_map(map = world_map, aes(map_id = country, fill = sum, group = group), color = "gray20", lwd = .05) + 
  scale_fill_distiller(palette = "YlOrRd", na.value = "gray87", direction = 1) + coord_map(xlim=xlim,ylim=ylim) +
  theme(legend.position = c(0.5, 0.12),
        legend.direction = "horizontal",
        legend.background = element_rect(color = "grey20"),
        legend.title = element_blank(),
        legend.text = element_text(family = "Gill Sans MT"),
        legend.key.height = unit(.08,"in"),
        legend.key.width = unit(.25,"in"),
        panel.background = element_rect(size = 2, color = "black"))

gg 


#SEIZURE TIMELINE#

tot <- read.csv("tot.csv")

ivory <- data.frame(subset(tot, Database == "Ivory"))

ivyrs <- data.frame(table(ivory$Year))
names(ivyrs) <- c("year", "Ivory")

rhino <- data.frame(subset(tot, Database == "Rhino Horn"))

ryrs <- data.frame(table(rhino$Year))
names(ryrs) <- c("year", "Rhino Horn")

rep <- data.frame(subset(tot, Database == "Reptiles"))

repyrs <- data.frame(table(rep$Year))
names(repyrs) <- c("year", "Reptiles")

bird <- data.frame(subset(tot, Database == "Birds"))

byrs <- data.frame(table(bird$Year))
names(byrs) <- c("year", "Birds")

timeline <- merge(merge(merge(byrs, repyrs, by = "year"), ryrs, by = "year"), ivyrs, by = "year")
timelinemelt <- melt(timeline)
timelinemelt$variable <- factor(timelinemelt$variable, levels = c("Ivory", "Rhino Horn", "Reptiles", "Birds"))
colnames(timelinemelt) <- c("Year", "Database", "Seizure.Count")
timelinemelt$text <- paste("Year:", timelinemelt$Year,
                           "</br>Database:", timelinemelt$Database,
                           "</br>Seizure Count:", timelinemelt$Seizure.Count)

gg <- ggplot(data = timelinemelt, aes(x = Year, y = Seizure.Count, group = Database, color = Database, text = text)) + 
  geom_line(lwd = 1.5) + 
  scale_color_manual(values = c('darkorange', 'red', 'green4', 'steelblue3')) +
  geom_point(aes(y=Seizure.Count), color = "Black", size = 2) + 
  theme_bw() +
  labs(x = "Year", y = "Seizure Count", group = "Database") +
  theme(panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(family = "Gill Sans MT", size = 15),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = .5),
        legend.title = element_blank(),
        legend.text = element_text(family = "Gill Sans MT", size = 15))

p <- ggplotly(gg, tooltip = "text")

plotly_POST(p, filename = "totaldbseizures_timeline")

#Total seizures timeline#

timeline$sum <- rowSums(timeline[,2:5])
timeline$text2 <- paste("Year:",timeline$year,
                        "</br>Total Seizures:",timeline$sum)

gg <- ggplot(timeline, aes(x = year, y = sum, text=text2)) + 
  geom_line(group = 1, lwd = 1.5, color = "maroon") + 
  geom_point(aes(y=sum), color = "Black", size = 2) + 
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(family = "Gill Sans MT", size = 12),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = .75),
        legend.title = element_blank(),
        legend.text = element_text(family = "Gill Sans MT", size = 12))

p <- ggplotly(gg, tooltip = "text2")
plotly_POST(p, filename = "totalseizures_timeline")


#COUNTRY SEIZURES#

trans <- read.csv("transittots.csv")
trans[trans == ""] <- NA

count <- trans %>% select(ID, Date, Database, Seizure.Country, Origin.Country1, Origin.Country2, Origin.Country3, Transit.Country1, Transit.Country2, Transit.Country3, Transit.Country4, Destination.Country1, Destination.Country2, Destination.Country3)
count <- subset(count[,4:14])
uni <- NULL

for (j in 1:length(count)){
  nat <- count[j]
  natu <- unique(nat)
  names(natu) <- c("country")
  uni <- rbind(natu, uni)
}

countries <- unique(uni)
countries <- data.frame(countries[!is.na(countries$country),])
names(countries) <- c("country")

for (i in 1:length(count)){
  data <- count[i]
  name <- names(data)
  data <- data.frame(table(data, exclude = c(NA, "")))
  names(data) <- c("country", name)
  countries <- merge(countries, data, by = "country", all=T)
}

countries$Origin <- rowSums(countries[,3:5], na.rm = T)
countries$Transit <- rowSums(countries[,6:9], na.rm = T)
countries$Destination <- rowSums(countries[10:12], na.rm = T)
countries$sums <- rowSums(countries[13:15], na.rm = T)

countries[is.na(countries)] <- 0

seizures <- countries %>% dplyr::select(country, Seizure.Country)
names(seizures) <- c("Country", "Seizures")
lseiz <- subset(seizures, Seizures>10)
lseiz$text <- paste("Country:", lseiz$Country,
                    "</br>Seizures:", lseiz$Seizures)

gg <- ggplot(data = lseiz, aes(x = reorder(Country, Seizures), y = Seizures, text=text)) + 
  geom_bar(stat = "identity", color = "black", fill = "maroon", width = .85, size = .4) + 
  theme_bw() + coord_flip() +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major.y = element_blank(), 
        panel.grid.major.x = element_line(size = .75), 
        axis.title = element_blank(), 
        axis.text = element_text(family = "Gill Sans MT", size = 12),
        legend.title = element_blank(),
        legend.text = element_text(family = "Gill Sans MT", size = 12))

p <- ggplotly(gg, tooltip = "text")

plotly_POST(p, filename = "countryseizures")


##Country Enforcement Index## >20 Instances


full <- read.csv("transittots_ratio.csv", header = T)
subset <- subset(full[1:773,])

oc1 <- dplyr :: count(subset, Origin.Country1.1)
names(oc1) <- c("country", "n1")
oc2 <- dplyr :: count(subset, Origin.Country2.1)
names(oc2) <- c("country", "n2")
oc3 <- dplyr :: count(subset, Origin.Country3.1)
names(oc3) <- c("country", "n3")

origin <- merge(merge(x = oc1, y = oc2, by = "country", all = T), oc3, by = "country", all = T)
origin <- origin[origin$country!="SEIZURE" & origin$country!="" & origin$country!=0 & origin$country!="#N/A",]
origin$origin <- rowSums(origin[,2:4], na.rm = T)
origin <- data.frame(subset(origin, select = c("country", "origin")))

tc1 <- dplyr :: count(subset, Transit.Country1.1)
names(tc1) <- c("country", "n1")
tc2 <- dplyr :: count(subset, Transit.Country2.1)
names(tc2) <- c("country", "n2")
tc3 <- dplyr :: count(subset, Transit.Country3.1)
names(tc3) <- c("country", "n3")
tc4 <- dplyr :: count(subset, Transit.Country4.1)
names(tc4) <- c("country", "n4")

transit <- merge(merge(merge(tc1, tc2, by = "country", all = T), tc3, by = "country", all = T), tc4, by = "country", all = T)
transit <- transit[transit$country!="SEIZURE" & transit$country!="" & transit$country!=0 & transit$country!="#N/A",]
transit$transit <- rowSums(transit[,2:5], na.rm = T)
transit <- data.frame(subset(transit, select = c("country", "transit")))

dc1 <- dplyr :: count(subset, Destination.Country1.1)
names(dc1) <- c("country", "n1")
dc2 <- dplyr :: count(subset, Destination.Country2.1)
names(dc2) <- c("country", "n2")
dc3 <- dplyr :: count(subset, Destination.Country3.1)
names(dc3) <- c("country", "n3")

dest <- merge(merge(x = dc1, y = dc2, by = "country", all = T), dc3, by = "country", all = T)
dest <- dest[dest$country!="SEIZURE" & dest$country!="" & dest$country!=0 & dest$country!="#N/A",]
dest$destination <- rowSums(dest[,2:4], na.rm = T)
dest <- data.frame(subset(dest, select = c("country", "destination")))

seizures <- data.frame(table(subset$Seizure.Country.1))
names(seizures) <- c("country", "seizures")
seizures <- seizures[seizures$country!="" & seizures$country!=0 & seizures$country!="#N/A",]
seizures[is.na(seizures),] <- 0

ratio <- merge(merge(merge(origin, transit, by = "country", all = T), dest, by = "country", all = T), seizures, by= "country", all = T)
ratio[is.na(ratio)] <- 0
ratio$tsums <- rowSums(ratio[,2:4], na.rm = T)
ratio$ratio <- ratio$seizures/ratio$tsums

ratiograph <- data.frame(subset(ratio, tsums > 20))
ratiograph$labels <- paste(round(ratiograph$ratio*100, 1), "%", sep = "")
ratiograph$text <- paste("Country:",ratiograph$country,
                         "</br>Seizures:", ratiograph$seizures,
                         "</br>Total Trafficking Instances:",ratiograph$tsums,
                         "</br>Country Enforcement Indicator Score:", ratiograph$labels)

gg<- ggplot(data = ratiograph, aes(x = reorder(country, ratio), y = ratio, text = text)) +
  geom_bar(stat = "identity", width = .8, color = "black", fill = "maroon") +
  scale_y_continuous(limits = c(0, 1), breaks = c(0,.25,.50,.75,1), labels = c("0%", "25%", "50%", "75%", "100%")) +
  coord_flip() +
  theme_bw() +
  theme(axis.title = element_blank(),
        axis.text = element_text(family = "Gill Sans MT", size = 15),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(size=.5))

p <- ggplotly(gg, tooltip = "text")
plotly_POST(p, filename = "Trends and Totals/countryenforcementindex")



###########################################IVORY###########################################


##Weight Seized##

tot <- read.csv("tot.csv")

ivory <- data.frame(subset(tot, Database == "Ivory"))
ivory <- transform(ivory, Weight..kg. = as.numeric(as.character(Weight..kg.)))

yrs <- data.frame(table(ivory$Year))
names(yrs) <- c("year", "seizures")

iv100 <- subset(ivory, Weight..kg. > 100)
yrs100 <- data.frame(table(iv100$Year))
names(yrs100) <- c("year", "seizures100")

iv500 <- subset(ivory, Weight..kg. > 500)
yrs500 <- data.frame(table(iv500$Year))
names(yrs500) <- c("year", "seizures500")

require(plyr)
ni <- ddply(ivory, .(Year), summarize, sumweight = sum(Weight..kg., na.rm = TRUE))
names(ni) <- c("year", "ivory")
ni$text <- paste("Year:",ni$year,
                 "</br>Ivory Weight:", ni$ivory,"kg")

timeline <- merge(merge(yrs, yrs100, by = "year"),yrs500, by= "year")
colnames(timeline) <- c("Year", "Total Seizures", "Seizures > 100 kg", "Seizures > 500 kg")
linegraph <- melt(timeline, id="Year")
linegraph$text <- paste("Year:",linegraph$Year,
                        "</br>Ivory Seizures:", linegraph$value)

gg <- ggplot(ni, aes(x = year, text = text)) + 
  geom_bar(aes(y=ivory), stat = "identity", color = "black", fill = "darkorange", width = .85, size = .4) +
  theme_bw() +
  scale_y_continuous(breaks = c(0,1000,2000,3000,4000,5000)) +
  scale_x_continuous(breaks = c(2009:2016)) +
  theme(panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(family = "Gill Sans MT", size = 12),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = .75))

p <- ggplotly(gg, tooltip = "text")

plotly_POST(p, filename = "ivweighttimeline")

## Total ivory seizures over time##

cols <- c("Total Seizures" = "#FFD6A7", "Seizures > 100 kg" = "#F98C0E", "Seizures > 500 kg" = "#4B2800")

gg <- ggplot(linegraph, aes(x = Year, text = text)) + 
  geom_line(aes(y=value, group=variable, color = variable), size = 1.5) + 
  geom_point(aes(y=value), color = "Black", size = 2) + 
  theme_bw() +
  scale_colour_manual(name="Seizures",values=cols,labels=c("All Seizures", "Seizures > 100kgs", "Seizures > 500kgs")) +
  theme(panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(family = "Gill Sans MT", size = 11),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = .75),
        legend.text = element_text(family = "Gill Sans MT", size = 11),
        legend.title = element_blank())

p <- ggplotly(gg, tooltip = "text")
plotly_POST(p, filename = "ivseizuretimeline")

##Ivory Average Weight Per Seizure##

nonum <- subset(ivory, select = c("Weight..kg.", "Year"), na.strings=c("","NA"))

require(dplyr)

empty_as_na <- function(x){
  if("factor" %in% class(x)) x <- as.character(x) ## since ifelse wont work with factors
  ifelse(as.character(x)!="", x, NA)
}

nonum <- nonum %>% mutate_each(funs(empty_as_na)) 
nonum <- nonum[complete.cases(nonum),]

sums <- ddply(nonum, .(Year), summarize, sums = sum(Weight..kg., na.rm = TRUE))

years <- data.frame(table(nonum$Year))
names(years) <- c("Year", "count")

ivavg <- merge(sums, years, by = "Year")
ivavg$avg <- ivavg$sums/ivavg$count
ivavg$text <- paste("Year:", ivavg$Year,
                    "</br>Average Weight Per Seizure:", round(ivavg$avg,1),"kg")

gg <- ggplot(ivavg, aes(x = Year, text=text)) + 
  geom_line(aes(y=avg), group = 1, color = "darkorange", size = 1.5) + 
  geom_point(aes(y=avg), color = "black", size = 2) + 
  theme_bw() +  
  scale_y_continuous(breaks = c(100,200,300,400,500,600)) +
  scale_x_continuous(breaks = c(2009:2016)) +
  theme(panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(family = "Gill Sans MT", size = 12),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = .75))

p <- ggplotly(gg, tooltip = "text")
plotly_POST(p, filename = "ivavgweighttimeline")





###########################################RHINO HORN###########################################

##Number of horns seized##

tot <- read.csv("tot.csv")

tot$Weight..kg. <- as.numeric(as.character(tot$Weight..kg.))

rhino <- data.frame(subset(tot, Database == "Rhino Horn"))

yrs <- data.frame(table(rhino$Year))
names(yrs) <- c("year", "seizures")

yrs5 <- subset(rhino, Number > 5)
yrs5 <- data.frame(table(yrs5$Year))
names(yrs5) <- c("year", "horns5")

yrs10 <- subset(rhino, Number > 10)
yrs10 <- data.frame(table(yrs10$Year))
names(yrs10) <- c("year", "horns10")

horns <- ddply(rhino, .(Year), summarize, sumhorn = sum(Number, na.rm = TRUE))
names(horns) <- c("year", "horns")
horns$text <- paste("Year:", horns$year,
                    "</br>Number of Horns Seized:", horns$horns)

gg <- ggplot(horns, aes(x = year, text=text)) + 
  geom_bar(aes(y=horns), stat = "identity", color = "black", fill = "red", width = .85, size = .4) +
  theme_bw() +
  scale_x_continuous(breaks = c(2008:2016)) +
  theme(panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(family = "Gill Sans MT", size = 12),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = .75))
p <- ggplotly(gg, tooltip = "text")
plotly_POST(p, filename = "rhhorntimeline")

##RH Seizure Timeline##

cols <- c("All Seizures" = "#FFA7A7", "Seizures > 5 Horns" = "#F90E0E", "Seizures > 10 Horns" = "#4B0000")

linegraph <- merge(merge(yrs, yrs5, by = "year", all=T), yrs10, by="year", all=T)
linegraph[is.na(linegraph)] <- 0
colnames(linegraph) <- c("year","All Seizures","Seizures > 5 Horns","Seizures > 10 Horns")
linegraph <- melt(linegraph, id = "year")
linegraph$text <- paste("Year:",linegraph$year,
                        "</br>Seizure Count:",linegraph$value)

gg <- ggplot(linegraph, aes(x = year, text=text)) + 
  geom_line(aes(y=value,group=variable,color=variable), size = 1.5) + 
  geom_point(aes(y=value), color = "Black", size = 2) + 
  theme_bw() +
  scale_colour_manual(name="Seizures", values=cols, labels=c("All Seizures", "Seizures > 5 Horns", "Seizures > 10 Horns")) +
  theme(panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(family = "Gill Sans MT", size = 11), 
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size=.75),
        legend.text = element_text(family = "Gill Sans MT", size = 11),
        legend.title = element_blank())
p <- ggplotly(gg, tooltip = "text")
plotly_POST(p, filename = "rhseizuretimeline")

##RH Average Horns##

nonum <- subset(rhino, select = c("Number", "Year"))
nonum <- nonum[complete.cases(nonum),]
sumhorn <- ddply(nonum, .(Year), summarize, sumhorn = sum(Number, na.rm = TRUE))
years <- data.frame(table(nonum$Year))
names(years) <- c("Year", "count")
hornavg <- merge(sumhorn, years, by = "Year")
hornavg$avg <- hornavg$sumhorn/hornavg$count
hornavg$text <- paste("Year:", hornavg$Year,
                      "</br>Average Horns Per Seizure:", round(hornavg$avg,1))


gg <- ggplot(hornavg, aes(x = Year, text = text)) + geom_line(aes(y=avg), group = 1, color = "red", size = 1.5) + 
  geom_point(aes(y=avg), color = "Black", size = 2) + 
  theme_bw() +
  scale_x_continuous(breaks = c(2009:2016)) +
  theme(panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(family = "Gill Sans MT", size = 12), 
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size=.75))

p <- ggplotly(gg, tooltip = "text")
plotly_POST(p, filename = "rhavghorntimeline")



###########################################REPTILES###########################################



tot <- read.csv("tot.csv")

rep <- data.frame(subset(tot, Database == "Reptiles"))

yrs <- data.frame(table(rep$Year))
names(yrs) <- c("year", "seizures")

rep100 <- subset(rep, Number > 100)
rep100 <- data.frame(table(rep100$Year))
names(rep100) <- c("year", "rep100")

rep1000 <- subset(rep, Number > 1000)
rep1000 <- data.frame(table(rep1000$Year))
names(rep1000) <- c("year", "rep1000")

require(plyr)
nr <- ddply(rep, .(Year), summarize, sumhorn = sum(Number, na.rm = TRUE))
names(nr) <- c("year", "reptiles")
nr$text <- paste("Year:", nr$year,
                 "</br>Reptiles Seized:", nr$reptiles)

gg <- ggplot(nr, aes(x = year, text = text)) + 
  geom_bar(aes(y=reptiles), stat = "identity", color = "black", fill = "green4", width = .85, size = .4) +
  theme_bw() +
  scale_x_continuous(breaks = c(2009:2016)) +
  theme(panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(family = "Gill Sans MT", size = 12),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = .75))

p <- ggplotly(gg, tooltip = "text")
plotly_POST(p, filename = "repnumbertimeline")

timeline <- merge(merge(yrs, rep100, by="year", all=T),rep1000,by="year",all=T)
timeline[is.na(timeline)] <- 0
colnames(timeline) <- c("year","All Seizures","Seizures > 100 Reptiles","Seizures > 500 Reptiles")

timemelt <- melt(timeline, id="year")
timemelt$text <- paste("Year:",timemelt$year,
                       "</br>Seizure Count:",timemelt$value)

cols <- c("All Seizures" = "#9DEF9D", "Seizures > 100 Reptiles" = "#0BC80B", "Seizures > 500 Reptiles" = "#003C00")

gg <- ggplot(data = timemelt, aes(x=year,y=value,text=text)) + 
  geom_line(aes(group=variable,color=variable), size = 1.5) + 
  geom_point(aes(y=value), color = "Black", size = 2) + 
  theme_bw() +
  scale_colour_manual(name="Seizures", values=cols, labels=c("All Seizures", "Seizures > 100 Reptiles", "Seizures > 1000 Reptiles")) +
  theme(panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(family = "Gill Sans MT", size = 11),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size=.5),
        legend.text = element_text(family = "Gill Sans MT", size = 11),
        legend.title = element_blank())

p <- ggplotly(gg, tooltip = "text")
plotly_POST(p, filename = "repseizuretimeline")

##Average Reptiles Per Seizure Timeline##

nonum <- subset(rep, select = c("Number", "Year"))
nonum <- nonum[complete.cases(nonum),]
sums <- ddply(nonum, .(Year), summarize, sums = sum(Number, na.rm = TRUE))
years <- data.frame(table(nonum$Year))
names(years) <- c("Year", "count")
avg <- merge(sums, years, by = "Year")
avg$avg <- avg$sums/avg$count
avg$text <- paste("Year:",avg$Year,
                  "</br>Average Reptiles Per Seizure:",round(avg$avg,1))

gg <- ggplot(avg, aes(x = Year, text=text)) + geom_line(aes(y=avg), group = 1, color = "green4", size = 1.5) + 
  geom_point(aes(y=avg), color = "Black", size = 2) + 
  theme_bw() +
  scale_x_continuous(breaks = c(2009:2016)) +
  theme(panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(family = "Gill Sans MT", size = 12), 
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size=.75))

p <- ggplotly(gg, tooltip = "text")

plotly_POST(p, filename = "repavgtimeline")



###########################################BIRDS###########################################


##Animal Timeline##

tot <- read.csv("tot.csv")

bird <- data.frame(subset(tot, Database == "Birds"))

yrs <- data.frame(table(bird$Year))
names(yrs) <- c("year", "seizures")

b50 <- subset(bird, Number > 50)
b50 <- data.frame(table(b50$Year))
names(b50) <- c("year", "b50")

b500 <- subset(bird, Number > 500)
b500 <- data.frame(table(b500$Year))
names(b500) <- c("year", "b500")

timeline <- merge(merge(yrs, b50, by="year", all=T),b500,by="year",all=T)
timeline[is.na(timeline)] <- 0
colnames(timeline) <- c("year", "All Seizures", "Seizures > 50 Birds", "Seizures > 500 Birds")

timemelt <- melt(timeline, id="year")
timemelt$text <- paste("Year:", timemelt$year,
                       "</br>Seizure Count:",timemelt$value)

bird <- transform(bird, Number = as.numeric(Number))
nb <- ddply(bird, .(Year), summarize, sumhorn = sum(Number, na.rm = TRUE))
names(nb) <- c("year", "birds")
nb$text <- paste("Year:",nb$year,
                 "</br>Birds Seized:", nb$birds)

gg <- ggplot(nb, aes(x = year, text=text)) + 
  geom_bar(aes(y=birds), stat = "identity", color = "black", fill = "steelblue3", width = .85, size = .4) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(family = "Gill Sans MT", size = 12),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = .75))

p <- ggplotly(gg, tooltip = "text")
plotly_POST(p, filename = "birdnumbertimeline") 

##Seizure Timeline##

cols <- c("All Seizures" = "#97C9E1", "Seizures > 50 Birds" = "#10709E", "Seizures > 500 Birds" = "#012030")

gg <- ggplot(timemelt, aes(x = year, text=text)) + 
  geom_line(aes(y=value,group=variable,color=variable), size = 1.5) + 
  geom_point(aes(y=value), color = "Black", size = 2) + 
  theme_bw() +
  scale_colour_manual(name="Seizures", values=cols, labels=c("All Seizures", "Seizures > 50 Birds", "Seizures > 500 Birds")) +
  theme(panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(family = "Gill Sans MT", size = 11),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = .75),
        legend.text = element_text(family = "Gill Sans MT", size = 11),
        legend.title = element_blank())

p <- ggplotly(gg, tooltip = "text")
plotly_POST(p, filename = "birdseizuretimeline") 


##Average Birds Per Seizure##

nonum <- subset(bird, select = c("Number", "Year"))
nonum <- nonum[complete.cases(nonum),]
sums <- ddply(nonum, .(Year), summarize, sums = sum(Number, na.rm = TRUE))
years <- data.frame(table(nonum$Year))
names(years) <- c("Year", "count")
avg <- merge(sums, years, by = "Year")
avg$avg <- avg$sums/avg$count
avg$text <- paste("Year:",avg$Year,
                  "</br>Average Birds Per Seizure:", round(avg$avg,1))

gg <- ggplot(avg, aes(x = Year, text=text)) + geom_line(aes(y=avg), group = 1, color = "steelblue3", size = 1.5) + 
  geom_point(aes(y=avg), color = "Black", size = 2) + 
  theme_bw() +
  scale_x_continuous(breaks = c(2009:2016)) +
  theme(panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(family = "Gill Sans MT", size = 12), 
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size=.75))

p <- ggplotly(gg, tooltip = "text")
plotly_POST(p, filename = "birdavgtimeline")
