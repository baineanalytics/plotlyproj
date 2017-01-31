library(plotly)
library(extrafont)
library(reshape2)

#Total Seizures by DB

tot <- read.csv("tot.csv")

dbcount <- data.frame(table(tot$Database))
dbcount$Var1 <- factor(dbcount$Var1, levels = c("Ivory", "Rhino Horn", "Reptiles", "Birds"))
colnames(dbcount) <- c("Database","Seizures")

gg <- ggplot(data = dbcount, aes(x = Database, y = Seizures)) + 
  geom_bar(stat = 'identity', color = "black", fill = "maroon") +
  theme_bw() + 
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_line(size = .5), 
        axis.title = element_blank(),
        axis.text = element_text(family = "Gill Sans MT", size = 18),
        legend.title = element_blank(),
        legend.text = element_text(family = "Gill Sans MT", size = 18))

p <- ggplotly(gg)
p

Sys.setenv("plotly_username"="C4ADSdata")
Sys.setenv("plotly_api_key"="jmaBJZcHAqtJwP8H2FGn")

plotly_POST(p, filename = "totaldbseizures")

#Total Seizures Timeline

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

gg <- ggplot(data = timelinemelt, aes(x = Year, y = Seizure.Count, group = Database, color = Database)) + 
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

p <- ggplotly(gg, tooltip = c("group", "x", "y"))

Sys.setenv("plotly_username"="C4ADSdata")
Sys.setenv("plotly_api_key"="jmaBJZcHAqtJwP8H2FGn")

plotly_POST(p, filename = "totaldbseizures_timeline")