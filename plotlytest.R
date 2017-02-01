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

Sys.setenv("plotly_username"="C4ADSdata")
Sys.setenv("plotly_api_key"="jmaBJZcHAqtJwP8H2FGn")

plotly_POST(p, filename = "totaldbseizures")

#Information Quality

tot <- read.csv("tot.csv")

tot[is.na(tot)] <- ""

info <- NULL

for (j in 1:length(tot)){
  data <- tot[j]
  name <- names(data)
  wifdata <- sum(data != "")
  dataperc <- data.frame((wifdata/773)*100)
  names(dataperc) <- name
  info <- dplyr::bind_cols(info, dataperc)
}

infoq <- info[,3:16]
infoq <- infoq[,-2]
names(infoq) <- c("Date", "Seizure Airport", "Seizure Location", "Origin", "Transit", "Destination", "Weight", "Number", "Airline", "Mode of Transport", "Method of Detection", "Obfuscation Method", "Species")

meltedinfo <- melt(infoq)
meltedinfo$Availability <- paste(round(meltedinfo$value, 1),"%",sep="")
meltedinfo$text <- paste("Category:", meltedinfo$variable,
                         "</br>Availability:",meltedinfo$Availability)

gg <- ggplot(data = meltedinfo, aes(x = reorder(variable,value), y = value, z = Availability, text = text)) + 
  geom_bar(stat = 'identity', color = "black", fill = "maroon") +
  #geom_text(aes(label = labs), hjust = 1.1, family = "Gill Sans MT", size = 5) +
  theme_bw() + coord_flip() +
  scale_y_continuous(limits = c(0, 100), breaks = c(0,25,50,75,100), labels = c("0%", "25%", "50%", "75%", "100%")) +
  labs(x = "Variable", y = "VALUE") +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major.y = element_blank(), 
        panel.grid.major.x = element_line(size = .5), 
        axis.title = element_blank(), 
        axis.text = element_text(family = "Gill Sans MT", size = 15),
        legend.title = element_blank(),
        legend.text = element_text(family = "Gill Sans MT", size = 15))

p <- ggplotly(tooltip = "text")
p

plotly_POST(p, filename = "info_availability")
