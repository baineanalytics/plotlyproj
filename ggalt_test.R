library(maps)
library(ggplot2)
library(ggalt)

world_map <- map_data("world")
world_map <- subset(world_map, region!="Antarctica")

#library(devtools)
#dev_mode(on=T)

library(ggalt)

gg <- ggplot(data = world_mapff, aes(x = long, y = lat, group = group, fill = sum)) + 
  geom_cartogram(dat = world_map, map = world_map, aes(map_id = region), color = "gray20", fill = "white", lwd = .05)

gg + coord_proj()
