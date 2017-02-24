lvls <- levels(cities$Unique)
flight$c1 <- factor(flight$c1, levels = lvls)
flight$c2 <- factor(flight$c2, levels = lvls)

routes <- NULL
list <- NULL

for (i in 1:length(cities)){
  city <- cities$Unique[i]
  for (j in 1:length(flight)){
    route <- flight[j,]
    if(route$c1 == city){
      routes <- paste(route$text, routes)
    } else if(route$c2 == city) {
      routes <- paste(route$text, routes)
    } else
      next
  }
  list <- cbind(list, routes)
}



for (i in 1:nrow(cities)){
  city <- cities$Unique[i]
  for (j in 1:length(flight)){
    route <- flight[j,]
    if(route$c1 == city){
      test <- paste(route$text, test, sep = ",")
    } 
    if(route$c2 == city){
      test <- paste(route$text, test, sep = ",")
    } else
      next
  }
  list <- cbind(list, routes)
}



if(route$c1 == city){
  test <- paste(route$text, test, sep = ",")
} 
if(route$c2 == city){
  test <- paste(route$text, test)
} else
  next
