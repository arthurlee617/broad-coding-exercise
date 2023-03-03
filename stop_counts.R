## stop_counts.R
## usage: Rscript stop_counts.R <path to .json>

## "Note that route can only be included if filter[route] is present and has exactly one /data/{index}/relationships/route/data/id." (https://api-v3.mbta.com/docs/swagger/index.html#/Stop/ApiWeb_StopController_index)
## here i will filter using the route parameter in the url for each route; alternatively can try to fiddle with query argument in GET() function

## extract informative route names for relevant subway lines

library(jsonlite)

args <- commandArgs(trailingOnly = TRUE)
path <- args[1]

x <- fromJSON(path)  ## https://api-v3.mbta.com/routes
x <- data.frame(x$data)
rail <- subset(x, attributes$type == 0 | attributes$type ==1)
## rail$id

## pass each subway line through fromJSON() and store number of stops in results table

results <- data.frame(matrix(ncol=2))
names(results) <- c("subway_line", "number_stops")
url <- NULL
routes <- list()

for (i in 1:length(rail$id)){
	url[i] <- paste("https://api-v3.mbta.com/stops?filter[route]=", rail$id[i], sep="")
	routes[[i]] <- fromJSON(url[i])
	results[i,1] <- rail$id[i]
	results[i,2] <- dim(routes[[i]]$data)[1]
}

cat("Subway route with the fewest stops: ", results$subway_line[which(results$number_stops == min(results$number_stops))], "(", min(results$number_stops), "stops)", "\n")

cat("Subway route with the most stops: ", results$subway_line[which(results$number_stops == max(results$number_stops))], "(", max(results$number_stops), "stops)", "\n")

## identify hubs

stops <- data.frame(stop_name= character(), subway_name = character())
stops_list <- list()
subway_list <- list()
for (i in 1:length(rail$id)) {
	stops_list[[i]] <- as.data.frame(routes[[i]]$data$attributes$name)
	subway_list[[i]]  <- as.data.frame(rep(rail$id[i], length(routes[[i]]$data$attributes$name)))
}
stops <- cbind(do.call(rbind, stops_list), do.call(rbind, subway_list)) # create a tall table by stitching together all stops (column 1) then attach subway name info (column 2)
names(stops) <- c("stop_name", "subway_name")

nodes <- table(stops[,1], stops[,2])
nodes <- nodes[which(rowSums(nodes) > 1),] ## only include stations that are represented in > 1 subway line ("hubs")

hubs <- data.frame(stop_name= character(), subway_name = character())
for(i in 1:dim(nodes)[1]){
	hubs[i,1] <- rownames(nodes)[i]
	hubs[i,2] <- paste(colnames(nodes)[which(nodes[i,] == 1)], collapse=", ")
}

cat("Table of connecting stops and their route names: ") 
hubs
