
## define subway lines that have a 1 degree of separation via the hubs defined above
direct_connections <- list()
for(i in 1:length(rail$id)){
	direct_connections[[i]] <- unique(unlist(strsplit(hubs$subway_name[grep(rail$id[i],hubs$subway_name)], ", ")))
}
names(direct_connections) <- rail$id

## define all stops and subway lines they belong to

stops_list <- list()
for (i in 1:length(rail$id)) {
	stops_list[[i]] <- routes[[i]]$data$attributes$name
}
names(stops_list) <- rail$id

## func.pathway takes a starting station and end station and returns the subway lines needed to connect the two stations
## usage: Rscript find_pathway.R Mattapan Airport

## step 1: given any two correctly spelled start and end stations, extract their subway line
## once you know the subway lines, you can simply treat each line as a node; the hubs will determine their connectivity; for the purposes of this exercise, this approach is much simpler than treating each station as a node
## there are three scenarios to consider: 1) both stations are in the same line (e.g., Harvard, Alewife); 2) different lines, but 1 degree of separation based on hubs (e.g., Harvard, Arlington); 3) differnt lines with > 1 degree of separation. this will require changing lines more than once (e.g., Harvard, Airport)

#start <- "Mattapan"
#end <- "Airport"

func.pathway <- function(start, end){
	found.start <- FALSE
	found.end <- FALSE
	for (i in 1:length(stops_list)){
		if (start %in% stops_list[[i]]){
			found.start <- TRUE
			start.line <- rail$id[i]
		}
		if (end %in% stops_list[[i]]){
			found.end <- TRUE
			end.line <- rail$id[i]
		}
    if(found.start ==TRUE & found.end == TRUE) {
      break ## stop looking as soon as you find both start and end values
    }
	}
	if(found.start == FALSE | found.end == FALSE){
		return("Start or end value not found. Please check spelling.")
	}
	pathway <- c(start.line)
	if (start.line == end.line){ ## scenario 1
		cat(paste(pathway))
	}
	else{
		while(start.line != end.line){
			if(end.line %in% direct_connections[[start.line]]){ ## scenario 2
				pathway <- c(pathway, end.line)
				cat(paste(pathway, collapse = " -> "))
				break
			}
			else{ ## scenario 3
				mid.line_i <- character() # intermediate lines
				mid.line_i <- sample(direct_connections[[start.line]][direct_connections[[start.line]] != start.line], 1) ## uses sample() function to randomly connect to a different line as long as they are directly connected; i know this is not an optimised solution but it should work; we can call it the 'drunken rider' algorithm who stumbles onto different lines until he gets home; the slice notation is so that you don't go from red line to red line for example; if you don't include it then you get weird behaviour like Mattapan -> Mattapan -> Mattapan
				pathway <- c(pathway, mid.line_i) # add intermediate lines to pathway
				start.line <- mid.line_i # update start.line to mid.line_i; this allows us to iterate until start.line == end.line
			}
		}
	}
}