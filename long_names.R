## long_names.R
## usage: Rscript long_names.R <path to .json>

library(jsonlite)
args <- commandArgs(trailingOnly = TRUE)
path <- args[1]

x <- fromJSON(path) ## wget -O routes.json https://api-v3.mbta.com/routes

## answer 1: For this exercise I will download entire set and filter locally. This is my choice because this gives me the option to customise filtering terms if necessary in the future. This approach also gives me a consistent reference in case the data changes in the future. I may decide to also repeat filtering using the API just as a sanity check. In some cases using the API might be preferable. For example if the filesizes are prohibitively large or if egress is expensive.

#summary(x)
x <- data.frame(x$data)
## filter for attributes.type == 0 or 1

rail <- subset(x$attributes, type == 0 | type ==1)

# head(rail)
# dim(rail)
# rail

long_names <- c(rail$long_name)

cat("Subway line long names:", paste(long_names, collapse = ", "))
