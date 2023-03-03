broad-coding-exercise

CodingTest.docx ## questions

/infiles/routes.json ## .json file required for long_names.R and stop_counts.R
/infiles/mbta.stops.tsv ## readable list of stop names to test subway_algorithm.R
/infiles/question2.objects.RData ## saved objects from stop_counts.R; required for subway_algorithm.R

Required libraries:
jsonlite

question 1
script and prose for question 1; use Rscript long_names.R <path to .json>
long_names.R

question 2
use Rscript stop_counts.R <path to .json>
stop_counts.R
  
question 3
usage: Rscript subway_algorithm Mattapan Airport ; if the station name is two words please use quotes ie Rscript subway_algorithm "Government Center" "Harvard"
spelling and case matter; see "mbta.stops.tsv" for list of stops with correct spelling
subway_algorithm.R
