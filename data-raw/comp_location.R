## code to prepare `comp_location` dataset goes here

file_path <- system.file('extdata/csv', 'comp_location.csv', package = 'storeMap')
comp_location <- read.csv(file_path, encoding = 'UTF-8', stringsAsFactors = FALSE)

usethis::use_data(comp_location, overwrite = TRUE)

rm(file_path)
rm(comp_location)
