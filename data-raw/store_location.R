## code to prepare `store_location` dataset goes here

file_path <- system.file('extdata/csv', 'store_location.csv', package = 'storeMap')
store_location <- read.csv(file_path, encoding = 'UTF-8', stringsAsFactors = FALSE)

usethis::use_data(store_location, overwrite = TRUE)

rm(file_path)
rm(store_location)
