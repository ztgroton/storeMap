## code to prepare `assoc_store_comp` dataset goes here

file_path <- system.file('extdata/csv', 'assoc_store_comp.csv', package = 'storeMap')
assoc_store_comp <- read.csv(file_path, encoding = 'UTF-8', stringsAsFactors = FALSE)

usethis::use_data(assoc_store_comp, overwrite = TRUE)

rm(file_path)
rm(assoc_store_comp)
