## code to prepare `banner_colors` dataset goes here

file_path <- system.file('extdata/csv', 'banner_colors.csv', package = 'storeMap')
banner_colors <- read.csv(file_path, encoding = 'UTF-8', stringsAsFactors = FALSE)

usethis::use_data(banner_colors, overwrite = TRUE)

rm(file_path)
rm(banner_colors)
