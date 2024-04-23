pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
appDir <- system.file('app', package = 'storeMap')
shiny::runApp(appDir)
