
#' 'Total Wine & More' Stores
#'
#' A sample dataset of TWM physical locations and their attributes
#'
#' @format A data frame with 272 rows and 10 columns:
#' \describe{
#'   \item{store_key}{(integer) unique store id}
#'   \item{price_zone}{(character) name of store 'price zone'}
#'   \item{name}{(character) name of store}
#'   \item{description}{(character) long description of store}
#'   \item{street_address}{(character) store location's street address}
#'   \item{city}{(character) store location's city}
#'   \item{state}{(character) store location's state}
#'   \item{zipcode}{(character) store location's zipcode}
#'   \item{latitude}{(numeric) latitude of store location}
#'   \item{longitude}{(numeric) longitude of store location}
#'   ...
#' }
#'
"store_location"

#' Competitor Stores
#'
#' A sample dataset of non-TWM physical locations and their attributes
#'
#' @format A data frame with 2037 rows and 8 columns:
#' \describe{
#'   \item{comp_key}{(integer) unique competitor id}
#'   \item{banner}{(character) name of competitor's parent company}
#'   \item{street_address}{(character) competitor location's street address}
#'   \item{city}{(character) competitor location's city}
#'   \item{state}{(character) competitor location's state}
#'   \item{zipcode}{(character) competitor location's zipcode}
#'   \item{latitude}{(numeric) latitude of competitor location}
#'   \item{longitude}{(numeric) longitude of competitor location}
#'   ...
#' }
#'
"comp_location"

#' Store/Competitor Associations
#'
#' A table of linkages between TWM and non-TWM locations, defining directed 'associations' between them.
#'
#' @format A data frame with 1834 rows and 4 columns:
#' \describe{
#'   \item{assoc_key}{(integer) unique association id}
#'   \item{store_key}{(integer) unique store id}
#'   \item{comp_key}{(integer) unique competitor id}
#'   \item{is_primary}{(logical) indicates whether an association should be maintained with high priority}
#'   ...
#' }
#'
"assoc_store_comp"

#' Banner Colors
#'
#' A lookup table of RGB color values associated with non-TWM parent companies.
#'
#' @format A data frame with 348 rows and 2 columns:
#' \describe{
#'   \item{banner}{(character) name of competitor's parent company}
#'   \item{color}{(character) RGB color code}
#'   ...
#' }
#'
"banner_colors"
