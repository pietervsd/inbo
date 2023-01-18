
#' \code{inbopvr} package
#'
#'Pakket die enkele functies voor verschillende projecten combineert. Voorlopig gaat dit over de PCI index voor everzwijnen en enkele populatiemodelleringsfuncties.
#' @docType package
#' @name inbopvr
#' @importFrom dplyr %>%
#' @importFrom purrr %||%
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if (getRversion() >= "2.15.1")  utils::globalVariables(c("."))
