#' Make a PDF field guide
#'
#' Make a PDF field guide using data contained in a \code{taxmap} object creted
#' from a function like \code{\link{search_place}} with data added from
#' functions like \code{\link{query_wiki}}.
#'
#' @param obj A \code{\link{taxmap}} object produced from \code{search_*}
#'   functions.
#' @param title The title of the guide.
#' @param cover The style of the cover (the first page). Should be one of the
#'   following: \describe{ \item{"none"/FALSE/NULL/NA}{No cover}
#'   \item{"basic"}{A basic bit of text on a white page.} \item{"picture"}{Same
#'   as "basic", but with a randomly chosen picture of a species in the
#'   background. Photos from iNaturalist will be needed in the input object}
#'   \item{"pictures"}{Same as "basic", but with a randomly chosen selection of
#'   pictures of species in the background.} \item{"tree"}{Same as "basic", but
#'   with a taxonomic tree of the species in the background.}
#'   \item{"workcloud"}{Same as "basic", but with a word cloud of speices names
#'   in the background.} }
#' @param about If \code{TRUE}, add a page descibing how this guide was made.
#' @param contents If \code{TRUE}, add a table of contents.
#' @param map If \code{TRUE}, add the overall species occurence map.
#' @param taxonomy If \code{TRUE}, add taxonomic tree for all species at the
#'   start of the guide.
#' @param sp_taxonomy If \code{TRUE}, add taxonomic information for each
#'   species.
#' @param sp_common If \code{TRUE}, add common names for species.
#' The \code{obj} supplied must have the result of \code{\link{query_common_name}}.
#' @param sp_map If \code{TRUE}, add a map for each species with occurence
#'   records.
#'  The \code{obj} supplied must have the result of \code{\link{query_sp_maps}}.
#' @param sp_photo If \code{TRUE}, add photos for each species from iNaturalist.
#'   The \code{obj} supplied must have the result of \code{\link{query_inat}}.
#' @param sp_photo_max The maximum number of photos to show per species.
#' @param sp_wiki If \code{TRUE}, add wikipedia content in \code{obj$data$wiki}.
#'   The \code{obj} supplied must have the result of \code{\link{query_wikipedia}}.

make_guide_pdf <- function(obj,
                           title = "Custom Field Guide",
                           cover = TRUE,
                           about = TRUE,
                           contents = TRUE,
                           map = "overall_map" %in% names(obj$data),
                           taxonomy = TRUE,
                           sp_taxonomy = TRUE,
                           sp_common = "common" %in% names(obj$data),
                           sp_map = "sp_maps" %in% names(obj$data),
                           sp_photo = "inat" %in% names(obj$data),
                           sp_photo_max = 6,
                           sp_wiki = "wiki" %in% names(obj$data)) {

}
