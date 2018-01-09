#' Make a PDF field guide
#'
#' Make a PDF field guide using data contained in a \code{taxmap} object creted
#' from a function like \code{\link{search_place}} with data added from
#' functions like \code{\link{query_wiki}}.
#'
#' @param obj A \code{\link{taxmap}} object produced from \code{search_*}
#'   functions.
#' @param about If \code{TRUE}, add a page descibing how this guide was made.
#' @param contents If \code{TRUE}, add a table of contents.
#' @param map If \code{TRUE}, add the overall species occurence map.
#' @param taxonomy If \code{TRUE}, add taxonomic tree for all species at the
#'   start of the guide.
#' @param sp_taxonomy If \code{TRUE}, add taxonomic information for each
#'   species.
#' @param sp_common If \code{TRUE}, add common names for species. The \code{obj}
#'   supplied must have the result of \code{\link{query_common_name}}.
#' @param sp_map If \code{TRUE}, add a map for each species with occurence
#'   records. The \code{obj} supplied must have the result of
#'   \code{\link{query_sp_maps}}.
#' @param sp_photo If \code{TRUE}, add photos for each species from iNaturalist.
#'   The \code{obj} supplied must have the result of \code{\link{query_inat}}.
#' @param sp_photo_max The maximum number of photos to show per species.
#' @param sp_wiki If \code{TRUE}, add wikipedia content in \code{obj$data$wiki}.
#'   The \code{obj} supplied must have the result of
#'   \code{\link{query_wikipedia}}.
#' @param out_path Where to save the output file. The path can include a file
#'   name. If no file name is present, a file name is made using the title. The
#'   default is a temporary file.
#' @param source_dir Where the save the source. The default is a temporary
#'   directory.
#'
#' @inheritParams simple_pdf_cover
#'
#' @export
make_guide_pdf <- function(obj,
                           title = "Custom Field Guide",
                           cover = "basic",
                           about = TRUE,
                           contents = TRUE,
                           map = "overall_map" %in% names(obj$data),
                           taxonomy = TRUE,
                           sp_taxonomy = TRUE,
                           sp_common = "common" %in% names(obj$data),
                           sp_map = "sp_maps" %in% names(obj$data),
                           sp_photo = "inat" %in% names(obj$data),
                           sp_photo_max = 6,
                           sp_wiki = "wiki" %in% names(obj$data),
                           out_path = NULL,
                           source_dir = NULL,
                           overwrite = FALSE) {
  content <- character(0)
  sp_content <- list()

  # Parse source directory option
  if (is.null(source_dir)) {
    source_dir <- tempfile(pattern = "fieldguide")
    dir.create(source_dir)
  } else if (dir.exists(source_dir)) {
    if (overwrite) {
      message(paste0('Replacing existing source directory at "', source_dir, '"'))
      unlink(source_dir, recursive = TRUE)
    } else {
      stop(paste0('Source directory "', source_dir, '" already exists.\n',
                  '  Set "overwrite" to TRUE to replace the directory.'))
    }
  }

  # Parse output path options
  if (is.null(out_path)) {
    out_path <- source_dir
  }
  if (dir.exists(out_path)) {
    out_path <- file.path(out_path, text_to_file_name(title))
  }
  if (! grepl(out_path, pattern = "\\.pdf$", ignore.case = TRUE)) {
    out_path <- paste0(out_path, ".pdf")
  }

  # Make cover
  if (! (is.null(cover) | is.na(cover))) {
    content <- c(content, cover = simple_pdf_cover(title, cover))
  }

  # Make about section

  # Make table of contents

  # Make overall map

  # Make overall taxonomy

  # Make header for each species

  # Make taxonomy for each species

  # Make map for each species

  # Make photo code for each species

  # Make wikipedia content for each species



  # Make guide source
  raw_source <- paste0(cover, collapse = "\n")
  source_path <- file.path(source_dir, "fieldguide_source.Rnw")
  writeChar(object = raw_source,  con = source_path, eos = NULL)

  # Build guide
  knitr::knit2pdf(source_path, output = out_path)

  return(out_path)
}


#' Make latex code for a cover
#'
#' Make latex code for a cover
#'
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
#'
#' @keywords internal
simple_pdf_cover <- function(title, cover) {
  cover <- tolower(cover)
  if (cover == "basic") {

  } else {
    stop(call. = FALSE, paste0('Unknown cover style "', cover, '".'))
  }
}
