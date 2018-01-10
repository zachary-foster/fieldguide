#' Make a PDF field guide
#'
#' Make a PDF field guide using data contained in a \code{taxmap} object creted
#' from a function like \code{\link{search_place}} with data added from
#' functions like \code{\link{query_wiki}}.
#'
#' @param obj A \code{\link{taxmap}} object produced from \code{search_*}
#'   functions.
#' @param description A sentance to a few paragraphs describing the guide.
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
#' @param open If \code{TRUE}, open the PDF after it is made. (experimental)
#'
#' @inheritParams simple_pdf_cover
#'
#' @export
make_guide_pdf <- function(obj,
                           title = "Custom Field Guide",
                           description = NULL,
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
                           out_name = NULL,
                           out_dir = NULL,
                           open = FALSE) {
  # Get list of species to use
  species <- unique(obj$data$occ$name)
  names(species) <- obj$data$occ$taxon_id[match(species, obj$data$occ$name)]

  # Prepare variables that will store output source
  content <- character(0)
  sp_content <- lapply(names(species), function(x) character(0))
  names(sp_content) <- names(species)

  # Parse output name options
  if (is.null(out_name)) {
    out_name <- text_to_file_name(title)
  }

  # Parse source directory option
  if (is.null(out_dir)) {
    out_dir <- tempfile(pattern = out_name)
  }
  if (! dir.exists(out_dir)) {
    dir.create(out_dir)
  }

  # Check that the output will not overwrite anything
  output_pdf_path <- file.path(out_dir, paste0(out_name, ".pdf"))
  if (file.exists(output_pdf_path)) {
    if (overwrite) {
      message(paste0('Replacing existing output at "', output_pdf_path, '"'))
    } else {
      stop(paste0('File "', source_dir, '" already exists.\n',
                  '  Set "overwrite" to TRUE to replace the file'))
    }
  }

  # Get figure path
  figure_dir_name <- "fieldguide_figures"
  figure_dir <- file.path(out_dir, figure_dir_name)
  dir.create(figure_dir)

  # Make cover
  if (! (is.null(cover) | is.na(cover))) {
    content <- c(content, cover = simple_pdf_cover(title, cover))
  }

  # Make table of contents
  if (about) {
    content <- c(content,
                 "\\tableofcontents")
  }

  # Make about section
  if (about) {
    content <- c(content,
                 "\\newpage",
                 "\\section{About this guide}",
                 "The guide was produced using the R pacakge `fieldguide` by compiling data from free public databases automatically.",
                 "The information might contain errors.",
                 "If you find an error or omission, consider trying to update the source database and rebuild the guide.",
                 "There are a lot of people around, so small contributions to public database really add up over time.",
                 "If you find this guide or the `fieldguide` R package useful, you can thank me by posting a photo to iNaturalist or improving a Wikipedia page that interests you.",
                 "If you have any feedback on how to make the `fieldguide` package better, consider submitting an issue to https://github.com/zachary-foster/fieldguide/issues.",
                 "This is an open source project and we welcome contributions!",
                 "\n\nI hope you have a great time exploring!",
                 "\n   - Zachary Foster",
                 "\\newpage")
  }

  # Make overall map
  if (map) {
    overall_map_name <- "overall_map"
    overall_map_path <- file.path(figure_dir, paste0(overall_map_name, ".png"))
    ggplot2::ggsave(obj$data$overall_map,
                    path = figure_dir,
                    width = 8.5, units = "in",
                    scale = 1,
                    filename = paste0(overall_map_name, ".png"))
    content <- c(content,
                 "\\newpage",
                 "\\section{Overall occurrence map}",
                 "This map was made using occurrence data from iNaturalist for the most common species found.",
                 "The backgound image was downloaded from Google Maps.",
                 "\\begin{center}",
                 paste0("  \\makebox[\\textwidth]{\\includegraphics[width=1.1\\paperwidth]{", overall_map_name, "}}"),
                 "\\end{center}",
                 "\\newpage")
  }

  # Make overall taxonomy
  if (taxonomy) {
    tax_fig_name <- "overall_taxonomy"
    tax_fig_path <- file.path(figure_dir, paste0(tax_fig_name, ".png"))
    tax_figure <- metacoder::heat_tree(obj,
                         node_label = taxon_names,
                         node_size = n_subtaxa + 1,
                         node_color = n_obs,
                         node_color_axis_label = "Number of occurrences",
                         output_file = tax_fig_path)
    content <- c(content,
                 "\\newpage",
                 "\\section{Taxonomy of species in this guide}",
                 "This information comes from occurrence data downloaded from GBIF.",
                 "\\begin{center}",
                 paste0("  \\makebox[\\textwidth]{\\includegraphics[width=\\linewidth]{", tax_fig_name, "}}"),
                 "\\end{center}",
                 "\\newpage")
  }

  # Make section for species information
  content <- c(content,
               "\\section{Species information}")

  # Make header for each species
  lapply(names(sp_content), function(n) {
    sp_name <- obj$data$occ$name[obj$data$occ$taxon_id == n][1]
    my_content <- paste(sep = "\n",
                        "\\newpage",
                        paste0("\\subsection{", sp_name, "}"))
    sp_content[[n]] <<- c(sp_content[[n]], title = my_content)
  })

  # Make taxonomy for each species
  sp_classifications <- obj$classifications(sep = " $>$ ")[names(sp_content)]
  lapply(names(sp_content), function(n) {
    my_content <- paste(sep = "\n",
                        paste0("Taxonomy: \\textbf{", sp_classifications[n], "}"))
    sp_content[[n]] <<- c(sp_content[[n]], taxonomy = my_content)
  })

  # Make map for each species

  # Make photo code for each species

  # Make wikipedia content for each species



  # Make guide source
  header <- paste(sep = "\n",
                  "\\documentclass{article}",
                  "\\usepackage[margin=1in]{geometry}",
                  "\\usepackage[utf8]{inputenc}",
                  "\\usepackage{graphicx}",
                  paste0("\\graphicspath{ {", figure_dir_name,  .Platform$file.sep, "} }"),
                  paste0("\\title{", title, "}\n"),
                  "\\begin{document}",
                  "")
  footer <-  paste(sep = "\n",
                   "",
                   "\\end{document}")
  raw_source <- paste(sep = "\n",
                      header,
                      paste0(content, collapse = "\n"),
                      paste0(unlist(sp_content), collapse = "\n"),
                      footer)
  source_name <- paste0(out_name, ".Rnw")
  source_path <- file.path(out_dir, source_name)

  # Write guide source
  writeChar(object = raw_source,  con = source_path, eos = NULL)

  # Build guide
  old_wd <- getwd()
  setwd(out_dir)
  on.exit(setwd(old_wd))
  knitr::knit2pdf(source_name)

  # Open guide
  if (open) {
    system2("see", output_pdf_path, stdout = FALSE, stderr = FALSE, wait = FALSE)
    system2("open", output_pdf_path, stdout = FALSE, stderr = FALSE, wait = FALSE)
  }

  # Print output file
  message(paste0("Output file created at:\n  ", output_pdf_path))

  return(output_pdf_path)
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
    # content <- paste(sep = "\n",
    #                  "\\topskip0pt",
    #                  "\\vspace*{\\fill}",
    #                  title,
    #                  "\\vspace*{\\fill}",
    #                  "%")
    content <- paste(sep = "\n",
                     "\\maketitle\n",
                     "\\newpage")
  } else {
    stop(call. = FALSE, paste0('Unknown cover style "', cover, '".'))
  }

  return(content)
}
