get_common_name <- function(db) {
  # Get common name
    my_print("Looking up common names from ", toupper(db), "...")
    common_names <- taxize::sci2comm(gbif_occ$name, db = db,
                                    verbose = FALSE, ask = FALSE, rows = 1)
    common_names <- lapply(common_names, function(x) {
      x <- x[!is.na(x)]
      if (length(x) > 0) {
        x <- Hmisc::capitalize(x)
        x <- unique(x)
      }
      paste0(x, collapse = ", ")
    })
    my_print("   Found common names.\n")

    return(common_names)
}
