#'@title Retrieves Taxonomy from World Register of Marine Species  
#'
#'@description Query a vector to taxa names against WORMS and retrives valid names and classification 
#'
#'@param x A vector of species names 
#'
#'@param ... additional arguments passed to worrms::wormsmatchsp and worrms::wormsmatchgen.
#'
#'@details
#' The taxonomic assignment of the ASVs resulting from blasting against GenBank is not allways up to date. This fuction will query the names 
#' against Worl Register of Marine Species at https://www.marinespecies.org/ by using the package worrms. Note that the R package worms is deprecated.
#' The function returns a data.frame with the columns "kingdom", "phylum", "class", "order",
#' "family", "genus", "AphiaID", "status", "scientificname", "valid_name" and"query_name".
#'
#' The function assumes that the separator between genus name and species name is either a space or an underscore. 
#' 
#'@author Mattew Tamayo
#'@import worrms dplyr
#'
#'@examples
#' data(deepMeio)
#' tax_deepMeio <- taxFromWorrms(deepMeio$Species[1:51])
#'
#'#example retrieve non only marine species 
#' tax_deepMeio <- taxFromWorrms(deepMeio$Species[1:51], marine_only=FALSE)
#' 
#' 
#' 
#'@export taxFromWorrms
#'@seealso \code{\link{worrms}} 


	taxFromWorrms <- function(x, ...) {
  
  wormsbynames <- function(names, chunksize = 50, ...) {
    rows <- lapply(names, function(nm) {
      tryCatch(
        worrms::wm_records_name(name = nm, fuzzy = FALSE, ...)[1, ],
        error = function(e) NULL
      )
    })
    
    out <- dplyr::bind_rows(rows)
    
    if (ncol(out) == 0) {
      stop("All WoRMS lookups failed — check species name formatting or API connectivity.")
    }
    

    if (length(rows) > nrow(out)) {
      template <- out[1, ][rep(1, length(names)), ]
      template[TRUE, ] <- NA
      success_idx <- which(!sapply(rows, is.null))
      template[success_idx, ] <- out
      return(template)
    }
    
    out
  }
  
  cnksize <- ifelse(length(x) %% 50 == 1, 55, 50)
  spnames <- gsub(x = x, "[ _]", " ")
  genus <- sub("[ _].*", "", x)
  wormsmatchsp <- wormsbynames(spnames, chunksize = cnksize, ...)
  wormsmatchsp <- wormsmatchsp[, c(1, 5, 3, 10)]
  wormsmatchgen <- wormsbynames(genus, chunksize = cnksize, ...)
  wormsmatchgen <- wormsmatchgen[, c(13:18)]
  dat <- data.frame(wormsmatchgen, wormsmatchsp, query_name = x)
  dat[is.na(dat)] <- "no_match"
  return(dat)
}
