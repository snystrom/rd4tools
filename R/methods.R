#' @noRd
region_input <- function(regions) UseMethod("region_input")
#' @noRd
bed_input <- function(regions) UseMethod("bed_input")

#' Convert GRanges to bed coordinates
#'
#' @param regions
#'
#' @return
#' @importFrom GenomicRanges start `start<-`
#'
#' @noRd
#' @export
region_input.GRanges <- function(regions){
  # convert from 1 to 0 index ranges
  start(regions) <- start(regions) - 1
  as.character(regions)
}

#' Return character as-is
#'
#' @param regions
#'
#' @return chr:start-end
#' @noRd
#' @export
region_input.character <- function(regions){
  regions
}

# TODO: keeping in case bed files eventually allowed.
#' Title
#'
#' @param regions GRanges object
#' @param path file path for bed file
#'
#' @return
#'
#' @importFrom utils write.table
#'
#' @noRd
#' @export
bed_input.GRanges <- function(regions, path = tempfile(fileext = "bed")){
  write.table(data.frame(regions)[1:3],
              file = path,
              quote = FALSE,
              sep = "\t",
              row.names = FALSE,
              col.names = FALSE)
  return(path)
}

#' Title
#'
#' @param regions regions
#'
#' @return
#' @noRd
#' @export
bed_input.character <- function(regions){
  GenomicRanges::GRanges(regions) %>%
    bed_input()
}
