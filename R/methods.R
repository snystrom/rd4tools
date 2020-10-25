region_input <- function(regions) UseMethod("region_input")
bed_input <- function(regions) UseMethod("bed_input")

#' Convert GRanges to bed coordinates
#'
#' @param regions
#'
#' @return
#' @importFrom GenomicRanges start
#'
#' @noRd
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
#' @noRd
bed_input.GRanges <- function(regions, path = tempfile(fileext = "bed")){
  write.table(data.frame(regions)[1:3],
              file = path,
              quote = FALSE,
              sep = "\t",
              row.names = FALSE,
              col.names = FALSE)
  return(path)
}

bed_input.character <- function(regions, path = tempfile(fileext = "bed")){
  GenomicRanges::GRanges(regions) %>%
    bed_input()
}
