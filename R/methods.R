region_input <- function(regions) UseMethod("region_input")

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

# TODO: keeping in case bed files eventually allowed.
#region_input.GRanges <- function(regions, path = tempfile()){
#  write.table(data.frame(regions)[1:3],
#              file = path,
#              quote = FALSE,
#              sep = "\t",
#              row.names = FALSE,
#              col.names = FALSE)
#}

#' Return character as-is
#'
#' @param regions
#'
#' @return chr:start-end
#' @noRd
region_input.character <- function(regions){
  regions
}
