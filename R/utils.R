#' Run d4utils
#'
#' @param util name of subcommand
#' @param flags flags to subcommand
#' @param path path to d4utils
#'
#' @return processx output
#'
#' @examples
#' @noRd
d4_run <- function(util, flags = NULL, path = NULL){
  d4path <- d4_search_path(path)
  processx::run(d4path, c(util, flags), error_on_status = FALSE)
}


#' View signal within regions
#'
#'
#' @param file path do d4 file
#' @param regions positions ("chr:start-end", or GRanges object). Coordiates should
#'   be 1 indexed (GRanges format), not 0 indexed (bed format).
#' @param d4utils path to d4utils (or use options("d4utils" = "path/to/d4utils"))
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom GenomicRanges GRanges
#'
#' @examples
d4_view <- function(file, regions, d4utils = NULL) {
  # TODO: unwrap flag? for unbinned signal?

  regions <- region_input(regions)
  flags <- c(file, regions)
  out <- d4_run("view", flags = flags, path = d4utils)

  # TODO:
  # check exit status, etc.
  # suggest helps if fail

  d4_view_lines_to_df(out$stdout) %>%
    d4_bed_to_granges(.)
}

#' View genome info as Seqinfo
#'
#' @param file d4 file
#' @param d4utils path to d4utils (or use options("d4utils" = "path/to/d4utils"))
#'
#' @return `Seqinfo` object.
#' @export
#'
#' @examples
d4_view_genome <- function(file, d4utils = NULL) {

  flags <- c("-g", file)
  out <- d4_run("view", flags = flags, path = d4utils)

  # TODO:
  # check exit status, etc.
  # suggest helps if fail

  d4_view_genome_lines_to_df(out$stdout) %>%
    d4_view_genome_to_seqinfo()

}

#' Compute summary statistics within target regions
#'
#' @param file path to d4 file
#' @param regions positions ("chr:start-end", or GRanges object). Coordiates should
#' @param method statistic to calculate. One of "mean" or "median".
#' @param threads number of threads to use
#' @param d4utils path to d4utils (or use options("d4utils" = "path/to/d4utils"))
#'
#' @return
#' @export
#'
#' @examples
d4_stat <- function(file, regions, method = c("mean", "median"), threads = 1, d4utils = NULL) {
  match.arg(method, c("mean", "median"))
  regions <- region_input(regions)
  flags <- c(file, "-r", regions, "-s", method, "-t", threads)
  out <- d4_run("stat", flags, path = d4utils)

  # TODO: checks

  df <- d4_view_lines_to_df(out$stdout)
  # TODO: parse method to get name of column
  names(df)[5] <- method
  df
}

#' Collect histogram of d4 file
#'
#' TODO: make regions optional (use cmdfun to parse?)
#'
#' @param file d4 file path
#' @param regions regions in "chr:start-end" format, or GRanges
#' @param threads number of threads to use
#' @param d4utils path to d4utils (or use options("d4utils" = "path/to/d4utils"))
#'
#' @return data.frame
#' @export
#'
#' @examples
d4_hist <- function(file, regions, threads = 1, d4utils = NULL) {

  regions <- bed_input(regions)
  flags <- c(file, "-r", regions, "-s", "hist", "-t", threads)
  out <- d4_run("stat", flags, path = d4utils)

  # TODO: checks

  d4_stat_hist_to_df(out$stdout)

}

## Unsure proper implementation
## But could be neat to wrap the svg for use with patchwork/cowplot
#' TODO: REMOVE NO RD
#' @noRd
d4_plot <- function() {

}

#' Work in progress
#'
#' @return
#' @export
#'
#' @examples
d4_create <- function(){

}

#' Work in progress
#'
#' @return
#' @export
#'
#' @examples
#' TODO: REMOVE NO RD
#' @noRd
d4_framedump <- function() {

}
