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


#' View d4 region
#'
#' @param file path do d4 file
#' @param pos positions ("chr:start-end", or GRanges object)
#' @param path path to d4utils
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom GenomicRanges GRanges
#'
#' @examples
d4_view <- function(file, pos, path = NULL) {

  regions <- region_input(pos)
  flags <- c(file, regions)
  out <- d4_run("view", flags = flags, path = path)

  # TODO:
  # check exit status, etc.
  # suggest helps if fail

  d4_view_lines_to_df(out$stdout) %>%
    d4_bed_to_granges(.)

}

#' View genome info as Seqinfo
#'
#' @param file d4 file
#' @param path path to d4utils
#'
#' @return
#' @export
#'
#' @examples
d4_view_genome <- function(file, path = NULL) {

  flags <- c("-g", file)
  out <- d4_run("view", flags = flags, path = path)

  # TODO:
  # check exit status, etc.
  # suggest helps if fail

  d4_view_genome_lines_to_df(out$stdout) %>%
    d4_view_genome_to_seqinfo()

}

d4_stat <- function(file, regions, method = "mean", threads = 1, d4utils = NULL) {
  regions <- region_input(regions)
  flags <- c(file, "-r", regions, "-s", method, "-t", threads)
  out <- d4_run("stat", flags, path = d4utils)

  # TODO: checks

  df <- d4_view_lines_to_df(out$stdout)
  # TODO: parse method to get name of column
  names(df)[4] <- method
  df
}

## Unsure proper implementation
## But could be neat to wrap the svg for use with patchwork/cowplot
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
d4_framedump <- function() {

}
