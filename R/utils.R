#' Run d4tools
#'
#' @param util name of subcommand
#' @param flags flags to subcommand
#' @param path path to d4tools
#'
#' @return processx output
#'
#' @examples
#' d4_run("create", c("input.fa", "output.d4"))
#' @noRd
d4_run <- function(util, flags = NULL, path = NULL){
  d4path <- d4_search_path(path)
  processx::run(d4path, c(util, flags), error_on_status = FALSE)
}

#' View signal within regions
#'
#'
#' @param file path to d4 file
#' @param regions positions ("chr:start-end", or GRanges object). Coordinates should
#'   be 1 indexed (GRanges format), not 0 indexed (bed format).
#' @param d4tools path to d4tools (or use options("d4tools" = "path/to/d4tools"))
#'
#' @return GRanges object with `score` column
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom GenomicRanges GRanges
#'
#' @examples
d4_view <- function(file, regions, d4tools = NULL) {
  # TODO: unwrap flag? for unbinned signal?

  regions <- region_input(regions)
  flags <- c(file, regions)
  out <- d4_run("view", flags = flags, path = d4tools)

  if (out$status > 0) {
    message(out$stderr)
    stop("Command had non-zero exit status")
  }

  d4_view_lines_to_df(out$stdout) %>%
    d4_bed_to_granges(.)
}

#' View genome info as Seqinfo
#'
#' @param file d4 file
#' @param d4tools path to d4tools (or use options("d4tools" = "path/to/d4tools"))
#'
#' @return `Seqinfo` object.
#' @export
#'
#' @examples
d4_view_genome <- function(file, d4tools = NULL) {

  flags <- c("-g", file)
  out <- d4_run("view", flags = flags, path = d4tools)

  if (out$status > 0) {
    message(out$stderr)
    stop("Command had non-zero exit status")
  }

  d4_view_genome_lines_to_df(out$stdout) %>%
    d4_view_genome_to_seqinfo()

}

#' Compute summary statistics within target regions
#'
#' @param file path to d4 file
#' @param regions positions ("chr:start-end", or GRanges object). Coordiates should
#' @param method statistic to calculate. One of "mean" or "median".
#' @param threads number of threads to use
#' @param d4tools path to d4tools (or use options("d4tools" = "path/to/d4tools"))
#'
#' @return
#' @export
#'
#' @examples
d4_stat <- function(file, regions, method = "mean", threads = 1, d4tools = NULL) {
  match.arg(method, c("mean", "median"))
  regions <- bed_input(regions)
  flags <- c(file, "-r", regions, "-s", method, "-t", threads)
  out <- d4_run("stat", flags, path = d4tools)

  if (out$status > 0) {
    message(out$stderr)
    stop("Command had non-zero exit status")
  }

  df <- d4_view_lines_to_df(out$stdout)
  # TODO: parse method to get name of column
  names(df)[4] <- method
  GRanges(df)
}

#' Collect histogram of d4 file
#'
#' @param file d4 file path
#' @param regions optional regions in which to compute histogram values.
#'   Formatted as string: "chr:start-end", or as GRanges. Note coordinates
#'   should use base 1 index, not bed-format 0 index. (default: NULL)
#' @param threads number of threads to use (default: 1)
#' @param d4tools path to d4tools (or use options("d4tools" = "path/to/d4tools"))
#'
#' @return data.frame
#' @export
#'
#' @examples
d4_histogram <- function(file, regions = NULL, threads = 1, d4tools = NULL) {

  if (!is.null(regions)) {
    regions <- bed_input(regions)
    flags <- c(file, "-r", regions, "-s", "hist", "-t", threads)
  } else {
    flags <- c(file, "-s", "hist", "-t", threads)
  }
  out <- d4_run("stat", flags, path = d4tools)

  if (out$status > 0) {
    message(out$stderr)
    stop("Command had non-zero exit status")
  }

  d4_stat_hist_to_df(out$stdout)

}

#' Work in progress
#'
#' @return
#' @export
#'
#' @noRd
#' @examples
d4_framedump <- function() {
  # TODO: REMOVE NO RD

}

## Unsure proper implementation
## But could be neat to wrap the svg for use with patchwork/cowplot
#' @noRd
d4_plot <- function() {
  # TODO: REMOVE NO RD

}
