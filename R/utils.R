#' Run d4utils
#'
#' @param util name of subcommand
#' @param flags flags to subcommand
#' @param path path to d4utils
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
#' @param d4utils path to d4utils (or use options("d4utils" = "path/to/d4utils"))
#'
#' @return GRanges object with `score` column
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
#' @param d4utils path to d4utils (or use options("d4utils" = "path/to/d4utils"))
#'
#' @return `Seqinfo` object.
#' @export
#'
#' @examples
d4_view_genome <- function(file, d4utils = NULL) {

  flags <- c("-g", file)
  out <- d4_run("view", flags = flags, path = d4utils)

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

  if (out$status > 0) {
    message(out$stderr)
    stop("Command had non-zero exit status")
  }

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

  if (out$status > 0) {
    message(out$stderr)
    stop("Command had non-zero exit status")
  }

  d4_stat_hist_to_df(out$stdout)

}


#' Convert a file to d4 format
#'
#' @param input path to BigWig, BAM, CRAM file
#' @param output path to save d4 output
#' @param ... see additional arguments table below for valid arguments
#' @param d4utils path to d4utils (or use options("d4utils" = "path/to/d4utils"))
#'
#' @section additional arguments:
#'
#' | name          | data type      | description                |
#' |:-------------:|:--------------:|:--------------------------:|
#' | deflate       | `logical(1)`   | Enable deflate compression |
#' | dict_auto     | `logical(1)`   | Automatically determine dictionary type by random sampling |
#' | dump_dict     | `logical(1)`   | do not profile BAM file, only dump dictionary |
#' | sparse        | `logical(1)`   | Sparse mode, same as `deflate = TRUE, dict_range = "0-1"`. Enables secondary table compression and disable primary table |
#' | deflate_level | `integer(1)`   | Configure the deflate algorithm, default 5 |
#' | dict_file     | `character(1)` | Path to a file defining the values of the dictionary |
#' | dict_range    | `character(1)` | Dictionary specification as a range: "a-b" encoding values from A to B (exclusive) |
#' | filter        | `character(1)` | A regex matching the genome name should present in the output file (note: use a shell regex, not R-style regexes) |
#' | genome        | `character(1)` | genome description file (Used by BED inputs) |
#' | mapping_qual  | `integer(1)`   | minimal mapping quality (only valid with CRAM/BAM inputs) |
#' | ref           | `character(1)` | path to reference genome file (only used with CRAM inputs) |
#' | threads       | `integer(1)`   | number of threads to use for encoding |
#'
#' @return path to new d4 object
#' @export
#' @md
#'
#' @examples
#' \donttest{
#' d4_create("example.fq", "example.d4")
#' }
d4_create <- function(input, output, ..., d4utils = NULL){

  user_flags <- cmdfun::cmd_args_dots() %>%
    cmdfun::cmd_list_interp()

  flags <- user_flags %>%
    cmdfun::cmd_list_to_flags(prefix = "--")

  flags <- c(flags, input, output)
  out <- d4_run("create", flags, path = d4utils)

  if (out$status > 0) {
    message(out$stderr)

    # Matches ... arguments to valid flags
    # provides suggestion if misspelled
    d4_run("create", "--help", path = d4utils)$stdout %>%
      cmdfun::cmd_help_parse_flags(split_newline = TRUE) %>%
      cmdfun::cmd_help_flags_similar(names(user_flags), .fun = ~{gsub("-", "_", .x)}) %>%
      cmdfun::cmd_help_flags_suggest()

    stop("Process had non-zero exit status")
  }

  if (!file.exists(output)) {
    stop(paste(output, "was not created"))
  }

  return(output)

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

## Unsure proper implementation
## But could be neat to wrap the svg for use with patchwork/cowplot
#' TODO: REMOVE NO RD
#' @noRd
d4_plot <- function() {

}
