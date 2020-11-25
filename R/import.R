#' Read d4 view into data.frame
#'
#' @param lines raw lines from d4tools view
#'
#' @return a data.frame of `chr`, `start`, `end`, `score`
#'
#' @examples
#' lines <- "chr3R\t16703068\t16703069\t259\nchr3R\t16703069\t16703070\t260\n"
#' d4_lines_to_df(lines)
#' @noRd
d4_view_lines_to_df <- function(lines){
  strsplit(lines, "\n")[[1]] %>%
    strsplit(., "\t") %>%
    lapply(function(x){
      data.frame("chr" = x[1],
                 "start" = as.integer(x[2]),
                 "end" = as.integer(x[3]),
                 "score" = as.double(x[4]))
    }) %>%
    do.call("rbind", .)
}

#' Title
#'
#' @param lines
#'
#' @return
#'
#' @noRd
d4_view_genome_lines_to_df <- function(lines){
  strsplit(lines, "\n")[[1]] %>%
    strsplit(., "\t") %>%
    lapply(function(x){
      data.frame("chr" = x[1],
                 "length" = as.integer(x[2]))
    }) %>%
    do.call("rbind", .)
}

#' Return genome info as seqinfo
#'
#' @param df
#'
#' @return
#' @importFrom GenomeInfoDb Seqinfo
#' @noRd
d4_view_genome_to_seqinfo <- function(df){
  Seqinfo(seqnames = df$chr,
          seqlengths = df$length)
}

#' Title
#'
#' take output from d4_view_lines_to_df, fix start position & convert to GRanges
#'
#' @param df output from d4_view_lines_to_df
#'
#' @return
#' @noRd
d4_bed_to_granges <- function(df){
  df["start"] <- df["start"] + 1
  GenomicRanges::GRanges(df)
}

#' Title
#'
#' TODO: figure out how to deal with <0. It would be nice for this to be an
#' integer column
#' @param lines
#'
#' @return data.frame("score", "n")
#' @noRd
d4_stat_hist_to_df <- function(lines){
  strsplit(lines, "\n")[[1]] %>%
    strsplit(., "\t") %>%
    lapply(function(x){
      data.frame("score" = x[1],
                 "n" = as.integer(x[2]))
    }) %>%
    do.call("rbind", .)

}
