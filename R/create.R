#' Create d4 file(s) from inputs
#'
#' This function will create a new d4 file for each file passed to `input`.
#' Additional command arguments can be passed to `...` to configure
#'
#' Note there is no type checking on inputs, so using `input`s with
#' heterogeneous file types may produce unintended consequences when using
#' file-type specific arguments in `...`.
#'
#' @param input path to BigWig, BAM, or CRAM files to convert to d4.
#' @param output path to save d4 output. If unset, the file is named identically
#'   and in the same path as the input file, but with the .d4 extension.
#' @param ... see additional arguments table below for valid arguments
#' @param d4tools path to d4tools (or use options("d4tools" = "path/to/d4tools"))
#'
#' @section additional arguments:
#'
#' | name          | data type      | description                |
#' |:--------------|:---------------|:---------------------------|
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
#' @importFrom tools file_path_sans_ext
#'
#' @examples
#' \dontrun{
#' # Will create "example.d4"
#' d4_create("example.fq")
#'
#' # Explicit naming is done by passing a name to "output"
#' d4_create("example.fq", "my_new_file.d4")
#' }
d4_create <- function(input, output = NULL, ..., d4tools = NULL){
  # Allows creation of multiple d4's from vector of inputs

  if (!is.null(output)) {
    if (length(input) != length(output)) {
      stop(paste("input & output have different lengths.",
                 "input length:", length(input),
                 "output length:", length(output)
                 )
           )
    }
  }

  d4_paths <- character(length(input))

  for (i in seq_along(input)) {
    file <- input[i]
    d4_output <- output[i]

    d4_paths[i] <- d4_create_file(file, d4_output, ..., d4tools = d4tools)
  }

  return(d4_paths)

}

#' @param input path to file converted to d4
#' @param output optional name for d4 output, else will name "<input>.d4"
#'
#' @noRd
d4_create_file <- function(input, output = NULL, ..., d4tools = NULL){

  if (is.null(output)) {
    name <- file_path_sans_ext(input)
    output <- paste0(name, ".d4")
  }

  user_flags <- cmdfun::cmd_args_dots() %>%
    cmdfun::cmd_list_interp()

  flags <- user_flags %>%
    cmdfun::cmd_list_to_flags(prefix = "--")

  flags <- c(flags, input, output)
  out <- d4_run("create", flags, path = d4tools)

  if (out$status > 0) {
    message(out$stderr)

    # Matches ... arguments to valid flags
    # provides suggestion if misspelled
    d4_run("create", "--help", path = d4tools)$stdout %>%
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
