#' @noRd
cargo_path_search <- cmdfun::cmd_path_search(option_name = "cargo",
                                             default_path = "~/.cargo/bin")

#' @noRd
d4_search_path <- cmdfun::cmd_path_search(option_name = "d4utils",
                                          default_path = "~/.cargo/bin")


#' Check that d4utils is installed
#'
#' @param d4utils path to d4utils (or use options("d4utils" = "path/to/d4utils"))
#'
#' @return Prints check if installed, red X if not detected
#'
#' @examples
#' @export
d4_install_check <- function(d4utils = NULL) {
  cmdfun::cmd_install_check(d4_search_path, path = d4utils)
}

#' Return boolean if d4utils is installed
#'
#' @param d4utils path to d4utils (or use options("d4utils" = "path/to/d4utils"))
#' @return bool
#'
#' @examples
#' @export
d4_is_installed <- function(d4utils = NULL) {
  cmdfun::cmd_install_is_valid(d4_search_path, path = d4utils)
}
