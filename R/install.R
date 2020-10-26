#' @noRd
cargo_path_search <- cmdfun::cmd_path_search(default_path = "~/.cargo/bin/cargo")

cargo_is_installed <- cmdfun::cmd_install_is_valid(cargo_path_search)

#' @noRd
d4_search_path <- cmdfun::cmd_path_search(option_name = "d4utils",
                                          default_path = "~/.cargo/bin/d4utils")


#' Check that d4utils is installed
#'
#' @param d4utils path to d4utils (or use options("d4utils" = "path/to/d4utils"))
#'
#' @return Prints green check if installed, red X if not detected
#'
#' @examples
#' d4_install_check()
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
#' d4_is_installed()
#'
#' @export
d4_is_installed <- function(d4utils = NULL) {
  cmdfun::cmd_install_is_valid(d4_search_path, path = d4utils)
}

#' Build & Install d4utils Using Cargo
#'
#' A convenience function for building & installing d4utils from crates.io using
#' cargo. Requires a Rust install and `gcc`.
#'
#' @param ... any valid cargo install flags passed as R function arguments. Omit
#'   leading dashes, and replace dashes between words with underscores (_).
#' @param cargo path to cargo if installed at a nonstandard location. Default: "~/.cargo/bin/cargo".
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' d4_install_cargo()
#' }
d4_install_cargo <- function(..., cargo = NULL){
  # TODO: Test this
  usethis::ui_yeah("This will build & install d4utils to your cargo directory")

  if(!(cargo_is_installed(path = cargo))) {
    stop("Cargo is not installed, or cannot be detected.")
  }

  flags <- cmdfun::cmd_args_dots() %>%
    cmdfun::cmd_list_interp() %>%
    cmdfun::cmd_list_to_flags()

  cargo <- cargo_path_search(path = cargo)
  processx::run(cargo, c("install", "d4utils", flags))

}
