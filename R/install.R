cargo_path_search <- cmdfun::cmd_path_search(option_name = "cargo",
                                             default_path = "~/.cargo/bin")

d4_search_path <- cmdfun::cmd_path_search(option_name = "d4utils",
                                          default_path = "~/.cargo/bin")

#d4_install_check <- cmdfun::cmd_install_check(d4_search_path)
d4_is_installed <- cmdfun::cmd_install_is_valid(d4_search_path)
