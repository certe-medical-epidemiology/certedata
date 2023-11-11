# ===================================================================== #
#  An R package by Certe:                                               #
#  https://github.com/certe-medical-epidemiology                        #
#                                                                       #
#  Licensed as GPL-v2.0.                                                #
#                                                                       #
#  Developed at non-profit organisation Certe Medical Diagnostics &     #
#  Advice, department of Medical Epidemiology.                          #
#                                                                       #
#  This R package is free software; you can freely use and distribute   #
#  it for both personal and commercial purposes under the terms of the  #
#  GNU General Public License version 2.0 (GNU GPL-2), as published by  #
#  the Free Software Foundation.                                        #
#                                                                       #
#  We created this package for both routine data analysis and academic  #
#  research and it was publicly released in the hope that it will be    #
#  useful, but it comes WITHOUT ANY WARRANTY OR LIABILITY.              #
# ===================================================================== #

#' Conflicts between the 'certedata' universe and other packages
#'
#' This function lists all the conflicts between packages in the 'certedata' universe and other packages that you have loaded.
#' @export
#' @importFrom purrr keep imap compact
#' @examples
#' certedata_conflicts()
certedata_conflicts <- function() {
  envs <- grep("^package:", search(), value = TRUE)
  envs <- stats::setNames(envs, envs)
  objs <- invert(lapply(envs, ls_env))
  
  core_pkg <- paste0("package:", get_core_available())
  conflcts <- objs[vapply(FUN.VALUE = logical(1), objs, function(x) length(x) > 1 & any(x %in% core_pkg))]
  
  conflict_funs <- imap(conflcts, confirm_conflict)
  conflict_funs <- compact(conflict_funs)
  
  # sort on package name, not on function name
  conflict_funs <- conflict_funs[names(sort(vapply(FUN.VALUE = character(1), conflict_funs, function(x) x[[1]])))]
  
  structure(conflict_funs, class = "certedata_conflicts")
}

#' @importFrom purrr map map_chr map2_chr
#' @importFrom crayon bold blue silver italic green red
#' @importFrom cli rule symbol
certedata_conflict_message <- function(x) {
  if (length(x) == 0) return("")
  
  header <- rule(
    left = bold("Conflicts"),
    right = "certedata_conflicts()"
  )
  
  pkgs <- x |> map(~ gsub("^package:", "", .))
  others <- pkgs |> map(`[`, -1)
  base_pkgs <- rownames(utils::installed.packages()[which(utils::installed.packages()[, "Priority"] == "base"), ])
  other_calls <- map2_chr(
    others, names(others),
    ~ paste0(blue(ifelse(!.x %in% base_pkgs,
                         red(.x),
                         .x)), "::", silver(paste0(.y, "()")), collapse = ", ")
  )
  
  winner <- pkgs |> map_chr(1)
  funs <- format(paste0(blue(winner), "::", green(paste0(names(x), "()"))))
  bullets <- paste0(
    blue(symbol$bullet), " ", funs,
    italic(" masks "), other_calls,
    collapse = "\n"
  )
  
  paste0(header, "\n", bullets)
}

#' @export
#' @importFrom cli cat_line
print.certedata_conflicts <- function(x, ..., keep_silent = FALSE) {
  cat_line(certedata_conflict_message(x))
}

#' @importFrom purrr map keep
confirm_conflict <- function(packages, name) {
  # only look at functions
  objs <- packages |>
    map(~ get(name, pos = .)) |>
    keep(is.function)
  
  if (length(objs) <= 1) {
    return(NULL)
  }
  
  # remove identical functions
  keep_pkg <- which(!duplicated(objs))
  if (length(keep_pkg) <= 1) {
    return(NULL)
  }
  
  packages[keep_pkg]
}

ls_env <- function(env) {
  ls(pos = env)
}
