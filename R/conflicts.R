#' Conflicts between the certedata and other packages
#'
#' This function lists all the conflicts between packages in the certedata
#' and other packages that you have loaded.
#'
#' @export
#' @examples
#' certedata_conflicts()
certedata_conflicts <- function() {
  envs <- grep("^package:", search(), value = TRUE)
  envs <- purrr::set_names(envs)
  objs <- invert(lapply(envs, ls_env))

  conflicts <- purrr::keep(objs, ~ length(.x) > 1)

  tidy_names <- paste0("package:", certedata_packages())
  conflicts <- purrr::keep(conflicts, ~ any(.x %in% tidy_names))

  conflict_funs <- purrr::imap(conflicts, confirm_conflict)
  conflict_funs <- purrr::compact(conflict_funs)
  
  # sorteren op package naam, niet op functie
  conflict_funs <- conflict_funs[names(sort(sapply(conflict_funs, function(x) x[[1]])))]

  structure(conflict_funs, class = "certedata_conflicts")
}

certedata_conflict_message <- function(x) {
  if (length(x) == 0) return("")

  header <- cli::rule(
    left = crayon::bold("Conflicts"),
    right = "certedata_conflicts()"
  )
  
  pkgs <- x %>% purrr::map(~ gsub("^package:", "", .))
  others <- pkgs %>% purrr::map(`[`, -1)
  other_calls <- purrr::map2_chr(
    others, names(others),
    ~ paste0(crayon::blue(ifelse(!.x %in% base_pkgs,
                                 crayon::red(.x),
                                 .x)), "::", crayon::silver(paste0(.y, "()")), collapse = ", ")
  )
  
  winner <- pkgs %>% purrr::map_chr(1)
  funs <- format(paste0(crayon::blue(winner), "::", crayon::green(paste0(names(x), "()"))))
  bullets <- paste0(
    crayon::blue(cli::symbol$bullet), " ", funs,
    crayon::italic(" masks "), other_calls,
    collapse = "\n"
  )

  paste0(header, "\n", bullets)
}

#' @export
print.certedata_conflicts <- function(x, ..., startup = FALSE) {
  cli::cat_line(certedata_conflict_message(x))
}

#' @importFrom magrittr %>%
confirm_conflict <- function(packages, name) {
  # Only look at functions
  objs <- packages %>%
    purrr::map(~ get(name, pos = .)) %>%
    purrr::keep(is.function)

  if (length(objs) <= 1)
    return()

  # Remove identical functions
  objs <- objs[!duplicated(objs)]
  packages <- packages[!duplicated(packages)]
  if (length(objs) == 1)
    return()

  packages
}

ls_env <- function(env) {
  x <- ls(pos = env)
  if (identical(env, "package:dplyr")) {
    x <- setdiff(x, c("intersect", "setdiff", "setequal", "union"))
  }
  x
}
