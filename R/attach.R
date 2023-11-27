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

base <- c("graphics",
          "grDevices",
          "stats",
          "utils")

core_all <- c(
  # these are loaded in alphabetical order
  sort(c(
    "AMR",
    "cleaner",
    "extrafont",
    "skimr",
    # tidyverse
    "broom",
    "dplyr",
    "ggplot2",
    "forcats",
    "glue",
    "lubridate",
    "purrr",
    "readr",
    "readxl",
    "stringr",
    "tibble",
    "tidyr")),
  
  # then these are loaded afterwards, to prevent function conflicts
  "flextable", # after 'purrr', as flextable::compose() conflicts with purrr::compose()
  
  # then the 'certedata' universe packages are loaded, as their functions should overwrite any other functions:
  "certedb",
  "certegis",
  "certemail",
  "certeplot2",
  "certeprojects",
  "certestats",
  "certestyle",
  "certetoolbox")

core_certe <- core_all[grepl("^certe", core_all)]

get_loc <- function(pkg) {
  if (pkg %in% loadedNamespaces()) {
    dirname(getNamespaceInfo(pkg, "path"))
  } else {
    NULL
  }
}

# attach the package from the same package library it was loaded from before
#' @importFrom cli cli_progress_step cli_text
attach_pkg <- function(pkg, silent = FALSE) {
  loc <- get_loc(pkg)
  if (silent == FALSE && interactive()) {
    cli_progress_step("Loading {.pkg {pkg}}",
                      msg_done = "Loaded {.pkg {pkg} {utils::packageVersion({pkg}, lib.loc = loc)}}",
                      msg_failed = "Failed to load {.pkg {pkg}}")
  }
  tryCatch({
    suppressWarnings(suppressPackageStartupMessages(
      # use do.call() to prevent a note in R CMD CHECK about using library() in a package
      do.call(
        library,
        list(pkg, lib.loc = loc, character.only = TRUE, warn.conflicts = FALSE))))
    if (silent == FALSE && !interactive()) cli_text("Loaded {.pkg {pkg} {utils::packageVersion({pkg}, lib.loc = loc)}}")
    return(TRUE)
  }, error = function(e) {
    if (silent == FALSE && !interactive()) cli_text("Failed to load {.pkg {pkg}}")
    return(FALSE)
  })
}

is_installed <- function(pkgs = core_all) {
  vapply(FUN.VALUE = logical(1),
         pkgs,
         function(pkg) pkg %in% rownames(utils::installed.packages(lib.loc = get_loc(pkg))),
         USE.NAMES = FALSE)
}

get_core_available <- function(pkgs = core_all) {
  pkgs[which(is_installed(pkgs))]
}

get_core_unavailable <- function(pkgs = core_all) {
  pkgs[which(!is_installed(pkgs))]
}

#' @importFrom cli cli_rule cli_text
#' @importFrom crayon bold green blue red col_align col_nchar
attach_all <- function(keep_silent = FALSE) {
  
  core_available <- get_core_available()
  core_unavailable <- get_core_unavailable()
  
  to_attach <- core_available[!paste0("package:", core_available) %in% search()]
  if (length(to_attach) == 0) {
    return(invisible(TRUE))
  }
  
  # at start-up, R will only have 'base' loaded, not other base packages
  # load them first, or our package will not overwrite their functions
  if (any(!paste0("package:", base) %in% search())) {
    to_attach <- c(base[!paste0("package:", base) %in% search()], to_attach)
  }
  
  if (keep_silent == FALSE) {
    cli_rule(left = "Attaching {.pkg certedata {utils::packageVersion('certedata')}}")
    if (any(to_attach %in% base)) {
      cli_text("Attaching first from base R: {.pkg {to_attach[to_attach %in% base]}}")
    }
  }
  
  # the actual attaching
  attach_success <- vapply(FUN.VALUE = logical(1), to_attach, attach_pkg, silent = keep_silent)
  
  invisible(attach_success)
}

#' Attach 'certedata' Universe Packages
#' 
#' @param ... unused at the moment
#' @details This function attaches all 'certedata' universe R packages and their accompanying third-party packages (in total `r length(core_all)` packages).
#' 
#' To install the packages, see [certedata_install_packages()].
#' @importFrom cli cli_inform symbol
#' @export
certedata_attach <- function(...) {
  
  keep_silent <- isTRUE(list(...)$keep_silent)
  attached <- attach_all(keep_silent = keep_silent)
  
  if (!"package:conflicted" %in% search() && keep_silent == FALSE) {
    x <- certedata_conflicts()
    overwritten_by_certe <- sum(vapply(FUN.VALUE = logical(1), x, function(x) x[1] %like% "^package:certe"), na.rm = TRUE)
    if (keep_silent == FALSE && overwritten_by_certe > 0) {
      if (interactive()) {
        cli_inform("{symbol$warning} Certe packages currently overwrite {overwritten_by_certe} functions from other packages. Run {.fn certedata_conflicts} to view the full list.")
      } else {
        print(certedata_conflicts())
      }
    }
  }
  
  pkg_plural <- function(n) {
    ifelse(n == 1, "- This package", "- These packages")
  }
  
  if ((!all(attached) || length(get_core_unavailable()) > 0) && keep_silent == FALSE) {
    cli_rule(left = "Notes")
    if (!all(attached)) {
      msg(italic(red(paste0(pkg_plural(length(which(!attached))),
                            " could not be attached:\n  ",
                            paste(sort(names(attached)[which(!attached)]),
                                  collapse = ", "), "."))),
          keep_silent = keep_silent)
    }
    if (length(get_core_unavailable()) > 0) {
      msg(italic(paste0(pkg_plural(length(get_core_unavailable())),
                        " should be available as part of the 'certedata' universe:\n  ",
                        paste(get_core_unavailable(), collapse = ", "),
                        ".\n  => Run certedata_install_packages() to install.")),
          keep_silent = keep_silent)
    }
  }
  return(invisible(attached))
}
