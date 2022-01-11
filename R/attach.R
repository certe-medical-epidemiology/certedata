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
    "glue",
    "lubridate",
    "purrr",
    "readr",
    "readxl",
    "stringr",
    "tibble",
    "tidyr")),
  
  # then these are loaded afterwards, to prevent function conflicts
  "flextable", # after purrr, as flextable::compose() conflicts with purrr::compose()
  "certetools", # old Certe pkg, to remove in later stage
  
  # then the 'certedata' universe packages are loaded, as their functions should overwrite any other functions:
  "certedb",
  "certegis",
  "certemail",
  "certeplot2",
  "certeprojects",
  "certestats",
  "certestyle",
  "certetoolbox")

get_loc <- function(pkg) {
  if (pkg %in% loadedNamespaces()) {
    dirname(getNamespaceInfo(pkg, "path"))
  }  else {
    NULL
  }
}

# attach the package from the same package library it was loaded from before
attach_pkg <- function(pkg) {
  loc <- get_loc(pkg)
  tryCatch({
    suppressPackageStartupMessages(
      do.call(
        "library",
        list(pkg, lib.loc = loc, character.only = TRUE, warn.conflicts = FALSE)))
    return(TRUE)
  }, error = function(e) {
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

#' @importFrom cli rule symbol
#' @importFrom crayon bold green blue red col_align col_nchar
attach_all <- function() {
  
  core_available <- get_core_available()
  core_unavailable <- get_core_unavailable()
  
  to_attach <- core_available[!paste0("package:", core_available) %in% search()]
  if (length(to_attach) == 0) {
    return(invisible(TRUE))
  }
  
  msg(
    rule(
      left = bold("Attaching 'certedata'"),
      right = paste0("certedata ", package_version("certedata"))
    ),
    startup = TRUE
  )
  
  # the actual attaching
  attach_success <- vapply(FUN.VALUE = logical(1), to_attach, attach_pkg)
  
  versions <- vapply(FUN.VALUE = character(1), to_attach, package_version)
  formatted <- format(c(to_attach, core_unavailable))
  packages <- c(paste(rep(green(symbol$tick), length(which(attach_success))), blue(formatted[which(attach_success)])),
                paste(rep(red(symbol$cross), length(which(!attach_success))), blue(formatted[which(!attach_success)])))
  
  if (length(core_unavailable) > 0) {
    versions <- c(versions, rep(red("?.?.?"), length(core_unavailable)))
    packages <- c(packages,
                  paste(red(symbol$cross), red(formatted[seq(length(to_attach) + 1, length(formatted))])))
  }
  packages <- paste0(packages, " ", col_align(versions, max(col_nchar(versions))))
  # sort on original order
  packages <- packages[order(core_all[core_all %in% trimws(formatted)])]
  
  if (length(packages) %% 2 == 1) {
    packages <- append(packages, "")
  }
  col1 <- seq_len(length(packages) / 2)
  info <- paste0(packages[col1], "     ", packages[-col1])
  
  msg(paste(info, collapse = "\n"), startup = TRUE)
  
  invisible(attach_success)
}

#' @importFrom crayon silver
package_version <- function(x) {
  version <- as.character(unclass(utils::packageVersion(x))[[1]])
  
  if (length(version) > 3) {
    version[4:length(version)] <- silver(as.character(version[4:length(version)]))
  }
  paste0(version, collapse = ".")
}

#' Attach 'certedata' Universe Packages
#' 
#' @param ... unused at the moment
#' @details This function attaches all 'certedata' universe R packages and their accompanying third-party packages.
#' @export
certedata_attach <- function(...) {
  
  startup <- isTRUE(list(...)$startup)
  
  attached <- attach_all()
  
  if (!"package:conflicted" %in% search()) {
    x <- certedata_conflicts()
    msg(certedata_conflict_message(x), startup = startup)
  }
  
  pkg_plural <- function(n) {
    ifelse(n == 1, "- This package", "- These packages")
  }
  
  if (!all(attached) || length(get_core_unavailable()) > 0) {
    msg(rule(left = bold("Notes")), startup = startup)
    if (!all(attached)) {
      msg(italic(red(paste0(pkg_plural(length(which(!attached))),
                            " could not be attached due to missing dependencies:\n  ",
                            paste(sort(names(attached)[which(!attached)]),
                                  collapse = ", "), "."))),
          startup = startup)
    }
    if (length(get_core_unavailable()) > 0) {
      msg(italic(paste0(pkg_plural(length(get_core_unavailable())),
                        " should be available as part of the 'certedata' universe:\n  ",
                        paste(get_core_unavailable(), collapse = ", "),
                        ".\n  => Run certedata_install_packages() to install.")),
          startup = startup)
    }
  }
  return(invisible(attached))
}
