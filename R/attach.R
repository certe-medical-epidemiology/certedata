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
    "lubridate",
    "purrr",
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
same_library <- function(pkg) {
  loc <- get_loc(pkg)
  do.call(
    "library",
    list(pkg, lib.loc = loc, character.only = TRUE, warn.conflicts = FALSE)
  )
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
certedata_attach <- function() {
  
  core_available <- get_core_available()
  core_unavailable <- get_core_unavailable()
  
  to_load <- core_available[!paste0("package:", core_available) %in% search()]
  if (length(to_load) == 0) {
    return(invisible(TRUE))
  }
  
  msg(
    rule(
      left = bold("Attaching packages"),
      right = paste0("certedata ", package_version("certedata"))
    ),
    startup = TRUE
  )
  
  versions <- vapply(to_load, package_version, character(1))
  formatted <- format(c(to_load, core_unavailable))
  packages <- paste(green(symbol$tick), blue(formatted[seq_len(length(to_load))]))
  if (length(core_unavailable) > 0) {
    versions <- c(versions, rep(red("?.?.?"), length(core_unavailable)))
    packages <- c(packages,
                  paste(red(symbol$cross), red(formatted[seq(length(to_load) + 1, length(formatted))])))
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
  
  suppressPackageStartupMessages(
    lapply(to_load, same_library)
  )
  
  invisible(TRUE)
}

#' @importFrom crayon silver
package_version <- function(x) {
  version <- as.character(unclass(utils::packageVersion(x))[[1]])
  
  if (length(version) > 3) {
    version[4:length(version)] <- silver(as.character(version[4:length(version)]))
  }
  paste0(version, collapse = ".")
}
