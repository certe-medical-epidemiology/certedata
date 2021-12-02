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
  "AMR",
  "cleaner",
  "dplyr",
  "extrafont",
  "ggplot2",
  "lubridate",
  "purrr",
  "skimr",
  "stringr",
  "tibble",
  "tidyr",
  
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
installed <- core_all %in% rownames(utils::installed.packages())
core_available <- core_all[which(installed)]
core_unavailable <- core_all[which(!installed)]

base_pkgs <- rownames(installed.packages()[which(installed.packages()[, "Priority"] == "base"), ])

core_unloaded <- function() {
  search <- paste0("package:", core_available)
  core_available[!search %in% search()]
}

# Attach the package from the same package library it was
# loaded from before. https://github.com/certedata/certedata/issues/171
same_library <- function(pkg) {
  loc <- if (pkg %in% loadedNamespaces()) dirname(getNamespaceInfo(pkg, "path"))
  do.call(
    "library",
    list(pkg, lib.loc = loc, character.only = TRUE, warn.conflicts = FALSE)
  )
}

certedata_attach <- function() {
  to_load <- core_unloaded()
  if (length(to_load) == 0)
    return(invisible())

  msg(
    cli::rule(
      left = crayon::bold("Attaching packages"),
      right = paste0("certedata ", package_version("certedata"))
    ),
    startup = TRUE
  )

  versions <- vapply(to_load, package_version, character(1))
  formatted <- format(c(to_load, core_unavailable))
  packages <- paste(crayon::green(cli::symbol$tick), crayon::blue(formatted[seq_len(length(to_load))]))
  if (length(core_unavailable) > 0) {
    versions <- c(versions, rep(crayon::red("?.?.?"), length(core_unavailable)))
    packages <- c(packages,
                  paste(crayon::red(cli::symbol$cross), crayon::red(formatted[seq(length(to_load) + 1, length(formatted))])))
  }
  packages <- paste0(packages, " ", crayon::col_align(versions, max(crayon::col_nchar(versions))))
  # sort on original order
  ord <- order(core_all)
  packages <- packages[ord]
  
  if (length(packages) %% 2 == 1) {
    packages <- append(packages, "")
  }
  col1 <- seq_len(length(packages) / 2)
  info <- paste0(packages[col1], "     ", packages[-col1])

  msg(paste(info, collapse = "\n"), startup = TRUE)

  suppressPackageStartupMessages(
    lapply(to_load, same_library)
  )

  invisible()
}

package_version <- function(x) {
  version <- as.character(unclass(utils::packageVersion(x))[[1]])

  if (length(version) > 3) {
    version[4:length(version)] <- crayon::silver(as.character(version[4:length(version)]))
  }
  paste0(version, collapse = ".")
}
