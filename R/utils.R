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

msg <- function(..., startup = FALSE) {
  if (startup) {
    if (!isTRUE(getOption("certedata.quiet"))) {
      packageStartupMessage(text_col(...))
    }
  } else {
    message(text_col(...))
  }
}

#' @importFrom rstudioapi isAvailable hasFun getThemeInfo
#' @importFrom crayon white black
text_col <- function(x) {
  # If RStudio not available, messages already printed in black
  if (!isAvailable()) {
    return(x)
  }

  if (!hasFun("getThemeInfo")) {
    return(x)
  }

  theme <- getThemeInfo()
  if (isTRUE(theme$dark)) white(x) else black(x)

}

#' List all packages in the 'certedata' universe
#' @param only_installed a [logical] to indicate whether only currently installed packages should be returned
#' @param include_self a [logical] to indicate whether the 'certedata' package should be included as well
#' @export
#' @examples
#' certedata_packages()
certedata_packages <- function(only_installed = TRUE, include_self = TRUE) {
  if (isTRUE(only_installed)) {
    pkgs <- get_core_available()
  } else {
    pkgs <- core_all
  }
  pkgs <- pkgs[grepl("^certe", pkgs)]
  if (isTRUE(include_self)) {
    pkgs <- c(pkgs, "certedata")
  }
  pkgs
}

#' Install all packages required for the 'certedata' universe
#' 
#' This function not only installs the 'certedata' universe, but also its accompanying third-party packages.
#' @details These packages will be installed if not already:
#' 
#' `r paste0(vapply(FUN.VALUE = character(1), sort(core_all), function(pkg) paste0("  * ``", pkg, "``")), collapse = "\n")`
#' @export
certedata_install_packages <- function() {
  pkgs <- get_core_unavailable()
  if (length(pkgs) == 0) {
    message("All required packages are already installed.")
    return(invisible())
  }
  if (!interactive()) {
    message("These packages require installation, but this can only be done in interactive mode: ",
            paste0(pkgs, collapse = ", "), ".")
    return(invisible())
  }
  if (isTRUE(utils::askYesNo("This will install the following packages: ", paste0(pkgs, collapse = ", ")))) {
    for (pkg in pkgs) {
      tryCatch(utils::install.packages(pkg, repos = unique(c(options()$repos, "https://certe-medical-epidemiology.r-universe.dev"))),
               error = function(e) invisible())
    }
  }
  return(invisible())
}

invert <- function(x) {
  if (length(x) == 0) return()
  stacked <- utils::stack(x)
  tapply(as.character(stacked$ind), stacked$values, list)
}

#' @importFrom crayon style make_style
style_grey <- function(level, ...) {
  style(
    paste0(...),
    make_style(grDevices::grey(level), grey = TRUE)
  )
}

is_attached <- function(x) {
  paste0("package:", x) %in% search()
}

packageVersion <- function(pkg) {
  if (all(vapply(FUN.VALUE = logical(1), pkg, function(x) isTRUE(requireNamespace(x, quietly = TRUE))))) {
    utils::packageVersion(pkg)
  } else {
    0
  }
}
