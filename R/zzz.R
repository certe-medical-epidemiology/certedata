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

#' @importFrom crayon num_colors italic red bold
#' @importFrom cli rule
.onAttach <- function(...) {
  core_available <- get_core_available()
  needed <- core_available[!is_attached(core_available)]
  if (length(needed) == 0) {
    return(invisible())
  }
  
  num_colors(TRUE)
  initialised <- certedata_attach()
  
  if (!"package:conflicted" %in% search()) {
    x <- certedata_conflicts()
    msg(certedata_conflict_message(x), startup = TRUE)
  }
  
  pkg_plural <- function(n) {
    ifelse(n == 1, "- This package", "- These packages")
  }
  
  if (!all(initialised) || length(get_core_unavailable()) > 0) {
    msg(rule(left = bold("Notes")), startup = TRUE)
    if (!all(initialised)) {
      msg(italic(red(paste0(pkg_plural(length(which(!initialised))),
                            " could not be attached due to missing dependencies:\n  ",
                            paste(sort(names(initialised)[which(!initialised)]),
                                  collapse = ", "), "."))),
          startup = TRUE)
    }
    if (length(get_core_unavailable()) > 0) {
      msg(italic(paste0(pkg_plural(length(get_core_unavailable())),
                        " should be available as part of the 'certedata' universe:\n  ",
                        paste(get_core_unavailable(), collapse = ", "),
                        ".\n  => Run certedata_install_packages() to install.")),
          startup = TRUE)
    }
  }
}
