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

.onAttach <- function(...) {
  needed <- core_available[!is_attached(core_available)]
  if (length(needed) == 0)
    return()
  
  crayon::num_colors(TRUE)
  tryCatch({
    certedata_attach()
    
    if (!"package:conflicted" %in% search()) {
      x <- certedata_conflicts()
      msg(certedata_conflict_message(x), startup = TRUE)
    }
    
    if (length(core_unavailable) > 0) {
      msg(crayon::italic(paste0(ifelse(length(core_unavailable) == 1, 
                                       "One package is",
                                       "Some packages are"),
                                " not installed, but should be loaded as part of the 'certedata' universe")),
          startup = TRUE)
    }
  }, error = function(e) packageStartupMessage(e$message))
  
}
