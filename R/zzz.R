.onAttach <- function(libname, pkgname) {
  required_version <- "2.0.3"

  # Only show the message in interactive sessions
  if (interactive() && requireNamespace("RAMP", quietly = TRUE)) {
    installed_version <- as.character(utils::packageVersion("RAMP"))

    if (utils::compareVersion(installed_version, required_version) < 0) {
      packageStartupMessage(
        paste0(
          "The installed version of RAMP (v", installed_version,
          ") has a known bug affecting model selection. \n",
          "To fix this issue, please update to the patched version by running:\n",
          "  remotes::install_github('Sciurus365/RAMP')"
        )
      )
    }
  }
}
