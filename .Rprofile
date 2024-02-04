source("renv/activate.R")
options(tidyverse.quiet = TRUE)

if (interactive()) {
  if (as.logical(Sys.getenv("ATTACH_STARTUP_PKGS"))) {
    usethis::ui_todo("Attaching development supporting packages...")
    suppressPackageStartupMessages(suppressWarnings({
      library(usethis)
      ui_done("Library {ui_value('usethis')} attached.")
      library(devtools)
      ui_done("Library {ui_value('devtools')} attached.")
      library(testthat)
      ui_done("Library {ui_value('testthat')} attached.")
      library(checkmate)
      ui_done("Library {ui_value('checkmate')} attached.")
    }))
  }
}
