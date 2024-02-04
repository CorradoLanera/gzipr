dev_pkgs <- c(
  "devtools", "usethis",
  "testthat", "checkmate",
  "lintr", "spelling",
  "here", "fs"
)

renv::install(
  dev_pkgs,
  dependencies = TRUE
)

invisible(lapply(dev_pkgs, \(x) {
  usethis::use_package(x, type = "Suggests")
}))

usethis::use_ccby_license()
usethis::use_tidy_description()

# Start-up settings ------------------------------------------------

usethis::edit_r_profile("project")
usethis::edit_r_environ("project")


# Documentation ---------------------------------------------------

usethis::use_roxygen_md()
usethis::use_readme_rmd()
usethis::use_code_of_conduct("corrado.lanera@ubep.unipd.it")
usethis::use_lifecycle_badge("experimental")
usethis::use_logo("man/img/LAIMS.png")

# Checks ----------------------------------------------------------

usethis::use_spell_check()
usethis::use_testthat()
# add # library(checkmate)
fs::file_create(here::here("tests/testthat/setup.R"))
usethis::use_test("setup")
usethis::use_git()
usethis::use_coverage()
