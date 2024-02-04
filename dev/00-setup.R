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
usethis::use_github(protocol = "ssh")
usethis::use_coverage()

# Basic functions' infrastructure ---------------------------------

usethis::use_r("utils")
usethis::use_test("utils")
usethis::use_r("functions")
usethis::use_test("functions")

# Isolation -------------------------------------------------------

usethis::git_vaccinate()
usethis::use_tidy_description()
renv::upgrade()
renv::update()
renv::status()
renv::snapshot()

# CI/CD -----------------------------------------------------------

usethis::use_github_action("check-release")
usethis::use_github_actions_badge("check-release")
usethis::use_github_action("test-coverage")
usethis::use_github_actions_badge("test-coverage")
usethis::use_github_action("lint")
usethis::use_github_actions_badge("lint")
usethis::use_tidy_github()
