all_pgs <- renv:::renv_lockfile_read("renv.lock")

small_subset <- all_pgs[["Packages"]][11L:12]

all_reps <- lapply(all_pgs[["Packages"]], function(i) i[["Repository"]] == "CRAN")

lapply(names(all_reps[lengths(all_reps) > 0]), function(ith_package_name) {
  ith_package <- all_pgs[["Packages"]][[ith_package_name]]
  devtools::install_version(package = ith_package[["Package"]],
                            version = ith_package[["Version"]],
                            upgrade = "never",
                            repos = "https://cloud.r-project.org")
})
