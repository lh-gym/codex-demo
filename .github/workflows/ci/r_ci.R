#!/usr/bin/env Rscript
options(warn = 2)

excl <- c("^\\.git/", "^\\.github/", "^renv/", "^\\.Rproj\\.user/",
          "^_bookdown_files/", "^\\.quarto/", "^_freeze/", "^output/")
all <- list.files(".", recursive = TRUE, all.files = FALSE, include.dirs = FALSE)
for (p in excl) all <- all[!grepl(p, all)]

r_files   <- grep("\\.R$",   all, value = TRUE)
rmd_files <- grep("\\.Rmd$", all, value = TRUE)

cat("R files: ", length(r_files),  "\n")
for (f in r_files) {
  cat(">>> source:", f, "\n")
  tryCatch(source(f, chdir = TRUE, echo = TRUE),
           error = function(e) { message("ERROR in ", f, ": ", e$message); quit(status = 1) })
}

cat("Rmd files:", length(rmd_files), "\n")
if (length(rmd_files)) {
  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    message("rmarkdown not installed"); quit(status = 1)
  }
  for (f in rmd_files) {
    cat(">>> render:", f, "\n")
    tryCatch(rmarkdown::render(f, envir = new.env(), quiet = FALSE),
             error = function(e) { message("ERROR in ", f, ": ", e$message); quit(status = 1) })
  }
}
