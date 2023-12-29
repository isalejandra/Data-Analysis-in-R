# Install and load the remotes package (if not already installed)
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}

# Install project dependencies
remotes::install_deps()