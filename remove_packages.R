# Get the list of all installed packages
pkg <- installed.packages()

# Remove all packages
sapply(pkg[, "Package"], remove.packages)