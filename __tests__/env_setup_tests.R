rm(list = setdiff(ls(), "GLOBAL_test_environment_wd"))

# Check if the code is running on GitHub Actions
if (Sys.getenv("GITHUB_ACTIONS") == "true") {
  # Set the working directory for GitHub Actions
  setwd("/home/runner/work/SBARTProject/SBARTProject")
} else {
  # Set the working directory for your local machine
  setwd(GLOBAL_test_environment_wd) 
}