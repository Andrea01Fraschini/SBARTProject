source("R/env_setup.R") # set up the environment
source("R/library_imports.R") # import the libraries
source("R/SBART/sbart.R") # import the SBART functions
source("data/get_data.R") # import the sample data
source("data/sample_data.R") # import the sample data
source("R/common/check_data.R") # import the data checking functions
source("config.R") # import the configuration
source("R/common/tree_utilities.R") # import the plot_trees function
source("R/SBART/plot_trees.R") # import the SBART functions

# Load the data
data <- get_data()

# Check the data
# result <- check_data(data)
# if (result$error) {
#     stop(result$message)
# }

# Train the model
for (i in 1:100) {
    set.seed(i)

    tryCatch({
        model <- sbart(
            x = data$x_predictors,
            y = data$y,
            ws = data$ws,
            siam = data$wind_matrix,
            missing_indexes = data$missing_indexes,
            n_trees = n_trees,
            n_iterations = n_iterations,
            warmup = warmup
        )
        save(model, file = paste0("output/", model_filename, i, ".Rdata"))
        print("-----> FINISHED 🎉✨🥇")
    }, error = function(e) {})
}

# if (file.exists(paste0("output/", model_filename, ".Rdata"))) {
#     load(paste0("output/", model_filename, ".Rdata"))
#     plot_decision_trees(dt_list, ncol = 5)
# }
