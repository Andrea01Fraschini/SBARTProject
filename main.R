source("R/env_setup.R") # set up the environment
source("R/library_imports.R") # import the libraries
source("R/SBART/sbart.R") # import the SBART functions
source("data/sample_data.R") # import the sample data

# Load the data
data <- sample_data()

# Train the model
model <- sbart(
    x = data$x_predictors,
    y = data$y,
    ws = data$ws,
    siam = data$wind_matrix,
    missing_indexes = data$missing_indexes,
    n_trees = 50L,
    n_iterations = 10000L,
    warmup = 1000L
)

save(model, file = "output/model.RData")
