source("R/env_setup.R") # set up the environment
source("R/library_imports.R") # import the libraries
source("R/SBART/sbart.R") # import the SBART functions
source("data/sample_data.R") # import the sample data

# Load the data
data <- get_data()

with(data, {
  Xpred <- Xpred
  Y <- Y
  Ws <- Ws
  wind_mat <- wind_mat
  mis.ind <- mis.ind
})

# Train the model
model <- sbart.fit(
    X = Xpred,
    y.train = Y[-mis.ind], 
    missing_indexes = mis.ind,
    W = Ws, 
    SIAM = wind_mat,
    n.trees = 10L
)

# Make predictions
predictions <- sbart.predict(
    sbart.output = model, 
    X.test = data$Xpred[mis.ind],
    mis.ind
)

# TODO: Save the predictions to a file