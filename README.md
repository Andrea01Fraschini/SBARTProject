# SBART Implementation on AgrImOnIA dataset

Group project for the Bayesian Statistics course in POLIMI (Politecnico di Milano) A.A. 2023/2024.

## Main goal

Apply the model in [Kim (2022)](https://doi.org/10.1080/00949655.2022.2102633), using the R code there for MCMC, for the analysis and prediction of air pollution data in Lombardy ([AGRIMONIA data](https://github.com/AgrImOnIA-project/AgrImOnIA_Data))

## Get started

To run the project you need to have R installed on your machine. You can download it from [here](https://www.r-project.org/). Additionally, you need to install the R package manager [renv](https://rstudio.github.io/renv/articles/renv.html).

### Install dependencies

To install the dependencies you need to run the following command in the R console:

```
 renv::restore()
```

### Run the project

You can either run the console version by executing "main.R" file or Run the more user friendly version.

#### Console version

```
 renv::run("main.R") 
```

#### UI version

```
 shiny::runApp()
```

## Tune it up

### Model parameters

You can tune the model by changing the parameters in the config.R file. The parameters are:

- `n_iterations`: number of iterations for the MCMC algorithm.
- `n_trees`: number of trees in the BART model.
- `warmup`: number of warmup iterations for the MCMC algorithm (See Kim article for more information).

> **Note:** Through the UI version you can change the parameters in the form.

### Data

Your data must be in a .csv file inside the `data` folder. The name of the file can be changed in the config.R file.

Further tweaks can be done in the `get_data.R` file, situated in the `data` folder.

> **Note:** Before the model is run a check is done to see if the data is in the correct format. The error message will be shown in the console.

#### get_data.R

Here you can change the data to be used in the model. Here you must explicitly define the following variables:
    - `y`: the response variable.
    - `x_predictors`: the covariates.
    - `wind_matrix`: the wind matrix (For more information see Andrea `README.md` documentation in `python` folder).

#### processed AgrImOnIA dataset

The result of the process using the grid is stored in the folder `data/AgrImOnIA/processed` with the name of `df.csv`.
To run the script and obtain the data follow the readme contained in `data/AgrImOnIA` folder.

### Results

When the model is run, the results are saved in the `output` folder. The results consist of:
    - `covariates_selection_chain`: covariates selection through the iterations.
    - `spatial_theta_chain`: spatial theta through the iterations.
    - `sigma2_chain`: sigma2 through the iterations.
    - `trees_chain`: trees through the iterations.
    - `w_selection_chain`: Weight matrix selection through the iterations.
    - `dt_history`: Tree structures history.
    - `y_predictions`: Final prediction history.
    - `y_predictions_history`: Predictions history.

> The information will be saved in a .RData file, the name of the file can be changed in the config.R file.




