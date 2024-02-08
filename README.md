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

#### Datasets

Remenber to put the datasets `AGC_Dataset_v_3_0_0.csv` and `Agrimonia_Dataset_v_3_0_0.csv` in the folder `data/AgrImOnIA/raw`.

## Tune it up

### Model parameters

You can tune the model by changing the parameters in the config.R file. The parameters are:

- `n_iterations`: number of iterations for the MCMC algorithm.
- `n_trees`: number of trees in the BART model.
- `warmup`: number of warmup iterations for the MCMC algorithm (See Kim article for more information).

Additionally, you can change other parameters like:
- `model_filename`: name of the file where the results will be saved.
- `date_begin`: starting date to cut the dataset.
- `date_end`: ending date to cut the dataset.
- `response_variable`: name of the response variable in the dataset.
- `covariates_of_interest`: names of the covariates in the dataset.

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




