## Get started

### Install dependencies

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

```R
 shiny::runApp()
```

### Run the tests
```
 renv::run("__tests__/run.R") 
```