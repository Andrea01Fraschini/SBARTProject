source("R/library_imports.R") # import the libraries
source("R/SBART/sbart.R") # import the SBART functions
source("data/sample_data.R") # import the sample data
source("R/common/tree_utilities.R") # import the plot_trees function

ui <- fluidPage(
  useShinyjs(), # Enable shinyjs
  titlePanel("SBART Model for spatial data"),
  sidebarPanel(
    numericInput("n_trees", "Number of Trees:", 10, min = 1, max = 100),
    numericInput("n_iterations", "Number of Iterations:", 100, min = 1, max = 20000),
    numericInput("warmup", "Warmup:", 50, min = 1, max = 5000),
    actionButton("start", "Start")
  ),
  titlePanel("Fitting model..."),
  progressBar(
    id = "pb",
    value = 0,
    total = 10000,
    title = "",
    display_pct = TRUE
  ),
  DT::dataTableOutput("table")
)

server <- function(input, output, session) {
  DF1 <- reactiveValues(data = as.data.frame(matrix(nrow = 1, ncol = 3)))
  started <- FALSE
  length_printed <- 0

  ## Displays Initial Table
  output$table <- DT::renderDataTable(DT::datatable({
    DF1$data
  }))

  observeEvent(input$start, {
    if (started) {
      started <<- FALSE
      shinyjs::runjs("$('#start').text('Start');")
      return() # TODO: NOT WORKING
    } else {
      started <<- TRUE
      shinyjs::runjs("$('#start').text('Stop');")
      temp <- as.data.frame(matrix(nrow = as.integer(input$n_trees), ncol = 3))
      names(temp) <- c("#", "depth", "mu_s")
      DF1$data <- temp
    }

    # Load the data
    data <- sample_data()

    # Train the model
    model <<- sbart(
      x = data$x_predictors,
      y = data$y,
      ws = data$ws,
      siam = data$wind_matrix,
      missing_indexes = data$missing_indexes,
      n_trees = as.integer(input$n_trees),
      n_iterations = as.integer(input$n_iterations),
      warmup = as.integer(input$warmup),
      progress = function(iteration, dt_list) {
        updateProgressBar(
          session = session,
          id = "pb",
          value = iteration,
          total = input$n_iterations,
          title = paste("Process", trunc(iteration / input$n_iterations))
        )
        # Update table with tree structures

        if (iteration == input$n_iterations) {
          temp <- DF1$data
          for (i in 1:as.integer(input$n_trees)) {
            temp[i, 1] <- i
            temp[i, 2] <- "comming soon"
            mu_values <- dt_list[[i]]$mu
            mu_string <- paste(mu_values[1], mu_values[2], mu_values[3], sep = ", ")
            temp[i, 3] <- mu_string
          }

          DF1$data <- temp
        }
      }
    )

    save(model, file = "output/model.RData")
  })
}

shinyApp(ui = ui, server = server)
