source("R/library_imports.R") # import the libraries
source("R/SBART/sbart.R") # import the SBART functions
source("R/SBART/plot_trees.R") # import the SBART functions
source("data/sample_data.R") # import the sample data
source("R/common/tree_utilities.R") # import the plot_trees function
source("R/common/get_model_score.R") # import the score function

ui <- dashboardPage(
    dashboardHeader(title = "SBART Model for spatial data"),
    dashboardSidebar(
        sidebarMenu(
            numericInput("n_trees", "Number of Trees:", 10, min = 1, max = 100),
            numericInput("n_iterations", "Number of Iterations:", 100, min = 1, max = 20000),
            numericInput("warmup", "Warmup:", 50, min = 1, max = 5000),
            actionButton("start", "Start")
        )
    ),
    dashboardBody(
        useShinyjs(),
        fluidRow(
            box(
                title = "Fitting model...",
                status = "primary",
                solidHeader = TRUE,
                width = 12,
                progressBar(
                    id = "pb",
                    value = 0,
                    total = 100,
                    title = "",
                    display_pct = TRUE
                )
            ),
            box(
                title = "Score plot",
                status = "primary",
                solidHeader = TRUE,
                width = 12,
                plotOutput("score_plot")
            ),
            box(
                title = "Final tree structures",
                status = "primary",
                solidHeader = TRUE,
                width = 12,
                tags$style(type = "text/css", "#treePlot { width: 100% !important; height: 600px !important; }"),
                uiOutput("trees")
            )
        )
    )
)

server <- function(input, output, session) {
    observeEvent(input$start, {
            
        # disable inputs
        shinyjs::disable("n_trees")
        shinyjs::disable("n_iterations")
        shinyjs::disable("warmup")

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
                    # Update tree structures
                    output$trees <- renderUI({
                        if (file.exists("output/model.RData")) {
                            load("output/model.RData")
                            plot_decision_trees(dt_list, ncol = 5)
                        }
                    })
                }
            }
        )


        # Get the scores
        scores <- sapply(2:input$n_iterations, function(i) {
            get_model_score(model$y_predictions_history[[i]], data$y_actual, data$missing_indexes)
        })

        # Plot the score
        output$score_plot <- renderPlot({
            plot(scores, type = "l", xlab = "Iteration", ylab = "Score")
        })

        save(model, file = "output/model.RData")
    })
}

shinyApp(ui = ui, server = server)
