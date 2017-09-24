#' Interactive exploration of embeddings
#'
#' Shiny application to interactively play with embeddings.
#' User provides a pivot word and the n most similar word
#' are projected on a scatter plot.
#'
#' @param annoy_model [RcppAnnoy] model generated with [get_annoy_model]
#' @importFrom shiny fluidPage textInput shinyApp sliderInput mainPanel titlePanel hr need validate h1 h2 h3 h4 h5 h6 numericInput selectInput sidebarPanel actionButton isolate observeEvent
#' @importFrom plotly renderPlotly plotlyOutput
#' @export
interactive_embedding_exploration <- function(annoy_model){

  ui <- fluidPage(title = "Text projectoR",
                  titlePanel(title = "Text projectoR"),
                  hr(),
                  sidebarPanel(
                    textInput("pivot_text", label = NULL, placeholder = "Pivot text"),
                    sliderInput("number_neighbors", label = h5("Number of neighbors (more == slower)"), min = 0, max =  annoy_model$getNItems(), value = 1000, ticks = TRUE, step = 100),
                    numericInput("min_size_cluster", label = h5("Minimum size of a cluster"), value = 3, step = 1),
                    selectInput("projection_algorithm", label = h5("Projection algorithm"), choices = c("PCA (rapid)" = "pca", "T-SNE (better, slower)" = "tsne"), selected = "tsne"),
                    actionButton("plot_button", "Plot!")
                  ),
                  mainPanel(
                    plotlyOutput("vector_neighbor_plot")
                  ))

  server <- function(input, output) {
    observeEvent(input$plot_button, {
      output$vector_neighbor_plot <- renderPlotly({

        isolate({
          validate(
            need(input$pivot_text != "", message = "Please, enter a word"),
            need(input$pivot_text %in% annoy_model@dict, message = "Provided text has not been included in the embeddings!")
          )
          df <- retrieve_neighbors(text = input$pivot_text, projection_type = input$projection_algorithm, annoy_model = annoy_model, n =  input$number_neighbors)
          plot_text(df, input$min_size_cluster)
        })
      })

    })
  }

  shinyApp(ui, server)
}
