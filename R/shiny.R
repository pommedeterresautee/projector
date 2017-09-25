#' Interactive exploration of embeddings
#'
#' Shiny application to interactively play with embeddings.
#' User provides a pivot word and the n most similar word
#' are projected on a scatter plot.
#'
#' For large list of texts, the autocomplete can be slow.
#'
#' Increasing the number of neighbors can make things very slow, in particular with T-SNE approach.
#' 1000 neighbors is usually a good value.
#'
#' Colors in the scatter plot represents clusters found by [dbscan].
#'
#' @param annoy_model [RcppAnnoy] model generated with [get_annoy_model]
#' @importFrom shiny fluidPage textInput shinyApp sliderInput mainPanel titlePanel hr need validate h1 h2 h3 h4 h5 h6 numericInput selectInput sidebarPanel actionButton isolate observeEvent conditionalPanel selectizeInput updateSelectizeInput
#' @importFrom plotly renderPlotly plotlyOutput
#' @export
interactive_embedding_exploration <- function(annoy_model){

  ui <- fluidPage(title = "Text projectoR",
                  titlePanel(title = "Text projectoR"),
                  hr(),
                  sidebarPanel(
                    selectizeInput("pivot_text", label = "Pivot text", choices = NULL),
                    sliderInput("number_neighbors", label = h5("Number of neighbors"), min = 0, max =  annoy_model$getNItems(), value = 1000, ticks = TRUE, step = 100),
                    numericInput("min_size_cluster", label = h5("Minimum size of a cluster"), value = 3, step = 1),
                    selectInput("projection_algorithm", label = h5("Projection algorithm"), choices = c("PCA (rapid)" = "pca", "T-SNE (better, slower)" = "tsne"), selected = "tsne"),
                    conditionalPanel(condition = "input.projection_algorithm == 'tsne'",
                      numericInput("epochs", label = h5("Number of epochs to train T-SNE"), value = 500, step = 10),
                      numericInput("perplexity", label = h5("Perplexity"), value = 30, step = 5, min = 1, max = 50)
                    ),
                    actionButton("plot_button", "Plot!")
                  ),
                  mainPanel(
                    plotlyOutput("vector_neighbor_plot")
                  ))

  server <- function(input, output, session) {
    # for the autocomplete in selectizeInput
    updateSelectizeInput(session = session, inputId = 'pivot_text', choices = annoy_model@dict, server = TRUE)

    observeEvent(input$plot_button, {
      output$vector_neighbor_plot <- renderPlotly({
        isolate({
          validate(
            need(input$pivot_text != "", message = "Please, enter a word"),
            need(input$pivot_text %in% annoy_model@dict, message = "Provided text has not been included in the embeddings!")
          )
          df <- retrieve_neighbors(text = input$pivot_text, projection_type = input$projection_algorithm, annoy_model = annoy_model, n =  input$number_neighbors, max_iter = input$epochs, perplexity = input$perplexity)
          plot_text(df, input$min_size_cluster)
        })
      })
    })
  }

  shinyApp(ui, server)
}
