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
#' @importFrom shiny fluidPage textInput shinyApp sliderInput mainPanel titlePanel hr need validate h1 h2 h3 h4 h5 h6 numericInput selectInput sidebarPanel actionButton isolate observeEvent conditionalPanel selectizeInput updateSelectizeInput checkboxGroupInput div icon
#' @importFrom plotly renderPlotly plotlyOutput
#' @importFrom shinythemes shinytheme
#' @importFrom shinyjs hideElement useShinyjs showElement hidden
#' @export
interactive_embedding_exploration <- function(annoy_model){

  ui <- fluidPage(theme = shinytheme("superhero"),
                  useShinyjs(),
                  titlePanel(title = "projectoR", windowTitle = "projectoR"),
                  hr(),
                  sidebarPanel(
                    selectizeInput("pivot_text", label = h4("Pivot text"), choices = NULL),
                    sliderInput("number_neighbors", label = h5("Number of neighbors"), min = 0, max =  annoy_model$getNItems(), value = 1000, ticks = TRUE, step = 100),
                    numericInput("min_size_cluster", label = h5("Minimum size of a cluster"), value = 3, step = 1),
                    selectInput("projection_algorithm", label = h5("Projection algorithm"), choices = c("T-SNE (better, slower)" = "tsne", "PCA (rapid)" = "pca"), selected = "tsne"),
                    conditionalPanel(condition = "input.projection_algorithm == 'tsne'",
                      numericInput("epochs", label = h5("Number of epochs to train T-SNE"), value = 500, step = 10),
                      numericInput("perplexity", label = h5("Perplexity"), value = 30, step = 5, min = 1, max = 50)
                    ),
                    conditionalPanel(condition = "input.projection_algorithm == 'pca'",
                                     checkboxGroupInput(inputId = "pca_transformation", label = h5("Transformations"), choices = c("center", "scaled"), selected = c("center", "scaled"))
                    ),
                    actionButton("plot_button", "Plot!", class = "btn-primary")
                  ),
                  mainPanel(
                    div(id = "do_page", h1(icon("ban", "fa-5x"))),
                    hidden(div(id = "div_loading_message", h1("Computing..."))),
                    div(id = "div_plot", plotlyOutput("vector_neighbor_plot"))
                  ))

  server <- function(input, output, session) {
    # for the autocomplete in selectizeInput
    updateSelectizeInput(session = session, inputId = 'pivot_text', choices = annoy_model@dict, server = TRUE)

    observeEvent(input$plot_button, {
      hideElement("do_page")
      output$vector_neighbor_plot <- renderPlotly({
        isolate({
          hideElement("div_plot")
          showElement("div_loading_message", animType = "fade", anim = TRUE, time = 0.8)
          validate(
            need(input$pivot_text != "", message = "Please, choose a text.")
          )
          df <- retrieve_neighbors(text = input$pivot_text, projection_type = input$projection_algorithm, annoy_model = annoy_model, n =  input$number_neighbors, max_iter = input$epochs, perplexity = input$perplexity, transformations = input$pca_transformation)
          hideElement("div_loading_message")
          showElement("div_plot", animType = "fade", anim = TRUE, time = 0.8)
          plot_text(df, input$min_size_cluster)
        })
      })
    })
  }
  shinyApp(ui, server)
}
