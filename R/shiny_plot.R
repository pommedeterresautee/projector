#' Interactive exploration of embeddings
#'
#' Shiny application to interactively play with embeddings.
#' User provides a pivot word and the n most similar word
#' are projected on a scatter plot.
#'
#' For large list of texts, the auto-complete can be slow.
#'
#' Increasing the number of neighbors can make things very slow,
#' in particular with `T-SNE` approach.
#' 500 neighbors is usually a good value to have an understanding
#' of the neighborhood of a vector.
#'
#' Colors in the scatter plot represents clusters found by [dbscan].
#'
#' @param annoy_model [RcppAnnoy] model generated with [get_annoy_model]
#' @param default_number_neighbors set the number of neighbors slider to this value
#' @param default_number_rounds set the number of `T-SNE` rounds slider to this value
#' @param default_perplexity set the `T-SNE` perplexity slider to this value
#' @examples
#' if(interactive()){
#' # This example should be run with a higher quality model
#' # than the one embedded in fastrtext
#'
#' library(projector)
#' library(fastrtext)
#'
#' model_test_path <- system.file("extdata",
#'                                "model_unsupervised_test.bin",
#'                                package = "fastrtext")
#'
#' model <- load_model(model_test_path)
#' word_embeddings <- get_word_vectors(model, words = head(get_dictionary(model), 2e5))
#' annoy_model <- get_annoy_model(word_embeddings, 5)
#'
#' interactive_embedding_exploration(annoy_model)
#' }
#' @importFrom shiny shinyApp need validate selectInput sidebarPanel isolate observeEvent conditionalPanel
#' @importFrom shinymaterial material_page material_row material_column material_card material_number_box material_slider material_button material_dropdown material_text_box material_checkbox
#' @importFrom plotly renderPlotly plotlyOutput
#' @importFrom shinythemes shinytheme
#' @export
interactive_embedding_exploration <- function(annoy_model, default_number_neighbors = 100, default_number_rounds = 500, default_perplexity = 30){

  ui <- material_page(title = "projectoR",
                      material_row(
                        material_column(
                          width = 4,
                          material_card(
                            depth = 2,
                            material_text_box("pivot_text", 
                                              label = "Pivot text"),
                            material_slider("number_neighbors", 
                                            label = "Number of neighbors", 
                                            min = 0, 
                                            max = annoy_model$getNItems(), 
                                            initial_value = default_number_neighbors),
                            material_number_box("min_size_cluster", 
                                                label = "Minimum size of a cluster", 
                                                initial_value = 3, 
                                                min_value = 2, 
                                                max_value = 100),
                            material_dropdown("projection_algorithm", 
                                              label = "Projection algorithm", 
                                              choices = c("T-SNE (better, slower)" = "tsne", "PCA (rapid)" = "pca"), 
                                              selected = "tsne"),
                            conditionalPanel(condition = "input.projection_algorithm == 'tsne'",
                                             material_number_box("epochs", 
                                                                 label = "Epochs", 
                                                                 initial_value = default_number_rounds, 
                                                                 min_value = 2, 
                                                                 max_value = 1000),
                                             material_number_box("perplexity", 
                                                                 label = "Perplexity", 
                                                                 initial_value = default_perplexity, 
                                                                 min_value = 1, 
                                                                 max_value = 50)
                            ),
                            conditionalPanel(condition = "input.projection_algorithm == 'pca'",
                                             material_checkbox("material_checkbox_center", "center"),
                                             material_checkbox("material_checkbox_scaled", "scaled")
                                             
                            ),
                            material_button("plot_button", "Plot!")
                          )
                        )
                      ,
                      material_column(
                        width = 7,
                        material_card(
                          depth = 2,
                          plotlyOutput("vector_neighbor_plot"))
                      )))

  server <- function(input, output, session) {

    observeEvent(input$plot_button, {
      output$vector_neighbor_plot <- renderPlotly({
        isolate({
          # display waiting message
          validate(
            need(input$pivot_text != "", message = "Please, choose a text.")
          )
          
          pca_transfo <- character()
          if (input$material_checkbox_center) pca_transfo <- c(pca_transfo, "center")
          if (input$material_checkbox_scaled) pca_transfo <- c(pca_transfo, "scaled")
          
          df <- retrieve_neighbors(text = input$pivot_text, 
                                   projection_type = input$projection_algorithm, 
                                   annoy_model = annoy_model, 
                                   n =  input$number_neighbors, 
                                   max_iter = input$epochs, 
                                   perplexity = input$perplexity, 
                                   transformations = pca_transfo)
          plot_texts(df, input$min_size_cluster)
        })
      })
    })
  }
  shinyApp(ui, server)
}
