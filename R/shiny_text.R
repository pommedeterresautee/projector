#' Interactive approximative search of embeddings (text)
#'
#' Shiny application to display closest embeddings to a query
#' User provides a pivot word and the n most similar word
#' are displayed.
#'
#' Word embeddings are retrieved from the provided [matrix] and averaged to get the embedding of the query.
#'
#' @param annoy_model [RcppAnnoy] model generated with [get_annoy_model]
#' @param word_embeddings_mat [matrix] containing embeddings of each word (used to generate the embedding of the query)
#' @param normalize_text [function] to clean the query before being transformed in embedding. Take a [character] as parameter and return a [character].
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
#' word_embeddings_mat <- get_word_vectors(model, words = head(get_dictionary(model), 2e5))
#' annoy_model <- get_annoy_model(word_embeddings_mat, 5)
#'
#' shiny_text(annoy_model = annoy_model,
#'            word_embeddings_mat = word_embeddings_mat,
#'            normalize_text = tolower)
#' }
#' @importFrom shiny textInput shinyApp tableOutput renderTable showNotification
#' @importFrom data.table data.table
#' @importFrom shinymaterial material_page material_row material_column material_card material_number_box
#' @importFrom assertthat assert_that is.string
#' @export
shiny_text <- function(annoy_model, word_embeddings_mat, normalize_text) {

  ui <- material_page(
    title = "Approx neighborhood search",

    material_row(
      material_column(
        width = 4,
        material_card(
          depth = 2,
          textInput("query_input",
                    label = "Query",
                    value = "",
                    width = NULL,
                    placeholder = "Enter query here"),

          material_number_box(input_id = "top_n",
                              min_value = 2,
                              max_value = 100,
                              initial_value = 20,
                              label = "Top N")
        )
      ),
      material_column(
        width = 7,
        material_card(
          title = "suggestions",
          tableOutput("table"),
          depth = 2
        )
      )
    )
  )

  server <- function(input, output, session) {

    projector_instance <- get_projector_instance(word_embeddings = word_embeddings_mat,
                                                 na_if_unknwown_word = TRUE)

    output$table <- renderTable({
      query_normalized <- normalize_text(input$query_input)
      assert_that(is.string(query_normalized))
      l <- try(get_neighbors_from_free_text(text = query_normalized,
                                            annoy_model = annoy_model,
                                            n = input$top_n,
                                            projector_instance = projector_instance,
                                            search_k = -1), silent = TRUE)
      if (class(l) != "try-error") {
        data.table(query = l$text,
                   distance = l$distance)
      } else {
        showNotification("This query is not present in the dataset used.")
        data.table(query = character(),
                   distance = character())
      }
    })
  }

  shinyApp(ui = ui, server = server)

}
