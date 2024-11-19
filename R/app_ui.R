#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  htmltools::tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    bslib::page_sidebar(
      title = 'Jotto',
      theme = bslib::bs_theme(bootswatch = 'darkly'),
      sidebar = bslib::sidebar(
        open = FALSE,
        shiny::actionButton('new_game', label = 'New Game'),
        shiny::actionButton('give_up', label = 'Give Up'),
        shiny::actionButton('help', label = 'Help'),
        shinyWidgets::materialSwitch('dark_mode', label = 'Dark Mode', value = TRUE)
      ),
      bslib::layout_columns(
        col_widths = c(3, 9),
        bslib::card(
          bslib::card_body(
            shinyWidgets::textInputIcon('guess', label = NULL, placeholder = 'Guess'),
            shiny::actionButton('make_guess', label = 'Guess'),
            shinyjs::disabled(
              shinyWidgets::checkboxGroupButtons(
                inputId = 'letters',
                label = 'Used Letters',
                choices = letters,
                justified = TRUE,
                size = 'sm'
              )
            )
          )
        ),
        bslib::card(
          shiny::tableOutput('history')
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "jotto"
    ),
    htmltools::tags$style('* { font-family: "Courier"};'),
    htmltools::tags$style("
      #letters .btn-group-container-sw {
        display: flex;
        flex-wrap: wrap;
        gap: 5px; /* Optional: adds space between buttons */
      }
      #letters .btn-group-container-sw .btn {
        flex: 1 1 auto; /* Allows buttons to resize flexibly */
        max-width: 150px; /* Optional: set a maximum width for buttons */
        border-radius: 0 !important; /* Removes rounded corners */
      }
    "),
    htmltools::tags$script('
      $(document).on("keyup", function(e) {
        if(e.keyCode == 13) {
          Shiny.onInputChange("keyPressed", Math.random());
        }
      });
    '),
    shinyjs::useShinyjs(),
  )
}
