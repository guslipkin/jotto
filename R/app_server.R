#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  word <- shiny::reactiveVal(.pick_word())
  history <- shiny::reactiveVal(.create_history())

  shiny::observe({
    session$setCurrentTheme(
      if (isTRUE(input$dark_mode)) bslib::bs_theme(bootswatch = 'darkly') else bslib::bs_theme(bootswatch = 'flatly')
    )
  }) |>
    shiny::bindEvent(input$dark_mode)

  shiny::observe({
    .reset_game(word, history)
  }) |>
    shiny::bindEvent(input$new_game, ignoreNULL = TRUE, ignoreInit = TRUE)

  shiny::observe({
    shinyWidgets::show_alert(
      title = '',
      text = paste0('Your word is: ', toupper(word()), collapse = '')
    )
    .reset_game(word, history)
  }) |>
    shiny::bindEvent(input$give_up, ignoreNULL = TRUE)

  shiny::observe({
    if (input$congratulations) .reset_game(word, history)
  }) |>
    shiny::bindEvent(input$congratulations, ignoreNULL = TRUE, ignoreInit = TRUE)

  shiny::observe({
    shinyWidgets::show_alert(
      title = 'Help',
      text = htmltools::HTML(paste(
        'Jotto is a game where you try to guess the five letter word.',
        '<ul>',
        '<li>The word will have no repeating letters</li>',
        '<li>The word is not a proper noun</li>',
        '<li>You are only told how many letters your guess has in common with the word</li>',
        '</ul>',
        sep = ''
      )),
      html = TRUE,
      type = 'info'
    )
  }) |>
    shiny::bindEvent(input$help, ignoreNULL = TRUE)

  shiny::observe({
    guess <- input$guess
    if (!.check_word(guess) && !(guess %in% c('history', 'help', 'exit', ''))) {
      shiny::showNotification(
        ui = 'Please choose a word that is five letters, has no repeated letters, and is not a proper noun',
        type = 'message'
      )
    } else if (guess != '') {
      common <- .compare_guess(.split_word(word()), .split_word(guess))
      history(.add_to_history(history(), guess, common))
      shinyWidgets::updateCheckboxGroupButtons(
        inputId = 'letters',
        selected = unique(c(input$letters, .split_word(guess)))
      )
      if (common == 5 && guess == word()) {
        shinyWidgets::confirmSweetAlert(
          inputId = 'congratulations',
          title = 'CONGRATULATIONS!!!',
          text = paste('You guessed', toupper(word()), 'in', nrow(history()), 'guesses.', sep = ' '),
          type = 'success',
          btn_labels = c('Close', 'New Game')
        )
      }
    }
    shinyWidgets::updateTextInputIcon(inputId = 'guess', value = NA_character_)
  }) |>
    shiny::bindEvent(input$make_guess, input[['keyPressed']], ignoreNULL = TRUE, ignoreInit = TRUE)

  output$history <- shiny::renderTable(history())
}
