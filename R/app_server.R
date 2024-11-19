#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  word <- shiny::reactiveVal(.pick_word(shiny::isolate(input$difficulty)))
  history <- shiny::reactiveVal(.create_history())

  shiny::observe({
    .reset_game(word, history, input$difficulty)
  }) |>
    shiny::bindEvent(input$new_game, input$difficulty, ignoreNULL = TRUE, ignoreInit = TRUE)

  shiny::observe({
    shinyWidgets::confirmSweetAlert(
      inputId = 'give_up_alert',
      title = '',
      text = paste0('Your word is: ', toupper(word()), collapse = ''),
      type = 'success',
      btn_labels = c('Close', 'New Game')
    )
    # .reset_game(word, history, input$difficulty)
  }) |>
    shiny::bindEvent(input$give_up, ignoreNULL = TRUE)

  shiny::observe({
    shinyWidgets::show_alert(
      title = 'Credits',
      text = htmltools::HTML(
        'This was R Shiny application was written by and is hosted by <a href="https://guslipkin.me">Gus Lipkin</a>.',
        '<br>',
        'The word list comes from <a href="http://wordlist.aspell.net/">SCOWL</a>.'
      ),
      html = TRUE
    )
  }) |>
    shiny::bindEvent(input$credits)

  shiny::observe({
    session$setCurrentTheme(
      if (isTRUE(input$dark_mode)) bslib::bs_theme(bootswatch = 'darkly') else bslib::bs_theme(bootswatch = 'flatly')
    )
  }) |>
    shiny::bindEvent(input$dark_mode)

  shiny::observe({
    if (input$give_up_alert) .reset_game(word, history, input$difficulty)
  }) |>
    shiny::bindEvent(input$give_up_alert)
  shiny::observe({
    if (input$congratulations) .reset_game(word, history, input$difficulty)
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
        '<li>The difficulty is described below by the dictionary\'s author:',
        '<blockquote cite="http://wordlist.aspell.net/scowl-readme/">Size 35 is the recommended small size, 50 the medium and 70 the large. Sizes 70 and below contain words found in most dictionaries while the 80 size contains all the strange and unusual words people like to use in word games such as Scrabble (TM).  While a lot of the words in the 80 size are not used very often, they are all generally considered valid words in the English language.  The 95 contains just about every English word in existence and then some.  Many of the words at the 95 level will probably not be considered valid English words by most people.</blockquote>',
        'In this case, small is equivalent to the "Easy" setting and large equivalent to "Hard".</li>',
        '</ul>',
        sep = ''
      )),
      html = TRUE,
      type = 'info'
    )
  }) |>
    shiny::bindEvent(input$help, ignoreNULL = TRUE)

  shiny::observe({
    guess <- input$guess |> tolower()
    guess <- sub('\\W', '', guess)
    if (!.check_word(guess) && !(guess %in% c('history', 'help', 'exit', ''))) {
      shinyWidgets::show_toast(
        title = 'Invalid guess',
        text = 'Please choose a word that is five letters, has no repeated letters, and is not a proper noun',
        type = 'info',
        position = 'top-end',
        timer = 5000
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
