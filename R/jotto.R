.make_word_list <- function() {
  # download the word list from http://wordlist.aspell.net/
  # unzip to the jotto/scowl
  word_list <-
    'scowl/final' |>
    list.files(pattern = 'words', full.names = TRUE) |>
    purrr::map(\(f) {
      f <- f |> readLines() |> iconv(to = 'UTF-8') |> tolower()
      f <- f[nchar(f) == 5]
      f <- f[grepl('\\w{5}', f)]
      f <- f[.check_word_duplicate_chars(f)]
      return(f)
    }, .progress = TRUE) |>
    unlist() |>
    sort()
  usethis::use_data(word_list, internal = TRUE, overwrite = TRUE)
}

.check_word_dictionary <- function(word) { word %in% word_list }

.check_word_length <- function(word) { nchar(word) == 5 }

.check_word_duplicate_chars <- function(word) { !grepl('.*?(\\w+).*\\1', word) }

.check_word <- function(word) {
  .check_word_length(word) & .check_word_duplicate_chars(word) & .check_word_dictionary(word)
}

.pick_word <- function() {
  shiny::showNotification(ui = 'Selecting a word', id = 'selecting_word')
  repeat {
    word <- sample(word_list, 1)
    if (.check_word(word)) {
      shiny::removeNotification(id = 'selecting_word')
      shiny::showNotification(
        ui = 'Your word has been selected',
        type = 'message'
      )
      return(word)
    }
  }
}

.split_word <- function(word) {
  word |>
    tolower() |>
    strsplit('') |>
    unlist()
}

.compare_guess <- function(split_word, split_guess) { sum(split_guess %in% split_word) }

.create_history <- function() {
  tibble::tibble(
    'round' = integer(),
    'guess' = character(),
    'common' = integer()
  )
}

.add_to_history <- function(history, guess, common) {
  tibble::tibble(
    'round' = if (nrow(history) == 0) 1L else max(history$round, na.rm = TRUE) + 1L,
    'guess' = guess,
    'common' = common
  ) |>
    dplyr::bind_rows(history) |>
    dplyr::filter(
      dplyr::n() == dplyr::row_number(),
      .by = 'guess'
    )
}

.reset_game <- function(word, history) {
  word(.pick_word())
  history(.create_history())
  shinyWidgets::updateTextInputIcon(inputId = 'guess', value = NA_character_)
  shinyWidgets::updateCheckboxGroupButtons(inputId = 'letters', selected = '')
}
