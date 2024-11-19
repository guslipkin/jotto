.make_word_list <- function() {
  # download the word list from http://wordlist.aspell.net/
  # unzip to the jotto/scowl
  # git2r::clone('https://github.com/en-wl/wordlist.git', 'scowl')
  # system(paste('cd ', getwd(), '/scowl && ./scowl word-list --size 80 > wl.txt', sep = ''))
  word_list <-
    'scowl/final' |>
    list.files(pattern = 'words', full.names = TRUE) |>
    tibble::tibble() |>
    `colnames<-`('file') |>
    dplyr::mutate(
      'difficulty' = .data$file |> stringi::stri_extract(regex = '\\d{2}') |> as.integer()
    ) |>
    purrr::pmap(\(file, difficulty) {
      f <- file |> readLines() |> iconv(to = 'UTF-8') |> tolower()
      f <- f[nchar(f) == 5]
      f <- f[grepl('\\w{5}', f)]
      f <- f[.check_word_duplicate_chars(f)]
      tibble::tibble('word' = f, 'difficulty' = difficulty)
    }, .progress = TRUE) |>
    purrr::list_rbind() |>
    dplyr::arrange(.data$difficulty, .data$word) |>
    dplyr::distinct()
  usethis::use_data(word_list, internal = TRUE, overwrite = TRUE)
}

.check_word_dictionary <- function(word) { word %in% word_list$word }

.check_word_length <- function(word) { nchar(word) == 5 }

.check_word_duplicate_chars <- function(word) { !grepl('.*?(\\w+).*\\1', word) }

.check_word <- function(word) {
  .check_word_length(word) & .check_word_duplicate_chars(word) & .check_word_dictionary(word)
}

.pick_word <- function(difficulty) {
  shinyWidgets::show_toast(
    title = 'Selecting a word',
    type = 'info',
    position = 'top-end'
  )
  difficulty_list <-
    word_list |>
    dplyr::filter(.data$difficulty <= .env$difficulty) |>
    dplyr::pull(.data$word)
  repeat {
    word <- sample(difficulty_list, 1)
    if (.check_word(word)) {
      shinyWidgets::show_toast(
        title = 'Your word has been selected',
        type = 'info',
        position = 'top-end'
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

.reset_game <- function(word, history, difficulty) {
  word(.pick_word(difficulty))
  history(.create_history())
  shinyWidgets::updateTextInputIcon(inputId = 'guess', value = NA_character_)
  shinyWidgets::updateCheckboxGroupButtons(inputId = 'letters', selected = '')
  shinyWidgets::updateCheckboxGroupButtons(inputId = 'scratch', selected = '')
}
