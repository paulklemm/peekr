# Hello, world!
#
# This is an example function named 'hello' 
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

#' Get overview over the data using the skimr packages
#'
#' @param dat Dataframe for previewing
#' @param digits: Number of digits for the kable tbale
#' @export
#' @import skimr magrittr
skim_formatted <- function(dat, digits = 4) {
  dat %>% skim() %>% skimr::kable(digits = digits)
}

#' Get the middle n rows of the dataframe
#'
#' @param dat Table to peek
#' @param n Number of rows to peek
#' @export
#' @import magrittr
peek_middle <- function(dat, n = 10) {
  n_rows <- dat %>% nrow()
  middle_row <- round(n_rows / 2)
  # Get min and max row indices
  min_val <- middle_row - floor(n / 2)
  max_val <- middle_row + floor(n / 2)
  # Check if middle is out of bounds
  if (min_val < 1) { min_val <- 1 }
  if (max_val > n_rows) { max_val <- n_rows }
  # Get middle rows
  dat[min_val:max_val, ] %>%
    return()
}

#' Get n random rows of the dataframe sorted by row index
#'
#' @param dat Dataframe for row peeking
#' @param n Number of random rows to peek
#' @return Dataframe of peeked elements
#' @export
#' @import magrittr
peek_random <- function(dat, n = 10) {
  n_rows <- dat %>% nrow()
  if (n > n_rows) {
    paste0("Elements to peek ", n, " exceed amount of rows (", n_rows, "). Setting n to ", n_rows) %>%
      warning()
    n <- n_rows
  }
  # Get random row ids
  random_rowids <-  sample(1:n_rows, n, replace = FALSE) %>%
    sort()
  # Return random rows
  dat[random_rowids, ] %>%
    return()
}

#' Peek into the data with visdat. If the table is too big you can set how to subset the data frame with `peek_mode`
#'
#' @param dat Table to peek
#' @param max_rows Max rows to peek and visualize
#' @param peek_mode Peek mode if number of rows in `dat` `exceeds max_rows`. Must be one of `head`, `tail`, `middle`, `random`
#' @export
#' @import magrittr visdat
peek_visdat <- function(dat, max_rows = 1000, peek_mode = "head") {
  supported_peek_modes <- "head, tail, middle, random"
  n_rows <- dat %>% nrow()
  # Number of rows in dataframe increase number of rows to peek
  if (n_rows > max_rows) {
    paste0("Elements to peek ", max_rows, " exceed amount of rows in the table (", n_rows, "). Peeking with mode ", peek_mode, ". Supported peek modes are ", supported_peek_modes) %>%
      warning()
    if (peek_mode == "head") {
      dat <- dat %>% head(n = max_rows)
    } else if(peek_mode == "tail") {
      dat <- dat %>% tail(n = max_rows)
    } else if(peek_mode == "middle") {
      dat <- dat %>% peek_middle(n = max_rows)
    } else if(peek_mode == "random") {
      dat <- dat %>% peek_random(n = max_rows)
    } else {
      # Handle not supported peek mode
      paste0("Mode ", peek_mode, "is not supported. Try one of ", supported_peek_modes) %>%
        stop()
    }
  }
  dat %>% vis_dat()
}