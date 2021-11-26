
library(tidyverse)
library(readxl)

governors_file <- 'XXXX' # replace with file path to governors data
positions_file <- 'YYYY' # replace with file path to positions data
columns <- c('A', 'B', 'C') # optional: replace with names of columns to keep 

governors <- read_excel(governors_file, sheet = 1)
positions <- read_excel(positions_file, sheet = 1)

  # STEP 1 ----
    # re-formats the governors data so that each row contains a single previous
    # position; a 'row' column is added beforehand to uniquely identify
    # each row of data in the original format
get_individual_positions <- function(governors) {
  n <- max(vapply(governors$PreviousPositionsStandardized, function(x) {
    length(unlist(str_split(x, '; ')))
  }, integer(1)))
  governors %>%
      # unique identifier for each row
    mutate(row = row_number()) %>%
      # fill = 'right' turns off the warning message
    separate(PreviousPositionsStandardized,
             str_c('P', seq_len(n)), sep = '; ', fill = 'right') %>%
    pivot_longer(str_c('P', seq_len(n)), values_to = 'previous') %>%
      # NA values occurring in P1 -> no previous experience
    filter(!is.na(previous) | name == 'P1')
}

  # STEP 2 ----
    # replaces the 'previous' column from the output in step 1 with a 'position'
    # column which drops the details in the brackets
    # replaces the 'name' column from the output in step 1 with a
    # 'position_order' column, in which 1 indicates the earliest position
get_main_position_name <- function(governors) {
  governors %>%
      # get the index of the opening bracket '['
    mutate(index = map_int(previous, function(x) str_locate(x, '\\[')[1])) %>%
      # NA index values indicate no '[' was found, i.e. no details to remove
    mutate(position = ifelse(is.na(index), previous,
                             str_sub(previous, end = index - 2))) %>%
    mutate(position_order = as.integer(str_sub(name, start = 2))) %>%
    select(-name, -previous, -index)
}

  # STEP 3 ----
    # joins the output from step 2 with the positions data
    # optionally keeps select columns if a vector of names is provided
get_clean_labeled_data <- function(governors, columns = NA) {
  clean_labeled_data <- governors %>%
    left_join(positions %>%
                select(position = 'Title/Position',
                       classification = Classification),
              by = 'position')
  if (!is.na(columns))
    clean_labeled_data <- clean_labeled_data %>%
      select(row, all_of(columns), position, position_order, classification)
  return(clean_labeled_data)
}

  # PUTTING IT TOGETHER ----
governors %>%
  get_individual_positions() %>%
  get_main_position_name() %>%
    # remove 'columns = columns' argument if keeping all columns
  get_clean_labeled_data(columns = columns) %>%
    # for saving to a separate csv file
  write.csv('governors_with_position_labels.csv', row.names = FALSE)
