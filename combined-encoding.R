
library(tidyverse)
library(readxl)
file_path <- 'XXXX' # replace with file path to combined governors data
df0 <- read_excel(file_path)

  # STEP 1: re-encode start/end year and term as integers ----
to_integer <- function(string) {
  ifelse(string == 'unknown', NA, as.integer(string))
}
df1 <- df0 %>%
  mutate(StartYear = map_int(StartYear, to_integer),
         EndYear = map_int(EndYear, to_integer),
         Term = map_int(Term, to_integer))

  # STEP 2: standardize fates ----
standardize_fate <- tribble(
  ~ Fate, ~ CleanFate,
  'KIA', 'Killed in Battle',
  'Killed in battle', 'Killed in Battle',
  'Non-violent fate', 'Non-violent Fate',
  'Non-Violent Fate', 'Non-violent Fate',
  'Non-violent fate/Willful violent removal',
      'Non-violent Fate/Willful Violent Removal',
  'unknown', 'Unknown',
  'Willful violent removal', 'Willful Violent Removal',
  'Willful Violent Removal', 'Willful Violent Removal',
  'Willful violent removal/Non-violent fate',
      'Non-violent Fate/Willful Violent Removal'
)
df2 <- df1 %>%
  left_join(standardize_fate, by = 'Fate') %>%
  select(-Fate) %>%
  rename(Fate = CleanFate) %>%
  mutate(Fate = ifelse(is.na(Fate), 'Unknown', Fate))
  
  # STEP 3: standardize ethnicity ----
standardize_ethnicity <- tribble(
  ~ Ethnicity, ~ CleanEthnicity,
  'Algerian', 'Non-Turkish',
  'European', 'Non-Turkish',
  'Kulughlu', 'Turkish',
  'Non-Ottoman', 'Non-Turkish',
  'Non-Turkish', 'Non-Turkish',
  'Ottoman', 'Ottoman',
  'Turkish', 'Turkish',
  'unknown', 'Unknown',
  'Unknown', 'Unknown'
)
df3 <- df2 %>%
  left_join(standardize_ethnicity, by = 'Ethnicity') %>%
  select(-Ethnicity) %>%
  rename(Ethnicity = CleanEthnicity) %>%
  mutate(Ethnicity = ifelse(is.na(Ethnicity), 'Unknown', Ethnicity))

  # STEP 4: standardize marriage information ----
standardize_marriage <- tribble(
  ~ Marriage_info, ~ Marriage,
  'FALSE', 'No',
  'no', 'No',
  'TRUE', 'Yes',
  'yes', 'Yes'
)
df4 <- df3 %>%
  left_join(standardize_marriage, by = 'Marriage_info') %>%
  select(-Marriage_info)
