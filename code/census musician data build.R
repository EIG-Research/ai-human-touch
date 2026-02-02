
# remove dependencies
rm(list = ls())

library(dplyr)
library(tidyr)
library(readxl)
library(stringr)
library(ipumsr)
library(purrr)

#################
### Set paths ###
#################
# Define user-specific project directories
project_directories <- list(
  "name" = "PATH TO DIRECTORY",
  "sarah" = "/Users/sarah/Documents/GitHub/high-skill-worker-compete",
  "jiaxinhe" = "/Users/jiaxinhe/Documents/GitHub/high-skill-worker-compete",
)

# Setting project path based on current user
current_user <- Sys.info()[["user"]]
if (!current_user %in% names(project_directories)) {
  stop("Root folder for current user is not defined.")
}

path_project <- project_directories[[current_user]]
path_data <- file.path(path_project, "data")
path_output <- file.path(path_project, "output")

######################
##### DATA BUILD #####
######################

ddi_part1 <- read_ipums_ddi(file.path(path_data, "usa_00071.xml"))
census_music_part1 <- read_ipums_micro(ddi = ddi_part1, data_file = file.path(path_data, "usa_00071.dat"))
musician_music_teacher_sum_part1 <- census_music_part1 %>%
  filter(OCC1950 == 57,
         (YEAR %in% c(1850:1900, 1920) & LABFORCE == 2) |
           (YEAR %in% c(1910, 1930, 1940) & EMPSTATD %in% 10:12)) %>%
  group_by(YEAR) %>%
  summarise(emp_musician_teacher = sum(PERWT)) %>%
  ungroup()
remove(census_music_part1)

ddi_part2 <- read_ipums_ddi(file.path(path_data, "usa_00070.xml"))
census_music_part2 <- read_ipums_micro(ddi = ddi_part2, data_file = file.path(path_data, "usa_00070.dat"))
musician_music_teacher_sum_part2 <- census_music_part2 %>%
  filter((YEAR == 1950 & OCC1950 == 57)|
           (YEAR == 1960 & OCC == 120) |
           (YEAR == 1970 & OCC %in% c(123, 185)) |
           (YEAR == 1980 & OCC %in% c(137, 186)) |
           (YEAR == 1990 & OCC %in% c(137, 186))
           ) %>%
  group_by(YEAR) %>%
  summarise(emp_musician_teacher = sum(PERWT)) %>%
  ungroup()
musician_only_sum_part2 <- census_music_part2 %>%
  filter((YEAR == 1970 & OCC == 185) |
           (YEAR == 1980 & OCC == 186) |
           (YEAR == 1990 & OCC == 186) |
           (YEAR == 2000 & OCC == 275) |
           (YEAR == 2010 & OCC == 2750) |
           (YEAR %in% c(2019, 2024) & OCC %in% c(2751, 2752))) %>%
  group_by(YEAR) %>%
  summarise(emp_musician_only = sum(PERWT)) %>%
  ungroup()
remove(census_music_part2)

census_acs_musicians <- bind_rows(musician_music_teacher_sum_part1,
                                  musician_music_teacher_sum_part2,
                                  data.frame(YEAR = c(2000, 2010, 2019, 2024),
                                             emp_musician_teacher = NA)) %>%
  left_join(musician_only_sum_part2, by = "YEAR") %>%
  
  # extend series
  ungroup() %>% arrange(YEAR) %>%
  mutate(growth_musician_only = emp_musician_only/lag(emp_musician_only)) %>%
  arrange(YEAR) %>%
  mutate(
    emp_musician_teacher_filled = {
      out <- emp_musician_teacher
      for (i in seq_len(n())) {
        if (is.na(out[i]) && i > 1 && !is.na(out[i - 1])) {
          out[i] <- out[i - 1] * growth_musician_only[i]
        }
      }
      out
    }
  ) %>% dplyr::select(-c(emp_musician_teacher, growth_musician_only)) %>% rename(emp_musician_teacher = emp_musician_teacher_filled)
  
write.csv(census_acs_musicians,
          file = file.path(path_output, "Census Musician Count 1850-2024.csv"),
          row.names = FALSE)
