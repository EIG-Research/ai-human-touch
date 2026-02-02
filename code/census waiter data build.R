
# remove dependencies
rm(list = ls())

library(dplyr)
library(tidyr)
library(readxl)
library(stringr)
library(ipumsr)

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
census_waiter_part1 <- read_ipums_micro(ddi = ddi_part1, data_file = file.path(path_data, "usa_00071.dat"))
waiter_sum_part1 <- census_waiter_part1 %>%
  filter(OCC1950 == 784,
         (YEAR %in% c(1850:1900, 1920) & LABFORCE == 2) |
           (YEAR %in% c(1910, 1930, 1940) & EMPSTATD %in% 10:12)) %>%
  group_by(YEAR) %>%
  summarise(emp_waiter_teacher = sum(PERWT)) %>%
  ungroup()
remove(census_waiter_part1)

ddi_part2 <- read_ipums_ddi(file.path(path_data, "usa_00070.xml"))
census_waiter_part2 <- read_ipums_micro(ddi = ddi_part2, data_file = file.path(path_data, "usa_00070.dat"))
waiter_sum_part2 <- census_waiter_part2 %>%
  filter(OCC1990 == 435) %>%
  group_by(YEAR) %>%
  summarise(emp_waiter_teacher = sum(PERWT)) %>%
  ungroup()

census_acs_waiters <- bind_rows(waiter_sum_part1,waiter_sum_part2)

write.csv(census_acs_waiters,
          file = file.path(path_output, "Census Waiter Count 1850-2024.csv"),
          row.names = FALSE)
