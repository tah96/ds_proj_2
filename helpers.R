library(tidyverse)
library(lubridate)

housing_data <- read_csv(file="data/Melbourne_Housing_FULL.csv",show_col_types = FALSE) %>%
  drop_na(Regionname,Type,Price,Date) %>%
  mutate(
    Date = dmy(Date)
  )

region_vector <- c("All",sort(unique(housing_data$Regionname[which(housing_data$Regionname != "#N/A")])))

type_vector <- c("House" = "h",
                 "Townhome" = "t",
                 "Unit/Apt" = "u")
