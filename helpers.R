library(tidyverse)

housing_data <- read_csv(file="data/Melbourne_Housing_FULL.csv",show_col_types = FALSE)

region_vector <- sort(unique(housing_data$Regionname[which(housing_data$Regionname != "#N/A")]))
