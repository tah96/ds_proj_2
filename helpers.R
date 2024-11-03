library(tidyverse)
library(lubridate)

housing_data <- read_csv(file="data/Melbourne_Housing_FULL.csv",show_col_types = FALSE) %>%
  drop_na(Regionname,Type,Price,Date,Rooms,Bathroom,SellerG,Method) %>%
  mutate(
    Date = dmy(Date)
  )

region_vector <- c("All",sort(unique(housing_data$Regionname[which(housing_data$Regionname != "#N/A")])))
room_vector <- c("All",sort(unique(housing_data$Rooms[which(housing_data$Rooms != "#N/A")])))
bathroom_vector <- c("All",sort(unique(housing_data$Bathroom[which(housing_data$Bathroom != "#N/A")])))
seller_vector <- c("All",sort(unique(housing_data$SellerG[which(housing_data$SellerG != "#N/A")])))

method_vector <- c(
  "Property Sold" = "S",
  "Property Sold Prior" = "SP",
  "Property Passed In" = "PI",
  "Sold Prior Not Disclosed" = "PN",
  "Sold Not Disclosed" = "SN",
  "No Bid" = "NB",
  "Vendor Bid" = "VB",
  "Withdrawn Prior to Auction" = "W",
  "Sold After Auction" = "SA",
  "Sold After Auction Price Not Disclosed" = "SS"
)

type_vector <- c("House" = "h",
                 "Townhome" = "t",
                 "Unit/Apt" = "u")
