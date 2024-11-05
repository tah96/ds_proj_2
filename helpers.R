library(tidyverse)
library(lubridate)

housing_data <- read_csv(file="data/Melbourne_Housing_FULL.csv",show_col_types = FALSE) %>%
  drop_na(Regionname,Type,Price,Date,Rooms,Bathroom,SellerG,Method) %>%
  mutate(
    Date = dmy(Date),
    Rooms = as.character(Rooms),
    Bathroom = as.character(Bathroom)
  )

region_vector <- c("All",sort(unique(housing_data$Regionname[which(housing_data$Regionname != "#N/A")])))
room_vector <- c("All",sort(unique(housing_data$Rooms[which(housing_data$Rooms != "#N/A")])))
bathroom_vector <- c("All",sort(unique(housing_data$Bathroom[which(housing_data$Bathroom != "#N/A")])))
seller_vector <- c("All",sort(unique(housing_data$SellerG[which(housing_data$SellerG != "#N/A")])))

cat_vars <- c(
  "Region" = "Regionname",
  "House Type" = "Type",
  "Real Estate Agent" = "SellerG",
  "Selling Method" = "Method",
  "Number Rooms" = "Rooms",
  "Number Bathrooms" = "Bathroom"
)

numeric_vars <- c(
  "Price" = "Price",
  "Land Size Meters" = "Landsize",
  "Building Area" = "BuildingArea"
)

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

## Functions

one_way_contingency <- function(data,column_name){
  column_sym <- sym(column_name)
  return_data <- data %>%
    drop_na(!!column_sym) %>%
    group_by(!!column_sym) %>%
    summarize(count=n())
  return(return_data)
}

two_way_contingency <- function(data,column_vec){
  if(length(column_vec) != 2){
    stop("You can only pass two columns when creating a two way contingency table")
  }
  column_a <- sym(column_vec[1])
  column_b <- sym(column_vec[2])
  return_data <- data %>%
    drop_na(!!column_a,!!column_b) %>%
    group_by(!!column_a,!!column_b) %>%
    summarize(count=n())
  return(return_data)
}

singleVarBar <- function(data, column_name){
  column_sym <- sym(column_name)
  plot <- ggplot(data %>%
                   drop_na(!!column_sym)
                 ,aes(x = !!column_sym)
                 ) +
    geom_bar() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = column_name, y = "Real Estate Sold", title = paste0("Number of Home Sales by ",column_name))
  return(plot)
}

fillVarBar <- function(data, column_name, fill_column){
  column_sym <- sym(column_name)
  fill_sym <- sym(fill_column)
  plot <- ggplot(data %>%
                   drop_na(!!column_sym,!!fill_sym)
                 ,aes(x = !!column_sym,fill = !!fill_sym)
  ) +
    geom_bar() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = column_name, y = "Real Estate Sold", title = paste0("Number of Home Sales by ",column_name, " and ", fill_column))
  return(plot)
}

summarizeNumeric <- function(data,numericVar,catVar="None"){
  if(length(numericVar) != 1){
    stop("Please enter at least one numeric variable")
  }
  numericSym <- sym(numericVar)
  catSym <- sym(catVar)
  
  if(catVar == "None"){
    return_data <- data %>%
      select(!!numericSym) %>%
      drop_na(!!numericSym) %>%
      summarize(across(everything(), .fns = list("mean" = mean,
                                                 "median" = median,
                                                 "var" = var,
                                                 "sd" = sd,
                                                 "IQR" = IQR), .names = "{.fn}_{.col}"))
  } else {
    return_data <- data %>%
      select(!!numericSym,!!catSym) %>%
      drop_na(!!numericSym,!!catSym) %>%
      group_by(!!catSym) %>%
      summarize(across(everything(), .fns = list("mean" = mean,
                                                 "median" = median,
                                                 "var" = var,
                                                 "sd" = sd,
                                                 "IQR" = IQR), .names = "{.fn}_{.col}"))
  }
  
  return(return_data)
}

generateScatter <- function(data,x_var,y_var,fill_var="None"){
  xSym <- sym(x_var)
  ySym <- sym(y_var)
  fillSym <- sym(fill_var)
  
  if(fill_var != "None"){
    return_plot <- ggplot(data %>%
                            drop_na(!!xSym,!!ySym,!!fillSym),
                          aes(x = !!xSym,y= !!ySym, color=!!fillSym)) +
      geom_point(position="jitter") +
      labs(x = x_var, y = y_var, title = paste0(x_var," and ", y_var, "scatter plot colored by", fill_var))
  } else {
    return_plot <- ggplot(data %>%
                            drop_na(!!xSym,!!ySym),
                          aes(x = !!xSym,y= !!ySym)) +
      geom_point(position="jitter") +
      labs(x = x_var, y = y_var, title = paste0(x_var," and ", y_var, "scatter plot"))
  }
  return(return_plot)
}

ggplot(housing_data %>%
         mutate(Type = ifelse(Type == "h","Household",ifelse(Type =="t","Townhome","Unit/Apt"))
         ) %>%
         drop_na(Landsize,BuildingArea,Type),
       aes(x = Landsize, y = BuildingArea, color = Type)) +
  geom_point(position="jitter") +
  xlim(0,10000) +
  ylim(0,5000) +
  labs(x = "Land Size (Meters)", y= "Building Area (Meters)", title = "Land Size and Building area colored by house Type")
