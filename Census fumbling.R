library(tidyverse)
library(readxl)
library(janitor)
library(writexl)

# Find number of excel sheets
nsheets <- length( excel_sheets( "2016 census stratified.xlsx") )

# Read in each page, save inside a list?

# Define empty list
list_census <- list()

# Add each Excel sheet to the list as a data frame
for(x in 1:nsheets){
  list_census[[x]] <- read_xlsx("2016 census stratified.xlsx", sheet = x)
}

class(list_census)

# Transpose the rows and columns

l1 <- map(list_census, as.matrix)
l2 <- map(l1, t)
list_census_t  <- map(l2, as.data.frame)
list_census_t

# Make the names of the first row the column names
# Keep copy of current transposed data frame in case of edits later
list_census_t2 <- map(list_census_t, row_to_names, row_number = 1)

# Delete unnecessary rows and columns

lc2 <- map(list_census_t2,
  ~filter(.x, !`Place of birth (272)` %in% 
           c("Total - Immigrant status and period of immigration [2]", 
             "Non-immigrants [3]", "Immigrants [4]", 
             "Non-permanent residents [6]"))%>%
  select(-"Geography = Canada [1]", -"Global non-response rate (GNR) =   5.1 %"))

# Delete first 3 rows, fix row name, delete unnecessary columns

for(i in 1:nsheets){
  lc2[[i]][7,3] <- "2011 to 2016"
}


View(lc2[[2]])

#Export back into Excel

