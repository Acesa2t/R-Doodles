library(tidyverse)
library(readxl)
library(janitor)
library(forcats)

# Find number of excel sheets
length( excel_sheets( "2016 census stratified.xlsx") )


# Read in each page
sheet_census <- read_xlsx("2016 census stratified.xlsx", sheet = 2)

# Transpose the rows and columns

sheet_census_t <- as.data.frame(t(as.matrix(sheet_census)))

# Make the names of the first row the column names
# Keep copy of current transposed data frame in case of edits later
sheet_census_t <- sheet_census_t %>% row_to_names(row_number = 1)

# Delete first 3 rows, fix row name, delete unnecessary columns
sheet_census_f <- sheet_census_t %>% 
  filter(!`Place of birth (272)` %in% 
           c("Total - Immigrant status and period of immigration [2]", 
             "Non-immigrants [3]", "Immigrants [4]", 
             "Non-permanent residents [6]"))%>%
  select(-"Geography = Canada [1]", -"Global non-response rate (GNR) =   5.1 %")

class(sheet_census_f)
class(mtcars)
sheet_census_f[7,3] <- "2011 to 2016"

View(sheet_census_f)

# Export to Excel


y <- list(1, 2, 3, 4, 5, 6)
y[1][2]
