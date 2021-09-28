# This document will be for re-formatting the countries into one column called country_of_origin and adding a column called "census_year" which in this case is 2016 
# May also be used to convert age groups into a set of year of births, or *median year*?


library(tidyverse)

# Read in Data

census <- read_xlsx("census_data3.xlsx")


# Remove unnecessary columns and clean up the others
census <- census%>%
  select(-total_placeofbirth, -`Born outside Canada`, -sex)

names(census)[names(census) == "Born in Canada"] <- 'Canada'

  # To get the indices of columns at the end of the dataset that need to be deleted
  # data.frame(colnames(census))

census <- census[,-c(272:303)]

# Attempt to use gather to solve my problem then create a bigger problem

census <- gather(census, key = "country_of_origin", value = "num_persons", 3:271)

# Use regular expression to remove some of the bracketed numbers
census$country_of_origin <- gsub("\\[|\\]|[0-9]", "", census$country_of_origin)

# I'm not convinced that I didn't create a bigger problem but this looks like it worked so I won't complain for now, I'll just move on

View(census%>%
  mutate(census_year = 2016))

# Write to Excel again
write_xlsx(census, path = "/Users/ajordan/OneDrive - McGill University/A TB/Analyses/TB prev measures/Census/census_additions.xlsx")

# Remaining problems: How to assign year_of_birth as a column, this same procedure will likely have to be edited and re-done for other census years, the country names may not be compatible with the countrycode function used within cleanseCensus
