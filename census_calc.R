library(tidyverse)
library(readxl)
library(writexl)
library(janitor)
library(taRifx)

# Find number of excel sheets
num_sheets <- length(excel_sheets("census_total.xlsx"))

# Define empty list
list_census_p2 <- list()

# Add each Excel sheet to the list as a data frame
for(x in 1:num_sheets){
  list_census_p2[[x]] <- read_xlsx("census_total.xlsx", sheet = x)
}

#View(list_census_p2)


# Split the data frame into two separate data frames
df1 <- map(list_census_p2, ~select(.x, age_group, sex, year_of_arrival))
#View(df1)

df2 <- map(list_census_p2, ~select(.x, -age_group, -sex, -year_of_arrival))
#View(df2[[2]])

for(x in 1:num_sheets){
# Convert all columns within every data frame to numeric

  df2[[x]] <- sapply(df2[[x]], as.numeric )
}

#View(df2[[1]])
# technically speaking could use some combo of assign and paste in order to make new data frames, but only 5 dataframes in the list so I'll just do this manually

# Divide second data frame by 10 or 6
df.divide1 <- df2[[1]]
df.divide2 <- df2[[2]]
df.divide3 <- df2[[3]]
df.divide4 <- df2[[4]]
df.divide5 <- df2[[5]]


df.divide1[2:31,1:303] <- df.divide1[2:31,1:303]/10
df.divide1[32:37,1:303] <- df.divide1[32:37,1:303]/6


df.divide2[2:31,1:303] <- df.divide2[2:31,1:303]/10
df.divide2[32:37,1:303] <- df.divide2[32:37,1:303]/6

df.divide3[2:31,1:303] <- df.divide3[2:31,1:303]/10
df.divide3[32:37,1:303] <- df.divide3[32:37,1:303]/6


df.divide4[2:31,1:303] <- df.divide4[2:31,1:303]/10
df.divide4[32:37,1:303] <- df.divide4[32:37,1:303]/6


df.divide5[2:31,1:303] <- df.divide5[2:31,1:303]/10
df.divide5[32:37,1:303] <- df.divide5[32:37,1:303]/6


# Back into list
z <- df1[[1]][3]

re_list <- list(df.divide1, df.divide2, df.divide3, df.divide4, df.divide5)
#re_list <- map(re_list, ~mutate(.x, year_of_arrival = z$year_of_arrival))
View(re_list)

list_census_calc2 <- list()
# Re-merge data frames-apparently the hard way. I'm not spending 4 hours troubleshooting this

View(dplyr::bind_cols(df1, re_list))

new_df1 <- cbind(df1[[1]], re_list[[1]])
new_df2 <- cbind(df1[[2]], re_list[[2]])
new_df3 <- cbind(df1[[3]], re_list[[3]])
new_df4 <- cbind(df1[[4]], re_list[[4]])
new_df5 <- cbind(df1[[5]], re_list[[5]])

census_data_clean <- bind_rows(new_df1,new_df2,new_df3,new_df4,new_df5)

write_xlsx(census_data_clean, path = "/Users/ajordan/OneDrive - McGill University/A TB/Analyses/TB prev measures/census_data_clean.xlsx")
