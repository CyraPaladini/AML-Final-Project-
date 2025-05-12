
library(readr)
library(dplyr)
library(ggplot2)
library(skimr)
library(stringr)


curvefeats <- read.csv("/Users/cyra/Documents/Applied Machine Learning/Final Project/python/curve_feats_counties.csv")
curvefeats <- curvefeats[, -which(grepl('2023', names(curvefeats)))]
svi <- read.csv("/Users/cyra/Documents/Applied Machine Learning/Final Project/SVI_2020_US_county.csv")
covid <- read.csv("/Users/cyra/Documents/Applied Machine Learning/Final Project/covid_confirmed_usafacts.csv")


glimpse(curvefeats)
skim(curvefeats)

# check missing values
cat("Missing values in curvefeats:\n")
colSums(is.na(curvefeats))

curvefeats <- curvefeats %>%
  mutate(
    county_clean = str_to_lower(str_replace_all(X, " County, | Parish, ", ", "))
  )

svi <- svi %>%
  mutate(
    county_clean = str_to_lower(str_replace_all(LOCATION, " County| Parish", ""))
  )

#need to visualize outliers 
ggplot(curvefeats, aes(x = married.slope_2022)) + 
  geom_histogram(bins = 30) +
  labs(title = "Distribution of Married Household Slopes", 
       x = "Annual Change in Married Households", 
       y = "Count")
#its la
summary(curvefeats$married.slope_2022)
outlier_county <- curvefeats[which.min(curvefeats$married.slope_2022), ]
print(paste("Extreme outlier:", outlier_county$X))

curvefeats_clean <- curvefeats %>%
  filter(X != "Los Angeles County, California") %>%
  mutate(across(where(is.numeric), ~ifelse(is.na(.), median(., na.rm=TRUE), .)))

#merge data
merged_data <- curvefeats_clean %>%
  left_join(svi, by = "county_clean") %>%
  mutate(FIPS = sprintf("%05d", as.numeric(FIPS)))


# convert FIPS to character with leading zeros so compatible across data
covid <- covid %>%
  mutate(countyFIPS = sprintf("%05d", countyFIPS))

date_cols <- grep("^X20", colnames(covid), value = TRUE)
last_date <- tail(date_cols, 1) #doing latest dates bc theyre in early 2023 and we're doing predictive with 2022 data 


covid_summary <- covid %>%
  select(countyFIPS, County.Name, State, all_of(last_date)) %>%
  rename(covid_cases = last_date)

merged_data <- merged_data %>%
  left_join(covid_summary, by = c("FIPS" = "countyFIPS")) %>%
  mutate(
    covid_cases_per_100k = (as.numeric(covid_cases) / E_TOTPOP) * 100000
  )
write.csv(merged_data, "/Users/cyra/Documents/Applied Machine Learning/Final Project/merged_data.csv", row.names = FALSE)
