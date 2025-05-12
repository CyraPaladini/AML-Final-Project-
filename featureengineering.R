
library(dplyr)
library(readr)

#outputs folder
if (!dir.exists("outputs")) dir.create("outputs")

merged_data <- read_csv("/Users/cyra/Documents/Applied Machine Learning/Final Project/merged_data.csv")


#feat engineering household volitility: sum of absolute changes in married and unmarried households
merged_data <- merged_data %>%
  mutate(
    household_volatility = abs(married.slope_2022) + abs(unmarried.slope_2022)
  )

# healthcare vulnerability: uninsurance + crowded housing (adjusted for NA)
merged_data <- merged_data %>%
  mutate(
    healthcare_vulnerability = coalesce(E_UNINSUR, 0) + coalesce(E_CROWD, 0)
  )


merged_data <- merged_data %>%
  mutate(
    z_POV150 = scale(E_POV150),
    z_AGE65 = scale(E_AGE65),
    z_UNINSUR = scale(E_UNINSUR),
    z_MINRTY = scale(E_MINRTY),
    z_healthcare_vulnerability = scale(healthcare_vulnerability),
    z_household_volatility = scale(household_volatility)
  )

# assign weights based on variable importance from model results
weights <- c(
  POV150 = 0.25,         
  AGE65 = 0.20,           
  UNINSUR = 0.15,
  MINRTY = 0.15,
 healthcare_vulnerability = 0.15,
  household_volatility = 0.10
)

# Calculate new priority score
merged_data <- merged_data %>%
  mutate(
    disease_risk_score = (
      z_POV150 * weights["POV150"] +
        z_AGE65 * weights["AGE65"] +
        z_UNINSUR * weights["UNINSUR"] +
        z_MINRTY * weights["MINRTY"] +
        z_healthcare_vulnerability * weights["healthcare_vulnerability"] +
        z_household_volatility * weights["household_volatility"]
    )
  )




#overall disease risk score (weighted combination)
#merged_data <- merged_data %>%
  #mutate(
    #disease_risk_score = (household_volatility * 0.4) + (healthcare_vulnerability * 0.6)
  #)


# final features for modeling
final_data <- merged_data %>%
  select(
    FIPS, county_clean, State,
    married.slope_2022, unmarried.slope_2022, household_volatility,
    E_POV150, E_UNINSUR, E_CROWD, E_NOVEH, E_MINRTY, E_AGE65,
    healthcare_vulnerability, disease_risk_score,
    covid_cases, covid_cases_per_100k
  )

final_data$disease_risk_score <- as.vector(final_data$disease_risk_score)
write_csv(final_data, "/Users/cyra/Documents/Applied Machine Learning/Final Project/final_data.csv")


# Rank counties by disease risk score and save top 100 for public health use
priority_counties <- final_data %>%
  arrange(desc(disease_risk_score)) %>%
  slice(1:100) %>%
  select(FIPS, county_clean, State, disease_risk_score, covid_cases_per_100k)

#is_list <- sapply(final_data, is.list)
#print(paste("List columns:", paste(names(final_data)[is_list], collapse=", ")))


write_csv(priority_counties, "outputs/priority_counties.csv")

