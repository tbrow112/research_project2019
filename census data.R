#June 27th 2019
#Taylor Janet Brown

library(tidyverse)
library(tidycensus)

hmda <- read_csv("hdma_cenyear.RDS")

hmda <- hmda %>% filter(as_of_year > 2012,
                        loan_purpose_name == "Home purchase",
                        action_taken_name == "Loan originated")

# Join to tract data
hmda <- hmda %>% 
  mutate(tract = gsub("[.]","", census_tract_number)) # create 6 digit tract var

tract_meaninc <- tract_meaninc %>% 
  mutate(tract = str_sub(GEOID, 6,11)) # create 6 digit tract var

hmda_acs <- full_join(tract_meaninc, hmda, by = "tract") # join

