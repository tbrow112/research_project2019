# Reading in the HDMA data #
# June 12th 2019 

# load my libraries 
# libraries are the same thing as "SSC INSTALL" for stata 

# read in the data 

# giving a name to the data & pulling it in with "read_csv"

hdma07 <- read_csv("raw/hdma/hmda_lar.csv")
hdma08 <- read_csv("raw/hdma/hmda_lar (1).csv")
hdma09 <- read_csv("raw/hdma/hmda_lar (3).csv")
hdma10 <- read_csv("raw/hdma/hmda_lar (4).csv")
hdma11 <- read_csv("raw/hdma/hmda_lar (5).csv")
hdma12 <- read_csv("raw/hdma/hmda_lar (6).csv")
hdma13<- read_csv("raw/hdma/hmda_lar (7).csv")
hdma14 <- read_csv("raw/hdma/hmda_lar (8).csv")
hdma15 <- read_csv("raw/hdma/hmda_lar (9).csv")
hdma16 <- read_csv("raw/hdma/hmda_lar (10).csv")
hdma17 <- read_csv("raw/hdma/hmda_lar (11).csv")

#bind rows is the same things as appending 
#list allows us to do more than two at a time 
hdma_cville <- bind_rows(list(hdma07,hdma08,hdma09,hdma10,
                       hdma11,hdma12,hdma13,hdma14,hdma15,
                       hdma16,hdma17))
hmda_alb <- read_csv("raw/hdma/hmda_lar (12).csv")

hmda <- bind_rows(list(hdma_cville,hmda_alb))
#summarize the data
summary(hdma)

# ctrl shift m makes " %>% "
# trying to summarize only the specific data 

#summary(select(hdma, as_of_year, population, minority_population, loan_amount_000s))

# cleaning the data 
## making categorical data into factors ## 
#table(hdma$action_taken_name)

hmda <- hmda %>% 
  mutate(action_taken_name = factor(action_taken_name),
         applicant_race_name_1 = factor(applicant_race_name_1))

summary(select(hmda, action_taken_name))

# vector of column names 
var <- names(hmda) 
# keep just column names with "_name" in them
var <- str_subset(var,"_name") 
# mutate_at will use the list in 'var' amd apply the transformation in the second argument(here, 'as.factor') to the list 

hmda <- hmda %>% mutate_at(var, as.factor)
# making state_abbr & sequence_number into factor variables 
hmda <- hmda %>% 
  mutate(state_abbr = factor(state_abbr),
         sequence_number = factor(sequence_number))

# trying to make a scatter plot recreation
library(ggplot2)
str(hmda)
ggplot(hmda, aes(x=applicant_income_000s,
                 y=loan_amount_000s)) + geom_point(shape=1, alpha = 0.05) +
  
  geom_smooth(method=lm) +
  
  scale_x_continuous(labels = scales::dollar, name = "Applicant Income in 1,000's") +
  
  scale_y_continuous(labels = scales::dollar, name = "Loan Amount in 1,000's", limits=c(0,5000)) +
  
  facet_wrap(~as_of_year) +
  
  theme(axis.text = element_text(face="italic", color="midnightblue"),
        
        axis.text.x = element_text(angle=90, vjust=0.5))


# bar graph of cencus tract and year 
counts <- table(hmda$census_tract_number, hmda$as_of_year)
barplot(counts, main="TITLE GOES HERE", xlab="year", ylab='number of loan applications')

# Making a table
# %>% is to pipe the data into the next line 
# make a bar plot of hmda_cenyear by the census_tract_number facet this by year 
loanmade <- hmda %>%
  filter(action_taken_name == "Loan originated" & loan_purpose_name =="Home purchase") %>%
  group_by(census_tract_number, as_of_year) %>%
  summarize(loansmade = n())

# table 2 number of refinances by census tract year 
finance <- hmda %>% 
  filter(action_taken_name == "Loan originated" & loan_purpose_name =="Refinancing") %>%
  group_by(census_tract_number, as_of_year) %>% 
  summarize(finance = n())
#table 3 total loan apps denied 
denied <- hmda %>% 
  filter(action_taken_name == "Application denied by financial institution" & loan_purpose_name =="Home purchase") %>%
  group_by(census_tract_number, as_of_year) %>% 
  summarize(denied = n())
#table 4 total loans made to a white applicant 
whiteloan <- hmda %>%
  
  filter(action_taken_name == "Loan originated" & loan_purpose_name =="Home purchase" & applicant_race_name_1 == "White") %>%
  
  group_by(census_tract_number, as_of_year) %>%
  
  summarize(whiteloan = n())

#table 5 total loans made to a black applicant 
blackloan <- hmda %>%
  
  filter(action_taken_name == "Loan originated" & loan_purpose_name =="Home purchase" & applicant_race_name_1 == "Black or African American") %>%
  
  group_by(census_tract_number, as_of_year) %>%
  
  summarize(blackloan = n())

# table 6 number of refinance loans made to white applicant 
refinanceloanw <- hmda %>% 
  filter(action_taken_name == "Loan originated" & loan_purpose_name =="Refinancing" & applicant_race_name_1 == "White") %>%
  group_by(census_tract_number, as_of_year) %>% 
  summarize(refinanceloanw = n())

# table 7 number of refinance loans made to white applicant 
refinanceloanb <- hmda %>% 
  filter(action_taken_name == "Loan originated" & loan_purpose_name =="Refinancing" & applicant_race_name_1 == "Black or African American") %>%
  group_by(census_tract_number, as_of_year) %>% 
  summarize(refinanceloanb = n())

#table 8 number of loans denied to a white applicant 
deniedw <- hmda %>% 
  filter(action_taken_name == "Application denied by financial institution" & loan_purpose_name =="Home purchase" & applicant_race_name_1 == "White") %>%
  group_by(census_tract_number, as_of_year) %>% 
  summarize(deniedw = n())

#table 9 number of loans denied to a black applicant 
deniedb <- hmda %>% 
  filter(action_taken_name == "Application denied by financial institution" & loan_purpose_name =="Home purchase" & applicant_race_name_1 == "Black or African American") %>%
  group_by(census_tract_number, as_of_year) %>% 
  summarize(deniedb = n())
#table 10 median loan amount 
medianloan <- hmda %>%
  filter(action_taken_name == "Loan originated" & loan_purpose_name =="Home purchase") %>%
  group_by(census_tract_number, as_of_year) %>%
  summarize(medianloan = median(loan_amount_000s, na.rm = TRUE))
#table 11 median applicant income 
medianincome <- hmda %>%
  filter(action_taken_name == "Loan originated" & loan_purpose_name =="Home purchase") %>%
  group_by(census_tract_number, as_of_year) %>%
  summarize(medianincome = median(applicant_income_000s, na.rm = TRUE))
#table 12 median loan purchase amount by white applicant 
medianincomew <- hmda %>%
  filter(action_taken_name == "Loan originated" & loan_purpose_name =="Home purchase" & applicant_race_name_1 == "White") %>%
  group_by(census_tract_number, as_of_year) %>%
  summarize(medianincomew = median(applicant_income_000s, na.rm = TRUE))
#table 12 median loan purchase amount by black applicant 
medianincomeb <- hmda %>%
  filter(action_taken_name == "Loan originated"  & loan_purpose_name =="Home purchase" & applicant_race_name_1 == "Black or African American") %>%
  group_by(census_tract_number, as_of_year) %>%
  summarize(medianincomewb = median(applicant_income_000s, na.rm = TRUE))
# table 13 median refinance loan amount by white applicant 
medrefinanceloanw <- hmda %>% 
  filter(action_taken_name == "Loan originated" & loan_purpose_name =="Refinancing" & applicant_race_name_1 == "White") %>%
  group_by(census_tract_number, as_of_year) %>% 
  summarize(medrefinanceloanw = median(loan_amount_000s, na.rm = TRUE))
# table 14 median refinance loan amount by black applicant 
medrefinanceloanb <- hmda %>% 
  filter(action_taken_name == "Loan originated" & loan_purpose_name =="Refinancing" & applicant_race_name_1 == "Black or African American") %>%
  group_by(census_tract_number, as_of_year) %>% 
  summarize(medrefinanceloanb = median(loan_amount_000s, na.rm = TRUE))
# table 15 median applicant income amoung purchase loan by white applicant 

# merging the table together
hdma_cenyear <- full_join(loanmade, finance)
hdma_cenyear <-full_join(hdma_cenyear, denied)
hdma_cenyear <-full_join(hdma_cenyear, whiteloan)
hdma_cenyear <-full_join(hdma_cenyear, blackloan)
hdma_cenyear <-full_join(hdma_cenyear, refinanceloanb)
hdma_cenyear <-full_join(hdma_cenyear, refinanceloanw)
hdma_cenyear <-full_join(hdma_cenyear, deniedw)
hdma_cenyear <-full_join(hdma_cenyear, deniedb)
hdma_cenyear <-full_join(hdma_cenyear, medianloan)
hdma_cenyear <-full_join(hdma_cenyear, medianincome)
hdma_cenyear <-full_join(hdma_cenyear, medianincomeb)
hdma_cenyear <-full_join(hdma_cenyear, medianincomew)
hdma_cenyear <-full_join(hdma_cenyear, medrefinanceloanb)
hdma_cenyear <-full_join(hdma_cenyear, medrefinanceloanw)


# change N/A to zeros !! 
var <- names(hdma_cenyear)[3:11]
hdma_cenyear <- hdma_cenyear %>% 
  mutate_at(var, list(~ifelse(is.na(.), 0,. )))

# save
saveRDS(hdma_cenyear, file = "hdma_cenyear.RDS")

hmda <- hmda %>% 
  mutate(denial = if_else(action_taken_name == "Application denied by financial institution", 1, 0), # binary outcome
         race = fct_recode(applicant_race_name_1,  # recode race 
                           white = "White", 
                           black = "Black or African American",
                           asian = "Asian",
                           unknown = "Information not provided by applicant in mail, Internet, or telephone application",
                           unknown = "Not applicable",
                           other = "American Indian or Alaska Native",
                           other = "Native Hawaiian or Other Pacific Islander"),
         race = fct_relevel(race, "white", "black", "asian", "other", "unknown"),
         sex = fct_recode(applicant_sex_name,
                          male = "Male",
                          female = "female",
                          unknown = "Information not provided by applicant in mail, Internet, or telephone application",
                          unknown = "Not applicable"),
         sex = fct_relevel(sex, "male", "female", "unknown"))
         # assert the order of the categories


################################################################
#                       CREATING A MODEL                       #
################################################################
mod <- glm(denial ~ race + sex + loan_amount_000s + applicant_income_000s + census_tract_number, 
                data = hmda,
                family = "binomial")
summary(mod)
