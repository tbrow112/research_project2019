# Taylor Janet 
# June 20th 2019
# HMDA albermale country data 


# add tidy verse 
library(tidyverse)

#import albermale hdma data as "data"
data <- read_csv("raw/hdma/hmda_lar (12).csv")

#turn all categorical variables into factors 
var <- names(data) 
var <- str_subset(var,"_name") 
data <- data %>% mutate_at(var, as.factor)
data<- data %>% 
  mutate(state_abbr = factor(state_abbr),
         agency_abbr = factor(agency_abbr))
# create a scatter plot
library(ggplot2)
str(data)
ggplot(data, aes(x=applicant_income_000s,
                 y=loan_amount_000s)) + geom_point(shape=1, alpha = 0.05) +
  geom_smooth(method=lm) +
  scale_x_continuous(labels = scales::dollar, name = "Applicant Income in 1,000's") +
  scale_y_continuous(labels = scales::dollar, name = "Loan Amount in 1,000's", limits=c(0,5000)) +
  facet_wrap(~as_of_year) +
  theme(axis.text = element_text(face="italic", color="midnightblue"),
        axis.text.x = element_text(angle=90, vjust=0.5))

# bar graph of cencus tract and year 
counts <- table(data$census_tract_number, data$as_of_year)
barplot(counts, main="Census Tract & Year", xlab="year", ylab='number of loan applications')


# make a bar plot of hdma_cenyear by the census_tract_number facet this by year 
# table 1 total loans made (originated, institution purchased)
loanmade <- data %>%
  filter(action_taken_name == "Loan originated" & loan_purpose_name =="Home purchase") %>%
  group_by(census_tract_number, as_of_year) %>%
  summarize(loansmade = n())

# table 2 number of refinances by census tract year 
finance <- data %>% 
  filter(action_taken_name == "Loan originated" & loan_purpose_name =="Refinancing") %>%
  group_by(census_tract_number, as_of_year) %>% 
  summarize(finance = n())
#table 3 total loan apps denied 
denied <- data %>% 
  filter(action_taken_name == "Application denied by financial institution" & loan_purpose_name =="Home purchase") %>%
  group_by(census_tract_number, as_of_year) %>% 
  summarize(denied = n())
#table 4 total loans made to a white applicant 
whiteloan <- data %>%
  
  filter(action_taken_name == "Loan originated" & loan_purpose_name =="Home purchase" & applicant_race_name_1 == "White") %>%
  
  group_by(census_tract_number, as_of_year) %>%
  
  summarize(whiteloan = n())

#table 5 total loans made to a black applicant 
blackloan <- data %>%
  
  filter(action_taken_name == "Loan originated" & loan_purpose_name =="Home purchase" & applicant_race_name_1 == "Black or African American") %>%
  
  group_by(census_tract_number, as_of_year) %>%
  
  summarize(blackloan = n())

# table 6 number of refinance loans made to white applicant 
refinanceloanw <- data %>% 
  filter(action_taken_name == "Loan originated" & loan_purpose_name =="Refinancing" & applicant_race_name_1 == "White") %>%
  group_by(census_tract_number, as_of_year) %>% 
  summarize(refinanceloanw = n())

# table 7 number of refinance loans made to white applicant 
refinanceloanb <- data %>% 
  filter(action_taken_name == "Loan originated" & loan_purpose_name =="Refinancing" & applicant_race_name_1 == "Black or African American") %>%
  group_by(census_tract_number, as_of_year) %>% 
  summarize(refinanceloanb = n())

#table 8 number of loans denied to a white applicant 
deniedw <- data %>% 
  filter(action_taken_name == "Application denied by financial institution" & loan_purpose_name =="Home purchase" & applicant_race_name_1 == "White") %>%
  group_by(census_tract_number, as_of_year) %>% 
  summarize(deniedw = n())

#table 9 number of loans denied to a black applicant 
deniedb <- data %>% 
  filter(action_taken_name == "Application denied by financial institution" & loan_purpose_name =="Home purchase" & applicant_race_name_1 == "Black or African American") %>%
  group_by(census_tract_number, as_of_year) %>% 
  summarize(deniedb = n())
#table 10 median loan amount 
medianloan <- data %>%
  filter(action_taken_name == "Loan originated" & loan_purpose_name =="Home purchase") %>%
  group_by(census_tract_number, as_of_year) %>%
  summarize(medianloan = median(loan_amount_000s, na.rm = TRUE))
#table 11 median applicant income 
medianincome <- data %>%
  filter(action_taken_name == "Loan originated" & loan_purpose_name =="Home purchase") %>%
  group_by(census_tract_number, as_of_year) %>%
  summarize(medianincome = median(applicant_income_000s, na.rm = TRUE))
#table 12 median loan purchase amount by white applicant 
medianincomew <- data %>%
  filter(action_taken_name == "Loan originated" & loan_purpose_name =="Home purchase" & applicant_race_name_1 == "White") %>%
  group_by(census_tract_number, as_of_year) %>%
  summarize(medianincomew = median(applicant_income_000s, na.rm = TRUE))
#table 12 median loan purchase amount by black applicant 
medianincomeb <- data %>%
  filter(action_taken_name == "Loan originated"  & loan_purpose_name =="Home purchase" & applicant_race_name_1 == "Black or African American") %>%
  group_by(census_tract_number, as_of_year) %>%
  summarize(medianincomewb = median(applicant_income_000s, na.rm = TRUE))
# table 13 median refinance loan amount by white applicant 
medrefinanceloanw <- data %>% 
  filter(action_taken_name == "Loan originated" & loan_purpose_name =="Refinancing" & applicant_race_name_1 == "White") %>%
  group_by(census_tract_number, as_of_year) %>% 
  summarize(medrefinanceloanw = median(loan_amount_000s, na.rm = TRUE))
# table 14 median refinance loan amount by black applicant 
medrefinanceloanb <- data %>% 
  filter(action_taken_name == "Loan originated" & loan_purpose_name =="Refinancing" & applicant_race_name_1 == "Black or African American") %>%
  group_by(census_tract_number, as_of_year) %>% 
  summarize(medrefinanceloanb = median(loan_amount_000s, na.rm = TRUE))
# table 15 median applicant income amoung purchase loan by white applicant 

# merging the table together
hdma_cenyear_albermale <- full_join(loanmade, finance)
hdma_cenyear_albermale <-full_join(hdma_cenyear_albermale, denied)
hdma_cenyear_albermale <-full_join(hdma_cenyear_albermale, whiteloan)
hdma_cenyear_albermale <-full_join(hdma_cenyear_albermale, blackloan)
hdma_cenyear_albermale <-full_join(hdma_cenyear_albermale, refinanceloanb)
hdma_cenyear_albermale <-full_join(hdma_cenyear_albermale, refinanceloanw)
hdma_cenyear_albermale <-full_join(hdma_cenyear_albermale, deniedw)
hdma_cenyear_albermale <-full_join(hdma_cenyear_albermale, deniedb)
hdma_cenyear_albermale <-full_join(hdma_cenyear_albermale, medianloan)
hdma_cenyear_albermale <-full_join(hdma_cenyear_albermale, medianincome)
hdma_cenyear_albermale <-full_join(hdma_cenyear_albermale, medianincomeb)
hdma_cenyear_albermale <-full_join(hdma_cenyear_albermale, medianincomew)
hdma_cenyear_albermale <-full_join(hdma_cenyear_albermale, medrefinanceloanb)
hdma_cenyear_albermale <-full_join(hdma_cenyear_albermale, medrefinanceloanw)


# change N/A to zeros !! 
var <- names(hdma_cenyear_albemarle)[3:11]
hdma_cenyear_albermale <- hdma_cenyear_albemarle %>% 
  mutate_at(var, list(~ifelse(is.na(.), 0,. )))

# save
saveRDS(hdma_cenyear_albermale, file = "hdma_cenyear_albemarle.RDS")


#install.packages('tigris')
#install.packages('sp')

library(tidyverse)
library(tigris)
library(sp)

#tigris_cache_dir(paste0(getwd(),"/censusgeo"))
hmda <- readRDS("hdma_cenyear_albemarle.RDS")
albemarle <- tracts(state = 'VA', county = c('Albemarle'))
plot(albemarle)

# merge post-2010 tracts with 2017 hmda data
# construct census tract number that matches GEOID
data <- data %>%
  mutate(tract = gsub("[.]","", census_tract_number))


#install.packages("leaflet")
library(leaflet)

# median income of applicant
# define popup info
popup <- paste0("Tract: ", data$tract, "<br>", "Median applicant income: ", data$medianincome)

# define palett
pal <- colorNumeric(palette = "YlGnBu",
                    domain = data$medianincome)

leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  
  # addTiles() %>% # to show streets more prominently
  
  addPolygons(data = data,
              fillColor = ~pal(data$medianincome),
              fillOpacity = 0.33,
              color = "white",
              weight = 1,
              smoothFactor = 0.2,
              popup = popup,
              highlight = highlightOptions(
                weight = 3,
                fillOpacity = 0.7,
                bringToFront = TRUE)) %>%
  addLegend(pal = pal,
            values = data$medianincome,
            position = "bottomright",
            opacity = 0.5,
            title = "Applicant Income",
            labFormat = labelFormat(prefix = "$"))



# resources

# https://rpubs.com/rosenblum_jeff/censustutorial1

# https://rstudio.github.io/leaflet/





# median loan amount

# define popup info

popup <- paste0("Tract: ", data$tract, "<br>", "Median loan: ", data$med_loan_purchase)

# define palette

pal <- colorNumeric(palette = "YlGnBu",
                    
                    domain = data$med_loan_purchase)



leaflet() %>%
  
  addProviderTiles("CartoDB.Positron") %>%
  
  # addTiles() %>% # to show streets more prominently
  
  addPolygons(data = data,
              
              fillColor = ~pal(data$med_loan_purchase),
              
              fillOpacity = 0.33,
              
              color = "white",
              
              weight = 1,
              
              smoothFactor = 0.2,
              
              popup = popup,
              
              highlight = highlightOptions(
                
                weight = 3,
                
                fillOpacity = 0.7,
                
                bringToFront = TRUE)) %>%
  
  addLegend(pal = pal,
            
            values = data$med_loan_purchase,
            
            position = "bottomright",
            
            opacity = 0.5,
            
            title = "Loan Value",
            
            labFormat = labelFormat(prefix = "$"))





# number of purchase loans - white

# define popup info

popup <- paste0("Tract: ", data$tract, "<br>", "Purchase Loans: ", data$whiteloan)

# define palette

pal <- colorNumeric(palette = "YlGnBu",
                    
                    domain = data$whiteloan)



leaflet() %>%
  
  addProviderTiles("CartoDB.Positron") %>%
  
  # addTiles() %>% # to show streets more prominently
  
  addPolygons(data = data,
              
              fillColor = ~pal(data$whiteloan),
              
              fillOpacity = 0.5,
              
              color = "white",
              
              weight = 1,
              
              smoothFactor = 0.2,
              
              popup = popup,
              
              highlight = highlightOptions(
                
                weight = 3,
                
                fillOpacity = 0.7,
                
                bringToFront = TRUE)) %>%
  
  addLegend(pal = pal,
            
            values = data$whiteloan,
            
            position = "bottomright",
            
            opacity = 0.5,
            
            title = "# of Purchase Loans-White")





# number of purchase loans - black

# define popup info

popup <- paste0("Tract: ", data$tract, "<br>", "Purchase Loans: ", data$blackloan)

# define palette

pal <- colorNumeric(palette = "YlGnBu",
                    
                    domain = data$blackloan)



leaflet() %>%
  
  addProviderTiles("CartoDB.Positron") %>%
  
  # addTiles() %>% # to show streets more prominently
  
  addPolygons(data = data,
              
              fillColor = ~pal(data$blackloan),
              
              fillOpacity = 0.5,
              
              color = "white",
              
              weight = 1,
              
              smoothFactor = 0.2,
              
              popup = popup,
              
              highlight = highlightOptions(
                
                weight = 3,
                
                fillOpacity = 0.7,
                
                bringToFront = TRUE)) %>%
  
  addLegend(pal = pal,
            
            values = data$blackloan,
            
            position = "bottomright",
            
            opacity = 0.5,
            
            title = "# of Purchase Loans-Black")



# number of refinance loans

# define popup info

popup <- paste0("Tract: ", data$tract, "<br>", "Median refinance loan: ", data$refinance)

# define palette

pal <- colorNumeric(palette = "YlGnBu",
                    
                    domain = data$refinance)



leaflet() %>%
  
  addProviderTiles("CartoDB.Positron") %>%
  
  # addTiles() %>% # to show streets more prominently
  
  addPolygons(data = data,
              
              fillColor = ~pal(data$refinance),
              
              fillOpacity = 0.5,
              
              color = "white",
              
              weight = 1,
              
              smoothFactor = 0.2,
              
              popup = popup,
              
              highlight = highlightOptions(
                
                weight = 3,
                
                fillOpacity = 0.7,
                
                bringToFront = TRUE)) %>%
  
  addLegend(pal = pal,
            
            values = data$refinance,
            
            position = "bottomright",
            
            opacity = 0.5,
            
            title = "# of Refinance Loans")



# get cville tracts (2000, post 2000 census)

albemarle00 <- tracts(state = 'VA', count = c('Albemarle'), year = "2000")
