
# TAYLOR BROWN 
# 17 JUNE 2019 



#install.packages('tigris')
#install.packages('sp')
library(tidyverse)
library(tigris)
library(sp)



#tigris_cache_dir(paste0(getwd(),"/censusgeo"))
# read hmda_cenyear
hmda <- readRDS("hdma_cenyear.RDS")



# get cville tracts (2016, post 2010 census)
cville <- tracts(state = 'VA', county = c('Charlottesville'))
plot(cville)
albemarle <- tracts(state = 'VA', county = c('Albemarle'))
plot(albemarle)
region <- rbind_tigris(cville,albemarle)
plot(region)


# merge post-2010 tracts with 2017 hmda data
# construct census tract number that matches GEOID
hmda <- hmda %>%
  mutate(tract = gsub("[.]","", census_tract_number))
hmda2017 <- hmda %>%
  filter(as_of_year == 2017 & !is.na(loansmade))
hmda2017region <- geo_join(region, hmda2017, "TRACTCE", "tract")
hmda2012 <- hmda %>%
  filter(as_of_year == 2012 & !is.na(loansmade))
hmda2012region <- geo_join(region, hmda2012, "TRACTCE", "tract")

#install.packages("leaflet")

library(leaflet)

# median income of applicant
# define popup info
# popup <- paste0("Tract: ", hmda2017region$tract, "<br>",
#"Median applicant income: ", hmda2017$medianincome)

################################################################
#            MEDICAN APPLICANT INCOME MAP  2017                #
################################################################


# define palette

pal <- colorNumeric(palette = "RdPu",
                    domain = hmda2017region$medianincome)
leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  
  # addTiles() %>% # to show streets more prominently
  
  addPolygons(data = hmda2017region,
              fillColor = ~pal(hmda2017region$medianincome),
              fillOpacity = 0.33,
              color = "white",
              weight = 1,
              smoothFactor = 0.2,
              
             # popup = popup,
              highlight = highlightOptions(
                weight = 3,
                fillOpacity = 0.7,
                bringToFront = TRUE)) %>%
  
  addLegend(pal = pal,
            values = hmda2017region$medianincome,
            position = "bottomright",
            opacity = 0.5,
            title = "2017 Median Applicant Income",
            labFormat = labelFormat(prefix = "$"))


################################################################
#                 MEDICAN LOAN AMOUNT MAP 2017                 #
################################################################
pal <- colorNumeric(palette = "BuGn",
                    domain = hmda2017region$medianloan)
leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = hmda2017region,
              fillColor = ~pal(hmda2017region$medianloan),
              fillOpacity = 0.33,
              color = "white",
              weight = 1,
              smoothFactor = 0.2,
              # popup = popup,
              highlight = highlightOptions(
                weight = 3,
                fillOpacity = 0.7,
                bringToFront = TRUE)) %>%
  addLegend(pal = pal,
            values = hmda2017region$medianloan,
            position = "bottomright",
            opacity = 0.5,
            title = " 2017 Median Loan Amount",
            labFormat = labelFormat(prefix = "$"))





################################################################
#   MEDICAN LOAN VALUE FOR WHITE APPLICANTS INCOME MAP 2017    #
################################################################
pal <- colorNumeric(palette = "YlOrRd",
                    domain = hmda2017region$whiteloan)
leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = hmda2017region,
              fillColor = ~pal(hmda2017region$whiteloan),
              fillOpacity = 0.33,
              color = "white",
              weight = 1,
              smoothFactor = 0.2,
              # popup = popup,
              highlight = highlightOptions(
                weight = 3,
                fillOpacity = 0.7,
                bringToFront = TRUE)) %>%
  addLegend(pal = pal,
            values = hmda2017region$whiteloan,
            position = "bottomright",
            opacity = 0.5,
            title = "2017 Median Loan Value for White Applicants",
            labFormat = labelFormat(prefix = "$"))






################################################################
#       MEDIAN LOAN VALUE FOR BLACK APPLICANTS  2017           #
################################################################
pal <- colorNumeric(palette = "Reds",
                    domain = hmda2017region$blackloan)
leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = hmda2017region,
              fillColor = ~pal(hmda2017region$blackloan),
              fillOpacity = 0.33,
              color = "white",
              weight = 1,
              smoothFactor = 0.2,
              
              # popup = popup,
              highlight = highlightOptions(
                weight = 3,
                fillOpacity = 0.7,
                bringToFront = TRUE)) %>%
  addLegend(pal = pal,
            values = hmda2017region$blackloan,
            position = "bottomright",
            opacity = 0.5,
            title = "2017 Median Loan Value for Black Applicants",
            labFormat = labelFormat(prefix = "$"))



################################################################
#            MEDICAN APPLICANT INCOME MAP  2012                #
################################################################


# define palette

pal <- colorNumeric(palette = "RdPu",
                    domain = hmda2012region$medianincome)
leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  
  # addTiles() %>% # to show streets more prominently
  
  addPolygons(data = hmda2012region,
              fillColor = ~pal(hmda2012region$medianincome),
              fillOpacity = 0.33,
              color = "white",
              weight = 1,
              smoothFactor = 0.2,
              
              # popup = popup,
              highlight = highlightOptions(
                weight = 3,
                fillOpacity = 0.7,
                bringToFront = TRUE)) %>%
  
  addLegend(pal = pal,
            values = hmda2012region$medianincome,
            position = "bottomright",
            opacity = 0.5,
            title = "2012 Median Applicant Income",
            labFormat = labelFormat(prefix = "$"))







################################################################
#                 MEDICAN LOAN AMOUNT MAP 2012                 #
################################################################
pal <- colorNumeric(palette = "BuGn",
                    domain = hmda2012region$medianloan)
leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = hmda2012region,
              fillColor = ~pal(hmda2012region$medianloan),
              fillOpacity = 0.33,
              color = "white",
              weight = 1,
              smoothFactor = 0.2,
              # popup = popup,
              highlight = highlightOptions(
                weight = 3,
                fillOpacity = 0.7,
                bringToFront = TRUE)) %>%
  addLegend(pal = pal,
            values = hmda2012region$medianloan,
            position = "bottomright",
            opacity = 0.5,
            title = "2012 Median Loan Amount",
            labFormat = labelFormat(prefix = "$"))





################################################################
#   MEDICAN LOAN VALUE FOR WHITE APPLICANTS INCOME MAP 2012    #
################################################################
pal <- colorNumeric(palette = "RdPu",
                    domain = hmda2012region$whiteloan)
leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = hmda2012region,
              fillColor = ~pal(hmda2012region$whiteloan),
              fillOpacity = 0.33,
              color = "white",
              weight = 1,
              smoothFactor = 0.2,
              # popup = popup,
              highlight = highlightOptions(
                weight = 3,
                fillOpacity = 0.7,
                bringToFront = TRUE)) %>%
  addLegend(pal = pal,
            values = hmda2012region$whiteloan,
            position = "bottomright",
            opacity = 0.5,
            title = "2012 Median Loan Value for White Applicants",
            labFormat = labelFormat(prefix = "$"))






################################################################
#       MEDIAN LOAN VALUE FOR BLACK APPLICANTS  2012           #
################################################################
pal <- colorNumeric(palette = "RdPu",
                    domain = hmda2012region$blackloan)
leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = hmda2012region,
              fillColor = ~pal(hmda2012region$blackloan),
              fillOpacity = 0.33,
              color = "white",
              weight = 1,
              smoothFactor = 0.2,
              
              # popup = popup,
              highlight = highlightOptions(
                weight = 3,
                fillOpacity = 0.7,
                bringToFront = TRUE)) %>%
  addLegend(pal = pal,
            values = hmda2012region$blackloan,
            position = "bottomright",
            opacity = 0.5,
            title = "2012 Median Loan Value for Black Applicants",
            labFormat = labelFormat(prefix = "$"))




# save files for markdown
saveRDS(hmda2017region, file = "hmda2017region.RDS")
saveRDS(hmda2012region, file = "hmda2012region.RDS")























































































































































####################################################################################################################################################################################################

popup <- paste0("Tract: ", hmda2017region$tract, "<br>", "Median loan: ", hmda2017$med_loan_purchase)

# define palette

pal <- colorNumeric(palette = "YlGnBu",
                    
                    domain = hmda2017region$med_loan_purchase)



leaflet() %>%
  
  addProviderTiles("CartoDB.Positron") %>%
  
  # addTiles() %>% # to show streets more prominently
  
  addPolygons(data = hmda2017region,
              
              fillColor = ~pal(hmda2017region$med_loan_purchase),
              
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
            
            values = hmda2017region$med_loan_purchase,
            
            position = "bottomright",
            
            opacity = 0.5,
            
            title = "Loan Value",
            
            labFormat = labelFormat(prefix = "$"))





# number of purchase loans - white

# define popup info

popup <- paste0("Tract: ", hmda2017region$tract, "<br>", "Purchase Loans: ", hmda2017region$whiteloan)

# define palette

pal <- colorNumeric(palette = "YlGnBu",
                    
                    domain = hmda2017region$whiteloan)



leaflet() %>%
  
  addProviderTiles("CartoDB.Positron") %>%
  
  # addTiles() %>% # to show streets more prominently
  
  addPolygons(data = hmda2017region,
              
              fillColor = ~pal(hmda2017region$whiteloan),
              
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
            
            values = hmda2017region$whiteloan,
            
            position = "bottomright",
            
            opacity = 0.5,
            
            title = "# of Purchase Loans-White")





# number of purchase loans - black

# define popup info

popup <- paste0("Tract: ", hmda2017region$tract, "<br>", "Purchase Loans: ", hmda2017region$blackloan)

# define palette

pal <- colorNumeric(palette = "YlGnBu",
                    
                    domain = hmda2017region$blackloan)



leaflet() %>%
  
  addProviderTiles("CartoDB.Positron") %>%
  
  # addTiles() %>% # to show streets more prominently
  
  addPolygons(data = hmda2017region,
              
              fillColor = ~pal(hmda2017region$blackloan),
              
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
            
            values = hmda2017region$blackloan,
            
            position = "bottomright",
            
            opacity = 0.5,
            
            title = "# of Purchase Loans-Black")



# number of refinance loans

# define popup info

popup <- paste0("Tract: ", hmda2017region$tract, "<br>", "Median refinance loan: ", hmda2017region$refinance)

# define palette

pal <- colorNumeric(palette = "YlGnBu",
                    
                    domain = hmda2017region$refinance)



leaflet() %>%
  
  addProviderTiles("CartoDB.Positron") %>%
  
  # addTiles() %>% # to show streets more prominently
  
  addPolygons(data = hmda2017region,
              
              fillColor = ~pal(hmda2017region$refinance),
              
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
            
            values = hmda2017region$refinance,
            
            position = "bottomright",
            
            opacity = 0.5,
            
            title = "# of Refinance Loans")



# get cville tracts (2000, post 2000 census)

cville00 <- tracts(state = 'VA', count = c('Charlottesville'), year = "2000")