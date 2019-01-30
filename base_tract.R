# This is likely to become an interactive map of the UML. 
#For now, it's a useful tool for checking on Multifamily complexes. Also a testing ground for the census API.




# Program parameters
# Make sure your census api key is set as the default option
# years to query. This program compiles a panel as long as the api query works for all years and jurisdictions included in these vectors.
years<-c(2015:2017)

counties<-c("Baltimore County",
            "Baltimore City",
            "Anne Arundel",
            "Howard",
            "Harford",
            "Carroll")

# required packages
library(tidycensus)
library(plyr)
library(dplyr)
library(reshape2)
###

acs17<-load_variables(2017,"acs5",cache=T)
tracts<-get_acs(geography = "tract",
                # county=counties,
                state = "MD",
                variables =  "B25003_001E",
                geometry = T,
                cache = TRUE)
  
# y <- NA

get.acs.tenure <- function(YEAR){
y<-YEAR
get_acs( year = y,
               geography = "tract", 
              variables = c("B25003_001E","B25003_003E"), 
              survey = "acs5", 
              state = "MD") %>%
    left_join(.,
              select(acs17,
                     variable=name,
                     label))%>%
           dcast(GEOID+NAME~label,value.var="estimate") %>%
           dplyr::mutate(year=y,
                  Share.Renter.Occupied=`Estimate!!Total!!Renter occupied`/`Estimate!!Total`)
}

Tenure <- rbind.fill(lapply(years,get.acs.tenure))

Tenure.sf<-left_join(select(tracts,
                            GEOID),
                     Tenure)

