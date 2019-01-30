
# census tract HCV counts in CSV
hcv.in <- "HUD_mdct_2010-2017.csv"#"Z:/Geocoding/Shapefiles/HCV/Housing_Choice_Vouchers_by_Tract.shp"
 library(dplyr)
library(sf)

# Read data
hcv <- unique(read.csv(hcv.in,
                stringsAsFactors = F,
                strip.white = T) %>%
  # filter(year==YEAR) %>%
  filter(program_label=="Housing Choice Vouchers")) 

# Join to census tract simple feature with the housing tenure attributes

hcv.tracts<-left_join(Tenure.sf,
          hcv,
          by=c("GEOID"="code","year"="year"))%>%
  filter(GEOID!="<NA>") %>%
  mutate(HCV.Share=total_units/`Estimate!!Total`,
         HCV.Share.Rentals=total_units/`Estimate!!Total!!Renter occupied`
         ) %>%
  mutate(HCV.Share=ifelse(HCV.Share>1,1,HCV.Share),
         HCV.Share.Rentals=ifelse(HCV.Share.Rentals>1,1,HCV.Share.Rentals))
hcv.tracts <- st_transform(hcv.tracts,4326)

