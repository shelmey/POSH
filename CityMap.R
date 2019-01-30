# Parameters

# Center of Map
y=39.2904
x=-76.6122
# Zoom level to start at
z=11.5

# Filters for data to map
YEAR=2017
county.fips<- "510"

# Endpoint of council district boundaries 
council.dist.url <- 'https://opendata.arcgis.com/datasets/33612ccc77254e56aa780da502ddd701_0.geojson'

# Prepare the source data
source("base_tract.R")
source("base_hcv.R")

# Required packages
library(leaflet)
library(viridis)
library(dplyr)
library(scales)
library(htmlwidgets)
library(jsonlite)
library(geojsonio)
library(sf)
library(htmltools)
library(classInt)
library(ggplot2)
library(RColorBrewer)

# Map theme for ggplots
map.theme.2 <- theme_void() + theme(plot.title = element_text(hjust=0.5), panel.grid.major = element_line(colour = "white"),legend.position = "bottom")



# Read council district geojson and convert to sf
council.dist <- geojson_read(council.dist.url,what = "sp") %>%
  st_as_sf() %>% 
  st_transform(4326) 


# Calculate the centroids of the council districts and get their numeric coordinates 
council.ctr<-st_centroid(council.dist)
ys<-function(shp){st_coordinates(shp)[,2]-.01} # manipulate latitude because the projection is kind of wonky. not great.
xs<-function(shp){st_coordinates(shp)[,1]}

# Put the district numbers in stylized HTML text
labs <- lapply(council.dist$AREA_NAME, function(n) {HTML(paste0("<font size='5' color='white'>",n,"</font>"))})

# Filter the source data 
data<-filter(hcv.tracts,
             year==YEAR)%>%
  filter(substr(GEOID,3,5) %in% county.fips)

# Calculate integer breaks for the raw counts
quantInv <- function(distr, value){ 
  brks <- (round(classIntervals(distr, n = 5, style = 'quantile')$brks))
  c(0,brks[1:length(brks)])}

# natural.interval = classIntervals(data$total_units, n = 5, style = 'jenks')$brks


total.pal <- colorBin("RdPu",bins = quantInv(data$total_units), na.color = NA)

# Labelling for the integer breaks
fix.labs <- function(type, cuts, p) {
  n = length(cuts)
  paste0(percent(cuts[-n]), " &ndash; ", percent(cuts[-1]))
}

# calculate jenks breaks that are probably unique and sort of make sense for percentages. Not use rn in favor of arbitrary easily legible breaks
quantInv.pct <- function(distr, value){ 
  brks <- (round(classIntervals(distr, n = 5, style = 'jenks')$brks*10000))/10000
  c(0,brks[1:length(brks)])}

share.renter.pal <- colorBin("plasma",bins = c(0,.05,.1,.15,.3,1),na.color = NA)
# share.renter.pal <- colorQuantile("plasma",n=5,domain=data$HCV.Share.Rentals,na.color = NA)

 share.total.pal <- colorBin("plasma",bins = c(0,.025,.05,.1,.15,1),na.color = NA)
# share.total.pal <- colorQuantile("plasma",n=5,domain=data$HCV.Share,na.color = NA)

 Renter.Occupied.pal <- colorBin("viridis",bins = c(0,.2,.4,.6,.8,1),na.color = NA)
# Renter.Occupied.pal <- colorQuantile("plasma",n=5,domain=data$Share.Renter.Occupied,na.color = NA)

 # basemap
tract_base <- 
  leaflet() %>%
  setView(lng = x, lat = y,zoom=z) %>%
  addProviderTiles(providers$Esri)

# add everything 
hcv.map<-tract_base %>%
  addPolygons(group="HCV Assisted Share of Rentals",
              data = data,
              fillColor = ~ share.renter.pal(HCV.Share.Rentals),
              fillOpacity = .4,
              color = 'white',
              popup=~paste0(sub("MD Maryland","",name),
                            "<br>",
                            "Voucher Assisted Share of renter-occupied units: ",
                            percent(HCV.Share.Rentals)),
              weight = 0)   %>%
  addPolygons(group="HCV Assisted Share of Total Units",
              data = data,
              fillColor = ~ share.total.pal(HCV.Share),
              fillOpacity = .4,
              color = 'white',
              popup=~paste0(sub("MD Maryland","",name),
                            "<br>",
                            "Voucher Assisted Share of occupied units: ",
                            percent(HCV.Share)),
              weight = 0)  %>%
  addPolygons(group="Total HCV Assisted Units",
              data = data,
              fillColor = ~ total.pal(total_units),
              fillOpacity = .4,
              color = 'white',
              popup=~paste0(sub("MD Maryland","",name),
                            "<br>",
                            "Total Voucher-Assisted Units: ",
                            total_units),
              weight = 0)  %>%
  
  addPolygons(group="Renter-Occupied Share of Total Units",
              data = data,
              fillColor = ~ Renter.Occupied.pal(Share.Renter.Occupied),
              fillOpacity = .4,
              color = 'white',
              popup=~paste0(sub("MD Maryland","",name),
                            "<br>",
                            "Renter-Occupied Share of Total Units: ",
                            percent(Share.Renter.Occupied)),
              weight = 0)  %>%
addPolylines(group="Total HCV Assisted Units",
             data = council.dist,
             fillOpacity = 1,
             color = 'white',
             weight = 2) %>%
addLabelOnlyMarkers(group="Total HCV Assisted Units",
                    lng = xs(council.ctr), lat = ys(council.ctr),
                    label = labs,
                    labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T))%>%
addPolylines(group="Renter-Occupied Share of Total Units",
             data = council.dist,
             fillOpacity = 1,
             color = 'white',
             weight = 2) %>%
addLabelOnlyMarkers(group="Renter-Occupied Share of Total Units",
                    lng = xs(council.ctr), lat = ys(council.ctr),
                    label = labs,
                    labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T)) %>%
addPolylines(group="HCV Assisted Share of Rentals",
             data = council.dist,
             fillOpacity = 1,
             color = 'white',
             weight = 2) %>%
addLabelOnlyMarkers(group="HCV Assisted Share of Rentals",
                    lng = xs(council.ctr), lat = ys(council.ctr),
                    label = labs,
                    labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T))%>%
addPolylines(group="HCV Assisted Share of Total Units",
             data = council.dist,
             fillOpacity = 1,
             color = 'white',
             weight = 2) %>%
addLabelOnlyMarkers(group="HCV Assisted Share of Total Units",
                    lng = xs(council.ctr), lat = ys(council.ctr),
                    label =  labs,
                    labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T))%>%
addLegend(group="Total HCV Assisted Units",
            data=data,
            pal=total.pal,values = ~total_units,
            opacity = .4,
            title = "Total HCV Assisted Units") %>%
  addLegend(group="HCV Assisted Share of Rentals",
            data=data,
            pal=share.renter.pal,values =~HCV.Share.Rentals,
            opacity = .4,
            title = "HCV Assisted Share of Rentals",
            labFormat = fix.labs
  ) %>%
  
  addLegend(group="HCV Assisted Share of Total Units",
            data=data,
            pal=share.total.pal,values =~HCV.Share,
            opacity = .4,
            labels = quant,
            title = "HCV Assisted Share of Units",
            labFormat = fix.labs) %>%
  
  addLegend(group="Renter-Occupied Share of Total Units",
            data=data,
            pal=Renter.Occupied.pal,values =~Share.Renter.Occupied,
            opacity = .4,
            title = "Renter-Occupied Share of Units",
            labFormat = fix.labs) %>%
  addLayersControl(overlayGroups = c("Total HCV Assisted Units",
                                     "HCV Assisted Share of Rentals",
                                     "HCV Assisted Share of Total Units",
                                     "Renter-Occupied Share of Total Units"
  ),
  options = layersControlOptions(collapsed = FALSE)) %>%
  hideGroup(c("HCV Assisted Share of Rentals",
              "HCV Assisted Share of Total Units",
              "Renter-Occupied Share of Total Units"
  ))

# Save as htm widget
saveWidget(hcv.map,"HCV Map City.htm")

# Now ggplot maps for outputting pdf, png, whatever
# Another breaks function 
natural.interval<-function(domain,n){classIntervals(domain,
                                                    n = n, 
                                                    style = 'jenks')$brks}

# Another breaks function
quantile.interval <- function(domain,n){round(quantile(domain, probs=seq(0, 1, by = 1/n),na.rm=T)/10)*10}

# Cutting function that converts numeric vector to a quantile factor with num levels
fillcut <- function(val,num){cut(val, breaks = quantile.interval(val,num), dig.lab = num)}

# Another cutting function that converts numeric vector to a quantile factor with num levels
fillcut.2 <- function(val,num){
  br<-round(classIntervals(val, n = num, style = 'quantile')$brks)
  br<-c(0,br[2:length(br)])
  cut(val, breaks = br, dig.lab = num)}

# just make the council district centroids into a dataframe
council.pts<-cbind.data.frame(xs(council.ctr),ys(council.ctr)+.01,council.ctr$AREA_NAME)
names(council.pts)<-c("X","Y","Name")

# formatting function for range labels (converts default level names generated by cut())
range.lab<-function(x){
  z<-sub("\\]","",sub("\\(","",sub(","," - ",x)))
  b<-sub("-.*$","",z)
  e<-sub("^.*-","",z)
  labs<-paste(
    as.numeric(b)+1,
    as.numeric(trimws(e)),
    sep="-"
  )
  
  labs<-ifelse(grepl("NA",labs),"Missing",labs)
}

# Same thing for proportions
range.lab.pct<-function(x){
  z<-sub("\\]","",sub("\\(","",sub(","," - ",x)))
  b<-sub("-.*$","",z)
  e<-sub("^.*-","",z)
  labs<-paste(
    percent(as.numeric(trimws(b))),
    percent(as.numeric(trimws(e))),
    sep="-"
  )
  
  labs<-ifelse(grepl("NA",labs),"Missing",labs)
}

# Ok cool let's make some maps
ggplot() +
  geom_sf(data=data,mapping=aes(fill=fillcut.2(total_units,5)),lwd=.00001)+
  scale_fill_manual(values =brewer.pal(5,"RdPu"),
                    labels=range.lab,
                    guide = "legend",
                    na.value = NA) +
  map.theme.2 +
  labs(title="HCV Counts by Census Tract",
       fill="Total HCV Assisted Units") +
  geom_sf(data=council.dist,color="white",fill=NA) +
  geom_text(data=council.pts,aes(X,Y,label=Name),size=3,color="white")

ggsave("Z:/Geocoding/Interactive Maps/HCV by Tract/HCV by Tract/Totals City.pdf")

ggplot() +
  geom_sf(data=data,mapping=aes(fill=cut(HCV.Share.Rentals,
                                         breaks=c(0,.05,.1,.15,.3,1))),lwd=0)+
  scale_fill_manual(values =brewer.pal(5,"RdPu"),
                    labels=range.lab.pct,
                    guide = "legend",
                    na.value = NA) +
  map.theme.2 +
  labs(title="HCV Share by Census Tract",
       fill="HCV Assisted Share of Rental Units") +
  geom_sf(data=council.dist,color="white",fill=NA) +
  geom_text(data=council.pts,aes(X,Y,label=Name),size=3,color="white")

ggsave("Z:/Geocoding/Interactive Maps/HCV by Tract/HCV by Tract/Rental Share City.pdf")

ggplot() +
  geom_sf(data=data,mapping=aes(fill=cut(HCV.Share,
                                         breaks=c(0,.025,.05,.1,.15,1))),lwd=0)+
  scale_fill_manual(values =brewer.pal(5,"RdPu"),
                    labels=range.lab.pct,
                    guide = "legend",
                    na.value = NA) +
  map.theme.2 +
  labs(title="HCV Share by Census Tract",
       fill="HCV Assisted Share of Occupied Units") +
  geom_sf(data=council.dist,color="white",fill=NA) +
  geom_text(data=council.pts,aes(X,Y,label=Name),size=3,color="white")

ggsave("Z:/Geocoding/Interactive Maps/HCV by Tract/HCV by Tract/Total Share City.pdf")
