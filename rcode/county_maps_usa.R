#to run this code from the R console type the following below command (make sure the path is correct!)
#source(file="/Users/mullerg/Dropbox/data_science/projects/nasa_space_apps_2024/r_code/zipcode_maps.R")

library(tidyverse)
library(tigris)

# load data
zip_fip <- read.csv("/Users/mullerg/Dropbox/data_science/projects/nasa_space_apps_2024/ZIP-COUNTY-FIPS_2017-06.csv", head = TRUE, colClasses = c(STCOUNTYFP = "character"))

places <- read.csv("/Users/mullerg/Dropbox/data_science/projects/nasa_space_apps_2024/PLACES__Local_Data_for_Better_Health__Census_Tract_Data_2024_release_20241002.csv", head = TRUE, colClasses = c(CountyFIPS = "character"))


measures <- as.data.frame(places %>% group_by(Measure) %>% summarize(Measure_id = first(MeasureId)))
measures <- measures[["Measure"]]
measure_of_interest = ""

plot_county_data <- function(measure_of_interest){
	
measure_id = measure_of_interest
print(paste("Measure currently being processed: ", measure_of_interest, sep = ""))

places_2022_measure <- places %>% filter(Year == 2022 & Measure == measure_id)
places_2022_measure_counties <- places_2022_measure %>% group_by(MeasureId, StateAbbr, StateDesc, CountyFIPS, Data_Value_Unit) %>% summarize(DataValue = mean(Data_Value))
data_unit <- places_2022_measure_counties[1,5]

counties_usa <- counties(year = 2000, cb = TRUE)
counties_usa$statecountyFIPS <- paste(counties_usa$STATE, counties_usa$COUNTY, sep = "")
counties_usa_measure <- left_join(counties_usa, places_2022_measure_counties, by = c("statecountyFIPS" = "CountyFIPS"))

ggplot() + geom_sf(data = counties_usa_measure, aes(fill = DataValue), linewidth = 0.1) + theme_minimal() + coord_sf(xlim = c(-125, -65), ylim = c(24, 51), expand = FALSE) + scale_fill_distiller(palette = "Spectral", guide = "colorbar") + labs(fill = paste(measure_id, " (prevalence in 2022, %)  ")) + theme(legend.position = "bottom")

ggsave(paste(paste("map_usa_counties_", measure_id, sep = ""),".png", sep = ""), device = "png", dpi = "retina", width = 4000, height = 1600, units = "px", path = "/Users/mullerg/Dropbox/data_science/projects/nasa_space_apps_2024/")
print(paste("Map has been saved in the NASA Space Apps 2024 folder for measure: ", measure_of_interest, sep = ""))
print(paste("Completed at: ", format(Sys.time(), "%H:%M:%S"), sep = ""))
}


for(measure_of_interest in measures){
	plot_county_data(measure_of_interest)
}

