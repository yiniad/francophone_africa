library(readxl)
library(tidyverse)
library(openxlsx)
library(ggplot2)
library(openxlsx)
library(dplyr)

countries <- c("Mali","Niger","Senegal","Morocco","Algeria")
all_exports_percentage <- data.frame()
all_imports_percentage <- data.frame()

for (co in countries){
  exports <- read_excel(paste("data/",co,"_Exports_and_Imports_by_Areas_and_Co.xlsx",sep=""),sheet="Exports, FOB",skip=6)
  imports <- read_excel(paste("data/",co,"_Exports_and_Imports_by_Areas_and_Co.xlsx",sep=""),sheet="Imports, CIF",skip=6)
  
  exports <- exports[-1,]
  imports <- imports[-1,]
  #create sub categories for countries
  euro_area <- exports$Country[(which(exports$Country == "Euro Area")+1):(which(exports$Country =="Australia")-1)]
  emerging_and_developing_asia <- exports$Country[(which(exports$Country == "Emerging and Developing Asia")+1):(which(exports$Country =="Emerging and Developing Europe")-1)]
  emerging_and_developing_europe <- exports$Country[(which(exports$Country == "Emerging and Developing Europe")+1):(which(exports$Country =="Middle East and Central Asia")-1)]
  middle_east_and_central_asia <- exports$Country[(which(exports$Country == "Middle East and Central Asia")+1):(which(exports$Country =="Sub-Saharan Africa")-1)]
  subsaharan_africa <- exports$Country[(which(exports$Country == "Sub-Saharan Africa")+1):(which(exports$Country =="Western Hemisphere")-1)]
  western_emisphere <- exports$Country[(which(exports$Country == "Western Hemisphere")+1):(which(exports$Country =="Other Countries not included elsewhere")-1)]
  
  #create categories for countries
  advanced_economies <- c(euro_area, "Australia", "Canada","China, P.R.: Hong Kong","China, P.R.: Macao","Czech Rep.","Denmark","Iceland","Israel","Japan","Korea, Rep. of","New Zealand","Norway","San Marino, Rep. of","Singapore","Sweden","Switzerland","Taiwan Province of China","United Kingdom","United States","Holy See")
  emerging_and_developing_economies <- c(emerging_and_developing_asia,emerging_and_developing_europe,middle_east_and_central_asia, subsaharan_africa, western_emisphere)
  other_countries <- c("Cuba",	"Korea, Dem. People's Rep. of")
  countries_not_specified <- "Countries & Areas not specified"
  #add category and subcategory columns to imports and exports datasets 
  exports$category <- ifelse(exports$Country %in% advanced_economies, "Advanced Economy",
                             ifelse(exports$Country %in% emerging_and_developing_economies, "Emerging and Developing Economies", 
                                    ifelse(exports$Country %in% other_countries, "Other Countries not included elsewhere","N/A")))
  exports$subcategory <- ifelse(exports$Country %in% euro_area, "Euro Area",
                                ifelse(exports$Country %in% emerging_and_developing_asia, "Emerging and Developing Asia",
                                       ifelse(exports$Country %in% emerging_and_developing_europe, "Emerging and Developing Europe",
                                              ifelse(exports$Country %in% middle_east_and_central_asia, "Middle East and Central Asia",
                                                     ifelse(exports$Country %in% subsaharan_africa, "Sub-Saharan Africa",
                                                            ifelse(exports$Country %in% western_emisphere, "Western Hemisphere",
                                                                   "N/A"))))))
  
  imports$category <- ifelse(imports$Country %in% advanced_economies, "Advanced Economy",
                             ifelse(imports$Country %in% emerging_and_developing_economies, "Emerging and Developing Economies", 
                                    ifelse( imports$Country %in% countries_not_specified, "Countries & Areas not specified",
                                            ifelse(imports$Country %in% other_countries, "Other Countries not included elsewhere","N/A"))))
  
  imports$subcategory <- ifelse(imports$Country %in% euro_area, "Euro Area",
                                ifelse(imports$Country %in% emerging_and_developing_asia, "Emerging and Developing Asia",
                                       ifelse(imports$Country %in% emerging_and_developing_europe, "Emerging and Developing Europe",
                                              ifelse(imports$Country %in% middle_east_and_central_asia, "Middle East and Central Asia",
                                                     ifelse(imports$Country %in% subsaharan_africa, "Sub-Saharan Africa",
                                                            ifelse(imports$Country %in% western_emisphere, "Western Hemisphere",
                                                                   "N/A"))))))
  
  
  #create dataset for breakdown by country only
  exports_by_country <- exports[exports$category != "N/A" , ]
  imports_by_country <- imports[imports$category != "N/A" , ]
  
  
  #reshape the dataset to long format and arrange by Import_Value
  exports_long <- exports_by_country %>%
    gather(key = "Year", value = "Export_Value", `2000`:`2022`) %>%
    arrange(Year)
  exports_long$Export_Value <- as.numeric(gsub("[^0-9.]", "",exports_long$Export_Value))
  
  imports_long <- imports_by_country %>%
    gather(key = "Year", value = "Import_Value", `2000`:`2022`) %>%
    arrange(Year)
  imports_long$Import_Value <- as.numeric(gsub("[^0-9.]", "",imports_long$Import_Value))
  
  exports_long$Year <- as.numeric(exports_long$Year)
  imports_long$Year <- as.numeric(imports_long$Year)
  
  imports_long <- imports_long %>%
    mutate(subcategory = ifelse(subcategory == "N/A", "Other", subcategory))
  exports_long <- exports_long %>%
    mutate(subcategory = ifelse(subcategory == "N/A", "Other", subcategory))
  # total exports for each year
  total_exports_by_year <- exports_long %>%
    group_by(Year) %>%
    summarize(Total_Export = sum(Export_Value, na.rm = TRUE))
  
  # total imports for each year
  total_imports_by_year <- imports_long %>%
    group_by(Year) %>%
    summarize(Total_Import = sum(Import_Value, na.rm = TRUE))
  
  # percentage of total exports and imports for each country per year
  exports_percentage <- exports_long %>%
    group_by(Country, Year) %>%
    summarise(Export_Percentage = (sum(Export_Value) / total_exports_by_year$Total_Export[match(Year, total_exports_by_year$Year)]) * 100)
  
  imports_percentage <- imports_long %>%
    group_by(Country, Year) %>%
    summarise(Import_Percentage = (sum(Import_Value, na.rm = TRUE) / total_imports_by_year$Total_Import[match(Year, total_imports_by_year$Year)]) * 100)
  
  exports_percentage$country_of_origin <- co
  imports_percentage$country_of_origin <- co
  # Append the exports_percentage and imports_percentage to the respective data frames
  all_exports_percentage <- bind_rows(all_exports_percentage, exports_percentage)
  all_imports_percentage <- bind_rows(all_imports_percentage, imports_percentage)
}

imports_from_france <- all_imports_percentage %>%
  filter(Country == "France")

# Create a line chart
ggplot(imports_from_france, aes(x = Year, y = Import_Percentage, group = country_of_origin, color = country_of_origin)) +
  geom_line() +
  labs(title = "Percentage of Imports from France by Country of Origin",
       x = "Year",
       y = "Import Percentage") +
  theme_minimal()+
  scale_color_discrete(name = "Country")
