library(readxl)
library(tidyverse)
library(openxlsx)
library(ggplot2)
library(openxlsx)
library(dplyr)

co <- "Mali"
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

# Total exports and imports by region
total_exports_by_subcategory <- exports_long %>%
  group_by(subcategory, Year) %>%
  summarize(Total_Export = sum(Export_Value, na.rm = TRUE))
total_imports_by_subcategory <- imports_long %>%
  group_by(subcategory, Year) %>%
  summarize(Total_Import = sum(Import_Value, na.rm = TRUE))
total_by_sub_combined <- merge(total_exports_by_subcategory, total_imports_by_subcategory, by = c("subcategory", "Year"), all = TRUE)
ggplot(total_by_sub_combined, aes(x = Year)) +
  geom_line(aes(y = Total_Export, color = "Exports")) +
  geom_line(aes(y = Total_Import, color = "Imports")) +
  facet_wrap(~ subcategory, ncol = 2, scales = "free_y") +
  labs(x = "Year", y = "Total Value",
       title = paste(co, ": Evolution of Total Exports and Imports by Region in USD Mn")) +
  scale_color_manual(values = c("Exports" = "blue", "Imports" = "red")) +
  theme_minimal()


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

# select the top 10 countries by percentage
top_countries_exports <- exports_percentage %>%
  group_by(Country) %>%
  summarise(Total_Percentage = sum(Export_Percentage, na.rm = TRUE)) %>%
  arrange(desc(Total_Percentage)) %>%
  slice(1:10) %>%
  pull(Country)
top_countries_imports <- imports_percentage %>%
  group_by(Country) %>%
  summarise(Total_Percentage = sum(Import_Percentage, na.rm = TRUE)) %>%
  arrange(desc(Total_Percentage)) %>%
  slice(1:10) %>%
  pull(Country)

# filter data for top 10 countries
top_exports <- exports_percentage %>%
  filter(Country %in% top_countries_exports)%>%
  arrange(Year,desc(Export_Percentage))

top_imports <- imports_percentage %>%
  filter(Country %in% top_countries_imports)%>%
  arrange(Year,desc(Import_Percentage))

# create the line plot
ggplot(top_exports, aes(x = Year, y = Export_Percentage, color = Country)) +
  geom_line() +
  labs(x = "Year", y = "Percentage of Total Exports (%)",
       title = "Evolution of Percentage of Total Exports by Country",
       color = "Country") +
  theme_minimal()
ggplot(top_imports, aes(x = Year, y = Import_Percentage, color = Country)) +
  geom_line() +
  labs(x = "Year", y = "Percentage of Total Imports (%)",
       title = "Evolution of Percentage of Total Imports by Country",
       color = "Country") +
  theme_minimal()

# percentage of imports and exports from/to France
imports_from_france <- imports_percentage %>%
  filter(Country == "France")
ggplot(imports_from_france, aes(x = Year, y = Import_Percentage)) +
  geom_line( color = "red") +
  labs(x = "Year", y = "Percentage out of Total Imports (%)",
       title = paste(co,": Evolution of Percentage of Imports from France")) +
  theme_minimal()

exports_from_france <- exports_percentage %>%
  filter(Country == "France")
ggplot(exports_from_france, aes(x = Year, y = Export_Percentage)) +
  geom_line(color = "blue") +
  labs(x = "Year", y = "Percentage out of Total Exports (%)",
       title = paste(co,": Evolution of Percentage of Exports to France")) +
  theme_minimal()

# percentage of imports and exports from/to France
imports_from_morocco <- imports_percentage %>%
  filter(Country == "Morocco")
ggplot(imports_from_morocco, aes(x = Year, y = Import_Percentage)) +
  geom_line( color = "red") +
  labs(x = "Year", y = "Percentage out of Total Imports (%)",
       title = paste(co,": Evolution of Percentage of Imports from Morocco")) +
  theme_minimal()

exports_from_morocco <- exports_percentage %>%
  filter(Country == "Morocco")
ggplot(exports_from_morocco, aes(x = Year, y = Export_Percentage)) +
  geom_line(color = "blue") +
  labs(x = "Year", y = "Percentage out of Total Exports (%)",
       title = paste(co,": Evolution of Percentage of Exports to Morocco")) +
  theme_minimal()

# evolution of top countries percentages (unstacked)
custom_colors <- c("#1f78b4", "#33a02c", "#e31a1c", "#ff7f00", "#6a3d9a",
                   "#a6cee3", "#b2df8a", "#fb9a99", "#fdbf6f", "#cab2d6")

ggplot(top_imports %>% filter(Year >= max(Year) - 6), aes(x = Year, y = Import_Percentage, fill = Country)) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(x = "Year", y = "Percentage of Total Import (%)",
       title = paste(co, ": Top 10 Countries by Import Percentage"),
       fill = "Country") +
  scale_fill_manual(values = custom_colors) +
  theme_minimal()

ggplot(top_exports %>% filter(Year >= max(Year) - 6), aes(x = Year, y = Export_Percentage, fill = Country)) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(x = "Year", y = "Percentage of Total Export (%)",
       title = paste(co, ": Top 10 Countries by Export Percentage"),
       fill = "Country") +
  scale_fill_manual(values = custom_colors) +
  theme_minimal()

# loop through lists of top countries (imports and exports) and create charts

for (c in top_countries_imports) {
  imports_from_c <- imports_percentage %>%
    filter(Country == c)
  p1 <- ggplot(imports_from_c, aes(x = Year, y = Import_Percentage)) +
    geom_line(color = "red") +
    labs(x = "Year", y = "Percentage out of Total Imports (%)",
         title = paste(co, ": Evolution of Percentage of Imports from", c)) +
    theme_minimal()
  print(p1)  
}

for (c in top_countries_exports) {
  exports_from_c <- exports_percentage %>%
    filter(Country == c)
  
  p <- ggplot(exports_from_c, aes(x = Year, y = Export_Percentage)) +
    geom_line(color = "blue") +
    labs(x = "Year", y = "Percentage out of Total Exports (%)",
         title = paste(co, ": Evolution of Percentage of Exports to", c)) +
    theme_minimal()
  
  print(p)  
}

#loop through list of countries to be checked and create charts
# check_countries <- c("Brazil", "India", "Bermuda")
# for (c in check_countries){
#   exports_from_c <- exports_percentage %>%
#     filter(Country == c)
#   p <- ggplot(exports_from_c, aes(x = Year, y = Export_Percentage)) +
#     geom_line(color = "blue") +
#     labs(x = "Year", y = "Percentage out of Total Exports (%)",
#          title = paste(co,": Evolution of Percentage of Exports to", c)) +
#     theme_minimal()
#   print(p)
# }
# for (c in check_countries){
#   imports_from_c <- imports_percentage %>%
#     filter(Country == c)
#   p <- ggplot(imports_from_c, aes(x = Year, y = Import_Percentage)) +
#     geom_line(color = "red") +
#     labs(x = "Year", y = "Percentage out of Total Imports (%)",
#          title = paste(co,": Evolution of Percentage of Imports from", c)) +
#     theme_minimal()
#   print(p)
# }
# 
# 
# countries <- c("Mali","Niger","Senegal","Morocco","Algeria")
# 
# for (c in countries){
#   exports_from_c <- exports_percentage %>%
#     filter(Country == c)
#   p <- ggplot(exports_from_c, aes(x = Year, y = Export_Percentage)) +
#     geom_line(color = "blue") +
#     labs(x = "Year", y = "Percentage out of Total Exports (%)",
#          title = paste(co,": Evolution of Percentage of Exports to", c)) +
#     theme_minimal()
#   print(p)
# }
# for (c in countries){
#   imports_from_c <- imports_percentage %>%
#     filter(Country == c)
#   p <- ggplot(imports_from_c, aes(x = Year, y = Import_Percentage)) +
#     geom_line(color = "red") +
#     labs(x = "Year", y = "Percentage out of Total Imports (%)",
#          title = paste(co,": Evolution of Percentage of Imports from", c)) +
#     theme_minimal()
#   print(p)
# }
