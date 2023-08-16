library(readxl)
library(tidyverse)
library(openxlsx)
library(ggplot2)
library(openxlsx)

# Enter Country
co <- "Mali"

# Imports data for entered country
exports <- read_excel(paste("data/",co,"_Exports_and_Imports_by_Areas_and_Co.xlsx",sep=""),sheet="Exports, FOB",skip=6)
imports <- read_excel(paste("data/",co,"_Exports_and_Imports_by_Areas_and_Co.xlsx",sep=""),sheet="Imports, CIF",skip=6)


# Prepare data
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

# create dataset for breakdown by country only
exports_by_country <- exports[exports$category != "N/A" , ]
imports_by_country <- imports[imports$category != "N/A" , ]

# reshape the dataset to long format 
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

#statistical analysis
summary(exports_long$Export_Value)
summary(imports_long$Import_Value)
# Summary statistics by category
exports_summary <- exports_long %>%
  group_by(category) %>%
  summarise(
    total_exports = sum(Export_Value, na.rm = TRUE),
    average_exports = mean(Export_Value, na.rm = TRUE),
    max_exports = max(Export_Value, na.rm = TRUE),
    min_exports = min(Export_Value, na.rm = TRUE)
  )
print(exports_summary)
imports_summary <- imports_long %>%
  group_by(category) %>%
  summarise(
    total_imports = sum(Import_Value, na.rm = TRUE),
    average_exports = mean(Import_Value, na.rm = TRUE),
    max_exports = max(Import_Value, na.rm = TRUE),
    min_exports = min(Import_Value, na.rm = TRUE)
  )
print(imports_summary)

#total imports and exports per year
total_exports_by_year <- exports_long %>%
  group_by(Year) %>%
  summarize(Total_Export = sum(Export_Value, na.rm = TRUE))
total_imports_by_year <- imports_long %>%
  group_by(Year) %>%
  summarize(Total_Import = sum(Import_Value, na.rm = TRUE))
total_combined <- merge(total_exports_by_year, total_imports_by_year, by = "Year", all = TRUE)
ggplot(total_combined, aes(x = Year)) +
  geom_line(size=1.2, aes(y = Total_Export, color = "Exports")) +
  geom_line(size=1.2, aes(y = Total_Import, color = "Imports")) +
  labs(x = "Year", y = "Total Value",
       title = paste(co,": Evolution of Total Exports and Imports by Year in USD mn")) +
  scale_color_manual(values = c("Exports" = "blue", "Imports" = "red")) +
  theme_minimal()

#Exports and imports by subcategory
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
       title = paste(co, ": Evolution of Total Exports and Imports by Subcategoryin USD Mn")) +
  scale_color_manual(values = c("Exports" = "blue", "Imports" = "red")) +
  theme_minimal()

ggplot(total_imports_by_subcategory, aes(x = Year, y = Total_Import, color = subcategory)) +
  geom_line(size = 1.2) +
  labs(x = "Year", y = "Total Import Value",
       title = paste(co, ": Evolution of Total Imports by Subcategory in USD mn")) +
  scale_color_discrete(name = "Subcategory") +
  theme_minimal()

ggplot(total_exports_by_subcategory, aes(x = Year, y = Total_Export, color = subcategory)) +
  geom_line(size = 1.2) +
  labs(x = "Year", y = "Total Export Value",
       title = paste(co, ": Evolution of Total Exports by Subcategory in USD mn")) +
  scale_color_discrete(name = "Subcategory") +
  theme_minimal()

# Exports and imports to France
ggplot() +
  geom_line(size = 1.2, data = exports_long %>% filter(Country == "France"),
            aes(x = Year, y = Export_Value, color = "Exports")) +
  geom_line(size = 1.2, data = imports_long %>% filter(Country == "France"),
            aes(x = Year, y = Import_Value, color = "Imports")) +
  labs(x = "Year", y = "Total Value",
       title = paste(co, ": Evolution of Exports and Imports to/from France")) +
  scale_color_manual(values = c("Exports" = "blue", "Imports" = "red")) +
  theme_minimal()

# Exports and imports to Morocco
ggplot() +
  geom_line(size = 1.2, data = exports_long %>% filter(Country == "Morocco"),
            aes(x = Year, y = Export_Value, color = "Exports")) +
  geom_line(size = 1.2, data = imports_long %>% filter(Country == "Morocco"),
            aes(x = Year, y = Import_Value, color = "Imports")) +
  labs(x = "Year", y = "Total Value",
       title = paste(co, ": Evolution of Exports and Imports to/from Morocco")) +
  scale_color_manual(values = c("Exports" = "blue", "Imports" = "red")) +
  theme_minimal()

#top countries stacked bars chart
top_countries_exports <- exports_long %>%
  #filter(Year >= max(Year) - 4) %>% 
  group_by(Year, Country) %>%
  summarize(Total_Export = sum(Export_Value, na.rm = TRUE)) %>%
  arrange(Year, desc(Total_Export)) %>%
  group_by(Year) %>%
  mutate(Total_Value = sum(Total_Export)) %>%
  mutate(Percentage = (Total_Export / Total_Value) * 100) %>%
  slice(1:5) %>%
  ungroup()

ggplot(top_countries_exports, aes(x = Year, y = Percentage, fill = Country)) +
  geom_bar(stat = "identity") +
  labs(x = "Year", y = "Percentage of Total Export (%)",
       title = paste(co,"Top 5 Countries by Export Percentage"),
       fill = "Country") +
  scale_fill_viridis_d() +  # Use a color scale
  theme_minimal()

top_countries_imports <- imports_long %>%
  #filter(Year >= max(Year) - 4) %>% 
  group_by(Year, Country) %>%
  summarize(Total_Import = sum(Import_Value, na.rm = TRUE)) %>%
  arrange(Year, desc(Total_Import)) %>%
  group_by(Year) %>%
  mutate(Total_Value = sum(Total_Import)) %>%
  mutate(Percentage = (Total_Import / Total_Value) * 100) %>%
  slice(1:5) %>%
  ungroup()

ggplot(top_countries_imports, aes(x = Year, y = Percentage, fill = Country)) +
  geom_bar(stat = "identity") +
  labs(x = "Year", y = "Percentage of Total Import (%)",
       title = paste(co,"Top 5 Countries by Import Percentage"),
       fill = "Country") +
  scale_fill_viridis_d() +  # Use a color scale
  theme_minimal()

# years that feature france as top country
years_with_france_imports <- top_countries_imports %>%
  filter(Country == "France") %>%
  select(Year, Percentage)

years_with_france_exports <- top_countries_exports %>%
  filter(Country == "France") %>%
  select(Year, Percentage)

#growth rate
imports_france <- imports_long %>%
  filter(imports_long$Country=="France")
imports_france$Growth_Rate <- c(NA, diff(imports_france$Import_Value) / imports_france$Import_Value[-nrow(imports_france)] * 100)

exports_france <- exports_long %>%
  filter(exports_long$Country=="France")
exports_france$Growth_Rate <- c(NA, diff(exports_france$Export_Value) / exports_france$Export_Value[-nrow(exports_france)] * 100)


# total_exports_subsahara <- exports_long %>%
#   filter(subcategory == "Sub-Saharan Africa")%>%
#   group_by(Year) %>%
#   summarize(Total_Export = sum(Export_Value, na.rm = TRUE))
# 
# total_imports_subsahara <- imports_long %>%
#   filter(subcategory == "Sub-Saharan Africa") %>%
#   group_by(subcategory, Year) %>%
#   summarize(Total_Import = sum(Import_Value, na.rm = TRUE))
# 
# ggplot(total_exports_subsahara, aes(x = Year, y = Total_Export)) +
#     geom_line(size = 1.2) +
#     labs(x = "Year", y = "Total Export Value",
#          title = paste(co, ": Evolution of Total Exports to Sub-Saharan Africa in USD mn")) +
#     scale_color_discrete(name = "Subcategory") +
#     theme_minimal()
# 
# ggplot(total_imports_subsahara, aes(x = Year, y = Total_Import)) +
#     geom_line(size = 1.2) +
#     labs(x = "Year", y = "Total Import Value",
#          title = paste(co, ": Evolution of Total Imports from Sub-Saharan Africa in USD mn")) +
#     scale_color_discrete(name = "Subcategory") +
#     theme_minimal()


# Export dataset (reshaped to long format)
setwd(dir = "/Users/adniaya/Desktop/Trade data")
wb <- createWorkbook()
addWorksheet(wb, "Exports")
writeData(wb, sheet = 1, x = exports_long)
addWorksheet(wb, "Imports")
writeData(wb, sheet = 2, x = imports_long)
saveWorkbook(wb, paste(co,"Exports and Imports.xlsx"), overwrite = TRUE)
