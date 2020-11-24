library(readr)
library(tidyverse)
library(readr)

# Paths to the Datasets
ict_indicator_path <- "/home/tarnold/R/1PrjFINAL/data/key-global-ict-indicators-from-itu_source.csv"
mapping_missing_iso_codes_path <- "/home/tarnold/R/1PrjFINAL/data/mapping_missing_iso_codes.csv"
iso_codes_path <- "/home/tarnold/R/1PrjFINAL/data/nationsonline_country_code_list.csv"
export_path <- "/home/tarnold/R/1PrjFINAL/transformed/dataset_itu_index.csv"

# Load ISO/UN Codes for Countries arround the World
iso_codes <- read_csv(iso_codes_path,
                      col_types = cols(
                        Row = col_double(),
                        Country = col_character(),
                        ISO2 = col_character(),
                        ISO3 = col_character(),
                        UN = col_character()
                      ))
names(iso_codes) <- c("Row","Country", "ISO2", "ISO3", "UN")

# Remove unused Row Column
iso_codes = subset(iso_codes, select = -c(Row) )

# Load Missing ISO/UN Codes from additional CSV
# The Country Ascension it self is part of Saint Helena, Ascension and Tristan da Cunha, therefore it was mapped as numeric value
# with intention to not further consider its data values, for completeness the data was not filtered or removed. 
mapping_missing_iso_codes <- read_csv(mapping_missing_iso_codes_path,
                                      col_types = cols(
                                        Country = col_character(),
                                        Alpha2 = col_character(),
                                        Alpha3Code = col_character(),
                                        UNCode = col_double()
                                      ))
names(mapping_missing_iso_codes) <- c("Country", "ISO2", "ISO3", "UN")


# Fix missing Ivory Coast / this is not a encoding issue / source file had perviously wrong encoding applied
IvoryCoastFix <- data.frame("CÃ´te d'Ivoire", "CI",   "CIV", 384)
names(IvoryCoastFix) <- c("Country", "ISO2", "ISO3", "UN")
# Bind Code Mapping to ISO Codes
iso_codes <- rbind(iso_codes,IvoryCoastFix)
# Append missing ISO/UN Codes to DS
iso_codes <- rbind(iso_codes,mapping_missing_iso_codes)
# Remove dupplicates by Group By
iso_codes <- iso_codes %>% group_by(Country,ISO2,ISO3,UN) %>% summarise()

# Load and specify Column Names/Datatype
ict_indicators <- read_delim(ict_indicator_path,";", escape_double = FALSE, trim_ws = TRUE,
                             col_types = cols(
                               'Date' = col_integer(),
                               'Frequency' = col_factor(levels = c("Annual")),
                               'Indicator Name' = col_factor(levels = c("Fixed-telephone subscriptions","Mobile-cellular telephone subscriptions","Fixed-broadband subscriptions","Individuals using the Internet","Proportion of households with Internet access at home","Proportion of households with Fixed line telephone","Individuals using the Internet - Male","Internet","Percentage of individuals using","Computer","Proportion of households with Mobile-cellular telephone","Proportion of households with Computer","Mobile","Individuals using the Internet - Female","Proportion of households with Radio","Active mobile-broadband subscriptions","Households with a computer","Households with Internet access at home","Use Sub Index","Access Sub Index","ICT Development Index (IDI)","Skills Sub Index","Proportion of households with TV")),
                               'INDICATOR_UNIT' = col_factor(levels = c("", "Rank=1")),
                               'Location Name' = col_factor(levels = c("CÃ´te d'Ivoire","Afghanistan","Albania","Algeria","American Samoa","Andorra","Angola","Anguilla","Antigua and Barbuda","Argentina","Armenia","Aruba","Ascension","Australia","Austria","Azerbaijan","Bahamas","Bahrain","Bangladesh","Barbados","Belarus","Belgium","Belize","Benin","Bhutan","Bermuda","Bolivia (Plurinational State of)","Bosnia and Herzegovina","Botswana","Brazil","British Virgin Islands","Brunei Darussalam","Bulgaria","Seychelles","Sierra Leone","Singapore","Slovakia","Slovenia","Solomon Islands","Somalia","South Africa","South Sudan","Spain","Sri Lanka","St. Helena","St. Pierre & Miquelon","Sudan","Suriname","Swaziland (Eswatini)","Sweden","Switzerland","Syrian Arab Republic","Taiwan, Province of China","Tajikistan","Tanzania","TFYR Macedonia","Thailand","Timor-Leste","Togo","Tokelau","Tonga","Trinidad & Tobago","Tunisia","Philippines","Poland","Portugal","Puerto Rico","Qatar","Romania","Russian Federation","Rwanda","Saint Kitts and Nevis","Saint Lucia","Saint Vincent and the Grenadines","Samoa","San Marino","Sao Tome and Principe","Saudi Arabia","Senegal","Serbia","Guyana","Haiti","Honduras","Hong Kong, China","Hungary","Iceland","India","Indonesia","Iran (Islamic Republic of)","Iraq","Ireland","Israel","Italy","Jamaica","Japan","Jersey","CIS","Europe","The Americas","Burkina Faso","Burundi","Cambodia","Cameroon","Canada","Cape Verde","Cayman Islands","Jordan","Kazakhstan","Kenya","Kiribati","Korea (Rep. of)","Kuwait","Central African Rep.","Mongolia","Chad","Chile","Colombia","Congo (Dem. Rep.)","Côte d'Ivoire","Czech Republic","Denmark","Djibouti","China","Ecuador","Comoros","Egypt","El Salvador","Equatorial Guinea","Eritrea","Estonia","Ethiopia","Fiji","Costa Rica","Croatia","Finland","France","Cuba","Cyprus","Gabon","Gambia","Georgia","Germany","Ghana","Greece","Dominica","Dominican Rep.","Grenada","Guatemala","Guinea","Guinea-Bissau","Kyrgyzstan","Lao P.D.R.","Latvia","Lebanon","Lesotho","Libya","Lithuania","Luxembourg","Macao, China","Madagascar","Malawi","Malaysia","Maldives","Mali","Malta","Mauritania","Mauritius","Mexico","Moldova","Monaco","Montenegro","Morocco","Mozambique","Myanmar","Namibia","Nepal (Republic of)","Netherlands","New Zealand","Nicaragua","Nigeria","Norway","Oman","Pakistan","Palestine","Panama","Paraguay","Peru","Turkey","Uganda","Ukraine","United Arab Emirates","United Kingdom","United States","Uruguay","Uzbekistan","Vanuatu","Viet Nam","Zambia","Zimbabwe","Venezuela","Developed","Developing","World","LDCs","Africa","Arab States","Asia & Pacific","Curacao","Liberia","Liechtenstein","Marshall Islands","Mayotte","Micronesia","Montserrat","Nauru","New Caledonia","Niger","Niue","Northern Marianas","Palau","Papua New Guinea","Turkmenistan","Turks & Caicos Is.","Tuvalu","Virgin Islands (US)","Wallis and Futuna","Yemen","Congo (Rep.)","Dem. People's Rep. of Korea","Falkland (Malvinas) Is.","Faroe Islands","French Polynesia","Gibraltar","Greenland","Guam","Guernsey","Neth. Antilles","Kosovo")),
                               'Measure Name' = col_factor(levels = c("Number","Per 100 inhabitants","Percentage","Millions","Value","Rank")),
                               'Measure Unit' = col_factor(levels = c("Number","Per 100 inhabitants","%","Millions")),
                               'Value' = col_double()))
ict_indicators <- as.data.frame(ict_indicators)

# Set Column Names of Dataset
namesOfColumns <- c("Year","Frequency","Indicator","Indicator Unit","Country","Measure Type","Measure Unit","Value")
colnames(ict_indicators) <- namesOfColumns

# Remove unused Row Column with NA Values
ict_indicators = subset(ict_indicators, select = -c(`Indicator Unit`) )

# Join ISO Codes to ITU Dataset
head(ict_indicators)
head(iso_codes)
merged_table <- left_join(ict_indicators,iso_codes,by=c("Country"="Country"), keep = TRUE)
merged_table %>% mutate_if(is.factor, as.character) -> merged_table

#Bug 04.11.2020 to assign Country
#check <- merged_table %>%
#  filter(is.na(Country.y))

# Convert DS to Tibble
dataset_itu <- as_tibble(merged_table)

# Select/Filter relevant Dataset
dataset_itu <- dataset_itu %>%
  dplyr::select("Country.x","Year","Indicator","Measure Type","Measure Unit","Value","UN") %>%
  filter(between(Year, 2008, 2019))  %>%
  filter(Indicator %in% c("Fixed-broadband subscriptions", "Fixed-telephone subscriptions","Individuals using the Internet", "Mobile-cellular telephone subscriptions","Active mobile-broadband subscriptions")) %>%
  filter(`Measure Type` %in% c("Per 100 inhabitants", "Percentage")) %>%
  filter(`Measure Unit` %in% c("Per 100 inhabitants","%"))  %>%
  arrange(desc(Year, Country) )

# Generate overview of NA Values in Dataset
dataset_itu <- merged_table %>%
  filter_all(any_vars( is.na(.)))

# Check Country Names in Dataset
dataset_itu <- merged_table %>%
  group_by(Country.x) %>% summarise()

# Find missing Country Allocations
dataset_itu <- merged_table %>%
  filter(is.na(UN)) %>% group_by(Country.x,UN) %>% summarise()

# Rewrite wrong Country Values / To be extended for further Country renaming
old_names <- c("CÃ´te d'Ivoire",
               "Hong Kong, China",
               "Macao, China",
               "Taiwan, Province of China")
new_names  <- c("Cote d'Ivoire",
                "Hong Kong",
                "Macao",
                "Taiwan")
for (i in 1:length(old_names)){
  merged_table$Country.x[merged_table$Country.x == old_names[i]] <- new_names[i]
  merged_table$Country.y[merged_table$Country.x == old_names[i]] <- new_names[i]
}

# Checking Data Mutation
dataset_itu <- merged_table %>%
  filter(is.na(UN)) %>% group_by(Country.x) %>% summarise()

# Rejoin Data Mutation Dataset with ISO Codes
merged_table <- left_join(merged_table,iso_codes,by=c("Country.x"="Country"))
dataset_itu <- merged_table

# Checking if NA Join Values disappeared - OK - Row Count dataset_itu == 0
dataset_itu <- merged_table %>%
  filter(is.na(UN.y)) %>% group_by(Country.x) %>% summarise()

# Checking Country Names - OK - Row Count dataset_itu 237
dataset_itu <- merged_table %>%
  group_by(Country.x) %>% summarise()

# Generate overview of NA Values in Dataset
dataset_itu <- as_tibble(merged_table)
dataset_itu = subset(dataset_itu, select = -c(`Measure Unit`) )
dataset_itu = subset(dataset_itu, select = -c(`ISO2.x`) )
dataset_itu = subset(dataset_itu, select = -c(`ISO3.x`) )
dataset_itu = subset(dataset_itu, select = -c(`UN.x`) )
dataset_itu = subset(dataset_itu, select = -c(`Country.y`) )
dataset_itu <- dataset_itu %>%
  filter_all(any_vars( is.na(.)))

# Finding: R interpretes 'NA' Character Values as NA Not Assigned Value definition -> counter intuitive
# Only Namibia is showing up - Namibia has ISO2 Code NA and is therefore in the overview
# Because Joining Behaviour with R on LEFT JOIN is counter intuitive as well -> Joining Value Country will be replaced with NA Value if no match
# therefore the KEEP Flag was used to perserve the initial value.  

# Export combined Index
dataset_itu <- as_tibble(merged_table)
dataset_itu = subset(dataset_itu, select = -c(`ISO2.x`) )
dataset_itu = subset(dataset_itu, select = -c(`ISO3.x`) )
dataset_itu = subset(dataset_itu, select = -c(`UN.x`) )
dataset_itu = subset(dataset_itu, select = -c(`Country.y`) )
View(dataset_itu)

#Assigning Export Dataset
data_itu_index <- dataset_itu  %>%
filter(between(Year, 2007, 2019))  %>%
  filter(Indicator %in% c("Fixed-broadband subscriptions", "Fixed-telephone subscriptions","Individuals using the Internet", "Mobile-cellular telephone subscriptions","Active mobile-broadband subscriptions")) %>%
  filter(`Measure Type` %in% c("Per 100 inhabitants", "Percentage")) %>%
  filter(`Measure Unit` %in% c("Per 100 inhabitants","%"))

# Set Column Names of Dataset
namesOfColumns_index <- c("Year","Frequency","Indicator","Country","Measure Type","Measure Unit","Value", "ISO2", "ISO3", "UN")
colnames(data_itu_index) <- namesOfColumns_index

# Writing datafile to transform folder
write.csv(data_itu_index,export_path, row.names = TRUE)

