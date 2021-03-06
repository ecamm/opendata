## This script cleans and processes the Watershed and Reservoir data collected before the Labware LIMS implementation
## on 2018-10-01.  It aligns formats and naming structures to the Open Data view.  Older reservoir
## data with multiple sites and data points have been consolidated to consistent monitoring locations
## and multiple data points for each date have been averaged.

library(tidyverse)
library(here)
library(janitor)
library(lubridate)

# import data - river sites
col_names <- names(read_csv(here("data", "prelabware_river.csv"), n_max = 0))
river <- read_csv(here("data", "prelabware_river.csv"), col_names = col_names, skip = 2, cols(
  .default = col_character()
))

# select relevant columns and rename to open_data naming structures
river <- river %>%
  select(Site:`E. coli`) %>%
  select(-`Sample Type`) %>%
  rename(
    `Sample Site` = Site,
    `Sample Date` = `Date Time`,
    `E.coli` = `E. coli`,
    `True Color` = `True Colour`,
    `Sodium Adsorption Ratio (SAR)(Calculated)` = `Sodium Adsorption Ratio (SAR) (Calculated)`,
    `Total Dissolved Solids (TDS)(Calculated)` = `Total Dissolved Solids (TDS) (Calculated)`
  )

# convert date to date_time format
river$`Sample Date` <- mdy_hm(river$`Sample Date`)

# align site names to open data format
river$`Sample Site` <- river$`Sample Site` %>%
  str_replace_all("Bow River below Ghost Dam", "Bow River Below Ghost Dam") %>%
  str_replace_all("Bow River at Hwy 22 Bridge", "Bow River Highway 22 Bridge") %>%
  str_replace_all("Bow River below Bearspaw Dam", "Bow River Below Bearspaw Dam") %>%
  str_replace_all("Bow River at Pumphouse", "Bow River Pumphouse") %>%
  str_replace_all("Bow River at Cushing Bridge", "Bow River Cushing Bridge") %>%
  str_replace_all("Bow River at Higgins Bridge", "Bow River Higgins Bridge") %>%
  str_replace_all("Bow River at Policemans Flats", "Bow River Policemans Flats") %>%
  str_replace_all("Bow River above Highwood River", "Bow River Upstream of Highwood River") %>%
  str_replace_all("Elbow River at Hwy 22 Bridge", "Elbow River Highway 22 Bridge") %>%
  str_replace_all("Elbow River above Cobble Flats", "Elbow River Cobble Flats") %>%
  str_replace_all("Elbow River above Bragg Creek", "Elbow River Above Bragg Creek") %>%
  str_replace_all("Elbow River at Twin Bridges", "Elbow River Twin Bridges") %>%
  str_replace_all("Elbow River at Sarcee Bridge", "Elbow River Sarcee Bridge") %>%
  str_replace_all("Elbow River at Weaselhead Foot Bridge", "Elbow River Weaselhead Foot Bridge") %>%
  str_replace_all("Elbow River at Sandy Beach", "Elbow River Sandy Beach") %>%
  str_replace_all("Elbow River at 9th Ave Bridge", "Elbow River 9th Ave Bridge") %>%
  str_replace_all("Ghost River at Benchlands", "Ghost River Benchlands") %>%
  str_replace_all("Jumpingpound Creek at Mouth", "Jumpingpound Creek Mouth") %>%
  str_replace_all("Prairie Creek near Mouth", "Prairie Creek Mouth") %>%
  str_replace_all("McLean Creek near Mouth", "McLean Creek Mouth") %>%
  str_replace_all("Bragg Creek at Mouth", "Bragg Creek Mouth") %>%
  str_replace_all("Nose Creek at 15 St", "Nose Creek 15th St") %>%
  str_replace_all("Nose Creek at Mouth", "Nose Creek Mouth") %>%
  str_replace_all("West Nose Creek at Mtn View Rd", "West Nose Creek Mountain View Rd") %>%
  str_replace_all("West Nose Creek at Mouth", "West Nose Creek Mouth") %>%
  str_replace_all("Fish Creek at 37 St Bridge", "Fish Creek 37th St") %>% 
  str_replace_all("Fish Creek at Mouth", "Fish Creek Mouth") %>% 
  str_replace_all("Lott Creek near Mouth", "Lott Creek Mouth")

# transform to long format
# have to keep as character format to preserve censored data (<RL)
river_long <- river %>%
  pivot_longer(
    cols = -c(`Sample Site`:`Sample Date`),
    names_to = "Parameter",
    values_to = "Formatted Result",
    values_transform = list(`Formatted Result` = "as.character")
  )

# create vectors for result unit assignments
unit_temperature <- "Water Temperature (Field)"
unit_conductance <- "Specific Conductance (Field)"
unit_do_sat <- "Dissolved Oxygen (Saturation) (Field)"
unit_bacti <- c("E.coli", "Total Coliforms", "Fecal coliforms by MF")
unit_ph <- "pH"
unit_alk <- c("Total Alkalinity", "Hardness")
unit_turb <- "Turbidity"
unit_color <- "True Color"
unit_sar <- "Sodium Adsorption Ratio (SAR)(Calculated)"

#assign units based on parameter
river_long <- river_long %>%
  mutate(
    `Result Units` = case_when(
      Parameter %in% unit_temperature ~ "°C",
      Parameter %in% unit_conductance ~ "µS/cm",
      Parameter %in% unit_do_sat ~ "%",
      Parameter %in% unit_bacti ~ "MPN/100mL",
      Parameter %in% unit_ph ~ "pH units",
      Parameter %in% unit_alk ~ "mg CaCO3/L",
      Parameter %in% unit_turb ~ "NTU",
      Parameter %in% unit_color ~ "CU",
      Parameter %in% unit_sar ~ "ratio",
      endsWith(Parameter, "(Total)") ~ "µg/L",
      endsWith(Parameter, "(Extractable)") ~ "µg/L",
      TRUE ~ "mg/L" # adds mg/L to all parameters without assigned units
    )
  )

# add dummy columns - not sure if needed for the open data join
river_long <- river_long %>%
  add_column(
    `Numeric Result` = NA,
    `Result Qualifier` = NA,
    `Analysis Name` = NA,
    `Sample Description` = NA,
    `Test Status` = NA,
    `Test Approved` = NA,
    `LIMS Component Name` = NA
  )

#assign site key based on site name
river_long <- river_long %>%
  mutate(
    `Site Key` = case_when(
      `Sample Site` == "Bow River Below Ghost Dam" ~ "SUR_BR-GD",
      `Sample Site` == "Bow River Highway 22 Bridge" ~ "SUR_BR-22",
      `Sample Site` == "Bow River Below Bearspaw Dam" ~ "SUR_BR-BD",
      `Sample Site` == "Bow River Pumphouse" ~ "SUR_BR-PH",
      `Sample Site` == "Bow River Cushing Bridge" ~ "SUR_BR-CB",
      `Sample Site` == "Bow River Higgins Bridge" ~ "SUR_BR-HB",
      `Sample Site` == "Bow River Policemans Flats" ~ "SUR_BR-PF",
      `Sample Site` == "Bow River Upstream of Highwood River" ~ "SUR_BR-HR",
      `Sample Site` == "Elbow River Cobble Flats" ~ "SUR_ER-CF",
      `Sample Site` == "Elbow River Above Bragg Creek" ~ "SUR_ER-BC",
      `Sample Site` == "Elbow River Highway 22 Bridge" ~ "SUR_ER-22",
      `Sample Site` == "Elbow River Twin Bridges" ~ "SUR_ER-TB",
      `Sample Site` == "Elbow River Sarcee Bridge" ~ "SUR_ER-SB",
      `Sample Site` == "Elbow River Weaselhead Foot Bridge" ~ "SUR_ER-WFB",
      `Sample Site` == "Elbow River Sandy Beach" ~ "SUR_ER-SYB",
      `Sample Site` == "Elbow River 9th Ave Bridge" ~ "SUR_ER-9",
      `Sample Site` == "Ghost River Benchlands" ~ "SUR_GR-BL",
      `Sample Site` == "Jumpingpound Creek Mouth" ~ "SUR_JPC-M",
      `Sample Site` == "Lott Creek Mouth" ~ "SUR_LC-M",
      `Sample Site` == "Prairie Creek Mouth" ~ "SUR_PC-M",
      `Sample Site` == "McLean Creek Mouth" ~ "SUR_MC-M",
      `Sample Site` == "Bragg Creek Mouth" ~ "SUR_BC-M",
      `Sample Site` == "Nose Creek 15th St" ~ "SUR_NC-15",
      `Sample Site` == "Nose Creek Mouth" ~ "SUR_NC-M",
      `Sample Site` == "West Nose Creek Mountain View Rd" ~ "SUR_WNC-MVR",
      `Sample Site` == "West Nose Creek Mouth" ~ "SUR_WNC-M",
      `Sample Site` == "Fish Creek 37th St" ~ "SUR_FC-37",
      `Sample Site` == "Fish Creek Mouth" ~ "SUR_FC-M",
    )
  )

#remove NAs for sample sites
river_long <- river_long %>%
  filter(!is.na(`Sample Site`))

# import coordinates as dataframe
coordinates <- tibble::tribble(
  ~`Sample Site`, ~`Latitude (Degrees)`, ~`Longitude (Degrees)`,
  "Elbow River Twin Bridges", 51.01358, -114.23731,
  "Elbow River Sandy Beach", 51.010237, -114.088008,
  "Elbow River Highway 22 Bridge", 51.032926, -114.465745,
  "Elbow River Weaselhead Foot Bridge", 50.991517, -114.146769,
  "Elbow River Above Bragg Creek", 50.943802, -114.58,
  "Fish Creek Mouth", 50.904204, -114.011341,
  "Bow River Cushing Bridge", 51.03857, -114.012943,
  "Bow River Below Bearspaw Dam", 51.10122, -114.27975,
  "Bow River Pumphouse", 51.04665, -114.114456,
  "Elbow River 9th Ave Bridge", 51.044958, -114.041782,
  "Ghost River Benchlands", 51.28111, -114.80344,
  "West Nose Creek Mountain View Rd", 51.189925, -114.141081,
  "Nose Creek 15th St", 51.170645, -114.02483,
  "Nose Creek Mouth", 51.047706, -114.019743,
  "Fish Creek 37th St", 50.929147, -114.139261,
  "West Nose Creek Mouth", 51.130121, -114.048182,
  "Bow River Below Ghost Dam", 51.22053, -114.70511,
  "Bow River Highway 22 Bridge", 51.18297, -114.48733,
  "Bow River Upstream of Highwood River", 50.81972, -113.79556,
  "Bow River Higgins Bridge", 50.938753, -114.008639,
  "Bow River Policemans Flats", 50.841969, -113.951134,
  "Jumpingpound Creek Mouth", 51.186658, -114.501906,
  "Elbow River Sarcee Bridge", 50.99533, -114.16519,
  "Elbow River Cobble Flats", 50.80519, -114.83581,
  "Prairie Creek Mouth", 50.86692, -114.78897,
  "McLean Creek Mouth", 50.90228, -114.67069,
  "Bragg Creek Mouth", 50.946924, -114.577839,
  "Lott Creek Mouth", 51.00881000,	-114.23654900,
  "Glenmore Reservoir Weaselhead", 50.98341, -114.12532,
  "Glenmore Reservoir Mid-Lake", 50.989344, -114.101033,
  "Glenmore Reservoir Heritage Cove", 50.978082, -114.10314,
  "Glenmore Reservoir Head Pond", 51.000283, -114.09816,
  "Bearspaw Reservoir West", 51.130606, -114.309915,
  "Bearspaw Reservoir Centre", 51.117405, -114.292106,
  "Bearspaw Reservoir East", 51.102391, -114.28714
)

# join coordinates to overall dataframe by Sample Site
river_long <- left_join(river_long, coordinates, by = "Sample Site")

river_long <- river_long %>%
  select(
    `Sample Site`, `Site Key`, `Sample Date`, `LIMS Component Name`, `Parameter`, `Numeric Result`,
    `Result Qualifier`, `Formatted Result`, `Result Units`, `Analysis Name`,
    `Sample Description`, `Test Status`, `Test Approved`, `Latitude (Degrees)`,
    `Longitude (Degrees)`
  )

#assign numeric result and result qualifier based on formatted result
#creates new columns for numeric values and the qualifier identification
river_long <- river_long %>%
  mutate(
    `Numeric Result` = `Formatted Result`,
    `Result Qualifier` = case_when(
      startsWith(`Formatted Result`, "<") ~ "<",
      startsWith(`Formatted Result`, ">") ~ ">",
      TRUE ~ ""
    )
  )

#remove character values to convert to column to numeric type 
river_long$`Numeric Result` <- river_long$`Numeric Result` %>%
  str_remove_all("<") %>%
  str_remove_all(">") %>%
  str_remove_all("b") %>%
  as.numeric()

river_long <- river_long %>%
  filter(`Formatted Result` != "TRUE")

# pre-2000 reservoir data - similar workflow to river data
col_names <- names(read_csv(here("data", "prelabware_reservoir.csv"), n_max = 0))
reservoir <- read_csv(here("data", "prelabware_reservoir.csv"), col_names = col_names, skip = 2, guess_max = 5000, cols(
  .default = col_character()))


reservoir <- reservoir %>%
  rename(
    `Sample Site` = Site,
    `Sample Date` = `Date Time`,
    `E.coli` = `E. coli`,
    `True Color` = `True Colour`,
    `Sodium Adsorption Ratio (SAR)(Calculated)` = `Sodium Adsorption Ratio (SAR) (Calculated)`,
    `Total Dissolved Solids (TDS)(Calculated)` = `Total Dissolved Solids (TDS) (Calculated)`
  )

reservoir$`Sample Date` <- dmy_hm(reservoir$`Sample Date`)

#renaming sites to open_data naming structure
reservoir <- reservoir %>%
  mutate(`Sample Site` = case_when(
    str_detect(`Sample Site`, "Heritage") ~ "Glenmore Reservoir Heritage Cove",
    str_detect(`Sample Site`, "Lake") ~ "Glenmore Reservoir Mid-Lake",
    str_detect(`Sample Site`, "Weaselhead") ~ "Glenmore Reservoir Weaselhead",
    str_detect(`Sample Site`, "Pond") ~ "Glenmore Reservoir Head Pond",
    TRUE ~ `Sample Site`
  ))

#remove parameter
reservoir <- reservoir %>%
  select(-"Chlorophyll a IV fluorescence")

##pre 1993 data that had multiple samples per site
pre_1993 <- reservoir %>% 
  mutate(year = year(`Sample Date`)) %>% 
  filter(year < 1993) %>% 
  select(-year)

#need to convert to numeric format to average values in next step
pre_1993 <- pre_1993 %>% 
  pivot_longer(
    cols = -c(`Sample Site`:`Sample`), names_to = "Parameter",
    values_to = "Formatted Result",
    values_transform = list(`Formatted Result` = "as.numeric")
  )

#averaging multiple results per site per day
pre_1993 <- pre_1993 %>% 
  select(-Sample) %>% 
  group_by(
    `Sample Site`, `Sample Date`, `Parameter`
  ) %>%
  mutate(`Formatted Result` = round(mean(`Formatted Result`), 3)) %>% 
  ungroup()

#convert back to character format 
pre_1993$`Formatted Result` <- as.character(pre_1993$`Formatted Result`)

pre_1993 <- pre_1993 %>% 
  mutate(
    `Numeric Result` = `Formatted Result`,
    `Result Qualifier` = case_when(
      startsWith(`Formatted Result`, "<") ~ "<",
      TRUE ~ ""
    )
  )

pre_1993$`Numeric Result` <- pre_1993$`Numeric Result` %>%
  str_remove_all("<") %>%
  str_remove_all(">") %>%
  str_remove_all("b") %>%
  as.numeric()

####
#filtering for data after 1993
reservoir <- reservoir %>% 
  mutate(year = year(`Sample Date`)) %>% 
  filter(year >= 1993) %>% 
  select(-year)

reservoir_long <- reservoir %>%
  pivot_longer(
    cols = -c(`Sample Site`:`Sample Date`), names_to = "Parameter",
    values_to = "Formatted Result",
    values_transform = list(`Formatted Result` = "as.character")
  )

reservoir_char <- reservoir_long %>%
  filter(!is.na(`Formatted Result`)) %>% 
  filter(Parameter != "Sample") %>%
  filter(`Sample Site` != "Glenmore Reservoir Screen House")

reservoir_char <- reservoir_char %>%
  mutate(
    `Numeric Result` = `Formatted Result`,
    `Result Qualifier` = case_when(
      startsWith(`Formatted Result`, "<") ~ "<",
      TRUE ~ ""
    )
  )

reservoir_char$`Numeric Result` <- reservoir_char$`Numeric Result` %>%
  str_remove_all("<") %>%
  str_remove_all(">") %>%
  str_remove_all("b") %>%
  as.numeric()

#joining pre 1993 averaged data and the rest of the reservoir data
reservoir_binded <- rbind(pre_1993, reservoir_char)
reservoir_binded <- reservoir_binded %>% 
  filter(`Sample Site`!= "Glenmore Reservoir Screen House") %>% 
  filter(`Sample Site`!= "Bearspaw Dam")

reservoir_binded <- reservoir_binded %>% 
  filter(Parameter != "Sample ID")


# create vectors for result unit assignments - unique reservoir parameters
unit_extinction <- "Extinction Coefficient (Calculated)"
unit_depth <- c("Euphotic Depth (Calculated)", "Secchi Depth", "Sample Depth", "Site Depth") 
unit_redox <- "Redox"
unit_chlorophyll <- "Chlorophyll a"

#assign units using unit vectors
reservoir_long <- reservoir_binded %>%
  mutate(
    `Result Units` = case_when(
      Parameter %in% unit_temperature ~ "°C",
      Parameter %in% unit_conductance ~ "µS/cm",
      Parameter %in% unit_do_sat ~ "%",
      Parameter %in% unit_bacti ~ "MPN/100mL",
      Parameter %in% unit_ph ~ "pH units",
      Parameter %in% unit_alk ~ "mg CaCO3/L",
      Parameter %in% unit_turb ~ "NTU",
      Parameter %in% unit_color ~ "CU",
      Parameter %in% unit_sar ~ "ratio",
      Parameter %in% unit_extinction ~ "µE/s/m2",
      Parameter %in% unit_depth ~ "m",
      Parameter %in% unit_redox ~ "mV",
      Parameter %in% unit_chlorophyll ~ "µg/L",
      TRUE ~ "mg/L" # adds mg/L to all parameters without assigned units
    )
  )

# add dummy columns - not sure if needed for the open data join
reservoir_long <- reservoir_long %>%
  add_column(
    `Analysis Name` = NA,
    `Sample Description` = NA,
    `Test Status` = NA,
    `Test Approved` = NA,
    `LIMS Component Name` = NA
  )

#assign site keys 
reservoir_long <- reservoir_long %>%
  mutate(
    `Site Key` = case_when(
      `Sample Site` == "Bearspaw Reservoir East" ~ "SUR_BP-E",
      `Sample Site` == "Bearspaw Reservoir Centre" ~ "SUR_BP-C",
      `Sample Site` == "Bearspaw Reservoir West" ~ "SUR_BP-W",
      `Sample Site` == "Glenmore Reservoir Weaselhead" ~ "SUR_GM-WH",
      `Sample Site` == "Glenmore Reservoir Heritage Cove" ~ "SUR_GM-HC",
      `Sample Site` == "Glenmore Reservoir Mid-Lake" ~ "SUR_GM-ML",
      `Sample Site` == "Glenmore Reservoir Head Pond" ~ "SUR_GM-HP"
    )
  )


# join coordinates to overall dataframe by Sample Site
reservoir_long <- left_join(reservoir_long, coordinates, by = "Sample Site")

#select parameters aligned to open_data format
reservoir_long <- reservoir_long %>%
  select(
    `Sample Site`, `Site Key`, `Sample Date`, `LIMS Component Name`, `Parameter`, `Numeric Result`,
    `Result Qualifier`, `Formatted Result`, `Result Units`, `Analysis Name`,
    `Sample Description`, `Test Status`, `Test Approved`, `Latitude (Degrees)`,
    `Longitude (Degrees)`
  )

reservoir_long <- distinct(reservoir_long) #remove duplicate values

# join river and reservoir dataframes
pre_2018 <- rbind(river_long, reservoir_long)
glimpse(pre_2018)

#filtering out NA values for results
pre_2018_filtered <- pre_2018 %>%
  filter(!is.na(`Formatted Result`))

# janitor::get_dupes(pre_2000_filtered)  #22 duplicate entries from Sarcee Bridge
#filtering out duplicate values
pre_2018_filtered <- pre_2018_filtered %>%
  distinct()



##### data standards issues
##### dealing with values entered erroneously as zero, decimal places, reporting limits

# one entry of total coliforms with zero entry for result
pre_2018_filtered <- pre_2018_filtered %>%
  mutate(
    `Formatted Result` = case_when(
      Parameter == "Total Coliforms" & `Formatted Result` == 0 ~ "<1",
      TRUE ~ `Formatted Result`
    )
  ) %>%
  mutate(
    `Numeric Result` = `Formatted Result`,
    `Result Qualifier` = case_when(
      startsWith(`Formatted Result`, "<") ~ "<",
      TRUE ~ ""
    )
  ) 

pre_2018_filtered$`Numeric Result` <- pre_2018_filtered$`Numeric Result` %>%
  str_remove_all("<") %>%
  str_remove_all(">") %>%
  str_remove_all("b") %>%
  as.numeric()



# Total Organic Carbon <2.0 mg/L
pre_2018_filtered <- pre_2018_filtered %>%
  mutate(
    `Formatted Result` = case_when(
      Parameter == "Total Organic Carbon (TOC)" & `Formatted Result` < 0.2 ~ "<0.2",
      TRUE ~ `Formatted Result`
    )
  ) %>%
  mutate(
    `Numeric Result` = `Formatted Result`,
    `Result Qualifier` = case_when(
      startsWith(`Formatted Result`, "<") ~ "<",
      TRUE ~ ""
    )
  )

pre_2018_filtered$`Numeric Result` <- pre_2018_filtered$`Numeric Result` %>%
  str_remove_all("<") %>%
  str_remove_all(">") %>%
  str_remove_all("b") %>%
  as.numeric()

pre_2018_filtered <- pre_2018_filtered %>%
  mutate_at(
    .vars = vars(`Numeric Result`),
    .funs = ~ case_when(
      Parameter == "Total Organic Carbon (TOC)" ~ round(.x, digits = 1), # round TOC values
      TRUE ~ `Numeric Result`
    )
  )
# replace formatted result
pre_2018_filtered <- pre_2018_filtered %>%
  mutate_at(
    .vars = vars(`Formatted Result`),
    .funs = ~ case_when(
      Parameter == "Total Organic Carbon (TOC)" ~ as.character(`Numeric Result`),
      TRUE ~ as.character(`Formatted Result`)
    )
  )

#to round Formatted Results, have to convert to numeric type, then reapply qualifier character to column
pre_2018_filtered <- pre_2018_filtered %>%
  mutate(
    `Formatted Result` = case_when(
      Parameter == "Total Organic Carbon (TOC)" ~ paste(`Result Qualifier`,`Numeric Result`, sep = ""),
      TRUE ~ `Formatted Result`
    )
  )


# TSS <0.5 mg/L, dataset has historical indication of RL = 0.5
pre_2018_filtered <- pre_2018_filtered %>%
  mutate(
    `Formatted Result` = case_when(
      Parameter == "Total Suspended Solids (TSS)" & `Formatted Result` < 0.5 ~ "<0.5",
      TRUE ~ `Formatted Result`
    )
  ) %>%
  mutate(
    `Numeric Result` = `Formatted Result`,
    `Result Qualifier` = case_when(
      startsWith(`Formatted Result`, "<") ~ "<",
      TRUE ~ ""
    )
  )

pre_2018_filtered$`Numeric Result` <- pre_2018_filtered$`Numeric Result` %>%
  str_remove_all("<") %>%
  str_remove_all(">") %>%
  str_remove_all("b") %>%
  as.numeric()

# True Color <1 CU, historical RL of 1
pre_2018_filtered <- pre_2018_filtered %>%
  mutate(
    `Formatted Result` = case_when(
      Parameter == "True Color" & `Formatted Result` < 1 ~ "<1",
      TRUE ~ `Formatted Result`
    )
  ) %>%
  mutate(
    `Numeric Result` = `Formatted Result`,
    `Result Qualifier` = case_when(
      startsWith(`Formatted Result`, "<") ~ "<",
      TRUE ~ ""
    )
  )

pre_2018_filtered$`Numeric Result` <- pre_2018_filtered$`Numeric Result` %>%
  str_remove_all("<") %>%
  str_remove_all(">") %>%
  str_remove_all("b") %>%
  as.numeric()

# Hardness value with decimal places
pre_2018_filtered <- pre_2018_filtered %>%
  mutate_at(
    .vars = vars(`Numeric Result`),
    .funs = ~ case_when(
      Parameter == "Hardness" ~ round(.x, digits = 0), # round hardness values
      TRUE ~ `Numeric Result`
    )
  )
# replace formatted result
pre_2018_filtered <- pre_2018_filtered %>%
  mutate_at(
    .vars = vars(`Formatted Result`),
    .funs = ~ case_when(
      Parameter == "Hardness" ~ as.character(`Numeric Result`),
      TRUE ~ as.character(`Formatted Result`)
    )
  )

#pH values rounded to 1 decimal place
pre_2018_filtered <- pre_2018_filtered %>%
  mutate_at(
    .vars = vars(`Numeric Result`),
    .funs = ~ case_when(
      Parameter == "pH" ~ round(.x, digits = 1), # round pH values
      TRUE ~ `Numeric Result`
    )
  )
# replace formatted result
pre_2018_filtered <- pre_2018_filtered %>%
  mutate_at(
    .vars = vars(`Formatted Result`),
    .funs = ~ case_when(
      Parameter == "pH" ~ as.character(`Numeric Result`),
      TRUE ~ as.character(`Formatted Result`)
    )
  )

write_excel_csv(pre_2018_filtered, here("output", "prelabware_watershed_final.csv")) # need to use write_excel_csv to account for special characters with UTF-8 encoding

