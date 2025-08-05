# ==============================================
# Script: Field Shapefile Processing for Rice Data
# Purpose: Match field boundaries with planting/harvest dates by year
# Author: [Your Name]
# Date: [Current Date]

# OVERVIEW:
# This script processes agricultural field boundary shapefiles and matches them with
# planting/harvest date information for rice fields across multiple farms. The main
# objectives are:
#
# 1. Standardize field names across different data sources
# 2. Match field boundaries with planting/harvest dates by year
# 3. Create yearly shapefiles for analysis
# 4. Generate merged datasets for comprehensive analysis
# 5. Export JavaScript code for visualization in Earth Engine

# DATA SOURCES:
# - Field boundary shapefiles from multiple farms (Arva, DWMRU, Matt Morris, Sullivan, etc.)
# - Planting/harvest date records from farm management systems
# - Additional reference data (points of interest, etc.)

# MAIN PROCESSING STEPS:
# 1. Load and clean field boundary shapefiles for each farm
# 2. Standardize field naming conventions across datasets
# 3. Match field boundaries with planting/harvest dates by year
# 4. Process each farm's data separately (Arva, DWMRU, etc.)
# 5. Merge all data into yearly composite shapefiles
# 6. Create a final merged dataset covering all years
# 7. Generate JavaScript code for visualization
# ==============================================

# ----------------------------
# 1. LIBRARIES
# ----------------------------
library(readxl)
library(lubridate)
library(dplyr)
library(purrr)
library(writexl)
library(ggplot2)
library(stringr)
library(sf)

# ----------------------------
# 3. ARVA FIELDS PROCESSING
# ----------------------------

# Define the file path
shapefile_path <- "C:/Users/rbmahbub/Documents/Data/DOPDOH-paper/ShapefileData/Arva/SHAPEFILE/Arvafieldboundaries.shp"
arvashp <- st_read(shapefile_path)# Read the shapefile

# Concatenate the 'field_id' and 'field_name' columns to create the 'FIELD_NAME' column
arvashp$FIELD_NAME <- paste('F',arvashp$field_id, arvashp$field_name, sep = "_")
arvashp <- arvashp %>%# Use dplyr's mutate and gsub to edit the FIELD_NAME column
  mutate(FIELD_NAME = FIELD_NAME %>%
           gsub(" - ", "_", .) %>%
           gsub("#", "_", .) %>%
           gsub("Reservoir", "Res", .) %>%
           gsub("South", "S", .) %>%
           gsub("North", "N", .) %>%
           gsub("East", "E", .) %>%
           gsub("Airstrip", "AP", .) %>%
           gsub("Main Field", "MF", .) %>%
           gsub("Willingham", "Wg", .) %>%
           gsub("Person", "Pn", .) %>%
           gsub("Martin Place", "MP", .) %>%
           gsub("Black Shop", "BS", .) %>%
           gsub("Smith Half Moon", "SHM", .) %>%
           gsub("Smith", "Sm", .) %>%
           gsub("Hemp Field", "HF", .) %>%
           gsub("West", "W", .) %>%
           gsub("Birdoe", "BD", .) %>%
           gsub("Carter", "Cr", .) %>%  # Replace "Carter" with "Cr"
           gsub(" ", "_", .) %>%  # Replace spaces with "_"
           gsub("__", "_", .))  # Replace double underscores with a single "_"

# Create 'FIELD_NAME' column
arvashp <- arvashp %>%
  dplyr::select(-fid, -field_id, -field_name, -farm_name)  # Remove unwanted columns

arva_datashp_2015 <- arva_rice_data %>% dplyr::filter(YEAR == 2015) %>% select(-PD, -HD)
arvamatched_2015 <- inner_join(arva_datashp_2015, arvashp, by = "FIELD_NAME") %>% 
  select(-field_hash, -total_acre)
unmatched_2015 <- anti_join(arva_datashp_2015, arvashp, by = "FIELD_NAME")
output_2015 <- "C:/Users/rbmahbub/Documents/Data/DOPDOH-paper/ShapefileData/Arva/SHAPEFILE/RenamedFieldNames/arvaFieldNAMEPD_2015.shp"
st_write(arvamatched_2015, output_2015, delete_layer = TRUE)
cat("Shapefile saved to:", output_2015, "\n")

arva_datashp_2016 <- arva_rice_data %>% dplyr::filter(YEAR == 2016) %>% select(-PD, -HD)
arvamatched_2016 <- inner_join(arva_datashp_2016, arvashp, by = "FIELD_NAME") %>% 
  select(-field_hash, -total_acre)
unmatched_2016 <- anti_join(arva_datashp_2016, arvashp, by = "FIELD_NAME")
output_2016 <- "C:/Users/rbmahbub/Documents/Data/DOPDOH-paper/ShapefileData/Arva/SHAPEFILE/RenamedFieldNames/arvaFieldNAMEPD_2016.shp"
st_write(arvamatched_2016, output_2016, delete_layer = TRUE)
cat("Shapefile saved to:", output_2016, "\n")

arva_datashp_2017 <- arva_rice_data %>% dplyr::filter(YEAR == 2017) %>% select(-PD, -HD)
arvamatched_2017 <- inner_join(arva_datashp_2017, arvashp, by = "FIELD_NAME") %>% 
  select(-field_hash, -total_acre)
unmatched_2017 <- anti_join(arva_datashp_2017, arvashp, by = "FIELD_NAME")
output_2017 <- "C:/Users/rbmahbub/Documents/Data/DOPDOH-paper/ShapefileData/Arva/SHAPEFILE/RenamedFieldNames/arvaFieldNAMEPD_2017.shp"
st_write(arvamatched_2017, output_2017, delete_layer = TRUE)
cat("Shapefile saved to:", output_2017, "\n")

arva_datashp_2018 <- arva_rice_data %>% dplyr::filter(YEAR == 2018) %>% select(-PD, -HD)
arvamatched_2018 <- inner_join(arva_datashp_2018, arvashp, by = "FIELD_NAME") %>% 
  select(-field_hash, -total_acre)
unmatched_2018 <- anti_join(arva_datashp_2018, arvashp, by = "FIELD_NAME")
output_2018 <- "C:/Users/rbmahbub/Documents/Data/DOPDOH-paper/ShapefileData/Arva/SHAPEFILE/RenamedFieldNames/arvaFieldNAMEPD_2018.shp"
st_write(arvamatched_2018, output_2018, delete_layer = TRUE)
cat("Shapefile saved to:", output_2018, "\n")

arva_datashp_2019 <- arva_rice_data %>% dplyr::filter(YEAR == 2019) %>% select(-PD, -HD)
arvamatched_2019 <- inner_join(arva_datashp_2019, arvashp, by = "FIELD_NAME") %>% 
  select(-field_hash, -total_acre)
unmatched_2019 <- anti_join(arva_datashp_2019, arvashp, by = "FIELD_NAME")
output_2019 <- "C:/Users/rbmahbub/Documents/Data/DOPDOH-paper/ShapefileData/Arva/SHAPEFILE/RenamedFieldNames/arvaFieldNAMEPD_2019.shp"
st_write(arvamatched_2019, output_2019, delete_layer = TRUE)
cat("Shapefile saved to:", output_2019, "\n")

arva_datashp_2020 <- arva_rice_data %>% dplyr::filter(YEAR == 2020) %>% select(-PD, -HD)
arvamatched_2020 <- inner_join(arva_datashp_2020, arvashp, by = "FIELD_NAME") %>% 
  select(-field_hash, -total_acre)
unmatched_2020 <- anti_join(arva_datashp_2020, arvashp, by = "FIELD_NAME")
output_2020 <- "C:/Users/rbmahbub/Documents/Data/DOPDOH-paper/ShapefileData/Arva/SHAPEFILE/RenamedFieldNames/arvaFieldNAMEPD_2020.shp"
st_write(arvamatched_2020, output_2020, delete_layer = TRUE)
cat("Shapefile saved to:", output_2020, "\n")

arva_datashp_2021 <- arva_rice_data %>% dplyr::filter(YEAR == 2021) %>% select(-PD, -HD)
arvamatched_2021 <- inner_join(arva_datashp_2021, arvashp, by = "FIELD_NAME") %>% 
  select(-field_hash, -total_acre)
unmatched_2021 <- anti_join(arva_datashp_2021, arvashp, by = "FIELD_NAME")
output_2021 <- "C:/Users/rbmahbub/Documents/Data/DOPDOH-paper/ShapefileData/Arva/SHAPEFILE/RenamedFieldNames/arvaFieldNAMEPD_2021.shp"
st_write(arvamatched_2021, output_2021, delete_layer = TRUE)
cat("Shapefile saved to:", output_2021, "\n")


arva_datashp_2022 <- arva_rice_data %>% dplyr::filter(YEAR == 2022) %>% select(-PD, -HD)
arvamatched_2022 <- inner_join(arva_datashp_2022, arvashp, by = "FIELD_NAME") %>% 
  select(-field_hash, -total_acre)
unmatched_2022 <- anti_join(arva_datashp_2022, arvashp, by = "FIELD_NAME")
output_2022 <- "C:/Users/rbmahbub/Documents/Data/DOPDOH-paper/ShapefileData/Arva/SHAPEFILE/RenamedFieldNames/arvaFieldNAMEPD_2022.shp"
st_write(arvamatched_2022, output_2022, delete_layer = TRUE)
cat("Shapefile saved to:", output_2022, "\n")


# Define the file path
shapefile_path <- "C:/Users/rbmahbub/Documents/Data/DOPDOH-paper/ShapefileData/COMBINEDSHAPEFILESHEET/PDHDcombinedshpgtd.shp"
# Read the shapefile
pdhd_shapefile <- st_read(shapefile_path)
# Print summary
pdhd_shapefile <- pdhd_shapefile %>%
  mutate(FIELD_NAME = str_trim(FIELD_NAME))  # Removes leading/trailing spaces and newline characters
arva_datashp_2023 <- arva_rice_data %>% dplyr::filter(YEAR == 2023) %>% select(-PD, -HD)
arvamatched_2023 <- inner_join(arva_datashp_2023, arvashp, by = "FIELD_NAME") %>% 
  select(-field_hash, -total_acre)
unmatched_2023 <- anti_join(arva_datashp_2023, arvashp, by = "FIELD_NAME")

output_2023 <- "C:/Users/rbmahbub/Documents/Data/DOPDOH-paper/ShapefileData/Arva/SHAPEFILE/RenamedFieldNames/arvaFieldNAMEPD_2023.shp"
st_write(arvamatched_2023, output_2023, delete_layer = TRUE)
cat("Shapefile saved to:", output_2023, "\n")

arva_datashp_2024 <- arva_rice_data %>% dplyr::filter(YEAR == 2024) %>% select(-PD, -HD)
arvamatched_2024 <- inner_join(arva_datashp_2024, arvashp, by = "FIELD_NAME") %>% 
  select(-field_hash, -total_acre)
unmatched_2024 <- anti_join(arva_datashp_2024, arvashp, by = "FIELD_NAME")
output_2024 <- "C:/Users/rbmahbub/Documents/Data/DOPDOH-paper/ShapefileData/Arva/SHAPEFILE/RenamedFieldNames/arvaFieldNAMEPD_2024.shp"
st_write(arvamatched_2024, output_2024, delete_layer = TRUE)
cat("Shapefile saved to:", output_2024, "\n")

# ----------------------------
# 5. -----DWMRU ----------
# ----------------------------
# Define the file path for the DWMRU shapefile
shapefile_dwmru_path <- "C:/Users/rbmahbub/Documents/Data/DOPDOH-paper/ShapefileData/DWMRU/DWMRU.shp"
DWMRU_shp <- st_read(shapefile_dwmru_path)# Read the DWMRU shapefile

# Drop specified columns and rename 'Name' to 'FIELD_NAME'
DWMRU_shp <- DWMRU_shp %>%
  select(-descriptio, -timestamp, -begin, -end, -altitudeMo, -tessellate, -extrude, -visibility, -drawOrder, -icon) %>%
  rename(FIELD_NAME = Name)

# Replace the values in the 'Field' column based on the given rules
DWMRU_shp <- DWMRU_shp %>%
  mutate(FIELD_NAME = case_when(
    FIELD_NAME == "Greenfield-NW19" ~ "Greenfield_NW19",
    FIELD_NAME == "NW18" ~ "NW_18",
    FIELD_NAME == "403" ~ "F_403",
    FIELD_NAME == "404" ~ "F_404",
    FIELD_NAME == "4N" ~ "F_4N",
    FIELD_NAME == "201" ~ "F_201",
    FIELD_NAME == "202" ~ "F_202",
    FIELD_NAME == "16SW" ~ "F_16SW",
    FIELD_NAME == "16NW" ~ "F_16NW",
    TRUE ~ as.character(FIELD_NAME)  # Keep other values unchanged
  ))

# For 2017
dwmru_datashp_2017 <- DWMRU_rice_data %>% dplyr::filter(YEAR == 2017) 
matched_2017 <- inner_join(dwmru_datashp_2017, DWMRU_shp, by = "FIELD_NAME") 
unmatched_2017 <- anti_join(dwmru_datashp_2017, DWMRU_shp, by = "FIELD_NAME")
output_2017 <- "C:/Users/rbmahbub/Documents/Data/DOPDOH-paper/ShapefileData/DWMRU/Yearwise/DWMRUFieldNAMEPD_2017.shp"

# Convert to 2D by removing the Z-coordinate from the geometry
dwmru_datashp_2017 <- st_as_sf(matched_2017)
dwmru_datashp_2017 <- st_zm(dwmru_datashp_2017)
st_write(dwmru_datashp_2017, output_2017, delete_layer = TRUE)
cat("Shapefile saved to:", output_2017, "\n")

# For 2018
dwmru_datashp_2018 <- DWMRU_rice_data %>%dplyr:: filter(YEAR == 2018) 
matched_2018 <- inner_join(dwmru_datashp_2018, DWMRU_shp, by = "FIELD_NAME") 
unmatched_2018 <- anti_join(dwmru_datashp_2018, DWMRU_shp, by = "FIELD_NAME")
output_2018 <- "C:/Users/rbmahbub/Documents/Data/DOPDOH-paper/ShapefileData/DWMRU/Yearwise/DWMRUFieldNAMEPD_2018.shp"
dwmru_datashp_2018 <- st_as_sf(matched_2018)
dwmru_datashp_2018 <- st_zm(dwmru_datashp_2018)
st_write(dwmru_datashp_2018, output_2018, delete_layer = TRUE)
cat("Shapefile saved to:", output_2018, "\n")

# For 2020
dwmru_datashp_2020 <- DWMRU_rice_data %>% dplyr::filter(YEAR == 2020)
matched_2020 <- inner_join(dwmru_datashp_2020, DWMRU_shp, by = "FIELD_NAME")
unmatched_2020 <- anti_join(dwmru_datashp_2020, DWMRU_shp, by = "FIELD_NAME")
output_2020 <- "C:/Users/rbmahbub/Documents/Data/DOPDOH-paper/ShapefileData/DWMRU/Yearwise/DWMRUFieldNAMEPD_2020.shp"
dwmru_datashp_2020 <- st_as_sf(matched_2020)
dwmru_datashp_2020 <- st_zm(dwmru_datashp_2020)
st_write(dwmru_datashp_2020, output_2020, delete_layer = TRUE)
cat("Shapefile saved to:", output_2020, "\n")

# For 2021
dwmru_datashp_2021 <- DWMRU_rice_data %>% dplyr::filter(YEAR == 2021)
matched_2021 <- inner_join(dwmru_datashp_2021, DWMRU_shp, by = "FIELD_NAME")
unmatched_2021 <- anti_join(dwmru_datashp_2021, DWMRU_shp, by = "FIELD_NAME")
output_2021 <- "C:/Users/rbmahbub/Documents/Data/DOPDOH-paper/ShapefileData/DWMRU/Yearwise/DWMRUFieldNAMEPD_2021.shp"
dwmru_datashp_2021 <- st_as_sf(matched_2021)
dwmru_datashp_2021 <- st_zm(dwmru_datashp_2021)
st_write(dwmru_datashp_2021, output_2021, delete_layer = TRUE)
cat("Shapefile saved to:", output_2021, "\n")

# For 2022
dwmru_datashp_2022 <- DWMRU_rice_data %>% dplyr::filter(YEAR == 2022) 
matched_2022 <- inner_join(dwmru_datashp_2022, DWMRU_shp, by = "FIELD_NAME") 
unmatched_2022 <- anti_join(dwmru_datashp_2022, DWMRU_shp, by = "FIELD_NAME")
output_2022 <- "C:/Users/rbmahbub/Documents/Data/DOPDOH-paper/ShapefileData/DWMRU/Yearwise/DWMRUFieldNAMEPD_2022.shp"
dwmru_datashp_2022 <- st_as_sf(matched_2022)
dwmru_datashp_2022 <- st_zm(dwmru_datashp_2022)
st_write(dwmru_datashp_2022, output_2022, delete_layer = TRUE)
cat("Shapefile saved to:", output_2022, "\n")

# For 2023
dwmru_datashp_2023 <- DWMRU_rice_data %>% dplyr::filter(YEAR == 2023) 
matched_2023 <- inner_join(dwmru_datashp_2023, DWMRU_shp, by = "FIELD_NAME")
unmatched_2023 <- anti_join(dwmru_datashp_2023, DWMRU_shp, by = "FIELD_NAME")
output_2023 <- "C:/Users/rbmahbub/Documents/Data/DOPDOH-paper/ShapefileData/DWMRU/Yearwise/DWMRUFieldNAMEPD_2023.shp"
dwmru_datashp_2023 <- st_as_sf(matched_2023)
dwmru_datashp_2023 <- st_zm(dwmru_datashp_2023)
st_write(dwmru_datashp_2023, output_2023, delete_layer = TRUE)
cat("Shapefile saved to:", output_2023, "\n")

# For 2024
dwmru_datashp_2024 <- DWMRU_rice_data %>% dplyr::filter(YEAR == 2024) 
matched_2024 <- inner_join(dwmru_datashp_2024, DWMRU_shp, by = "FIELD_NAME")
unmatched_2024 <- anti_join(dwmru_datashp_2024, DWMRU_shp, by = "FIELD_NAME")
output_2024 <- "C:/Users/rbmahbub/Documents/Data/DOPDOH-paper/ShapefileData/DWMRU/Yearwise/DWMRUFieldNAMEPD_2024.shp"
dwmru_datashp_2024  <- st_as_sf(matched_2024)
dwmru_datashp_2024  <- st_zm(dwmru_datashp_2024 )
st_write(dwmru_datashp_2024 , output_2024, delete_layer = TRUE)
cat("Shapefile saved to:", output_2024, "\n")

# ----------------------------
#  MATT MORRIS
# ----------------------------
# Define the file path for the Matt Morris shapefile
shapefile_mattmorris_path <- "C:/Users/rbmahbub/Documents/Data/DOPDOH-paper/ShapefileData/MattMorrisShapefile/MattMorrisFarm.shp"
MattMorris_shp <- st_read(shapefile_mattmorris_path)# Read the Matt Morris shapefile
# Drop specified columns and rename 'Name' to 'FIELD_NAME'
MattMorris_shp <- MattMorris_shp %>%
  select(-descriptio, -timestamp, -begin, -end, -altitudeMo, -tessellate, -extrude, -visibility, -drawOrder, -icon) %>%
  rename(FIELD_NAME = Name)

matt_morris_data_2024 <- matt_morris_data %>% dplyr::filter(YEAR == 2024)
matched_2024 <- inner_join(matt_morris_data_2024, MattMorris_shp, by = "FIELD_NAME")
unmatched_2024 <- anti_join(matt_morris_data_2024, MattMorris_shp, by = "FIELD_NAME")
output_2024 <- "C:/Users/rbmahbub/Documents/Data/DOPDOH-paper/ShapefileData/MattMorrisShapefile/Yearwise/MattMorrisFieldNAMEPD_2024.shp"
matt_morris_datashp_2024  <- st_as_sf(matched_2024)
matt_morris_datashp_2024  <- st_zm(matt_morris_datashp_2024 )
st_write(matt_morris_datashp_2024 , output_2024, delete_layer = TRUE)
cat("Shapefile saved to:", output_2024, "\n")

# ----------------------------
# SULLIVAN
# ----------------------------
# Define the file path for the Sullivan shapefile
shapefile_sullivan_path <- "C:/Users/rbmahbub/Documents/Data/DOPDOH-paper/ShapefileData/RyanMoore_Sullivan/2023_SFA_Rice_Boundaries.shp"
sullivanshp <- st_read(shapefile_sullivan_path) %>% select(c(-FIELD_ID, -ORG_ID, -CLIENT_ID, -POLYGONTYP,
                                                             -FARM_ID, -CLIENT_NAM, -FARM_NAME))# Read the Sullivan shapefile
ryan_moore_sullivan_data_2023 <- ryan_moore_sullivan_data %>% dplyr::filter(YEAR == 2023) ## ONLY ONE YEAR
matched_2023 <- inner_join(ryan_moore_sullivan_data_2023, sullivanshp, by = "FIELD_NAME")
unmatched_2023 <- anti_join(ryan_moore_sullivan_data_2023, sullivanshp, by = "FIELD_NAME")
output_2023 <- "C:/Users/rbmahbub/Documents/Data/DOPDOH-paper/ShapefileData/RyanMoore_Sullivan/Yearwise/SullivanPD_2024.shp"
sullivan_data_shp_2023  <- st_as_sf(matched_2023)
sullivan_data_shp_2023  <- st_zm(sullivan_data_shp_2023 )
st_write(sullivan_data_shp_2023 , output_2024, delete_layer = TRUE)
cat("Shapefile saved to:", output_2024, "\n")


# ----------------------------
# 6. SCOTT SEEDING RICE DATA
# ----------------------------
# Define the file path for the Scott Matthews shapefile
shapefile_scottmatthews_path <- "C:/Users/rbmahbub/Documents/Data/DOPDOH-paper/ShapefileData/ScottMatthewsShapefile/boundaries/boundaries.shp"
scottmatthewshp <- st_read(shapefile_scottmatthews_path)# Read the Scott Matthews shapefile

scottmatthewshp<-scottmatthewshp %>%
  rename(FIELD_NAME = FIELD_NAME) %>%
  mutate(FIELD_NAME = paste(FARM_NAME, FIELD_NAME, sep = "_"))%>% select(-FARM_NAME , -CLIENT_NAM, 
                                                                         -POLYGONTYP, -CLIENT_ID,
                                                                         -FARM_ID, -FIELD_ID, -ORG_ID)

seeding_rice_data_2024 <- seeding_rice_data %>%
  dplyr::filter(YEAR == 2024)
matched_2024 <- inner_join(seeding_rice_data_2024, scottmatthewshp, by = "FIELD_NAME")
unmatched_2024 <- anti_join(seeding_rice_data_2024, scottmatthewshp, by = "FIELD_NAME")
output_2024 <- "C:/Users/rbmahbub/Documents/Data/DOPDOH-paper/ShapefileData/ScottMatthewsShapefile/Yearwise/seedingrice_shp_2024_2024.shp"
seedingrice_shp_2024  <- st_as_sf(matched_2024)
seedingrice_shp_2024  <- st_zm(seedingrice_shp_2024)
st_write(seedingrice_shp_2024 , output_2024, delete_layer = TRUE)
cat("Shapefile saved to:", output_2024, "\n")



# ----------------------------
#  UNILEVER
# ----------------------------
# Define the file path for the Unilever shapefile
shapefile_unilever_path <- "C:/Users/rbmahbub/Documents/Data/DOPDOH-paper/ShapefileData/Unilever/ShapefileRiceFieldsLastUpdate_clippedbyisbellpolygons_2324/UnileverFS_diff_ISbell_polygons_2324.shp"
unilever_shp <- st_read(shapefile_unilever_path)%>% select(-c( OID_, FolderPath, SymbolID, 
                                                               AltMode, Base, Clamped,,Extruded ,
                                                               Snippet ,PopupInfo, Shape_Leng, 
                                                               Shape_Area, Field_Area, Farm, Rotation, Years_Rice,
                                                               descriptio, timestamp, begin, end, altitudeMo, 
                                                               tessellate, extrude, visibility, drawOrder, icon,layer, path))

# Replace the values in the 'Field' column based on the given rules
unilever_shp <- unilever_shp %>%
  rename(FIELD_NAME = Name)  %>%
  mutate(FIELD_NAME = case_when(
    FIELD_NAME == "1-Farm 8" ~ "F_1_Farm_8",
    FIELD_NAME == "4East-Farm 8" ~ "F_4East_Farm_8",
    FIELD_NAME == "6E-Farm 8" ~ "F_6E_Farm_8",
    FIELD_NAME == "B5-Farm 5" ~ "B5_Farm_5",
    FIELD_NAME == "Field 2-Farm 8" ~ "Field_2_Farm_8",
    FIELD_NAME == "G2-Farm 5" ~ "G2_Farm_5",
    FIELD_NAME == "H110-Farm 3" ~ "H110_Farm_3",
    FIELD_NAME == "HT1-Farm 1" ~ "HT1_Farm_1",
    FIELD_NAME == "K24-Farm 3" ~ "K24_Farm_3",
    FIELD_NAME == "K140E-Farm 3" ~ "K140E_Farm_03",
    FIELD_NAME == "K140W-Farm 3" ~ "K140W_Farm_03",
    FIELD_NAME == "K164-Farm 3" ~ "K164_Farm_03",
    FIELD_NAME == "LIB2-Farm 1" ~ "LIB2_Farm_01",
    FIELD_NAME == "LOB-Farm 1" ~ "LOB_Farm_01",
    FIELD_NAME == "N4-Farm 5" ~ "N4_Farm_05",
    FIELD_NAME == "North 0-Farm 9" ~ "North_0_Farm_09",
    FIELD_NAME == "P2-Farm 1" ~ "P2_Farm_01",
    FIELD_NAME == "R10E-Farm 2" ~ "R10E_Farm_02",
    FIELD_NAME == "R10W-Farm 2" ~ "R10W_Farm_02",
    FIELD_NAME == "R2-Farm 2" ~ "R2_Farm_02",
    FIELD_NAME == "Regions 40-Farm 6" ~ "Regions_40_Farm_06",
    FIELD_NAME == "RS 5W-Farm 2" ~ "RS_5W_Farm_02",
    FIELD_NAME == "S01-Farm 1" ~ "S01_Farm_01",
    FIELD_NAME == "S03-Farm 1" ~ "S03_Farm_01",
    FIELD_NAME == "South 0-Farm 9" ~ "South_0_Farm_09",
    FIELD_NAME == "T11" ~ "T11",
    FIELD_NAME == "T18M-Farm 5" ~ "T18M_Farm_05",
    FIELD_NAME == "T19M-Farm 5" ~ "T19M_Farm_05",
    FIELD_NAME == "Town Square-Farm 6" ~ "Town_Square_Farm_06",
    FIELD_NAME == "Hartley 2" ~ "Hartley_2",
    FIELD_NAME == "EM-17" ~ "EM_17",
    FIELD_NAME == "EM-9E" ~ "EM_9E",
    FIELD_NAME == "Pond North" ~ "Pond_North",
    FIELD_NAME == "Pond South" ~ "Pond_South",
    FIELD_NAME == "South 0" ~ "South_0",
    FIELD_NAME == "North 0" ~ "North_0",
    FIELD_NAME == "EM-12" ~ "EM_12",
    FIELD_NAME == "EM-10" ~ "EM_10",
    FIELD_NAME == "Schafer SE" ~ "Schafer_SE",
    FIELD_NAME == "4 East" ~ "4_East",
    FIELD_NAME == "SW Pond" ~ "SW_Pond",
    FIELD_NAME == "Field 2" ~ "Field_2",
    TRUE ~ as.character(FIELD_NAME)  # Keep other values unchanged
  ))

# Filter and save for 2018
unilever_data_2018 <- unilever_data %>% dplyr::filter(YEAR == 2018)
matched_2018 <- inner_join(unilever_data_2018, unilever_shp, by = "FIELD_NAME")
unmatched_2018 <- anti_join(unilever_data_2018, unilever_shp, by = "FIELD_NAME")
output_2018 <- "C:/Users/rbmahbub/Documents/Data/DOPDOH-paper/ShapefileData/Unilever/Yearwise/UnileverPD_2018.shp"
unilever_data_shp_2018  <- st_as_sf(matched_2018)
unilever_data_shp_2018  <- st_zm(unilever_data_shp_2018)
st_write(unilever_data_shp_2018 , output_2018, delete_layer = TRUE)
cat("Shapefile saved to:", output_2018, "\n")

# Filter and save for 2019
unilever_data_2019 <- unilever_data %>% dplyr::filter(YEAR == 2019)
matched_2019 <- inner_join(unilever_data_2019, unilever_shp, by = "FIELD_NAME")
unmatched_2019 <- anti_join(unilever_data_2019, unilever_shp, by = "FIELD_NAME")
output_2019 <- "C:/Users/rbmahbub/Documents/Data/DOPDOH-paper/ShapefileData/Unilever/Yearwise/UnileverPD_2019.shp"
unilever_data_shp_2019  <- st_as_sf(matched_2019)
unilever_data_shp_2019  <- st_zm(unilever_data_shp_2019)
st_write(unilever_data_shp_2019 , output_2019, delete_layer = TRUE)
cat("Shapefile saved to:", output_2019, "\n")

# Filter and save for 2020
unilever_data_2020 <- unilever_data %>% dplyr::filter(YEAR == 2020)
matched_2020 <- inner_join(unilever_data_2020, unilever_shp, by = "FIELD_NAME")
unmatched_2020 <- anti_join(unilever_data_2020, unilever_shp, by = "FIELD_NAME")
output_2020 <- "C:/Users/rbmahbub/Documents/Data/DOPDOH-paper/ShapefileData/Unilever/Yearwise/UnileverPD_2020.shp"
unilever_data_shp_2020  <- st_as_sf(matched_2020)
unilever_data_shp_2020  <- st_zm(unilever_data_shp_2020)
st_write(unilever_data_shp_2020 , output_2020, delete_layer = TRUE)
cat("Shapefile saved to:", output_2020, "\n")

# Filter and save for 2021
unilever_data_2021 <- unilever_data %>% dplyr::filter(YEAR == 2021)
matched_2021 <- inner_join(unilever_data_2021, unilever_shp, by = "FIELD_NAME")
unmatched_2021 <- anti_join(unilever_data_2021, unilever_shp, by = "FIELD_NAME")
output_2021 <- "C:/Users/rbmahbub/Documents/Data/DOPDOH-paper/ShapefileData/Unilever/Yearwise/UnileverPD_2021.shp"
unilever_data_shp_2021  <- st_as_sf(matched_2021)
unilever_data_shp_2021  <- st_zm(unilever_data_shp_2021)
st_write(unilever_data_shp_2021 , output_2021, delete_layer = TRUE)
cat("Shapefile saved to:", output_2021, "\n")


# Filter and save for 2022
unilever_data_2022 <- unilever_data %>% dplyr::filter(YEAR == 2022)
matched_2022 <- inner_join(unilever_data_2022, unilever_shp, by = "FIELD_NAME")
unmatched_2022 <- anti_join(unilever_data_2022, unilever_shp, by = "FIELD_NAME")
output_2022 <- "C:/Users/rbmahbub/Documents/Data/DOPDOH-paper/ShapefileData/Unilever/Yearwise/UnileverPD_2022.shp"
unilever_data_shp_2022  <- st_as_sf(matched_2022)
unilever_data_shp_2022  <- st_zm(unilever_data_shp_2022)
st_write(unilever_data_shp_2022 , output_2022, delete_layer = TRUE)
cat("Shapefile saved to:", output_2022, "\n")

# Filter and save for 2023
unilever_data_2023 <- unilever_data %>% dplyr::filter(YEAR == 2023)
matched_2023 <- inner_join(unilever_data_2023, unilever_shp, by = "FIELD_NAME")
unmatched_2023 <- anti_join(unilever_data_2023, unilever_shp, by = "FIELD_NAME")
output_2023 <- "C:/Users/rbmahbub/Documents/Data/DOPDOH-paper/ShapefileData/Unilever/Yearwise/UnileverPD_2023.shp"
unilever_data_shp_2023  <- st_as_sf(matched_2023)
unilever_data_shp_2023  <- st_zm(unilever_data_shp_2023)
st_write(unilever_data_shp_2023 , output_2023, delete_layer = TRUE)
cat("Shapefile saved to:", output_2023, "\n")

# Filter and save for 2024
unilever_data_2024 <- unilever_data %>% dplyr::filter(YEAR == 2024)
matched_2024 <- inner_join(unilever_data_2024, unilever_shp, by = "FIELD_NAME")
unmatched_2024 <- anti_join(unilever_data_2024, unilever_shp, by = "FIELD_NAME")
output_2024 <- "C:/Users/rbmahbub/Documents/Data/DOPDOH-paper/ShapefileData/Unilever/Yearwise/UnileverPD_2024.shp"
unilever_data_shp_2024  <- st_as_sf(matched_2024)
unilever_data_shp_2024  <- st_zm(unilever_data_shp_2024)
st_write(unilever_data_shp_2024 , output_2024, delete_layer = TRUE)
cat("Shapefile saved to:", output_2024, "\n")
unmatched_2023
unmatched_2024
# ----------------------------
# 6. ISBELL
# ----------------------------
# Define the file path for the Isbell shapefile
shapefile_isbell_path <- "C:/Users/rbmahbub/Documents/Data/DOPDOH-paper/ShapefileData/IsbellFarm/IsbellFarmShapefile/polygons.shp"
isbellshp <- st_read(shapefile_isbell_path)%>%select(c(-geometry_t, -SP_ID, -NDVI_media))# Read the Isbell shapefile

# Replace the values in the 'Field' column based on the given rules
isbellshp <- isbellshp %>%
  mutate(FIELD_NAME = case_when(
    FIELD_NAME %in% c(1, 1.0, "1", "1.0", "East#1", "East 01") ~ "East_01",
    FIELD_NAME %in% c(2, 2.0, "2", "2.0", "East#2", "East 02") ~ "East_02",
    FIELD_NAME %in% c(3, 3.0, "3", "3.0", "East#3", "East 03") ~ "East_03",
    FIELD_NAME %in% c(4, 4.0, "4", "4.0", "East#4", "East 04") ~ "East_04",
    FIELD_NAME %in% c(5, 5.0, "5", "5.0", "East#5", "East 05") ~ "East_05",
    FIELD_NAME %in% c(6, 6.0, "6", "6.0", "East#6", "East 06") ~ "East_06",
    FIELD_NAME %in% c(7, 7.0, "7", "7.0", "East#7", "East 07") ~ "East_07",
    FIELD_NAME %in% c(8, 8.0, "8", "8.0", "East#8", "East 08") ~ "East_08",
    FIELD_NAME %in% c(9, 9.0, "9", "9.0", "East#9", "East 09") ~ "East_09",
    FIELD_NAME %in% c(10, 10.0, "10", "10.0", "East#10", "East 10") ~ "East_10",
    FIELD_NAME %in% c(11, 11.0, "11", "11.0", "East#11", "East 11") ~ "East_11",
    FIELD_NAME %in% c(12, 12.0, "12", "12.0", "East#12", "East 12") ~ "East_12",
    FIELD_NAME %in% c(13, 13.0, "13", "13.0", "East#13", "East 13") ~ "East_13",
    FIELD_NAME %in% c(14, 14.0, "14", "14.0", "East#14", "East 14") ~ "East_14",
    FIELD_NAME == "Baker 20" ~ "Baker_20",
    FIELD_NAME == "Baker 30" ~ "Baker_30",
    FIELD_NAME == "Baker 40" ~ "Baker_40",
    FIELD_NAME == "Baker 50" ~ "Baker_50",
    FIELD_NAME %in% c("Bott of Res", "Bott of Res.", "Bottom Res.", "Bottom Res") ~ "Bott_of_Res",
    FIELD_NAME %in% c("Way_1", "Way 1", "Way 01") ~ "Way_01",
    FIELD_NAME %in% c("Way_2", "Way 2", "Way 02") ~ "Way_02",
    FIELD_NAME %in% c("Way_3", "Way 3", "Way 03") ~ "Way_03",
    FIELD_NAME %in% c("Way_4", "Way 4", "Way 04") ~ "Way_04",
    FIELD_NAME %in% c("Way_5", "Way 5", "Way 05") ~ "Way_05",
    FIELD_NAME %in% c("Kelly_1", "Kelly 1", "Kelly 01") ~ "Kelly_01",
    FIELD_NAME %in% c("Kelly_2", "Kelly 2", "Kelly 02") ~ "Kelly_02",
    FIELD_NAME %in% c("Kelly_3", "Kelly 3", "Kelly 03") ~ "Kelly_03",
    FIELD_NAME %in% c("Kelly_4", "Kelly 4", "Kelly 04") ~ "Kelly_04",
    FIELD_NAME %in% c("Kelly_5", "Kelly 5", "Kelly 05") ~ "Kelly_05",
    FIELD_NAME %in% c("Kelly_6", "Kelly 6", "Kelly 06") ~ "Kelly_06",
    FIELD_NAME %in% c("Kelly_7", "Kelly 7", "Kelly 07") ~ "Kelly_07",
    FIELD_NAME %in% c("Kelly_8", "Kelly 8", "Kelly 08") ~ "Kelly_08",
    FIELD_NAME %in% c("Kelly_9", "Kelly 9", "Kelly 09") ~ "Kelly_09",
    FIELD_NAME %in% c("Kelly_10", "Kelly 10") ~ "Kelly_10",
    FIELD_NAME %in% c("Kelly_11", "Kelly 11") ~ "Kelly_11",
    FIELD_NAME == "East #1"  ~ "East_01",
    FIELD_NAME == "East #2"  ~ "East_02",
    FIELD_NAME == "East #3"  ~ "East_03",
    FIELD_NAME == "East #4"  ~ "East_04",
    FIELD_NAME == "East #5"  ~ "East_05",
    FIELD_NAME == "East 6"  ~ "East_06",
    FIELD_NAME == "East #7"  ~ "East_07",
    FIELD_NAME == "East #8"  ~ "East_08",
    FIELD_NAME == "East #9"  ~ "East_09",
    FIELD_NAME == "East #10" ~ "East_10",
    FIELD_NAME == "East #11" ~ "East_11",
    FIELD_NAME == "East #12" ~ "East_12",
    FIELD_NAME == "East #13" ~ "East_13",
    FIELD_NAME == "East #14" ~ "East_14",
    FIELD_NAME %in% c("Walls_1", "Walls 1")  ~ "Walls_01",
    FIELD_NAME %in% c("Walls_2", "Walls 2")  ~ "Walls_02",
    FIELD_NAME %in% c("Walls_3", "Walls 3")  ~ "Walls_03",
    FIELD_NAME %in% c("Walls_4", "Walls 4")  ~ "Walls_04",
    FIELD_NAME %in% c("Walls_5", "Walls 5")  ~ "Walls_05",
    FIELD_NAME %in% c("Walls_6", "Walls 6")  ~ "Walls_06",
    FIELD_NAME %in% c("Walls_7", "Walls 7")  ~ "Walls_07",
    FIELD_NAME %in% c("Walls_6_+7", "Walls_67", "Walls 6&7") ~ "Walls_06_07",
    FIELD_NAME %in% c("Walls_8", "Walls 8")  ~ "Walls_08",
    FIELD_NAME %in% c("Walls_9", "Walls 9")  ~ "Walls_09",
    FIELD_NAME %in% c("Walls_10", "Walls 10") ~ "Walls_10",
    FIELD_NAME %in% c("Walls_11", "Walls 11") ~ "Walls_11",
    FIELD_NAME %in% c("Walls_12", "Walls 12") ~ "Walls_12",
    FIELD_NAME %in% c("Walls_13", "Walls 13") ~ "Walls_13",
    FIELD_NAME %in% c("Top_of_Res", "Top of Res.", "Top of Res") ~ "Top_of_Res",
    FIELD_NAME == "Hudson's" ~ "Hudsons",
    # New Field Name Mappings
    FIELD_NAME %in% c("Bransford Est", "Bransford East", "East Bransford", "East Bransfo") ~ "Bransford_Est",
    FIELD_NAME %in% c("Carr North") ~ "Carr_North",
    FIELD_NAME == "Carr South" ~ "Carr_South",
    FIELD_NAME %in% c( "Cattlet 1", "Cattlet Top", "catlett top") ~ "Cattlet_01",
    FIELD_NAME %in% c( "Cattlet 2", "Cattlet Bott", "cattlet bottom", "Cattlet Bottom") ~ "Cattlet_02",
    FIELD_NAME == "Cattlet 2" ~ "Cattlet_02",
    FIELD_NAME == "Cotton Patch" ~ "Cotton_Patch",
    #FIELD_NAME %in% c("East Harvey", "Top Harvey") ~ "East_Harvey",
    FIELD_NAME == "East Harvey" ~ "East_Harvey",
    FIELD_NAME %in% c("East Joe T", "Joe T East") ~ "East_Joe_T",
    FIELD_NAME %in% c("Experimental Plot", "Experiment")  ~ "Experiment",
    FIELD_NAME %in% c("Flat") ~ "Flat",
    FIELD_NAME %in% c( "Pop's Flat", "pops flat", "Pops Flat") ~ "Pops_Flat",
    FIELD_NAME == "Frog 40" ~ "Frog_40",
    FIELD_NAME %in% c("Frog 40 NW", "Frog 40  NW") ~ "Frog_40_NW",
    FIELD_NAME == "Haley" ~ "Haley",
    FIELD_NAME == "Hole 40" ~ "Hole_40",
    FIELD_NAME %in% c("Hole 40 NE", "Hole 40  NE") ~ "Hole_40_NE",
    FIELD_NAME == "HQ Shop" ~ "HQ_Shop",
    FIELD_NAME == "Hudsons" ~ "Hudsons",
    FIELD_NAME %in% c("Hwy Shop 40", "40 Shop Hwy ", "Hwy Shop_40") ~ "Hwy_Shop_40",
    FIELD_NAME == "Hwy Shop 40 SE" ~ "Hwy_Shop_40_SE",
    FIELD_NAME %in% c("Judy's", "Judy's Field", "Judys Field") ~ "Judys",
    FIELD_NAME == "Lentz" ~ "Lentz",
    FIELD_NAME == "Lost 40" ~ "Lost_40",
    FIELD_NAME == "Lyntz" ~ "Lyntz",
    FIELD_NAME %in% c("Md 40 West", "Md_40 West", 'middle_40 west', "West Mid_40") ~ "Md_40_West",
    FIELD_NAME == "Md 40 West MidW" ~ "Md_40_West_MidW",
    FIELD_NAME %in% c("Mid 40 Hwy", "Mid_40 Hwy") ~ "Mid_40_Hwy",
    FIELD_NAME %in% c('Mid 40 Hwy MidE') ~ "Mid_40_Hwy_MidE",
    FIELD_NAME %in% c("Middle Bransford", "Mid Bransford", "Middle Brans") ~ "Mid_Bransford",
    FIELD_NAME == "Morris" ~ "Morris",
    FIELD_NAME == "New Ground" ~ "New_Ground",
    FIELD_NAME == "North Cotton" ~ "North_Cotton",
    FIELD_NAME %in% c( "Pop's Field", "Pops Field") ~ "Pops",
    FIELD_NAME == "Pops" ~ "Pops",
    FIELD_NAME == "Seed Rice" ~ "Seed_Rice",
    FIELD_NAME %in% c( "Shanes", "Shane's") ~ "Shanes",
    FIELD_NAME == "SI Shop" ~ "SI_Shop",
    FIELD_NAME %in% c( "The 90", "90") ~ "The_90",
    FIELD_NAME == "Walls 6 +7" ~ "Walls_06_07",
    FIELD_NAME %in% c("West Harvey", "Bottom Harvey", "Bottom Harve") ~ "West_Harvey",
    FIELD_NAME == "West Harvey" ~ "West_Harvey",
    FIELD_NAME  %in% c( "West Joe T", "Joe T West") ~ "West_Joe_T",
    FIELD_NAME %in% c( "West Bransfo", "West Bransford", "Wst Bransford") ~ "Wst_Bransford",
    FIELD_NAME %in% c( "Wst Shop 40", "West Shop_40", "West Shop 40") ~ "Wst_Shop_40",
    FIELD_NAME == "Wst Shop 40 SW" ~ "Wst_Shop_40_SW",
    #FIELD_NAME %in% c("Lentz") ~ "Lyntz",  # Added Lentz to Lyntz
    TRUE ~ as.character(FIELD_NAME)  # Keep other values unchanged
  ))

sort(unique(isbellshp$FIELD_NAME))
isbelshpsjoin_data <- inner_join(isbellcl, isbellshp, by = "FIELD_NAME")
isbellcl_not_in_shp <- anti_join(isbellcl, isbellshp, by = "FIELD_NAME")
shp_not_in_isbellcl <- anti_join(isbellshp, isbellcl, by = "FIELD_NAME")

#harvestcl_not_in_shp <- anti_join(HDmerged_data, isbellshp, by = "FIELD_NAME")
#shp_not_in_harvestcl <- anti_join(isbellshp, HDmerged_data, by = "FIELD_NAME")
#sort(unique(harvestcl_not_in_shp$FIELD_NAME))


#------------2015--------------
isbell_2015 <- dplyr::filter(isbellcl, YEAR == "2015")
matched_2015 <- inner_join(isbell_2015, isbellshp, by = "FIELD_NAME")
unmatched_2015 <- anti_join(isbell_2015, isbellshp, by = "FIELD_NAME")
output_2015 <- "C:/Users/rbmahbub/Documents/Data/DOPDOH-paper/ShapefileData/IsbellFarm/IsbellFarmShapefile/Yearwise/IsbellPD_2018.shp"
isbell_data_2015_shp_2015  <- st_as_sf(matched_2015)
isbell_data_2015_shp_2015  <- st_zm(isbell_data_2015_shp_2015)
st_write(isbell_data_2015_shp_2015 , output_2015, delete_layer = TRUE)
cat("Shapefile saved to:", output_2015, "\n")


#------------2016#------------
matched_2016 <- inner_join(isbell_2016, isbellshp, by = "FIELD_NAME")
unmatched_2016 <- anti_join(isbell_2016, isbellshp, by = "FIELD_NAME")
output_2016 <- "C:/Users/rbmahbub/Documents/Data/DOPDOH-paper/ShapefileData/IsbellFarm/IsbellFarmShapefile/Yearwise/IsbellPD_2018.shp"
isbell_data_2016_shp_2016  <- st_as_sf(matched_2016)
isbell_data_2016_shp_2016  <- st_zm(isbell_data_2016_shp_2016)
st_write(isbell_data_2016_shp_2016 , output_2016, delete_layer = TRUE)
cat("Shapefile saved to:", output_2016, "\n")

#------------2017#------------
matched_2017 <- inner_join(isbell_2017, isbellshp, by = "FIELD_NAME")
unmatched_2017 <- anti_join(isbell_2017, isbellshp, by = "FIELD_NAME")
output_2017 <- "C:/Users/rbmahbub/Documents/Data/DOPDOH-paper/ShapefileData/IsbellFarm/IsbellFarmShapefile/Yearwise/IsbellPD_2018.shp"
isbell_data_2017_shp_2017  <- st_as_sf(matched_2017)
isbell_data_2017_shp_2017  <- st_zm(isbell_data_2017_shp_2017)
st_write(isbell_data_2017_shp_2017 , output_2017, delete_layer = TRUE)
cat("Shapefile saved to:", output_2017, "\n")

#------------2018#------------
matched_2018 <- inner_join(isbell_2018, isbellshp, by = "FIELD_NAME")
unmatched_2018 <- anti_join(isbell_2018, isbellshp, by = "FIELD_NAME")
output_2018 <- "C:/Users/rbmahbub/Documents/Data/DOPDOH-paper/ShapefileData/IsbellFarm/IsbellFarmShapefile/Yearwise/IsbellPD_2018.shp"
isbell_data_2018_shp_2018  <- st_as_sf(matched_2018)
isbell_data_2018_shp_2018  <- st_zm(isbell_data_2018_shp_2018)
st_write(isbell_data_2018_shp_2018 , output_2018, delete_layer = TRUE)
cat("Shapefile saved to:", output_2018, "\n")

#------------2019#------------
matched_2019 <- inner_join(isbell_2019, isbellshp, by = "FIELD_NAME")
unmatched_2019 <- anti_join(isbell_2019, isbellshp, by = "FIELD_NAME")
output_2019 <- "C:/Users/rbmahbub/Documents/Data/DOPDOH-paper/ShapefileData/IsbellFarm/IsbellFarmShapefile/Yearwise/IsbellPD_2019.shp"
isbell_data_2019_shp_2019  <- st_as_sf(matched_2019)
isbell_data_2019_shp_2019  <- st_zm(isbell_data_2019_shp_2019)
st_write(isbell_data_2019_shp_2019 , output_2019, delete_layer = TRUE)
cat("Shapefile saved to:", output_2019, "\n")

#------------2020#------------
matched_2020 <- inner_join(isbell_2020, isbellshp, by = "FIELD_NAME")
unmatched_2020 <- anti_join(isbell_2020, isbellshp, by = "FIELD_NAME")
output_2020 <- "C:/Users/rbmahbub/Documents/Data/DOPDOH-paper/ShapefileData/IsbellFarm/IsbellFarmShapefile/Yearwise/IsbellPD_2020.shp"
isbell_data_2020_shp_2020  <- st_as_sf(matched_2020)
isbell_data_2020_shp_2020  <- st_zm(isbell_data_2020_shp_2020)
st_write(isbell_data_2020_shp_2020 , output_2020, delete_layer = TRUE)
cat("Shapefile saved to:", output_2020, "\n")

#------------2021#------------
matched_2021 <- inner_join(isbell_2021, isbellshp, by = "FIELD_NAME")
unmatched_2021 <- anti_join(isbell_2021, isbellshp, by = "FIELD_NAME")
output_2021 <- "C:/Users/rbmahbub/Documents/Data/DOPDOH-paper/ShapefileData/IsbellFarm/IsbellFarmShapefile/Yearwise/IsbellPD_2021.shp"
isbell_data_2021_shp_2021  <- st_as_sf(matched_2021)
isbell_data_2021_shp_2021  <- st_zm(isbell_data_2021_shp_2021)
st_write(isbell_data_2021_shp_2021 , output_2021, delete_layer = TRUE)
cat("Shapefile saved to:", output_2021, "\n")


#####2022########
matched_2022 <- inner_join(isbell_2022, isbellshp, by = "FIELD_NAME")
unmatched_2022 <- anti_join(isbell_2022, isbellshp, by = "FIELD_NAME")
output_2022 <- "C:/Users/rbmahbub/Documents/Data/DOPDOH-paper/ShapefileData/IsbellFarm/IsbellFarmShapefile/Yearwise/IsbellPD_2022.shp"
isbell_data_2022_shp_2022  <- st_as_sf(matched_2022)
isbell_data_2022_shp_2022  <- st_zm(isbell_data_2022_shp_2022)
st_write(isbell_data_2022_shp_2022 , output_2022, delete_layer = TRUE)
cat("Shapefile saved to:", output_2022, "\n")

#####2023########
matched_2023 <- inner_join(isbell_2023, isbellshp, by = "FIELD_NAME")
unmatched_2023 <- anti_join(isbell_2023, isbellshp, by = "FIELD_NAME")
output_2023 <- "C:/Users/rbmahbub/Documents/Data/DOPDOH-paper/ShapefileData/IsbellFarm/IsbellFarmShapefile/Yearwise/IsbellPD_2023.shp"
isbell_data_2023_shp_2023  <- st_as_sf(matched_2023)
isbell_data_2023_shp_2023  <- st_zm(isbell_data_2023_shp_2023)
st_write(isbell_data_2023_shp_2023 , output_2023, delete_layer = TRUE)
cat("Shapefile saved to:", output_2023, "\n")

#####2024########
matched_2024 <- inner_join(isbell_2024, isbellshp, by = "FIELD_NAME")
unmatched_2024 <- anti_join(isbell_2024, isbellshp, by = "FIELD_NAME")
output_2024 <- "C:/Users/rbmahbub/Documents/Data/DOPDOH-paper/ShapefileData/IsbellFarm/IsbellFarmShapefile/Yearwise/IsbellPD_2024.shp"
isbell_data_2024_shp_2024  <- st_as_sf(matched_2024)
isbell_data_2024_shp_2024  <- st_zm(isbell_data_2024_shp_2024)
st_write(isbell_data_2024_shp_2024 , output_2024, delete_layer = TRUE)
cat("Shapefile saved to:", output_2024, "\n")
#################
####Mergeby year#
#################
# Define the base directory
combinedPOI <- sf::st_read("C:/Users/rbmahbub/Documents/Data/DOPDOH-paper/ShapefileData/COMBINEDSHAPEFILESHEET/CombinedwithPOI/PDHDshp2015_2024_POI.shp")%>%
  select(c(FIELD_NAME, PATHROWTIL))
combinedPOI<-as.data.frame(combinedPOI)%>%select(c(- geometry))
# Assuming your dataframe is named df
combinedPOI <- combinedPOI %>%
  mutate(collection = case_when(
    PATHROWTIL == "24.0_36.0_15SXU" ~ "processedPOI1",
    PATHROWTIL == "23.0_35.0_16SBE" ~ "processedPOI2",
    PATHROWTIL == "25.0_36.0_15SWV" ~ "processedPOI3",
    PATHROWTIL == "23.0_35.0_15SYV" ~ "processedPOI4",
    PATHROWTIL == "24.0_36.0_15SWU" ~ "processedPOI5",
    PATHROWTIL == "23.0_35.0_15SXV" ~ "processedPOI6",
    TRUE ~ NA_character_  # For any unmatched values
  ))



# Step 1: Get year-wise list of sources
yearwise_sources <- combined_data %>%
  group_by(YEAR) %>%
  summarise(sources = paste(unique(source), collapse = ", "))

# Step 2: Print year and its sources
for (i in 1:nrow(yearwise_sources)) {
  cat(paste0("Year ", yearwise_sources$YEAR[i], ": ", yearwise_sources$sources[i], "\n"))
}


base_path <- "C:/Users/rbmahbub/Documents/Data/DOPDOH-paper/ShapefileData/COMBINEDSHAPEFILESHEET/Yearwise"

# Ensure folders for each year exist before writing the shapefiles
years <- 2015:2024
for (year in years) {
  dir.create(file.path(base_path, as.character(year)), recursive = TRUE, showWarnings = FALSE)
}

### 2015 ##
PDHD2015 <- rbind(arvamatched_2015, isbell_data_2015_shp_2015) # Merge the two shapefiles
PDHD2015<-dplyr::left_join(PDHD2015, combinedPOI, by = "FIELD_NAME")
output_path <- file.path(base_path, "2015", "PDHD2015.shp")
st_write(PDHD2015, output_path, delete_layer = TRUE) # Write the merged shapefile
dups <- unique(PDHD2015$FIELD_NAME[duplicated(PDHD2015$FIELD_NAME)])
if (length(dups)) cat("Duplicates:\n", dups, sep = "\n") else cat("No duplicates\n")


### 2016 ##
PDHD2016 <- rbind(arvamatched_2016, isbell_data_2016_shp_2016) # Merge the two shapefiles
PDHD2016<-dplyr::left_join(PDHD2016, combinedPOI, by = "FIELD_NAME")
output_path <- file.path(base_path, "2016", "PDHD2016.shp")
st_write(PDHD2016, output_path, delete_layer = TRUE) # Write the merged shapefile
dups <- unique(PDHD2016$FIELD_NAME[duplicated(PDHD2016$FIELD_NAME)])
if (length(dups)) cat("Duplicates:\n", dups, sep = "\n") else cat("No duplicates\n")

### 2017 ##
PDHD2017 <- rbind(arvamatched_2017, dwmru_datashp_2017, isbell_data_2017_shp_2017) # Merge the shapefiles
PDHD2017<-dplyr::left_join(PDHD2017, combinedPOI, by = "FIELD_NAME")
PDHD2017 <- PDHD2017 %>%
  group_by(FIELD_NAME) %>%
  dplyr::filter(if (n() > 1 && n_distinct(PDDOY) == 1) {
    (max(HDDOY, na.rm = TRUE) - min(HDDOY, na.rm = TRUE)) <= 4
  } else TRUE) %>%
  slice(1) %>%
  ungroup()
output_path <- file.path(base_path, "2017", "PDHD2017.shp")
st_write(PDHD2017, output_path, delete_layer = TRUE) # Write the merged shapefile
dups <- unique(PDHD2017$FIELD_NAME[duplicated(PDHD2017$FIELD_NAME)])
if (length(dups)) cat("Duplicates:\n", dups, sep = "\n") else cat("No duplicates\n")


### 2018 ##
PDHD2018 <- rbind(arvamatched_2018, dwmru_datashp_2018, isbell_data_2018_shp_2018, unilever_data_shp_2018) # Merge the shapefiles
PDHD2018<-dplyr::left_join(PDHD2018, combinedPOI, by = "FIELD_NAME")
output_path <- file.path(base_path, "2018", "PDHD2018.shp")
st_write(PDHD2018, output_path, delete_layer = TRUE) # Write the merged shapefile
dups <- unique(PDHD2018$FIELD_NAME[duplicated(PDHD2018$FIELD_NAME)])
if (length(dups)) cat("Duplicates:\n", dups, sep = "\n") else cat("No duplicates\n")

### 2019 ##
PDHD2019 <- rbind(arvamatched_2019, isbell_data_2019_shp_2019, unilever_data_shp_2019) # Merge the shapefiles
PDHD2019<-dplyr::left_join(PDHD2019, combinedPOI, by = "FIELD_NAME")
output_path <- file.path(base_path, "2019", "PDHD2019.shp")
st_write(PDHD2019, output_path, delete_layer = TRUE) # Write the merged shapefile
dups <- unique(PDHD2019$FIELD_NAME[duplicated(PDHD2019$FIELD_NAME)])
if (length(dups)) cat("Duplicates:\n", dups, sep = "\n") else cat("No duplicates\n")

### 2020 ##
PDHD2020 <- rbind(arvamatched_2020, dwmru_datashp_2020, isbell_data_2020_shp_2020, unilever_data_shp_2020) # Merge the shapefiles
PDHD2020<-dplyr::left_join(PDHD2020, combinedPOI, by = "FIELD_NAME")
output_path <- file.path(base_path, "2020", "PDHD2020.shp")
st_write(PDHD2020, output_path, delete_layer = TRUE) # Write the merged shapefile
dups <- unique(PDHD2020$FIELD_NAME[duplicated(PDHD2020$FIELD_NAME)])
if (length(dups)) cat("Duplicates:\n", dups, sep = "\n") else cat("No duplicates\n")

### 2021 ##
PDHD2021 <- rbind(arvamatched_2021, dwmru_datashp_2021, isbell_data_2021_shp_2021, unilever_data_shp_2021) # Merge the shapefiles
PDHD2021<-dplyr::left_join(PDHD2021, combinedPOI, by = "FIELD_NAME")
PDHD2021 <- PDHD2021 %>%
  group_by(FIELD_NAME) %>%
  arrange(HDDOY) %>%
  dplyr::filter(!(n() > 1 & n_distinct(PDDOY) == 1 & (max(HDDOY) - min(HDDOY)) <= 4)) %>%
  ungroup()
output_path <- file.path(base_path, "2021", "PDHD2021.shp")
st_write(PDHD2021, output_path, delete_layer = TRUE) # Write the merged shapefile
dups <- unique(PDHD2021$FIELD_NAME[duplicated(PDHD2021$FIELD_NAME)])
if (length(dups)) cat("Duplicates:\n", dups, sep = "\n") else cat("No duplicates\n")

### 2022 ##
PDHD2022 <- rbind(arvamatched_2022, dwmru_datashp_2022, isbell_data_2022_shp_2022, 
                  unilever_data_shp_2022) # Merge the shapefiles
PDHD2022<-dplyr::left_join(PDHD2022, combinedPOI, by = "FIELD_NAME")
PDHD2022 <- PDHD2022 %>%
  group_by(FIELD_NAME) %>%
  mutate(
    dup_count = n(),
    pd_match = n_distinct(PDDOY) == 1,
    hd_range = max(HDDOY, na.rm = TRUE) - min(HDDOY, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  dplyr::filter(!(dup_count > 1 & pd_match & hd_range <= 1)) %>%  # remove one of the duplicates
  select(-dup_count, -pd_match, -hd_range)
output_path <- file.path(base_path, "2022", "PDHD2022.shp")
st_write(PDHD2022, output_path, delete_layer = TRUE) # Write the merged shapefile
dups <- unique(PDHD2022$FIELD_NAME[duplicated(PDHD2022$FIELD_NAME)])
if (length(dups)) cat("Duplicates:\n", dups, sep = "\n") else cat("No duplicates\n")

### 2023 ##
PDHD2023 <- rbind(arvamatched_2023, dwmru_datashp_2023, isbell_data_2023_shp_2023, 
                  sullivan_data_shp_2023, unilever_data_shp_2023) # Merge the shapefiles
PDHD2023<-dplyr::left_join(PDHD2023, combinedPOI, by = "FIELD_NAME")
output_path <- file.path(base_path, "2023", "PDHD2023.shp")
st_write(PDHD2023, output_path, delete_layer = TRUE) # Write the merged shapefile
dups <- unique(PDHD2023$FIELD_NAME[duplicated(PDHD2023$FIELD_NAME)])
if (length(dups)) cat("Duplicates:\n", dups, sep = "\n") else cat("No duplicates\n")


### 2024 ##
PDHD2024 <- rbind(arvamatched_2024, dwmru_datashp_2024, isbell_data_2024_shp_2024, matt_morris_datashp_2024,
                  unilever_data_shp_2024) # Merge the shapefiles
PDHD2024<-dplyr::left_join(PDHD2024, combinedPOI, by = "FIELD_NAME")
output_path <- file.path(base_path, "2024", "PDHD2024.shp")
st_write(PDHD2024, output_path, delete_layer = TRUE) # Write the merged shapefile
dups <- unique(PDHD2024$FIELD_NAME[duplicated(PDHD2024$FIELD_NAME)])
if (length(dups)) cat("Duplicates:\n", dups, sep = "\n") else cat("No duplicates\n")

##########
###2023#####
##########

colnames(unilever_data_shp_2018)
colnames(arvamatched_2015)
colnames(dwmru_datashp_2017)
colnames(matt_morris_datashp_2024)
colnames(isbell_data_2015_shp_2015)
colnames(sullivan_data_shp_2023)
colnames(seedingrice_shp_2024)


# Define the output directory
output_dir <- "C:/Users/rbmahbub/Documents/Data/DOPDOH-paper/ShapefileData/COMBINEDSHAPEFILESHEET/CombinedAll"
# Ensure the output directory exists
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# List of all yearly shapefiles (without modifying YEAR)
shapefile_list <- list(
  arvamatched_2015, isbell_data_2015_shp_2015,
  arvamatched_2016, isbell_data_2016_shp_2016,
  arvamatched_2017, dwmru_datashp_2017, isbell_data_2017_shp_2017,
  arvamatched_2018, dwmru_datashp_2018, isbell_data_2018_shp_2018, unilever_data_shp_2018,
  arvamatched_2019, isbell_data_2019_shp_2019, unilever_data_shp_2019,
  arvamatched_2020, dwmru_datashp_2020, isbell_data_2020_shp_2020, unilever_data_shp_2020,
  arvamatched_2021, dwmru_datashp_2021, isbell_data_2021_shp_2021, unilever_data_shp_2021,
  unilever_data_shp_2022, unilever_data_shp_2023, unilever_data_shp_2024,
  arvamatched_2022, dwmru_datashp_2022, isbell_data_2022_shp_2022, unilever_data_shp_2022,
  arvamatched_2023, dwmru_datashp_2023, isbell_data_2023_shp_2023, sullivan_data_shp_2023,
  arvamatched_2024, dwmru_datashp_2024, isbell_data_2024_shp_2024, matt_morris_datashp_2024
)

# Step 1: Standardize columns (keeping all existing ones)
all_fields <- Reduce(intersect, lapply(shapefile_list, names)) # Keep only common columns

# Step 2: Standardize all shapefiles (maintaining YEAR and FIELD_NAME)
shapefile_list <- lapply(shapefile_list, function(shp) {
  shp <- shp[, all_fields, drop = FALSE]  # Ensure consistent column order
  return(shp)
})

# Step 3: Merge all standardized shapefiles
merged_shapefile <- do.call(rbind, shapefile_list)
merged_shapefile <- merged_shapefile %>%
  group_by(FIELD_NAME) %>%
  dplyr::filter(YEAR == max(YEAR)) %>%
  ungroup()

merged_shapefile <- merged_shapefile %>%
  distinct(FIELD_NAME, YEAR, .keep_all = TRUE)

# Step 4: Save the final merged shapefile
output_merged_path <- file.path(output_dir, "PDHD_Merged_2015_2024.shp")
st_write(merged_shapefile, output_merged_path, delete_layer = TRUE)


filtered_rows <- merged_shapefile[grep("^[0-9]", merged_shapefile$FIELD_NAME), ]
print(filtered_rows)

####NROWS OF EACH YEAR
nrow(PDHD2015)+nrow(PDHD2016)+nrow(PDHD2017)+
  nrow(PDHD2018) + nrow(PDHD2019) + nrow(PDHD2020) + 
  nrow(PDHD2021) + nrow(PDHD2022) + nrow(PDHD2023) + 
  nrow(PDHD2024)



# ----------------------------
# 9. JAVASCRIPT EXPORT
# ----------------------------
#-----------------------
#2015
#-----------------------
for (value in unique(PDHD2015$FIELD_NAME)) {
  cat(paste0("var ", value, " = PDHD2015.filter(ee.Filter.eq('FIELD_NAME', '", value, "'));\n"))
}

# Generate the JavaScript results object code
output_lines <- c("var results = {")
for (i in 1:nrow(PDHD2015)) {
  field_name <- PDHD2015$FIELD_NAME[i]
  collection <- PDHD2015$collection[i]
  js_line <- paste0("  '", field_name, "': reduceByFeatureCollection(", 
                    collection, ", ", field_name, "),")
  output_lines <- c(output_lines, js_line)
}
# Remove last comma and close the object
output_lines[length(output_lines)] <- sub(",$", "", output_lines[length(output_lines)])
output_lines <- c(output_lines, "};")
# Print the complete JavaScript code
cat(paste(output_lines, collapse = "\n"))

#---------------------
# Meteo 2015
#-----------------------
output_lines <- c("var meteoResults = {")
for (i in 1:nrow(PDHD2015)) {
  field_name <- PDHD2015$FIELD_NAME[i]
  js_line <- paste0("  '", field_name, "': reduceByFeatureCollection(", 
                    "mergedCollection", ", ", field_name, "),")
  output_lines <- c(output_lines, js_line)
}
output_lines[length(output_lines)] <- sub(",$", "", output_lines[length(output_lines)])
output_lines <- c(output_lines, "};")
cat(paste(output_lines, collapse = "\n"))



##########################################
# 2016
##########################################
for (value in unique(PDHD2016$FIELD_NAME)) {
  cat(paste0("var ", value, " = PDHD2016.filter(ee.Filter.eq('FIELD_NAME', '", value, "'));\n"))
}

output_lines <- c("var results = {")
for (i in 1:nrow(PDHD2016)) {
  field_name <- PDHD2016$FIELD_NAME[i]
  collection <- PDHD2016$collection[i]
  js_line <- paste0("  '", field_name, "': reduceByFeatureCollection(", 
                    collection, ", ", field_name, "),")
  output_lines <- c(output_lines, js_line)
}
output_lines[length(output_lines)] <- sub(",$", "", output_lines[length(output_lines)])
output_lines <- c(output_lines, "};")
cat(paste(output_lines, collapse = "\n"))

# Meteo
output_lines <- c("var meteoResults = {")
for (i in 1:nrow(PDHD2016)) {
  field_name <- PDHD2016$FIELD_NAME[i]
  js_line <- paste0("  '", field_name, "': reduceByFeatureCollection(", 
                    "mergedCollection", ", ", field_name, "),")
  output_lines <- c(output_lines, js_line)
}
output_lines[length(output_lines)] <- sub(",$", "", output_lines[length(output_lines)])
output_lines <- c(output_lines, "};")
cat(paste(output_lines, collapse = "\n"))

##########################################
# 2017
##########################################
for (value in unique(PDHD2017$FIELD_NAME)) {
  cat(paste0("var ", value, " = PDHD2017.filter(ee.Filter.eq('FIELD_NAME', '", value, "'));\n"))
}

output_lines <- c("var results = {")
for (i in 1:nrow(PDHD2017)) {
  field_name <- PDHD2017$FIELD_NAME[i]
  collection <- PDHD2017$collection[i]
  js_line <- paste0("  '", field_name, "': reduceByFeatureCollection(", 
                    collection, ", ", field_name, "),")
  output_lines <- c(output_lines, js_line)
}
output_lines[length(output_lines)] <- sub(",$", "", output_lines[length(output_lines)])
output_lines <- c(output_lines, "};")
cat(paste(output_lines, collapse = "\n"))

# Meteo
output_lines <- c("var meteoResults = {")
for (i in 1:nrow(PDHD2017)) {
  field_name <- PDHD2017$FIELD_NAME[i]
  js_line <- paste0("  '", field_name, "': reduceByFeatureCollection(", 
                    "mergedCollection", ", ", field_name, "),")
  output_lines <- c(output_lines, js_line)
}
output_lines[length(output_lines)] <- sub(",$", "", output_lines[length(output_lines)])
output_lines <- c(output_lines, "};")
cat(paste(output_lines, collapse = "\n"))

##########################################
# 2018
##########################################
for (value in unique(PDHD2018$FIELD_NAME)) {
  cat(paste0("var ", value, " = PDHD2018.filter(ee.Filter.eq('FIELD_NAME', '", value, "'));\n"))
}

output_lines <- c("var results = {")
for (i in 1:nrow(PDHD2018)) {
  field_name <- PDHD2018$FIELD_NAME[i]
  collection <- PDHD2018$collection[i]
  js_line <- paste0("  '", field_name, "': reduceByFeatureCollection(", 
                    collection, ", ", field_name, "),")
  output_lines <- c(output_lines, js_line)
}
output_lines[length(output_lines)] <- sub(",$", "", output_lines[length(output_lines)])
output_lines <- c(output_lines, "};")
cat(paste(output_lines, collapse = "\n"))

# Meteo
output_lines <- c("var meteoResults = {")
for (i in 1:nrow(PDHD2018)) {
  field_name <- PDHD2018$FIELD_NAME[i]
  js_line <- paste0("  '", field_name, "': reduceByFeatureCollection(", 
                    "mergedCollection", ", ", field_name, "),")
  output_lines <- c(output_lines, js_line)
}
output_lines[length(output_lines)] <- sub(",$", "", output_lines[length(output_lines)])
output_lines <- c(output_lines, "};")
cat(paste(output_lines, collapse = "\n"))

##########################################
# 2019
##########################################
for (value in unique(PDHD2019$FIELD_NAME)) {
  cat(paste0("var ", value, " = PDHD2019.filter(ee.Filter.eq('FIELD_NAME', '", value, "'));\n"))
}

output_lines <- c("var results = {")
for (i in 1:nrow(PDHD2019)) {
  field_name <- PDHD2019$FIELD_NAME[i]
  collection <- PDHD2019$collection[i]
  js_line <- paste0("  '", field_name, "': reduceByFeatureCollection(", 
                    collection, ", ", field_name, "),")
  output_lines <- c(output_lines, js_line)
}
output_lines[length(output_lines)] <- sub(",$", "", output_lines[length(output_lines)])
output_lines <- c(output_lines, "};")
cat(paste(output_lines, collapse = "\n"))

# Meteo
output_lines <- c("var meteoResults = {")
for (i in 1:nrow(PDHD2019)) {
  field_name <- PDHD2019$FIELD_NAME[i]
  js_line <- paste0("  '", field_name, "': reduceByFeatureCollection(", 
                    "mergedCollection", ", ", field_name, "),")
  output_lines <- c(output_lines, js_line)
}
output_lines[length(output_lines)] <- sub(",$", "", output_lines[length(output_lines)])
output_lines <- c(output_lines, "};")
cat(paste(output_lines, collapse = "\n"))

##########################################
# 2020
##########################################
for (value in unique(PDHD2020$FIELD_NAME)) {
  cat(paste0("var ", value, " = PDHD2020.filter(ee.Filter.eq('FIELD_NAME', '", value, "'));\n"))
}

output_lines <- c("var results = {")
for (i in 1:nrow(PDHD2020)) {
  field_name <- PDHD2020$FIELD_NAME[i]
  collection <- PDHD2020$collection[i]
  js_line <- paste0("  '", field_name, "': reduceByFeatureCollection(", 
                    collection, ", ", field_name, "),")
  output_lines <- c(output_lines, js_line)
}
output_lines[length(output_lines)] <- sub(",$", "", output_lines[length(output_lines)])
output_lines <- c(output_lines, "};")
cat(paste(output_lines, collapse = "\n"))

# Meteo
output_lines <- c("var meteoResults = {")
for (i in 1:nrow(PDHD2020)) {
  field_name <- PDHD2020$FIELD_NAME[i]
  js_line <- paste0("  '", field_name, "': reduceByFeatureCollection(", 
                    "mergedCollection", ", ", field_name, "),")
  output_lines <- c(output_lines, js_line)
}
output_lines[length(output_lines)] <- sub(",$", "", output_lines[length(output_lines)])
output_lines <- c(output_lines, "};")
cat(paste(output_lines, collapse = "\n"))

##########################################
# 2021
##########################################
for (value in unique(PDHD2021$FIELD_NAME)) {
  cat(paste0("var ", value, " = PDHD2021.filter(ee.Filter.eq('FIELD_NAME', '", value, "'));\n"))
}

output_lines <- c("var results = {")
for (i in 1:nrow(PDHD2021)) {
  field_name <- PDHD2021$FIELD_NAME[i]
  collection <- PDHD2021$collection[i]
  js_line <- paste0("  '", field_name, "': reduceByFeatureCollection(", 
                    collection, ", ", field_name, "),")
  output_lines <- c(output_lines, js_line)
}
output_lines[length(output_lines)] <- sub(",$", "", output_lines[length(output_lines)])
output_lines <- c(output_lines, "};")
cat(paste(output_lines, collapse = "\n"))

#--------------------------------------
# Meteo
#--------------------------------------
output_lines <- c("var meteoResults = {")
for (i in 1:nrow(PDHD2021)) {
  field_name <- PDHD2021$FIELD_NAME[i]
  js_line <- paste0("  '", field_name, "': reduceByFeatureCollection(", 
                    "mergedCollection", ", ", field_name, "),")
  output_lines <- c(output_lines, js_line)
}
output_lines[length(output_lines)] <- sub(",$", "", output_lines[length(output_lines)])
output_lines <- c(output_lines, "};")
cat(paste(output_lines, collapse = "\n"))

##########################################
# 2022
##########################################
for (value in unique(PDHD2022$FIELD_NAME)) {
  cat(paste0("var ", value, " = PDHD2022.filter(ee.Filter.eq('FIELD_NAME', '", value, "'));\n"))
}

output_lines <- c("var results = {")
for (i in 1:nrow(PDHD2022)) {
  field_name <- PDHD2022$FIELD_NAME[i]
  collection <- PDHD2022$collection[i]
  js_line <- paste0("  '", field_name, "': reduceByFeatureCollection(", 
                    collection, ", ", field_name, "),")
  output_lines <- c(output_lines, js_line)
}
output_lines[length(output_lines)] <- sub(",$", "", output_lines[length(output_lines)])
output_lines <- c(output_lines, "};")
cat(paste(output_lines, collapse = "\n"))


# Meteo
output_lines <- c("var meteoResults = {")
for (i in 1:nrow(PDHD2022)) {
  field_name <- PDHD2022$FIELD_NAME[i]
  js_line <- paste0("  '", field_name, "': reduceByFeatureCollection(", 
                    "mergedCollection", ", ", field_name, "),")
  output_lines <- c(output_lines, js_line)
}
output_lines[length(output_lines)] <- sub(",$", "", output_lines[length(output_lines)])
output_lines <- c(output_lines, "};")
cat(paste(output_lines, collapse = "\n"))

##########################################
# 2023
##########################################
for (value in unique(PDHD2023$FIELD_NAME)) {
  cat(paste0("var ", value, " = PDHD2023.filter(ee.Filter.eq('FIELD_NAME', '", value, "'));\n"))
}

output_lines <- c("var results = {")
for (i in 1:nrow(PDHD2023)) {
  field_name <- PDHD2023$FIELD_NAME[i]
  collection <- PDHD2023$collection[i]
  js_line <- paste0("  '", field_name, "': reduceByFeatureCollection(", 
                    collection, ", ", field_name, "),")
  output_lines <- c(output_lines, js_line)
}
output_lines[length(output_lines)] <- sub(",$", "", output_lines[length(output_lines)])
output_lines <- c(output_lines, "};")
cat(paste(output_lines, collapse = "\n"))
# Meteo
output_lines <- c("var meteoResults = {")
for (i in 1:nrow(PDHD2023)) {
  field_name <- PDHD2023$FIELD_NAME[i]
  js_line <- paste0("  '", field_name, "': reduceByFeatureCollection(", 
                    "mergedCollection", ", ", field_name, "),")
  output_lines <- c(output_lines, js_line)
}
output_lines[length(output_lines)] <- sub(",$", "", output_lines[length(output_lines)])
output_lines <- c(output_lines, "};")
cat(paste(output_lines, collapse = "\n"))

##########################################
# 2024
##########################################
for (value in unique(PDHD2024$FIELD_NAME)) {
  cat(paste0("var ", value, " = PDHD2024.filter(ee.Filter.eq('FIELD_NAME', '", value, "'));\n"))
}

output_lines <- c("var results = {")
for (i in 1:nrow(PDHD2024)) {
  field_name <- PDHD2024$FIELD_NAME[i]
  collection <- PDHD2024$collection[i]
  js_line <- paste0("  '", field_name, "': reduceByFeatureCollection(", 
                    collection, ", ", field_name, "),")
  output_lines <- c(output_lines, js_line)
}
output_lines[length(output_lines)] <- sub(",$", "", output_lines[length(output_lines)])
output_lines <- c(output_lines, "};")
cat(paste(output_lines, collapse = "\n"))

# Meteo
output_lines <- c("var meteoResults = {")
for (i in 1:nrow(PDHD2024)) {
  field_name <- PDHD2024$FIELD_NAME[i]
  js_line <- paste0("  '", field_name, "': reduceByFeatureCollection(", 
                    "mergedCollection", ", ", field_name, "),")
  output_lines <- c(output_lines, js_line)
}
output_lines[length(output_lines)] <- sub(",$", "", output_lines[length(output_lines)])
output_lines <- c(output_lines, "};")
cat(paste(output_lines, collapse = "\n"))






# ###############################
# #############OLDCODE###########
# ###############################
# 
# ###################################
# ##################################
# ################################
# ###################
# ###Remove Arva non rice fields####
# ##########################
# # Find rows in pdhd_shapefile that have a match in arva_non_rice_data
# arvanonricematched_rows <- inner_join(pdhd_shapefile, arva_non_rice_data, by = "FIELD_NAME")
# 
# arvanonricematched_rows$FIELD_NAME
# # Remove matching rows from pdhd_shapefile
# pdhd_shapefile_nonricearva <- anti_join(pdhd_shapefile, arva_non_rice_data, by = "FIELD_NAME")
# pdhd_shapefile_nonricearva <- anti_join(pdhd_shapefile, seeding_non_rice_data, by = "FIELD_NAME")
# # Keep only the desired columns
# pdhd_shapefile_nonricearva <- pdhd_shapefile_nonricearva %>%
#   select(CLIENT_NAM, FARM_NAME, FIELD_NAME, layer, geometry)
# # Convert to 2D by dropping Z-dimension
# pdhd_shapefile_nonricearva <- st_zm(pdhd_shapefile_nonricearva, drop = TRUE, what = "ZM")
# # Define the output file path
# output_path <- "C:/Users/rbmahbub/Documents/Data/DOPDOH-paper/ShapefileData/COMBINEDSHAPEFILESHEET/FilteredCombinedShapefile/pdhd_shapefile_nonricearva.shp"
# # Save as a new shapefile
# st_write(pdhd_shapefile_nonricearva, output_path, delete_layer = TRUE)
# 
# #####Fix the names##################
# # Replace the values in the 'FIELD_NAME' column based on the given rules
# pdhd_shapefile_nonricearva <- pdhd_shapefile_nonricearva %>%
#   filter(!is.na(FIELD_NAME)) %>%
#   mutate(FIELD_NAME = case_when(
#     FIELD_NAME %in% c("1-Farm 8") ~ "1_Farm_8",
#     FIELD_NAME %in% c("102") ~ "3938_102",
#     FIELD_NAME %in% c("107") ~ "5002_107",
#     FIELD_NAME %in% c("110") ~ "5002_110",
#     FIELD_NAME %in% c("114") ~ "1723_114",
#     FIELD_NAME %in% c("152") ~ "2093_152",
#     FIELD_NAME %in% c("16NW") ~ "16NW",
#     FIELD_NAME %in% c("16SW") ~ "16SW",
#     FIELD_NAME %in% c("17") ~ "1723_17",
#     FIELD_NAME %in% c("201") ~ "201",
#     FIELD_NAME %in% c("202") ~ "202",
#     FIELD_NAME %in% c("227") ~ "1723_227",
#     FIELD_NAME %in% c("28") ~ "5002_28",
#     FIELD_NAME %in% c("40 Shop Hwy S") ~ "40_Shop_Hwy_S",
#     FIELD_NAME %in% c("403") ~ "403",
#     FIELD_NAME %in% c("404") ~ "404",
#     FIELD_NAME %in% c("48") ~ "1723_48",
#     FIELD_NAME %in% c("4East-Farm 8") ~ "4East_Farm_8",
#     FIELD_NAME %in% c("4N") ~ "4N",
#     FIELD_NAME %in% c("54(1)") ~ "54_1",
#     FIELD_NAME %in% c("55(1)") ~ "55_1",
#     FIELD_NAME %in% c("60") ~ "3938_60",
#     FIELD_NAME %in% c("64") ~ "2093_64",
#     FIELD_NAME %in% c("66") ~ "66",
#     FIELD_NAME %in% c("67") ~ "67",
#     FIELD_NAME %in% c("6E-Farm 8") ~ "6E_Farm_8",
#     FIELD_NAME %in% c("75") ~ "75",
#     FIELD_NAME %in% c("77") ~ "77",
#     FIELD_NAME %in% c("B5-Farm 5") ~ "B5_Farm_5",
#     FIELD_NAME %in% c("Baker 20") ~ "Baker_20",
#     FIELD_NAME %in% c("Baker 30") ~ "Baker_30",
#     FIELD_NAME %in% c("Baker 40") ~ "Baker_40",
#     FIELD_NAME %in% c("Baker 50") ~ "Baker_50",
#     FIELD_NAME %in% c("Berry S #1") ~ "Berry_S_1",
#     FIELD_NAME %in% c("Berry S #2") ~ "Berry_S_2",
#     FIELD_NAME %in% c("Berry S #3") ~ "Berry_S_3",
#     FIELD_NAME %in% c("Berry S #4") ~ "Berry_S_4",
#     FIELD_NAME %in% c("Big Field") ~ "Big_Field",
#     FIELD_NAME %in% c("Bottom Harvey") ~ "Bottom_Harvey",
#     FIELD_NAME %in% c("Bottom Res.") ~ "Bott_of_Res",
#     FIELD_NAME %in% c("Brown extra field-Farm 9") ~ "Brown_extra_field_Farm_9",
#     FIELD_NAME %in% c("Carr South") ~ "Carr_South",
#     FIELD_NAME %in% c("Cattlet Bottom") ~ "Cattlet_Bottom",
#     FIELD_NAME %in% c("Cattlet Top") ~ "Cattlet_Top",
#     FIELD_NAME %in% c("Clark13") ~ "Clark13",
#     FIELD_NAME %in% c("Clark14") ~ "Clark14",
#     FIELD_NAME %in% c("Cotton Patch") ~ "Cotton_Patch",
#     FIELD_NAME %in% c("Dewitt #1") ~ "Dewitt_1",
#     FIELD_NAME %in% c("Dewitt #2") ~ "Dewitt_2",
#     FIELD_NAME %in% c("Dewitt #3") ~ "Dewitt_3",
#     FIELD_NAME %in% c("Dewitt #4") ~ "Dewitt_4",
#     FIELD_NAME %in% c("East 01") ~ "East_01",
#     FIELD_NAME %in% c("East 02") ~ "East_02",
#     FIELD_NAME %in% c("East 03") ~ "East_03",
#     FIELD_NAME %in% c("East 04") ~ "East_04",
#     FIELD_NAME %in% c("East 05") ~ "East_05",
#     FIELD_NAME %in% c("East 06") ~ "East_06",
#     FIELD_NAME %in% c("East 07") ~ "East_07",
#     FIELD_NAME %in% c("East 08") ~ "East_08",
#     FIELD_NAME %in% c("East 09") ~ "East_09",
#     FIELD_NAME %in% c("East 10") ~ "East_10",
#     FIELD_NAME %in% c("East 11") ~ "East_11",
#     FIELD_NAME %in% c("East 12") ~ "East_12",
#     FIELD_NAME %in% c("East 13") ~ "East_13",
#     FIELD_NAME %in% c("East 14") ~ "East_14",
#     FIELD_NAME %in% c("East Bransford") ~ "Bransford_Est",
#     FIELD_NAME %in% c("EC #1") ~ "EC_1",
#     FIELD_NAME %in% c("EC #2") ~ "EC_2",
#     FIELD_NAME %in% c("Experimental Plot") ~ "Experiment",
#     FIELD_NAME %in% c("Fairley NE #1") ~ "Fairley_NE_1",
#     FIELD_NAME %in% c("Fairley NE #2") ~ "Fairley_NE_2",
#     FIELD_NAME %in% c("Fairley NE #3") ~ "Fairley_NE_3",
#     FIELD_NAME %in% c("Fairley NE #4") ~ "Fairley_NE_4",
#     FIELD_NAME %in% c("Fairley NW #1") ~ "Fairley_NW_1",
#     FIELD_NAME %in% c("Fairley NW #2") ~ "Fairley_NW_2",
#     FIELD_NAME %in% c("Fairley NW #3") ~ "Fairley_NW_3",
#     FIELD_NAME %in% c("Fairley NW #4") ~ "Fairley_NW_4",
#     FIELD_NAME %in% c("Fairley SE #1") ~ "Fairley_SE_1",
#     FIELD_NAME %in% c("Fairley SE #2") ~ "Fairley_SE_2",
#     FIELD_NAME %in% c("Fairley SE #3") ~ "Fairley_SE_3",
#     FIELD_NAME %in% c("Fairley SE #4") ~ "Fairley_SE_4",
#     FIELD_NAME %in% c("Fairley SW #1") ~ "Fairley_SW_1",
#     FIELD_NAME %in% c("Fairley SW #2") ~ "Fairley_SW_2",
#     FIELD_NAME %in% c("Fairley SW #3") ~ "Fairley_SW_3",
#     FIELD_NAME %in% c("Fairley SW #4") ~ "Fairley_SW_4",
#     FIELD_NAME %in% c("Field 2-Farm 8") ~ "Field_2_Farm_8",
#     FIELD_NAME %in% c("Field1") ~ "Field1",
#     FIELD_NAME %in% c("Field10") ~ "Field10",
#     FIELD_NAME %in% c("Field11") ~ "Field11",
#     FIELD_NAME %in% c("Field12east") ~ "Field12east",
#     FIELD_NAME %in% c("Field12west") ~ "Field12west",
#     FIELD_NAME %in% c("Field14") ~ "Field14",
#     FIELD_NAME %in% c("Field16east") ~ "Field16east",
#     FIELD_NAME %in% c("Field16west") ~ "Field16west",
#     FIELD_NAME %in% c("Field17east") ~ "Field17east",
#     FIELD_NAME %in% c("Field17west") ~ "Field17west",
#     FIELD_NAME %in% c("Field2") ~ "Field2",
#     FIELD_NAME %in% c("Field3") ~ "Field3",
#     FIELD_NAME %in% c("Field4") ~ "Field4",
#     FIELD_NAME %in% c("Field5") ~ "Field5",
#     FIELD_NAME %in% c("Field6") ~ "Field6",
#     FIELD_NAME %in% c("Field7") ~ "Field7",
#     FIELD_NAME %in% c("Field8") ~ "Field8",
#     FIELD_NAME %in% c("Field9") ~ "Field9",
#     FIELD_NAME %in% c("FieldA") ~ "FieldA",
#     FIELD_NAME %in% c("FieldE") ~ "FieldE",
#     FIELD_NAME %in% c("FieldF") ~ "FieldF",
#     FIELD_NAME %in% c("FieldJ") ~ "FieldJ",
#     FIELD_NAME %in% c("FieldK") ~ "FieldK",
#     FIELD_NAME %in% c("Flor #10") ~ "Flor_10",
#     FIELD_NAME %in% c("Flor #11") ~ "Flor_11",
#     FIELD_NAME %in% c("Flor #12") ~ "Flor_12",
#     FIELD_NAME %in% c("Frog 40") ~ "Frog_40",
#     FIELD_NAME %in% c("G2-Farm 5") ~ "G2_Farm_5",
#     FIELD_NAME %in% c("Greenfield-NW19") ~ "Greenfield_NW19",
#     FIELD_NAME %in% c("H110-Farm 3") ~ "H110_Farm_3",
#     FIELD_NAME %in% c("Haley") ~ "Haley",
#     FIELD_NAME %in% c("Hardy #7") ~ "Hardy_7",
#     FIELD_NAME %in% c("Hardy #8") ~ "Hardy_8",
#     FIELD_NAME %in% c("Hardy #9") ~ "Hardy_9",
#     FIELD_NAME %in% c("Hole 40") ~ "Hole_40",
#     FIELD_NAME %in% c("HQ Shop") ~ "HQ_Shop",
#     FIELD_NAME %in% c("HS NE #1") ~ "HS_NE_1",
#     FIELD_NAME %in% c("HS NE #2") ~ "HS_NE_2",
#     FIELD_NAME %in% c("HS NE #3") ~ "HS_NE_3",
#     FIELD_NAME %in% c("HS NE #4") ~ "HS_NE_4",
#     FIELD_NAME %in% c("HS NW #1") ~ "HS_NW_1",
#     FIELD_NAME %in% c("HS NW #2") ~ "HS_NW_2",
#     FIELD_NAME %in% c("HS NW #3") ~ "HS_NW_3",
#     FIELD_NAME %in% c("HS NW #4") ~ "HS_NW_4",
#     FIELD_NAME %in% c("HT1-Farm 1") ~ "HT1_Farm_1",
#     FIELD_NAME %in% c("Joe T East") ~ "Joe_T_East",
#     FIELD_NAME %in% c("Joe T West") ~ "West_Joe_T",
#     FIELD_NAME %in% c("Judy's Field") ~ "Judys",
#     FIELD_NAME %in% c("K140E-Farm 3") ~ "K140E_Farm_03",
#     FIELD_NAME %in% c("K140W-Farm 3") ~ "K140W_Farm_03",
#     FIELD_NAME %in% c("K164-Farm 3") ~ "K164_Farm_03",
#     FIELD_NAME %in% c("K24-Farm 3") ~ "K24_Farm_03",
#     FIELD_NAME %in% c("Kelly 01") ~ "Kelly_01",
#     FIELD_NAME %in% c("Kelly 02") ~ "Kelly_02",
#     FIELD_NAME %in% c("Kelly 03") ~ "Kelly_03",
#     FIELD_NAME %in% c("Kelly 04") ~ "Kelly_04",
#     FIELD_NAME %in% c("Kelly 05") ~ "Kelly_05",
#     FIELD_NAME %in% c("Kelly 06") ~ "Kelly_06",
#     FIELD_NAME %in% c("Kelly 07") ~ "Kelly_07",
#     FIELD_NAME %in% c("Kelly 08") ~ "Kelly_08",
#     FIELD_NAME %in% c("Kelly 09") ~ "Kelly_09",
#     FIELD_NAME %in% c("Kelly 10") ~ "Kelly_10",
#     FIELD_NAME %in% c("Kelly 11") ~ "Kelly_11",
#     FIELD_NAME %in% c("Landfill 60") ~ "Landfill_60",
#     FIELD_NAME %in% c("LIB2-Farm 1") ~ "LIB2_Farm_01",
#     FIELD_NAME %in% c("LOB-Farm 1") ~ "LOB_Farm_01",
#     FIELD_NAME %in% c("Lyntz") ~ "Lyntz",
#     FIELD_NAME %in% c("Mid 40 Hwy") ~ "Mid_40_Hwy",
#     FIELD_NAME %in% c("Middle Bransford") ~ "Mid_Bransford",
#     FIELD_NAME %in% c("Morris") ~ "Morris",
#     FIELD_NAME %in% c("MP #11") ~ "MP_11",
#     FIELD_NAME %in% c("MP #12") ~ "MP_12",
#     FIELD_NAME %in% c("MP #13") ~ "MP_13",
#     FIELD_NAME %in% c("MT1") ~ "MT1",
#     FIELD_NAME %in% c("MT3") ~ "MT3",
#     FIELD_NAME %in% c("N3-Farm 5") ~ "N3_Farm_05",
#     FIELD_NAME %in% c("N4-Farm 5") ~ "N4_Farm_05",
#     FIELD_NAME %in% c("New Ground") ~ "New_Ground",
#     FIELD_NAME %in% c("North 0-Farm 9") ~ "North_0_Farm_09",
#     FIELD_NAME %in% c("NW18") ~ "NW_18",
#     FIELD_NAME %in% c("P2-Farm 1") ~ "P2_Farm_01",
#     FIELD_NAME %in% c("Pop's Field") ~ "Pops",
#     FIELD_NAME %in% c("Pop's Flat") ~ "Flat",
#     FIELD_NAME %in% c("R10E-Farm 2") ~ "R10E_Farm_02",
#     FIELD_NAME %in% c("R10W-Farm 2") ~ "R10W_Farm_02",
#     FIELD_NAME %in% c("R2-Farm 2") ~ "R2_Farm_02",
#     FIELD_NAME %in% c("Regions 40-Farm 6") ~ "Regions_40_Farm_06",
#     FIELD_NAME %in% c("Rock House #1") ~ "Rock_House_01",
#     FIELD_NAME %in% c("Rock House #2") ~ "Rock_House_02",
#     FIELD_NAME %in% c("Rockhouse 2018-Farm 7") ~ "Rockhouse_2018_Farm_07",
#     FIELD_NAME %in% c("Rockhouse 2022-Farm 7") ~ "Rockhouse_2022_Farm_07",
#     FIELD_NAME %in% c("RS 5W-Farm 2") ~ "RS_5W_Farm_02",
#     FIELD_NAME %in% c("S01-Farm 1") ~ "S01_Farm_01",
#     FIELD_NAME %in% c("S03-Farm 1") ~ "S03_Farm_01",
#     FIELD_NAME %in% c("SeagravesNorth") ~ "SeagravesNorth",
#     FIELD_NAME %in% c("SeagravesSouth") ~ "SeagravesSouth",
#     FIELD_NAME %in% c("Seed Rice") ~ "Seed_Rice",
#     FIELD_NAME %in% c("Shane's") ~ "Shanes",
#     FIELD_NAME %in% c("South 0-Farm 9") ~ "South_0_Farm_09",
#     FIELD_NAME %in% c("Store #1") ~ "Store_01",
#     FIELD_NAME %in% c("Store #2") ~ "Store_02",
#     FIELD_NAME %in% c("Store #3") ~ "Store_03",
#     FIELD_NAME %in% c("T18M-Farm 5") ~ "T18M_Farm_05",
#     FIELD_NAME %in% c("T19M-Farm 5") ~ "T19M_Farm_05",
#     FIELD_NAME %in% c("The 90") ~ "The_90",
#     FIELD_NAME %in% c("Top Carr") ~ "Top_Carr",
#     FIELD_NAME %in% c("Top Harvey") ~ "Top_Harvey",
#     FIELD_NAME %in% c("Top of Res.") ~ "Top_of_Res",
#     FIELD_NAME %in% c("Town Square-Farm 6") ~ "Town_Square_Farm_06",
#     FIELD_NAME %in% c("Tresle1-Farm 7") ~ "Tresle1_Farm_07",
#     FIELD_NAME %in% c("Tresle2-Farm 7") ~ "Tresle2_Farm_07",
#     FIELD_NAME %in% c("Walls 1") ~ "Walls_01",
#     FIELD_NAME %in% c("Walls 10") ~ "Walls_10",
#     FIELD_NAME %in% c("Walls 11") ~ "Walls_11",
#     FIELD_NAME %in% c("Walls 12") ~ "Walls_12",
#     FIELD_NAME %in% c("Walls 13") ~ "Walls_13",
#     FIELD_NAME %in% c("Walls 2") ~ "Walls_02",
#     FIELD_NAME %in% c("Walls 3") ~ "Walls_03",
#     FIELD_NAME %in% c("Walls 4") ~ "Walls_04",
#     FIELD_NAME %in% c("Walls 5") ~ "Walls_05",
#     FIELD_NAME %in% c("Walls 6&7") ~ "Walls_06_07",
#     FIELD_NAME %in% c("Walls 8") ~ "Walls_08",
#     FIELD_NAME %in% c("Walls 9") ~ "Walls_09",
#     FIELD_NAME %in% c("Way 01") ~ "Way_01",
#     FIELD_NAME %in% c("Way 02") ~ "Way_02",
#     FIELD_NAME %in% c("Way 03") ~ "Way_03",
#     FIELD_NAME %in% c("Way 04") ~ "Way_04",
#     FIELD_NAME %in% c("Way 05") ~ "Way_05",
#     FIELD_NAME %in% c("Ways Triangle") ~ "Ways_Triangle",
#     FIELD_NAME %in% c("WC #1") ~ "WC_01",
#     FIELD_NAME %in% c("WC #2") ~ "WC_02",
#     FIELD_NAME %in% c("WC #3") ~ "WC_03",
#     FIELD_NAME %in% c("WC #4") ~ "WC_04",
#     FIELD_NAME %in% c("WC #5") ~ "WC_05",
#     FIELD_NAME %in% c("WC #6") ~ "WC_06",
#     FIELD_NAME %in% c("WC #7") ~ "WC_07",
#     FIELD_NAME %in% c("WC #8") ~ "WC_08",
#     FIELD_NAME %in% c("West Bransford") ~ "Wst_Bransford",
#     FIELD_NAME %in% c("West Mid 40") ~ "Md_40_West",
#     FIELD_NAME %in% c("West Shop 40") ~ "Wst_Shop_40",
#     FIELD_NAME %in% c("Wildy W #1") ~ "Wildy_W_01",
#     FIELD_NAME %in% c("Wildy W #2") ~ "Wildy_W_02",
#     FIELD_NAME %in% c("Wildy W #3") ~ "Wildy_W_03",
#     FIELD_NAME %in% c("Wildy W #4") ~ "Wildy_W_04",
#     FIELD_NAME %in% c("Wildy W #5") ~ "Wildy_W_05",
#     FIELD_NAME %in% c("Wildy W #6") ~ "Wildy_W_06",
#     FIELD_NAME %in% c("Wildy W #7") ~ "Wildy_W_07",
#     FIELD_NAME %in% c("Wildy W #8") ~ "Wildy_W_08",
#     TRUE ~ as.character(FIELD_NAME)  # Keep other values unchanged
#   ))
# 
# sort(unique(pdhd_shapefile_nonricearva$FIELD_NAME))
# #########################################################
# #########################################################
# 
# ############################################
# ##########Repeated FIELD_NAME in Excel sheet###
# ############################################
# length(unique(combined_data_cleaned$FIELD_NAME))
# length((combined_data_cleaned$FIELD_NAME))
# 
# # Count the occurrences of each farm name
# farm_counts <- table(combined_data_cleaned$FIELD_NAME)
# # Filter farm names that repeat (i.e., count greater than 1)
# repeated_farms <- farm_counts[farm_counts > 1]
# 
# # Create a year column from the PD column
# combined_data_cleaned$year <- year(ymd(combined_data_cleaned$PD))
# combined_data_cleaned$FIELD_NAME_year <- paste(combined_data_cleaned$FIELD_NAME, combined_data_cleaned$year, sep = "_")
# 
# # Filter to keep only 'unilever_data' entries for repeating FIELD_NAME_year combinations, removing 'isbell_merged_data' ones.
# # Filter the data to keep only 'unilever_data' when there are duplicate FIELD_NAME_years with isbell data
# combined_data_cleaned_filtered <- combined_data_cleaned %>%
#   group_by(FIELD_NAME_year) %>%
#   filter(!(source == "isbellcl" & 
#              FIELD_NAME_year %in% 
#              combined_data_cleaned$FIELD_NAME_year[combined_data_cleaned$source == "unilever_data"])) %>%
#   ungroup()
# 
# 
# # Remove duplicates where `FIELD_NAME`, `PDDOY`, and `source` are the same, keeping the first occurrence.
# # Assuming your data is stored in a data frame called `df`
# combined_data_cleaned_filtered_clean <- combined_data_cleaned_filtered %>%
#   distinct(FIELD_NAME, PDDOY, source, .keep_all = TRUE)
# 
# # Find which combinations of FIELD_NAME and year repeat
# repeated_field_names_years <- combined_data_cleaned_filtered_clean %>%
#   group_by(FIELD_NAME_year) %>%
#   filter(n() > 1) %>%
#   arrange(FIELD_NAME_year)
# repeated_field_names_years
# ############################################
# ##########Repeated FIELD_NAME in QGIS###
# ############################################
# repeatedfieldnamesSHP <- pdhd_shapefile_nonricearva %>%
#   group_by(FIELD_NAME) %>%
#   filter(n() > 1) %>%
#   arrange(FIELD_NAME)
# 
# repeatedfieldnamesSHP
# 
# ##########################################################################
# ##########################################################################
# # Perform a left join, keeping the spatial attributes of pdhd_shapefile
# merged_data <- pdhd_shapefile %>%
#   left_join(combined_data_cleaned_filtered_clean, by = "FIELD_NAME")
# 
# # Find rows in pdhd_shapefile that are NOT in combined_data_cleaned_filtered_clean
# unmatched_rows <- anti_join(pdhd_shapefile_nonricearva, combined_data_cleaned_filtered_clean, by = "FIELD_NAME")
# # Find rows in pdhd_shapefile that have a match in combined_data_cleaned_filtered_clean
# matched_rows <- inner_join(pdhd_shapefile_nonricearva, combined_data_cleaned_filtered_clean, by = "FIELD_NAME")
# 
# 
# # These are layers for which unmatched FIELD_NAME values were found with the planting dates dataframe
# # Filter for each layer and store in separate variables
# ### arva has non rice fields
# ##Remove non rice in  matt morris (boundaries)
# arva_field_boundaries <- unmatched_rows %>% filter(layer == "Arvafieldboundaries")
# boundaries <- unmatched_rows %>% filter(layer == "boundaries")
# matt_morris_farm <- unmatched_rows %>% filter(layer == "MattMorrisFarm")
# polygons <- unmatched_rows %>% filter(layer == "polygons")
# unilever_fs_diff_isbell_polygons <- unmatched_rows %>% filter(layer == "UnileverFS_diff_ISbell_polygons")
# DWMRU <- unmatched_rows %>% filter(layer == "DWMRU")
# 
# sort(unique(polygons$FIELD_NAME))
# sort(unique(arva_field_boundaries$FIELD_NAME))
# sort(unique(boundaries$FIELD_NAME))
# sort(unique(unilever_fs_diff_isbell_polygons$FIELD_NAME))
# sort(unique(DWMRU$FIELD_NAME))
# sort(unique(matt_morris_farm$FIELD_NAME))
# 
# 
# arva_field_boundaries <- matched_rows %>% filter(layer == "Arvafieldboundaries")
# boundaries <- matched_rows %>% filter(layer == "boundaries")
# matt_morris_farm <- matched_rows %>% filter(layer == "MattMorrisFarm")
# polygons <- matched_rows %>% filter(layer == "polygons")
# unilever_fs_diff_isbell_polygons <- matched_rows %>% filter(layer == "UnileverFS_diff_ISbell_polygons")
# DWMRU <- matched_rows %>% filter(layer == "DWMRU")
# 
# sort(unique(polygons$FIELD_NAME))
# sort(unique(isbellcl$FIELD_NAME))
# sort(unique(arva_field_boundaries$FIELD_NAME))
# sort(unique(arva_rice_data$FIELD_NAME))
# sort(unique(boundaries$FIELD_NAME))
# sort(unique(ryan_moore_sullivan_data$FIELD_NAME))
# sort(unique(unilever_fs_diff_isbell_polygons$FIELD_NAME))
# sort(unique(unilever_data$FIELD_NAME))
# sort(unique(DWMRU$FIELD_NAME))
# sort(unique(colby_rice_data$FIELD_NAME))
# sort(unique(matt_morris_farm$FIELD_NAME))
# sort(unique(matt_morris_farm$FIELD_NAME))
# 
# 
# # Join arva_field_boundaries with arva_non_rice_data based on FIELD_NAME
# arva_field_boundaries_non_rice <- arva_field_boundaries %>%
#   left_join(arva_non_rice_data, by = "FIELD_NAME")
# arva_field_boundaries_non_rice$FIELD_NAME
# 
# ####arva_field_boundaries matches witht other crop types
# #### matt morris had string problem
# boundaries$FIELD_NAME
# (polygons %>% filter(layer == "polygons"))$FIELD_NAME
# (polygons %>% filter(layer == "polygons"))$FIELD_NAME
# (combined_data_cleaned_filtered_clean %>% filter(source == "isbell_merged_data"))$FIELD_NAME
# 
# 
# sort(unique(pdhd_shapefile%>% filter(layer == "polygons"))$FIELD_NAME)
# sort(unique(unilever_data$FIELD_NAME))
# sort(unilever_fs_diff_isbell_polygons$FIELD_NAME)
# 
# (DWMRU%>% filter(layer == "DWMRU"))$FIELD_NAME
# 
# sort(unique(matched_rows%>% filter(layer == "DWMRU"))$FIELD_NAME)
# sort(unique(pdhd_shapefile%>% filter(layer == "DWMRU"))$FIELD_NAME)
# sort(unique((combined_data_cleaned_filtered_clean %>% filter(source == "colby_rice_data"))$FIELD_NAME))
# 
# 
# # Keep only the desired columns
# pdhd_shapefile_nonricearva <- pdhd_shapefile_nonricearva %>%
#   select(CLIENT_NAM, FARM_NAME, FIELD_NAME, layer, geometry)
# # Convert to 2D by dropping Z-dimension
# pdhd_shapefile_nonricearva <- st_zm(pdhd_shapefile_nonricearva, drop = TRUE, what = "ZM")
# # Define the output file path
# output_path <- "C:/Users/rbmahbub/Documents/Data/DOPDOH-paper/ShapefileData/COMBINEDSHAPEFILESHEET/FilteredCombinedShapefile/pdhd_shapefile_nonricearva.shp"
# # Save as a new shapefile
# st_write(pdhd_shapefile_nonricearva, output_path, delete_layer = TRUE)
# 
# ########################################################################
# ##########FILTER PDHD BY YEAR########################################
# #########################################################################
# # Find rows in pdhd_shapefile that have a match in combined_data_cleaned_filtered_clean
# combined_data_cleaned_filtered_clean_2015 <- combined_data_cleaned_filtered_clean %>% filter(year == 2015)
# combined_data_cleaned_filtered_clean_2016 <- combined_data_cleaned_filtered_clean %>% filter(year == 2016)
# combined_data_cleaned_filtered_clean_2017 <- combined_data_cleaned_filtered_clean %>% filter(year == 2017)
# combined_data_cleaned_filtered_clean_2018 <- combined_data_cleaned_filtered_clean %>% filter(year == 2018)
# combined_data_cleaned_filtered_clean_2019 <- combined_data_cleaned_filtered_clean %>% filter(year == 2019)
# combined_data_cleaned_filtered_clean_2020 <- combined_data_cleaned_filtered_clean %>% filter(year == 2020)
# combined_data_cleaned_filtered_clean_2021 <- combined_data_cleaned_filtered_clean %>% filter(year == 2021)
# combined_data_cleaned_filtered_clean_2022 <- combined_data_cleaned_filtered_clean %>% filter(year == 2022)
# combined_data_cleaned_filtered_clean_2023 <- combined_data_cleaned_filtered_clean %>% filter(year == 2023)
# combined_data_cleaned_filtered_clean_2024 <- combined_data_cleaned_filtered_clean %>% filter(year == 2024)
# 
# matched_rows_2015 <- inner_join(pdhd_shapefile_nonricearva, combined_data_cleaned_filtered_clean_2015, by = "FIELD_NAME")
# matched_rows_2016 <- inner_join(pdhd_shapefile_nonricearva, combined_data_cleaned_filtered_clean_2016, by = "FIELD_NAME")
# matched_rows_2017 <- inner_join(pdhd_shapefile_nonricearva, combined_data_cleaned_filtered_clean_2017, by = "FIELD_NAME")
# matched_rows_2018 <- inner_join(pdhd_shapefile_nonricearva, combined_data_cleaned_filtered_clean_2018, by = "FIELD_NAME")
# matched_rows_2019 <- inner_join(pdhd_shapefile_nonricearva, combined_data_cleaned_filtered_clean_2019, by = "FIELD_NAME")
# matched_rows_2020 <- inner_join(pdhd_shapefile_nonricearva, combined_data_cleaned_filtered_clean_2020, by = "FIELD_NAME")
# matched_rows_2021 <- inner_join(pdhd_shapefile_nonricearva, combined_data_cleaned_filtered_clean_2021, by = "FIELD_NAME")
# matched_rows_2022 <- inner_join(pdhd_shapefile_nonricearva, combined_data_cleaned_filtered_clean_2022, by = "FIELD_NAME")
# matched_rows_2023 <- inner_join(pdhd_shapefile_nonricearva, combined_data_cleaned_filtered_clean_2023, by = "FIELD_NAME")
# matched_rows_2024 <- inner_join(pdhd_shapefile_nonricearva, combined_data_cleaned_filtered_clean_2024, by = "FIELD_NAME")
# 
# library(dplyr)
# 
# # Function to ensure unique field names
# replace_special_chars <- function(df) {
#   # Replace special characters "-" and spaces with "_"
#   colnames(df) <- gsub("[-# ]", "_", colnames(df))
#   
#   # Truncate field names to 10 characters and ensure uniqueness
#   colnames(df) <- substr(colnames(df), 1, 10)
#   
#   # Check for duplicates in field names and make them unique
#   duplicated_cols <- duplicated(colnames(df)) | duplicated(colnames(df), fromLast = TRUE)
#   colnames(df)[duplicated_cols] <- paste0(colnames(df)[duplicated_cols], "_dup")
#   
#   # If date columns exist, replace "-" in dates as well
#   date_columns <- sapply(df, inherits, "Date")  # Check if any columns are Date class
#   
#   # Format date columns to replace "-" with "_"
#   for (col in names(df)[date_columns]) {
#     df[[col]] <- format(df[[col]], "%Y_%m_%d")  # Format date values to replace "-" with "_"
#   }
#   
#   return(df)
# }
# 
# # Apply the function to each matched dataframe
# matched_rows_2015 <- replace_special_chars(matched_rows_2015)
# matched_rows_2016 <- replace_special_chars(matched_rows_2016)
# matched_rows_2017 <- replace_special_chars(matched_rows_2017)
# matched_rows_2018 <- replace_special_chars(matched_rows_2018)
# matched_rows_2019 <- replace_special_chars(matched_rows_2019)
# matched_rows_2020 <- replace_special_chars(matched_rows_2020)
# matched_rows_2021 <- replace_special_chars(matched_rows_2021)
# matched_rows_2022 <- replace_special_chars(matched_rows_2022)
# matched_rows_2023 <- replace_special_chars(matched_rows_2023)
# matched_rows_2024 <- replace_special_chars(matched_rows_2024)
# 
# 
# # Define the base directory for saving shapefiles
# base_dir <- "C:/Users/rbmahbub/Documents/Data/DOPDOH-paper/ShapefileData/COMBINEDSHAPEFILESHEET/Yearwise"
# 
# # List of data frames to save (for each year)
# matched_rows_list <- list(
#   matched_rows_2015,
#   matched_rows_2016,
#   matched_rows_2017,
#   matched_rows_2018,
#   matched_rows_2019,
#   matched_rows_2020,
#   matched_rows_2021,
#   matched_rows_2022,
#   matched_rows_2023,
#   matched_rows_2024
# )
# 
# # Check if any column name exceeds 10 characters
# columns_exceeding_10 <- names(arva_rice_data)[column_lengths > 10]
# # Print columns that exceed 10 characters
# if(length(columns_exceeding_10) > 0) {
#   cat("Columns exceeding 10 characters:", columns_exceeding_10, "\n")
# } else {
#   cat("No columns exceed 10 characters.\n")
# }
# # Check the number of characters in each column name
# column_lengths <- nchar(names(arva_rice_data))
# # Display column names with their lengths
# data.frame(Column = names(arva_rice_data), Length = column_lengths)
