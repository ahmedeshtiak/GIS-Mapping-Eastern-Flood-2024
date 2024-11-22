# Install necessary packages
install.packages(c("sf", "ggplot2", "dplyr", "tmap", "leaflet"))
install.packages("viridis")
install.packages("scales")
install.packages("svglite")
install.packages("gridExtra")

# Load the packages
library(sf)         # For handling spatial data
library(ggplot2)    # For plotting
library(dplyr)      # For data manipulation
library(tmap)       # For thematic mapping
library(leaflet)    # For interactive maps
library(viridis)    # Color palette for better color contrast
library(scales)     # For scaling data in visualizations
library(svglite)    # For saving plots as SVGs
library(gridExtra)  # For arranging multiple plots

# Load Bangladesh shapefile and assign it to bangladesh_shapefile
bangladesh_shapefile <- st_read(file.choose())  # Prompts user to choose a shapefile

# Create a data frame for total damage data by district
damage_data <- data.frame(
  District = c("Noakhali", "Comilla", "Feni", "Chittagong", "Lakshmipur", 
               "Maulvibazar", "Brahamanbaria", "Habiganj", "Khagrachhari", 
               "Cox's Bazar", "Sylhet"),
  Total_Damage = c(4191.62, 3390.35, 2683.14, 1676.94, 1403.91, 
                   506.07, 144.01, 143.59, 127.23, 100.48, 20.51)
)

# Load agricultural damage data from a CSV file
agri_damage <- read.csv(file.choose())

# Print data for checking
print(damage_data)
print(bangladesh_shapefile)
names(bangladesh_shapefile)  # Check column names in the shapefile

# Merge the shapefile data with economic damage data on district name
merged_data <- bangladesh_shapefile %>%
  left_join(damage_data, by = c("ADM2_EN" = "District"))  

# Check column names of the merged dataset
names(merged_data)
names(merged_data2)  # Check if there is a merged_data2 (it may need creating first)

# Ensure Damage_Category is created based on Total_Damage column in merged_data
merged_data <- merged_data %>%
  mutate(Damage_Category = case_when(
    Total_Damage >= 0 & Total_Damage <= 100 ~ "0-100",
    Total_Damage > 100 & Total_Damage <= 500 ~ "101-500",
    Total_Damage > 500 & Total_Damage <= 1500 ~ "501-1500",
    Total_Damage > 1500 & Total_Damage <= 3000 ~ "1501-3000",
    Total_Damage > 3000 & Total_Damage <= 4500 ~ "3001-4500"
  ))

# Create a map for economic loss by district
economic_loss_map <- ggplot(data = merged_data) +
  geom_sf(aes(fill = Damage_Category), color = "white", size = 0.2) +  # Add district borders and color by damage category
  scale_fill_manual(
    values = c(
      "0-100" = "#FFFFB2",        # Colors for each damage category
      "101-500" = "#FED976",       
      "501-1500" = "#FEB24C",      
      "1501-3000" = "#FD8D3C",     
      "3001-4500" = "#E31A1C"      
    ),
    name = "Total damage (in crore Tk.)",
    breaks = c("0-100", "101-500", "501-1500", "1501-3000", "3001-4500"),
    labels = c("Minimal damage (0-100 crore Tk.)", 
               "Low damage (101-500 crore Tk.)",
               "Moderate damage (501-1500 crore Tk.)",
               "High damage (1501-3000 crore Tk.)",
               "Very high damage (3001-4500 crore Tk.)")
  ) +
  geom_sf_text(aes(label = ADM2_EN), size = 2 , color = "black", check_overlap = TRUE) +  # Add district names
  labs(
    title = "Total Damage by District",
    fill = "Total Damage"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),   # Center and bold title
    plot.subtitle = element_text(hjust = 0.5, size = 13),               # Center subtitle
    legend.position = "right",                                          # Legend position
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10),
    panel.grid = element_blank(),                                       # Remove grid
    axis.text = element_blank(),                                        # Remove axis text
    axis.ticks = element_blank(),                                       # Remove axis ticks
    axis.title = element_blank()                                        # Remove axis titles
  )

# Print the economic loss map
print(economic_loss_map)

# Add damage category for agriculture damage data based on thresholds
merged_data2 <- merged_data2 %>%
  mutate(Damage_Category = case_when(
    is.na(Agriculture.Damage..Crore.Tk..) ~ "Nothing",
    Agriculture.Damage..Crore.Tk.. < 100 ~ "0-100",
    Agriculture.Damage..Crore.Tk.. >= 100 & Agriculture.Damage..Crore.Tk.. < 300 ~ "100-300",
    Agriculture.Damage..Crore.Tk.. >= 300 & Agriculture.Damage..Crore.Tk.. < 600 ~ "300-600",
    Agriculture.Damage..Crore.Tk.. >= 600 & Agriculture.Damage..Crore.Tk.. < 900 ~ "600-900",
    TRUE ~ "900+"
  ))

# Create a map for agriculture damage by district
agri_damage_map <- ggplot(data = merged_data2) +
  geom_sf(aes(fill = Damage_Category), color = "white", size = 0.2) +  # Add district borders and color by agri damage category
  scale_fill_manual(
    values = c(
      "0-100" = "#d9f0d3",       # Colors for each agriculture damage category
      "100-300" = "#a8ddb5",    
      "300-600" = "#4eb3d3",    
      "600-900" = "#2b8cbe",     
      "900+" = "#8856a7"         
    ),
    name = "Total Damage (in crore Tk.)",
    breaks = c("0-100", "100-300", "300-600", "600-900", "900+"),
    labels = c("Minimal Damage (0 - 100 crore Tk.)", 
               "Moderate Damage (100 - 300 crore Tk.)",
               "Significant Damage (300 - 600 crore Tk.)",
               "Severe Damage (600 - 900 crore Tk.)",
               "Critical Damage (900+ crore Tk.)")
  ) +
  geom_sf_text(aes(label = ADM2_EN), size = 2, color = "black", check_overlap = TRUE) +  # Add district names
  labs(
    title = "Agriculture and Forestry Damage by District",
    fill = "Total Damage"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),   # Center and bold title
    plot.subtitle = element_text(hjust = 0.5, size = 13),               # Center subtitle
    legend.position = "right",                                          # Legend position
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10),
    panel.grid = element_blank(),                                       # Remove grid
    axis.text = element_blank(),                                        # Remove axis text
    axis.ticks = element_blank(),                                       # Remove axis ticks
    axis.title = element_blank()                                        # Remove axis titles
  )

# Print the agriculture damage map
print(agri_damage_map)

# Arrange both maps side-by-side
grid.arrange(agri_damage_map, economic_loss_map, ncol = 2)
