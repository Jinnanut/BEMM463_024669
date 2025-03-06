################ SmartWatch Project ##################

# Install necessary packages if not already installed
install.packages("readxl")
install.packages("tidyverse")
install.packages("cluster")
install.packages("openxlsx")


# Load the required libraries
library(readxl)     # For reading Excel files
library(tidyverse)  # For data manipulation and visualisation
library(cluster)    # For clustering methods
library(openxlsx)   # For exporting data to Excel

################## PREPARATION ###################

# IMPORTING DATA FROM EXCEL 
SmartWatch <- read_excel(file.choose())
# View the imported dataset (opens in a separate viewer)
View(SmartWatch)

# INITIAL DATA EXPLORATION 
# Display column names of the dataset
names(SmartWatch)
# Display basic summary statistics (e.g., min, max, mean, etc.) for each variable
summary(SmartWatch) 
# Explore the structure of the dataset (e.g., column types, number of rows)
str(SmartWatch)   


# Remove binary data column to avoid the bias in result
df <- SmartWatch[-c(8,9,10)]
names(df)
df$Income <- ((df$Income - 1) / (5 - 1)) * (7 - 1) + 1  # Method 1: Rescal Income from 1-5 to 1-7 (Avoid bias)
# Method 2: Normalize to (0-1) Range
view(df)

# STANDARDISE DATA
dfz <- scale(df)      
# Bring binary data back to data
binary_data <- SmartWatch[, 8:10]
dfz <- cbind(dfz, binary_data)
# Reorder column
dfz <- dfz[, c(1, 2, 3, 4, 5,6,7,10,11,12,8,9)]
dfz$Degree <- dfz$Degree - 1  # Changes (1 → 0) and (2 → 1)
# View the standardised data
View(dfz)

################### SEGMENTATION STEP ###################

# CALCULATE EUCLIDEAN DISTANCE
distance <- dist(dfz, method = 'euclidean')

# CLUSTER DENDROGRAM 
# Perform hierarchical clustering
hc.w <- hclust(distance, method = 'ward.D')   
# Plot the dendrogram to visualise the clustering
plot(hc.w, main = "Cluster Dendrogram", xlab = "Observations", ylab = "Height")

# DETERMINE THE OPTIMAL NUMBER OF CLUSTERS
# Use the elbow method to decide the number of clusters.
x <- c(1:12)
sort_height <- sort(hc.w$height, decreasing = TRUE) 
y <- sort_height[1:12] 

# Plot elbow plot
plot(x, y, type = "b", main = "Elbow Plot", xlab = "Clusters", ylab = "WSS")     # Hint: "Elbow Plot"
lines(x, y, col = "blue")

# CUT DENDROGRAM INTO 4 CLUSTERS
cluster <- cutree(hc.w, k = 4)     
# Display cluster assignments
print(cluster)
# Create a frequency table to see the size of each cluster
table(cluster)
# Add cluster assignments back to the original data
df_final <- as.data.frame(cbind(SmartWatch, cluster))   # df_final <- cbind(SmartWatch, cluster)    
# Check the updated dataset
View(df_final)

################### DESCRIPTION STEP ###################

# CALCULATE SEGMENT SIZES
proportions <- table(df_final$cluster) / nrow(df_final)     
percentages <- proportions * 100
# Display segment sizes in percentages
print(percentages)     

# EXPLORE MEAN VALUES OF VARIABLES IN EACH CLUSTER
segments<-
  df_final %>% 
  group_by(cluster) %>% 
  summarise(across(where(is.numeric), mean, .names = "{col}_mean"))
# Display the calculated means
segments

# SAVE MEAN TABLE TO EXCEL 
write.xlsx(segments, 'segments.xlsx')     
