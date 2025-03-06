# Install the dplyr package for data manipulation
install.packages("readxl")
install.packages("dplyr")

# Load necessary libraries
library(readxl)    # For reading the Excel file
library(dplyr)     # For data manipulation

# UpLoad  dataset
cluster1 <- read_excel("/Users/jinnanut/Library/Mobile Documents/com~apple~CloudDocs/NUT/Exeter University/Master/Term 2/BEMM463 Marketing Analyst/Case Study Report/R Programming/Factor Analysis/Cluster1.xlsx")
cluster2 <- read_excel("/Users/jinnanut/Library/Mobile Documents/com~apple~CloudDocs/NUT/Exeter University/Master/Term 2/BEMM463 Marketing Analyst/Case Study Report/R Programming/Factor Analysis/Cluster2.xlsx")
cluster3 <- read_excel("/Users/jinnanut/Library/Mobile Documents/com~apple~CloudDocs/NUT/Exeter University/Master/Term 2/BEMM463 Marketing Analyst/Case Study Report/R Programming/Factor Analysis/Cluster3.xlsx")
cluster4 <- read_excel("/Users/jinnanut/Library/Mobile Documents/com~apple~CloudDocs/NUT/Exeter University/Master/Term 2/BEMM463 Marketing Analyst/Case Study Report/R Programming/Factor Analysis/Cluster4.xlsx")

#################### DATA PREPARATION STEP ###########################
# Combine all clusters into one dataset
df <- bind_rows(cluster1, cluster2, cluster3, cluster4)
# Check the structure of the data to ensure it is combined correctly
str(df)
# List of all factors (columns) to perform ANOVA Test
attributes <- c("ConstCom", "TimelyInf", "TaskMgm", "DeviceSt", "Wellness", "Athlete", "Style", "AmznP", "Income")

# CONVERT SCIENTIFIC NUMBER TO REGULAR NUMBER FORMAT
options(scipen = 999)   # Disable scientific notation (easier to read numbers)
x <- 2e-05              # Scientific notation
print(x)                # Outputs: regular number format

################### ANOVA TEST ################################
# Perform ANOVA for each attribute (column)
for (attribute in attributes) {
  print(paste("Running ANOVA for:", attribute))
  # Run ANOVA for the current attribute
  result <- aov(as.formula(paste(attribute, "~ cluster_col")), data = df)
  # Display the summary of the ANOVA result
  print(summary(result))
  
  #Store the p-value for each attribute in a dataframe
  p_value <- summary(result)[[1]]$`Pr(>F)`[1]  
  print(paste("P-value for", attribute, ":", p_value))
}

anova_results <- data.frame(Attribute = attributes, P_Value = NA)

for (i in 1:length(attributes)) {
  attribute <- attributes[i]
  result <- aov(as.formula(paste(attribute, "~ cluster_col")), data = df)
  anova_results$P_Value[i] <- summary(result)[[1]]$`Pr(>F)`[1]  # Extract the p-value from the ANOVA result
}

# Save the p-values results to Excel
library(openxlsx)
write.xlsx(anova_results, "ANOVA_results.xlsx")
