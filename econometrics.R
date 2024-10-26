# Load necessary library
library(readxl)

# Read the data from the Excel file
data <- read_excel("C:/Users/user/OneDrive/Bureau/unemploy.xlsx")

# Inspect the structure and first few rows of the data
str(data)
head(data)

# Handle missing values by removing rows with NA in specific columns
data <- data[!is.na(data$Unemployment) & !is.na(data$`Education level`) & !is.na(data$`investment rate`), ]

# Ensure all relevant columns are numeric
data$Unemployment <- as.numeric(data$Unemployment)
data$`Education level` <- as.numeric(data$`Education level`)
data$`investment rate` <- as.numeric(data$`investment rate`)

# Check for NA/NaN/Inf in the Unemployment column after conversion
data <- data[!is.na(data$Unemployment) & !is.infinite(data$Unemployment), ]

# Check the structure and first few rows again to ensure missing values are handled
str(data)
head(data)

# Extract columns for analysis
investment_rate <- data$`investment rate`
unemployment <- data$Unemployment
education_level <- data$`Education level`

# Fit the linear models
lm_model <- lm(Unemployment ~ `Education level`, data = data)
summary(lm_model)

lm1_model <- lm(Unemployment ~ `investment rate`, data = data)
summary(lm1_model)

multiple_lm_model <- lm(Unemployment ~ `Education level` + `investment rate`, data = data)
summary(multiple_lm_model)

data <- na.omit(data[, c("Unemployment", "Education level", "investment rate")])

# Compute correlations between variables
correlation_matrix <- cor(data)

# Print correlation matrix
print(correlation_matrix)
