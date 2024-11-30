# Install necessary packages
install.packages("dplyr")
install.packages("ggplot2")
install.packages("readr")
install.packages("tidyr")
install.packages("plotly")

# Load the packages
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(plotly)

# Get working directory
getwd()

# Load the csv data files
customer_data <- read_csv("customer_data.csv")
product_data <- read_csv("product_data.csv")
sales_data <- read_csv("sales_data.csv")

# Preview the data to understand what we are working with
head(customer_data)
head(product_data)
head(sales_data)

# View the data structure in each file
str(customer_data)
str(product_data)
str(sales_data)

# Cleaning customer_data File

# Preview the customer_data to understand what we are working with
head(customer_data)

# View the customer_data structure in each file
str(customer_data)

# Check for duplicates in CustomerID
if (any(duplicated(customer_data$CustomerID))) {
  print("There are duplicate CustomerID values.")
} else {
  print("CustomerID is unique. It can serve as a primary key.")
}

# Find the duplicated CustomerID values
duplicates <- customer_data[duplicated(customer_data$CustomerID), ]

# Print the rows with duplicated CustomerID values
print(duplicates)

# Find all CustomerIDs that are duplicated
duplicate_ids <- customer_data$CustomerID[duplicated(customer_data$CustomerID) | duplicated(customer_data$CustomerID, fromLast = TRUE)]

# Remove all rows with duplicated CustomerID values and overwrite customer_data
customer_data <- customer_data %>% filter(!CustomerID %in% duplicate_ids)

# Print the cleaned dataset to verify
print(customer_data)

# Check for missing values in CustomerID
if (any(is.na(customer_data$CustomerID))) {
  print("There are missing values in CustomerID.")
} else {
  print("No missing values in CustomerID. It can serve as a primary key.")
}

# Combine FullAddress, ...6, ...7, and ...8 into one column
customer_data <- customer_data %>%
  unite("FullAddress_Combined", FullAddress, ...6, ...7, ...8, sep = ", ", na.rm = TRUE)

# Print the cleaned data with the combined address
print(customer_data)

# Check for missing values in the CustomerName column and filter those rows
missing_customername_data <- customer_data %>%
  filter(is.na(CustomerName)) %>%
  select(CustomerID, CustomerName, Email, PhoneNumber, FullAddress_Combined)

# Print the result
print(missing_customername_data)

# Update CustomerName for the specific CustomerIDs
customer_data$CustomerName[customer_data$CustomerID == 3] <- "Michael"
customer_data$CustomerName[customer_data$CustomerID == 14] <- "Michael"
customer_data$CustomerName[customer_data$CustomerID == 17] <- "Thomas Anderson"

# Print the updated rows to verify
updated_customers <- customer_data %>%
  filter(CustomerID %in% c(3, 14, 17))

print(updated_customers)

# Check for missing values in the CustomerName column
missing_customername_data <- customer_data %>%
  filter(is.na(CustomerName))

# Print the result
print(missing_customername_data)

# Format phone numbers to (###)-###-####
customer_data$PhoneNumber <- gsub(
  pattern = "\\D",               # Remove any non-digit characters
  replacement = "",               # Replace them with an empty string
  x = customer_data$PhoneNumber
)

# Apply the (###)-###-#### format
customer_data$PhoneNumber <- gsub(
  pattern = "(\\d{3})(\\d{3})(\\d{4})",
  replacement = "(\\1)-\\2-\\3",
  x = customer_data$PhoneNumber
)

# Print the first few rows to verify the formatting
print(head(customer_data$PhoneNumber))

# Save the edited dataset
write.csv(customer_data, "abc_customer_data.csv", row.names = FALSE)

# Clean product_data file

# Preview the product_data to understand what we are working with
head(product_data)

# View the customer_data structure
str(product_data)

# Check for duplicates in ProductID and filter them out
duplicates_product_data <- product_data %>%
  filter(duplicated(ProductID))

# Print the rows with duplicated ProductID
print(duplicates_product_data)

# Check for missing values in ProductID column
missing_product_ids <- product_data %>%
  filter(is.na(ProductID))

# Print rows where ProductID is missing
print(missing_product_ids)

# Check for missing values in Category column and filter the rows
missing_category_data <- product_data %>%
  filter(is.na(Category))

# Print rows where Category is missing
print(missing_category_data)

# List unique categories in the Category column
unique_categories <- product_data %>%
  distinct(Category) %>%
  pull(Category)

# Print the unique categories
print(unique_categories)

# Manually update the Category for specific ProductIDs
product_data$Category[product_data$ProductID == 16] <- "Furniture"
product_data$Category[product_data$ProductID == 23] <- "Electronics"
product_data$Category[product_data$ProductID == 25] <- "Personal Care"
product_data$Category[product_data$ProductID == 27] <- "Home Appliances"
product_data$Category[product_data$ProductID == 41] <- "Home Appliances"
product_data$Category[product_data$ProductID == 42] <- "Appliances"
product_data$Category[product_data$ProductID == 60] <- "Outdoor"
product_data$Category[product_data$ProductID == 72] <- "Home Decor"
product_data$Category[product_data$ProductID == 78] <- "Electronics"
product_data$Category[product_data$ProductID == 89] <- "Electronics"

# Print the updated rows to verify the changes
updated_products <- product_data %>%
  filter(ProductID %in% c(16, 23, 25, 27, 41, 42, 60, 72, 78, 89))

print(updated_products)

# Replace missing values in the Price column with 0 and overwrite the existing product_data
product_data$Price <- ifelse(is.na(product_data$Price), 0, product_data$Price)

# Print the first few rows to verify the changes
print(head(product_data))

# Replace missing values in the StockQuantity column with 0 and overwrite the existing product_data
product_data$StockQuantity <- ifelse(is.na(product_data$StockQuantity), 0, product_data$StockQuantity)

# Print the first few rows to verify the changes
print(head(product_data))

# Add a new column called StockValue which is Price * StockQuantity
product_data <- product_data %>%
  mutate(StockValue = Price * StockQuantity)

# Print the first few rows to verify the new column
print(head(product_data))

# Save the edited dataset
write.csv(product_data, "inventory_data.csv", row.names = FALSE)

# Clean sales_data file

# Preview the product_data to understand what we are working with
head(sales_data)

# View the sales_data structure
str(sales_data)

# Check for duplicates in the SalesID column
duplicates_sales_data <- sales_data %>%
  filter(duplicated(SalesID))

# Print the rows with duplicated SalesID
print(duplicates_sales_data)

# Remove rows where SalesID is missing or blank
sales_data_clean <- sales_data %>%
  filter(!is.na(SalesID) & nzchar(SalesID))

# Print the first few rows to verify the cleaning
print(head(sales_data_clean))

# Remove rows where SalesID is missing or blank, and overwrite the existing sales_data
sales_data <- sales_data %>%
  filter(!is.na(SalesID) & nzchar(SalesID))

# Print the first few rows to verify the changes
print(head(sales_data))

# Check for missing values in the SalesID column
missing_sales_id <- sales_data %>%
  filter(is.na(SalesID))

# Print rows with missing SalesID values
print(missing_sales_id)

# Replace missing values in CustomerID with 0 in the sales_data file
sales_data$CustomerID <- ifelse(is.na(sales_data$CustomerID), 0, sales_data$CustomerID)

# Print the first few rows to verify the changes
print(head(sales_data))

# Replace missing values in ProductID with 0 in the sales_data file
sales_data$ProductID <- ifelse(is.na(sales_data$ProductID), 0, sales_data$ProductID)

# Print the first few rows to verify the changes
print(head(sales_data))

# Replace missing values in SaleAmount with 0 in the sales_data file
sales_data$SaleAmount <- ifelse(is.na(sales_data$SaleAmount), 0, sales_data$SaleAmount)

# Print the first few rows to verify the changes
print(head(sales_data))

# Ensure SaleDate is of Date type if not already
sales_data$SaleDate <- as.Date(sales_data$SaleDate)

# Function to calculate the mean of previous and next non-missing dates
replace_missing_date <- function(dates) {
  for (i in which(is.na(dates))) {
    # Find the previous non-missing date
    prev_date <- max(dates[1:(i-1)], na.rm = TRUE)
    
    # Find the next non-missing date
    next_date <- min(dates[(i+1):length(dates)], na.rm = TRUE)
    
    # Calculate the mean of the two dates and assign it
    dates[i] <- as.Date(mean(c(as.numeric(prev_date), as.numeric(next_date))), origin = "1970-01-01")
  }
  return(dates)
}

# Apply the function to the SaleDate column
sales_data$SaleDate <- replace_missing_date(sales_data$SaleDate)

# Print the first few rows to verify the changes
print(head(sales_data))

#Add ProductID 0 to account for uncategorized sales

# Create a new row with the specified values
new_row <- data.frame(ProductID = 0, 
                      ProductName = "Unnamed", 
                      Category = "UnCategorized", 
                      Price = 0, 
                      StockQuantity = 0,
                      StockValue = 0)

# Add the new row to the product_data table
product_data <- rbind(product_data, new_row)

# Print the updated product_data to verify the new row
print(tail(product_data))  # Print the last few rows to verify

#Add Anonymous Customer

# Create a new row with the specified values
new_customer_row <- data.frame(
  CustomerID = 0, 
  CustomerName = "Anonymous Customer", 
  Email = NA, 
  PhoneNumber = NA, 
  FullAddress_Combined = NA
)

# Add the new row to the customer_data table
customer_data <- rbind(customer_data, new_customer_row)

# Print the updated customer_data to verify the new row
print(tail(customer_data))  # Print the last few rows to verify

# Ensure SaleDate is of Date type if not already
sales_data$SaleDate <- as.Date(sales_data$SaleDate)

# Create a new column Sale_Day that returns the day of the week
sales_data$Sale_Day <- weekdays(sales_data$SaleDate)

# Rearrange the columns to place Sale_Day right after SaleDate
sales_data <- sales_data %>%
  select(everything(), Sale_Day)

# Print the first few rows to verify the changes
print(head(sales_data))

# Rename the Sale_Day column to SaleDay
sales_data <- sales_data %>%
  rename(SaleDay = Sale_Day)

# Reorder the columns to place SaleDay between SaleDate and SaleAmount
sales_data <- sales_data %>%
  select(SalesID, CustomerID, ProductID, SaleDate, SaleDay, SaleAmount, everything())

# Print the first few rows to verify the changes
print(head(sales_data))

# Merge sales_data with customer_data based on CustomerID
customer_sales_data <- sales_data %>%
  left_join(customer_data, by = "CustomerID") %>%
  left_join(product_data, by = "ProductID") %>%
  select(SalesID, CustomerID, CustomerName, ProductID, ProductName, Category, SaleDate, SaleDay, SaleAmount)

# Print the first few rows of the merged dataset to verify
print(head(customer_sales_data))

# Save the combined dataset
write.csv(customer_sales_data, "customer_sales_data.csv", row.names = FALSE)


# # Visualization (to be completed - visualization done in excel)
# 
# # Create the ggplot for sales distribution by day of the week
# p_day <- ggplot(customer_sales_data, aes(x = SaleDay, y = SaleAmount, fill = SaleDay)) +
#   geom_bar(stat = "identity") +
#   labs(title = "Sales Distribution by Day of the Week", x = "Day of the Week", y = "Total Sales") +
#   theme_minimal()
# 
# # Convert the ggplot to an interactive plotly plot
# interactive_day_plot <- ggplotly(p_day)
# 
# # Display the interactive plot
# interactive_day_plot
# 
# # Create the ggplot for sales by product category
# p_category <- ggplot(customer_sales_data, aes(x = Category, y = SaleAmount, fill = Category)) +
#   geom_bar(stat = "identity") +
#   labs(title = "Sales by Product Category", x = "Category", y = "Total Sales") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# 
# # Convert to interactive plot
# interactive_category_plot <- ggplotly(p_category)
# 
# # Display the interactive plot
# interactive_category_plot
# 
# # Aggregate sales by CustomerName to create top_customers
# top_customers <- customer_sales_data %>%
#   group_by(CustomerName) %>%
#   summarize(TotalSales = sum(SaleAmount, na.rm = TRUE)) %>%
#   arrange(desc(TotalSales)) %>%
#   head(10)  # Get the top 10 customers
# 
# 
# # Plot top customers by sales amount (converted to interactive)
# p_customers <- ggplot(top_customers, aes(x = reorder(CustomerName, -TotalSales), y = TotalSales, fill = CustomerName)) +
#   geom_bar(stat = "identity") +
#   labs(title = "Top 10 Customers by Sales Amount", x = "Customer", y = "Total Sales") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# 
# # Convert to interactive plot
# interactive_customers_plot <- ggplotly(p_customers)
# 
# # Display the interactive plot
# interactive_customers_plot
# 
# # Create the ggplot for sales over time
# p_sales_trend <- ggplot(customer_sales_data, aes(x = SaleDate, y = SaleAmount)) +
#   geom_line() +
#   labs(title = "Sales Trend Over Time", x = "Date", y = "Sales Amount") +
#   theme_minimal()
# 
# # Convert to interactive plot
# interactive_sales_trend_plot <- ggplotly(p_sales_trend)
# 
# # Display the interactive plot
# interactive_sales_trend_plot
# 
# # Aggregate sales by CustomerName and Category
# customer_category_sales <- customer_sales_data %>%
#   group_by(CustomerName, Category) %>%
#   summarize(TotalSales = sum(SaleAmount, na.rm = TRUE))
# 
# # Print the first few rows to verify
# print(head(customer_category_sales))
# 
# # Create the ggplot heatmap for customer purchases by product category
# p_customer_category <- ggplot(customer_category_sales, aes(x = Category, y = CustomerName, fill = TotalSales)) +
#   geom_tile() +
#   labs(title = "Customer Purchases by Product Category", x = "Category", y = "Customer") +
#   scale_fill_gradient(low = "lightblue", high = "darkblue") +
#   theme_minimal()
# 
# # Convert to interactive plot
# interactive_customer_category_plot <- ggplotly(p_customer_category)
# 
# # Display the interactive plot
# interactive_customer_category_plot
# 
# # Aggregate sales by ProductID and ProductName to create top_products
# top_products <- customer_sales_data %>%
#   group_by(ProductName, ProductID) %>%
#   summarize(TotalSales = sum(SaleAmount, na.rm = TRUE)) %>%
#   arrange(desc(TotalSales)) %>%
#   head(10)  # Get the top 10 products
# 
# # Print the first few rows to verify
# print(top_products)
# 
# # Create the ggplot for top-selling products by sales amount
# p_top_products <- ggplot(top_products, aes(x = reorder(ProductName, -TotalSales), y = TotalSales, fill = ProductName)) +
#   geom_bar(stat = "identity") +
#   labs(title = "Top 10 Selling Products by Sales Amount", x = "Product", y = "Total Sales") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# 
# # Convert to interactive plot
# interactive_top_products_plot <- ggplotly(p_top_products)
# 
# # Display the interactive plot
# interactive_top_products_plot

