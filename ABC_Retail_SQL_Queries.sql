--Best Selling Products
SELECT 
    ProductName,
    SUM(SaleAmount) AS TotalSales
FROM 
    customer_sales_data
GROUP BY 
    ProductName
ORDER BY 
    TotalSales DESC;

	
--Best Selling Month
SELECT 
    strftime('%Y-%m', SaleDate) AS SaleMonth,
    SUM(SaleAmount) AS TotalSales
FROM 
    customer_sales_data
GROUP BY 
    SaleMonth
ORDER BY 
    TotalSales DESC;

	
--Best Selling Days of the Week
SELECT 
    strftime('%w', SaleDate) AS DayOfWeek,  -- '%w' returns day of week (0 = Sunday, 6 = Saturday)
    CASE strftime('%w', SaleDate)
        WHEN '0' THEN 'Sunday'
        WHEN '1' THEN 'Monday'
        WHEN '2' THEN 'Tuesday'
        WHEN '3' THEN 'Wednesday'
        WHEN '4' THEN 'Thursday'
        WHEN '5' THEN 'Friday'
        WHEN '6' THEN 'Saturday'
    END AS DayName,
    SUM(SaleAmount) AS TotalSales
FROM 
    customer_sales_data
GROUP BY 
    DayOfWeek
ORDER BY 
    TotalSales DESC;
	
	
--Top Customers	
SELECT 
    CustomerName,
    COUNT(*) AS PurchaseFrequency,
    SUM(SaleAmount) AS TotalSpending
FROM 
    customer_sales_data
GROUP BY 
    CustomerName
ORDER BY 
    TotalSpending DESC;


--List Product Inventory	
SELECT 
    ProductID,
    ProductName,
    StockQuantity,
    Price,
    (StockQuantity * Price) AS StockValue
FROM 
    inventory_data;


--Low Stock	
	SELECT 
    ProductID,
    ProductName,
    StockQuantity
FROM 
    inventory_data
WHERE 
    StockQuantity < 5
ORDER BY 
    StockQuantity ASC;

