
SET GLOBAL sql_mode = 'ONLY_FULL_GROUP_BY';
select @@GLOBAL.sql_mode;

CREATE DATABASE `superstoresdb` ;

# setting the superstoresdb as a default schema
USE `superstoresdb`; 

# Imported all the excel data files in to database called superstoresdb and reviewing the table structures.

describe cust_dimen;
describe market_fact;
describe orders_dimen;
describe prod_dimen;
describe shipping_dimen;

select * from cust_dimen limit 5;
select * from market_fact limit 5;
select * from orders_dimen limit 5;
select * from prod_dimen limit 5;
select * from shipping_dimen limit 5;

-- Verifying the total number of records in all the five tables and matching with excel file total count.
select count(*) from cust_dimen;
select count(*) from market_fact;
select count(*) from orders_dimen;
select count(*) from prod_dimen;
select count(*) from shipping_dimen;

/*
*********************************************************************
Task1
*********************************************************************
Database Name: superstoresdb
Superstore sales data has multiple files containing information about the details of its customers,orders,products,shipping and market transaction data mainly stores sales data.
To automate the entire process of managing these excel files for consistency they have decided to create a superstoredb  in a database with all those files.Data is imported to mainly 5 tables one to one.

cust_dimen table gives information about the customers of the superstore which has columns Cust_id(varchar(255)) Not null ,Customer_Name(text),Province(text),Region(text),Customer_Segment(text).
orders_dimen table containing all the details of orders  which has columns Order_ID(INT(11)),Order_Date(date),Order_Priority(text), Ord_id(varchar(255)).
prod_dimen table containing all the details of the products which has columns Prod_id(varchar(255)) Not null ,Product_Category(text),Product_Sub_Category(text).
shipping_dimen table containing shipping details which has columns Ship_id(varchar(255)),Order_ID(int(11)),Ship_Mode(text), Ship_Date(date).
market_fact is a fact table(store transaction table) containing the marketing facts for all the products customers and orders which has columns Ord_id(varchar(255)),Prod_id(varchar(255)),Ship_id(varchar(255)),Cust_id(varchar(255)),Sales(Double),Discount(Double),Order_Quantity(int(11)),Profit(Double),Shipping_Cost(Double),Product_Base_Margin(Double).

Generally Text data types are not suggested as primary keys in real projects reason String types are usually requiring more space and are much slower then integer types.
It is much easier to compare always two numbers, than two strings and this comparison of strings may introduce additional processing time.

Given this is a learning case study so I'm considering Cust_id,Prod_id,Ord_id,Ship id varchar/text columns as a Primary key from the tables observation is on these columns we have unique records.

This Schema resembles as a Start Schema.

cust_dimen - Cust_id is Primary Key, no foreign key exists.
prod_dimen - Prod_id as Primary Key, no foreign key.
orders_dimen - Ord_id as Primary Key,  no foreign key.Order_ID is a Integer but it has duplicates so we cann't use this to join with shipping_dimen. If we want to join between orders_dim,shipping_dim then using Market_fact we cna do that.
shipping_dimen - Ship id as primary key and no foreign key
market_fact -  Ord_id,Prod_id,Ship_id,Cust_id as foreign key no primary keys exists.

*********************************************************************
*/

# Modifying the data Types,Creating the primary keys and foreign keys in the tables.


ALTER TABLE cust_dimen MODIFY Cust_id VARCHAR(255) NOT NULL;
ALTER TABLE cust_dimen add PRIMARY KEY(cust_id);

ALTER TABLE `superstoresdb`.`orders_dimen` 
CHANGE COLUMN `Ord_id` `Ord_id` VARCHAR(255) NOT NULL ,
ADD PRIMARY KEY (`Ord_id`);

ALTER TABLE `superstoresdb`.`shipping_dimen` 
CHANGE COLUMN `Ship_id` `Ship_id` VARCHAR(255) NOT NULL ,
ADD PRIMARY KEY (`Ship_id`);

ALTER TABLE `superstoresdb`.`market_fact` 
CHANGE COLUMN `Ord_id` `Ord_id` VARCHAR(255) NULL DEFAULT NULL ,
CHANGE COLUMN `Prod_id` `Prod_id` VARCHAR(255) NULL DEFAULT NULL ,
CHANGE COLUMN `Ship_id` `Ship_id` VARCHAR(255) NULL DEFAULT NULL ,
CHANGE COLUMN `Cust_id` `Cust_id` VARCHAR(255) NULL DEFAULT NULL ;

ALTER TABLE `superstoresdb`.`market_fact` 
ADD INDEX `ord_id_idx` (`Ord_id` ASC),
ADD INDEX `Prod_id_idx` (`Prod_id` ASC),
ADD INDEX `Ship_id_idx` (`Ship_id` ASC),
ADD INDEX `Cust_id_idx` (`Cust_id` ASC);
ALTER TABLE `superstoresdb`.`market_fact` 
ADD CONSTRAINT `ord_id`
  FOREIGN KEY (`Ord_id`)
  REFERENCES `superstoresdb`.`orders_dimen` (`Ord_id`)
  ON DELETE NO ACTION
  ON UPDATE NO ACTION,
ADD CONSTRAINT `Prod_id`
  FOREIGN KEY (`Prod_id`)
  REFERENCES `superstoresdb`.`prod_dimen` (`Prod_id`)
  ON DELETE NO ACTION
  ON UPDATE NO ACTION,
ADD CONSTRAINT `Ship_id`
  FOREIGN KEY (`Ship_id`)
  REFERENCES `superstoresdb`.`shipping_dimen` (`Ship_id`)
  ON DELETE NO ACTION
  ON UPDATE NO ACTION,
ADD CONSTRAINT `Cust_id`
  FOREIGN KEY (`Cust_id`)
  REFERENCES `superstoresdb`.`cust_dimen` (`Cust_id`)
  ON DELETE NO ACTION
  ON UPDATE NO ACTION;
  
ALTER TABLE Prod_dimen MODIFY Prod_id VARCHAR(255) NOT NULL;
ALTER TABLE Prod_dimen ADD PRIMARY KEY(Prod_id);


/*
*********************************************************************
Task2 :Basic Analysis
*********************************************************************
*/

# A. Find the total and the average sales (display total_sales and avg_sales)

SELECT
   ROUND(SUM(Sales),2) as 'Total_Sales', 
   ROUND(AVG(Sales),2) as 'Avg_Sales'
FROM market_fact;


/* B. Display the number of customers in each region in decreasing order of no_of_customers. 
 The result should contain columns Region, no_of_customers */

SELECT Region, 
	COUNT(Cust_id) as 'No_Of_Customers'
FROM cust_dimen
GROUP BY Region
ORDER BY 2 DESC;

# C. Find the region having maximum customers (display the region name and max(no_of_customers)

SELECT region,
       COUNT(Cust_id) as 'No_Of_Customers' 
 FROM cust_dimen 
 GROUP BY region HAVING COUNT(Cust_id) = (SELECT MAX(no_of_customers) 
     FROM (SELECT COUNT(Cust_id) as 'No_Of_Customers' 
           FROM cust_dimen
           GROUP BY Region
			) mytable
                    );
                    
# D. Find the number and id of products sold in decreasing order of products sold (display product id, no_of_products sold)

SELECT Prod_id, 
       SUM(Order_Quantity) as 'No_Of_Products_Sold'
FROM market_fact
GROUP BY Prod_id
ORDER BY 2 DESC;

/* E. Find all the customers from Atlantic region who have ever purchased ‘TABLES’ and the number of tables purchased 
 (display the customer name, no_of_tables purchased) */


SELECT  c.Customer_Name as 'Customer_Name', 
        SUM(m.Order_Quantity) as 'No_Of_Tables'
FROM market_fact m 
		JOIN cust_dimen c ON m.Cust_id = c.Cust_id
        JOIN prod_dimen p ON m.Prod_id = p.Prod_id
WHERE c.Region = "ATLANTIC" AND p.Product_Sub_Category = "TABLES"
GROUP BY c.Customer_Name
ORDER BY 2 DESC;

/*
*********************************************************************
Task3 :Advanced Analysis
*********************************************************************
*/

# A. Display the product categories in descending order of profits (display the product category wise profits i.e. product_category, profits)?
SELECT p.Product_Category AS 'Product_Category', 
       ROUND(SUM(m.Profit), 2) AS 'Profits'
FROM market_fact m 
        JOIN prod_dimen p ON m.Prod_id = p.Prod_id
GROUP BY p.Product_Category
ORDER BY 2 DESC;

# B. Display the product category, product sub-category and the profit within each sub-category in three columns. 

SELECT p.Product_Category as 'Product_Category', 
	   p.Product_Sub_Category as 'Product_Sub_Category',
	   ROUND(SUM(m.Profit), 2) as 'Profit'
FROM market_fact m 
        JOIN prod_dimen p ON m.Prod_id = p.Prod_id
GROUP BY p.Product_Category,p.Product_Sub_Category
ORDER BY 1;

# C. Where is the least profitable product subcategory shipped the most? For the least profitable product sub-category, display the region-wise no_of_shipments and the profit made in each region in decreasing order of profits (i.e. region, no_of_shipments, profit_in_each_region)

/* Option 1:
SELECT c.Region as 'Region',
       COUNT(m.Ship_id) as 'No_Of_Shipments', 
	   ROUND(SUM(m.Profit),2) as 'Profit'
FROM market_fact m 
		JOIN cust_dimen c ON m.Cust_id = c.Cust_id
        JOIN prod_dimen p ON m.Prod_id = p.Prod_id
WHERE Product_Sub_Category = (
				SELECT p.Product_Sub_Category 
				FROM market_fact m 
					JOIN prod_dimen p ON m.Prod_id = p.Prod_id
					GROUP BY Product_Sub_Category
					ORDER BY SUM(m.Profit)
					LIMIT 1) 
GROUP BY c.Region
ORDER BY Profit DESC;
*/


SELECT c.Region as 'Region',
       COUNT(m.Ship_id) as 'No_Of_Shipments', 
	   ROUND(SUM(m.Profit),2) as 'Profit'
FROM market_fact m 
		JOIN cust_dimen c ON m.Cust_id = c.Cust_id
        JOIN prod_dimen p ON m.Prod_id = p.Prod_id
WHERE Product_Sub_Category = (
				SELECT p.Product_Sub_Category
 FROM market_fact m 
	 JOIN prod_dimen p ON m.Prod_id = p.Prod_id
 GROUP BY p.prod_id HAVING ROUND(sum(m.Profit),2) = (SELECT ROUND(min(mysum),2) 
     FROM (select ROUND(sum(m2.Profit),2) mysum FROM market_fact m2 GROUP BY m2.prod_id) mytable
     )) 
GROUP BY c.Region
ORDER BY Profit DESC;


/*                    
  
--Finding the least profitable product sub-category    
SELECT p.Product_Sub_Category,
       ROUND(sum(m.Profit),2)
 FROM market_fact m 
	 JOIN prod_dimen p ON m.Prod_id = p.Prod_id
 GROUP BY p.prod_id HAVING ROUND(sum(m.Profit),2) = (SELECT ROUND(min(mysum),2) 
     FROM (select ROUND(sum(m2.Profit),2) mysum FROM market_fact m2 GROUP BY m2.prod_id) mytable
     );

--- Verfiying the profit for the above query:

 SELECT p.Product_Sub_Category,ROUND(sum(m.Profit),2) as 'Profit' 
				FROM market_fact m 
					JOIN prod_dimen p ON m.Prod_id = p.Prod_id
					GROUP BY Product_Sub_Category
					ORDER BY sum(m.Profit) desc;    
     */

  