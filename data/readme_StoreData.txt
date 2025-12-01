Ye, Taotao and Venkatesh Shankar (2024), Close a Store to Open a Pandora’s Box? The Effects of Store Closure on Sales, Omnichannel Shopping, and Mobile App Usage 


This replication file consists of two main folders. The first folder replicates the results at the aggregate level, including Tables 3-5 and Figure 2 from the paper. 
The second folder contains the data and code necessary to replicate the individual-level analyses, covering Tables 6-10. Both folders use a subsample of the original panel data with disguised values to maintain confidentiality.

Due to a non-disclosure agreement, location-specific information for each county has been removed, and the numerical values in the dataset have been altered by adding random numbers and multiplying by random factors (between 1 and 1.1). 
Therefore, the estimates provided here may differ slightly from those in the original paper.


The first folder, titled "Aggregate Level Analysis Tables 3 - 5, and Figure 2" contains three key files used to replicate the main findings at the aggregate level.

The Stata code file "StoreClosure Regression Tables 3, 4 and 5.do" contains the stata commands to generate the main findings of the paper. Specifically, this file produces (1). Table 3 (Main results), Columns 1-8; (2). Table 4 (Main results by channel), Columns 1-6; and (3) Table 5 (Main results by channel with Moderating Effects), Columns 1 - 3.

The R code file "StoreClosure Figure 2.R" provided code to replicate Figure 2 in the main paper. The y-axis values in the figures are different from the original study, as we have disguised the original values with random numbers and factors for confidentiality purposes.

The data file, StoreData.csv, consists of 23 columns and 1,188 observations. These variables include the county identifier number, month index, dummy variables for the difference-in-differences regression, a correction term (IMR), four moderators, and key disguised dependent variables. They are listed as follows:



Variables: Description

county_id: Unique identifier for a county
month: Index for identifying months, taking values from 1 to 18, where 1 to 6 is before the store closure, and 7 to 18 is after the store closure
treat: Dummy variable indicating whether the county has a closed store in January 2016 (1 if treated, 0 if control)
imr: Inverse Mills Ratio from the selection model
treat_x_post: Interaction of dummy variable treat and post, where post is 1 for the period after January 2016, otherwise 0

sales_frequency_total: Number of sales transactions in the time period
sales_value_total: Monetary value of sales in the time period ($)
sales_quantity_total: Number of items bought in the time period
returns_frequency_total: Number of product return transactions in the time period
returns_value_total: Monetary value of product returns in the time period ($)
returns_quantity_total: Number of items returned in the time period
return_rate： Ratio of returns to sales
net_sales_value： Net sales value after accounting for returns (Monetary value of sales – Monetary value of returns) ($)
sales_frequency_offline: Number of offline sales transactions in the time period
sales_value_offline: Monetary value of offline sales in the time period ($)
sales_quantity_offline: Number of items bought offline in the time period
sales_frequency_online: Number of online sales transactions in the time period
sales_value_online: Monetary value of online sales in the time period ($)
sales_quantity_online: Number of items bought online in the time period
pct_incremental_distance: Percentage increase in distance to the nearest open store after a store closure
pct_discounts: Percentage of discounts offered in the county before store closure
pct_online_sales: Percentage of sales generated online before store closure
past_return_value: Monetary value of product returns in the county before store closure



The second folder "Individual Level Analysis Tables 6 - 10" contains a Stata code file, Individual Level Analysis Code.do, which replicates the results presented in Tables 6 - 10. Additionally, the folder includes five datasets for different shopper groups: AllShoppersData.csv; VisitorsClosedStoresData.csv; NonVisitorsClosedStoresData.csv; AppUsersData.csv; NonAppUsersData.csv. These dataset contains the following variables:

customer_id: Unique identifier for each customer
month: Index for months, ranging from 1 to 18, where 1-6 represents the period before store closure and 7-18 represents the period after store closure
treat: Dummy variable indicating whether the customer is from a county affected by a store closure in January 2016 (1 for treated customers, 0 for control)
treat_x_post: Interaction of the treatment (store closure) and post-closure periods, where post is 1 after January 2016 and 0 before

purchase_frequency_total: Total number of purchases made by the customer during the time period
purchase_value_total: Total monetary value of purchases made by the customer during the time period ($)
purchase_quantity_total: Total number of items purchased by the customer during the time period
returns_frequency_total: Total number of returns made by the customer during the time period
returns_value_total: Total monetary value of returns made by the customer during the time period ($)
returns_quantity_total: Total number of items returned by the customer during the time period
net_purchase_value: Net monetary value of purchases after accounting for returns ($)
purchase_frequency_offline: Number of offline purchases made by the customer during the time period
purchase_value_offline: Total monetary value of offline purchases made by the customer during the time period ($)
purchase_quantity_offline: Total number of items purchased offline by the customer during the time period
purchase_frequency_online: Number of online purchases made by the customer during the time period
purchase_value_online: Total monetary value of online purchases made by the customer during the time period ($)
purchase_quantity_online: Total number of items purchased online by the customer during the time period

topic_purchase: Score for engagement with topics related to purchases
topic_rewards_program: Score for engagement with rewards programs
topic_store_info: Score for engagement with store-related information
topic_trade_center: Score for engagement with trade center-related topics
topic_general_info: Score for engagement with general information
topic_product_info: Score for engagement with product information
topic_inactive_status: Score indicating inactive customer status