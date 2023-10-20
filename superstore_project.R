library(tidyverse)
library(ggplot2)
library(scales)
library(dplyr)

setwd("C://Users//Asus//OneDrive//Desktop//Pipproject")
df <- read.csv("Sample-Superstore.csv")

head(df)

df %>% summarise_all(funs(sum(is.na())))

df <- df %>%
  mutate(`Order.Date` = mdy(`Order.Date`),
         `Ship.Date` = mdy(`Ship.Date`),
         'Shipping Speed' = `Ship.Date` - `Order.Date`)

#Top 5 product
top_product <- df  %>% 
  group_by(Product.Name) %>%
  summarise(Total_Sales = n()) %>%
  arrange(-Total_Sales) %>%
  slice(1:5)

product <- top_product %>%
  ggplot(aes(x=reorder(`Product.Name`, Total_Sales), y=Total_Sales,color =`Product.Name` )) +
  geom_bar(stat="identity", fill="steelblue") +
  coord_flip() +
  labs(title="Top 5 Products by Sales",
       x="Product Name",
       y="Total Sales") +
  theme_minimal()

top_product %>%
  ggplot(aes(x = 2, y = Total_Sales, fill = `Product.Name`)) +
  geom_bar(width = 1, stat = 'identity', color = 'white') +
  geom_text(aes(label = Total_Sales), position = position_stack(0.5), color = 'white', size = 6, fontface = 'bold') +
  coord_polar(theta = "y") +
  xlim(c(0, 2.5))+
  labs(title='Top 5 Product  by Total Purchase')+
  theme_void()

#Total Sales by region
top_reg <- df %>%
  group_by(Region) %>%
  summarise(total_sales = sum(Sales)) %>%
  mutate(total_sales = sprintf("%.2f", total_sales)) %>%
  arrange(desc(total_sales))

top_reg %>%
  ggplot(aes(x = reorder(Region, as.numeric(total_sales)), y = as.numeric(total_sales),fill = `Region`)) +
  geom_bar(linewidth = 0.5 ,stat = "identity") +
  geom_text(aes(label = total_sales), position = position_stack(0.5), color = 'white', size = 6, fontface = 'bold')+
  coord_flip() +
  labs(title = "Total Sales by Region", x = "Region", y = "Total Sales") +
  scale_y_continuous(labels = comma) +  # Use comma formatting for y-axis
  theme_minimal()


#Total Profit by Category
df_profit_sum <- df %>%
  group_by(`Sub.Category`) %>%
  summarize(Profit = sum(Profit))

ggplot(data = df_profit_sum, aes(x = `Sub.Category`, y = Profit, fill = Profit)) + 
  geom_bar(stat = "identity") +
  scale_fill_viridis_c() + 
  labs(title = 'Total Profit by Item Category', x = 'Category', y = 'Profit') +
  theme_minimal() +
  theme(text = element_text(size = 12), 
        title = element_text(size = 15))











