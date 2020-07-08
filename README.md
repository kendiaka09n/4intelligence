# 4intelligence
 
R-Cloud
https://rstudio.cloud/project/1443782

#CASE 1

# Packages ----------------------------------------------------------------
library(forecast)
library(tidyverse)
library(lubridate)
library(magrittr)
library(dplyr) 


# Loading and Modeling ----------------------------------------------------
tfp <- read_csv('TFP.csv')


# Exploratory Analysis ----------------------------------------------------


str(tfp)
unique(tfp$isocode)
#e countries - CAN, MEX and USA
summary(tfp) 
#Date from 1950 to 2011
head(tfp)
tail(tfp)

dados <- tibble(year = c(tfp$year), country = c(tfp$isocode), rft = c(tfp$rtfpna)) %>% view()

#There isa great distribution of rtfpna to Mexico and USA, if compared to CANADA. 
ggplot(dados,aes(y=rft,x=country))+ggtitle("Distribution")+geom_point()

#In a time chart we can observe that the countries Mexico and USA are getting close to rft 1,0 along the time. 
ggplot(dados,aes(y=rft,x=year, color = country))+ggtitle("Distribution")+geom_point()

cycle(usa)

usa <- dados %>% filter(country %in% c("USA"))
usa %>% select(rft, year) %>% 
  forecast.ets(usa, h = 10, lambda = FALSE) 



# Case 1.3 ----------------------------------------------------------------

#3- Check in the following link pages 2 and 3: https://cran.r-project.org/web/packages/pwt8/pwt8.pdf to see a list of all variables in the original dataset. Can you think about another feature that could be helpful in explaining TFP series? Explain.

#R3: The variables  pop, emp, csh_c and xr would be helpful to explain the TFP series. 
#The number of population and people engaged we would observe the growth of the poplation and the increasing/decreasing of the quantity of jobs. The difference between pop and emp would explain how the is the development of industry and the economic health of the country. The csh_c variable would indicate the behavior of the population, if there`s money is circling thought the market or if it`s stocked with the population due to the high prices or economical stability.
Finaly the xr variable would indicate the country`s health and how is the Exterior commerce and outside investment in the country.



#CASE 2


# Packages ----------------------------------------------------------------

library(ggplot2)
library(forecast)
library(tidyverse)
library(lubridate)
library(magrittr)
library(dplyr) 


# Extraction and Modeling -------------------------------------------------

#Extracting data from csv and encoding to Latin 3. Date is treated and inserted day, month and year. 
agro <- read.csv('data_comexstat.csv', encoding = "Latin3")
agro <- agro %>%  mutate(date = as.Date(date)) %>% 
  mutate( date = ymd(parse_date_time(date, 
                                   orders = c("ymd", "dmy", "mdy"))) )%>%
  mutate(date = ymd(date),
         dia = day(date),
         mes = month(date),
         ano = year(date))

str(agro)
levels(agro$product)
summary(agro)
unique(agro$product)


# Case 2.1 ----------------------------------------------------------------

#1- Show the evolution of total monthly and total annual exports from Brazil (all states and to everywhere) of 'soybeans', 'soybean oil' and 'soybean meal';
#An interesting observation is the decrease in exportation due to late 2019. Looking close to the great importer of the brazilian commodity, we can see the decrese of almost 20% of all the soybeans sales to china in 2018.This may me related to Covid-19, and it is needed further study. 

#Total Monthly
agro_expor_mes <- agro %>% filter (product %in% c ("soybeans", "soybean_oil", "soybean_meal"), type %in% c ("Export")) %>% 
group_by(date ,product, country) %>% 
summarise("tons" = sum(tons), "usd" = sum(usd)) %>% 
  arrange(desc(usd))

agro_expor_mes %>% ggplot(aes(y=usd, x=date, color = product)) + 
  ggtitle("Exportation (US$) by Year and Product") + 
  geom_line() + 
  geom_point(size = 1)

#A close look for the importation to china in december 2019, time of the outbreak of Covid-19.
agro_expor_mes_China <- agro %>% filter (product %in% c ("soybeans", "soybean_oil", "soybean_meal"), country %in% ("China"), type %in% c ("Export")) %>% 
  group_by(date ,product, country) %>% 
  summarise("tons" = sum(tons), "usd" = sum(usd)) %>% 
  arrange(desc(date))
agro_expor_mes_China %>% head(20)


#Total Annual

agro_expor_ano <- agro %>% filter (product %in% c ("soybeans", "soybean_oil", "soybean_meal"), type %in% c ("Export")) %>% 
  group_by( ano ,product, country) %>% 
  summarise("tons" = sum(tons), "usd" = sum(usd)) %>% 
  arrange(desc(usd)) 
agro_expor_ano %>% head(10)

agro_expor_ano %>% 
  ggplot(aes(y=usd, x=ano, color = product)) + ggtitle("Exportation (US$) by Year and Product") + geom_line() + geom_point(size = 1)




# Case 2.2 ----------------------------------------------------------------

#2- What are the 3 most important products exported by Brazil in the last 5 years?
#R: The 3 main products for Brazilian Exportation in the last 5 years were soybeans (1), sugar (2) and soybean meal (3). 
agro_top3 <- agro %>%filter ( type %in% c ("Export"), date > "2015-01-01") %>% 
  group_by(product) %>% 
  summarise("tons" = sum(tons), "usd" = sum(usd)) %>% 
  arrange(desc(usd)) 
agro_top3 %>% head(3)


agro_top3 %>% ggplot(aes(x= product, y=usd, color = product)) + ggtitle("Exportation (US$) by Product") + geom_col()


# Case 2.3 ----------------------------------------------------------------

#3- What are the main routes through which Brazil have been exporting 'corn' in the last few years? Are there differences in the relative importancem of routes depending on the product?
#R1:.The main exporting route is through sea, once the main buyers of the brazilian commodity are from other continets, so the cheapest way to conclude the trade is by sea. 
agro_expor_corn <- agro %>% filter (product %in% c ("corn"),type %in% c("Export")) %>% 
  group_by( country, route) %>% 
  summarise("tons" = sum(tons), "usd" = sum(usd)) %>% 
  arrange(desc(usd)) %>% 
  view()

agro_expor_corn %>% 
  ggplot(aes( y= usd, x = route , color = route)) + 
  geom_col() 



#R2:We do have some differences between the type of transport due to the location of the country. One example is Paraguay, a country that Brazil only have ground borders. The ground transport appears mostly to south america countries, this may be due to the location of the production site (farm), which can be more expensive to the farmer to send it to brazilian ports than tranport it by trucks to others countries located near to brazilian border.   

agro_expo_total <- agro %>% 
  group_by(route, country) %>% 
  summarise("tons" = sum(tons), "usd" = sum(usd)) %>% 
  arrange(desc(usd)) %>% 
  view()

agro_expo_total <- agro %>% 
  group_by(route,product, country) %>% 
  summarise("tons" = sum(tons), "usd" = sum(usd)) %>% 
  arrange(desc(usd)) %>% 
  view()

agro_expo_total %>% 
  filter( (country %in% c("Argentina" , "Uruguai", 'Paraguai'))) %>%
  view()


# Case 2.4 ----------------------------------------------------------------
#4-Which countries have been the most important trade partners for Brazil in terms of 'corn' and 'sugar' in the last 3 years?

#R: Dividing the analysis with exportation and importation, we can see that Iran, Bangladesh and Egypt are the top 3 buyers of Sugar and corn (Cash Value), the same is true to top 3 countries that Brazil exports to. 
#If we look this data specifically for each commodity, we find that the top 3 corn buyers are Iran, Japan and Vietnam and the top 3 sugar buyers are Algeria, Bangladesh and India. 
#For importation we observe the same differences.The top 3 countries that brazil most import are Paraguay, Argentina and United States, but when we split the data though the products, we`ll fin that for corn Paraguay, Argentina and United States are the top 3 and for Sugar ar United States, Chile and Argentina.   
#So, depending how deep the necessity of the analysis (Improting or exporting), we should look differently

#Exportation and Importation
agro_top_country_export_import <- agro %>%filter (product %in% c ("corn",'sugar'), date > '2017-01-1') %>% 
  group_by(country) %>% 
  summarise("tons" = sum(tons), "usd" = sum(usd)) %>% 
  arrange(desc(usd))

agro_top_country_export_import %>% head(3)

#Exportation overall
agro_top_country_export_overall <- agro %>%filter (product %in% c ("corn",'sugar'), type %in% c ("Export"), date > '2017-01-1') %>% 
  group_by(country) %>% 
  summarise("tons" = sum(tons), "usd" = sum(usd)) %>% 
  arrange(desc(usd))

agro_top_country_export_overall %>% head(3)

#Exportation by product
agro_top_country_export_product <- agro %>%filter (product %in% c ("corn",'sugar'), type %in% c ("Export"), date > '2017-01-1') %>% 
  group_by(country, product) %>% 
  summarise("tons" = sum(tons), "usd" = sum(usd))%>% 
  arrange(desc(usd))

agro_top_country_export_product %>% head(10)

#Importation overall
agro_top_country_import_overall <- agro %>%filter (product %in% c ("corn",'sugar'), type %in% c ("Import"), date > '2017-01-1') %>% 
  group_by(country) %>% 
  summarise("tons" = sum(tons), "usd" = sum(usd))%>% 
  arrange(desc(usd)) 

agro_top_country_import_overall %>% head(3)

#Importation by product
agro_top_country_import_product <- agro %>%filter (product %in% c ("corn",'sugar'), date > '2017-01-1') %>% 
  group_by(country, product) %>% 
  summarise("tons" = sum(tons), "usd" = sum(usd))%>% 
  arrange(desc(usd)) 
  
agro_top_country_import_product %>% head(10)


# Case 2.5 ----------------------------------------------------------------
#5 - For each of the products in the dataset, show the 5 most important states in terms of exports
#R: As the question 4, we can analyze the by the total amount (US$) exported, which we can number the top 5 states Mato Grosso, Paran?, S?o Paulo, Rio Grande do Sul and Goi?s.
#If we split the data through each product, we can find another sequence of top states: corn (MT, PR, GO, MS, SP), soybean_meal(MT, PR, RS, GO, BA), soybean_oil (PR, RS, MT, SC, GO), soybeans(MT, PR, RS, GO, MS), sugar (SP, MG, PR, AL, MS) and wheat (RS, PR, SC, SP, MS)       



#Exportation overall by state
agro_state <- agro %>%filter (type %in% c ("Export")) %>% 
  group_by(state) %>% 
  summarise("tons" = sum(tons), "usd" = sum(usd)) %>% 
  arrange(desc(usd))

agro_state %>% 
  head(5)

#Exportation overall by state and corn
agro_state_corn <- agro %>%filter (type %in% c ("Export"), product %in% c ("corn")) %>% 
  group_by(state) %>% 
  summarise("tons" = sum(tons), "usd" = sum(usd)) %>% 
  arrange(desc(usd))

agro_state_corn %>% 
  head(5)

#Exportation overall by state and soybean_meal
agro_state_soybean_meal<- agro %>%filter (type %in% c ("Export"), product %in% c ("soybean_meal")) %>% 
  group_by(state) %>% 
  summarise("tons" = sum(tons), "usd" = sum(usd)) %>% 
  arrange(desc(usd))

agro_state_soybean_meal %>% 
  head(5)

#Exportation overall by state and soybean_oil
agro_state_soybean_oil<- agro %>%filter (type %in% c ("Export"), product %in% c ("soybean_oil")) %>% 
  group_by(state) %>% 
  summarise("tons" = sum(tons), "usd" = sum(usd)) %>% 
  arrange(desc(usd))

agro_state_soybean_oil %>% 
  head(5)


#Exportation overall by state and soybeans
agro_state_soybeans <- agro %>%filter (type %in% c ("Export"), product %in% c ("soybeans")) %>% 
  group_by(state) %>% 
  summarise("tons" = sum(tons), "usd" = sum(usd)) %>% 
  arrange(desc(usd))

agro_state_soybeans %>% 
  head(5)

#Exportation overall by state and sugar
agro_state_sugar <- agro %>%filter (type %in% c ("Export"), product %in% c ("sugar")) %>% 
  group_by(state) %>% 
  summarise("tons" = sum(tons), "usd" = sum(usd)) %>% 
  arrange(desc(usd))

agro_state_sugar %>% 
  head(5)

#Exportation overall by state and wheat
agro_state_wheat <- agro %>%filter (type %in% c ("Export"), product %in% c ("wheat")) %>% 
  group_by(state) %>% 
  summarise("tons" = sum(tons), "usd" = sum(usd)) %>% 
  arrange(desc(usd))

agro_state_wheat %>% 
  head(5)

