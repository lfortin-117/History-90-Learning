library(dplyr)

#Creating the dataframe for the hypothetical bookstore.
title <- c("Moby Dick", "Ulysses","Hamlet","War and Peace","The Great Gatsby","The Divine Comedy","The Illiad","Pride and Predjudice","Heart of Darkness","The Sun Also Rises")
yearP <- c(1851,1904,1601, 1869, 1925, 1320,675,1813,1899,1925)
author <- c("Herman Melville","James Joyce","Shakespeare","Tolstoy","F. Scott Fitzgerald","Dante Alighieri","Homer","Jane Austin","Joseph Conrad","Ernest Hemingway")
Nstock <- c(28,4,15,16,87,18,23,34,27,54)
Price <- c(25.99,19.99,15.00,34.99,10.49,13.89,15.99,20.59,8.99,9.89)

Bookstore <- data.frame (title, yearP,author,Nstock, Price)


#Sorting data frame in descending order by year published, and then in ascending
#(cont) order by author if published same year.
yearsorted <- Bookstore %>% arrange(desc(yearP),author)

#Creating sale for the bookstore with respect to the years published and their stock
Bookstore$sale1 <- ifelse(Bookstore$yearP>1850,Price,Price*.75 )
Bookstore2 <- Bookstore %>% mutate(sale1=ifelse(yearP>1850, Price, Price*.75)) %>%
  mutate(sale1=ifelse(Nstock>25,Price*0.6,sale1)) %>%
  mutate(sale1=ifelse(Nstock>25 & yearP<1850, Price*0.5,sale1 ))

#Creating a subset that includes only the books on sale, their authors, and the price.
SALEBOOKS <- Bookstore2 %>% select(title, author, sale1) %>% filter(Nstock>25 | yearP<1850)
