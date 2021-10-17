library(rvest) #web scrapping package
library(tidyverse) #utility package
library(janitor)

url <- "https://famiqs.viasyst.net/certified-sites"
data <- read_html(url) 
    
table <- data %>% html_table() #html_table returns a list
table <- table[[1]]
table <- clean_names(table) #janitor package

#data_numbers <- data %>% html_nodes("tr") %>%
#    html_attr("data-number")

#scrapping the id values that the website uses to uniquely identify each organisation and
#access their webpages.
ids <- data %>% html_nodes("tr") %>%
    html_attr("data-href") %>%
    str_replace("/certificate/", "") %>%
    tail(-1)

#attrs <- tibble(data_numbers, id) %>%
#    mutate(data_numbers = str_replace(data_numbers, "[\\\n\\\r\\\t ]+", "")) %>%
#    tail(-1)

table <- cbind(table, ids)
