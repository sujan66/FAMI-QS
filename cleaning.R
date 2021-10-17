library(jsonlite)

site <- "https://famiqs.viasyst.net/certificate/"

activities_table <- tibble() #stores the activities of each site
ingredients_table <- tibble() #stores the ingredients of each site, if any
mixtures_table <- tibble() #stores the mixtures of each site, if any
jsons <- list() 

for (i in 1:nrow(table)) {
    
    id <- table$ids[i]
    link <- paste(site, id, sep = "")
    print(paste("processing:", link))
    #download.file(link, destfile = "page.html", quiet = T)
    temp <- read_html(link)
    
    #collect the titles from each site page. if number of title is 4, then
    #both ingredients and mixtures exist for the site. If number of title is 3,
    #then either ingredients or mixtures exist for the site
    titles <- temp %>% 
        html_nodes("div.d-block > h6") %>% html_text()
    
    #extract the activity of each site
    activities <- temp %>% 
        html_nodes("div.list-group:nth-child(2) > div") %>% 
        html_text()
    if (length(activities) == 0) {
        activities <- temp %>% 
            html_nodes("div.list-group:nth-child(3) > div") %>% 
            html_text()
    }
    
    #Due to the irregularity of the web pages, the ingredients or mixtures
    #can exist as the 4th or the 5th child, 6th or 7th child if both 
    #ingredients and mixtures exist for the site
    ingredients = ""
    mixtures = ""
    if (length(titles) == 4) {
        ingredients <- temp %>% 
            html_nodes("div.list-group:nth-child(4) > div") %>% 
            html_text()
        if (length(ingredients) == 0) {
            ingredients <- temp %>% 
                html_nodes("div.list-group:nth-child(5) > div") %>% 
                html_text()
        }
        
        mixtures <- temp %>% 
            html_nodes("div.list-group:nth-child(6) > div") %>% 
            html_text()
        if (length(mixtures) == 0) {
            mixtures <- temp %>% 
                html_nodes("div.list-group:nth-child(7) > div") %>% 
                html_text()
        }
    } else {
        #detect if titles have ingredients or mixtures in it
        if (str_detect(titles, "Ingredients")[2]) {
            ingredients <- temp %>% 
                html_nodes("div.list-group:nth-child(4) > div") %>% 
                html_text()
            if (length(ingredients) == 0) {
                ingredients <- temp %>% 
                    html_nodes("div.list-group:nth-child(5) > div") %>% 
                    html_text()
            }
        } else {
            mixtures <- temp %>% 
                html_nodes("div.list-group:nth-child(4) > div") %>% 
                html_text()
            if (length(mixtures) == 0) {
                mixtures <- temp %>% 
                    html_nodes("div.list-group:nth-child(5) > div") %>% 
                    html_text()
            }
        }
    }
    
    #binding new rows to their respective tables
    df_row <- data.frame(id, activities)
    activities_table <- rbind(activities_table, df_row)
    
    df_row <- data.frame(id, ingredients)
    ingredients_table <- rbind(ingredients_table, df_row)
    
    df_row <- data.frame(id, mixtures)
    mixtures_table <- rbind(mixtures_table, df_row)
    
    if (length(activities) == 1)
        activities <- unbox(activities)
    if (length(ingredients) == 1)
        ingredients <- unbox(ingredients)
    if (length(mixtures) == 1)
        mixtures = unbox(mixtures)
    
    info <- list(activities, ingredients, mixtures)
    names(info) <- c("activities", "ingredients", "mixtures")
    jsons[[id]] <- toJSON(info)
    
    Sys.sleep(10)
}

#manually imputing values to erroneous rows
activities_table[activities_table$id == "2694007/0", "activities"] <- NA
mixtures_table[mixtures_table$id == "2694007/0",
               "mixtures"] <- "Mixture functionality: Nutritional"

activities_table <- activities_table %>%
    group_by(id) %>%
    summarise(activities = toString(activities)) %>%
    ungroup()

#adding NAs to empty cells
mixtures_table <- mixtures_table %>%
    mutate_all(na_if, "") %>%
    mutate_all(na_if, "  ()")

ingredients_table <- ingredients_table %>%
    mutate_all(na_if, "")

#joining the original table and the activities table
table <- table %>%
    inner_join(activities_table, by = c("ids" = "id"))
