library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(DT) #package for interactive tables
library(tidyverse)

load("main.RData")

header <- dashboardHeader(
    title = "FAMI_QS"
)

sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Main Table", tabName = "main_table"),
        menuItem("Ingredients / Mixtures", tabName = "ingredients_mixtures")
    )
)

body <- dashboardBody(
    tabItems(
        tabItem(tabName = "main_table",
            sidebarLayout(
                sidebarPanel(
                    radioButtons('act', "Sort by (activities)", selected = "None",
                                 choices = c("None", "Production", "Trading", 
                                             "Production, Trading")),
                    searchInput('qs_no', "Search by QS-number"),
                    searchInput('site_name', "Search by site name")
                ),
                mainPanel(
                    DTOutput('main_table')
                )
            )
        ),
        tabItem(tabName = "ingredients_mixtures",
            sidebarLayout(
                sidebarPanel(
                    radioButtons('type', "Select type", selected = "Ingredients",
                                 choices = c("Ingredients", "Mixtures")),
                    searchInput('qs_no1', "Search by QS-Number")
                ),
                mainPanel(
                    DTOutput('type_table')
                )
            )
        )
    )
)

ui <- dashboardPage(header, sidebar, body, skin = "yellow")


server <- function(input, output, session) {
    
    #function to subset data by activities
    rval_act <- reactive({
        if (input$act == "None")
            act <- table
        else {
            act <- table %>% 
                filter(table$activities == input$act)
        }
        act
    })
    
    #function to return either ingredients or mixtures table
    rval_type <- reactive({
        if (input$type == "Ingredients")
            type <- ingredients_table
        else
            type <- mixtures_table
        type
    })
    
    #function to subset data based on serached values
    rval_main <- reactive({
        act_table <- rval_act()
        if (input$qs_no == "" && input$site_name == "")
            main <- act_table
        else if (input$site_name == "") {
            main <- act_table %>%
                filter(str_detect(fami_qs_number, input$qs_no))
        } else if (input$qs_no == "") {
            main <- act_table %>%
                filter(str_detect(site_name, input$site_name))
        } else {
            main <- act_table %>%
                filter(str_detect(fami_qs_number, input$qs_no)) %>%
                filter(str_detect(site_name, input$site_name))
        }
        main
    })
    
    #function to join the main table and either the ingredients or mixtures table
    rval_secondary <- reactive({
        type_table <- rval_type()
        secondary <- table %>%
            filter(str_detect(fami_qs_number, input$qs_no1)) %>%
            left_join(type_table, by = c("ids" = "id"))
        secondary
    })
    
    output$main_table <- renderDT({
        dt <- rval_main()
        dt %>% select(-ids)
    })
    
    output$type_table <- renderDT({
        type_dt <- rval_secondary()
        type_dt %>% select(-(3:9)) #keep only the name, number and type selected
    })
    
}


shinyApp(ui = ui, server = server)
