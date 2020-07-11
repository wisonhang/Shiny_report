library(shiny)
library(shinydashboard)
library(tidyverse)
library(highcharter)
library(DT)
library(shinymanager)
library(RSQLite)
library(periscope)
# credentials <- data.frame(
#   user = c("shiny", "shinymanager"),
#   password = c("shiny", "shinymanager"),
#   comment = c("Easy interactive web applications with R",
#               "Simple and secure authentification mechanism for single 'Shiny' applications."),
#   admin = c(FALSE, TRUE),
#   stringsAsFactors = FALSE
# )
# 
# create_db(
#   credentials_data = credentials,
#   sqlite_path = "database.sqlite", # will be created
#   passphrase = 'z00dawei'
# )



# sample data
demographics <- tibble(
  category = c(rep("Gender", 2), rep("Age",5), rep("Social", 7)),
  demographic = c("Male", "Female", "15-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75+", LETTERS[1:5]),
  percent = c(48.706585, 51.293415, 18.676534, 21.136115, 19.066600, 18.326197, 10.709079, 7.270722, 
              4.814752, 8.143243, 33.772399, 34.756400, 15.035762, 8.292197)
)

# source modules
source("modules.R")

ui <- dashboardPage(
  dashboardHeader(title = "Shiny Modules"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    #singleton(tags$head(tags$link(rel="stylesheet", type = "text/css", href = "animation.css")))
    
    tags$head(tags$style(HTML('
          html {
                height: 100%;
                width: 100%;
                position: relative;
              }
            body {
            width: 100%;
            min-height: 100%;
            position: relative;
             -moz-transform: 80%; /* Moz-browsers */
             zoom: 80%; /* Other non-webkit browsers */
             zoom: 80%; /* Webkit browsers */
            }
            .page-main{
            height: 100%;
            position:relative;
            }
           .content-wrapper {
             background-color:#ffffff ;
             hight:auto;
           }
           .content-wrapper,
            .left-side {
            background-color: #ffffff;
            }
           .main-sidebar {
             background-color: white ;}
           .skin-blue .main-sidebar {
            background-color:  white;}
        '))),
    fluidRow(chartTableBoxUI(id = "Age"),
             chartTableBoxUI(id='Gender')
             ), # render the tabBox inside a fluidRow
    fluidRow(
    tabBox(width=12,
           tabPanel(icon('table'),
                    h2('DAU split by DAU Source',style='weight:bold'),
                    filter_ui('DAU_A')
           ),
           tabPanel(icon('table'),
                    h2('DAU split by Member Source',style='weight:bold'),
                    filter_ui('DAU_B')
           )
           
    ),
    div(class= "col-xs-12 col-sm-6 col-md-6",
      tabBox(width=12,
               tabPanel(icon("bar-chart"),downladTableUI("iris")),
               tabPanel(icon("bar-chart"),downladTableUI("cars"))
              )
        )
      )
   )
    
           
           
)
ui <- secure_app(ui,id = "auth",
                 tag_img = tags$img(
                   src = "https://i.postimg.cc/h4dk6HWH/OYO.png", width = 200
                 ),
                 tag_div = tags$div(
                   tags$p(
                     "Default account is your email address before @, and the password is 12345.
                      If you have no authentication, please contact ",
                     tags$a(
                       href = "david.hang@oyohotels.cn",
                       target="_top", "david.hang"
                     )
                   )
                 ),
                 enable_admin = T)


server <- function(input, output, session) {
  # res_auth <- secure_server(
  #   check_credentials = check_credentials(credentials)
  # )
  res_auth <- secure_server(
    check_credentials = check_credentials(
      db = "database.sqlite",
      passphrase = 'z00dawei'
    ), timeout = 0
  )
  callModule(chartTableBox, id = "Age", data = demographics, dem_group = "Age")
  callModule(chartTableBox, id = "Gender", data = demographics, dem_group = "Gender")
  
  callModule(filter_ui_select,'DAU_A')
  callModule(filter_ui_update,'DAU_A')
  callModule(filter_ui_select,'DAU_B')
  callModule(filter_ui_update,'DAU_B')
  
  callModule(downloadDT,'iris',iris)
  callModule(downloadDT,'cars',cars)
}

shinyApp(ui, server)