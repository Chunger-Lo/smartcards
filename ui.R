require(shiny)
library(shinydashboard)
require(shinydashboardPlus)
# library(shinycssloader)
require(fresh)
require(dplyr)


mytheme <- create_theme(
  adminlte_color(
    light_blue = "#019C96"
  )
)
## header ----
header <- dashboardHeader(tags$li(class = "dropdown",
                                  tags$style(".main-header {max-height: 80px}"),
                                  tags$style(".main-header .logo {height: 80px}")
                                  )
                          )

header$children[[2]]$children[[2]] <- header$children[[2]]$children[[1]]
header$children[[2]]$children[[1]] <- tags$a(href='https://www.esunbank.com.tw/bank/personal/credit-card/intro',
                                             tags$img(src='long.png', width = 220, height = 80),

                                             target = '_blank', #,
                                             height='6',width='228.6', align = 'right')
## sidebar ----
sidebar <- dashboardSidebar(
  width = 200,
  tags$style(".left-side, .main-sidebar {padding-top: 100px}"),
  sidebarMenu(
    id = 'sidebar',
    menuItem("卡片推薦", tabName = 'recommendation', icon = icon('lightbulb') ),
    menuItem( "關於玉山", tabName = 'about', icon = icon('info-circle'))
  )
)
## body ----
body <- dashboardBody(
  use_theme(mytheme),
  tabItems(
    ## tab1: recommendation ----
    tabItem(
      tabName = 'recommendation',
      column(width = 5,
             uiOutput(outputId = 'questionare1'),

                                     # actionButton(inputId="next", label="NEXT")
                                     actionButton(inputId = 'go', label = '卡片推薦', icon = icon('rocket'),
                                                  style="color: #fff; background-color: #009e8e; border-color: #009e8e"),
             textOutput(outputId = 'result'),
             uiOutput(outputId = 'questionare2')
             ),
      uiOutput(outputId = 'recPanel')

    ),
    ## tab2: dashboard ----
    tabItem(
      tabName = 'dashboard'
    )
    )
)


ui <-
  dashboardPage(header, sidebar, body)


