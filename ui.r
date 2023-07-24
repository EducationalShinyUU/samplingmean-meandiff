# Copyright statement:
# This shiny application is developed by Lientje Maas to be used for educational purposes.
# Is is part of a program sponsored by the Education Incentive Funds of Utrecht University. 
# The layout for the shiny applications for this program is developed by Kimberley Lek. 
# The application is licensed under the GNU General Public License V3.0

# Author Comment:
# I have tried to code this according to the Google R Style Guide to improve readability:
# https://google.github.io/styleguide/Rguide.xml
# For any questions or comments you can contact me at j.a.m.maas@uu.nl.

# File description:
# This file contains the user interface (UI) for the application related to sampling distributions.

# Loading libraries 
library(shiny)
library(shinydashboard)

# User interface design
ui <- dashboardPage(skin = "black",
dashboardHeader(title = "Sampling distributions",titleWidth = 240), 
dashboardSidebar(width = 240,
                 sidebarMenu(#menuItem("", tabName = "home", icon = icon("home")),
                             menuItem("Distribution of the mean", tabName = "tab1"),
                             menuItem(HTML("Distribution of the difference<br/>between two means"), tabName = "tab2"),
                             menuItem("Disclaimer", tabName = "Disclaimer"), 
                             HTML("<br><br><br><br><br><br><br><br><br><br><br><br><br><br>"), 
                             img(src= "logo.png", align = "left"),
                             HTML("<br><br><br><br><br><br><br>"), 
                             # div("Shiny app by",
                             #     a(href="https://www.uu.nl/staff/ktnamesniksilvester",
                             #       target = "_blank",
                             #       "Kirsten Namesnik"),
                             #     "and",
                             #     a(href="https://www.uu.nl/staff/jammaas",target="_blank",
                             #       "Lientje Maas"),
                             #     align="right", style = "font-size: 10pt"),
                             
                             div("Base R code by",
                                 a(href="https://www.uu.nl/staff/jammaas",target="_blank",
                                   "Lientje Maas"),align="right", style = "font-size: 10pt"),
                             
                             div("Base Layout by",
                                 a(href="https://www.uu.nl/medewerkers/KMLek/0",target="_blank",
                                   "Kimberley Lek"),align="right", style = "font-size: 10pt"),
                             
                             div("Shiny source files:",
                                 a(href="https://github.com/EducationalShinyUU/samplingmean-meandiff",
                                   target="_blank","GitHub"),align="right", style = "font-size: 10pt")
                             ) #end sidebarMenu
                 ), #end dashboardsidebar
dashboardBody(
# CSS styles lay-out settings from Kimberley
  tags$style(HTML(".irs-bar {background: #EAC626}")),
  tags$style(HTML(".irs-bar {border-top: 1px solid black}")),
  tags$style(HTML(".irs-bar-edge {background: #EAC626}")),
  tags$style(HTML(".irs-bar-edge {border: 1px solid black}")),
  tags$style(HTML(".irs-single {background: #EAC626}")),
  tags$style(HTML(".selectize-input {border-color: #EAC626}")),
  tags$style(HTML(".selectize-dropdown {border-color: #EAC626}")),
  tags$head(tags$style(HTML('.skin-black .main-header .logo {
                             background-color: #EAC626;
                             } .skin-black .main-header .logo:hover {
                             background-color: #EAC626;
                             }
                             /* active selected tab in the sidebarmenu */
                             .skin-black .main-sidebar .sidebar .sidebar-menu .active a{
                             background-color: #EAC626;
                             }
                             /* navbar (rest of the header) */
                             .skin-black .main-header .navbar {
                             background-color: #EAC626;
                             }
                             /* toggle button when hovered  */                    
                             .skin-black .main-header .navbar .sidebar-toggle:hover{
                             background-color: #EAC626;
                             }
                             /* other links in the sidebarmenu when hovered */
                             .skin-black .main-sidebar .sidebar .sidebar-menu a:hover{
                             background-color: #EAC626;
                             }
                             /* other links in the sidebarmenu */
                             .skin-black .main-sidebar .sidebar .sidebar-menu a{
                             background-color: #EAC626;
                             color: #000000;
                             }
                             /* active selected tab in the sidebarmenu */
                             .skin-black .main-sidebar .sidebar .sidebar-menu .active a{
                             background-color: #000000;
                             color: #FFFFFF;
                             }
                             .skin-black .main-sidebar {color: #000000; background-color: #EAC626;}
                             ') # end html
                       ) # end tags$style
            ), # end tags$head
  tabItems(
    # now for the content of the different tabs.
    # Disclaimer tab, university disclaimer.
    tabItem(tabName = "Disclaimer", 
            box(width = 12, 
                h5("Terms of Usage Utrecht Unversity Shiny Server", 
                   br(), 
                   br(),
                   tags$ul(
                     tags$li("Purpose of the service “utrecht-university.shinyapps.io” 
                             is to provide a digital place for trying out, evaluating 
                             and/or comparing methods developed by researchers of Utrecht University 
                             for the scientific community worldwide. 
                             The app and its contents may not be preserved in such a way that it can 
                             be cited or can be referenced to. "), 
                     tags$li("The web application is provided ‘as is’ and ‘as available’ and is without any warranty. 
                             Your use of this web application is solely at your own risk."), 
                     tags$li("	You must ensure that you are lawfully entitled and have full authority to upload 
                             data in the web application. The file data must not contain any data which can raise 
                             issues relating to abuse, confidentiality, privacy,  data protection, licensing, and/or 
                             intellectual property. You shall not upload data with any confidential or proprietary information 
                             that you desire or are required to keep secret. "),
                     tags$li("By using this app you agree to be bound by the above terms.")
                     ) # end tags$ul
                     )
                   ) # end box
                   ), # end tab Item (disclaimer tab.)
    
    # Sampling distribution of the mean tab, university disclaimer.
    tabItem(tabName = "tab1", 
            fluidRow( 
              box ( width = 14, align = "left", # box for population specification
                    column(width = 2,align = "left",
                           selectInput("pop.distr", "Population distribution", c("Beta", "Uniform", "Normal"), selected = "Normal"),
                           br(),
                           h5("Generating parameters:"),
                           uiOutput("ui"),
                           br(),
                           actionButton("genButton", HTML("Generate<br/>population"))
                           ),
                    column(width = 10, align = "left",
                          tableOutput("poptable"),
                          plotOutput("popplot", width = 500, height = 375)
                          )
              ), # end box
              box( width = 14, align = "left", # box sampling
                   column(width = 2,align = "left",
                          selectInput("sample.size", "Sample size",c(10, 30, 100), 10),
                          br(),
                          actionButton("samplebutton", "Sample"), # action button that allows the previous point to be deleted 
                          br(),
                          br(),
                          actionButton("sample10button", HTML("Show next<br/>10 samples")), # add ten new samples
                          br(),
                          br(),
                          actionButton("sample1000button", HTML("Show next<br/>1000 samples")), # add thousand new samples
                          br(),
                          br(),
                          actionButton("reset", "Reset") # action button to restart with clean plot
                          ),
                   column( width = 10, align = "left",
                           tableOutput('samplingtable'),
                           plotOutput("meanhist", width = 500, height = 375) # histogram of sampling distribution
                           ) 
              ) # end box
            ) # end fluidrow
    ), # end tabItem
    tabItem(tabName = "tab2",
            fluidRow(
              box( width = 14, align = "left", # box for population specification
                   column(width = 2,align = "left",
                          selectInput("pop.distr2", "Population distribution", c("Beta", "Uniform", "Normal"), selected = "Normal"),
                          br(),
                          h5("Generating parameters:"),
                          uiOutput("ui2_1"),
                          br(),
                          uiOutput("ui2_2"),
                          br(),
                          actionButton("genButton2", HTML("Generate<br/>populations"))
                   ),
                   column( width = 10, align = "left",
                           tableOutput("poptable2"),
                           plotOutput("popplot2",width = 500, height = 500) )
              ), # end box
              box ( width = 12, align = "left", # box for sampling
                    column(width = 2,align = "left",
                           selectInput("sample.size2", "Sample size",c(10, 30, 100), 10),
                           br(),
                           actionButton("samplebutton2", "Sample"), # action button that allows the previous point to be deleted 
                           br(),
                           br(),
                           actionButton("sample10button2", HTML("Show next<br/>10 samples")), # add ten new samples
                           br(),
                           br(),
                           actionButton("sample1000button2", HTML("Show next<br/>1000 samples")), # add thousand new samples
                           br(),
                           br(),
                           actionButton("reset2", "Reset") # action button to restart with clean plot
                    ),
                    column( width = 10, align = "left",
                            tableOutput('samplingtable2'),
                            plotOutput("meanhist2",width = 500, height = 375)  ) # histogram of sampling distribution
              ) # end box
            ) # end fluidrow
    ) # end tabItem
  ) # end tabItems
) # end dashboardbody
) # end dashboardpage

