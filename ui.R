library(shiny)
library(shinydashboard)

library(shinycssloaders)

source("preprocessing.R")

shinyUI(
  dashboardPage(
    dashboardHeader(title = "Clusters in Bibmet")
    ,dashboardSidebar(
      width = 300
      ,DT::dataTableOutput("ilabels")
      
      ,selectInput('starty', 'Start year', 1980:as.integer(format(Sys.Date(), "%Y")))
      ,selectInput('clusterlevels', 'Select cluster level', clusterlevels, selected=clusterlevels[2])
      #,actionButton("update" ,"Update View", icon("refresh" ,class = "btn btn-primary"))
      ,sidebarMenu(
        menuItem("Publications", tabName = "year")
        # ,menuItem("Publications per country", tabName = "publications_per_country")
        # ,menuItem("Publications per author", tabName = "publications_per_author")
        # ,menuItem("Publications per organization", tabName = "publications_per_organization_country")
        # ,menuItem("Publications per city", tabName = "publications_per_city")
        # ,menuItem("Addresses", tabName = "addresses")
        ,menuItem("About", tabName = "about")
      )
    ),
    dashboardBody(
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
        ,tags$style(HTML("
        
                    "))
      )
      ,fluidRow(
      tabItems( 
        tabItem( tabName = "year",
            tabBox( width = 800
                    ,height = 1200
             ,tabPanel("Tab1"
                       ,fluidRow(
                         
                       )
                      ,box(width = 12,
                           title = "Publication count per year", status = "success", solidHeader = TRUE, collapsible = TRUE 
                           ,actionButton("update" ,"Update View", icon("refresh") ,class = "btn btn-primary")
                           ,textOutput("current_label")
                           ,withSpinner(
                             plotOutput('plot_time')
                             #,type = getOption("spinner.type", default = 3)
                             #,color.background = getOption("spinner.color.background", "#ffffff")
                           )
                      )
                    )
             ,tabPanel("Publications per country"
             ,box(
               width = 12
               ,title = "Publications per country", status = "primary", solidHeader = TRUE, collapsible = TRUE
               ,withSpinner(plotOutput("plot_countrycount", height=300))
             )
               ,box(
                 width = 12
                 ,title = "Publications per country", status = "primary", solidHeader = TRUE, collapsible = TRUE
                 ,withSpinner(DT::dataTableOutput("publ_country"))
               )
              )
              ,tabPanel("Publications per author"
                ,box(
                  width = 12
                  ,title = "Publications per author", status = "primary", solidHeader = TRUE, collapsible = TRUE
                  ,withSpinner(plotOutput("plot_authorcount", height=300))
                )
                ,box(
                  width = 12
                  ,title = "Publications per author", status = "primary", solidHeader = TRUE, collapsible = TRUE
                  ,withSpinner(DT::dataTableOutput("publ_author"))
                )
              )
             ,tabPanel("Publications per organization and country"
               ,box(
                 width = 12
                 ,title = "Publications per organization and country", status = "primary", solidHeader = TRUE, collapsible = TRUE
                 ,withSpinner(DT::dataTableOutput("publ_org"))
               )
             )
             ,tabPanel("Publications per city"
                       ,box(
                         width = 12
                         ,title = "Publications per city", status = "primary", solidHeader = TRUE, collapsible = TRUE
                         ,withSpinner(DT::dataTableOutput("publ_cities"))
                       )
             )
             ,tabPanel("Adresses"
                       ,box(
                         width = 12
                         ,title = "Addresses", status = "primary", solidHeader = TRUE, collapsible = TRUE
                         ,withSpinner(DT::dataTableOutput("addresstable"))
                       )
             )
            )
        )
        #,tabItem( tabName = "publications_per_country"
          # ,box(
          #   width = 12
          #   ,title = "Publications per country", status = "primary", solidHeader = TRUE, collapsible = TRUE
          #   ,withSpinner(plotOutput("plot_countrycount", height=300))
          # )
          # ,box(
          #   width = 12
          #   ,title = "Publications per country", status = "primary", solidHeader = TRUE, collapsible = TRUE
          #   ,withSpinner(DT::dataTableOutput("publ_country"))
          # )
        #)
        #,tabItem( tabName = "publications_per_author"
          # ,box(
          #   width = 3
          #   ,title = "Publications per author", status = "primary", solidHeader = TRUE, collapsible = TRUE
          #   ,withSpinner(plotOutput("plot_authorcount", height=200))
          # )
          # ,box(
          #   width = 8
          #   ,title = "Publications per author", status = "primary", solidHeader = TRUE, collapsible = TRUE
          #   ,withSpinner(DT::dataTableOutput("publ_author"))
          # )
        #)
        #,tabItem( tabName = "publications_per_organization_country"
          # ,box(
          #   width = 12
          #   ,title = "Publications per organization and country", status = "primary", solidHeader = TRUE, collapsible = TRUE
          #   ,withSpinner(DT::dataTableOutput("publ_org"))
          # )
        #)
        #,tabItem( tabName = "publications_per_city"
          # ,box(
          #   width = 12
          #   ,title = "Publications per city", status = "primary", solidHeader = TRUE, collapsible = TRUE
          #   ,withSpinner(DT::dataTableOutput("publ_cities"))
          # )
        #)
        #,tabItem( tabName = "addresses"
          # ,box(
          #   width = 12
          #   ,title = "Addresses", status = "primary", solidHeader = TRUE, collapsible = TRUE
          #   ,withSpinner(DT::dataTableOutput("addresstable"))
          # )
        #)
        ,tabItem( tabName = "about"
          ,h1("About")
          ,verbatimTextOutput("userinfo")
          ,verbatimTextOutput("username")
          ,verbatimTextOutput("groups")
          ,verbatimTextOutput("token")
          #,DT::dataTableOutput("pre_authors")
        )
      )
    )
    )
  )
)

