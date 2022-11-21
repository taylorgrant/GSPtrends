
library(shiny)

myApp <- function(...) {

  ## function to convert from one tab to another ##
  convertMenuItem <- function(mi,tabName) {
    mi$children[[1]]$attribs['data-toggle']="tab"
    mi$children[[1]]$attribs['data-value'] = tabName
    if(length(mi$attribs$class)>0 && mi$attribs$class=="treeview"){
      mi$attribs$class=NULL
    }
    mi
  }

  # header ------------------------------------------------------------------
  header <- shinydashboard::dashboardHeader(title = "Google Trends",
                                            shiny::tags$li(shiny::a(href = 'https://goodbysilverstein.com/',
                                                                    shiny::img(src = 'https://images.squarespace-cdn.com/content/v1/5ea9e40f1c49ae0355b4d859/1615824766494-NPIZRASFQS47W5GHS0UY/RepData_Goodby_Silverstein.png',
                                                                               title = "GS&P", height = "30px"),
                                                                    style = "padding-top:10px; padding-bottom:10px;"),
                                                           class = "dropdown"))

  # sidebar -----------------------------------------------------------------
  sidebar <- shinydashboard::dashboardSidebar(
    width = 325,
    shinydashboard::sidebarMenu(
      id = "tabs",
      convertMenuItem(
        shinydashboard::menuItem("Trend Query", tabName = "query",
                                 icon = shiny::icon("trowel"),
                                 selected=T,
                                 shiny::uiOutput("searchTerms"),
                                 shiny::uiOutput("dateRange"),
                                 shiny::uiOutput("customRange"),
                                 shiny::uiOutput("searchType"),
                                 shiny::uiOutput("filterCategory"),
                                 shiny::uiOutput("getTrends")

        ), "query"),
      convertMenuItem(
        shinydashboard::menuItem("Share of Search", tabName = "share",
                                 icon = shiny::icon("trowel"),
                                 selected=F,
                                 shiny::uiOutput("maSlider")
        ), "share")
    )
  )

  # body --------------------------------------------------------------------
  body <- shinydashboard::dashboardBody(
    ## CSS styling for the validation error message on Monthly Sales ##
    shiny::tags$head(
      shiny::tags$style(shiny::HTML("
    .shiny-output-error-validation {
      color: white;
      font-size: 100%;
    }
    #getTrends{
      width: 200px;
      position:relative;
      left:62.5px;
    }
  "))
    ),
  shinydashboard::tabItems(
    # conditionally render the output using inputs (server side)
    shinydashboard::tabItem("query", shiny::uiOutput("tab1"),
                            shinysky::busyIndicator(text = 'Please wait...', wait = 1500)),
    shinydashboard::tabItem("share", shiny::uiOutput("tab2"),
                            shinysky::busyIndicator(text = 'Please wait...', wait = 1500))
  )
  )

  # build the UI ------------------------------------------------------------
  ui = shinydashboard::dashboardPage(header, sidebar, body, skin = "black")



  # SERVER ------------------------------------------------------------------

  server = function(input, output, session) {

    # TAB 1 (Dynamic Render)
    output$searchTerms <- shiny::renderUI({})
    output$dateRange <- shiny::renderUI({})
    output$customRange <- shiny::renderUI({})
    output$searchType <- shiny::renderUI({})
    output$filterCategory <- shiny::renderUI({})
    output$getTrends <- shiny::renderUI({})
    shiny::outputOptions(output, "searchTerms", suspendWhenHidden = FALSE)
    shiny::outputOptions(output, "dateRange", suspendWhenHidden = FALSE)
    shiny::outputOptions(output, "customRange", suspendWhenHidden = FALSE)
    shiny::outputOptions(output, "searchType", suspendWhenHidden = FALSE)
    shiny::outputOptions(output, "filterCategory", suspendWhenHidden = FALSE)
    shiny::outputOptions(output, "getTrends", suspendWhenHidden = FALSE)
    # TAB 2 (Dynamic Render)
    output$maSlider <- shiny::renderUI({})
    shiny::outputOptions(output, "maSlider", suspendWhenHidden = FALSE)

    # Tab 1  ------------------------------------------------------------------
    output$searchTerms <- shiny::renderUI({
      shiny::textInput("terms", label = "Enter search term(s)")
      })
    output$dateRange <- shiny::renderUI({
      shiny::selectInput('range', label = "Date Range",
                         list("Last hour" = 'now 1-H',
                              "Last 4 hours" = "now 4-H",
                              "Past day" = "now 1-d",
                              "Past 7 days" = "now 7-d",
                              "Past 30 days" = "today 1-m",
                              "Past 90 days" = "today 3-m",
                              "Past 12 months" = "today 12-m",
                              "Past 5 years" = "today+5-y",
                              "2004 - Present" = "all",
                              "Custom range" = 'custom'),
                         selected = c("Past 12 months" = "today 12-m"))
    })
    shiny::observeEvent(input$range, {
      if (input$range == "custom") {
        output$customRange <- shiny::renderUI({
          shiny::dateRangeInput("customrange", label = "")
        })
      } else {
        return()
      }
    })
    date_range <- shiny::reactive({
      if (input$range == "custom") {
        glue::glue("{input$customrange[1]} {input$customrange[2]}")
      } else {
        input$range
      }
    })
    output$searchType <- shiny::renderUI({
      shiny::selectInput('type', label = "Search type",
                         list("Web search" = 'web',
                              "News search" = "news",
                              "Image search" = "images",
                              "Google shopping" = "froogle",
                              "YouTube search" =  "youtube"))
    })
    output$filterCategory <- shiny::renderUI({
      shiny::selectizeInput("category", label = "", choices = NULL)
    })
    shiny::updateSelectizeInput(session, 'category', server = TRUE, choices = c("", categories$name),
                         selected = "All categories")

    output$getTrends <- shiny::renderUI({
      shiny::actionButton('trends', "Get Trends!")
    })

    # rendering the query data as a check #
    output$tab1 <- shiny::renderUI({
      shinydashboard::tabItem("query",
                              shiny::h5("Building the query"),
                              value="test1",
                              shiny::fluidRow(
                                shinydashboard::box(
                                  width = 12,
                                  shiny::renderUI(
                                    shiny::HTML(glue::glue("<b>Search terms:</b> {input$terms}<br>
                                                           <b>Date range:</b> {date_range()}<br>
                                                           <b>Search Type:</b> {input$type}<br>
                                                           <b>Category:</b> {input$category}<br>"))
                                  )
                                )
                              ))
    })

    ## on button push, render the data for now
    rv <- shiny::reactiveValues(data = data.frame(), name = "data")
    shiny::observeEvent(input$trends, {
      category_id <- categories[categories$name == input$category,]$id
      # build the argument list to pass to function
      arg_list <- list(kw = unique(bare_combine(input$terms)),
                       duration = date_range(),
                       gprop = input$search,
                       category = category_id)

      # get some data
      # rv$data <<- pull_gtr(arg_list)
      rv$data <- readRDS("~/Desktop/trends.rds")

      output$table <- DT::renderDT(
        rv$data
      )

      output$tab1 <- shiny::renderUI({
        shinydashboard::tabItem("query",
                                shiny::h5("Building the query"),
                                value="test1",
                                shiny::fluidRow(
                                  shinydashboard::box(
                                    width = 12,
                                    DT::dataTableOutput("table")
                                  )
                                ))
      })
    })


  # TAB 2 -------------------------------------------------------------------
  # MA sliders
    output$maSlider <- renderUI({
      shinyWidgets::sliderTextInput(
        inputId = "slider",
        label = "Smoother",
        choices = c("0", "3", "5", "7", "9", "11", "13"),
        grid = TRUE
      )
    })

    data_out <- reactive({
      share_calc(rv$data, input$slider)
    })

    output$plot <- shiny::renderPlot(
      data_out()$plot
    )


    output$tab2 <- shiny::renderUI({
      shinydashboard::tabItem("query",
                              shiny::h4("Share of Search"),
                              value="test1",
                              shiny::fluidRow(
                                shinydashboard::box(
                                  width = 12,
                                  shiny::plotOutput("plot")
                                )
                              ))
    })




  }

  shiny::shinyApp(ui, server)

}
