library(shiny)
data(categories, package = "GSPtrends")
data(states, package = "GSPtrends")

## function to convert from one tab to another ##
convertMenuItem <- function(mi,tabName) {
  mi$children[[1]]$attribs['data-toggle']="tab"
  mi$children[[1]]$attribs['data-value'] = tabName
  if(length(mi$attribs$class)>0 && mi$attribs$class=="treeview"){
    mi$attribs$class=NULL
  }
  mi
}


# palettes ----------------------------------------------------------------
line_pal <- c('#e6194B', '#3cb44b',  '#4363d8', '#f58231', '#42d4f4',
              '#469990', '#dcbeff', '#9A6324', '#800000', '#aaffc3', '#ffe119',
              '#000075', '#a9a9a9')


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
                               shiny::uiOutput('geoTarget'),
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
                               shiny::uiOutput("palChoice"),
                               shiny::uiOutput("maSlider1")
      ), "share"),
    convertMenuItem(
      shinydashboard::menuItem("Single Keywords", tabName = "solo",
                               icon = shiny::icon("trowel"),
                               selected=F,
                               shiny::uiOutput("soloKeyword"),
                               shiny::uiOutput("maSlider2")

      ), "solo"),
    convertMenuItem(
      shinydashboard::menuItem("About", tabName = "about",
                               icon = shiny::icon("trowel"),
                               selected=F
      ), "about")

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
                          shinysky::busyIndicator(text = 'Please wait...', wait = 1500)),
  shinydashboard::tabItem("solo", shiny::uiOutput("tab3"),
                          shinysky::busyIndicator(text = 'Please wait...', wait = 1500)),
  shinydashboard::tabItem("about", shiny::uiOutput("tab4"),
                          shinysky::busyIndicator(text = 'Please wait...', wait = 1500))
)
)

# build the UI ------------------------------------------------------------
ui = shinydashboard::dashboardPage(header, sidebar, body, skin = "black")



# SERVER ------------------------------------------------------------------

server = function(input, output, session) {

  # TAB 1 (Dynamic Render)
  output$searchTerms <- shiny::renderUI({})
  output$geoTarget <- shiny::renderUI({})
  output$dateRange <- shiny::renderUI({})
  output$customRange <- shiny::renderUI({})
  output$searchType <- shiny::renderUI({})
  output$filterCategory <- shiny::renderUI({})
  output$getTrends <- shiny::renderUI({})
  shiny::outputOptions(output, "searchTerms", suspendWhenHidden = FALSE)
  shiny::outputOptions(output, "geoTarget", suspendWhenHidden = FALSE)
  shiny::outputOptions(output, "dateRange", suspendWhenHidden = FALSE)
  shiny::outputOptions(output, "customRange", suspendWhenHidden = FALSE)
  shiny::outputOptions(output, "searchType", suspendWhenHidden = FALSE)
  shiny::outputOptions(output, "filterCategory", suspendWhenHidden = FALSE)
  shiny::outputOptions(output, "getTrends", suspendWhenHidden = FALSE)
  # TAB 2 (Dynamic Render)
  output$maSlider1 <- shiny::renderUI({})
  output$palChoice <- shiny::renderUI({})
  shiny::outputOptions(output, "maSlider1", suspendWhenHidden = FALSE)
  shiny::outputOptions(output, "palChoice", suspendWhenHidden = FALSE)
  # TAB 3 (Dynamic Render)


  # Modal Window ------------------------------------------------------------
  shiny::showModal(shiny::modalDialog(shiny::HTML("<b>What is this?</b> This is an interface to query Google Trends. This will take your search terms, retrieve
                                                  the data, and help to visualize the results. All tables and graphs are downloadable and everything is interactive.
                                                  <br><br><b>How do I use it?</b> Use the drop downs in the left rail under the tab, 'Trend Query'. Enter your terms
                                                  in the text box - <b>separate each term with a comma</b>, select the geography of interest (within the US only for the time being),
                                                  and specify your time frame, search type, and any specific category. Remember that different time frames return different
                                                  time intervals between data points. See
                                                  <a href='https://support.google.com/trends/?hl=en#topic=6248052' target='_blank'>here</a> for
                                                  more information about Trends queries.<br><br>For more information about the results, see the <code>About</code> tab."),
                                      easyClose = T))

  # Tab 1  ------------------------------------------------------------------

  # setting up the sidebar
  output$searchTerms <- shiny::renderUI({
    shiny::textInput("terms", label = "Enter search term(s)")
    })
  output$geoTarget <- shiny::renderUI({
    shiny::selectInput('geotarget', label = "Narrow to state?",
                       choices = states$name)
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
                                                         <b>Geography:</b> {input$geotarget}<br>
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
    state_id <- states[states$name == input$geotarget,]$id

    # build the argument list to pass to function
    arg_list <- list(kw = unique(bare_combine(input$terms)),
                     geo = as.character(state_id),
                     duration = as.character(date_range()),
                     gprop = input$type,
                     category = category_id,
                     category_name = input$category)

    # get data from Google Trends
    rv$data <- get_trends(arg_list)
    # gt <<- rv$data

    # raw table output (wide format)
    output$raw_table <- DT::renderDT(
      query_summary(rv$data)$table |>
        DT::datatable(
          extensions = "Buttons",
          filter = list(position = "top", clear = FALSE),
          rownames = FALSE,
          options = list(
            # searching = FALSE,
            dom = "Blftp",
            buttons = c('excel')),
          caption = glue::glue("Timeframe: {arg_list$duration}; Category: {arg_list$category_name}")
        ),
      server = FALSE
    )

    # raw line plot
    output$raw_plot <- plotly::renderPlotly(
      query_summary(rv$data)$plot
    )

    # summary table with CI
    output$summary_table <- DT::renderDT(
      query_summary(rv$data)$table2 |>
        DT::datatable(
          colnames = c("Term", "n", "Minimum", "Average", "Maximum",
                       "S.D.", "95% C.I."),
          extensions = "Buttons",
          filter = list(position = "top", clear = FALSE),
          rownames = FALSE,
          options = list(
            # searching = FALSE,
            dom = "Blftp",
            buttons = c('excel'),
            columnDefs = list(list(className = "dt-right", targets = c("ci")))),
          caption = glue::glue("Summary data: Timeframe - {arg_list$duration}; Category - {input$category}")
        ),
      server = FALSE
    )

    output$tab1 <- shiny::renderUI({
      shinydashboard::tabItem("query",
                              shiny::h4("Query Results"),
                              value="test1",
                              shiny::fluidRow(
                                shinydashboard::tabBox(
                                  width = 10,
                                  shiny::tabPanel("Results plot", plotly::plotlyOutput("raw_plot", height = "500px")),
                                  shiny::tabPanel("Results table", DT::dataTableOutput("raw_table")),
                                  shiny::tabPanel("Summary stats", DT::dataTableOutput("summary_table"))
                                )
                              )
                              )
    })
  })


  # TAB 2 -------------------------------------------------------------------

  # MA sliders
  output$maSlider1 <- shiny::renderUI({
    shinyWidgets::sliderTextInput(
      inputId = "slider1",
      label = "Smoother",
      choices = c("0","2","3","4","5","6","7","8","9","10","11","12","13"),
      grid = TRUE
    )
  })
  output$palChoice <- shiny::renderUI({
    shiny::selectInput("palette_choice", label = "Select palette",
                       list("Solar" = "solr",
                            "Blues" = "blue",
                            "Reds" = "red",
                            "Greens" = "green",
                            "Purples"= "purple",
                            "Pinks" = "pink",
                            "Blue to Yellow" = "bty"
                            ))
  })

  share_data <- shiny::reactive({
    calculate_share(rv$data, input$slider1, input$palette_choice)
  })

  output$maRawTable <- DT::renderDT(
    share_data()$ma_raw_table,
    server = FALSE
  )
  output$maShareTable <- DT::renderDT(
    share_data()$ma_share_table,
    server = FALSE
  )
  output$zMATable <- DT::renderDT(
    share_data()$zma_raw_table,
    server = FALSE
  )
  output$shareLine <- plotly::renderPlotly(
    share_data()$share_line
  )
  output$shareArea <- plotly::renderPlotly(
    share_data()$share_area
  )
  output$zscoreArea <- plotly::renderPlotly(
    share_data()$zscore_area
  )


  output$tab2 <- shiny::renderUI({
    shiny::validate(
      shiny::need(nrow(rv$data) > 0, "Need data")
    )
    shinydashboard::tabItem("share",
                            shiny::h4("Data Output & Graphs"),
                            value="test2",
                            shiny::fluidRow(
                              shinydashboard::tabBox(
                                width = 11,
                                shiny::tabPanel("Share of search (line)", plotly::plotlyOutput("shareLine", height = "500px")),
                                shiny::tabPanel("Share of search (area)", plotly::plotlyOutput("shareArea", height = "500px")),
                                shiny::tabPanel("Change in search (area)", plotly::plotlyOutput("zscoreArea", height = "500px")),
                                shiny::tabPanel("Table - MA Raw", DT::dataTableOutput("maRawTable")),
                                shiny::tabPanel("Table - MA Share", DT::dataTableOutput("maShareTable")),
                                shiny::tabPanel("Table - MA Z-Score", DT::dataTableOutput("zMATable"))
                              )
                            ))
  })

# TAB 3 - MORE STATISTICS -------------------------------------------------

  keyword_terms <- shiny::reactive({
    stringr::str_to_title(unique(rv$data$keyword))
  })
  output$soloKeyword <- shiny::renderUI({
    shiny::selectInput('solo_keyword', label = "Term",
                       choices = keyword_terms()
    )
  })
  output$maSlider2 <- shiny::renderUI({
    shinyWidgets::sliderTextInput(
      inputId = "slider2",
      label = "Smoother",
      choices = c("0","2","3","4","5","6","7","8","9","10","11","12","13"),
      grid = TRUE
    )
  })

  output$singleSmooth <- plotly::renderPlotly(
    single_keyword_smooth(rv$data, input$solo_keyword, input$slider2)
  )

  # get range of data
  data_range <- shiny::reactive({
    range(rv$data$date)
  })

  # TS decomposition plots
  output$tsPlot1 <- plotly::renderPlotly(
    ts_decomposition(rv$data, input$solo_keyword)$tsp1
  )
  output$tsPlot2 <- plotly::renderPlotly(
    ts_decomposition(rv$data, input$solo_keyword)$tsp2
  )
  output$tsPlot3 <- plotly::renderPlotly(
    ts_decomposition(rv$data, input$solo_keyword)$tsp3
  )

  output$tab3 <- shiny::renderUI({
    shiny::validate(
      shiny::need(nrow(rv$data) > 0, "Need data")
    )
    shinydashboard::tabItem("solo",
                            shiny::h4("Additional information"),
                            value="test3",
                            shiny::fluidRow(
                              shinydashboard::tabBox(
                                width = 11,
                                shiny::tabPanel("Single Keyword (MA)", plotly::plotlyOutput("singleSmooth", height = "500px")),
                                shiny::tabPanel("STL Decomp", plotly::plotlyOutput("tsPlot1", height = "550px")),
                                shiny::tabPanel("Seasonally Adjusted", plotly::plotlyOutput("tsPlot2", height = "550px")),
                                shiny::tabPanel("Seasonal vs. Trend", plotly::plotlyOutput("tsPlot3", height = "550px"))
                              )
                            ))
  })


  # TAB 4 - ABOUT ----------------------------------------------------

  output$aboutInfo <- renderUI({
      HTML("<b>Share of Search:</b><br>Les Binet has argued that Share of Search is predictive of a brand's market share
           within its category. He has further argued that Google Trends data can be used as an accurate measure of consumer search
           interest (see <a href='https://ipa.co.uk/effworks/effworksglobal-2020/share-of-search-as-a-predictive-measure' target='_blank'>here</a>
           for more information).<br><br>Share of search is simply calculated as the percentage of the total search index
           (e.g., Term A / (Term A + Term B + Term C)) for any point of time. While Google Trends only returns results for up to 5 terms,
           and those indices returned are relative to the peak performer (whichever term earns the 100), we are able to increase the number of
           allowable terms by ensuring that each search includes the peak performer. In so doing, the index results are consistent for each
           term, regardless of what search group it was included in. This app is set up to allow up to 13 search terms.
           <br><br>
            <b>Change in Search:</b><br>The change in search is the simple z-score for each term's search interest.
           Each term's index is first centered - the mean search interest is subtracted from the index value - and then
           scaled - the centered value is divided by the standard deviation of of the term's search interest.<br><br>This
           metric provides an easy way of understanding how consumer interest is changing over time. Values above (below) 0 indicate that
           search interest is above (below) average.<br><br>
           <b>Smoother:</b> Within the <code>Share of Search</code> tab, a moving average can be applied to the data. A
           moving average is the estimate of the trend-cycle at time <em>t</em> and is obtained by averaging values of the time series within
           <em>k</em> periods of <em>t</em>. Observations that are nearby in time are also likely to be close in value.
           Therefore, the average eliminates some of the randomness in the data, leaving a smooth trend-cycle component. The
           order of the moving average determines the smoothness of the trend-cycle estimate; a larger order means a smoother
           curve.<br><br>
           <b>Time Series Decomposition:</b> Within the <code>Single Keywords</code> tab are three tabs
           (STL, Seasonally Adjusted, and Seasonal vs. Trend). STL stands for 'Seasonal and Trend decomposition using Loess.'
           This is a statistical method of decomposing a time series data into 3 components containing seasonality,
           trend and residual. Trend gives you a general direction of the overall data. Seasonality is a regular and predictable pattern
           that recurs at a fixed interval of time. For example, searches for gym memberships routinely peak in January. Or
           in a tighter time window, electricity consumption peaks at night. The residual in the STL plot is the random fluctuation
           or unpredictable change in the series - once we take out the trend and seasonality, what is left to drive search interest.")
  })

  output$tab4 <- shiny::renderUI({
    shinydashboard::tabItem("about",
                            shiny::h4("ABout the results and graphs"),
                            value="test4",
                            shiny::fluidRow(
                              shinydashboard::tabBox(
                                width = 11,
                                shiny::tabPanel("", shiny::uiOutput("aboutInfo"))
                              )
                            ))
  })


}

shiny::shinyApp(ui, server)


