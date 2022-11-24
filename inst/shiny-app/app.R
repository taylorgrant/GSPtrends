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

# pacman::p_load(showtext)
# font_add_google("Open Sans", "Open Sans")
# showtext_auto()

## temp adding in theme
theme_xf <- function(base_family="Open Sans", base_size = 11.5,
                     plot_title_family=base_family, plot_title_size = 16,
                     plot_title_face="plain", plot_title_margin = 4,
                     subtitle_family="Open Sans", subtitle_size = 12,
                     subtitle_face = "plain", subtitle_margin = 15,
                     strip_text_family = base_family, strip_text_size = 12,
                     strip_text_face = "plain",
                     caption_family = "Open Sans", caption_size = 9,
                     caption_face = "italic", caption_margin = 10,
                     axis_title_family = base_family, axis_title_size = 13,
                     axis_title_face = "plain", axis_title_just = "rt",
                     plot_margin = ggplot2::margin(10,10,10,10),
                     panel_spacing = ggplot2::unit(0.5, "lines"),
                     grid_col = "#cccccc", grid = TRUE,
                     axis_col = "#cccccc",axis = FALSE,
                     ticks = FALSE) {

  ret <- ggplot2::theme_minimal(base_family=base_family, base_size=base_size)

  ret <- ret + ggplot2::theme(legend.background=ggplot2::element_blank())
  ret <- ret + ggplot2::theme(legend.key=ggplot2::element_blank())

  if (inherits(grid, "character") | grid == TRUE) {

    ret <- ret + ggplot2::theme(panel.grid=ggplot2::element_line(color=grid_col, linewidth=0.10))
    ret <- ret + ggplot2::theme(panel.grid.major=ggplot2::element_line(color=grid_col, linewidth=0.1))
    ret <- ret + ggplot2::theme(panel.grid.minor=ggplot2::element_line(color=grid_col, linewidth=0.1))

    if (inherits(grid, "character")) {
      if (regexpr("X", grid)[1] < 0) ret <- ret + ggplot2::theme(panel.grid.major.x=ggplot2::element_blank())
      if (regexpr("Y", grid)[1] < 0) ret <- ret + ggplot2::theme(panel.grid.major.y=ggplot2::element_blank())
      if (regexpr("x", grid)[1] < 0) ret <- ret + ggplot2::theme(panel.grid.minor.x=ggplot2::element_blank())
      if (regexpr("y", grid)[1] < 0) ret <- ret + ggplot2::theme(panel.grid.minor.y=ggplot2::element_blank())
    }

  } else {
    ret <- ret + ggplot2::theme(panel.grid=ggplot2::element_blank())
  }

  if (inherits(axis, "character") | axis == TRUE) {
    ret <- ret + ggplot2::theme(axis.line=ggplot2::element_line(color="#2b2b2b", linewidth=0.15))
    if (inherits(axis, "character")) {
      axis <- tolower(axis)
      if (regexpr("x", axis)[1] < 0) {
        ret <- ret + ggplot2::theme(axis.line.x=ggplot2::element_blank())
      } else {
        ret <- ret + ggplot2::theme(axis.line.x=ggplot2::element_line(color=axis_col, linewidth=0.15))
      }
      if (regexpr("y", axis)[1] < 0) {
        ret <- ret + ggplot2::theme(axis.line.y=ggplot2::element_blank())
      } else {
        ret <- ret + ggplot2::theme(axis.line.y=ggplot2::element_line(color=axis_col, linewidth=0.15))
      }
    } else {
      ret <- ret + ggplot2::theme(axis.line.x=ggplot2::element_line(color=axis_col, linewidth=0.15))
      ret <- ret + ggplot2::theme(axis.line.y=ggplot2::element_line(color=axis_col, linewidth=0.15))
    }
  } else {
    ret <- ret + ggplot2::theme(axis.line=ggplot2::element_blank())
  }

  if (!ticks) {
    ret <- ret + ggplot2::theme(axis.ticks = ggplot2::element_blank())
    ret <- ret + ggplot2::theme(axis.ticks.x = ggplot2::element_blank())
    ret <- ret + ggplot2::theme(axis.ticks.y = ggplot2::element_blank())
  } else {
    ret <- ret + ggplot2::theme(axis.ticks = ggplot2::element_line(linewidth=0.15))
    ret <- ret + ggplot2::theme(axis.ticks.x = ggplot2::element_line(linewidth=0.15))
    ret <- ret + ggplot2::theme(axis.ticks.y = ggplot2::element_line(linewidth=0.15))
    ret <- ret + ggplot2::theme(axis.ticks.length = grid::unit(5, "pt"))
  }

  xj <- switch(tolower(substr(axis_title_just, 1, 1)), b=0, l=0, m=0.5, c=0.5, r=1, t=1)
  yj <- switch(tolower(substr(axis_title_just, 2, 2)), b=0, l=0, m=0.5, c=0.5, r=1, t=1)

  ret <- ret + ggplot2::theme(axis.text.x=ggplot2::element_text(margin=ggplot2::margin(t=0)))
  ret <- ret + ggplot2::theme(axis.text.y=ggplot2::element_text(margin=ggplot2::margin(r=0)))
  ret <- ret + ggplot2::theme(axis.title=ggplot2::element_text(size=axis_title_size, family=axis_title_family))
  ret <- ret + ggplot2::theme(axis.title.x=ggplot2::element_text(hjust=xj, size=axis_title_size,
                                                                 family=axis_title_family, face=axis_title_face))
  ret <- ret + ggplot2::theme(axis.title.y=ggplot2::element_text(hjust=yj, size=axis_title_size,
                                                                 family=axis_title_family, face=axis_title_face))
  ret <- ret + ggplot2::theme(strip.text=ggplot2::element_text(hjust=0, size=strip_text_size,
                                                               face=strip_text_face, family=strip_text_family))
  ret <- ret + ggplot2::theme(panel.spacing.x=grid::unit(.5, "lines"))
  ret <- ret + ggplot2::theme(panel.spacing.y=grid::unit(.5, "lines"))
  ret <- ret + ggplot2::theme(plot.title=ggplot2::element_text(hjust=0, size=plot_title_size,
                                                               margin=ggplot2::margin(b=plot_title_margin),
                                                               family=plot_title_family, face=plot_title_face))
  ret <- ret + ggplot2::theme(plot.subtitle=ggplot2::element_text(hjust=0, size=subtitle_size,
                                                                  margin=ggplot2::margin(b=subtitle_margin),
                                                                  family=subtitle_family, face=subtitle_face))
  ret <- ret + ggplot2::theme(plot.caption=ggplot2::element_text(hjust=1, size=caption_size,
                                                                 margin=ggplot2::margin(t=caption_margin),
                                                                 family=caption_family, face=caption_face))
  ret <- ret + ggplot2::theme(plot.margin=plot_margin)

  ret <-  ret + ggplot2::theme(panel.spacing=panel_spacing)

  ret

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

      ), "solo")

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
    gt <<- rv$data
    # rv$data <- readRDS("~/Desktop/last5year.rds") |>
    #   dplyr::mutate(date = as.Date(date))

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
        )
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
        )
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
    share_data()$ma_raw_table
  )
  output$maShareTable <- DT::renderDT(
    share_data()$ma_share_table
  )
  output$zMATable <- DT::renderDT(
    share_data()$zma_raw_table
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

  output$tsPlot1 <- plotly::renderPlotly(
    ts_decomposition(rv$data, input$solo_keyword)$tsp1
  )
  output$tsPlot2 <- plotly::renderPlotly(
    ts_decomposition(rv$data, input$solo_keyword)$tsp2
  )
  output$tsPlot3 <- plotly::renderPlotly(
    ts_decomposition(rv$data, input$solo_keyword)$tsp3
  )

  # # correlations
  # output$correlation_table <- DT::renderDT(
  #   trend_correlation(rv$data)$corr_tbl
  # )


  output$tab3 <- shiny::renderUI({
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

}

shiny::shinyApp(ui, server)


