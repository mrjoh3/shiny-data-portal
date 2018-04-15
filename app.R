#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(dbplyr)

con <- DBI::dbConnect(RPostgres::Postgres(), 
                      host = 'localhost',
                      port = 5432,
                      db = 'gisdata',
                      user = 'gisuser',
                      password = rstudioapi::askForPassword())

# add testing data to the database
if (!all(c('flights', 'iris') %in% DBI::dbListTables(con))) {
  copy_to(con, nycflights13::flights, 'flights', 
          temporary = FALSE,
          overwrite = TRUE,
          indexes = list(
            c("year", 'month', 'day'),
            'carrier',
            'tailnum',
            'dest'
          ))
  copy_to(con, iris, 'iris', 
          temporary = FALSE,
          overwrite = TRUE,
          indexes = list(
            'Species'
          ))
}

# Define UI for application that draws a histogram
ui <- shinyUI(
  navbarPage("Dynamic Data Import",
   
             tabPanel('Get Data',
                      
                      # Sidebar with a slider input for number of bins 
                       sidebarLayout(
                          sidebarPanel(
                             selectInput('table', label = 'Table', selected = NULL,
                                            choices = list('Flights' = 'flights',
                                                           'Iris' = 'iris')
                                            ),
                             selectInput('xcolumn', label = 'Filter Column', choices = NULL),
                             selectInput('value', label = 'Filter Value', choices = NULL, multiple = TRUE, selectize = TRUE),
                             shiny::dateRangeInput('date', label = 'Date Range'),
                             selectInput('groupby', label = 'Aggregate by Column', choices = NULL, multiple = TRUE, selectize = TRUE),
                             shiny::actionButton('submit', label = 'Get Data')
                          ),
                          
                          # Show a plot of the generated distribution
                          mainPanel(
                            conditionalPanel(
                               'input.submit > 0',
                               DT::dataTableOutput("tbl"),
                               tags$h3('Table retieved with SQL: '),
                                 tags$pre(
                                   textOutput('qry')
                                 )
                            )
                          )
                       )
                      ),
             tabPanel('Assess Data',
                      tags$p('This section under construction'),
                      fluidRow(
                        column(2),
                        column(8,
                          uiOutput('describe')
                        ),
                        column(2)
                      )
             )

))

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  input_table <- reactive({input$table})
  input_column <- reactive({input$xcolumn})
  input_vals <- reactive({input$value})
  
  # get list of columns to query
  observe({

    if (!is.null(input_table())) {
      
      cols <- DBI::dbListFields(con, input_table())
      updateSelectInput(session, 'xcolumn', choices = cols)
      updateSelectInput(session, 'groupby', choices = cols, selected = NULL)
    
    }
    
  }, priority = 1)
  
  # get list of values for selected column
  observeEvent(input$xcolumn, {

    req(selected_col <- input$xcolumn)
    
    if (!is.null(selected_col) | selected_col != '') {
      
      vals <- tbl(con, input_table()) %>%
        select_(selected_col) %>%
        distinct() %>%
        collect()

      updateSelectizeInput(session, 'value', choices = vals)

    }
    
  }, priority = 2)

  
  # after parameters set build query and get data
  observeEvent(input$submit, {
    
    print(paste(input_column(), 'col to filter'))
    print(paste(input_vals(), 'val to filter'))
    
    cl <- input_column()
    vl <- input_vals()
    
    query <- tbl(con, input_table()) %>%
      filter(!!rlang::sym(cl) %in% vl) 
    
    # check if there is a groupby value
    if (length(input$groupby) > 1) {

      query <- query %>%
        group_by_at(vars(one_of(input$groupby))) %>%
        summarise(count = n())
    
      }
    
    db_table <- query %>%
      collect()

    output$tbl <- DT::renderDataTable({db_table})
    output$qry <- renderText({dbplyr::sql_render(query)})
    output$describe <- renderUI({
      di <- Hmisc::describe(db_table)
      
      tagList(
        lapply(di, function(d) {
            tagList(
              fluidRow(
              tags$h3(d$descript),
              column(width = 3,
                DT::renderDataTable({data.frame(d$counts)})
                ),
              column(width = 9,
                #DT::renderDataTable({data.frame(d$values)})
                c3::renderC3({
                  data.frame(d$values) %>% 
                    mutate(value = as.character(value),
                           frequency = as.numeric(frequency)) %>%
                    c3::c3(x = 'value', y = 'frequency') %>% c3::c3_bar()
                })
              )
            )
          )
        })
      )
      
      })

  })
  

}

# Run the application 
shinyApp(ui = ui, server = server)

