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
ui <- fluidPage(
   
   # Application title
   titlePanel("Dynamic Data Import"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         selectInput('table', label = 'Table', selected = NULL,
                        choices = list('Flights' = 'flights',
                                       'Iris' = 'iris')
                        ),
         selectInput('xcolumn', label = 'Column', choices = NULL),
         selectInput('value', label = 'Filter Value', choices = NULL),
         shiny::dateRangeInput('date', label = 'Date Range'),
         shiny::actionButton('submit', label = 'Get Data')
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         DT::dataTableOutput("tbl")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  input_table <- reactive({input$table})
  input_column <- reactive({input$xcolumn})
  input_vals <- reactive({input$value})
  
  # get list of columns to query
  observe({
    
    req(tbl_in <- input$table)
    print(tbl_in)

    if (!is.null(tbl_in)) {
      
      cols <- DBI::dbListFields(con, tbl_in)

      updateSelectInput(session, 'xcolumn', choices = cols)
    
    }
    
  }, priority = 1)
  
  # get list of values for selected column
  observeEvent(input$xcolumn, {

    req(selected_col <- input$xcolumn)
    print(selected_col)

    vals <- ''
    
    print(paste(input_table(), ' - inside'))
    
    if (!is.null(selected_col) | selected_col != '') {

      new_col <- input_column()
      
      vals <- tbl(con, input_table()) %>%
        select_(new_col) %>%
        distinct() %>%
        collect()

      updateSelectizeInput(session, 'value', choices = vals)

    }
    
  }, priority = 2)

  
  observeEvent(input$submit, {
    
    print(paste(input_column(), 'col to filter'))
    print(paste(input_vals(), 'val to filter'))
    
    cl <- input_column()
    vl <- input_vals()
    
    db_table <- tbl(con, input_table()) %>%
      filter(!!rlang::sym(cl) == vl) %>%
      collect()

    output$tbl <- DT::renderDataTable({db_table})

  })
  

}

# Run the application 
shinyApp(ui = ui, server = server)

