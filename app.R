#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Old Faithful Geyser Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         selectInput('table', label = 'Table', selected = NULL,
                        choices = list('Flights' = 'flights',
                                       'Empty' = 'empty')
                        ),
         selectInput('column', label = 'Column', choices = NULL),
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
  
  con <- DBI::dbConnect(RPostgres::Postgres(), 
                        host = 'localhost',
                        port = 5432,
                        db = 'gisdata',
                        user = 'gisuser',
                        password = 'kubexapa')
  
  
  # get list of columns to query
  observe({
    
    tbl_in <- input$table
    print(tbl_in)
    
    table_in <<- tbl(con, tbl_in)

    if (!is.null(tbl_in)) {
      
      cols <- DBI::dbListFields(con, tbl_in)

      updateSelectInput(session, 'column', choices = cols)
    
    }
    
  })
  
  # get list of values for column
  observe({

    selected_col <- input$column
    print(selected_col)
    
    if (!is.null(selected_col) | selected_col != '') {

      vals <- tbl(con, input$table) %>%
        select_(selected_col) %>%
        distinct() %>%
        collect()

      updateSelectizeInput(session, 'value', choices = vals)

    }
  })

  observeEvent(input$submit, {

    db_table <- table_in %>%
      filter_(input$column == input$value) %>%
      collect()

    output$tbl <- DT::renderDataTable({db_tbl})

  })
  

}

# Run the application 
shinyApp(ui = ui, server = server)

