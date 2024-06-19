uploadUI <- function(id) {
  tagList(
    page_fillable(
      layout_columns(
        card(
          selectInput(NS(id, "file.menu"), "Select from 'Data' folder (csv, xls, or xlsx)",
                      selected = "None selected", multiple = FALSE,
                      choices = c("None selected", list.files(here::here("Data")))
                      ),
          fileInput(NS(id, "file.browse"),
                    "Or other folder (same format)",
                    placeholder = "None selected",
                    accept = c(".csv", ".xls", ".xlsx")),
          verbatimTextOutput(NS(id, "file.name")),
          verbatimTextOutput(NS(id, "file.path")),
          selectInput(NS(id, "sheet.select"), "Select sheet", choices = NULL, multiple = FALSE),
          actionButton(NS(id, "reset.button"), "Reset", width = "50%"),
          actionButton(NS(id, "upload.button"), "Export to next", width = "50%")
          ),
        card(
          tableOutput(NS(id, "df_raw")),
          verbatimTextOutput(NS(id, "df_raw_summary"))
        ),
        col_widths = c(3,9)
      )
    )
  )
}

# The corresponding server module for creating a reactive object from the number selected by the slider.
uploadServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    values <- reactiveValues(
      browse_state = "empty"
    )
    
    file_input_name <- reactiveVal()
    file_input_path <- reactiveVal()
    
    observeEvent(input$file.browse, {
      if(!is.null(input$file.browse)){
        reset("file.menu")
        file_input_name(input$file.browse$name)
        file_input_path(input$file.browse$datapath)
        if(is.null(sheets())){
          updateSelectInput(session, "sheet.select", choices = character(0), selected = character(0)) 
        }
      }
      if(is.null(input$file.browse) & input$file.menu != "None selected"){
        file_input_name(input$file.menu)
        file_input_path(paste0(here::here("Data"), "/", input$file.menu))
        if(is.null(sheets())){
          updateSelectInput(session, "sheet.select", choices = character(0), selected = character(0)) 
        }
      }
      if(is.null(input$file.browse) & input$file.menu == "None selected"){
        file_input_name(NULL)
        file_input_path(NULL)
        sheets(NULL)
        updateSelectInput(session, "sheet.select", choices = character(0), selected = character(0))
        file.ext <- reactiveVal()
        df.data <- reactiveVal()
      }
    })
    
    observeEvent(input$file.menu, {
      if(input$file.menu != "None selected"){
        reset("file.browse")
        file_input_name(input$file.menu)
        file_input_path(paste0(here::here("Data"), "/", input$file.menu))
        if(is.null(sheets())){
          updateSelectInput(session, "sheet.select", choices = character(0), selected = character(0)) 
        }
      }
      if(!is.null(input$file.browse) & input$file.menu == "None selected"){
        file_input_name(input$file.browse$name)
        file_input_path(input$file.browse$datapath)
        if(is.null(sheets())){
          updateSelectInput(session, "sheet.select", choices = character(0), selected = character(0)) 
        }
      }
      if(is.null(input$file.browse) & input$file.menu == "None selected"){
        file_input_name(NULL)
        file_input_path(NULL)
        sheets(NULL)
        updateSelectInput(session, "sheet.select", choices = character(0), selected = character(0))
        file.ext(NULL)
        df.data(NULL)
      }
    })
    
    observeEvent(input$reset.button, {
      sheets(NULL)
      reset('file.browse')
      reset('file.menu')
      reset('file.name')
      reset('file.path')
      updateSelectInput(session, "sheet.select", choices = character(0), selected = character(0))
      file_input_name(NULL)
      file_input_path(NULL)
      file.ext(NULL)
      df.data(NULL)
    })
    
    output$file.name <- renderText({
      if(!is.null(file_input_name())){
        file_input_name()
      } else {
        "None selected"
      }
    })
    
    output$file.path <- renderText({
      if(!is.null(file_input_path())){
        file_input_path()
      } else {
        "No file path"
      }
    })
    
    sheets <- reactiveVal()
    file.ext <- reactiveVal()
    df.data <- reactiveVal()
    
    observeEvent(file_input_path(), {
      if(!is.null(file_input_path())){
        file.ext(tolower(tools::file_ext(file_input_path())))
        if(file.ext() %in% c("xls", "xlsx")) {
          sheets(excel_sheets(file_input_path()))
            df.data(readxl::read_excel(file_input_path(), sheet = 1))
        } else {
          if(file.ext() == "csv"){
            sheets(NULL)
            updateSelectInput(session, "sheet.select", choices = character(0), selected = character(0))
            df.data(read.csv(file_input_path()))
          } else {
            output$file.name <- "Warning: Select csv, xls, or xlsx file"
            output$file.path <- "Warning: Select csv, xls, or xlsx file"
          }
        }
      } else {
        sheets(NULL)
        file.ext(NULL)
        df.data(NULL)
      }
    })
    
    observeEvent(df.data(), {
      req(df.data())
      out <- df.data()
      colnames(out) <- make.names(colnames(out))
      df.data(out)
    })
    
    observeEvent( sheets(), ignoreNULL = FALSE, {
      updateSelectInput(session, "sheet.select", choices = sheets())
    })

    observeEvent(input$sheet.select, {
      if(!is.null(file_input_path())){
        if(file.ext() %in% c("xls", "xlsx")) {
          df.data(readxl::read_excel(file_input_path(), sheet = input$sheet.select))
        } else {
          df.data(read.csv(file_input_path()))
        }
      }
    })

    # Generate preview of the raw data.
    output$df_raw <- renderTable({
      # This won't render until Dat() has been instantiated.
      req(df.data())
      head(df.data())
    })
    # Summarize raw data columns.
    output$df_raw_summary <- renderPrint({
      req(df.data())
      summary(df.data())
    })
    
    return(df.data)
  })
}