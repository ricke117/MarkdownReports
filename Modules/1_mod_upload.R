uploadUI <- function(id) {
  tagList(
    fileInput(NS(id, "file"), "Choose csv, xls, or xlsx file", accept = c(".csv", ".xls", ".xlsx")),
    tableOutput(NS(id, "df_raw")),
    verbatimTextOutput(NS(id, "df_raw_summary"))
  )
}

# The corresponding server module for creating a reactive object from the number selected by the slider.
uploadServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Prepare empty reactive for data.
    df.data <- reactiveVal()
    # When a file is chosen...
    observeEvent(input$file, {
      # get its path
      data_file <- input$file[["datapath"]]
      # and extension.
      ext <- tolower(tools::file_ext(data_file))
      # If it's an xls or xlsx file
      if(ext %in% c("xls", "xlsx")) {
        # get its sheets.
        sheets <- excel_sheets(data_file)
        # If there's just one sheet
        if(length(sheets) == 1L) {
          # Make Dat() the data in that sheet.
          df.data(readxl::read_excel(data_file,sheet = 1L))
          # If there's more than one sheet
        } else {
          # Use this data-upload widget for selecting sheets.
          inputSweetAlert(
            session,
            inputId = "sheet",
            title = "Select the sheet",
            type = "question",
            input = "select",
            inputOptions = sheets
          )
          # And if "sheet" has been selected
          observeEvent(input[["sheet"]], {
            # Make Dat() that sheet's data.
            df.data(read_excel(input[["file"]][["datapath"]], sheet = input[["sheet"]]))
          })
        }
        # If the data is a csv file
      } else {
        if(ext == "csv"){
          # Make Dat() that csv's data.
          df.data(read.csv(data_file))
          # And if it's not a csv, either,
        } else {
          # Tell the user the file's wrong.
          sendSweetAlert(
            session,
            title = "Wrong file",
            text = "Please upload a `xlsx` file.",
            type = "error"
          )
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