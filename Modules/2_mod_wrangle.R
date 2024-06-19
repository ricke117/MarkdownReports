wrangleUI <- function(id) {
  tagList(
    page_fillable(
      layout_columns(
        card(
          selectInput(NS(id, "D.format"),
                      "Does the data have a 'D_' column (ProUCL)?",
                      choices = c(TRUE, FALSE), multiple = FALSE),
          selectInput(NS(id, "rm.cols1"),
                      "Remove columns",
                      choices = NULL, multiple = TRUE),
          selectInput(NS(id, "nonanalytes"),
                      "Select non-analytes",
                      choices = NULL, multiple = TRUE, selected = NULL),
          actionButton(NS(id, "filter.button"), "Pivot analytes (wide to long)", width = "80%"),
          selectInput(NS(id, "data.item"),
                      "Select for download or export",
                      choices = c("Original", "Edited, not pivoted", "Pivoted"), multiple = FALSE),
          downloadButton(NS(id, "download.edited"), "Download edited data",
                         style = "width:80%;"),
          actionButton(NS(id, "export.button"), "Export to next tab (inactive)", width = "80%")
        ),
        card(
          tableOutput(NS(id, "df.data.head")),
          verbatimTextOutput(NS(id, "df.data.summary")),
          tableOutput(NS(id, "df.pivoted.head")),
          verbatimTextOutput(NS(id, "df.pivoted.summary"))
        ),
        col_widths = c(3,9)
      )
    )
  )
}

# The corresponding server module for creating a reactive object from the number selected by the slider.
wrangleServer <- function(id, df.in) {
  
  moduleServer(id, function(input, output, session) {
    
    req(df.in)
    
    df.data <- reactiveVal()
    
    observeEvent(df.in(),  {
      
      df.data(df.in())
      
      updateSelectInput(session, "rm.cols1", choices = names(df.in()))
    })
    
    output$df.data.head <- renderTable({
      head(df.data())
    })
    
    output$df.data.summary <- renderPrint({
      summary(df.data())
    })
    
    observe({
      if(is.null(input$rm.cols1)){
        out <- df.in()
      } else {
        out <- df.in() %>%
          select(-input$rm.cols1)
      }
        df.data(out)
    })

    observeEvent(df.data(), {
      
      if(input$D.format == TRUE){
        choices.nonanalytes <- df.data() %>%
          select(-starts_with("D_")) %>%
          colnames()
      } else {
        choices.nonanalytes <- df.data() %>%
          colnames()
      }
      updateSelectInput(session, "nonanalytes",
                        choices = choices.nonanalytes, selected = NULL)
    })
    
    df.pivoted <- reactiveVal()

    observeEvent(input$filter.button, {

      out <- df.data() %>%
        mutate(across(!input$nonanalytes, as.numeric))
      
      out.analytes <- out %>%
        select(!starts_with("D_")) %>%
        pivot_longer(cols = !input$nonanalytes,
                     names_to = c("Analyte"),
                     values_to = c("Concentration"))

      if(input$D.format == TRUE){
        out.ND.col <- out %>%
          pivot_longer(cols = starts_with("D_"),
                       names_to = c("Analyte"),
                       values_to = c("ND")) %>%
          select(ND)

        out <- cbind(out.analytes, out.ND.col)
      } else {
        out <- out.analytes
      }

      df.pivoted(out)
    })
    
    output$df.pivoted.head <- renderTable({
      head(df.pivoted())
    })
    
    output$df.pivoted.summary <- renderPrint({
      summary(df.pivoted())
    })
    
    df.out <- eventReactive(input$export.button, {
      switch(input$data.item,
             "Original" = df.in(),
             "Edited, not pivoted" = df.data(),
             "Pivoted" = df.pivoted())
    })
    
    output$download.edited <- downloadHandler(
      filename = "df.csv",
      content = function(file = "ShinyOutputs/"){
        write.csv(df.out(), file, row.names = FALSE)
      }
    )
    return(df.out)
  })
}