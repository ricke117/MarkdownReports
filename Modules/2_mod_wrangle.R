wrangleUI <- function(id) {
  tagList(
    page_fillable(
      layout_columns(
        card(
          tableOutput(NS(id, "df.data.head")),
          selectInput(NS(id, "D.format"),
                      "Does the data have a 'D_' column (ProUCL)?",
                      choices = c(TRUE, FALSE), multiple = FALSE),
          selectInput(NS(id, "rm.cols"),
                      "Remove columns",
                      choices = NULL, multiple = TRUE),
          selectInput(NS(id, "nonanalytes"),
                      "Select non-analytes",
                      choices = NULL, multiple = TRUE),
          actionButton(NS(id, "filter.button"), "Pivot data", width = "36%")
        ),
        card(
          tableOutput(NS(id, "df.edited.head")),
          verbatimTextOutput(NS(id, "df.out.summary")),
          downloadButton(NS(id, "download.edited"), "Download edited data", width = "36%"),
          selectInput(NS(id, "export.item"),
                      "Export...",
                      choices = c("Original", "Edited"), multiple = FALSE),
          actionButton(NS(id, "export.button"), "Export to next tab", width = "36%")
        ),
        col_widths = c(6,6)
      )
    )
  )
}

# The corresponding server module for creating a reactive object from the number selected by the slider.
wrangleServer <- function(id, df.in) {
  
  moduleServer(id, function(input, output, session) {
    
    req(df.in)
    
    df.data <- eventReactive(df.in(), {
      df.in() %>%
        rename_all(make.names)
    })
    
    output$df.data.head <- renderTable({
      req(df.data)
      df.data() %>%
        select(-input$rm.cols) %>%
        head()
    })
    
    observeEvent(df.data(), {
      
      updateSelectInput(session,
                        "rm.cols",
                        choices = names(df.data()))
    })
    
    choices.nonanalytes <- reactive({
      df.data() %>%
        select(-input$rm.cols) %>%
        select(-starts_with("D_")) %>%
        colnames()
        })
    
    observeEvent(choices.nonanalytes(), {
      updateSelectInput(session, "nonanalytes", choices = choices.nonanalytes())
    })

    df.edited <- eventReactive(input$filter.button, {
      
      out <- df.data()
      
      out <- out %>%
        select(-input$rm.cols) %>%
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
    })

      output$df.edited.head <- renderTable({
        req(df.edited())
        head(df.edited())
      })

      output$df.edited.summary <- renderPrint({
        req(df.edited())
        summary(df.edited())
      })

      # This will be saved as a csv.
      output$download.edited <- downloadHandler(
        filename = "df.edited.csv",
        content = function(file = "ShinyOutputs/"){
          write.csv(df.out(), file, row.names = FALSE)
        }
      )

      df.out <- eventReactive(input$export.button, {
        switch(input$export.item,
               "Original" = df.data(),
               "Edited" = df.edited())
      })
    return(df.out)
  })
}