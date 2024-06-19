sorttablesUI <- function(id) {
  tagList(
    page_fillable(
      layout_columns(
        card(
          selectInput(NS(id, "table.select"), "Select tables(s)",
                      selected = NULL, choices = NULL, multiple = TRUE),
          actionButton(NS(id, "table.rm.button"), "Remove tables(s)", width = "40%"),
          actionButton(NS(id, "reset.table.button"), "Reset", width = "40%"),
          textInput(NS(id, "table.label"), "Table label"),
          actionButton(NS(id, "table.label.button"), "Label tables(s)", width = "40%")
        ),
        card(
          shinyjqui::orderInput(NS(id, "sort.tables"), "Reorder tables", items = NULL),
          uiOutput(NS(id, "table.preview"))
        ),
        col_widths = c(3,9)
        )
      )
  )
}

sorttablesServer <- function(id, tables.in) {
  moduleServer(id, function(input, output, session) {
    
    # TABLES
    tables <- reactiveValues()
    
    observeEvent(c(tables.in(), input$reset.table.button), {
      tables$out <- tables.in()
      updateSelectInput(session, "table.select",
                        choices = names(tables.in()),
                        selected = NULL)
    })
    
    observeEvent(tables$out, {
      updateSelectInput(session, "table.select",
                        choices = names(tables$out),
                        selected = NULL)
      
      shinyjqui::updateOrderInput(session, "sort.tables",
                                  items = names(tables$out),
                                  item_class = "success")
    })
    
    observeEvent(input$table.rm.button, {
      req(tables.in())
      tables$out <<- tables.in()[!names(tables.in()) %in% input$table.select]
    })
    
    observeEvent(input$table.label.button, {
      n.tables <- length(input$table.select)
      if(n.tables == 1){
        names(tables$out)[names(tables$out) == input$table.select] <<- input$table.label
      }
      if(n.tables > 1){
        for(i in c(1:n.tables)){
          names(tables$out)[names(tables$out) %in% input$table.select][[1]] <<- paste0(input$table.label,".",i, collapse = "")
        }
      }
    })
    
    observeEvent(tables$out, {
      shinyjqui::updateOrderInput(session,
                                  "sort.tables",
                                  items = names(tables$out),
                                  item_class = "success")
    })
    
    out.table.list.final <- eventReactive(input$sort.tables,{
      out <- tables$out
      out <- out[order(match(names(out), input$sort.tables))]
      out
    })
    
    output$table.preview <- renderUI({
      
      lapply(1:length(out.table.list.final()), function(i){
        
        id <- names(out.table.list.final())[[i]]
        item_label <- id
        
        tableOutput(outputId = id)
        textOutput(outputId = item_label)
        
        return(list(
          output[[item_label]] <- renderText({ item_label }),
          output[[id]] <- renderTable({
            out.table.list.final()[[i]]
          })
        ))
      })
    })
    
    return(out.table.list.final)
   })
}