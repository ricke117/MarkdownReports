sortUI <- function(id) {
  tagList(
    page_fillable(
      layout_columns(
        card(
          selectInput(NS(id, "plot.select"), "Select plot(s)",
                      selected = NULL, choices = NULL, multiple = TRUE),
          actionButton(NS(id, "plot.rm.button"), "Remove plot(s)", width = "40%"),
          actionButton(NS(id, "reset.plot.button"), "Reset", width = "40%"),
          textInput(NS(id, "plot.label"), "Plot label"),
          actionButton(NS(id, "plot.label.button"), "Label plot(s)", width = "40%"),
          selectInput(NS(id, "table.select"), "Select tables(s)",
                      selected = NULL, choices = NULL, multiple = TRUE),
          actionButton(NS(id, "table.rm.button"), "Remove tables(s)", width = "40%"),
          actionButton(NS(id, "reset.table.button"), "Reset", width = "40%"),
          textInput(NS(id, "table.label"), "Table label"),
          actionButton(NS(id, "table.label.button"), "Label tables(s)", width = "40%")
        ),
        card(
          shinyjqui::orderInput(NS(id, "sort.plots"), "Reorder plots", items = NULL),
          uiOutput(NS(id, "plot.preview"))
        ),
        card(
          shinyjqui::orderInput(NS(id, "sort.tables"), "Reorder tables", items = NULL),
          uiOutput(NS(id, "table.preview"))
        ),
        col_widths = c(3,4,5)
        )
      )
  )
}

sortServer <- function(id, plots.in, tables.in) {
  moduleServer(id, function(input, output, session) {
    
    # PLOTS
    plots <- reactiveValues()
    
    observeEvent(c(plots.in(), input$reset.plot.button), {
      
      plots$out <- plots.in()[!is.null(names(plots.in()))]
      
      updateSelectInput(session, "plot.select",
                        choices = names(plots$out),
                        selected = NULL)
    })
    
    observeEvent(plots$out, {
      updateSelectInput(session, "plot.select",
                        choices = names(plots$out),
                        selected = NULL)
      
      shinyjqui::updateOrderInput(session, "sort.plots",
                                  items = names(plots$out),
                                  item_class = "success")
    })
    
    observeEvent(input$plot.rm.button, {
      req(plots.in())
      plots$out <<- plots.in()[!names(plots.in()) %in% input$plot.select]
      })

    observeEvent(input$plot.label.button, {
      n.plots <- length(input$plot.select)
      if(n.plots == 1){
        names(plots$out)[names(plots$out) == input$plot.select] <<- input$plot.label
      }
      if(n.plots > 1){
        for(i in c(1:n.plots)){
          # Note: you want to take the first element because there will be one
          # fewer matching name in each loop.
          names(plots$out)[names(plots$out) %in% input$plot.select][[1]] <<- paste0(input$plot.label,".",i, collapse = "")
        }
      }
    })
    
    observeEvent(plots$out, {
      shinyjqui::updateOrderInput(session,
                       "sort.plots",
                       items = names(plots$out),
                       item_class = "success")
      })
 
    out.plot.list.final <- eventReactive(input$sort.plots,{
        out <- plots$out
        out <- out[order(match(names(out), input$sort.plots))]
        out
      })

    # output$plot.preview <- renderPlot({ plot_grid(labels = names(out.plot.list.final()),
    #                                                              plotlist = out.plot.list.final(), ncol = 1) })
    
    output$plot.preview <- renderUI({
      
      lapply(1:length(out.plot.list.final()), function(i){
        
        id <- names(out.plot.list.final())[[i]]
        item_label <- id
        
        plotOutput(outputId = id)
        textOutput(outputId = item_label)
        
        return(list(
        output[[item_label]] <- renderText({ item_label }),
        output[[id]] <- renderPlot({
        out.plot.list.final()[[i]]
        })
        ))
      })
    })
    
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
          # Note: you want to take the first element because there will be one
          # fewer matching name in each loop.
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
    
    # output$plot.preview <- renderPlot({ plot_grid(labels = names(out.plot.list.final()),
    #                                                              plotlist = out.plot.list.final(), ncol = 1) })
    
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
    
    return(list(plots = out.plot.list.final, tables = out.table.list.final))
   })
}