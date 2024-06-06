sortUI <- function(id) {
  tagList(
    page_fillable(
      layout_columns(
        card(
          selectInput(NS(id, "plot.select"), "Select plot(s)",
                      selected = NULL, choices = NULL, multiple = TRUE),
          actionButton(NS(id, "plot.rm.button"), "Remove plot(s)"),
          actionButton(NS(id, "reset.button"), "Reset"),
          textInput(NS(id, "plot.label"), "Plot label"),
          actionButton(NS(id, "plot.label.button"), "Label plot(s)"),
          shinyjqui::orderInput(NS(id, "sort.plots"), "Reorder plots", items = NULL),
          uiOutput(NS(id, "plot.preview"))
        ),
        card(
          selectInput(NS(id, "table.select"), "Select tables(s)",
                      selected = NULL, choices = NULL, multiple = TRUE),
          actionButton(NS(id, "table.rm.button"), "Remove tables(s)"),
          textInput(NS(id, "table.label"), "Table label"),
          actionButton(NS(id, "table.label.button"), "Label table(s)"),
          verbatimTextOutput(NS(id, "tables.for.report"))
        ),
        col_widths = c(5,5)
        )
      )
  )
}

sortServer <- function(id, plots.in, tables.in) {
  moduleServer(id, function(input, output, session) {
    
    # PLOTS
    plots <- reactiveValues()
    
    observeEvent(c(plots.in(), input$reset.button), {
      plots$out <- plots.in()
      updateSelectInput(session, "plot.select",
                        choices = names(plots.in()),
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
        item_label <- paste0(id,"_label")
        
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
    
    return(out.plot.list.final)
   })
}