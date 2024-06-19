sortplotsUI <- function(id) {
  tagList(
    page_fillable(
      layout_columns(
        card(
          selectInput(NS(id, "plot.select"), "Select plot(s)",
                      selected = NULL, choices = NULL, multiple = TRUE),
          actionButton(NS(id, "plot.rm.button"), "Remove plot(s)", width = "45%"),
          actionButton(NS(id, "reset.plot.button"), "Reset", width = "45%"),
          textInput(NS(id, "plot.label"), "Plot label"),
          actionButton(NS(id, "plot.label.button"), "Label plot(s)", width = "45%"),
        ),
        card(
          shinyjqui::orderInput(NS(id, "sort.plots"), "Reorder plots", items = NULL),
          uiOutput(NS(id, "plot.preview"))
        ),
        card(
          verbatimTextOutput(NS(id, "check.thumbnails")),
          actionButton(NS(id, "check.order.button"), "Print plot order"),
          actionButton(NS(id, "thumbnails.reset"), "Reset thumbnails"),
          useShinyjs(),
          uiOutput(NS(id, "plot.thumbnails"))
        ),
        col_widths = c(3,6,3)
        )
      )
  )
}

sortplotsServer <- function(id, plots.in) {
  moduleServer(id, function(input, output, session) {
    
    # PLOTS
    plots <- reactiveValues()
    
    observeEvent(c(plots.in(), input$reset.plot.button), {
      
      plots$out <- plots.in()[!is.null(names(plots.in()))]
      
      updateSelectInput(session, "plot.select",
                        choices = names(plots$out),
                        selected = NULL)
      })
    
    observeEvent(input$thumbnails.reset, {
      req(plots$out)
      out <- plots$out
      do.call(file.remove, list(list.files(here::here("Outputs", "Plots", "Thumbnails"), full.names = TRUE)))
      lapply(names(out), function(x){
        ggsave(plot = out[[x]],
               filename = paste0(here::here("Outputs", "Plots", "Thumbnails"), "/", x, ".png"),
               dpi = 100, scale = 0.5)
      })
    })
    
    observeEvent(plots$out, {
      updateSelectInput(session, "plot.select",
                        choices = names(plots$out),
                        selected = NULL)
      
      shinyjqui::updateOrderInput(session, "sort.plots",
                                  items = names(plots$out),
                                  item_class = "success")
    })
    
    observeEvent(plots$out, {

      out <- lapply(names(plots$out), function(x){
        out <- tagList(
          tags$div(
            p(x),
            img(src = base64enc::dataURI(
               file = paste0(here::here("Outputs", "Plots", "Thumbnails"), "/", x, ".png"),
               mime = "image/png"),
               alt = x,
               style = "cursor:pointer;"
               )))
        names(out) <- x
        out
        })
      
      out <- lapply(out, `[[`, 1)

      output$plot.thumbnails <- renderUI({
        tagList(
        tags$div(
          id = "plot.thumbnails",
          out
          ),
        sortable_js(css_id = "plot.thumbnails")
      )
      })
    })
    
    observeEvent(input$plot.thumbnails, {
      output$check.thumbnails <- renderText({ input$plot.thumbnails})
    })
    
    observeEvent(input$plot.rm.button, {
      req(plots.in())
      plots$out <<- plots.in()[!names(plots.in()) %in% input$plot.select]
      })

    observeEvent(input$plot.label.button, {
      n.plots <- length(input$plot.select)
      if(n.plots == 1){
        # Rename thumbnails
        # current.thumbnail.paths <- list.files(here::here("Outputs", "Plots", "Thumbnails"), full.names = TRUE)
        # current.thumbnail.names <- list.files(here::here("Outputs", "Plots", "Thumbnails"))
        # thumbnail.names.to.change <- input$plot.select[input$plot.select %in% gsub(x = current.thumbnail.names, pattern = ".png", replacement = "")]
        # file.rename(
        # Rename plot
        names(plots$out)[names(plots$out) == input$plot.select] <<- input$plot.label
      }
      if(n.plots > 1){
        for(i in c(1:n.plots)){
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
    
    return(out.plot.list.final)
   })
}