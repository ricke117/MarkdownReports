plotterUI <- function(id) {

  tagList(
    page_fillable(
      layout_columns(
        card(
          actionButton(NS(id, "df.head.button"),
                       "Toggle data head"),
          selectInput(NS(id, "splitvars"),
                      "Select splitting variables",
                      choices = NULL, multiple = TRUE),
          actionButton(NS(id, "split.button"),
                       "Split the data (optional)"),
          selectInput(NS(id, "splits.keep"),
                      "Select splits to plot",
                      choices = NULL, multiple = TRUE),
          selectInput(NS(id, "xvar"),
                      "Select X variable",
                      choices = NULL, multiple = FALSE),
          selectInput(NS(id, "yvar"),
                      "Select Y variable",
                      choices = NULL, multiple = FALSE),
          selectInput(NS(id, "facet"),
                      "Select facets (2 max)",
                      choices = NULL, multiple = TRUE),
          actionButton(NS(id, "plot.button"),
                       "Make plot"),
          sliderInput(NS(id, "height"), "Plot height",
                      min = 100,
                      max = 1000,
                      value = 500),
          sliderInput(NS(id, "width"), "Plot width",
                      min = 100,
                      max = 1000,
                      value = 500)
          ),
        card(
          tableOutput(NS(id, "df.head")),
          plotOutput(NS(id, "plot"))
          ),
        card(
          selectInput(NS(id, "plot.type"),
                      "Add one plot or many subplots?",
                      choices = NULL, multiple = FALSE),
          actionButton(NS(id, "add.plot.button"),
                       "Add plot to outputs"),
          multiInput(NS(id, "outputs"), "Outputs",
                      choices = character(0),
                     width = "200%",
                     options = list(
                       enable_search = FALSE,
                       non_selected_header = "Options:",
                       selected_header = "Selected:"
                     )),
          actionButton(NS(id, "export.button"), "Export to next tab"),
          verbatimTextOutput(NS(id, "output.list")),
          ),
        col_widths = c(3, 5, 4)
        )
      )
  )
}

plotterServer <- function(id, df.in) {
  
  moduleServer(id, function(input, output, session) {
    
    req(df.in)
    
    mod.out.plots <- reactiveValues()
    selections <- reactiveValues()
    
    observeEvent(df.in(), {
      mod.out.plots <- reactiveValues()
      selections$d <- NULL
    })
 
    conditions <- reactiveValues(head.switch = FALSE)
    
    observeEvent(input$df.head.button, {
      conditions$head.switch <<- !conditions$head.switch
      if(conditions$head.switch == TRUE){
        output$df.head <- renderTable({ head(df.in()) })
      } else {
        output$df.head <- NULL
      }
    })
    
    observeEvent(df.in(), {
      updateSelectInput(session, "splitvars",
                        choices = colnames(df.in()), selected = NULL)
      updateSelectInput(session, "xvar",
                        choices = colnames(df.in()), selected = NULL)
      updateSelectInput(session, "yvar",
                        choices = colnames(df.in()), selected = NULL)
      updateSelectInput(session, "facet",
                        choices = colnames(df.in()), selected = NULL)
    })
    
    df.split <- eventReactive(input$split.button, {
      req(input$splitvars)
    
      df.temp <- df.in() %>%
        group_by_at(input$splitvars)
    
      df.split <- df.temp %>%
        group_split()
    
      df.temp <- group_keys(df.temp) %>% unite(names)
    
      names(df.split) <- df.temp[["names"]]
    
      return(df.split)
    })
    
    observeEvent(df.split(), {
      updateSelectInput(session, "splits.keep",
                        choices = names(df.split()),
                        selected = names(df.split()))
    })
    
    list.plots <- eventReactive(input$plot.button, {
      req(input$xvar, input$yvar, input$splits.keep)
    
      out <- df.split()[names(df.split()) %in% input$splits.keep]
    
      out <- lapply(out, function(x){
    
        g <- x %>%
          ggplot(aes_string(input$xvar, input$yvar)) +
          geom_point()
    
        if(length(input$facet) == 1){
          g <- g +
            facet_wrap(reformulate(input$facet), scales = "free")
        }
    
        if(length(input$facet) == 2){
          req(input$facet)
          g <- g +
            facet_grid(reformulate(input$facet[[1]],
                                   input$facet[[2]]),
                       scales = "free")
        }
    
        return(g)
    
      })
      return(out)
    })
    
    observeEvent(list.plots(), {
    
      output$plot <- renderPlot(
        width = function() input$width,
        height = function() input$height,
        {plot_grid(plotlist = list.plots())}
        )
    
      updateSelectInput(session, "plot.type",
                        choices = c("Whole plot", "Subplots"),
                        selected = "Whole plot")
      })
    
   observeEvent(input$add.plot.button, {
     
      # Generate the new items to add
      req(list.plots())
     
     if(input$plot.type == "Whole plot"){
      out <- plot_grid(plotlist = list.plots())
      out <- list(out)
      names(out) <- paste0(names(list.plots()), collapse = "_")
     }
     
      if(input$plot.type == "Subplots"){
        out <- list.plots()
      }
      
      for(i in 1:length(out)){
        mod.out.plots[[names(out)[[i]]]] <<- out[[i]]
      }
     
     updateMultiInput(
       session, "outputs", selected = NULL, choices = names(mod.out.plots))}
     )
   
   observeEvent(input$outputs, {
     if(is.null(selections$d)) {
       out = NULL
     } else {
       new = setdiff(input$outputs, selections$d)
       out = c(selections$d, new)
     }
     selections$d <<- out
     
     updateMultiInput(session,
                      "outputs",
                      selected = selections$d)
   })
   
   mod.out.plots.final <- eventReactive(input$export.button, {
     req(mod.out.plots, input$outputs)
     # The final output...
     out <- reactiveValuesToList(mod.out.plots)
     # is the sorted content
     out <- out[order(match(names(out), input$outputs))]
     # minus what we don't want.
     out <- out[(names(out) %in% input$outputs)]
     
     # Here, we also save thumbnails of the plots
     # do.call(file.remove, list(list.files(here::here("Outputs", "Plots", "Thumbnails"), full.names = TRUE)))
     # lapply(names(out), function(x){
     #   ggsave(plot = out[[x]],
     #          filename = paste0(here::here("Outputs", "Plots", "Thumbnails"), "/", x, ".png"),
     #          dpi = 100, scale = 0.5)
     # })
     
     return(out)
     })
   
   observe({
    req(mod.out.plots.final())
    output$output.list <- renderPrint({ names(mod.out.plots.final()) })
    })

  return(mod.out.plots.final)
  })
}