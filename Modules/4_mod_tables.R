tablesUI <- function(id) {

  tagList(
    page_fillable(
      layout_columns(
        card(
          actionButton(NS(id, "df.head.button"),
                       "Toggle data head"),
          selectInput(NS(id, "rm.cols"),
                      "Remove columns",
                      choices = NULL, multiple = TRUE),
          selectInput(NS(id, "split.vars"),
                      "Select splitting variables",
                      choices = NULL, multiple = TRUE),
          actionButton(NS(id, "split.button"),
                       "Split the data (optional)"),
          selectInput(NS(id, "splits.keep"),
                      "Select splits to plot",
                      choices = NULL, multiple = TRUE),
          selectInput(NS(id, "group.vars"),
                      "Select grouping variables",
                      choices = NULL, multiple = TRUE),
          selectInput(NS(id, "summary.var"),
                      "Numeric variable to summarize",
                      choices = NULL, multiple = TRUE),
          actionButton(NS(id, "table.button"),
                       "Make tables"),
          selectInput(NS(id, "table.view.select"),
                      "View table",
                      choices = NULL, multiple = FALSE),
          selectInput(NS(id, "table.out.select"),
                      "Select outputs",
                      choices = NULL, multiple = TRUE),
          actionButton(NS(id, "add.table.button"),
                      "Add table(s) to output")
          # multiInput(NS(id, "outputs"), "Outputs",
          #            choices = character(0),
          #            width = "200%",
          #            options = list(
          #              enable_search = FALSE,
          #              non_selected_header = "Options:",
          #              selected_header = "Selected:")
          #            ),
          # actionButton(NS(id, "export.button"), "Export to next tab")
          ),
        card(
          tableOutput(NS(id, "df.head")),
          tableOutput(NS(id, "table.view"))
          ),
        card(
          multiInput(NS(id, "outputs"), "Outputs",
                     choices = character(0),
                     width = "200%",
                     options = list(
                       enable_search = FALSE,
                       non_selected_header = "Options:",
                       selected_header = "Selected:"
                     )),
          actionButton(NS(id, "export.button"), "Export to next tab"),
          verbatimTextOutput(NS(id, "output.list"))
        ),
        col_widths = c(3, 5, 4)
        )
      )
    )
}

tablesServer <- function(id, df.in) {
  
  moduleServer(id, function(input, output, session) {
    
    req(df.in)  
    
    observeEvent(df.in(), {
      mod.out.tables <<- reactiveValues()
      selections <<- reactiveValues(d = NULL)
      
      updateSelectInput(session, "rm.cols",
                        choices = colnames(df.in()), selected = NULL)
    })
    
    df.out <- reactive({
          df.in() %>%
          select(-input$rm.cols)
    })
    
    observeEvent(df.out(), {
      updateSelectInput(session, "split.vars",
                        choices = colnames(df.out()), selected = NULL)
      updateSelectInput(session, "group.vars",
                        choices = colnames(df.out()), selected = NULL)
      updateSelectInput(session, "summary.var",
                        choices = colnames(df.out()), selected = NULL)
    })

    conditions <- reactiveValues(head.switch = FALSE)

    observeEvent(input$df.head.button, {
      req(df.out)
      conditions$head.switch <<- !conditions$head.switch
      if(conditions$head.switch == TRUE){
        output$df.head <- renderTable({ head(df.out()) })
      } else {
        output$df.head <- NULL
      }
    })

    df.split <- eventReactive(input$split.button, {
      req(input$split.vars)

      df.temp <- df.out() %>%
        group_by_at(input$split.vars)

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

    list.tables <- eventReactive(input$table.button, {
      req(input$group.vars, input$summary.var)
      # if(exists(df.split())){
      out <- df.split()[names(df.split()) %in% input$splits.keep]
      # } else {
      #   out <- list(no.split = df.out())
      # }

      out <- lapply(out, function(x){
        x %>%
          group_by_(input$group.vars) %>%
          summarize(
            N = n(),
            mean = mean(!!rlang::sym(input$summary.var), na.rm = TRUE))
        })
      return(out)
    })
  
    observeEvent(list.tables(), {
      
      updateSelectInput(session, "table.view.select",
                        choices = c("All tables (combined)",
                                    # "All tables (individual)",
                                    names(list.tables())),
                        selected = "All tables (combined)")
      
      updateSelectInput(session, "table.out.select",
                        choices = c("All tables (combined)",
                                    # "All tables (individual)",
                                    names(list.tables())),
                        selected = "All tables (combined)")
    })
    
    observeEvent(input$table.view.select, {
      
      if(input$table.view.select == "All tables (combined)"){

        out <- bind_rows(list.tables())

        output$table.view <- renderTable(out)
      }
      
      if(!(input$table.view.select %in% c("All tables (combined)"))){
        
        out <- list.tables()[[input$table.view.select]]
        
        output$table.view <- renderTable(out)
      }
    })
   
   observeEvent(input$add.table.button, {
     
     if("All tables (combined)" %in% input$table.out.select){
       mod.out.tables[["All tables (combined)"]] <- bind_rows(list.tables())
     
       } else {
       
         out <- list.tables()[input$table.out.select]
         names(out) <- input$table.out.select
         
         for(i in 1:length(out)){
         mod.out.tables[[names(out)[[i]]]] <<- out[[i]]
       }
     }

     updateMultiInput(
       session, "outputs", selected = NULL, choices = names(mod.out.tables))
  })

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
   
   mod.out.tables.final <- eventReactive(input$export.button, {
     req(mod.out.tables, input$outputs)
     # The final output...
     out <- reactiveValuesToList(mod.out.tables)
     # is the sorted content
     out <- out[order(match(names(out), input$outputs))]
     # minus what we don't want.
     out[(names(out) %in% input$outputs)]
   })

   observe({
    req(mod.out.tables.final())
    output$output.list <- renderPrint({ names(mod.out.tables.final()) })
    })
   
  return(mod.out.tables.final)
  })
}