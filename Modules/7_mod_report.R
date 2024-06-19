reportUI <- function(id) {
  tagList(
    page_fillable(
      layout_columns(
        card(
          selectInput(NS(id, "report.type.select"), "How to generate report?",
                      selected = NULL, multiple = FALSE,
                      choices = c("From Markdown", "From Docx")),
          selectInput(NS(id, "report.select.list"), "Select report outline",
                      selected = NULL, choices = NULL,
                      multiple = FALSE),
          verbatimTextOutput(NS(id, "report.selected")),
          actionButton(NS(id, "report.preview"), "Preview report", width = "40%"),
          textInput(NS(id, "report.name"), "Report file name:"),
          actionButton(NS(id, "download"), "Download report", width = "40%")
          ),
        col_widths = 6,
      )
    )
  )
}

reportServer <- function(id, plots.in, tables.in) {
  moduleServer(id, function(input, output, session) {
    
    observeEvent(input$report.type.select, {
      updateSelectInput(session, "report.select.list",
                        choices = switch(input$report.type.select,
                                         "From Docx" = list.files(here::here("ReportTemplates", "Docx")),
                                         "From Markdown" = list.files(here::here("ReportTemplates", "MarkdownToDocx"))
                        ),
                        selected = NULL)
    })
    
    output$report.selected <- renderText({ input$report.select.list })
    
    observeEvent(input$download, {
      
        if(input$report.type.select == "From Markdown"){
        
        # PLOTS
        if(file.exists(here::here("Temp", "plots.RDS"))) {
          file.remove(here::here("Temp", "plots.RDS"))
        }
        if(length(names(plots.in())) > 0){
          saveRDS(plots.in(), file = here::here("Temp", "plots.RDS"))
        } else {
          saveRDS(NULL, file = here::here("Temp", "plots.RDS"))
        }
        
        #TABLES
        if(file.exists(here::here("Temp", "tables.RDS"))) {
          file.remove(here::here("Temp", "tables.RDS"))
        }
        if(length(names(tables.in())) > 0){
          saveRDS(tables.in(), file = here::here("Temp", "tables.RDS"))
        } else {
          saveRDS(NULL, file = here::here("Temp", "tables.RDS"))
        }
        
        params <- list(
          file_plots = "plots.RDS",
          file_tables = "tables.RDS"
          )
        
        rmarkdown::render(
          input = here::here("ReportTemplates", "MarkdownToDocx", input$report.select.list),
          output_dir = here::here('Outputs', 'Reports'),
          output_file = paste0(input$report.name, ".docx"),
          params = params,
          envir = new.env(parent = globalenv()))
      
        }
      
      if(input$report.type.select == "From Docx"){
        
        source(here::here("Functions", "render_officer.R"))
        
        doc <- render.officer(plots.in = plots.in(),
                       tables.in = tables.in(),
                       report.select.list = input$report.select.list,
                       report.name = input$report.name)
        
        print(x = doc,
              target = paste0(here::here("Outputs", "Reports"), "/", input$report.name, ".docx"))
      }
      
    })
    })
}