reportUI <- function(id) {
  tagList(
    page_fillable(
      layout_columns(
        card(
          selectInput(NS(id, "report.select.list"), "Select report outline",
                      selected = NULL,
                      choices = setdiff(list.files(here::here("ReportTemplates", "MarkdownToDocx"),
                                                   rec = FALSE),
                                        list.dirs(here::here("ReportTemplates", "MarkdownToDocx"),
                                                  recursive = FALSE, full.names = FALSE)),
                      multiple = FALSE),
          verbatimTextOutput(NS(id, "report.selected")),
          textInput(NS(id, "report.name"), "Report file name:"),
          actionButton(NS(id, "download"), "Generate report", width = "40%")
          ),
        col_widths = 6,
      )
    )
  )
}

reportServer <- function(id, plots.in, tables.in) {
  moduleServer(id, function(input, output, session) {
    
    output$report.selected <- renderText({ input$report.select.list })
    
    observeEvent(input$download, {
      
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
    })
    })
}