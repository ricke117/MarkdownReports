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
          # selectInput(NS(id, "style.select.list"), "Select style template",
          #             selected = NULL,
          #             choices = setdiff(list.files(here::here("StyleTemplates", "WordStyleTemplates"),
          #                                          rec = FALSE),
          #                               list.dirs(here::here("StyleTemplates", "WordStyleTemplates"),
          #                                         recursive = FALSE, full.names = FALSE)),
          #             multiple = FALSE),
          # verbatimTextOutput(NS(id, "style.selected")),
          textInput(NS(id, "report.name"), "Report file name:"),
          actionButton(NS(id, "download"), "Generate report")
          ),
        col_widths = 6,
      )
    )
  )
}

reportServer <- function(id, plots.in, tables.in) {
  moduleServer(id, function(input, output, session) {
    
    output$report.selected <- renderText({ input$report.select.list })
    
    # output$style.selected <- renderText({ input$style.select.list })
    
    observeEvent(input$download, {
      
      saveRDS(plots.in(), file = here::here("plots.RDS"))
      
      params <- list(
        file_plots = "plots.RDS"
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