# Preliminary ----
library(officer, flextable)

item.add <- function(doc,
                     plots.in,
                     tables.in,
                     item.type = c("plots", "tables"),
                     item.bookmark = NULL,
                     doc.summary){
  
  if(is.null(item.bookmark)){
    item.bookmark <- switch(item.type,
                            "plots" = "Insert-plot:",
                            "tables" = "Insert-table:"
    )
  }
  
  doc.items.add <- doc.summary %>%
    filter(grepl(x = text, pattern = item.bookmark))
  
  # If there's at least one...
  if(length(doc.items.add) > 0){
    # At each of them...
    for(i in 1:nrow(doc.items.add)){
      # Take the plot label (index)
      doc.text <- doc.items.add$text[[i]]
      item.index <- gsub(x = doc.text, pattern = item.bookmark, "")
      # If the index is a number, convert it to numeric.
      if(!is.na(as.numeric(item.index))){
        item.index <- as.numeric(item.index)
      }
      # Move the virtual cursor to the bookmark.
      doc <- cursor_reach(doc, doc.text)
      # Paste the correct item, using the index.
      doc <- switch(item.type,
                    "plots" = body_add_gg(x = doc,
                                          value = plots.in[[item.index]],
                                          style = "centered"),
                    "tables" = body_add_flextable(x = doc,
                                                  value = tables.in[[item.index]])
      )
      # Move the virtual cursor back to the bookmark.
      doc <- cursor_reach(doc, doc.text)
      # And remove the bookmark.
      doc <- body_remove(doc)
    }
  }
  return(doc)
}

render.officer <- function(plots.in, tables.in, report.select.list, report.name){
  
  doc <- read_docx(here::here("ReportTemplates/Docx/", report.select.list))
  
  doc.summary <- docx_summary(doc)
  
  doc <- item.add(doc,
                  plots.in,
                  tables.in,
                  item.type = "plots",
                  doc.summary = doc.summary)
  
  docx_summary(doc)
  
  # Table properties. Add arguments to change these.
  set_flextable_defaults(
    split = TRUE,
    table_align = "center",
    table.layout = "autofit"
  )
  
  tables.in <- lapply(tables.in, flextable)
  
  doc <- item.add(doc,
                  plots.in,
                  tables.in,
                  item.type = "tables",
                  doc.summary = doc.summary)
  
  return(doc)
}