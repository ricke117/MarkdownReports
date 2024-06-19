# Preliminary code ----

# Clean the global environment.
## Note: you don't want this in the report templates, because it will clear the parameters that go into them.
rm(list = ls())

# Load packages.
## The following only loads packages that are required for this template.
# p_load from the pacman package checks if packages are installed, installs them if not, then loads them.
if (!require("pacman")) install.packages("pacman")

pacman::p_load(devtools, BiocManager, # packages for loading/installing other packages
               here, # using project-relative paths
               openxlsx, readxl, # reading in data
               lubridate, # date-times
               EnvStats, # toxicological functions 
               cowplot, gridGraphics, # for plotting
               flextable, # for tables
               knitr, bookdown, # for additional knitting options
               officedown, officer, mschart, rvg, # for producing Office documents and charts
               shiny, shinyWidgets, shinydashboard, shinyFiles, sortable, bslib, shinyjs, shinyalert, shinyjqui, # Shiny packages
               palmerpenguins, # Extra demo data
               DT, # data tables
               rmarkdown, knitr, tinytex, # Markdown and related
               base64enc,
               tidyverse) # for general data manipulation and piping

# Load all modules in the modules folder, but not those in "NotUsing".
modfiles <- list.files(here::here("Modules/"))
invisible(sapply(paste0(here::here("Modules/"), "/", modfiles[modfiles != "NotUsing"]), source))

# Test data 
df.test <- read.csv(here::here("Data/Zn-Cu-two-zones-NDs.csv"))

g1 <- mtcars %>%
  ggplot(aes(x = cyl, y = mpg, group = cyl)) +
  geom_boxplot()

g2 <- iris %>%
  ggplot(aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point() +
  facet_wrap(~Species)

g3 <- beaver1 %>%
  ggplot(aes(x = time, y = temp)) +
  geom_line() +
  geom_point()

test.plot.list <- list(Plot1 = g1, Plot2 = g2, Plot3 = g3)

test.table.list <- list(Table1 = mtcars, Table2 = penguins)

# The app ----
TestMod <- function() {
  
  ui <- fluidPage(
    tabsetPanel(
      tabPanel("Import",
               uploadUI("Upload")),
      tabPanel("Wrangle",
               wrangleUI("Wrangle")),
      # Make download button less wide
      # Develop into standalone app
      tabPanel("Plots",
               plotterUI("Plots")),
      # Make sure that splitting is optional
      tabPanel("Sort plots",
               sortplotsUI("Sortplots")),
      # Make remove and label buttons 40%
      # Make it like Adobe Acrobat (thumbnails at right, highlighted in center).
      # Make it more straightforward to assign group labels as well as individual labels
      # Allow saving all plots and tables together as hyperlinked pdfs or htmls, or ppts.
      ## These can function as individual appendices, or as cross-references for
      ## labeling plots/ tables before passing into the report.
      tabPanel("Tables",
               tablesUI("Tables")),
      # Allow resorting columns
      tabPanel("Sort tables",
               sorttablesUI("Sorttables")),
      # Make remove and label buttons 40%
      # Make it like Adobe Acrobat (thumbnails at right, highlighted in center).
      # Make it more straightforward to assign group labels as well as individual labels.
      # Potentially allow viewing the report template in parallel with the plots/tables,
      # and the places that correspond to different labels.
      # Include stats like row number, column number, etc.
      # Allow defining individual table styles as well as default style,
      # and viewing flextable previews in central console.
      tabPanel("Send to report",
               reportUI("Report"))
      # Allow viewing overall report preview.
      # For rendeing with officer, make it possible to pull out summary stats
      # into the body text, like with Markdown. 
    )
  )
  
  server <- function(input, output, session){

    upload.out <- uploadServer("Upload")

    wrangle.out <- wrangleServer("Wrangle", df.in = upload.out)

    plotter.out <- plotterServer("Plots", df.in = wrangle.out)

    tables.out <- tablesServer("Tables", df.in = wrangle.out)

    sortplots.out <- sortplotsServer("Sortplots",
                        #plots.in = plotter.out
                        plots.in = reactive({ test.plot.list })
                        )

    sorttables.out <- sorttablesServer("Sorttables",
                                #tables.in = tables.out
                                tables.in = reactive({ test.table.list })
                                )

    reportServer("Report",
                 #plots.in = sortplots.out,
                 plots.in = reactive({ test.plot.list }),
                 #tables.in = sorttables.out
                 tables.in = reactive({ test.table.list })
    )
    
  }
  shinyApp(ui, server)
}

TestMod()