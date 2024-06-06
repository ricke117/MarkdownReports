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
               DT, # data tables
               rmarkdown, knitr, tinytex, # Markdown and related
               tidyverse) # for general data manipulation and piping

# Load all modules in the modules folder, but not those in "NotUsing".
modfiles <- list.files(here::here("Modules/"))
invisible(sapply(paste0(here::here("Modules/"), "/", modfiles[modfiles != "NotUsing"]), source))

# And functions in functions folder
fnfiles <- list.files(here::here("Functions/"))
invisible(sapply(paste0(here::here("Functions/"), "/", fnfiles[fnfiles != "NotUsing"]), source))

# Test data 
df.test <- read.csv(here::here("Data/TestData/Zn-Cu-two-zones-NDs.csv"))

g <- iris %>%
  ggplot(aes(x = Species, y = Sepal.Length)) +
  geom_point()

g2 <- iris %>%
  ggplot(aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point() +
  facet_wrap(~Species)

test.plot.list <- list(Plot1 = g, Plot2 = g2)

test.table.list <- list(Table1 = iris, Table2 = mtcars)

# The app ----

# Here I write a small app to test the latest modules.
TestMod <- function() {
  
  ui <- fluidPage(
    tabsetPanel(
      tabPanel("Import",
               uploadUI("Upload")),
      tabPanel("Wrangle",
               wrangleUI("Wrangle")),
      tabPanel("Plots",
               plotterUI("Plots")),
      tabPanel("Tables",
               tablesUI("Tables")),
      tabPanel("Organize report items",
               sortUI("Organize")),
      tabPanel("Send to report",
               reportUI("Report"))
    )
  )
  
  server <- function(input, output, session){
    
    upload.out <- uploadServer("Upload")

    wrangle.out <- wrangleServer("Wrangle", df.in = upload.out)

    plotter.out <- plotterServer("Plots", df.in =  wrangle.out)

    tables.out <- tablesServer("Tables", df.in = wrangle.out)

    sort.out <- sortServer("Organize",
                 plots.in = plotter.out,
                 tables.in = tables.out)
    
    reportServer("Report", plots.in = sort.out)
  }
  shinyApp(ui, server)
}

TestMod()