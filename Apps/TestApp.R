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
               tidyverse) # for general data manipulation and piping

# Load all modules in the modules folder, but not those in "NotUsing".
modfiles <- list.files(here::here("Modules/"))
invisible(sapply(paste0(here::here("Modules/"), "/", modfiles[modfiles != "NotUsing"]), source))

# Test data 
df.test <- read.csv(here::here("Data/TestData/Zn-Cu-two-zones-NDs.csv"))

g <- mtcars %>%
  ggplot(aes(x = disp, y = mpg)) +
  geom_point()

g2 <- iris %>%
  ggplot(aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point() +
  facet_wrap(~Species)

g3 <- beaver1 %>%
  ggplot(aes(x = time, y = temp)) +
  geom_line() +
  geom_point()


test.plot.list <- list(Plot1 = g, Plot2 = g2, Plot3 = g3)

test.table.list <- list(Table1 = mtcars, Table2 = penguins)

# The app ----
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
     
    plotter.out <- plotterServer("Plots", df.in = wrangle.out)
    
    tables.out <- tablesServer("Tables", df.in = wrangle.out)

    sort.out <- sortServer("Organize",
                           plots.in = plotter.out,
                           #plots.in = reactive({ test.plot.list }),
                           tables.in = tables.out)
                           #tables.in = reactive({test.table.list}))
    
    reportServer("Report",
                 plots.in = sort.out$plots,
                 #plots.in = reactive({ test.plot.list  }),
                 tables.in = sort.out$tables
                 #tables.in = reactive({ test.table.list })
    )
    
  }
  shinyApp(ui, server)
}

TestMod()