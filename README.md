This repository will demonstrate how .docx, .pdf, and .html reports can be written using a combination of R Shiny and R Markdown. Priority is given to .docx files. For those, I've relied on officedown and other packages in the [officeverse](https://ardata-fr.github.io/officeverse/).

**Currently, only the files in ShinyMarkdownReports/ReportTemplates/MarkdownToDocx are functional.** 

For a thorough introduction to R Markdown, check the [R Markdown Cookbook (CRC Press)](https://bookdown.org/yihui/rmarkdown-cookbook/).

This repository is a first step toward the generation of parameterized reports, as conceived in the image, below. Importantly, the parameters of reports, including the data included in them, can be generated in other R scripts, including Shiny apps, then passed as options to the YAML header when knitting Rmd files (via knitr). These parameters are not restricted to YAML options; they can then be passed to functions within the body of the report. Using functions similar to block_pour_docx() in officedown, a preview of the knitted report could potentially be rendered within a Shiny app and re-knit with new parameters.

![MarkdownShinyReportsDiagram](https://github.com/ricke117/MarkdownReports/assets/143446674/72fd7cdf-e8dd-4fd8-aa71-0f3b7e85f68c)

It's also possible to generate reports with sequential calls to functions in knitr and other packages (for Word files, the officer package). Unlike the files in this repository, which were written in the source pane of the RStudio editor, knitting functions do not need to be called within the R Markdown document that will be knitted. It's possible that Markdown (Rmd) files can be avoided entirely. In this context, the parameters of reports would not have to be passed as options to a Markdown YAML header.

Where possible, Jacobs company styles are used. For Word files, these are represented in a .docx template file in the "Templates" folder. The template file was developed in the process described here.

Note that headers and footers are not produced in the current version of the .docx Markdown document, and the list of figures and tables may need to be updated in Word to show correct labels and page numbers. This will be improved in future versions.
