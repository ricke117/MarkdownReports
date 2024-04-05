This repository demonstrates how .docx, .pdf, and .html reports can be written in R Markdown files in RStudio. Each file type has its own folder.

The pdf and html folders are forthcoming.

The Word Markdown file uses officedown and other packages in the officeverse.

For a thorough introduction to R Markdown, check the R Markdown Cookbook (CRC Press).

This document is a first step toward the generation of parameterized reports. Importantly, the parameters of reports, including the data included in them, can be generated in other R scripts, including Shiny apps, then passed as options to the YAML header when knitting Rmd files (via knitr). These parameters are not restricted to YAML options; they can then be passed to functions within the body of the report. Using functions similar to block_pour_docx() in officedown, a preview of the knitted report could potentially be rendered within a Shiny app and re-knit with new parameters.

Because Markdown and Shiny can serve as platforms for each other, there are a lot of options when it comes to generating reports with them. These will be the topics of upcoming repositories.

It's also possible to generate reports with sequential calls to functions in knitr and other packages (for Word files, the officer package). Unlike the files in this repository, which were written in the source pane of the RStudio editor, knitting functions do not need to be called within the R Markdown document that will be knitted. It's possible that Markdown (Rmd) files can be avoided entirely. In this context, the parameters of reports would not have to be passed as options to a Markdown YAML header.

Where possible, Jacobs company styles are used. For Word files, these are represented in a .docx template file in the "Templates" folder. The template file was developed in the process described here.

Note that headers and footers are not produced in the current version of the .docx Markdown document, and the list of figures and tables may need to be updated in Word to show correct labels and page numbers. This will be improved in future versions.
