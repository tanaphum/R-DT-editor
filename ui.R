#
# code (DT editor) from https://blog.rstudio.com/2018/03/29/dt-0-4/
# by Yihui Xie
#
# concept code (undo) from https://rdrr.io/github/GerkeLab/grkShinyThings/man/undoHistory.html
#
# developed code by Tanaphum Wichaita
# https://github.com/tanaphum

library(shiny)
library(DT)


# Define UI for application that draws a histogram
shinyUI(fluidPage(
    tags$h2("past"),
    verbatimTextOutput('past'),
    tags$h2("current"),
    verbatimTextOutput('current'),
    tags$h2("future"),
    verbatimTextOutput('future'),
    actionButton("undo", "Undo"),
    actionButton("redo", "Redo"),
    DTOutput('x1')
))
