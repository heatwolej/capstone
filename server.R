# Required libraries
require(stringi)
require(stringr)
require(data.table)
require(ggplot2)
require(gridExtra)

# -----------------------------------------------------------------------------
# Startup processing (One-time)
# -----------------------------------------------------------------------------
uni_dat <- readRDS("unigram_data.RDS")
bi_dat <- readRDS("bigram_data.RDS")
tri_dat <- readRDS("trigram_data.RDS")
quad_dat <- readRDS("quadgram_data.RDS")
source('PredictWord.R', local=TRUE)

# -----------------------------------------------------------------------------
# Shiny server
# -----------------------------------------------------------------------------
shinyServer(function(input, output, session) {
  rv <- reactiveValues(data = NULL)
  observeEvent(input$go, {
    rv$limit <- as.numeric(input$inputLimit)
    output$limtMsg <- renderText(c("Return data table size set to ", input$inputLimit))
  })
    output$suggestions = renderTable ( {
    message(paste("Input: ", input$phrase))
    if (is.null(rv$limit)) {
      limit <- 10
    } else {
      limit <- rv$limit
    }
    data.table (`Suggested Word`=predictWord(input$phrase, limit))
}
  )

})