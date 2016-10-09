library(shiny)
library(shinydashboard)
#library(DT)
#library(markdown)

shinyUI(dashboardPage(
    dashboardHeader(title = "Word Suggestions"),
    dashboardSidebar(
        sidebarMenu(id = 'menu',
                    menuItem("Word Suggestions", tabName = "suggest", icon = icon("mortar-board")),
                    menuItem("Set Size of List", tabName = "setSize", icon = icon("wrench"))
        )
    ),
    dashboardBody(
        tabItems(
            # -----------------------------------------------------------------
            # Tab: Word Suggestions
            # -----------------------------------------------------------------
            tabItem(tabName = 'suggest',
                    fluidRow(
                        box(title = "Natural Language Processing - Word Suggestion Utility", width = 12, status = "success", solidHeader = TRUE,
                            tags$p('The Word Suggestion utility is simple and easy to use.  Select the number of replies you desire from the tab menu at left and click submit.  Select Word suggestion tabe and simply begin typing a sentence or phrase in English.  Each time you enter a space, the word suggestion utility will refresh your table of suggested next words.  Enjoy!'),
                            textInput('phrase',
                                      'Enter your phrase:',
                                      value = "",
                                      width = NULL,
                                      placeholder = 'Input Phrase')
                        ) #box
                    ), #fluidRow
                    fluidRow(
                        box(title = "List of Suggested Next Words, ordered by a Stupid Backoff Algorithm:", width = 12, status = "primary", solidHeader = TRUE,
                            tableOutput('suggestions')
                        ) #box
                    ) #fluidRow
                ), #tabITem
            # -----------------------------------------------------------------
            # Tab: Set Size of List
            # -----------------------------------------------------------------
            tabItem(tabName = 'setSize',
                    fluidRow(
                       box(title = "Set Return Limit", width = 12, status = "info", solidHeader = TRUE,
                          tags$p('Choose the number of outputs that you would like to see.  You must select an integer value between 1 and 25'),
                          selectInput('inputLimit', 'Number of Values to Return', c(1:25),selected=10, width='80px'),
                          actionButton(inputId ="go", label = "Submit")
                          ) #box
                    ), #fluidRow
                    fluidRow(
                      box(title = "Message:", width = 12, status = "primary", solidHeader = FALSE,
                        tableOutput('limtMsg')
                      ) #box
                    ) #fluidRow
            ) #tabITem
        ) #tabItems
    ) #dashboardBody
  ) #dashboardPage

) #shinyUI
